// Copyright (c) 2014, Jeff Cohen
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
// 1. Redistributions of source code must retain the above copyright notice,
//    this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#include "InDeflator.h"
#include "DeflatedObject.h"
#include "entity/Class.h"
#include "entity/Namespace.h"
#include "entity/Parameters.h"
#include "type/Type.h"
#include "util/Integer.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"
#include "errno.h"
#include <algorithm>
#include <array>

// Why not use virtual functions?
//
// Answer: Performance and code size.
//
// Code size should be obvious.  Non-virtual calls take less code.  Not using
// virtual calls requires explicit testing of the serialization mode.  It turns
// out this is free for all intents and purposes, because the resulting branch
// is nearly 100% correctly predicted; after all, one does not mix the two types
// of serialization.
//
// It does look a bit uglier, granted, and it doesn't scale well to additional
// types of serializations, but that's OK for now.

// Template mode supports the capture of generic entities prior to parameter
// binding.  As such, a single object hierarchy is deflated and inflated, and
// some subset of referenced objects are excluded from the deflation.
//
// Module mode supports the export of a compiled module to a file.  As such,
// everything that is referenced must be deflated, no exceptions.  Subsets of
// objects are inflated on demand by an importing module, so each deflated
// object must be inflatable independently of any other; hence, no object is
// deflated inline within another.

// Set of types with which to seed the type mappings.  Note that these are all
// constexpr methods, so theoretically nothing executes at startup.
static auto &KnownTypes() {
  static std::array<Type, 20> g_knownTypes = { {
    Type(),     // must be first
    Type::Void(),
    Type::JType(),
    Type::Namespace(),
    Type::Bool(),
    Type::Float(),
    Type::Double(),
    Type::Char8(),
    Type::Char16(),
    Type::Char32(),
    Type::Transfers(),
    Type::Unbound(),
    Type::Suppress(),
    Type::NotTyped(),
    Type::PseudoInteger(),
    Type::PseudoFloat(),
    Type::PseudoChar(),
    Type::PseudoString(),
    Type::PseudoList(),
    Type::PseudoEnum()
  } };
  return g_knownTypes;
}

static const char g_fileHeader[] = "jolt module v0";

Deflator::Deflator(bool inModuleMode)
    : InDeflator(inModuleMode) {
  m_objectBlob.reserve(1024);
  m_objects.reserve(128);
}

void Deflator::operator<<(Object *obj) {
  DebugAppend(di_object_start);

  if (obj) {
    if (m_inModuleMode) {
      static_cast<ModuleDeflator *>(this)->DeflateObjectPointer_(obj);
    } else {
      // Don't serialize any reference from inside a template to the outside.
      bool save = m_forceReference;
      m_forceReference = false;

      if (Parameters **p = obj->GetTemplateRootHolder()) {
        auto I = std::find(m_templates.begin(), m_templates.end(), *p);
        m_forceReference = I == m_templates.end();
      }

      obj->GetMetadata().m_deflator(*this, obj);
      m_forceReference = save;
    }
  } else {
    Append(mopc_null);
  }

  DebugAppend(di_object_end);
}

void Deflator::operator<<(Parameters *obj) {
  m_templates.push_back(obj);
  *this << static_cast<Object *>(obj);
  m_templates.pop_back();
}

void Deflator::operator<<(std::string &s) {
  uint32_t size = uint32_t(s.size());
  *this << size;

  if (size > 0)
    Append(s.data(), size);
}

void Deflator::operator<<(Location sl) {
  if (!m_inModuleMode)
    Append(&sl, sizeof(Location));
}

void Deflator::operator<<(Type t) {
  if (m_inModuleMode)
    static_cast<ModuleDeflator *>(this)->DeflateType_(t);
  else
    Append(&t, sizeof(Type));
}

bool Deflator::StartObject(Object *self) {
  // In module mode, an object is asked to be deflated precisely once.  It
  // would have already been assigned an ordinal when that happens, so this
  // check here is both unnecessary and would lead to incorrect results.
  if (!m_inModuleMode) {
    // First check if this object has already been deflated.
    auto I = m_objects.find(self);
    if (I != m_objects.end()) {
      Append(mopc_ordinal);
      DebugAppend(di_ordinal);
      *this << I->second;
      return false;
    }

    // Nope, go and remember it, assigning it a new ordinal.
    uint32_t ord = uint32_t(m_objects.size());
    m_objects.emplace(self, ord);
  }

  // Emit external id of class.
  Metadata &mmd = self->GetMetadata();
  verify(mmd.m_externalId < mopc_smallest);
  Append(mmd.m_externalId);
  DebugAppend(di_creator);

  // Signal object to deflate its fields.
  return true;
}

bool Deflator::AppendExternal(Object *self) {
  if (m_inModuleMode)
    return StartObject(self);

  return static_cast<TemplateDeflator *>(this)->AppendExternal_(self);
}

/******************************************************************************/

TemplateDeflator::TemplateDeflator() : Deflator(false) {
  m_externals.reserve(32);
  m_externalsList.reserve(32);
}

TemplateDeflator::~TemplateDeflator() { }

DeflatedObject *TemplateDeflator::operator()(Object *obj) {
  verify(!m_objectBlob.size() && !m_externals.size() && !m_objects.size());
  DebugAppend(di_blob_start);
  *this << obj;
  DebugAppend(di_blob_end);

  size_t size = m_objectBlob.size();
  uint8_t *blob = new uint8_t[size];
  memcpy(blob, m_objectBlob.data(), size);

  return new DeflatedObject(blob, m_externalsList);
}

DeflatedObject *TemplateDeflator::operator()(Parameters *obj) {
  m_templates.push_back(obj);
  DeflatedObject *p = (*this)(static_cast<Object *>(obj));
  m_templates.pop_back();
  return p;
}

inline bool TemplateDeflator::AppendExternal_(Object *self) {
  auto I = m_externals.find(self);
  uint32_t ord;

  if (I != m_externals.end()) {
    ord = I->second;
  } else {
    ord = uint32_t(m_externalsList.size());
    m_externalsList.push_back(self);
    m_externals.emplace(self, ord);
  }

  Append(mopc_external);
  DebugAppend(di_external);
  *this << ord;
  return false;
}

/******************************************************************************/

ModuleDeflator::ModuleDeflator(FILE *f)
    : Deflator(true),
      m_file(f) {
  m_queue.reserve(64);
  m_offsets.reserve(256);
  m_types.reserve(256);
  m_typeBlob.reserve(1024);
  m_typeOffsets.reserve(256);
  m_typeQueue.reserve(64);

  // Seed types with various "well-known" types.
  uint32_t ord = 0;
  for (Type t: KnownTypes())
    m_types.emplace(t, ord++);
}

ModuleDeflator::~ModuleDeflator() {
  // Write file header.
  fputs(g_fileHeader, m_file);
  fputc(0, m_file);

  // Write object blob.
  uint32_t size = uint32_t(m_objectBlob.size());
  fwrite(&size, sizeof(size), 1, m_file);
  fwrite(m_objectBlob.data(), 1, size, m_file);

  // Write mapping of object ordinals to offsets.
  size = uint32_t(m_offsets.size());
  fwrite(&size, sizeof(size), 1, m_file);
  fwrite(m_offsets.data(), sizeof(uint32_t), size, m_file);

  // Write type blob.
  size = uint32_t(m_typeBlob.size());
  fwrite(&size, sizeof(size), 1, m_file);
  fwrite(m_typeBlob.data(), 1, size, m_file);

  // Write mapping of type ordinals to offsets.
  size = uint32_t(m_typeOffsets.size());
  fwrite(&size, sizeof(size), 1, m_file);
  fwrite(m_typeOffsets.data(), sizeof(uint32_t), size, m_file);
}

uint32_t ModuleDeflator::operator()(Object *obj) {
  // First check if object has already been deflated.
  auto I = m_objects.find(obj);
  if (I != m_objects.end())
    return I->second;

  // It has not.  Go assign it the next ordinal.
  uint32_t ord = uint32_t(m_objects.size());
  m_objects.emplace(obj, ord);
  m_offsets.push_back(uint32_t(m_objectBlob.size()));

  // Go deflate it.
  AppendOneObject(obj);

  // Go deflate any new objects and types referenced by the just-deflated
  // object.
  while (m_queue.size() > 0 || m_typeQueue.size() > 0) {
    while (m_queue.size() > 0) {
      auto q = m_queue.back();
      m_queue.pop_back();

      verify(m_offsets[q.second] == 0);
      m_offsets[q.second] = uint32_t(m_objectBlob.size());
      AppendOneObject(q.first);
    }

    while (m_typeQueue.size() > 0) {
      auto q = m_typeQueue.back();
      m_typeQueue.pop_back();

      verify(m_typeOffsets[q.second] == 0);
      m_typeOffsets[q.second] = uint32_t(m_typeBlob.size());
      AppendOneType(q.first);
    }
  }

  return ord;
}

void ModuleDeflator::AppendOneObject(Object *obj) {
  DebugAppend(di_object_start);

  // Need to special case imported entities.  We don't want to deflate them;
  // we want the importer of this module to inflate them from the same module
  // we got our copy from.
  if (auto e = dyn_cast<Entity *>(obj)) {
    if (e->Linkage() == lk_import) {
      if (auto ne = dyn_cast<Namespace *>(e)) {
        if (String *name = ne->GetModuleName()) {
          Append(mopc_imp_root);
          DebugAppend(di_impref_start);
          *this << name;
          goto done;
        }
      }

      Append(mopc_imp_entity);
      DebugAppend(di_impref_start);
      e->DeflateImportReference(*this);

done:
      DebugAppend(di_impref_end);
      DebugAppend(di_object_end);
      return;
    }
  }

  Metadata &mmd = obj->GetMetadata();
  mmd.m_deflator(*this, obj);
  DebugAppend(di_object_end);
}

inline void ModuleDeflator::DeflateObjectPointer_(Object *obj) {
  uint32_t ord = AssignObjectOrdinal(obj);

  // Emit reference to object, regardless of whether it's already deflated.
  Append(mopc_ordinal);
  DebugAppend(di_ordinal);
  *this << ord;
}

inline void ModuleDeflator::DeflateType_(Type t) {
  uint32_t ord = AssignTypeOrdinal(t);

  // Emit reference to type, regardless of whether it's already deflated.
  DebugAppend(di_type);
  *this << ord;
}

void ModuleDeflator::AppendOneType(Type t) {
  DebugTypeAppend(di_object_start);

  // If the type has modifiers, emit the unqualified type with annotations.
  Type u = t.DropQualifiers();
  if (t != u) {
    auto rt = t.RefKind();
    auto ct = t.ConstKind();
    uint8_t header = uint8_t(rt) << 1 | uint8_t(ct) | 0x80;
    m_typeBlob.push_back(header);
    AppendToTypeBlob(u);
  } else {
    auto kind = t.RawKind();
    m_typeBlob.push_back(uint8_t(kind));

    switch (kind) {
      case tk_fatpointer:
        // Note: there's only one possible descriptor for fat pointers, so they
        // can be treated the same as thin pointers.  This might change in the
        // future.
        verify(t.DescriptorType() == Type::IntPtrT());
        /* fall through */

      case tk_pointer:
        AppendToTypeBlob(t.BaseType());
        break;

      case tk_class:
        AppendToTypeBlob(t.Class());
        break;

      case tk_float:
        AppendToTypeBlob(t == Type::Double());
        break;

      case tk_integer:
        AppendToTypeBlob(t.IntLowerBound());
        AppendToTypeBlob(t.IntUpperBound());
        m_typeBlob.push_back(uint8_t(t.IsSigned()));
        break;

      case tk_char:
        AppendToTypeBlob(t.CharLowerBound());
        AppendToTypeBlob(t.CharUpperBound());
        break;

      case tk_array:
        AppendToTypeBlob(t.ElementType());
        AppendToTypeBlob(t.IndexType());
        break;

      case tk_set:
        AppendToTypeBlob(t.ElementType());
        break;

      case tk_tuple:
        AppendToTypeBlob(uint32_t(t.FieldCount()));
        for (size_t i = 0; i < t.FieldCount(); i++) {
          String *name = t.IdentAt(i);
          bool b = name != nullptr;
          AppendToTypeBlob(b);
          if (b)
            AppendToTypeBlob(name);
          AppendToTypeBlob(t.TypeOf(i));
        }
        break;

      case tk_generic:
      case tk_concept:
        AppendToTypeBlob(uint32_t(t.FieldCount()));
        for (size_t i = 0; i < t.FieldCount(); i++)
          AppendToTypeBlob(t.TypeOf(i));
        break;

      case tk_union:
        AppendToTypeBlob(t.TagType());
        break;

      case tk_function: {
        AppendToTypeBlob(t.ReturnType());
        uint32_t flags = (t.IsMethod() << 16) | (t.Throws() << 8) |
                         t.Variadic();
        AppendToTypeBlob(flags);
        AppendToTypeBlob((uint32_t)t.ArgCount());
        for (size_t i = 0; i < t.ArgCount(); i++)
          AppendToTypeBlob(t.ArgType(i));
        break;
      }

      case tk_enum: {
        Type et = t.ElementType();
        AppendToTypeBlob(et);
        AppendToTypeBlob(uint32_t(t.IdentCount()));
        for (size_t i = 0; i < t.IdentCount(); i++)
          AppendToTypeBlob(t.IdentAt(i));
        if (et != tk_void)
          for (size_t i = 0; i < t.IdentCount(); i++)
            AppendToTypeBlob(t.ValueAt(i));
        break;
      }

      case tk_void:
        break;

      case tk_codefragment: {
        Type u = t.BaseType();
        AppendToTypeBlob(u.IsKnown());
        if (u.IsKnown())
          AppendToTypeBlob(u.BaseType());
        break;
      }

      default:
        verify(false && "Deflating unknown type kind");
    }
  }

  DebugTypeAppend(di_object_end);
}

uint32_t ModuleDeflator::AssignObjectOrdinal(Object *obj) {
  // First check if this object has already been deflated.
  auto I = m_objects.find(obj);
  if (I != m_objects.end()) {
    // Yes.  Use previously assigned ordinal.
    return I->second;
  } else {
    // Not yet.  Assign it an ordinal and add to the queue for future deflating.
    uint32_t ord = uint32_t(m_objects.size());
    m_objects.emplace(obj, ord);
    m_offsets.push_back(0);
    m_queue.emplace_back(obj, ord);
    return ord;
  }
}

uint32_t ModuleDeflator::AssignTypeOrdinal(Type t) {
  // First check if this type has already been deflated.
  auto I = m_types.find(t);
  if (I != m_types.end()) {
    // Yes.  Use previously assigned ordinal.
    return I->second;
  } else {
    // Not yet.  Assign it an ordinal and add to the queue for future deflating.
    // Note that m_typeOffsets will be shorter than m_types by the size of
    // g_knownTypes.
    uint32_t ord = uint32_t(m_types.size());
    m_types.emplace(t, ord);
    m_typeQueue.emplace_back(t, uint32_t(m_typeOffsets.size()));
    m_typeOffsets.push_back(0);
    verify(m_types.size() == m_typeOffsets.size() + KnownTypes().size());
    return ord;
  }
}

void ModuleDeflator::AppendToTypeBlob(Type t) {
  uint32_t ord = AssignTypeOrdinal(t);
  DebugTypeAppend(di_type);
  AppendToTypeBlob(ord);
}

void ModuleDeflator::AppendToTypeBlob(bool value) {
  DebugTypeAppend(di_value_start);
  uint8_t *p = reinterpret_cast<uint8_t *>(&value);
  m_typeBlob.insert(m_typeBlob.end(), p, p + sizeof(value));
  DebugTypeAppend(di_value_end);
}

void ModuleDeflator::AppendToTypeBlob(uint32_t value) {
  DebugTypeAppend(di_value_start);
  uint8_t *p = reinterpret_cast<uint8_t *>(&value);
  m_typeBlob.insert(m_typeBlob.end(), p, p + sizeof(value));
  DebugTypeAppend(di_value_end);
}

void ModuleDeflator::AppendToTypeBlob(Object *obj) {
  DebugTypeAppend(di_ordinal);
  uint32_t ord = AssignObjectOrdinal(obj);
  AppendToTypeBlob(ord);
}

/******************************************************************************/

void Inflator::RegisterObject(Object *obj) {
  // Called by the inflator for Object.  This happens before any fields
  // for any subclass of Object get a chance to inflate anything, so by
  // mapping the object ordinal to the address here and now, cycles in the
  // object graph referring to this object before it completes inflating can
  // nonetheless still find it without resorting to backpatching kludges.
  verify(m_current && *m_current == nullptr);
  *m_current = obj;
}

Object *Inflator::InflateObjectPointer() {
  DebugScan(di_object_start);
  uint8_t code = Scan();
  Object *obj;

  switch (code) {
    case mopc_null:
      obj = nullptr;
      break;

    case mopc_external: {
      verify(!m_inModuleMode);
      uint32_t ord = 0;
      DebugScan(di_external);
      *this >> ord;

      auto self = static_cast<TemplateInflator *>(this);
      obj = self->m_deflated->m_externals[ord];
      break;
    }

    case mopc_ordinal: {
      uint32_t ord = 0;
      DebugScan(di_ordinal);
      *this >> ord;
      verify(ord < m_objects.size());
      if (Object *o = m_objects[ord]) {
        obj = o;
      } else {
        verify(m_inModuleMode);
        obj = static_cast<ModuleInflator *>(this)->operator()(ord);
        verify(obj == m_objects[ord]);
      }
      break;
    }

    default: {
      verify(!m_inModuleMode);
      DebugScan(di_creator);
      size_t ord = m_objects.size();
      m_objects.push_back(nullptr);
      Object **save = m_current;
      m_current = &m_objects.back();
      obj = Metadata::GetInflator(code)(*this);
      m_current = save;
      verify(m_objects[ord] == obj);
      break;
    }
  }

  DebugScan(di_object_end);
  return obj;
}

void Inflator::operator>>(std::string &s) {
  uint32_t size = 0;
  *this >> size;

  if (size > 0) {
    s.resize(size);
    Scan(const_cast<char *>(s.data()), size);
  }
}

void Inflator::operator>>(Location &sl) {
  if (m_inModuleMode)
    sl = Location();
  else
    Scan(&sl, sizeof(Location));
}

void Inflator::operator>>(Type &t) {
  if (m_inModuleMode)
    t = static_cast<ModuleInflator *>(this)->InflateType_();
  else
    Scan(&t, sizeof(Type));
}

uint32_t Inflator::GetUninflatedObject() {
  verify(m_inModuleMode);
  return static_cast<ModuleInflator *>(this)->GetUninflatedObject_();
}

/******************************************************************************/

TemplateInflator::TemplateInflator() : Inflator(false) { }

TemplateInflator::~TemplateInflator() { }

Object *TemplateInflator::operator()(DeflatedObject *d) {
  verify(!m_cursor && !m_deflated && !m_objects.size());
  m_deflated = d;
  m_objects.reserve(d->m_nObjects);
  m_cursor = d->m_blob;

  DebugScan(di_blob_start);
  Object *obj = nullptr;
  *this >> obj;
  DebugScan(di_blob_end);

  return obj;
}

/******************************************************************************/

static inline void readf(void *buf, size_t sz, size_t cnt, FILE *f) {
  size_t amt = fread(buf, sz, cnt, f);
  verify(amt == cnt);
}

ModuleInflator::ModuleInflator(FILE *f, const std::string &fname)
      : Inflator(true) {
  // Seed types with various "well-known" types.
  m_types.assign(KnownTypes().begin(), KnownTypes().end());

  // Read and verify file header.
  char header[sizeof(g_fileHeader)];
  readf(header, 1, sizeof(g_fileHeader), f);
  if (memcmp(header, g_fileHeader, sizeof(g_fileHeader))) {
    EmitFatal() << fname << ": not a module";
    return;
  }

  // Read object blob.
  uint32_t size;
  readf(&size, sizeof(size), 1, f);
  m_blob.resize(size);
  readf(m_blob.data(), 1, size, f);

  // Read mapping of object ordinals to offsets.
  readf(&size, sizeof(size), 1, f);
  m_offsets.resize(size);
  m_objects.resize(size);
  readf(m_offsets.data(), sizeof(uint32_t), size, f);

  // Read type blob.
  readf(&size, sizeof(size), 1, f);
  m_typeBlob.resize(size);
  readf(m_typeBlob.data(), 1, size, f);

  // Read mapping of type ordinals to offsets.
  readf(&size, sizeof(size), 1, f);
  m_typeOffsets.resize(size);
  m_types.resize(size + KnownTypes().size());
  readf(m_typeOffsets.data(), sizeof(uint32_t), size, f);
}

ModuleInflator::~ModuleInflator() { }

Object *ModuleInflator::operator()(uint32_t ord) {
  verify(ord < m_offsets.size());

  if (Object *cached = m_objects[ord])
    return cached;

  // In a debug build, a deflated object has a minimum of four bytes: three
  // DebugScans and a class id.
  verify(m_offsets[ord] <= m_blob.size() - 4);

  uint8_t *saved_cursor = m_cursor;
  m_cursor = m_blob.data() + m_offsets[ord];

  DebugScan(di_object_start);

  uint8_t code = Scan();
  DebugScan(code >= mopc_smallest ? di_impref_start : di_creator);

  Object *obj = nullptr;
  if (code >= mopc_smallest) {
    if (code == mopc_imp_root) {
      String *name = nullptr;
      *this >> name;

      obj = Namespace::FindModule(name);
    } else if (code == mopc_imp_entity) {
      obj = Entity::InflateImportReference(*this);
    } else {
      verify(false);
    }

    m_objects[ord] = obj;
    DebugScan(di_impref_end);
  } else {
    Object **save = m_current;
    m_current = &m_objects[ord];

    // If m_namespace is not set, a module is being imported and the relevant
    // fields will be set by Namespace::LinkToModule.
    obj = Metadata::GetInflator(code)(*this);
    if (auto e = dyn_cast<Entity *>(obj))
      if (m_namespace || ord)
        e->SetImportReference(m_namespace, ord);

    verify(m_objects[ord] == obj);
    verify(m_current == &m_objects[ord]);
    m_current = save;
  }

  DebugScan(di_object_end);

  m_cursor = saved_cursor;
  return obj;
}

Type ModuleInflator::InflateType_() {
  DebugScan(di_type);
  uint32_t ord;
  *this >> ord;

  verify(ord < m_types.size());
  Type t = m_types[ord];
  if (t.IsKnown())
    return t;

  // Type unknown is used to mark an uninflated type, but we know that this
  // type is assigned the ordinal zero, so...
  if (ord == 0)
    return t;

  // In a debug build, a deflated type has a minimum of eight bytes: three
  // DebugScans, a type header, and an ordinal.
  verify(ord >= KnownTypes().size());
  uint32_t offset = m_typeOffsets[ord - KnownTypes().size()];
  verify(offset <= m_typeBlob.size() - 8);

  uint8_t *saved_cursor = m_cursor;
  m_cursor = m_typeBlob.data() + offset;

  DebugScan(di_object_start);
  uint8_t header = Scan();

  // Check for annnotated type.
  if (header & 0x80) {
    t = InflateType_();

    ConstType ct = ConstType(header & 1);
    if (ct == ct_const)
      t = t.Const();

    RefType rt = RefType((header >> 1) & 3);
    if (rt == rt_rvalueref)
      t = t.RValueRef();
    else if (rt == rt_lvalueref)
      t = t.LValueRef();
  } else {
    TypeKind tk = TypeKind(header);
    switch (tk) {
      case tk_fatpointer:
        t = Type::PointerTo(InflateType_());
        verify(t.RawKind() == tk_fatpointer);
        break;

      case tk_pointer:
        t = Type::ThinPointerTo(InflateType_());
        break;

      case tk_class: {
        // This is potentially messy.  This is the only place a recursive type
        // can occur:  a class with a field whose type references that class.
        // Infinite recursion won't happen.  Even if the class object is in the
        // middle of being inflated, our attempt to inflate it will return
        // immediately with its address.  It's only necessary that the class's
        // m_type field has already been constructed.
        //
        // BUT:  If any of the inherited entity fields indirectly reference the
        // type of this class, bad things will probably happen.  This should be
        // a pathological case, but it isn't obviously impossible.
        auto ce = safe_cast<Class *>(InflateObject_());
        t = ce->AsType();
        break;
      }

      case tk_float:
        t = Scan() == 0 ? Type::Float() : Type::Double();
        break;

      case tk_integer: {
        auto lower = safe_cast<Integer *>(InflateObject_());
        auto upper = safe_cast<Integer *>(InflateObject_());
        uint8_t is_signed = Scan();
        t = Type::IntegerSubrange(lower, upper, is_signed != 0);
        break;
      }

      case tk_char: {
        uint32_t lower, upper;
        *this >> lower;
        *this >> upper;
        t = Type::CharSubrange(lower, upper);
        break;
      }

      case tk_array: {
        Type et = InflateType_();
        Type it = InflateType_();
        t = Type::ArrayOf(et, it);
        break;
      }

      case tk_set: {
        Type et = InflateType_();
        t = Type::SetOf(et);
        break;
      }

      case tk_tuple: {
        uint32_t count;
        *this >> count;

        vector<std::pair<String *, Type>> fields(count);
        for (auto& f: fields) {
          f.first = nullptr;
          bool b;
          *this >> b;
          if (b)
            f.first = safe_cast<String *>(InflateObject_());
          f.second = InflateType_();
        }

        t = Type::Tuple(fields);
        break;
      }

      case tk_generic:
      case tk_concept: {
        uint32_t count;
        *this >> count;

        vector<Type> fields(count);
        for (auto& f: fields)
          f = InflateType_();

        t = Type::Tuple(fields);
        if (tk == tk_concept)
          t = Type::Concept(t);
        else if (tk == tk_generic)
          t = Type::Generic(t);
        break;
      }

      case tk_union: {
        t = Type::Union(InflateType_());
        break;
      }

      case tk_function: {
        Type rt = InflateType_();

        uint32_t flags;
        *this >> flags;
        bool method = (flags >> 16) & 1;
        bool throws = (flags >> 8) & 1;
        VariadicType vt = VariadicType(flags & 0x7f);

        uint32_t argcnt;
        *this >> argcnt;
        vector<Type> argtypes(argcnt);
        for (auto& t : argtypes)
          t = InflateType_();

        t = Type::Function(rt, argtypes, method, throws, vt);
        break;
      }

      case tk_enum: {
        t = InflateType_();
        uint32_t count;
        *this >> count;

        vector<String *> idents(count);
        for (auto& n : idents)
          n = safe_cast<String *>(InflateObject_());

        if (t == tk_void) {
          t = Type::Enum(idents);
        } else {
          vector<Value *> values(count);
          for (auto& v : values)
            v = safe_cast<Value *>(InflateObject_());

          t = Type::Enum(idents, t, values);
        }
        break;
      }

      case tk_void:
        t = Type::Void();
        break;

      case tk_codefragment: {
        bool known;
        *this >> known;
        if (known)
          t = Type::CodeFragment(InflateType_());
        else
          t = Type::CodeFragment();
        break;
      }

      default:
        verify(false && "Deflating unknown type kind");
    }
  }

  DebugScan(di_object_end);

  m_cursor = saved_cursor;
  m_types[ord] = t;
  return t;
}

Object *ModuleInflator::InflateObject_() {
  DebugScan(di_ordinal);
  uint32_t ord;
  *this >> ord;
  return (*this)(ord);
}

uint32_t ModuleInflator::GetUninflatedObject_() {
  DebugScan(di_object_start);
  uint8_t code = Scan();
  verify(code == mopc_ordinal);

  uint32_t ord = 0;
  DebugScan(di_ordinal);
  *this >> ord;
  verify(ord < m_objects.size());

  DebugScan(di_object_end);
  return ord;
}
