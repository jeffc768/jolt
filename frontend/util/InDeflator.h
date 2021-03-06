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

/******************************************************************************/

// Serialization framework.
//
// Template Deflator/Inflator captures the pristine state of generic classes
// and methods prior to derivations.  Only work units that are generated by AST
// translation need to support serialization, and they only need to serialize
// fields relevant to a pristine copy.  It is NOT an external, portable or
// persistent representation
//
// Module Deflator/Inflator DOES create an external, persistent (but not
// portable) representation of a compiled module suitable for importing into
// another module compilation.

#pragma once

#include "Object.h"
#include "parser/Location.h"
#include "type/Type.h"
#include <cstring>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

class DeflatedObject;
class Inflator;
class Namespace;
class Parameters;

using std::vector;
using std::unordered_map;

template<class T> class P;

// Helper class for inflating/deflating types such as enums.
template<typename T> struct WX {
  T                  &m_value;

  WX(T &v) : m_value(v) { }
};

class InDeflator {
  // Declarations common to both Deflator and Inflator.

  InDeflator(const InDeflator &) = delete;
  InDeflator &operator=(const InDeflator &) = delete;

protected:
  InDeflator(bool inModuleMode) : m_inModuleMode(inModuleMode) { }

  bool                m_inModuleMode;

  // Special managed object pointer codes.
  enum {
    mopc_null       = 255, // null pointer
    mopc_external   = 254, // pointer to non-deflated object
    mopc_ordinal    = 253, // object deflated out-of-line
    mopc_imp_root   = 252, // reference to imported module root
    mopc_imp_entity = 251, // reference to imported entity
    mopc_smallest   = 251
  };

  // Special codes to catch mismatched deflators/inflators.
  enum {
    di_blob_start = 34,
    di_blob_end,
    di_object_start,
    di_object_end,
    di_value_start,
    di_value_end,
    di_creator,
    di_external,
    di_ordinal,
    di_impref_start,
    di_impref_end,
    di_type
  };

public:
  bool InModuleMode() { return m_inModuleMode; }
};

class Deflator: public InDeflator {
protected:
  Deflator(bool inModuleMode);

public:
  // Methods to deflate individual managed object fields.
  void operator<<(Object *obj);
  void operator<<(Parameters *obj);
  void operator<<(std::string &s);
  void operator<<(Location sl);
  void operator<<(Type t);

  template<class T> void operator<<(T *ptr) {
    DeflatorHelper(ptr, ptr);
  }

  template<class T> void operator<<(T value) {
    verify(sizeof(T) <= sizeof(size_t));
    DebugAppend(di_value_start);
    Append(&value, sizeof(T));
    DebugAppend(di_value_end);
  }

  template<class T> void operator<<(WX<T> &&wrapper) {
    verify(sizeof(T) <= sizeof(size_t));
    DebugAppend(di_value_start);
    Append(&wrapper.m_value, sizeof(T));
    DebugAppend(di_value_end);
  }

  template<class T, class U> void operator<<(std::pair<T *, U> value) {
    DebugAppend(di_value_start);
    Append(value.first, value.second * sizeof(T));
    DebugAppend(di_value_end);
  }

  // Every Deflate method must use one of the two following overloads before
  // deflating any fields.  This one is for deflatable objects.  Returns false
  // if the object has already been deflated (caller should immediately return).
  bool StartObject(Object *self);

  // And this one for objects that do not go into the deflated blob, but rather
  // a (managed) pointer to the object goes into the blob.  It gets deflated
  // anyway for module mode; it's equivalent to StartObject().
  bool AppendExternal(Object *self);

  // Reference, not serialize, the current object when this is set.
  bool                                          m_forceReference = false;

protected:
  void Append(uint8_t value) { m_objectBlob.push_back(value); }
  void Append(const void *p, size_t size) {
    const uint8_t *q = reinterpret_cast<const uint8_t *>(p);
    m_objectBlob.insert(m_objectBlob.end(), q, q + size);
  }

  // Handle non-interface pointers to managed objects.
  template<class T> void DeflatorHelper(T *dummy, Object *ptr) {
    *this << ptr;
  }

  // Handle interface pointers to managed objects.
  template<class T> void DeflatorHelper(T *ptr, ...) {
    // Verify the pointer is actually to an interface; will fail to compile
    // otherwise.
    verify(!ptr || static_cast<typename T::interface_t *>(ptr));

    if (ptr) {
      Object *obj = ptr->GetObjectBase();
      *this << obj;
      uint16_t offset = uint16_t(intptr_t(ptr) - intptr_t(obj));
      Append(&offset, sizeof(offset));
    } else {
      *this << (Object *)nullptr;
    }
  }

#ifdef NDEBUG
  void DebugAppend(uint8_t value) { }
#else
  void DebugAppend(uint8_t value) { Append(value); }
#endif

  // Deflated blob under construction.
  vector<uint8_t>                               m_objectBlob;

  // Map of already deflated objects and their assigned ordinal.
  unordered_map<Object *, uint32_t>             m_objects;

  // Stack of template roots.
  vector<Parameters *>                          m_templates;
};

class TemplateDeflator: public Deflator {
  friend class Deflator;

public:
  TemplateDeflator();
  ~TemplateDeflator();

  // Deflate single object; return blob for it.
  DeflatedObject *operator()(Object *obj);
  DeflatedObject *operator()(Parameters *obj);

private:
  bool AppendExternal_(Object *self);

  // Map of external (non-deflated) objects to their assigned ordinal.
  unordered_map<Object *, uint32_t>             m_externals;
  vector<Object *>                              m_externalsList;
};

class ModuleDeflator: public Deflator {
  friend class Deflator;

public:
  ModuleDeflator(FILE *f);
  ~ModuleDeflator();

  // Deflate object and append to blob (if it hasn't been appended already,
  // directly or indirectly).  Returns ordinal assigned to object.
  uint32_t operator()(Object *obj);

private:
  void DeflateObjectPointer_(Object *obj);
  void DeflateType_(Type t);

  void AppendOneObject(Object *obj);
  void AppendOneType(Type t);

  uint32_t AssignObjectOrdinal(Object *obj);
  uint32_t AssignTypeOrdinal(Type t);

  void AppendToTypeBlob(Type t);
  void AppendToTypeBlob(bool);
  void AppendToTypeBlob(uint32_t);
  void AppendToTypeBlob(Object *obj);

  // The file to which the module shall be written.
  FILE               *m_file = nullptr;

  // Queue of objects waiting to be deflated.
  using queue_element_t = std::pair<Object *, uint32_t>;
  vector<queue_element_t>                       m_queue;

  // Offsets of object within the deflated blob.  Indexed by object ordinal.
  vector<uint32_t>                              m_offsets;

  // Tracks types seen so far.
  unordered_map<Type, uint32_t>                 m_types;
  vector<uint8_t>                               m_typeBlob;
  vector<uint32_t>                              m_typeOffsets;

  using type_queue_element_t = std::pair<Type, uint32_t>;
  vector<type_queue_element_t>                  m_typeQueue;

#ifdef NDEBUG
  void DebugTypeAppend(uint8_t value) { }
#else
  void DebugTypeAppend(uint8_t value) { m_typeBlob.push_back(value); }
#endif
};

class Inflator: public InDeflator {
protected:
  Inflator(bool inModuleMode) : InDeflator(inModuleMode) { }

public:
  void RegisterObject(Object *obj);
  void operator>>(std::string &s);
  void operator>>(Location &sl);
  void operator>>(Type &s);

  template<class T> void operator>>(T *&ptr) {
    InflatorHelper(ptr, ptr);
  }

  // Handle non-interface pointers to managed objects.
  template<class T> void InflatorHelper(T *&ptr, Object *dummy) {
    Object *obj = InflateObjectPointer();
    // FIXME: Can't use safe_cast, because cycles in the graph means it gets
    // called on a partially constructed object, which blows up.
    ptr = static_cast<T *>(obj); // safe_cast<T *>(obj);
  }

  // Handle interface pointers to managed objects.
  template<class T> void InflatorHelper(T *&ptr, ...) {
    if (Object *obj = InflateObjectPointer()) {
      uint16_t offset = 0;
      Scan(&offset, sizeof(offset));
      intptr_t addr = intptr_t(obj) + offset;
      ptr = reinterpret_cast<T *>(addr);
      verify(!ptr ||
          offset == (uintptr_t(ptr) - uintptr_t(ptr->GetObjectBase())));
    } else {
      ptr = nullptr;
    }
  }

  template<class T> void operator>>(T &value) {
    verify(sizeof(T) <= sizeof(size_t));
    DebugScan(di_value_start);
    Scan(&value, sizeof(T));
    DebugScan(di_value_end);
  }

  template<class T> void operator>>(WX<T> &&wrapper) {
    verify(sizeof(T) <= sizeof(size_t));
    DebugScan(di_value_start);
    Scan(&wrapper.m_value, sizeof(T));
    DebugScan(di_value_end);
  }

  template<class T, class U> void operator>>(std::pair<T *, U> value) {
    DebugScan(di_value_start);
    Scan(value.first, value.second * sizeof(T));
    DebugScan(di_value_end);
  }

  // Used by Scope inflator to defer inflation of entities within that scope.
  uint32_t GetUninflatedObject();

protected:
  Object *InflateObjectPointer();
  uint8_t Scan() { return *Advance(1); }
  void Scan(void *p, size_t size) { memcpy(p, Advance(size), size); }

  uint8_t *Advance(size_t size) {
    uint8_t *cursor = m_cursor;
    m_cursor += size;
    return cursor;
  }

#ifdef NDEBUG
  void DebugScan(uint8_t value) { }
#else
  void DebugScan(uint8_t value) {
    uint8_t code = Scan();
    verify(code == value);
  }
#endif

  // Current offset into deflated blob.
  uint8_t            *m_cursor = nullptr;

  // Object currently being inflated.
  Object            **m_current = nullptr;

  // Vector of previously inflated objects.
  vector<Object *>    m_objects;
};

class TemplateInflator: public Inflator {
  friend class Inflator;

public:
  TemplateInflator();
  ~TemplateInflator();

  Object *operator()(DeflatedObject *d);

private:
  // The DeflatedObject being inflated.
  DeflatedObject     *m_deflated = nullptr;

};

class ModuleInflator: public Inflator {
  friend class Inflator;

public:
  ModuleInflator(FILE *f, const std::string &fname);
  ~ModuleInflator();

  // Return inflated requested object, identified by its ordinal.
  Object *operator()(uint32_t ord);

  void SetLinkedNamespace(Namespace *ns) { m_namespace = ns; }

private:
  Type InflateType_();
  Object *InflateObject_();
  uint32_t GetUninflatedObject_();

  // A blob of bytes, a map of object ordinals to offsets within that blob,
  // and a map of ordinals to inflated objects.  (m_objects is inherited.)
  vector<uint8_t>                               m_blob;
  vector<uint32_t>                              m_offsets;

  // Likewise for types.
  vector<uint8_t>                               m_typeBlob;
  vector<uint32_t>                              m_typeOffsets;
  vector<Type>                                  m_types;

  // Pointer back to linked namespace.
  Namespace                                    *m_namespace = nullptr;
};

template<class T> void operator<<(Deflator &DF, const vector<T> &vec) {
  size_t count = vec.size();
  const T *ary = vec.data();
  DF << count;
  for (size_t i = 0; i < count; i++)
    DF << ary[i];
}

template<class T> void operator>>(Inflator &IF, vector<T> &vec) {
  size_t count = 0;
  IF >> count;
  vec.resize(count);
  T *ary = vec.data();
  for (size_t i = 0; i < count; i++)
    IF >> ary[i];
}

template<class K, class V, class H, class C>
void operator<<(Deflator &DF, const unordered_map<K, V, H, C> &map) {
  size_t count = map.size();
  DF << count;
  for (auto I = map.begin(); I != map.end(); ++I) {
    DF << I->first;
    DF << I->second;
  }
}

template<class K, class V, class H, class C>
void operator>>(Inflator &IF, unordered_map<K, V, H, C> &map) {
  verify(map.empty());

  size_t count = 0;
  IF >> count;
  map.reserve(count);

  for (size_t i = 0; i < count; i++) {
    K key;
    IF >> key;
    IF >> map[key];
  }
}
