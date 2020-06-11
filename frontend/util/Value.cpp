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

#include "Value.h"
#include "Hash.h"
#include "InDeflator.h"
#include "Integer.h"
#include "String.h"
#include "entity/Class.h"
#include "entity/Generic.h"
#include "entity/Derivation.h"
#include "entity/Namespace.h"
#include "node/Node.h"
#include "parser/Token.h"
#include "util/BufferWriter.h"
#include "util/CodeFragment.h"
#include "util/Integer.h"
#include "util/SHA512.h"
#include <ctype.h>
#include <cmath>

IMPLEMENT_OBJECT_NOSIZE(Value)

namespace {
  struct StrRef {
    const char *chars;
    size_t      len;
  };
}

Value::Value(Type t)
    : m_type(t), m_size((uint32_t)t.StorageSize()) {
  memset(&m_data, 0, m_size);
}

Value::Value(Type t, const void *addr, size_t size)
    : m_type(t), m_size((uint32_t)size) {
  memcpy(&m_data, addr, m_size);
}

void Value::PreDestroy() {
  if (m_type == tk_codefragment) {
    auto cf = As<CodeFragment *>();
    CodeFragment::DropRef(cf);
  }
}

void Value::DeflateFields(Deflator &DF) {
  DF << m_type;

  // FIXME: if the type is of something that is a Object, or a pointer
  // to a managed object, then it needs to be deflated as something other than
  // a raw sequence of bytes, especially for module mode.
  // FIXME: if the type is a Class, then the fields must be recursively
  // deflated.  Upon inflation, the vtable, if present, must be set.
  verify(m_type == rt_rvalue);
  switch (m_type.Kind()) {
    case tk_unknown:
    case tk_void:
    case tk_bool:
    case tk_float:
    case tk_char:
    case tk_integer:
    case tk_enum:
    case tk_set:
      break;
    case tk_class:
      verify(!m_type.Class()->GetVtableSize());
      break;
    case tk_type:
      DF << AsType();
      return;
    case tk_array:
      verify(m_type.IndexType().IsKnown());
      verify(m_type.ElementType() == tk_char); // FIXME
      break;
    case tk_pseudo:
      DF << As<Object *>();
      return;
    case tk_pointer:
      verify(m_type.IsThin() && !m_data);
      return;
    case tk_tuple:
      // Verify all members have acceptable types and values.
      // Don't deflate padding between members.
      verify(false);
    case tk_codefragment:
      verify(false);  // FIXME: just serialize the node tree?
    default:
      verify(false);
  }

  verify(m_size == m_type.StorageSize());
  if (m_size > 0)
    DF << std::make_pair((uint8_t *)&m_data, m_size);
}

Value *Value::Inflate(Inflator &IF) {
  Value *obj = nullptr;
  Type t;
  IF >> t;

  if (t == tk_type) {
    Type type;
    IF >> type;
    obj = NewType(type);
  } else if (t == tk_pseudo) {
    Object *o;
    IF >> o;
    obj = NewPseudo(t, o);
  } else if (t == tk_pointer) {
    obj = NewPtr(t, nullptr);
  } else {
    size_t size = t.StorageSize();

    if (size > 0) {
      vector<char> bytes(size);
      IF >> std::make_pair(bytes.data(), size);
      obj = NewRaw(t, bytes.data(), size);
    } else {
      obj = New(t);
    }
  }

  IF.RegisterObject(obj);
  return obj;
}

size_t Value::GetObjectSize() {
  return sizeof(Value) + m_size - sizeof(m_data);
}

Value *Value::New(Type t) {
  size_t size = t.StorageSize();
  void *p = malloc(sizeof(Value) + size - sizeof(uintptr_t));
  new(p) Value(t);
  return reinterpret_cast<Value *>(p);
}

Value *Value::NewRaw(Type t, const void *bytes) {
  size_t size = t.StorageSize();
  void *p = malloc(sizeof(Value) + size - sizeof(uintptr_t));
  new(p) Value(t, bytes, size);
  return reinterpret_cast<Value *>(p);
}

// This variant shouldn't really exist.  It's needed, unfortunately, because
// built-in attributes are created before their class types are resolved.
Value *Value::NewRaw(Type t, const void *bytes, size_t size) {
  //verify(size == t.StorageSize());
  void *p = malloc(sizeof(Value) + size - sizeof(uintptr_t));
  new(p) Value(t, bytes, size);
  return reinterpret_cast<Value *>(p);
}

Value *Value::NewPseudo(Type t, Object *ptr) {
  verify(t == tk_pseudo);
  size_t size = t.StorageSize();
  void *p = malloc(sizeof(Value) + size - sizeof(uintptr_t));
  new(p) Value(t, &ptr, size);
  return reinterpret_cast<Value *>(p);
}

Value *Value::NewPtr(Type t, const void *ptr) {
  verify(t == tk_pointer || t == tk_generic || t == tk_namespace ||
         t == tk_codefragment);
  size_t size = sizeof(void *);
  void *p = malloc(sizeof(Value) + size - sizeof(uintptr_t));
  new(p) Value(t, &ptr, size);
  return reinterpret_cast<Value *>(p);
}

Value *Value::NewType(Type t) {
  size_t size = sizeof(Type);
  void *p = malloc(sizeof(Value) + size - sizeof(uintptr_t));
  new(p) Value(Type::JType(), &t, size);
  return reinterpret_cast<Value *>(p);
}

Value *Value::NewBool(bool b) {
  size_t size = sizeof(bool);
  void *p = malloc(sizeof(Value) + size - sizeof(uintptr_t));
  new(p) Value(Type::Bool(), &b, size);
  return reinterpret_cast<Value *>(p);
}

Value *Value::NewFloat(Type t, double value) {
  if (t == Type::Float()) {
    float f = float(value);
    return NewRaw(t, &f, sizeof(f));
  } else if (t == Type::Double()) {
    return NewRaw(t, &value, sizeof(value));
  } else {
    verify(false);
  }
}

int Value::Compare(Value *other) {
  if (this == other)
    return 0;

  if (int rv = m_type.Compare(other->m_type))
    return rv;

  return memcmp(&m_data, &other->m_data, m_size);
}

uint32_t Value::Hash() {
  uint32_t hash = ::Hash(m_type);
  return hash ^ ::Hash((char *)&m_data, m_size);
}

bool Value::IsSubtypeOf(Type t) {
  // FIXME: also look for values of enum types with only a single, suitable
  // member.
  if (m_type == t)
    return true;

  if (m_type == Type::PseudoInteger()) {
    if (t != tk_integer && t != tk_float && t != Type::PseudoFloat())
      return false;
#if 0 // FIXME: Integer test must be overhauled
    Integer *value = safe_cast<Integer *>(As<Object*>());
    if (value->Lt(t.IntLowerBound()))
      return false;
    if (value->Gt(t.IntUpperBound()))
      return false;
#endif
    return true;
  } else if (m_type == Type::PseudoFloat()) {
    return t == tk_float;
  } else if (m_type == Type::PseudoEnum()) {
    String *ident = safe_cast<String *>(As<Object*>());
    if (t == tk_enum)
      return t.OrdinalOf(ident) != -1;
    if (t == tk_pointer)
      return ident == String::Get(wks_null);
    if (t == tk_integer || t == Type::PseudoInteger())
      return ident == String::Get(wks_min)
          || ident == String::Get(wks_max);
    if (t == tk_float || t == Type::PseudoFloat())
      return ident == String::Get(wks_nan)
          || ident == String::Get(wks_infinity);
    return false;
  } else if (m_type == Type::PseudoChar()) {
    if (t != tk_char)
      return false;
    String *s = safe_cast<String *>(As<Object*>());
    // FIXME: handle utf encodings
    unsigned char c = *s->c_str();
    if (c < t.CharLowerBound())
      return false;
    if (c > t.CharUpperBound())
      return false;
    return true;
  } else if (m_type == Type::PseudoString()) {
    if (t == tk_pointer && t.BaseType() == Type::Char8().Const())
      return true;
    if (t != tk_array || t.ElementType() != tk_char)
      return false;
    intptr_t card = t.IndexType().Cardinality();
    if (t.IndexType().IsKnown() && card == -1)
      return false;
    String *s = safe_cast<String *>(As<Object*>());
    if (card >= 0 && size_t(card) != s->Length())
      return false;
    return true;
  } else if (m_type == Type::PseudoList()) {
    verify(false); // That would be a List node
  } else {
    return false;
  }
}

Value *Value::Lower(Type t) {
  if (m_type == t)
    return this;

  // FIXME: ought to have real compilation errors here, especiall for in-
  // appropriate enum literals being lowered to integer/float/pointer.
  verify(!t.IsKnown() || IsSubtypeOf(t));
  if (m_type == Type::PseudoInteger()) {
    // FIXME: handle bigger numbers.
    Integer *value = safe_cast<Integer *>(As<Object *>());
    uint64_t num = value->IsNegative() ? static_cast<int64_t>(*value)
                                       : static_cast<uint64_t>(*value);
    if (t == tk_float)
      return Value::NewFloat(t, double(num));
    if (t == Type::PseudoFloat())
      return Value::NewPseudo(t, new Float(num));
    if (!t.IsKnown())
      t = value->GetSizeInBits() < 32 ? Type::Int() : Type::Long();
    return Value::NewInt(t, num);
  } else if (m_type == Type::PseudoFloat()) {
    if (!t.IsKnown())
      t = Type::Double();
    return Value::NewFloat(t, AsDouble());
  } else if (m_type == Type::PseudoEnum()) {
    String *ident = safe_cast<String *>(As<Object *>());
    if (t == tk_enum)
      return Value::NewInt(t, t.OrdinalOf(ident));
    if (t == tk_pointer) {
      if (Type u = t.BaseType(); u == tk_array)
        if (!u.IndexType().IsKnown())
          t = Type::PointerTo(u.SetIndexType(0));
      return Value::New(t);  // assume .null
    }
    if (t == tk_integer || t == Type::PseudoInteger()) {
      Integer *value = ident == String::Get(wks_min) ? t.IntLowerBound()
                                                     : t.IntUpperBound();
      uint64_t num = value->IsNegative() ? static_cast<int64_t>(*value)
                                         : static_cast<uint64_t>(*value);
      // FIXME: need Integer representation of .min and .max.
      verify(t != tk_pseudo);
      return Value::NewInt(t, num);
    }
    if (t == tk_float || t == Type::PseudoFloat()) {
      double d = ident == String::Get(wks_nan) ? std::nan("") : INFINITY;
      if (t == tk_float)
        return Value::NewFloat(t, d);
      else
        return Value::NewPseudo(t, new Float(d));
    }
    verify(false);
  } else if (m_type == Type::PseudoChar()) {
    String *s = safe_cast<String *>(As<Object *>());
    verify(s->Length() == 1); // FIXME: handle utf8 encodings
    return Value::NewRaw(Type::Char8(), s->c_str(), 1);
  } else if (m_type == Type::PseudoString()) {
    String *s = safe_cast<String *>(As<Object *>());
    size_t sz = s->Length() +
        (t == tk_pointer && t.BaseType().DropQualifiers() == Type::Char8());
    Type u = Type::StringRef(sz).RValue();
    return Value::NewRaw(u, s->c_str(), sz);
  } else {
    verify(false); // FIXME
  }

  return nullptr;
}

String *Value::AsString() {
  verify(m_type.IsString());
  verify(m_type == rt_lvalueref);

  verify(m_size == sizeof(StrRef));
  StrRef &ref = As<StrRef>();
  return String::Get(ref.chars, ref.len);
}

Integer *Value::AsInteger() {
  if (m_type == Type::PseudoInteger())
    return As<Integer *>();

  verify(m_type == rt_rvalue && m_type == tk_integer);
  if (m_type.IsSigned()) {
    switch (m_size) {
      case 1: return Integer::Get(As<int8_t>());
      case 2: return Integer::Get(As<int16_t>());
      case 4: return Integer::Get(As<int32_t>());
      case 8: return Integer::Get(As<int64_t>());
      default: break;
    }
  } else {
    switch (m_size) {
      case 1: return Integer::Get(As<uint8_t>());
      case 2: return Integer::Get(As<uint16_t>());
      case 4: return Integer::Get(As<uint32_t>());
      // No case 8!
      default: break;
    }
  }

  return Integer::Get(Address(), int(m_size), m_type.IsSigned());
}

uint32_t Value::AsChar() {
  if (m_type == Type::PseudoChar()) {
    verify(m_size == sizeof(void *));
    String *s = As<String *>();
    verify(s->Length() == 1); // FIXME: compilation error?
    return (s->c_str())[0]; // FIXME: handle utf encodings
  }

  verify(m_type == rt_rvalue && m_type == tk_char);
  switch (m_size) {
    case 1: return As<uint8_t>();
    case 2: return As<uint16_t>();
    case 4: return As<uint32_t>();
    default: verify(false);
  }
  return 0;
}

intptr_t Value::AsIntPtr() {
  verify(m_type == rt_rvalue && m_type == tk_integer);
  verify(m_size <= sizeof(intptr_t));
  if (m_type.IsSigned()) {
    switch (m_size) {
      case 1: return As<int8_t>();
      case 2: return As<int16_t>();
      case 4: return As<int32_t>();
      case 8: return (intptr_t)As<int64_t>();
      default: verify(0); return 0; // FIXME???
    }
  } else {
    switch (m_size) {
      case 1: return As<uint8_t>();
      case 2: return As<uint16_t>();
      case 4: return As<uint32_t>();
      case 8: return (intptr_t)As<uint64_t>();
      default: verify(0); return 0; // FIXME???
    }
  }
}

double Value::AsDouble() {
  if (m_type == Type::PseudoFloat()) {
    verify(m_size == sizeof(void *));
    Float *f = As<Float *>();
    return *f;
  } else if (m_type == Type::Float()) {
    verify(m_size == sizeof(float));
    return As<float>();
  } else if (m_type == Type::Double()) {
    verify(m_size == sizeof(double));
    return As<double>();
  } else {
    verify(false);
  }
}

void Value::AppendToHash(SHA512 &hash, bool includeType) {
  if (includeType)
    m_type.AppendToHash(hash);

  switch (m_type.Kind()) {
    case tk_void:
      break;
    case tk_type:
      AsType().AppendToHash(hash);
      break;
    case tk_bool:
    case tk_float:
    case tk_char:
    case tk_integer:
    case tk_enum:
      verify(m_type == rt_rvalue);
      switch (m_type.StorageSize()) {
        case 1: hash.Append(*(uint8_t *)&m_data); break;
        case 2: hash.Append(*(uint16_t *)&m_data); break;
        case 4: hash.Append(*(uint32_t *)&m_data); break;
        case 8: hash.Append(*(uint64_t *)&m_data); break;
        default: verify(false);
      }
      break;
    case tk_pseudo:
      if (m_type == Type::PseudoInteger())
        As<Integer *>()->AppendToHash(hash);
      else if (m_type == Type::PseudoFloat())
        hash.Append(AsDouble());
      else if (m_type == Type::PseudoString())
        As<String *>()->AppendToHash(hash);
      else if (m_type == Type::PseudoChar())
        As<String *>()->AppendToHash(hash);
      break;
    case tk_array: {
      // FIXME: only handling strings for now
      verify(m_type.IsString());
      if (m_type == rt_lvalueref) {
        verify(m_size == sizeof(StrRef));
        StrRef &ref = As<StrRef>();
        hash.AppendString(ref.chars, ref.len);
      } else {
        hash.AppendString((char *)&m_data, m_type.StorageSize());
      }
      break;
    }
    case tk_set:
      hash.AppendString((char *)&m_data, m_type.StorageSize());
      break;
    case tk_namespace: {
      Namespace *ne = As<Namespace *>();
      hash.Append(ne->ExternalName());
      break;
    }
    case tk_class:
      verify(false); // Only literable classes allowed
      break;
    case tk_tuple:
      verify(false); // Enumerate tuple members (without member types)
      break;
    case tk_union:
      verify(false); // Union tag, followed by relevant variant
      break;
    case tk_generic: {
      auto dv = As<Derivation *>();
      hash.Append(dv->m_entity->ExternalName());
      dv->m_type.AppendToHash(hash);
      hash.Append('<');
      bool first = true;
      for (auto v : dv->m_bindings) {
        if (!first)
          hash.Append(',');
        first = false;
        if (v)
          v->AppendToHash(hash);
      }
      hash.Append('>');
      break;
    }
    case tk_codefragment:
      verify(false); // Gotta have entire node tree!
      // or is just the root address sufficient?
      break;
    default:
      verify(false);
  }
}

static bool DumpEscapedChar(BufferWriter &bw, char ch) {
  if (bw.Append('\\'))
    return true;
  return bw.Append(ch);
}

static bool DumpChar(BufferWriter &bw, char ch, char delim) {
  switch (ch) {
    case '\\': return DumpEscapedChar(bw, '\\');
    case '\r': return DumpEscapedChar(bw, 'r');
    case '\n': return DumpEscapedChar(bw, 'n');
    case '\t': return DumpEscapedChar(bw, 't');
    case '\0': return DumpEscapedChar(bw, '0');
    default: break;
  }

  if (ch == delim) {
    return DumpEscapedChar(bw, ch);
  } else if (isprint(ch)) {
    return bw.Append(ch);
  } else {
    char buf[32];
    int len = sprintf(buf, "\\x%2.2x", ch);
    return bw.Append(buf, len);
  }
}

bool Value::Dump(BufferWriter &bw) {
  if (m_type == tk_type)
    return AsType().Dump(bw);

  if (m_type == tk_bool)
    return AsBool() ? bw.Append("true", 4) : bw.Append("false", 5);

  if (m_type == tk_void)
    return false;

  if (m_type == tk_char || m_type == Type::PseudoChar()) {
    if (bw.Append('\''))
      return true;
    if (DumpChar(bw, AsChar(), '\''))
      return true;
    return bw.Append('\'');
  }

  if (m_type == tk_integer) {
    char buf[64];
    int len = 0;

    if (m_type.IsSigned()) {
      int64_t num = 0;
      switch (m_type.StorageSize()) {
        case 1: num = *(int8_t *)&m_data; break;
        case 2: num = *(int16_t *)&m_data; break;
        case 4: num = *(int32_t *)&m_data; break;
        case 8: num = *(int64_t *)&m_data; break;
      }
      len = sprintf(buf, "%lld", static_cast<long long int>(num));
    } else {
      uint64_t num = 0;
      switch (m_type.StorageSize()) {
        case 1: num = *(uint8_t *)&m_data; break;
        case 2: num = *(uint16_t *)&m_data; break;
        case 4: num = *(uint32_t *)&m_data; break;
        case 8: num = *(uint64_t *)&m_data; break;
      }
      len = sprintf(buf, "%llu", static_cast<long long unsigned>(num));
    }

    return bw.Append(buf, len);
  }

  if (m_type == tk_float) {
    char buf[64];
    double value = 0;

    if (m_type.StorageSize() == 4) {
      verify(m_type.StorageSize() == 4 && sizeof(float) == 4);
      value = *(float *)&m_data;
    } else {
      verify(m_type.StorageSize() == 8 && sizeof(double) == 8);
      value = *(double *)&m_data;
    }

    int len = sprintf(buf, "%g", value);
    return bw.Append(buf, len);
  }

  if (m_type == Type::PseudoInteger()) {
    Integer *num = AsInteger();
    char buf[64];
    int len = 0;
    if (num->IsNegative()) {
      // Ugh.  Clang and GCC cannot agree on definition of int64_t.
      int64_t v = static_cast<int64_t>(*num);
      len = sprintf(buf, "%lld", static_cast<long long int>(v));
    } else {
      uint64_t v = static_cast<uint64_t>(*num);
      len = sprintf(buf, "%llu", static_cast<long long unsigned>(v));
    }
    return bw.Append(buf, len);
  }

  if (m_type == Type::PseudoFloat()) {
    char buf[128];
    int len = sprintf(buf, "%f", AsDouble());
    return bw.Append(buf, len);
  }

  if (m_type.IsString() && m_type == rt_lvalueref) {
    StrRef &ref = As<StrRef>();
    if (bw.Append('"'))
      return true;
    for (size_t i = 0; i < ref.len; i++)
      if (DumpChar(bw, ref.chars[i], '"'))
        return true;
    return bw.Append('"');
  }

  if (m_type == Type::PseudoString()) {
    String *s = safe_cast<String *>(As<Object*>());
    if (!s)
      return bw.Append("<null>", 6);
    if (bw.Append('"'))
      return true;
    const char *p = s->c_str();
    for (size_t i = 0; i < s->Length(); i++)
      if (DumpChar(bw, p[i], '"'))
        return true;
    return bw.Append('"');
  }

  if (m_type == tk_enum) {
    size_t ord = 0;
    switch (m_type.StorageSize()) {
      case 1: ord = *(uint8_t *)&m_data; break;
      case 2: ord = *(uint16_t *)&m_data; break;
      case 4: ord = *(uint32_t *)&m_data; break;
      default: verify(false);
    }

    if (bw.Append('.'))
      return true;
    return bw.Append(m_type.IdentAt(ord));
  }

  if (m_type == Type::PseudoEnum()) {
    String *ident = safe_cast<String *>(As<Object*>());
    if (bw.Append('.'))
      return true;
    return bw.Append(ident);
  }

  if (m_type == tk_tuple) {
    // FIXME
  }

  if (m_type == tk_array && m_type == rt_rvalue &&
      m_type.ElementType().DropQualifiers() == Type::Char8()) {
    if (bw.Append('"'))
      return true;
    const char *p = (const char *)&m_data;
    for (size_t i = 0; i < m_type.StorageSize(); i++)
      DumpChar(bw, p[i], '"');
    return bw.Append('"');
  }

  if (m_type == tk_set) {
    // FIXME
  }

  if (m_type == tk_codefragment)
    return false;

  if (m_type == tk_pointer) {
    if (*(void **)&m_data == nullptr)
      return bw.Append("nullptr");
  }

  return bw.Append("<unformatable>", 14);
}
