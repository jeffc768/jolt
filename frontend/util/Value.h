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

// A Value represents the result of evaluating a Jolt expression at compile
// time.

#pragma once

#include "util/Object.h"
#include "type/Type.h"
#include <type_traits>

class BufferWriter;
class Deflator;
class Inflator;
class SHA512;
class String;

class Value: public Object {
  DECLARE_OBJECT(Value)

  Type                m_type;
  uint32_t            m_size;
  uintptr_t           m_data;

protected:
  Value(Type t);
  Value(Type t, const void *addr, size_t size);

  virtual void PreDestroy();

  void DeflateFields(Deflator &DF);
  static Value *Inflate(Inflator &IF);

public:
  static Value *New(Type t);
  static Value *NewRaw(Type t, const void *bytes);
  static Value *NewRaw(Type t, const void *bytes, size_t size);
  static Value *NewPseudo(Type t, Object *obj);
  static Value *NewPtr(Type t, const void *ptr);
  static Value *NewType(Type t);
  static Value *NewBool(bool v);
  static Value *NewFloat(Type t, double value);

  template<typename T> static Value *NewInt(Type t, T value) {
    verify(t == tk_integer || t == tk_enum || t == tk_char);
    uint64_t v;
    if constexpr (std::is_signed<T>::value)
      v = static_cast<int64_t>(value);
    else
      v = static_cast<uint64_t>(value);

    Value *rv = new Value(t);
    switch (t.StorageSize()) {
      case 1: rv->As<uint8_t>() = (uint8_t)v;  break;
      case 2: rv->As<uint16_t>() = (uint16_t)v; break;
      case 4: rv->As<uint32_t>() = (uint32_t)v; break;
      case 8: rv->As<uint64_t>() = v; break;
      default: verify(false);
    }
    return rv;
  }

  Type ObjectType() { return m_type; }
  void *Address() { return &m_data; }

  int Compare(Value *other);

  uint32_t Hash();

  bool Dump(BufferWriter &bw);

  template<typename T> T &As() {
    return *reinterpret_cast<T *>(&m_data);
  }

  // Is this value a subtype of t?  Note that this isn't quite the same as
  // Type::IsSubtypeOf, as it takes the value into consideration.  Only applies
  // to psuedo literals; for any other kind of type, returns t == m_type.
  bool IsSubtypeOf(Type t);

  // Lower this value to an actual type (assume IsSubtypeOf would return true).
  Value *Lower(Type t);


  bool AsBool() {
    // FIXME: sloppy code prevents this assert: verify(m_type == tk_bool);
    return *reinterpret_cast<bool *>(&m_data);
  }

  Type AsType() {
    verify(m_type == tk_type);
    return *reinterpret_cast<Type *>(&m_data);
  }

  String *AsString();
  Integer *AsInteger();
  uint32_t AsChar();
  intptr_t AsIntPtr();
  double AsDouble();

  void AppendToHash(SHA512 &hash, bool includeType = true);
};
