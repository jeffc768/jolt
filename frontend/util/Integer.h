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

// An Integer represents an integer value of unlimited precision.  Integer
// objects are unique; two pointers to Integers are equal if and only if they
// refer to the exact same numeric value.

#pragma once

#include "Object.h"
#include <cstdint>
#ifndef NDEBUG
#include <string>
#endif

class SHA512;

enum WellKnownInteger {
  wki_zero,
  wki_one,
  wki_two,
  wki_ten,
  wki_2e8,
  wki_2e16,
  wki_2e32,
  wki_2e64,
  wki_int8_min,
  wki_int8_max,
  wki_uint8_max,
  wki_int16_min,
  wki_int16_max,
  wki_uint16_max,
  wki_int32_min,
  wki_int32_max,
  wki_uint32_max,
  wki_int64_min,
  wki_int64_max,
  wki_uint64_max,
  wki_max_wki
};

// Template magic to determine the correct-sized integer in which to perform
// arbitrary precision arithmetic.  The chunk size is half the smaller of either
// the pointer size or int size.  Missing combinations will fail to compile.
template<size_t PT, size_t IT> struct IntegerChunk_ { };

template<> struct IntegerChunk_<4, 4> {
  using Type   = uint16_t;
  using SType  = int16_t;
  using DType  = uint32_t;
  using DSType = int32_t;

  static int GetInt(int size, Type *v) {
    if (size == 1)
      return SType(v[0]);
    verify(size == 2);
    return (v[1] << 16) | v[0];
  }

  static int64_t GetInt64(int size, Type *v) {
    if (size == 1)
      return SType(v[0]);
    if (size == 2)
      return DSType((v[1] << 16) | v[0]);
    uint32_t lower = (v[1] << 16) | v[0];
    int64_t full = (uint64_t(v[2]) << 32) | lower;
    if (size == 3)
      return (full << 16) >> 16;
    verify(size == 4);
    return (uint64_t(v[3]) << 48) | full;
  }

  static uint64_t GetUInt64(int size, Type *v) {
    if (size == 1)
      return v[0];
    if (size == 2)
      return (v[1] << 16) | v[0];
    uint32_t lower = (v[1] << 16) | v[0];
    uint64_t full = (uint64_t(v[2]) << 32) | lower;
    if (size == 3)
      return full;
    verify(size == 4);
    return (uint64_t(v[3]) << 48) | full;
  }

  static intptr_t GetIntPtr(int size, Type *v) {
    // FIXME: this won't work on systems with 64-bit pointers but size_t is
    // 32-bits.  Jolt is unlikely to see such systems for a long, long time...
    verify(sizeof(intptr_t) == 4);
    if (size == 1)
      return SType(v[0]);
    verify(size == 2);
    return (v[1] << 16) | v[0];
  }

  static int SetInt(Type *v, int value) {
    if (static_cast<SType>(static_cast<Type>(value)) == value) {
      v[0] = Type(value);
      return 1;
    } else {
      v[0] = Type(value);
      v[1] = Type(value >> 16);
      return 2;
    }
  }

  static int SetInt64(Type *v, int64_t value) {
    if (static_cast<SType>(static_cast<Type>(value)) == value) {
      v[0] = Type(value);
      return 1;
    } else if (static_cast<DSType>(static_cast<DType>(value)) == value) {
      v[0] = Type(value);
      v[1] = Type(value >> 16);
      return 2;
    } else if (((value << 16) >> 16) == value) {
      v[0] = Type(value);
      v[1] = Type(value >> 16);
      v[2] = Type(value >> 32);
      return 3;
    } else {
      v[0] = Type(value);
      v[1] = Type(value >> 16);
      v[2] = Type(value >> 32);
      v[3] = Type(value >> 48);
      return 4;
    }
  }

  static int SetIntPtr(Type *v, intptr_t value) {
    // FIXME: this won't work on systems with 64-bit pointers but size_t is
    // 32-bits.  Jolt is unlikely to see such systems for a long, long time...
    verify(sizeof(intptr_t) == 4);
    return SetInt(v, int(value));
  }
};

template<> struct IntegerChunk_<8, 4> {
  using Type   = uint32_t;
  using SType  = int32_t;
  using DType  = uint64_t;
  using DSType = int64_t;

  static int GetInt(int size, Type *v) {
    verify(size == 1);
    return *v;
  }

  static int64_t GetInt64(int size, Type *v) {
    if (size == 1)
      return SType(v[0]);
    verify(size == 2);
    return (int64_t(v[1]) << 32) | v[0];
  }

  static uint64_t GetUInt64(int size, Type *v) {
    if (size == 1)
      return v[0];
    verify(size == 2);
    return (uint64_t(v[1]) << 32) | v[0];
  }

  static intptr_t GetIntPtr(int size, Type *v) {
    if (size == 1)
      return SType(v[0]);

    // The shift amount of 32 is obfuscated so that certain compilers do not
    // complain about an excessive shift amount on 32-bit builds.  This code
    // is unused on such builds anyway.
    verify(size == 2);
    return (intptr_t(v[1]) << (sizeof(intptr_t) * 4)) | v[0];
  }

  static int SetInt(Type *v, int value) {
    v[0] = value;
    return 1;
  }

  static int SetInt64(Type *v, int64_t value) {
    if (static_cast<SType>(static_cast<Type>(value)) == value) {
      v[0] = Type(value);
      return 1;
    } else {
      v[0] = Type(value);
      v[1] = Type(value >> 32);
      return 2;
    }
  }

  static int SetIntPtr(Type *v, intptr_t value) {
    verify(sizeof(intptr_t) == 8);
    return SetInt64(v, value);
  }
};

struct IntegerChunk : public IntegerChunk_<sizeof(size_t), sizeof(int)> { };

class Integer: public Object {
  // We need a custom version of this, unfortunately, so that the destructor
  // is accessible to HashTable<String>.
  //DECLARE_OBJECT(Integer)
  friend Metadata;

public:
  // size_t is better than uintptr_t, for if size_t is smaller than uintptr_t,
  // the machine probably cannot do arithmetic efficiently in uintptr_t sizes.
  // Not that the Jolt compiler is ever likely to run on such a machine...
  using Chunk   = IntegerChunk::Type;
  using SChunk  = IntegerChunk::SType;
  using DChunk  = IntegerChunk::DType;
  using DSChunk = IntegerChunk::DSType;
  static const int MINCHUNKS = sizeof(int) / sizeof(Chunk);

private:
  // Chunks are stored with the least significant chunk at index 0.
  int                 m_size;
  Chunk               m_value[MINCHUNKS];

protected:
  void DeflateFields(Deflator &DF);
  static Integer *Inflate(Inflator &IF);

  virtual size_t GetObjectSize();
  Object *GetObjectBase() { return this; }

  Integer() : m_size(1) { m_value[0] = 0; }
  Integer(Chunk value) : m_size(1) { m_value[0] = value; }
  Integer(const Chunk *value, int size);

  static Integer *BuildInteger(const Chunk *v, int size);

public:
  using base_t = Object;
  static Metadata s_metadata;
  virtual Metadata &GetMetadata();

  struct HashTrait;
  friend struct HashTrait;

  static Integer *Get() { return Get(wki_zero); }

  static Integer *Get(WellKnownInteger wki);

  template<typename T> static Integer *Get(T value) {
    Chunk c[MINCHUNKS + sizeof(T)/sizeof(Chunk)];
    int len = IntegerChunk::SetInt64(c, value);
    return BuildInteger(c, len);
  }

  // Note: size must be a multiple of Chunk.
  static Integer *Get(const void *data, int size, bool isSigned);

  // Returns the value as an ordinary int or intptr_t.  Caller promises value
  // will actually fit.
  operator int() { return IntegerChunk::GetInt(m_size, m_value); }
  operator int64_t() { return IntegerChunk::GetInt64(m_size, m_value); }
  operator uint64_t() { return IntegerChunk::GetUInt64(m_size, m_value); }
  intptr_t AsIntPtr() { return IntegerChunk::GetIntPtr(m_size, m_value); }

  // Get number of bits it takes to hold the actual value (excluding sign bit).
  unsigned GetSizeInBits();

  bool IsNegative() { return (m_value[m_size-1] >> (sizeof(Chunk)*8-1)) & 1; }
  bool IsZero() { return m_size == 1 && m_value[0] == 0; }

  // Perform arbitrary precision arithmetic.  Operands are not modified.
  Integer *Add(Integer *rhs);
  Integer *Sub(Integer *rhs);
  Integer *Mul(Integer *rhs);
  Integer *Div(Integer *rhs);
  Integer *Neg();

  // Perform comparisons.
  bool Eq(Integer *rhs);
  bool Lt(Integer *rhs);
  bool Ne(Integer *rhs) { return !Eq(rhs); }
  bool Le(Integer *rhs) { return !rhs->Lt(this); }
  bool Gt(Integer *rhs) { return rhs->Lt(this); }
  bool Ge(Integer *rhs) { return !Lt(rhs); }

  // Miscellaneous operations.
  Integer *Min(Integer *rhs) { return this->Lt(rhs) ? this : rhs; }
  Integer *Max(Integer *rhs) { return this->Gt(rhs) ? this : rhs; }

  void AppendToHash(SHA512 &hash);

#ifndef NDEBUG
  std::string AsString();
#endif
};
