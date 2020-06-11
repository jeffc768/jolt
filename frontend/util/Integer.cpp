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

#include "Integer.h"
#include "Deduplicator.h"
#include "InDeflator.h"
#include "SHA512.h"

// Arbitrary precision arithmetic algorithms based on Hacker's Delight by
// Henry S. Warren, Jr., which is itself based on Knuth.

// We need a custom version of this, unfortunately.
//IMPLEMENT_OBJECT_NOSIZE(Integer)

using Chunk  = Integer::Chunk;
using SChunk = Integer::SChunk;
using DChunk = Integer::DChunk;

static const int CHUNKSIZE = sizeof(Chunk) * 8;
static const int CHUNKSEXT = CHUNKSIZE - 1;

static inline int nlz(Chunk v) {
  verify(v != 0);
  int i = __builtin_clz(v);
  return i - (sizeof(unsigned int) - sizeof(Chunk)) * 8;
}

struct Integer::HashTrait {
  using ELEM = Integer::Chunk;
  using SIZE = int;

  static const int base = Integer::MINCHUNKS;

  static void New(Integer &data, const ELEM *elem, int size) {
    new(&data) Integer(elem, size);
  }

  static int Size(Integer &data) { return data.m_size; }
  static const ELEM *Data(Integer &data) { return data.m_value; }
};

static Deduplicator<Integer> g_Integers;

Metadata Integer::s_metadata { &Object::s_metadata };

Metadata &Integer::GetMetadata() {
  return s_metadata;
}

void Integer::DeflateFields(Deflator &DF) {
  DF << m_size;
  DF << std::make_pair(&m_value[0], m_size);
}

Integer *Integer::Inflate(Inflator &IF) {
  int size;
  IF >> size;

  // FIXME: use dynamically-sized array once Microsoft gets its C++14 act
  // together.
  vector<Chunk> value(size);
  IF >> std::make_pair(value.data(), size);

  Integer *obj = BuildInteger(value.data(), size);
  IF.RegisterObject(obj);
  return obj;
}

Integer::Integer(const Chunk *value, int size) : Object(), m_size(size) {
  memcpy(m_value, value, size * sizeof(Chunk));
}

size_t Integer::GetObjectSize() {
  return sizeof(Integer) + (m_size - MINCHUNKS) * sizeof(Chunk);
}

Integer *Integer::Get(const void *data, int size, bool isSigned) {
  verify((size % sizeof(Chunk)) == 0);

  // The easy case:
  if (isSigned)
    return BuildInteger(reinterpret_cast<const Chunk *>(data),
                        size / sizeof(Chunk));

  // Next easiest: an unsigned value with a leading zero bit.
  if (*reinterpret_cast<const uint8_t *>(data) < 0x80)
    return BuildInteger(reinterpret_cast<const Chunk *>(data),
                        size / sizeof(Chunk));

  // Hardest: allocate a copy with an extra leading zero chunk.
  Chunk *copy = (Chunk *)alloca(size + sizeof(Chunk));
  *copy = 0;
  memcpy(copy + 1, data, size);
  return BuildInteger(copy, (size / sizeof(Chunk)) + 1);
}

unsigned Integer::GetSizeInBits() {
  Chunk msc = m_value[m_size - 1];
  Chunk sign = SChunk(msc) >> CHUNKSEXT;
  msc ^= sign;
  return CHUNKSIZE * m_size - (msc ? nlz(msc) : CHUNKSIZE);
}

Integer *Integer::BuildInteger(const Chunk *v, int size) {
  // Find the true size of the result.
  Chunk sign = SChunk(v[size-1]) >> CHUNKSEXT;

  if (size > 1 && v[size-1] == sign) {
    // Drop chunks that are nothing but extended signs.
    do {
      size--;
    } while (size > 1 && v[size-1] == sign);

    // But keep one of them if the sign of the value otherwise changes.
    if ((sign ^ v[size-1]) >> CHUNKSEXT)
      size++;
  }

  // Construct and return a new Integer object.
  return g_Integers.FindOrAdd(v, size);
}

Integer *Integer::Add(Integer *rhs) {
  // Allocate big enough buffer guaranteed to hold the result.
  int newsize = m_size > rhs->m_size ? m_size + 1 : rhs->m_size + 1;
  Chunk *v = reinterpret_cast<Chunk *>(alloca(newsize * sizeof(Chunk)));

  // The number of chunks to add is the smaller of the two values.
  int n = m_size > rhs->m_size ? rhs->m_size : m_size;

  // Start with no carry.
  DChunk carry = 0;

  // Add chunks.
  for (int i = 0; i < n; i++) {
    carry += DChunk(m_value[i]) + DChunk(rhs->m_value[i]);
    v[i] = Chunk(carry);
    carry >>= CHUNKSIZE;
  }

  // If values were of different lengths, we got more work to do.
  if (m_size != rhs->m_size) {
    Chunk *w = (m_size > rhs->m_size) ? m_value : rhs->m_value;
    Chunk sign = (m_size > rhs->m_size) ? rhs->m_value[rhs->m_size - 1]
                                        : m_value[m_size - 1];
    sign = SChunk(sign) >> CHUNKSEXT;
    for (int i = n; i < newsize - 1; i++) {
      carry += DChunk(w[i]) + DChunk(sign);
      v[i] = Chunk(carry);
      carry >>= CHUNKSIZE;
    }
  }

  // Finally, handle overflow into the most significant chunk.
  Chunk ov = (v[newsize - 2] ^ m_value[m_size - 1]) &
             (v[newsize - 2] ^ rhs->m_value[rhs->m_size - 1]);
  if (ov >> CHUNKSEXT)
    v[newsize - 1] = SChunk(~v[newsize - 2]) >> CHUNKSEXT;
  else
    newsize--;

  return BuildInteger(v, newsize);
}

Integer *Integer::Sub(Integer *rhs) {
  // Allocate big enough buffer guaranteed to hold the result.
  int newsize = m_size > rhs->m_size ? m_size + 1 : rhs->m_size + 1;
  Chunk *v = reinterpret_cast<Chunk *>(alloca(newsize * sizeof(Chunk)));

  // The number of chunks to add is the smaller of the two values.
  int n = m_size > rhs->m_size ? rhs->m_size : m_size;

  // Start with no borrow.
  DChunk borrow = 0;

  // Add chunks.
  for (int i = 0; i < n; i++) {
    DChunk t = DChunk(m_value[i]) - DChunk(rhs->m_value[i]) - borrow;
    v[i] = Chunk(t);
    borrow = t >> (sizeof(DChunk) * 8 - 1);
  }

  // If values were of different lengths, we got more work to do.
  if (m_size != rhs->m_size) {
    Chunk *w = (m_size > rhs->m_size) ? m_value : rhs->m_value;
    Chunk sign = (m_size > rhs->m_size) ? rhs->m_value[rhs->m_size - 1]
                                        : m_value[m_size - 1];
    sign = SChunk(sign) >> CHUNKSEXT;
    for (int i = n; i < newsize - 1; i++) {
      DChunk t = DChunk(w[i]) - DChunk(sign) - borrow;
      v[i] = Chunk(t);
      borrow = t >> (sizeof(DChunk) * 8 - 1);
    }
  }

  // Finally, handle overflow into the most significant chunk.
  Chunk ov = (v[newsize - 2] ^ m_value[m_size - 1]) &
             (m_value[m_size - 1] ^ rhs->m_value[rhs->m_size - 1]);
  if (ov >> CHUNKSEXT)
    v[newsize - 1] = SChunk(~v[newsize - 2]) >> CHUNKSEXT;
  else
    newsize--;

  return BuildInteger(v, newsize);
}

Integer *Integer::Mul(Integer *rhs) {
  // Allocate big enough buffer guaranteed to hold the result.
  int newsize = m_size + rhs->m_size;
  Chunk *v = reinterpret_cast<Chunk *>(alloca(newsize * sizeof(Chunk)));

  // Initialize result holder (up to length of lhs).
  memset(v, 0, m_size * sizeof(Chunk));

  // Multiply and sum chunks.
  for (int i = 0; i < rhs->m_size; i++) {
    DChunk k = 0;
    for (int j = 0; j < m_size; j++) {
      DChunk t = DChunk(m_value[j]) * DChunk(rhs->m_value[i]) +
                 DChunk(v[i + j]) + k;
      v[i + j] = Chunk(t);
      k = t >> CHUNKSIZE;
    }
    v[i + m_size] = Chunk(k);
  }

  // The above computed an unsigned product.  We want a signed product, so we
  // must apply corrective factors for each negative operand.
  if (SChunk(m_value[m_size - 1]) < 0) {
    DChunk borrow = 0;
    for (int i = 0; i < rhs->m_size; i++) {
      DChunk t = DChunk(v[i + m_size]) - DChunk(rhs->m_value[i]) - borrow;
      v[i + m_size] = Chunk(t);
      borrow = t >> (sizeof(DChunk) * 8 - 1);
    }
  }

  if (SChunk(rhs->m_value[rhs->m_size - 1]) < 0) {
    DChunk borrow = 0;
    for (int i = 0; i < m_size; i++) {
      DChunk t = DChunk(v[i + rhs->m_size]) - DChunk(m_value[i]) - borrow;
      v[i + rhs->m_size] = Chunk(t);
      borrow = t >> (sizeof(DChunk) * 8 - 1);
    }
  }

  return BuildInteger(v, newsize);
}

// Helper function to negate.  Source and destination may be the same.  Assumes
// that the last chunk has no value on entry; it is used only if overflow
// occurs.
static inline void Negate(Chunk *d, Chunk *s, int &len) {
  // Start with no borrow.
  DChunk borrow = 0;

  // Must remember the most significant chunk for overflow detection (in case
  // s == d).
  Chunk msc = s[len - 1];

  // Negate chunks.
  int m = len;
  for (int i = 0; i < m; i++) {
    DChunk t = -DChunk(s[i]) - borrow;
    d[i] = Chunk(t);
    borrow = t >> (sizeof(DChunk) * 8 - 1);
  }

  // Finally, handle overflow into the most significant chunk.  Note that only
  // the negation of a negative number can overflow.
  Chunk ov = d[m - 1] & msc;
  if (ov >> CHUNKSEXT)
    d[len++] = 0;
}

Integer *Integer::Div(Integer *rhs) {
  // Copy the dividend, as we need a mutable copy.  If it's negative, make it
  // positive and remember it was negative.  Note that negation might cause the
  // value to grow by one chunk.  It will grow by yet another chunk after
  // normalization.
  int m = m_size;
  Chunk *u = reinterpret_cast<Chunk *>(alloca((m + 2) * sizeof(Chunk)));
  bool neg_dividend = SChunk(m_value[m - 1]) < 0;
  if (neg_dividend)
    Negate(u, m_value, m);
  else
    memcpy(u, m_value, m * sizeof(Chunk));

  // The divisor must be positive.  If it isn't, make it so.
  int n = rhs->m_size;
  Chunk *v = reinterpret_cast<Chunk *>(alloca((n + 1) * sizeof(Chunk)));
  bool neg_divisor = SChunk(rhs->m_value[n - 1]) < 0;
  if (neg_divisor)
    Negate(v, rhs->m_value, n);
  else
    memcpy(v, rhs->m_value, n * sizeof(Chunk));

  // The algorithm requires that the divisor have fewer chunks than the
  // dividend.  As if this is the case the quotient is zero, simply return
  // zero.
  if (m < n)
    return Integer::Get(wki_zero);

  // A divisor of zero is a no-no.
  // FIXME: implement positive and negative infinity.
  verify(n > 1 || v[0] != 0);

  const DChunk base = DChunk(1) << CHUNKSIZE;

  // Allocate big enough buffer guaranteed to hold the quotient.
  int newsize = m - n + 1;
  Chunk *q = reinterpret_cast<Chunk *>(alloca(newsize * sizeof(Chunk)));

  // Handle single chunk divisors here.  Note that this is not simply an
  // optimization; the multi-chunk divisor code below cannot handle a single-
  // chunk divisor.
  if (n == 1) {
    DSChunk k = 0;
    for (int i = m - 1; i >= 0; i--) {
      q[i] = Chunk((k * base + u[i]) / v[0]);
      k = (k * base + u[i]) - DChunk(q[i]) * v[0];
    }
    if (neg_divisor ^ neg_dividend)
      Negate(q, q, newsize);
    return BuildInteger(q, newsize);
  }

  // Normalize the divisor so that the most significant bit of the most
  // significant chunk is one.
  int s = nlz(v[n-1]);
  if (s > 0) {
    for (int i = n - 1; i > 0; i--)
      v[i] = Chunk((v[i] << s) | (v[i - 1] >> (CHUNKSIZE - s)));
    v[0] = v[0] << s;

    // The dividend must be shifted the same amount.  An extra chunk is added
    // to hold the overflow, regardless of it actually being needed.
    u[m] = Chunk(u[m - 1] >> (CHUNKSIZE - s));
    for (int i = m - 1; i > 0; i--)
      u[i] = Chunk((u[i] << s) | (u[i - 1] >> (CHUNKSIZE - s)));
    u[0] = u[0] << s;
  } else {
    u[m] = 0;
  }

  // The Main Loop.
  for (int i = m - n; i >= 0; i--) {
    // Compute estimate qhat of q[i].
    DChunk qhat = (u[i + n] * base + u[i + n - 1]) / v[n - 1];
    DChunk rhat = (u[i + n] * base + u[i + n - 1]) - qhat * v[n - 1];
    while (qhat >= base || qhat * v[n - 2] > base * rhat + u[i + n - 2]) {
      qhat--;
      rhat += v[n - 1];
      if (rhat >= base)
        break;
    }

    // Multiple and subtract.
    DSChunk k = 0;
    for (int j = 0; j < n; j++) {
      DChunk p = qhat * v[j];
      DSChunk t = u[i + j] - k - (p & (base - 1));
      u[i + j] = Chunk(t);
      k = (p >> CHUNKSIZE) - (t >> CHUNKSIZE);
    }
    DSChunk t = u[i + n] - k;
    u[i + n] = Chunk(t);

    // Store quotient chunk.  If we subtracted too much, add back.
    q[i] = Chunk(qhat);
    if (t < 0) {
      q[i]--;
      k = 0;
      for (int j = 0; j < n; j++) {
        t = k + u[i + j] + v[j];
        u[i + j] = Chunk(t);
        k = t >> CHUNKSIZE;
      }
      u[i + n] += Chunk(k);
    }
  }

  // Negate quotient if either dividend or divisor (but not both) were negative.
  if (neg_divisor ^ neg_dividend)
    Negate(q, q, newsize);
  return BuildInteger(q, newsize);
}

Integer *Integer::Neg() {
  int newsize = m_size;
  Chunk *v = reinterpret_cast<Chunk *>(alloca((newsize + 1) * sizeof(Chunk)));
  Negate(v, m_value, newsize);
  return BuildInteger(v, newsize);
}

bool Integer::Eq(Integer *rhs) {
  if (m_size != rhs->m_size)
    return false;
  return memcmp(m_value, rhs->m_value, m_size * sizeof(Chunk)) == 0;
}

bool Integer::Lt(Integer *rhs) {
  // First quick check:  operands have different signs.
  bool lhsNeg = IsNegative();
  bool rhsNeg = rhs->IsNegative();
  if (lhsNeg != rhsNeg)
    return lhsNeg;

  // Second quick check:  operands have obviously different magnitudes.
  if (m_size != rhs->m_size)
    return m_size < rhs->m_size ? !lhsNeg : lhsNeg;

  // Do subtraction, but we only care about the sign of the result.
  DChunk borrow = 0;
  SChunk r = 0;
  for (int i = 0; i < m_size; i++) {
    DChunk t = DChunk(m_value[i]) - DChunk(rhs->m_value[i]) - borrow;
    r = SChunk(t);
    borrow = t >> (sizeof(DChunk) * 8 - 1);
  }

  // Handle overflow.
  Chunk ov = (r ^ m_value[m_size - 1]) &
             (m_value[m_size - 1] ^ rhs->m_value[m_size - 1]);
  if (ov >> CHUNKSEXT)
    r = (~r) >> CHUNKSEXT;

  // Simply check the sign.
  return r < 0;
}

std::string Integer::AsString() {
  // Not efficient, but only used (not that much) in debug builds.
  if (IsNegative())
    return '-' + Neg()->AsString();

  Integer *ten = Integer::Get(wki_ten);
  Integer *num = this;
  std::string rv;
  while (!num->IsZero()) {
    Integer *q = num->Div(ten);
    Integer *r = num->Sub(q->Mul(ten));
    rv = char(int(*r) + '0') + rv;
    num = q;
  }

  if (rv.empty())
    rv = "0";
  return rv;
}

static Integer *g_wki[wki_max_wki];

Integer *Integer::Get(WellKnownInteger wki) {
  if (g_wki[wki])
    return g_wki[wki];

  Integer *i = nullptr;
  switch (wki) {
    case wki_zero:
      i = Integer::Get(0);
      break;
    case wki_one:
      i = Integer::Get(1);
      break;
    case wki_two:
      i = Integer::Get(2);
      break;
    case wki_ten:
      i = Integer::Get(10);
      break;
    case wki_2e8:
      i = Integer::Get(256);
      break;
    case wki_2e16:
      i = Get(wki_2e8)->Mul(Get(wki_2e8));
      break;
    case wki_2e32:
      i = Get(wki_2e16)->Mul(Get(wki_2e16));
      break;
    case wki_2e64:
      i = Get(wki_2e32)->Mul(Get(wki_2e32));
      break;
    case wki_int8_min:
      i = Integer::Get(-128);
      break;
    case wki_int8_max:
      i = Integer::Get(127);
      break;
    case wki_uint8_max:
      i = Integer::Get(255);
      break;
    case wki_int16_min:
      i = Integer::Get(-32768);
      break;
    case wki_int16_max:
      i = Integer::Get(32767);
      break;
    case wki_uint16_max:
      i = Integer::Get(65535);
      break;
    case wki_int32_min:
      i = Get(wki_2e16)->Mul(Get(wki_int16_min));
      break;
    case wki_int32_max:
      i = Get(wki_uint32_max)->Div(Get(wki_two));
      break;
    case wki_uint32_max:
      i = Get(wki_2e32)->Sub(Get(wki_one));
      break;
    case wki_int64_min:
      i = Get(wki_2e32)->Mul(Get(wki_int32_min));
      break;
    case wki_int64_max:
      i = Get(wki_uint64_max)->Div(Get(wki_two));
      break;
    case wki_uint64_max:
      i = Get(wki_2e64)->Sub(Get(wki_one));
      break;
    case wki_max_wki:
      verify(false);
  }

  g_wki[wki] = i;
  return g_wki[wki];
}

void Integer::AppendToHash(SHA512 &hash) {
  unsigned bitcnt = GetSizeInBits();
  Chunk *p = m_value;

  hash.Append(bitcnt);

  while (bitcnt > sizeof(Chunk) * 8) {
    hash.Append(*p++);
    bitcnt -= sizeof(Chunk) * 8;
  }

  hash.Append(*p);
}
