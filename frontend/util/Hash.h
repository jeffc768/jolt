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

// Hashing function adpated from MurmurHash3 by Austin Appleby.
// https://code.google.com/p/smhasher/

#pragma once

#include "util/Verify.h"
#include <stdint.h>
#include <stdlib.h>

inline uint32_t rotl(uint32_t x,int8_t r) {
  return (x << r) | (x >> (32 - r));
}

struct HashState {
  uint32_t    h1;

  static const uint32_t c1 = 0xcc9e2d51;
  static const uint32_t c2 = 0x1b873593;

  HashState(uint32_t x = 0) : h1(0x971e137b + x) { }
  HashState(uint64_t x) : h1(0x971e137b + uint32_t(x)) { }

  void bmix32(uint32_t k1) {
    k1 *= c1;
    k1 = rotl(k1, 15);
    k1 *= c2;

    h1 ^= k1;
    h1 = rotl(h1, 13);
    h1 = h1 * 5 + 0xe6546b64;
  }

  uint32_t fmix32() {
    h1 ^= h1 >> 16;
    h1 *= 0x85ebca6b;
    h1 ^= h1 >> 13;
    h1 *= 0xc2b2ae35;
    h1 ^= h1 >> 16;
    return h1;
  }

  void pmix32(uint32_t k1) {
    k1 *= c1;
    k1 = rotl(k1, 16);
    k1 *= c2;
    h1 ^= k1;
  }

  void Hash(const uint8_t *p, size_t len);
  void Hash(const uint16_t *p, size_t len);
  void Hash(const uint32_t *p, size_t len);

  template<class T> void Hash(const T *p, size_t len) {
    if ((sizeof(T) & 3) == 0)
      Hash(reinterpret_cast<const uint32_t *>(p), len * (sizeof(T) / 4));
    else if ((sizeof(T) & 1) == 0)
      Hash(reinterpret_cast<const uint16_t *>(p), len * (sizeof(T) / 2));
    else
      Hash(reinterpret_cast<const uint8_t *>(p), len * sizeof(T));
  }

  template<class T> void Hash(const T &obj) {
    verify((sizeof(T) & 3) == 0);
    return Hash_<sizeof(T)/4>(reinterpret_cast<const uint32_t *>(&obj));
  }

private:
  template<unsigned LEN> void Hash_(const uint32_t *p) {
    unsigned len = LEN;

    while (len > 0) {
      bmix32(*p++);
      len--;
    }
  }
};

inline uint32_t Hash(const uint8_t *p, size_t len) {
  HashState hs;
  hs.Hash(p, len);
  return hs.fmix32();
}

inline uint32_t Hash(const uint16_t *p, size_t len) {
  HashState hs;
  hs.Hash(p, len);
  return hs.fmix32();
}

inline uint32_t Hash(const uint32_t *p, size_t len) {
  HashState hs;
  hs.Hash(p, len);
  return hs.fmix32();
}

template<class T> inline uint32_t Hash(const T *p, size_t len) {
  HashState hs;
  hs.Hash(p, len);
  return hs.fmix32();
}

// While this can and is used to hash multi-field objects, beware that this is
// correct only if field alignment doesn't create unused space between two
// fields!
template<class T> inline uint32_t Hash(const T &obj) {
  HashState hs;
  hs.Hash(obj);
  return hs.fmix32();
}
