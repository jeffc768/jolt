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

#include "Hash.h"

// Hashing function adpated from MurmurHash3 by Austin Appleby.
// https://code.google.com/p/smhasher/
void HashState::Hash(const uint8_t *p, size_t len) {
  bmix32(uint32_t(len));

  while (len >= 4) {
    uint32_t k1 = *(const uint32_t *)p;
    p += 4;
    len -= 4;
    bmix32(k1);
  }

  uint32_t k1 = 0;
  switch (len) {
    case 3: k1 ^= p[2] << 16; [[fallthrough]];
    case 2: k1 ^= p[1] << 8;  [[fallthrough]];
    case 1: k1 ^= p[0];
            pmix32(k1);
  }
}

void HashState::Hash(const uint16_t *p, size_t len) {
  bmix32(uint32_t(len) * 2);

  while (len >= 2) {
    uint32_t k1 = *(const uint32_t *)p;
    p += 2;
    len -= 2;
    bmix32(k1);
  }

  if (len > 0)
    pmix32(*p);
}

void HashState::Hash(const uint32_t *p, size_t len) {
  bmix32(uint32_t(len) * 4);

  while (len > 0) {
    bmix32(*p++);
    len--;
  }
}
