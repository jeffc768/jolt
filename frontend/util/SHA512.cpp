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

#include "SHA512.h"

#include <algorithm>
#include <cstring>

static SHA512Hash const H = { {
  0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1,
  0x510e527fade682d1, 0x9b05688c2b3e6c1f, 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179
} };

static uint64_t const K[80] = {
  0x428a2f98d728ae22, 0x7137449123ef65cd, 0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc,
  0x3956c25bf348b538, 0x59f111f1b605d019, 0x923f82a4af194f9b, 0xab1c5ed5da6d8118,
  0xd807aa98a3030242, 0x12835b0145706fbe, 0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2,
  0x72be5d74f27b896f, 0x80deb1fe3b1696b1, 0x9bdc06a725c71235, 0xc19bf174cf692694,
  0xe49b69c19ef14ad2, 0xefbe4786384f25e3, 0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65,
  0x2de92c6f592b0275, 0x4a7484aa6ea6e483, 0x5cb0a9dcbd41fbd4, 0x76f988da831153b5,
  0x983e5152ee66dfab, 0xa831c66d2db43210, 0xb00327c898fb213f, 0xbf597fc7beef0ee4,
  0xc6e00bf33da88fc2, 0xd5a79147930aa725, 0x06ca6351e003826f, 0x142929670a0e6e70,
  0x27b70a8546d22ffc, 0x2e1b21385c26c926, 0x4d2c6dfc5ac42aed, 0x53380d139d95b3df,
  0x650a73548baf63de, 0x766a0abb3c77b2a8, 0x81c2c92e47edaee6, 0x92722c851482353b,
  0xa2bfe8a14cf10364, 0xa81a664bbc423001, 0xc24b8b70d0f89791, 0xc76c51a30654be30,
  0xd192e819d6ef5218, 0xd69906245565a910, 0xf40e35855771202a, 0x106aa07032bbd1b8,
  0x19a4c116b8d2d0c8, 0x1e376c085141ab53, 0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8,
  0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb, 0x5b9cca4f7763e373, 0x682e6ff3d6b2b8a3,
  0x748f82ee5defb2fc, 0x78a5636f43172f60, 0x84c87814a1f0ab72, 0x8cc702081a6439ec,
  0x90befffa23631e28, 0xa4506cebde82bde9, 0xbef9a3f7b2c67915, 0xc67178f2e372532b,
  0xca273eceea26619c, 0xd186b8c721c0c207, 0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178,
  0x06f067aa72176fba, 0x0a637dc5a2c898a6, 0x113f9804bef90dae, 0x1b710b35131c471b,
  0x28db77f523047d84, 0x32caab7b40c72493, 0x3c9ebe0a15c9bebc, 0x431d67c49c100d4c,
  0x4cc5d4becb3e42b6, 0x597f299cfc657e2a, 0x5fcb6fab3ad6faec, 0x6c44198c4a475817
};

static inline uint64_t S(uint64_t x, unsigned amt) {
  return (x >> amt) | (x << (64 - amt));
}

static inline uint64_t Ch(uint64_t x, uint64_t y, uint64_t z) {
  return (x & y) ^ (~x & z);
}

static inline uint64_t Maj(uint64_t x, uint64_t y, uint64_t z) {
  return (x & y) ^ (x & z) ^ (y & z);
}

static inline uint64_t Sigma0(uint64_t x) {
  return S(x, 28) ^ S(x, 34) ^ S(x, 39);
}

static inline uint64_t Sigma1(uint64_t x) {
  return S(x, 14) ^ S(x, 18) ^ S(x, 41);
}

static inline uint64_t sigma0(uint64_t x) {
  return S(x, 1) ^ S(x, 8) ^ (x >> 7);
}

static inline uint64_t sigma1(uint64_t x) {
  return S(x, 19) ^ S(x, 61) ^ (x >> 6);
}

SHA512::SHA512() : m_index(0), m_total(0), m_hash(H) { }

void SHA512::Append(void const *p, size_t len) {
  while (len > 0) {
    size_t amt = std::min(len, size_t(128 - m_index));
    memcpy(m_message + m_index, p, amt);
    m_index += amt;
    len -= amt;

    if (m_index == 128)
      DoRounds();
  }
}

void SHA512::AppendString(const char *p, size_t len) {
  while (len >= 255) {
    Append((uint8_t)255);
    Append(p, 255);
    p += 255;
    len -= 255;
  }

  Append((uint8_t)len);
  Append(p, len);
}

void SHA512::Append(uint64_t v) {
  uint8_t *p = (uint8_t*)&v;
  if (m_index < 128 - 8) {
    for (int i = 0; i < 8; i++)
      m_message[m_index++] = *p++;
  } else {
    for (int i = 0; i < 8; i++)
      Append(*p++);
  }
}

void SHA512::Append(uint32_t v) {
  uint8_t *p = (uint8_t*)&v;
  if (m_index < 128 - 4) {
    for (int i = 0; i < 4; i++)
      m_message[m_index++] = *p++;
  } else {
    for (int i = 0; i < 4; i++)
      Append(*p++);
  }
}

void SHA512::Append(uint16_t v) {
  uint8_t *p = (uint8_t*)&v;
  if (m_index < 128 - 2) {
    m_message[m_index++] = *p++;
    m_message[m_index++] = *p;
  } else {
    Append(*p++);
    Append(*p);
  }
}

SHA512Hash &SHA512::GetHash() {
  // Finalize length of hashed data.
  m_total += m_index;

  // Append a "1" bit to the end.
  m_message[m_index++] = 0x80;
  if (m_index == 128)
    DoRounds();

  // We need to append 128 bits, which contain the message length in bits.
  // Make sure there's enough room in the current block.
  if (m_index >= 128 - 16) {
    // Pad with zeroes and start new block.
    memset(m_message + m_index, 0, 128 - m_index);
    DoRounds();
  }

  // Pad block until 8 bytes remain.  (Let's not kid ourselves; the length
  // will not exceed the capacity of 64 bits!)
  memset(m_message + m_index, 0, 128 - m_index - 8);

  // Put the length as the final eight bytes.
  uint64_t num = m_total * 8;
  for (int i = 0; i < 8; i++)
    m_message[127 - i] = (num >> (i * 8)) & 0xff;

  // One more set of rounds to go!
  DoRounds();

  // And return the hash.
  return m_hash;
}

void SHA512::DoRounds() {
  // Compute the expanded message block.
  uint64_t W[80];
  uint8_t *p = m_message;
  for (int i = 0; i < 16; i++) {
    // Don't worry; compilers are smart enough to replace all this with a
    // single bswap instruction (and completely unroll the loop, too).
    uint64_t a = p[0];
    uint64_t b = p[1];
    uint64_t c = p[2];
    uint64_t d = p[3];
    uint64_t e = p[4];
    uint64_t f = p[5];
    uint64_t g = p[6];
    uint64_t h = p[7];
    p += 8;
    W[i] = (a << 56) | (b << 48) | (c << 40) | (d << 32) |
           (e << 24) | (f << 16) | (g << 8) | h;
  }
  for (int i = 16; i < 80; i++)
    W[i] = sigma1(W[i-2]) + W[i-7] + sigma0(W[i-15]) + W[i-16];

  // Prepare the registers used by the rounds.
  uint64_t a = m_hash.m_data[0];
  uint64_t b = m_hash.m_data[1];
  uint64_t c = m_hash.m_data[2];
  uint64_t d = m_hash.m_data[3];
  uint64_t e = m_hash.m_data[4];
  uint64_t f = m_hash.m_data[5];
  uint64_t g = m_hash.m_data[6];
  uint64_t h = m_hash.m_data[7];

  // Do 80 rounds.
  for (int j = 0; j < 80; j++) {
    uint64_t t1 = h + Sigma1(e) + Ch(e,f,g) + K[j] + W[j];
    uint64_t t2 = Sigma0(a) + Maj(a,b,c);
    h = g;
    g = f;
    f = e;
    e = d + t1;
    d = c;
    c = b;
    b = a;
    a = t1 + t2;
  }

  // Compute next intermediate hash.
  m_hash.m_data[0] += a;
  m_hash.m_data[1] += b;
  m_hash.m_data[2] += c;
  m_hash.m_data[3] += d;
  m_hash.m_data[4] += e;
  m_hash.m_data[5] += f;
  m_hash.m_data[6] += g;
  m_hash.m_data[7] += h;

  // Reset the message block for more data.
  m_index = 0;
  m_total += 128;
}

std::string &operator+=(std::string &lhs, SHA512 &rhs) {
  // Convert hash to string and append.  Note that not all bits of the hash are
  // used.  There will be six bits per character, using upper and lower case
  // letters, the digits, and $ and @.
  // FIXME: just how many bits to use is a good question; for now, 240.
  auto &h = rhs.GetHash();
  for (int i = 0; i < 4; i++) {
    uint64_t v = h.m_data[i];
    for (int j = 0; j < 10; j++) {
      const char *p =
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXZY0123456789#$";
      lhs += p[v & 63];
      v >>= 6;
    }
  }

  return lhs;
}
