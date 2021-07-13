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

// SHA-512 cryptographic hash algorithm, used to produce mangled external names.
// As external names are used in linking object files, and object files for
// different architectures will never be linked together, it's okay for hashes
// to be machine dependent.

#pragma once

#include <cstdint>
#include <cstdlib>
#include <string>

struct SHA512Hash {
  uint64_t    m_data[8];
};

class SHA512 {
public:
  SHA512();

  // Append more data to hash.
  void Append(void const *p, size_t len);

  void Append(const std::string &s) { Append(s.c_str(), s.size()); }
  void AppendString(const char *p, size_t len);

  void Append(int64_t v) { Append((uint64_t)v); }
  void Append(uint64_t v);

  void Append(int32_t v) { Append(uint32_t(v)); }
  void Append(uint32_t v);

  void Append(int16_t v) { Append(uint16_t(v)); }
  void Append(uint16_t v);

  void Append(char c) { Append(uint8_t(c)); }
  void Append(uint8_t v) {
    m_message[m_index++] = v;
    if (m_index == 128)
      DoRounds();
  }

  void Append(double v) { Append(&v, sizeof(double)); }

  // Finish computing the hash and return it.
  SHA512Hash &GetHash();

private:
  // Current message block (possibly under accumulation).
  uint8_t       m_message[128];

  // Bytes so far appended to current message block.
  unsigned      m_index;

  // Total bytes so far appended (all blocks).
  unsigned      m_total;

  // Current (or intermediate) hash.
  SHA512Hash    m_hash;

  void DoRounds();
};

std::string &operator+=(std::string &lhs, SHA512 &rhs);
