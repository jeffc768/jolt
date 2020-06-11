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

// Location notes where in the source code a token was scanned.  To support
// helpful diagnostics originating from expanded macros, a Location can be
// chained to another Location marking the point of expansion.

#pragma once

#include <string>
#include <vector>

class BufferWriter;

using std::vector;

class Location {
  uint64_t            m_file : 16;
  uint64_t            m_line : 24;
  uint64_t            m_col  : 24;

  static vector<std::string> s_files;

public:
  Location() : m_file(0), m_line(0), m_col(0) { }

  Location(uint32_t line, uint32_t col)
      : m_file(s_files.size()),
        m_line(line),
        m_col(col) { }

  const std::string &File() const;
  int Line() const { return m_line; }
  int Col() const { return m_col; }

  operator bool() const { return m_file != 0; }

  bool Dump(BufferWriter &bw);

  static void SetFile(const char *file, char *source, size_t len);
};
