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

#include "Location.h"
#include "Token.h"
#include "util/BufferWriter.h"

vector<std::string> Location::s_files;

// Inside flex generated scanner.
Token *ScanBuffer(char *p, size_t len);

void Location::SetFile(const char *file, char *source, size_t len) {
  bool first = s_files.empty();

  s_files.push_back(file);

  Token *t = ScanBuffer(source, len);
  Token::TokenizeFile(first, t);
}

static std::string g_no_file { "<unknown>" };

const std::string &Location::File() const {
  return m_file ? s_files[m_file - 1] : g_no_file;
}

bool Location::Dump(BufferWriter &bw) {
  // Can't use std::string because that may allocate memory and this is called
  // from within debuggers.
  char buf[128];
  size_t len = sprintf(buf, "%d:%d:%d", (int)m_file, (int)m_line, (int)m_col);
  return bw.Append(buf, len);
}
