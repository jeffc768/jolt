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

#include "String.h"
#include "BufferWriter.h"

// Because DebugBuffer is used to format data for display in a debugger, it is
// a bad idea to do heap allocations--doubly so to depend on the timing of
// destructor calls.  Hence this array of static, fixed-sized buffers.
static const int NBUFFERS = 16;
static const int BUFLEN = 4096;

static char g_buffers[NBUFFERS][BUFLEN];
static int g_index = 0;

DebugBuffer::DebugBuffer() : m_buffer(g_buffers[g_index]), m_len(0) {
  g_index = (g_index + 1) & (NBUFFERS - 1);
}

bool DebugBuffer::Append(char c) {
  if (m_len < BUFLEN - 1) {
    m_buffer[m_len++] = c;
    return false;
  }
  return true;
}

bool DebugBuffer::Append(const std::string &s) {
  return Append(s.c_str(), s.size());
}

bool DebugBuffer::Append(String *s) {
  if (s)
    return Append(s->c_str(), s->Length());
  else
    return Append("<no_name>", 9);
}

bool DebugBuffer::Append(const char *p, size_t len) {
  bool rv = m_len + len >= BUFLEN;
  const char *end = p + len;
  if (rv)
    end = p + BUFLEN - m_len - 1;

  while (p < end)
    m_buffer[m_len++] = *p++;

  return rv;
}

const char *DebugBuffer::Get() {
  m_buffer[m_len < BUFLEN ? m_len : BUFLEN - 1] = 0;
  return m_buffer;
}

bool StringBuffer::Append(char c) {
  m_buffer += c;
  return false;
}

bool StringBuffer::Append(const std::string &s) {
  m_buffer += s;
  return false;
}

bool StringBuffer::Append(String *s) {
  if (s)
    return Append(s->c_str(), s->Length());
  else
    return Append("<no_name>", 9);
}

bool StringBuffer::Append(const char *p, size_t len) {
  m_buffer.append(p, len);
  return false;
}
