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

// BufferWriter builds up a string in a buffer.  There are two types of buffers:
// DebugBuffer and StringBuffer.  The former is used by debuggers, i.e. gdb and
// lldb, for formatting stuff for display.  The latter is used to implement
// reflection.
//
// Note: because writers to these buffers may execute within a debugger, they
// need to avoid doing anything that may cause heap allocation.  Specifically,
// that means no std::string.

#pragma once

#include <string>

class String;

class BufferWriter {
public:
  virtual ~BufferWriter() { }

  virtual bool IsDebug() { return false; }

  // These all return true if space has been exhausted.  Any further appends
  // are silently discarded.
  virtual bool Append(char c) = 0;
  virtual bool Append(const std::string &s) = 0;
  virtual bool Append(String *s) = 0;
  virtual bool Append(const char *p, size_t len) = 0;
};

class DebugBuffer: public BufferWriter {
  char           *m_buffer;
  size_t          m_len;

public:
  DebugBuffer();

  virtual bool IsDebug() { return true; }

  // These all return true if space has been exhausted.  Any further appends
  // are silently discarded.
  virtual bool Append(char c);
  virtual bool Append(const std::string &s);
  virtual bool Append(String *s);
  virtual bool Append(const char *p, size_t len);

  // Pointer is valid until after a few creations of DebugBuffer.
  const char *Get();
};

class StringBuffer: public BufferWriter {
  std::string     m_buffer;

public:
  // True is never returned; std::strings grow as necessary.
  virtual bool Append(char c);
  virtual bool Append(const std::string &s);
  virtual bool Append(String *s);
  virtual bool Append(const char *p, size_t len);

  const std::string &Get() { return m_buffer; }
};
