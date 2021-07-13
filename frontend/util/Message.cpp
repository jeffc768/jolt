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

#include "Message.h"
#include "entity/Entity.h"
#include "parser/Location.h"
#include "util/String.h"
#include <cstdio>
#include <cstring>
#include <map>

using namespace messageimpl;

MsgEmitter EmitWarning1(ErrorLevel::warning1);
MsgEmitter EmitWarning2(ErrorLevel::warning2);
MsgEmitter EmitWarning3(ErrorLevel::warning3);
MsgEmitter EmitWarning4(ErrorLevel::warning4);
MsgEmitter EmitError(ErrorLevel::error);
MsgEmitter EmitFatal(ErrorLevel::fatal);

// FIXME: generic derivation can cause errors to repeat for each derivation.
// Need to weed out multiple occurances of an error at a given line and
// column.  Also need to be able to suppress *all* errors for a method when
// that method uses a unbound generic.

namespace messageimpl {
  class MessageKey {
  public:
    MessageKey(Location sl, ErrorLevel lvl)
        : m_location(sl),
          m_level(lvl) {
    }

    bool operator<(const MessageKey &that) const {
      Location lhs = this->m_location;
      Location rhs = that.m_location;

      if (!lhs)
        return rhs;
      if (!rhs)
        return false;

      if (&lhs.File() != &rhs.File()) {
        int n = lhs.File().compare(rhs.File());
        if (n != 0)
          return n < 0;
      }

      if (lhs.Line() < rhs.Line())
        return true;
      if (lhs.Line() > rhs.Line())
        return false;
      return lhs.Col() < rhs.Col();
    }

    Location       m_location;
    ErrorLevel     m_level;
  };

  inline void MsgStream::Append(char c) {
    m_text += c;
  }

  inline void MsgStream::Append(const char *s) {
    m_text += s;
  }

  inline void MsgStream::Append(const char *s, size_t len) {
    m_text.insert(m_text.end(), s, s + len);
  }

  inline void MsgStream::Append(const std::string &s) {
    m_text += s;
  }

  MsgStream &&operator<<(MsgStream &&ms, char c) {
    ms.Append(c);
    return std::move(ms);
  }

  MsgStream &&operator<<(MsgStream &&ms, const char *s) {
    ms.Append(s);
    return std::move(ms);
  }

  MsgStream &&operator<<(MsgStream &&ms, const std::string &s) {
    ms.Append(s);
    return std::move(ms);
  }

  MsgStream &&operator<<(MsgStream &&ms, Entity *e) {
    String *s = e->Name();
    ms.Append(s->c_str(), s->Length());
    return std::move(ms);
  }

  MsgStream &&operator<<(MsgStream &&ms, String *s) {
    ms.Append(s->c_str(), s->Length());
    return std::move(ms);
  }
}

// FIXME: this only keeps one error per location!
using MessageMap = std::map<MessageKey, std::string>;
static MessageMap g_messages;

static ErrorLevel g_severestLevel = ErrorLevel::none;

static const char *g_levels[] = {
  "FATAL",
  "Error",
  "Warning1",
  "Warning2",
  "Warning3",
  "Warning4"
};

MsgStream::~MsgStream() {
  if (m_level < g_severestLevel)
    g_severestLevel = m_level;

  g_messages.insert(MessageMap::value_type(MessageKey(m_location, m_level),
                                           std::move(m_text)));

  if (m_level == ErrorLevel::fatal) {
    PrintErrorMessages();
    exit(1);
  }
}

static void Print(ErrorLevel level, const Location sl, const std::string &msg) {
  int lvl = (int)level;
  if (sl)
    printf("%s(%d:%d): %s: %s\n", sl.File().c_str(), sl.Line(), sl.Col(),
                                  g_levels[lvl], msg.c_str());
  else
    printf("%s: %s\n", g_levels[lvl], msg.c_str());
}

ErrorLevel GetSeverestErrorLevelAdded() {
  return g_severestLevel;
}

void PrintErrorMessages() {
  for (auto &I : g_messages)
    Print(I.first.m_level, I.first.m_location, I.second);
  g_messages.clear();
}
