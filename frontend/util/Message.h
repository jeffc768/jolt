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

// Messages are a polite term for compilation errors and warnings.  To emit
// a message, do something like this:
//
//    EmitError(source_location) << "text" << entity_ptr << 10;
//
// The temp object returned by EmitError will accumulate text across all the
// << operators, and log the completed message when it goes out of scope.  If
// needed, this temp object can be saved as a local variable:
//
//    {
//      auto msg = EmitError(source_location);
//      msg << stuff;
//      if (some_test)
//        msg << more_stuff;
//      // magic happens when it goes out of scope.
//    }
//
// Yes, this ought to use a message catalog of some sort, but I doubt I will
// be flooded with volunteers to translate the messages into other languages
// before the compiler gets rewritten in Jolt (if ever).  It can be properly
// addressed at that time.

#pragma once

#include "parser/Location.h"
#include <string>
#include <utility>

class Entity;
class String;

enum class ErrorLevel {
  fatal,          // Internal compiler error
  error,          // Compilation error that prevents successful completion
  warning1,       // A warning that just avoids being an error
  warning2,       // A modest warning that still ought to be noticed
  warning3,       // A minor warning that can be safely ignored
  warning4,       // A trivial warning that no one wants to be bothered with
  none
};

namespace messageimpl {
  class MsgEmitter;

  class MsgStream {
    friend class MsgEmitter;

    const ErrorLevel      m_level;
    Location              m_location;
    std::string           m_text;

    MsgStream(ErrorLevel level, Location sl)
        : m_level(level), m_location(sl) { }

  public:
    ~MsgStream();

    // Not for public use, even though they're declared public.  Link error
    // will occur if used inappropriately.
    void Append(char c);
    void Append(const char *s);
    void Append(const char *s, size_t len);
    void Append(const std::string &s);
  };

  class MsgEmitter {
    const ErrorLevel      m_level;

  public:
    MsgEmitter(ErrorLevel level) : m_level(level) { }

    MsgEmitter(const MsgEmitter &) = delete;
    MsgEmitter &operator=(const MsgEmitter &) = delete;

    MsgStream operator()() {
      return MsgStream(m_level, { });
    }

    MsgStream operator()(Location sl) {
      return MsgStream(m_level, sl);
    }

    template<class T> MsgStream operator()(T *where) {
      return MsgStream(m_level, GetLocation(where));
    }
  };

  MsgStream &&operator<<(MsgStream &&ms, char c);
  MsgStream &&operator<<(MsgStream &&ms, const char *s);
  MsgStream &&operator<<(MsgStream &&ms, const std::string &s);

  MsgStream &&operator<<(MsgStream &&ms, Entity *e);
  MsgStream &&operator<<(MsgStream &&ms, String *s);

  template<class T> MsgStream &&operator<<(MsgStream &&ms, T n) {
    ms.Append(std::to_string(n));
    return std::move(ms);
  }

  template<class T> inline MsgStream &&operator<<(MsgStream &ms, T&& value) {
    return std::move(ms) << std::forward<T>(value);
  }
}

extern messageimpl::MsgEmitter EmitWarning1;
extern messageimpl::MsgEmitter EmitWarning2;
extern messageimpl::MsgEmitter EmitWarning3;
extern messageimpl::MsgEmitter EmitWarning4;
extern messageimpl::MsgEmitter EmitError;
extern messageimpl::MsgEmitter EmitFatal;

// Return the level of the most severe message added.
ErrorLevel GetSeverestErrorLevelAdded();

// Sort the accumulated messages and print them to standard output.
void PrintErrorMessages();
