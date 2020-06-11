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

// Formal arguments describes a method's formal arguments list.

#pragma once

#include "parser/ParserDecls.h"
#include "util/Object.h"

class Location;
class SymbolTable;

enum MethodKind: uint8_t;

class FormalArguments: public Object {
  DECLARE_OBJECT(FormalArguments)

protected:
  void DeflateFields(Deflator &DF);
  FormalArguments(Inflator &IF);

public:
  FormalArguments() { }
  FormalArguments(Location sl, AST::SafeArray<AST::MemberItem> formals,
                  AST::MemberItem rettype, bool exprBody);

  // Add implicit arguments for various method kinds.
  void AddImplicitArguments(MethodKind kind);

  void BindNames(SymbolTable &st);

  void ResolveFully();

  // Order this argument list with respect to the other.  Returns 0 if the two
  // represent the same signature, otherwise 1 or -1 is returned based on which
  // one is considered "greater" or "lesser" (arbitrary, but must be
  // consistent).
  int Compare(FormalArguments *other);

  // Get the number of implicit arguments.  These arguments are not part of
  // the method signature and are implicitly supplied by the compiler.
  size_t ImplicitArgCount();

  Location            m_location;

  vector<Argument *>  m_arguments;
  Argument           *m_returnValue           = nullptr;

  Type                m_type;

  // There are three possible implicit arguments.  These flags mark their
  // presence.  While not part of the method signature, they appear before
  // all other arguments in the lowered argument list.
  // order.
  bool                m_hasThis               = false;
  bool                m_hasStaticLink         = false;

  // Useful flags that summarize the arguments.
  VariadicType        m_variadic              = vt_none;
  bool                m_deducedReturnType     = false;

  // The total number of arguments in the method signature.
  int                 m_nSlots                = -1;
};
