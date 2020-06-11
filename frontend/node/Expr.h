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

// Expr owns an expression subtree, providing an interface to the rest of the
// compiler.

#pragma once

#include "type/Type.h"
#include "parser/Location.h"
#include "util/Object.h"

class Node;
class SymbolTable;
class Value;

class Expr: public Object {
  DECLARE_OBJECT(Expr)

protected:
  void DeflateFields(Deflator &DF);
  Expr(Inflator &IF);

public:
  Expr(Node *e) : m_expr(e) { verify(e); }

  bool ErrorOccurred() { return m_error; }
  Location GetLocation();
  Type GetType();
  Node *Root() { return m_expr; }
  void Replace(Node *e) { m_expr = e; }

  // Resolve the expression to determined its type.  Note that a valid type
  // may result even if an error occurred.
  Type ResolveType();

  // Fully resolve the expression.  Returns whether an error occurred.
  bool ResolveFully();

  // Is this expression evaluable at compile time?  In general, it cannot
  // refer to local variables or labels outside of itself.  If diagnose is
  // true, errors will be reported if it is not evaluable.
  bool IsEvaluable(bool diagnose);

  // Evaluate an expression as a constant expression, returning a Value.  Null
  // is returned if an error occurred.  The expression will be fully resolved.
  Value *Evaluate(Type t = Type());
  Type EvaluateType();

  void BindNames(SymbolTable &st);

private:
  // The expression tree we own.
  Node               *m_expr          = nullptr;

  // Has an error occurred compiling the expression?
  bool                m_error         = false;

  // Is this expression known to be evaluable?
  bool                m_evaluable     = false;
};

namespace messageimpl {
  Location GetLocation(Expr *e);
}
