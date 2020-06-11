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

// A parameter tracks the resolution of a template parameter declaration,
// including its name.  type and default value.  Once these have all been
// resolved, it is no longer needed.

#pragma once

#include "type/Type.h"
#include "util/Object.h"

class DeflatedObject;
class Deflator;
class Expr;
class Inflator;
class String;
class SymbolTable;
class Value;

namespace AST {
  struct TemplateInfo;
}

class Parameter: public Object {
  DECLARE_OBJECT(Parameter)

protected:
  void DeflateFields(Deflator &DF);
  Parameter(Inflator &IF);

public:
  Parameter(char *n, Type t);
  Parameter(AST::TemplateInfo &ti);

  void BindNames(SymbolTable &st);

  // Expr yielding the name of the parameter.
  String             *m_name            = nullptr;

  // Expr yielding the type of the parameter.  Null if omitted.
  Expr               *m_typeExpr        = nullptr;
  Type                m_type;

  // Expr yielding the default value of the parameter.  Null if omitted.  If
  // it's an expression that uses an earlier generic parameter, we must re-
  // evaluate it for each derivation; a deflated copy is kept for this purpose.
  Expr               *m_value           = nullptr;
  DeflatedObject     *m_valueExpr       = nullptr;
  Value              *m_literalValue    = nullptr;
};
