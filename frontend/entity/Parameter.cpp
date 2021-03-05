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

#include "Parameter.h"
#include "parser/ParserDecls.h"
#include "parser/Token.h"
#include "node/Expr.h"
#include "node/Ident.h"
#include "util/DeflatedObject.h"
#include "util/InDeflator.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_OBJECT(Parameter)

Parameter::Parameter(char *n, Type t)
    : m_name(String::Get(n)),
      m_type(t) { }

Parameter::Parameter(AST::TemplateInfo &ti)
    : m_name(ti.m_name->StringValue()),
      m_typeExpr(ti.m_type ? new Expr(ti.m_type) : nullptr),
      m_type(ti.m_type ? Type() : Type::JType()),
      m_value(ti.m_init ? new Expr(ti.m_init) : nullptr) {
  verify(!ti.m_isVariadic); // FIXME

  if (m_value) {
    // FIXME: look for simple literals or identifiers that are obviously not
    // generic parameters so we can avoid this overhead.
    TemplateDeflator DF;
    m_valueExpr = DF(m_value);
  }
}

void Parameter::DeflateFields(Deflator &DF) {
  DF << m_name;
  DF << m_typeExpr;
  DF << m_type;
  DF << m_value;
  DF << m_valueExpr;

  verify(!m_typeExpr || !m_type);
  verify(!m_literalValue);
}

Parameter::Parameter(Inflator &IF) : Object(IF) {
  IF >> m_name;
  IF >> m_typeExpr;
  IF >> m_type;
  IF >> m_value;
  IF >> m_valueExpr;
}

void Parameter::BindNames(SymbolTable &st) {
  if (m_typeExpr)
    m_typeExpr->BindNames(st);

  if (m_value)
    m_value->BindNames(st);
}
