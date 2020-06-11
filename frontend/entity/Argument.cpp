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

#include "Argument.h"
#include "AttributeList.h"
#include "Class.h"
#include "node/Expr.h"
#include "node/Load.h"
#include "node/VarAddr.h"
#include "node/VarDecl.h"
#include "parser/ParserDecls.h"
#include "parser/SymbolTable.h"
#include "util/InDeflator.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_ENTITY(Argument)

Argument::Argument(AST::MemberItem &f, bool rv)
    : Var(f.m_value ? f.m_value->m_location : Location(), f.m_name,
          f.m_value ? new Expr(f.m_value) : nullptr),
      m_mechanism(rv ? am_out : am_in),
      m_isArray(false), // FIXME
      m_isDeducedType(rv && !f.m_value) {
  verify(rv || f.m_value);

  if (f.m_attrs)
    verify(false); //SetAttributes(new AttributeList(Parent(), 0, f->m_attributes));
}

Argument::Argument(Location sl, ArgumentMechanism m, Type t, StringHelper name)
    : Var(name, sl, t),
      m_mechanism(m) { }

void Argument::DeflateFields(Deflator &DF) {
  Var::DeflateFields(DF);
  DF << m_mechanism;
  DF << m_isArray;
  DF << m_isDeducedType;

  if (m_state == st_resolved) {
    DF << m_slot;
    DF << m_isReturned;
  } else {
    verify(m_slot == -1);
    verify(!m_isReturned);
  }
}

Argument::Argument(Inflator &IF)
    : Var(IF) {
  IF >> m_mechanism;
  IF >> m_isArray;
  IF >> m_isDeducedType;

  if (m_state == st_resolved) {
    IF >> m_slot;
    IF >> m_isReturned;
  }
}

void Argument::BindNames(SymbolTable &st) {
  Var::BindNames(st);

  if (AttributeList *al = Attributes())
    al->BindNames(st);
}

void Argument::SetType(Type t) {
  // When no type expression has been provided, we must wait either for
  // Method to plug in the class type (for "this"), or for MethodBody to do
  // so (for deduced return type).
  verify(!m_typeExpr && !m_type.IsKnown());
  m_type = t;
}

bool Argument::HasType() {
  return m_type.IsKnown() || m_typeExpr;
}

void Argument::ResolveFully() {
  if (m_state == st_resolved)
    return;

  // FIXME: look for @build and other attributes.
  if (auto *al = Attributes())
    al->ResolveFully();

  verify(m_type.IsKnown() || m_typeExpr);
  if (!m_type.IsKnown()) {
    if (Value *v = m_typeExpr->Evaluate(Type::JType()))
      m_type = v->AsType();
    else
      m_type = Type::Suppress();
  }

  m_state = st_resolved;
}
