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

#include "Var.h"
#include "AttributeList.h"
#include "node/Expr.h"
#include "node/Load.h"
#include "node/VarAddr.h"
#include "node/VarDecl.h"
#include "util/InDeflator.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_ENTITY(Var)

Var::Var(Location sl, StringHelper name, Expr *type)
    : Entity(nullptr, sl, name),
      m_typeExpr(type) {
  // FIXME: make sure attributes get set.
}

Var::Var(StringHelper name, Location loc, Type type)
    : Entity(nullptr, loc, name),
      m_type(type) {
  if (m_type == rt_rvalueref)
    m_type = m_type.LValueRef();
  // expression nodes.
}

void Var::DeflateFields(Deflator &DF) {
  Entity::DeflateFields(DF);
  DF << WX(m_state);
  DF << m_type;

  // Note: m_vardecl is not serialized.

  if (m_state != st_resolved)
    DF << m_typeExpr;
}

Var::Var(Inflator &IF)
    : Entity(IF) {
  IF >> WX(m_state);
  IF >> m_type;

  if (m_state != st_resolved)
    IF >> m_typeExpr;
}

void Var::BindNames(SymbolTable &st) {
  Entity::BindNames(st);

  if (m_typeExpr)
    m_typeExpr->BindNames(st);
}

void Var::ResolveFully() {
  if (m_state == st_resolved)
    return;

  if (AttributeList *al = Attributes())
    al->ResolveFully();

  if (!m_type) {
    // The type must come from the initializer, which is owned by VarDecl.
    if (!m_typeExpr) {
      m_state = st_resolved;
      return;
    }

    Value *v = m_typeExpr->Evaluate(Type::JType());
    m_type = v ? v->AsType() : Type::Suppress();
  }

  if (m_type == rt_rvalueref)
    m_type = m_type.LValueRef();

  m_state = st_resolved;
}

Node *Var::AsValue(Location sl) {
  ResolveFully();

  // The value representation of a var entity is a Deref node of a VarAddr node.
  Node *e = new VarAddr(sl, this);
  if (m_type != rt_rvalue)
    e = new Load(sl, e->Deref());

  return e->Deref();
}

Entity::ResolutionState Var::GetResolutionState() {
  verify(m_state == st_resolved);
  if (m_type == Type::Unbound())
    return rs_hasUnbound;
  else if (m_type == Type::Suppress())
    return rs_hasError;
  else
    return rs_ok;
}
