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

#include "Const.h"
#include "AttributeList.h"
#include "Class.h"
#include "Namespace.h"
#include "node/Expr.h"
#include "node/GlobalAddr.h"
#include "node/Literal.h"
#include "node/LiteralAddr.h"
#include "parser/SymbolTable.h"
#include "parser/Token.h"
#include "target/Target.h"
#include "target/TranslateClosure.h"
#include "util/InDeflator.h"
#include "util/SHA512.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_ENTITY(Const)

Const::Const(Token *name, bool isType, Expr *value)
    : Entity(nullptr, name->GetLocation(), name),
      m_isType(isType),
      m_expr(value) {
  // FIXME: only declarations inside a concept can omit the value.  Must know
  // if we are in one.  Generic types can also omit the value.
  verify(value);
}

Const::Const(Entity *parent, StringHelper name, Type t, void *data, size_t size)
    : Entity(parent, { }, name),
      m_state(st_resolved) {
  // Created in the resolved state.
  m_object = Value::NewRaw(t, data, size);
}

Const::Const(Entity *parent, StringHelper name, Object *value)
    : Entity(parent, { }, name),
      m_state(st_resolved) {
  // Created in the resolved state.
  if (auto v = dyn_cast<Value *>(value))
    m_object = v;
  else
    m_entity = safe_cast<Entity *>(value);
}

void Const::DeflateFields(Deflator &DF) {
  Entity::DeflateFields(DF);

  DF << WX(m_state);

  if (m_state == st_resolved) {
    DF << m_object;
    DF << m_entity;
  } else {
    DF << m_isType;
    DF << m_expr;
  }
}

Const::Const(Inflator &IF)
    : Entity(IF) {
  IF >> WX(m_state);

  if (m_state == st_resolved) {
    IF >> m_object;
    IF >> m_entity;
  } else {
    IF >> m_isType;
    IF >> m_expr;
  }
}

void Const::BindNames(SymbolTable &st) {
  Entity::BindNames(st);

  if (m_expr)
    m_expr->BindNames(st);
}

void Const::ResolveFully() {
  if (m_state == st_resolved)
    return;

  if (AttributeList *al = Attributes())
    al->ResolveFully();

  // If the value is another entity, a namespace or class, then directly
  // point to it.
  Type t = m_isType ? Type::JType() : Type();
  Value *v = m_expr->Evaluate(t);
  t = v->ObjectType();
  if (t == tk_namespace)
    m_entity = Namespace::GetFromObject(v);
  else if (t == tk_pointer && t.BaseType() == tk_class &&
           t.BaseType().Class()->IsMetaclass())
    m_entity = Class::GetFromObject(v);
  else
    m_object = v;

  //m_expr = nullptr;
  m_state = st_resolved;
}

Node *Const::AsValue(Location sl) {
  // The value representation of a const entity is the value of the compile time
  // expression.
  ResolveFully();
  if (m_entity) {
    return m_entity->AsValue(sl);
  } else {
    Type t = m_object->ObjectType();

    if (t == tk_pseudo || t.IsSimpleType())
      return new Literal(sl, m_object);
    else
      return (new LiteralAddr(sl, Type::PointerTo(t), m_object, this))->Deref();
  }
}

void *Const::GlobalStorage(Epoch ep) {
  // FIXME: this isn't actually used.
  verify(ep == ep_compile || m_entity);
  if (m_entity)
    return m_entity->GlobalStorage(ep);
  else
    return m_object->Address();
}

const std::string &Const::GlobalStorageName() {
  // FIXME: this isn't actually used.
  if (m_entity)
    return m_entity->GlobalStorageName();
  else
    return ExternalName();
}

Entity::ResolutionState Const::GetResolutionState() {
  verify(m_state == st_resolved);
  if (m_entity)
    return m_entity->GetResolutionState();
  Type t = m_object->ObjectType();
  if (t == Type::Unbound())
    return rs_hasUnbound;
  else if (t == Type::Suppress())
    return rs_hasError;
  else
    return rs_ok;
}

void Const::AddToClosure(TranslateClosure *tc) {
  if (m_entity)
    return m_entity->AddToClosure(tc);

  if (tc->GetEpoch() == ep_run) {
    if (!m_runTarget)
      m_runTarget = Target::Get(ep_run)->For(this);
  }

  // Register our dependencies.
  Type t = m_object->ObjectType();
  if (t == tk_class)
    tc->AddToClosure(t.Class());
}

void Const::FinalizeClosure(TranslateClosure *tc) {
  // Translate to C++.
  // FIXME: Const isn't target-independent for ep_compile.
  if (tc->GetEpoch() == ep_run) {
    m_runTarget->Generate();
  }
}

void Const::UpdateValue(Object *value) {
  verify(m_state == st_resolved);
  if (auto v = dyn_cast<Value *>(value)) {
    m_object = v;
    m_entity = nullptr;
  } else {
    m_entity = safe_cast<Entity *>(value);
    m_object = nullptr;
  }
}

void Const::AppendToHash(SHA512 &hash) {
  hash.Append('D');
  m_object->ObjectType().AppendToHash(hash);
}
