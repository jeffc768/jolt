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

#include "Generic.h"
#include "AttributeList.h"
#include "Parameter.h"
#include "Specialization.h"
#include "node/Expr.h"
#include "node/Literal.h"
#include "parser/SymbolTable.h"
#include "util/DeflatedObject.h"
#include "util/InDeflator.h"
#include "util/SHA512.h"
#include "util/String.h"
#include "util/Value.h"
#include <vector>

IMPLEMENT_ENTITY(Generic)

Generic::Generic(Entity *parent, vector<Entity *> &defs)
    : Entity(parent, defs[0]->GetLocation(), defs[0]->Name()),
      m_nonSpecialized(safe_cast<Specialization *>(defs[0])) {
  // FIXME: handle specializations and generic types.
  verify(defs.size() == 1);
}

void Generic::DeflateFields(Deflator &DF) {
  Entity::DeflateFields(DF);

  DF << WX(m_state);

  if (m_state == st_resolved) {
    DF << m_nonSpecialized;
    DF << m_baseGeneric;
    DF << m_defaults;
    DF << m_derivations;
  } else {
    verify(m_state == st_initial);
    verify(!m_nonSpecialized);
    verify(!m_baseGeneric);
    verify(!m_defaults.size());
    verify(m_derivations.empty());
  }
}

Generic::Generic(Inflator &IF)
    : Entity(IF) {
  IF >> WX(m_state);

  if (m_state == st_resolved) {
    IF >> m_nonSpecialized;
    IF >> m_baseGeneric;
    IF >> m_defaults;
    IF >> m_derivations;
  }
}

void Generic::BindNames(SymbolTable &st) {
  Entity::BindNames(st);

  SymbolTable::PushScope scope(st);
  m_nonSpecialized->BindNames(st);
}

void Generic::ResolveFully() {
  if (m_state == st_resolved)
    return;

  if (AttributeList *al = Attributes())
    al->ResolveFully();

  m_nonSpecialized->ResolveFully();

  // We now know the number and types of the generic parameters, so go construct
  // the base Derivation.
  vector<Parameter *> &params = m_nonSpecialized->GetParameters();
  vector<Type> types(params.size());
  for (size_t i = 0; i < params.size(); i++)
    types[i] = params[i]->m_type;
  Type t = Type::Generic(Type::Tuple(types));
  m_baseGeneric = new Derivation(this, t);
  m_derivations[m_baseGeneric] = nullptr;

  // Finally, go gather up the default values.
  m_defaults.resize(params.size());
  for (size_t i = 0; i < params.size(); i++) {
    Parameter *p = params[i];
    if (p->m_literalValue)
      m_defaults[i] = p->m_literalValue;
    else if (p->m_valueExpr)
      m_defaults[i] = p->m_valueExpr;
  }

  m_state = st_resolved;
}

Node *Generic::AsValue(Location sl) {
  ResolveFully();

  Value *v = Value::NewRaw(m_baseGeneric->m_type,
                           &m_baseGeneric, sizeof(void *));
  return new Literal(GetLocation(), v);
}

Entity::ResolutionState Generic::GetResolutionState() {
  verify(m_state == st_resolved);
  return rs_ok;
  // FIXME: do we need more?
}

void Generic::AddToClosure(TranslateClosure *tc) {
  // FIXME:  Anything to add?  Theoretically, a specialization with no unbound
  // parameters could be defined, which could be compiled to object code just
  // like any other class.  What about stuff with bound parameters??
}

void Generic::FinalizeClosure(TranslateClosure *) {
}

void Generic::AppendToHash(SHA512 &hash) {
  hash.Append('G');

  switch (m_nonSpecialized->m_kind) {
    case gk_class:    hash.Append('C');   break;
    case gk_concept:  verify(false);      break;
    case gk_type:     hash.Append('T');   break;
  }

  // FIXME: anything more?  Requirements, certainly, once implemented.
}
