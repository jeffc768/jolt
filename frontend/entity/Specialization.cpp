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

#include "Specialization.h"
#include "AttributeList.h"
#include "Class.h"
#include "Parameter.h"
#include "Parameters.h"
#include "Requirement.h"
#include "node/Expr.h"
#include "node/Ident.h"
#include "parser/SymbolTable.h"
#include "parser/Token.h"
#include "util/DeflatedObject.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"

namespace BG {
  using bg_t = Entity *(Derivation *);

  extern bg_t ConformantArray;
  extern bg_t FixedArray;
  extern bg_t Pointer;
  extern bg_t Set;
  extern bg_t SignedSubrange;
  extern bg_t UnsignedSubrange;
  extern bg_t CharacterSubrange;
  extern bg_t CodeFragment;

  // Must match BuiltinGeneric enum.
  static bg_t *table[] = {
    nullptr,
    ConformantArray,
    FixedArray,
    Pointer,
    Set,
    SignedSubrange,
    UnsignedSubrange,
    CharacterSubrange,
    CodeFragment
  };
}

IMPLEMENT_ENTITY(Specialization)

Specialization::Specialization(Entity *parent,
                               GenericKind kind,
                               String *name,
                               BuiltinGeneric bg,
                               const vector<Type> &paramtypes)
    : Entity(parent, { }, name),
      m_kind(kind),
      m_builtin(bg) {
  for (size_t i = 0; i < paramtypes.size(); i++) {
    char name[2];
    name[0] = char('A' + i);
    name[1] = 0;
    m_params.push_back(new Parameter(name, paramtypes[i]));
  }
}

Specialization::Specialization(Entity *e, AST::TemplateClause &tmpl,
                               AST::SafeArray<Node *> specs)
    : Entity(nullptr, tmpl.m_keyword->GetLocation(), e->Name()),
      m_root(new Parameters(e)) {
  for (AST::TemplateInfo &ti : tmpl.m_parms) {
    m_params.push_back(new Parameter(ti));
    auto name = safe_cast<String *>(ti.m_name->m_value);
    if (!m_root->ReserveParameter(name)) {
      EmitError(ti.m_name) << "Duplicate declaration of generic parameter "
                           << name << ".";
    }
  }

  switch (e->Kind()) {
    case ek_Class:    m_kind = gk_class;  break;
    case ek_Const:    m_kind = gk_type;   break;
    default:          verify(false);
  }

  if (m_kind == gk_class) {
    // Also throw in an entry for the class itself; the class name always refers
    // to the derivation within itself.
    if (!m_root->ReserveParameter(e->Name()))
      verify(false);
  }

  // FIXME: no specializations yet
  verify(!specs.size());

  // FIXME: we probably want to hold on to ce and use it to type-check the
  // template, but type-checking cannot be done until after the attributes
  // have been resolved.  For now, let it become garbage.
}

void Specialization::DeflateFields(Deflator &DF) {
  // Exclude our parent from the serialization.  The inflated copies would
  // have new parents anyway.
  Entity::DeflateFields(DF);

  DF << WX(m_state);
  DF << m_kind;
  DF << m_declaration;
  DF << m_names;
  DF << m_requirements;

  if (m_state == st_resolved) {
    DF << m_predecessors;
    DF << m_builtin;
  } else {
    DF << m_params;
    DF << m_root;

    verify(m_builtin == bg_none);
    verify(m_predecessors.size() == 0);
  }
}

Specialization::Specialization(Inflator &IF)
    : Entity(IF) {
  IF >> WX(m_state);
  IF >> m_kind;
  IF >> m_declaration;
  IF >> m_names;
  IF >> m_requirements;

  if (m_state == st_resolved) {
    IF >> m_predecessors;
    IF >> m_builtin;
  } else {
    IF >> m_params;
    IF >> m_root;
  }
}

void Specialization::BindNames(SymbolTable &st) {
  Entity::BindNames(st);

  if (m_builtin != bg_none)
    return;

  if (m_root)
    m_root->BindNames(st);

  // FIXME: need to interleave these with adding parameter names to the
  // scope above.
  for (auto p : m_params)
    p->BindNames(st);
}

vector<Parameter *> &Specialization::GetParameters() {
  verify(m_state == st_resolved);
  return m_params;
}

Entity *Specialization::Derive(Derivation *d) {
  if (m_builtin != bg_none)
    return (BG::table[m_builtin])(d);

  // Inflate a new copy of the declaration.
  TemplateInflator IF;
  auto ps = safe_cast<Parameters *>(IF(m_declaration));

  // Add parameter bindings.
  for (size_t i = 0; i < m_names.size(); i++)
    ps->AddParameter(m_names[i], d->m_bindings[i]);

  Entity *body = ps->GetBody();
  if (m_kind == gk_class) {
    // Also throw in an entry for the class itself; the class name always refers
    // to the derivation within itself.
    auto ce = safe_cast<Class *>(body);
    Value *v = Value::NewType(ce->AsType());
    ps->AddParameter(ce->Name(), v);
    ce->SetGenericBindings(d);
    ce->Metaclass()->SetGenericBindings(d);
  }

  ps->Setup();
  return body;
}

void Specialization::ResolveFully() {
  if (m_state == st_resolved)
    return;

  if (AttributeList *al = Attributes())
    al->ResolveFully();

  // Create the template and save it.  Do not save any member attributes; they
  // will be evaluated (once) as part of this specialization and re-used for
  // each derivation (assuming no build(false), naturally).  Do not pass along
  // the lexical parent either, because (1) it will try to deflate it and that's
  // bad, (2) if the parent is also generic we don't know what it will be, and
  // (3) upon inflation a scope containing the generic parameter values will be
  // inserted as the immediate parent.
  TemplateDeflator DF;
  m_declaration = DF(m_root);

  for (auto p : m_params) {
    if (p->m_typeExpr)
      p->m_type = p->m_typeExpr->EvaluateType();

    // FIXME: issue compilation errors.
    verify(p->m_type != tk_valueless);

    // FIXME: also complain if specializations provide a type or default value.

    if (p->m_value) {
      p->m_literalValue = p->m_value->Evaluate()->Lower(p->m_type);
      // FIXME: handle default values that use earlier generic parameters.
    }

    m_names.push_back(p->m_name);
  }

  m_state = st_resolved;
}
