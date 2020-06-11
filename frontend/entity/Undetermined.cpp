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

#include "Undetermined.h"
#include "AttributeList.h"
#include "Generic.h"
#include "Method.h"
#include "Nullified.h"
#include "OverloadSet.h"
#include "node/Expr.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"

IMPLEMENT_ENTITY(Undetermined)

Undetermined::Undetermined(Entity *parent, String *name, Scope *s)
    : Entity(parent, { }, name),
      m_scope(s) {
  verify(name);
}

void Undetermined::DeflateFields(Deflator &DF) {
  Entity::DeflateFields(DF);
  DF << WX(m_state);
  DF << m_definitions;
  DF << m_scope;
  DF << m_replacement;

  verify(m_state == st_initial);
  verify(!m_generator);
}

Undetermined::Undetermined(Inflator &IF)
    : Entity(IF) {
  IF >> WX(m_state);
  IF >> m_definitions;
  IF >> m_scope;
  IF >> m_replacement;
}

void Undetermined::AddDefinition(Entity *e) {
  if (Parent() && !e->Parent())
    e->SetParent(Parent());
  m_definitions.push_back(e);
}

void Undetermined::BindNames(SymbolTable &st) {
  Entity::BindNames(st);

  for (auto e : m_definitions)
    e->BindNames(st);
}

void Undetermined::BindNames(SymbolTable &st, bool (*filter)(Entity *e)) {
  Entity::BindNames(st);

  for (auto e : m_definitions) {
    if (filter(e))
      e->BindNames(st);
  }
}

void Undetermined::BindNamesInAttributes(SymbolTable &st) {
  for (auto e : m_definitions) {
    if (AttributeList *al = e->Attributes())
      al->BindNames(st);
  }
}

void Undetermined::SetParent(Entity *chain) {
  Entity::SetParent(chain);
  for (auto def : m_definitions)
    def->SetParent(chain);
}

void Undetermined::ResolveFully() {
  if (m_state == st_resolved)
    return;

  // Iterate the definitions, waiting for their attribute lists to resolve.
  size_t len = m_definitions.size();
  for (size_t i = 0; i < len; ) {
    Entity *e = m_definitions[i];

    bool built = true;
    if (AttributeList *al = e->Attributes()) {
      al->ResolveFully();
      built = al->GetBuildValue();
    }

    if (!built) {
      // This definition won't be built.  Drop it.
      m_definitions[i] = m_definitions[--len];
      m_definitions.resize(len);
    } else {
      i++;
    }
  }

  // We have one of three legal situations:
  //   1. There are zero definitions.  Replace ourself with a Nullified entity.
  //   2. There are one or more method definitions.  Replace with a
  //      OverloadSet.
  //      FIXME: handle generic methods
  //   3. There is a generic class definition.  Replace with a Generic.
  //      FIXME: handle specializations and generic types.
  //   4. There is exactly one (non-method) definition.  Replace ourself
  //      with it.
  // Anything else is a compilation error.
  Entity *replacement = nullptr;
  if (m_definitions.size() == 0 && !m_generator) {
    replacement = new Nullified(Parent(), Name());
  } else {
    // Verify that non-methods aren't being overloaded.
    Entity *nonmethod = nullptr;
    for (auto def : m_definitions)
      if (def->Kind() != ek_Method)
        nonmethod = def;
    if (nonmethod && m_definitions.size() > 1) {
      EmitError(nonmethod) << "Illegal overloading of " << nonmethod << ".";
      replacement = new Nullified(Parent(), Name());
      // FIXME: stop this error from cascading...  special type of null
      // entity?
    } else {
      replacement = ProcessDefinitions(Parent(), m_definitions);
    }
  }

  replacement->TrackResolution();
  m_replacement = replacement;
  m_scope->ReplaceEntity(replacement);

  m_state = st_resolved;
}

Node *Undetermined::AsValue(Location sl) {
  verify(m_state != st_resolved);
  return nullptr;
}

Entity::ResolutionState Undetermined::GetResolutionState() {
  verify(m_state == st_resolved);
  return rs_ok;
}

Entity *Undetermined::ProcessDefinitions(Entity *parent, vector<Entity *> &defs) {
  // If the definition isn't a member, return it.  Otherwise, give all the
  // definitions to a new OverloadSet and let it deal with them.
  if (defs.size() > 0 && defs[0]->Kind() != ek_Method) {
    // But if that non-member definition is a generic specialization, wrap it
    // in a Generic.
    verify(defs.size() == 1);
    Entity *e = defs[0];
    if (e->Scoping() == es_generic)
      e = new Generic(parent, defs);
    return e;
  } else {
    verify(m_generator || defs.size() > 0);
    vector<Method *> ms;
    ms.reserve(defs.size());

    for (auto e : defs) {
      switch (e->Kind()) {
        case ek_Method:
          ms.push_back(static_cast<Method *>(e));
          break;
        default:
          verify(false); // FIXME: compilation error
      }
    }

    OverloadSet *ose = new OverloadSet(parent, Name(), ms);
    if (m_generator)
      ose->SetGenerator(m_generator);
    return ose;
  }
}
