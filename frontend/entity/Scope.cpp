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

#include "Scope.h"
#include "AttributeList.h"
#include "Class.h"
#include "DeferredInflate.h"
#include "Namespace.h"
#include "Nullified.h"
#include "OverloadSet.h"
#include "Undetermined.h"
#include "node/Expr.h"
#include "node/Ident.h"
#include "util/InDeflator.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_OBJECT(Scope)

void Scope::DeflateFields(Deflator &DF) {
  // We can't just deflate m_map.  All the entities may need to be wrapped
  // so that they're inflated on demand.
  DF << WX(m_state);
  DF << m_parentClass;
  DF << uint32_t(m_map.size());
  for (auto& I : m_map) {
    DF << I.first;

    if (I.second->Kind() == ek_DeferredInflate) {
      // This happens when an imported generic entity is re-deflated, either
      // to capture a prestine copy in the case of a template deflate, or
      // because it's referenced by something being module exported.
      // FIXME:  In the latter case, we really shouldn't have to re-inflate,
      // as it isn't actually going to be deflated; we just need to know the
      // entity's name and parent.
      Entity *e = safe_cast<DeferredInflate *>(I.second)->Inflate();
      DF << e;
    } else {
      DF << I.second;
    }
  }

  // FIXME: Should these be inflated on demand as well?
  DF << m_usedNamespaces;
  DF << m_macroToExpr;
}

Scope::Scope(Inflator &IF)
    : Object(IF) {
  IF >> WX(m_state);
  IF >> m_parentClass;
  uint32_t size;
  IF >> size;
  m_map.reserve(size);
  while (size-- > 0) {
    String *s;
    IF >> s;

    Entity *e;
    if (IF.InModuleMode()) {
      uint32_t ord = IF.GetUninflatedObject();
      e = new DeferredInflate(s, IF, ord);
    } else {
      IF >> e;
    }

    m_map.emplace(s, e);
  }

  IF >> m_usedNamespaces;
  IF >> m_macroToExpr;
}

void Scope::BindNames(SymbolTable &st) {
  for (auto I : m_macroToExpr)
    I.second->BindNames(st);

  EnumerateMembers([&st](String *n, Entity *e) {
    e->BindNames(st);
  });
}

void Scope::BindNames(SymbolTable &st, bool (*filter)(Entity *e)) {
  for (auto I : m_macroToExpr)
    I.second->BindNames(st);

  EnumerateMembers([&st, filter](String *n, Entity *e) {
    auto kind = e->Kind();
    if (kind == ek_OverloadSet) {
      auto ose = safe_cast<OverloadSet *>(e);
      ose->BindNames(st, filter);
    } else if (kind == ek_Undetermined) {
      auto ue = safe_cast<Undetermined *>(e);
      ue->BindNames(st, filter);
    } else {
      if (filter(e))
        e->BindNames(st);
    }
  });
}

void Scope::BindNamesInAttributes(SymbolTable &st) {
  EnumerateMembers([&st](String *n, Entity *e) {
    auto kind = e->Kind();
    if (kind == ek_OverloadSet) {
      auto ose = safe_cast<OverloadSet *>(e);
      ose->BindNamesInAttributes(st);
    } else if (kind == ek_Undetermined) {
      auto ue = safe_cast<Undetermined *>(e);
      ue->BindNamesInAttributes(st);
    } else if (kind == ek_Argument) {
      // Ignore.  Already handled by FormalArguments.
    } else {
      if (AttributeList *al = e->Attributes())
        al->BindNames(st);
    }
  });
}

void Scope::HandleNameMacros() {
  if (m_state >= st_macros_applied)
    return;

  unordered_map<String *, String *> expansionToMacro;

  for (auto I : m_macroToExpr) {
    Value *v = I.second->Evaluate(Type::StringRef());
    String *s = v->AsString();

    // FIXME: look for macros expanding to same name!
    expansionToMacro[s] = I.first;
  }

  m_macroToExpr.clear();

  // Rename scope members affected by name macros.
  for (auto I : expansionToMacro) {
    auto J = m_map.find(I.first);
    verify(J == m_map.end()); // FIXME: come up with appropriate name

    J = m_map.find(I.second);
    verify(J != m_map.end());
    Entity *e = J->second;
    // FIXME: Member nodes are not bound by the symbol table, so the original
    // macro name must remain.  Applies to this.x, where "this." is not
    // explicit.
    //m_map.erase(J);
    m_map[I.first] = e;
  }

  Visit(m_map, [](String *, Entity *e) {
    e->TrackResolution();
  });

  m_state = st_macros_applied;
}

void Scope::ResolveFully() {
  if (m_state == st_resolved)
    return;

  verify(m_state == st_macros_applied);

  // Resolve all Undetermineds and replace them with the entity to which
  // they resolve.
  for (auto I : m_map) {
    if (auto ue = dyn_cast<Undetermined *>(I.second)) {
      ue->ResolveFully();
      I.second = ue->GetReplacement();
    }
  }

  m_state = st_resolved;
}

Entity *Scope::LookupEntity(String *name) {
  HandleNameMacros();

  while (true) {
    auto I = m_map.find(name);

    if (I != m_map.end()) {
      if (I->second->Kind() == ek_DeferredInflate)
        I->second = safe_cast<DeferredInflate *>(I->second)->Inflate();

      Entity *e = I->second;
      auto kind = e->Kind();
      if (kind == ek_Nullified) {
        // Until the scope is frozen, treat a nullified declaration as "try
        // later".  It might get replaced by an inherited entity.
        if (!m_parentClass || !m_parentClass->ResolveBases())
          return e;
        continue;
      } else if (kind == ek_Undetermined) {
        auto *ue = safe_cast<Undetermined *>(e);
        ue->ResolveFully();
        I->second = ue->GetReplacement();
        return I->second;
      }

      EntityScoping es = e->Scoping();
      if (es == es_member || es == es_nonmember)
        return e;
      verify(false); // FIXME generic??
      continue;
    } else if (!m_parentClass || !m_parentClass->ResolveBases()) {
      return nullptr;
    } else {
      m_parentClass->ResolveBases();
      continue;
    }
  }
}

Entity *Scope::RawGet(String *name) {
  auto I = m_map.find(name);
  return I != m_map.end() ? I->second : nullptr;
}

void Scope::AddEntity(Entity *e) {
  String *name = e->Name();
  verify(!e->Attributes() || !e->Attributes()->IsFullyResolved());
  bool success = AddEntity(name, e);
  verify(success);
  (void) success;
}

bool Scope::AddEntity(String *n, Entity *e) {
  verify(n);

  auto I = m_map.find(n);
  if (I != m_map.end()) {
    Entity *old = I->second;
    if (old->Kind() != ek_Nullified)
      return e == old;  // inherited common are added twice, alas
  }

  m_map[n] = e;
  return true;
}

void Scope::ReplaceEntity(Entity *e) {
  String *name = e->Name();
  verify(name);
  verify(e->Kind() != ek_Undetermined);

  auto I = m_map.find(name);
  verify(I != m_map.end() && I->second->Scoping() == es_undetermined);
  I->second = e;
}

Undetermined *Scope::LocateUndetermined(Entity *parent, String *name) {
  auto I = m_map.find(name);
  Entity *old = I == m_map.end() ? nullptr : I->second;
  if (auto ue = dyn_cast<Undetermined *>(old)) {
    return ue;
  } else {
    verify(!old);

    ue = new Undetermined(parent, name, this);
    m_map[name] = ue;

    // If the name was a macro, track it for expansion.
    if (name->IsNameMacro()) {
      // FIXME: get location for new ident.
      Node *e = new Ident({ }, name->AsNormal());
      m_macroToExpr[name] = new Expr(e);
    }

    return ue;
  }
}

void Scope::AddUndetermined(Entity *parent, Entity *e) {
  verify(m_state == st_initial);
  verify(e->Kind() != ek_Undetermined);

  Undetermined *ue = LocateUndetermined(parent, e->Name());
  ue->AddDefinition(e);
}

void Scope::AddAuto(Class *parent, WellKnownString name,
                    void (Class::*gen)(OverloadSet *ose)) {
  Undetermined *ue = LocateUndetermined(parent, String::Get(name));
  ue->SetAuto(gen);
}

void Scope::Replace(String *name, Entity *e) {
  verify(e->Kind() == ek_Namespace);
  auto I = m_map.find(name);
  if (I != m_map.end()) {
    verify(I->second->Kind() == ek_Namespace);
    I->second = e;
    return;
  }
  verify(false);
}
