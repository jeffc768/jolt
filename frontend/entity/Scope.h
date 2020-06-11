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

// A scope maps names to entity definitions.  Of course, it's not that simple.
// First, A name may map to multiple entities, as in the case of overloaded
// methods.  Second, it may map to a single entity, but it's not known initially
// which entity, as all but one have the build(false) attribute.
//
// Mapping is affected by attributes, so it is necessary to track the member
// attributes associated with each definition.  These attributes include not
// just the ones syntactically attached to the definitions, but also the ones
// in the immediately preceding global attribute list (| | construct), and even
// the attributes attached to the enclosing namespace declaration.
//
// In addition to the usual suspects (Class, OverloadSet, etc...), a name may
// also map to one of these "pseudo" entities:
//
//   Undetermined
//     A raw collection of definitions that, due to unprocess attributes, we
//     have yet to know what it actually is.
//
//   Nullified
//     A placeholder entity for an identifier for which all definitions had
//     build(false).
//
//   Ambiguous
//     A placeholder entity for when conflicting definitions are inherited.
//
// In addition to all this, a Scope also tracks the namespaces that are being
// used in that lexical scope.

#pragma once

#include "util/Object.h"
#include "util/UnorderedMapExtras.h"
#include <vector>

class Class;
class Entity;
class Expr;
class Namespace;
class OverloadSet;
class String;
class SymbolTable;
class Undetermined;

enum WellKnownString: int;

using std::vector;

class Scope: public Object {
  DECLARE_OBJECT(Scope)

  friend class Undetermined;

  enum state_t: uint8_t {
    st_initial,
    st_macros_applied,
    st_resolved
  };

  state_t                           m_state = st_initial;

  unordered_map<String *, Entity *> m_map;
  vector<Namespace *>               m_usedNamespaces;

  // Null if parent isn't a class entity.
  Class                            *m_parentClass       = nullptr;

  // Map from name macro to expression evaluating it.
  unordered_map<String *, Expr *>   m_macroToExpr;

protected:
  void DeflateFields(Deflator &DF);
  Scope(Inflator &IF);

public:
  Scope() { }
  void SetClass(Class *ce) { m_parentClass = ce; }
  void HandleNameMacros();
  void ResolveFully();

  void BindNames(SymbolTable &st);
  void BindNames(SymbolTable &st, bool (*filter)(Entity *e));
  void BindNamesInAttributes(SymbolTable &st);

  // Retrieves entity by name.  Returns nullptr if not found.
  Entity *LookupEntity(String *name);

  // Retrieves entity by name.  If not present, or present as a TryLater,
  // nullptr is returned; nothing is added to the map.
  Entity *RawGet(String *name);

  // Adds a name/entity pair.  The name must not already exist in this scope
  // and the entity's attributes must be fully resolved.
  void AddEntity(Entity *e);

  // Add existing entity under new name.  Returns false if new name already
  // exists in this scope.
  bool AddEntity(String *n, Entity *e);

  // Replaces a name/entity pair.  Must only be used by UndeterminedEntity.
  void ReplaceEntity(Entity *e);

  // Adds an undetermined name/entity pair.  The name must not already exist in
  // this scope, unless it maps to other Undetermined entities, in which case it
  // is added to the list of Undetermined entities for that name.
  void AddUndetermined(Entity *parent, Entity *e);

  // Ensure a TDE exists for a name, even if it's empty, so that stuff can be
  // auto-generated in it later on.
  void AddAuto(Class *parent, WellKnownString name,
               void (Class::*gen)(OverloadSet *ose));

  // Replace the mapping for a name to a different entity.  Can only be used
  // to replace one namespace with another.
  void Replace(String *name, Entity *e);


  // Is scope devoid of definitions?
  bool IsEmpty() { return m_map.size() == 0 &&
                          m_usedNamespaces.size() == 0; }

  // Call supplied functor for all members of this scope.
  template<typename F, typename... ARGS>
  void EnumerateMembers(F &&functor, ARGS&&... args) {
    Visit(m_map, std::forward<F>(functor), args...);
  }

  size_t UsedNamespaceCount() { return m_usedNamespaces.size(); }
  Namespace *UsedNamespace(size_t i) { return m_usedNamespaces[i]; }

  size_t DefinitionCount() { return m_map.size(); }

private:
  Undetermined *LocateUndetermined(Entity *parent, String *name);
};

// FIXME:  Find a better home for this.
struct InheritedEntity  {
  Entity             *m_entity;
  size_t              m_baseOrd;

  InheritedEntity(Entity *e, size_t ord) : m_entity(e), m_baseOrd(ord) { }

  void UpdateManagedAddresses() {
    *this = *this;
  }
};

using InheritedEntities = vector<InheritedEntity>;
