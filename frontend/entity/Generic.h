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

// A generic entity represents a collection of entities that are derived from
// the same declaration, including zero or more specialized declarations, each
// with different values for generic parameters.  Class and type declarations
// may be generic, while concept declarations are always generic.

#pragma once

#include "Derivation.h"
#include "Entity.h"
#include <unordered_map>

class Requirement;
class Specialization;

using std::unordered_map;

// FIXME: only handles generic classes, and generic classes without
// specializations at that.
class Generic: public Entity {
  DECLARE_ENTITY(Generic)

  enum state_t: uint8_t {
    st_initial,
    st_resolved
  };

  state_t             m_state                 = st_initial;

  friend class Derivation;
  friend class Specialization;

  struct BindingsHash {
    uint32_t operator()(Derivation *d) const {
      return d->Hash();
    }
  };

  struct BindingsCmp {
    bool operator()(Derivation *left, Derivation *right) const {
      return left->Compare(right) == 0;
    }
  };

protected:
  void DeflateFields(Deflator &DF);
  Generic(Inflator &IF);

  virtual void AppendToHash(SHA512 &hash);

public:
  Generic(Entity *parent, vector<Entity *> &defs);

  Derivation *BaseGeneric() { return m_baseGeneric; }

  virtual void BindNames(SymbolTable &st);
  virtual EntityScoping Scoping() { return es_nonmember; }
  virtual void ResolveFully();
  virtual Node *AsValue(Location sl);
  virtual ResolutionState GetResolutionState();
  virtual void AddToClosure(TranslateClosure *tc);
  virtual void FinalizeClosure(TranslateClosure *tc);

private:
  // The non-specialized template, used when there is no appropriate
  // specialization.
  Specialization     *m_nonSpecialized        = nullptr;

  // The generic value for this entity with no bound parameters.
  Derivation         *m_baseGeneric           = nullptr;

  // Default generic parameter expressions.  There are three possibilities:
  //   1. No default -- null pointer.
  //   2. Literal value -- pointer to a Value.
  //   3. Expr using other generic parameters -- pointer to a DeflatedObject.
  // The ordering of elements matches the types in the generic value type in
  // m_baseGeneric and the parameter names in m_nonSpecialized.
  vector<Object *>    m_defaults;

  // Map from Derivations to entities derived from the corresponding bindings.
  // The entity will be null if any parameters are unbound.  Two pointers to
  // Derivations are equal if and only if they refer to the same Generic and
  // have the same set of bound parameters.
  using Map = unordered_map<Derivation *, Entity *, BindingsHash, BindingsCmp>;
  Map                 m_derivations;
};
