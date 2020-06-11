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

// An Undetermined entity is a collection of all definitions with the same name,
// none to all of which may have build(false).  It's just a placeholder that
// drives the resolution of these definitions until it is known what is actually
// being built.  At that point, it replaces itself with that entity.

#pragma once

#include "Entity.h"
#include "Scope.h"

class OverloadSet;

class Undetermined: public Entity {
  DECLARE_ENTITY(Undetermined)

  enum state_t: uint8_t {
    st_initial,
    st_resolved
  };

  state_t             m_state       = st_initial;

protected:
  void DeflateFields(Deflator &DF);
  Undetermined(Inflator &IF);

public:
  Undetermined(Entity *parent, String *name, Scope *s);

  // Note: all definitions must be added before this entity is put onto the
  // ready queue for the first time.
  void AddDefinition(Entity *e);
  void RemoveDefinition(size_t index) {
    m_definitions.erase(m_definitions.begin() + index);
  }
  bool HasDefinitions() { return m_definitions.size() > 0; }
  size_t Count() { return m_definitions.size(); }
  Entity *GetDefinition(size_t i) { return m_definitions[i]; }

  void SetAuto(void (Class::*gen)(OverloadSet *ose)) { m_generator = gen; }
  bool IsAuto() { return m_generator != nullptr; }

  // Propagate to the contained entities.
  virtual void SetParent(Entity *parent);

  virtual ResolutionState GetResolutionState();

  virtual void BindNames(SymbolTable &st);
  void BindNames(SymbolTable &st, bool (*filter)(Entity *e));
  void BindNamesInAttributes(SymbolTable &st);

  virtual void ResolveFully();

  virtual EntityScoping Scoping() { return es_undetermined; }
  virtual Node *AsValue(Location sl);

  // Turn a collection of definitions into a single entity.
  Entity *ProcessDefinitions(Entity *parent, vector<Entity *> &defs);

  Entity *GetReplacement() { return m_replacement; }

private:
  // Definitions found in this class or namespace.
  vector<Entity *>    m_definitions;

  // The scope that holds us.
  Scope              *m_scope           = nullptr;

  // The entity that replaces us.
  Entity             *m_replacement     = nullptr;

  // Auto method generator.
  void (Class::*m_generator)(OverloadSet *ose) = nullptr;
};

