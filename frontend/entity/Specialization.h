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

// Represents a specialization of a generically declared entity.  These are
// partially ordered so that it's known which are more specialized than others.
// A specialization only keeps the requirements that are not possessed by its
// less-specialized relatives; this is OK, as it won't be considered unless its
// less-specialized relatives have met all their requirements.  Note: it also
// holds the non-specialized declaration as well.
//
// A specialization isn't really an entity, but we must pretend that it is so
// it can be added to a scope.  The scope will automatically create the true
// entity, Generic, the first time a specialization for a name is added.  This
// specialization will be placed in that entity by the scope.

#pragma once

#include "Entity.h"
#include "Generic.h"
#include "parser/ParserDecls.h"

class DeflatedObject;
class Parameter;

enum BuiltinGeneric: uint8_t {
  bg_none,
  bg_ConformantArray,
  bg_FixedArray,
  bg_Pointer,
  bg_Set,
  bg_SignedSubrange,
  bg_UnsignedSubrange,
  bg_CharacterSubrange,
  bg_CodeFragment
};

class Specialization: public Entity {
  DECLARE_ENTITY(Specialization)

  enum state_t: uint8_t {
    st_initial,
    st_resolved
  };

  state_t             m_state                     = st_initial;

protected:
  void DeflateFields(Deflator &DF);
  Specialization(Inflator &IF);

public:
  Specialization(Entity *parent, GenericKind kind,
                 String *name, BuiltinGeneric bg,
                 const vector<Type> &paramtypes);

  Specialization(Entity *e, AST::TemplateClause &tmpl,
                 AST::SafeArray<Node *> specs);

  virtual ResolutionState GetResolutionState() { return rs_ok; }
  virtual void BindNames(SymbolTable &st);
  virtual EntityScoping Scoping() { return es_generic; }

  virtual void ResolveFully();

  // Return array of Parameters.  IsReady() must be true for this to be called.
  vector<Parameter *> &GetParameters();

  // What kind of entity is encapsulated by this specialization.
  GenericKind         m_kind;

  // Which builtin generic, if any, does this specialization represent?
  BuiltinGeneric      m_builtin                   = bg_none;

  // Derive new class from a set of generic parameter bindings.
  virtual Entity *Derive(Derivation *d);

  // Deflated pristine copy of the generic declaration.  Will be null for
  // built-ins.
  DeflatedObject     *m_declaration               = nullptr;

  // Generic parameter names, in declared order.
  vector<String *>    m_names;

  // Generic parameter declarations.
  vector<Parameter *> m_params;

  // List of requirements, in order that they are to be checked.  Requirements
  // already checked by less-specialized versions are not included (though all
  // are initially present).  Must be empty for the non-specialized version.
  vector<Requirement *> m_requirements;

  // List of less-specialized versions.  All must have their requirements
  // satisfied for this to be considered.  Must be empty for the non-
  // specialized version.
  vector<Specialization *> m_predecessors;

private:
  // Root of the template body.
  Parameters         *m_root                      = nullptr;
};
