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

// A var entity represents a Jolt local variable.

#pragma once

#include "Entity.h"
#include "type/Type.h"

class VarDecl;

enum VarKinds: uint8_t { vk_local, vk_argument };

class Var: public Entity {
  DECLARE_ENTITY(Var)

  // Var is not a normal entity.  It exists only as a way to get a VarDecl node
  // into a Scope, which can only map names to entities, and appears
  // only in Scopes that belong to statement blocks.  Hence, it does not have
  // a parent and cannot inherit a global attribute list.  Var has a
  // subclass in Method to represent arguments.

protected:
  enum state_t: uint8_t {
    st_initial,
    st_resolved
  };

  state_t             m_state     = st_initial;

  // Used by Argument.
  Var(StringHelper name, Location loc, Type type);

  void DeflateFields(Deflator &DF);
  Var(Inflator &IF);

public:
  // Used by variable declarations.
  Var(Location sl, StringHelper name, Expr *type);

  Var(Location loc, Type type)
      : Var(StringHelper(), loc, type) { }

  virtual void BindNames(SymbolTable &st);
  virtual EntityScoping Scoping() { return es_nonmember; }

  virtual void ResolveFully();

  virtual Node *AsValue(Location sl);
  virtual ResolutionState GetResolutionState();

  virtual VarKinds VarKind() { return vk_local; }

  // Only used by VarDecl node.
  Expr *GetTypeExpr() { return m_typeExpr; }

  // Declared type of the variable.
  Type                m_type;

  // Associated VarDecl node, if any.
  VarDecl            *m_vardecl   = nullptr;

  // Is this a temp?  (Destruct mode leaves full expression.)
  bool                m_isTemp  = false;

protected:
  Expr               *m_typeExpr  = nullptr;
};
