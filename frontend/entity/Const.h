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

// A const entity represents a Jolt const or type declaration.

#pragma once

#include "Entity.h"
#include "type/Type.h"

class TargetConst;
class Token;
class Value;

enum Epoch: uint8_t;

class Const: public Entity {
  DECLARE_ENTITY(Const)

  enum state_t: uint8_t {
    st_initial,
    st_resolved
  };

  state_t             m_state         = st_initial;

protected:
  void DeflateFields(Deflator &DF);
  Const(Inflator &IF);

  virtual void AppendToHash(SHA512 &hash);

public:
  Const(Token *name, bool isType, Expr *value);
  Const(Entity *parent, StringHelper name, Type t, void *data, size_t size);
  Const(Entity *parent, StringHelper name, Object *value);

  virtual void BindNames(SymbolTable &st);
  virtual EntityScoping Scoping() { return es_nonmember; }

  virtual void ResolveFully();

  virtual Node *AsValue(Location sl);
  virtual void *GlobalStorage(Epoch ep);
  virtual const std::string &GlobalStorageName();
  virtual ResolutionState GetResolutionState();
  virtual void AddToClosure(TranslateClosure *tc);
  virtual void FinalizeClosure(TranslateClosure *tc);

  // Used when updating template parameters upon derivation.
  void UpdateValue(Object *value);

  // The value of this const entity.  Only one of m_object or m_entity can be
  // set.
  Value              *m_object        = nullptr;
  Entity             *m_entity        = nullptr;

private:
  bool                m_isType        = false;

  // Expr yielding the constant value.
  Expr               *m_expr          = nullptr;

  // The target object for this const (no support for compile target yet).
  // FIXME: not used at all; should it?
  TargetConst        *m_runTarget     = nullptr;
};
