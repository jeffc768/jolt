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

// A derivation represents an instance of a Jolt generic value type.  These are
// visible to and manipulatable by Jolt epoch(compile) code.  It tracks which
// generic parameters are bound (and their values) and the ordering of the
// remaining unbound parameters.  It also points back to its parent Generic.

#pragma once

#include "type/Type.h"
#include "util/Object.h"

class Class;
class Const;
class Entity;
class Generic;
class Metadata;
class Value;

enum GenericKind {
  gk_class,
  gk_type,
  gk_concept
};

class Derivation: public Object {
  DECLARE_OBJECT(Derivation)

  friend Metadata;

protected:
  Derivation(Derivation *that);

  void DeflateFields(Deflator &DF);
  Derivation(Inflator &IF);

public:
  // Create new bindings with all unbound values.
  Derivation(Generic *ge, Type t);

  // Hash these bindings.
  uint32_t Hash();

  // Order these bindings with respect to the other.  Returns 0 if they are
  // equal, otherwise 1 or -1 is returned based on which one is considered
  // "greater" or "lesser" (arbitrary, but must be consistent).
  int Compare(Derivation *that);

  // Return a Derivation formed by binding values to some unbound parameters
  // of this Derivation.
  // FIXME: permit re-ordering of remaining unbound parameters.
  Derivation *BindParameters(const vector<Value *> &args);

  // Get the kind of generic entity this value represents.
  GenericKind Kind();

  // Cast this Derivation to something else.
  Class *CastToClassObject();
  Const *CastToTypeValue();

  // The family of class derivations these bindings belong to.
  Generic            *m_entity      = nullptr;

  // The generic value type corresponding to the remaining unbound parameters.
  Type                m_type;

  // Order in which remaining unbound parameters are to be bound.  Values are
  // indices of m_bindings.
  vector<int>         m_params;

  // Parameter bindings.  An unbound parameter has a null pointer.  Element
  // order is the same as the parameter declaration order in the original,
  // non-specialized declaration.
  vector<Value *>     m_bindings;

private:
  // Cast helper.
  Entity *CastToEntity();
};
