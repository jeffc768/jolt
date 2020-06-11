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

// A overload set entity describes a collection of Jolt methods with the same
// name but different signatures.  It may also include a field with the same
// name.

#pragma once

#include "Entity.h"
#include "Scope.h"
#include "type/Type.h"

class Apply;
class Argument;
class Field;
class Method;
class String;

enum Epoch: uint8_t;

class OverloadSet: public Entity {
  DECLARE_ENTITY(OverloadSet)

protected:
  void DeflateFields(Deflator &DF);
  OverloadSet(Inflator &IF);

public:
  OverloadSet(Entity *parent, StringHelper name, vector<Method *> &methods);

  virtual void BindNames(SymbolTable &st);
  void BindNames(SymbolTable &st, bool (*filter)(Entity *e));
  void BindNamesInAttributes(SymbolTable &st);
  virtual EntityScoping Scoping() { return es_member; }

  virtual void ResolveFully();

  virtual Node *AsValue(Location sl);
  virtual void *GlobalStorage(Epoch ep);
  virtual const std::string &GlobalStorageName();
  virtual ResolutionState GetResolutionState();
  virtual void AddToClosure(TranslateClosure *tc);
  virtual void FinalizeClosure(TranslateClosure *tc);

  // Assign and set vtable slots for the methods in this set.
  bool HasVirtualMethods();
  void AssignVtblSlots(size_t &vtblSize);
  void SetVtblSlots();

  // Set generator of methods, to be executed once all user-supplied methods
  // have resolved their signatures.
  void SetGenerator(void (Class::*gen)(OverloadSet *));

  // Set list of inherited OverloadSets with the same name.
  void SetInherited(InheritedEntities &ies);

  // Lookup the best matching method signature for an actual argument list.  If
  // successful, also returns a mapping of actual arguments to corresponding
  // Argument objects.
  Method *Lookup(Apply *an, vector<Argument *> &argEntities,
                 Argument *&returnValue, bool constReceiver);

  // Find a method with a specific signature.  Two signatures currently
  // supported.  Note that this might be an inherited method.  Used to see
  // if there's a need for an auto-generated method.
  Method *Find();           // no arguments
  Method *Find(Type t);     // single ref argument

  void AddGeneratedMethod(Method *me);

  bool HasExplicitDestructor();

  bool HasEmptyArgListOverload();

private:
  // Helper method for looking up a signature.
  Method *LookupFinalize(Method *me, vector<Argument *> &argEntities,
                         Argument *&returnValue);

  // All the signatures of a (potentially) overloaded method.
  vector<Method *>    m_methods;

  InheritedEntities   m_inherited;
  void                (Class::*m_generator)(OverloadSet *) = nullptr;

  bool                m_resolved = false;
};
