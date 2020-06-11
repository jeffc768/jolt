// Copyright (c) 2015, Jeff Cohen
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

// A field entity describes a data member of a Jolt class.

#pragma once

#include "Entity.h"
#include "type/Type.h"

enum Epoch: uint8_t;

class TargetGlobal;

namespace AST {
  struct VarInfo;
}

class Field: public Entity {
  DECLARE_ENTITY(Field)

  enum state_t: uint8_t {
    st_initial,
    st_type_known,
    st_resolved
  };

  state_t             m_state           = st_initial;

protected:
  void DeflateFields(Deflator &DF);
  Field(Inflator &IF);

  virtual void AppendToHash(SHA512 &hash);

public:
  Field(AST::VarInfo &vi);

  Field(Entity *parent, StringHelper name, Type t);

  virtual void BindNames(SymbolTable &st);
  virtual EntityScoping Scoping() { return es_member; }
  Node *GetInitializer();

  void ResolveType();
  virtual void ResolveFully();

  Type GetType() { return m_type; }
  virtual ResolutionState GetResolutionState();
  virtual void AddToClosure(TranslateClosure *tc);
  virtual void FinalizeClosure(TranslateClosure *tc);

  // These are valid only for global fields; i.e., members of namespaces.
  virtual Node *AsValue(Location sl);
  virtual void *GlobalStorage(Epoch ep);
  virtual const std::string &GlobalStorageName();

  // Only valid for class members.
  size_t GetOrdinal();
  void SetOrdinal(size_t ord);
  size_t GetOffset();
  void SetOffset(size_t offset);

  // Get the target object for this field.
  TargetGlobal *GetTarget(Epoch ep);

private:
  // Field initializer.  This expression tree is shared by all constructors
  // that do not explicitly construct the field themselves.
  Expr               *m_initexpr        = nullptr;
  bool                m_sharedInit      = false;

  // Field type.
  Type                m_type;
  Expr               *m_typeExpr        = nullptr;


  // Offset of this fields within the parent class.  Unused for namespace
  // members.
  size_t              m_offset          = 0;

  // Ordinal of this field within the parent class.  Unused for namespace
  // members.
  size_t              m_ord             = 0;

  // The target objects for this field, one for each epoch.  Unused for
  // class members.
  TargetGlobal       *m_runTarget       = nullptr;
  TargetGlobal       *m_compileTarget   = nullptr;
};
