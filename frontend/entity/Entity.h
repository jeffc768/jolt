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

// An entity represents a declaration, more or less.  Examples of entities are
// classes, namespaces and methods, basically anything that can be given a name.
// Entities usually have dependencies on other dependencies, so class Entity,
// the base class for all entity classes,inherits from WorkUnit.
//
// In addition to dependency management and having a name in one or more scopes,
// all entities may also have member attributes associated with them.  This is
// represented by AttributeList, a collection of attribute expressions.  Each
// entity has an AttributeList listing the member attributes syntactically
// associated with the entity's declaration.  This AttributeList may point to
// an AttributeList listing the global member attributes, which in turn may
// point to another one listing the member attributes of the enclosing
// namespace, and so on.

#pragma once

#include "parser/TokenHelpers.h"
#include "parser/Location.h"
#include "util/Object.h"
#include <string>

class AttributeList;
class Entity;
class Expr;
class Location;
class Namespace;
class Node;
class SHA512;
class String;
class SymbolTable;
class TranslateClosure;

enum Epoch: uint8_t;

enum EntityKind {
  ek_Ambiguous,
  ek_Argument,
  ek_Class,
  ek_Const,
  ek_DeferredInflate,
  ek_Field,
  ek_Generic,
  ek_Method,
  ek_Namespace,
  ek_Nullified,
  ek_OverloadSet,
  ek_Specialization,
  ek_Undetermined,
  ek_Var
};

enum EntityScoping {
  es_undetermined,
  es_generic,
  es_member,
  es_nonmember
};

enum Linkage: uint8_t {
  lk_internal,
  lk_import,
  lk_export,
  lk_once
};

#define DECLARE_ENTITY2(N, BASE) \
  DECLARE_OBJECT2(N, BASE) \
public: \
  virtual EntityKind Kind(); \
private:

#define DECLARE_ENTITY(N) DECLARE_ENTITY2(N, Entity)

#define IMPLEMENT_ENTITY(N) \
  IMPLEMENT_OBJECT(N) \
  EntityKind N::Kind() { return ek_##N; }

class Entity: public Object {
  // The lexical environment containing this entity.  This refers to the
  // original entity in which this one was declared, and not to entities which
  // contains this one due to a use statement.
  Entity             *m_parent          = nullptr;

  // Source location of this entity's declaration.
  Location            m_location;

  // The (original, un-macro-expanded) name of this entity within its
  // (original) container.
  String             *m_name            = nullptr;

  // Managled C++ name for this entity.
  std::string         m_externalName;

  // Member attributes of this entity.
  AttributeList      *m_attributes      = nullptr;

  // Linkage of this entity.
  Namespace          *m_module          = nullptr;
  uint32_t            m_moduleOrd       = 0;
  ::Linkage           m_linkage         = lk_internal;

  // Nearest enclosing template root.
  Parameters         *m_templateRoot    = nullptr;

  Entity             *m_nextTracked     = nullptr;

protected:
  Entity(Entity *parent, Location loc, StringHelper name);

  void DeflateFields(Deflator &DF);
  Entity(Inflator &IF);

  void SetLocation(Location loc) {
    if (!m_location)
      m_location = loc;
  }

  virtual void AppendToHash(SHA512 &hash);

  // Under certain conditions, entities may bypass the normal hash-based
  // external name generation.
  void BypassNameMangling();

public:
  using base_t = Object;
  static Metadata s_metadata;

  void SetLinkage(::Linkage lk) { m_linkage = lk; }

  Entity *Parent();
  virtual void SetParent(Entity *chain);
  virtual void SetParent(Entity *parent, AttributeList *attrGroup);
  void SetNameAndParent(Entity *parent, String *name);

  static void SetModuleName(const std::string &name);

  // Top-down walk of the parse tree to bind names to the extend possible.
  virtual void BindNames(SymbolTable &st);
  virtual Parameters **GetTemplateRootHolder();

  virtual void ResolveFully() { }

  void TrackResolution();
  static Entity *GetNextTracked();

  void DeflateImportReference(Deflator &DF);
  void SetImportReference(Namespace *ns, uint32_t ord);
  static Entity *InflateImportReference(Inflator &IF);

  String *Name() { return m_name; }
  const std::string &ExternalName();

  // Retrieves entity by name.  Returns nullptr if not found.  Only
  // overridden by Class and Namespace.
  virtual Entity *LookupEntity(String *name);

  ::Linkage Linkage() { return m_linkage; }

  AttributeList *Attributes() { return m_attributes; }
  void SetAttributes(AttributeList *al) { m_attributes = al; }

  virtual EntityKind Kind() = 0;
  virtual EntityScoping Scoping() = 0;

  Location GetLocation() { return m_location; }

  // Return a new sub-expression that represents the entity.
  virtual Node *AsValue(Location sl);

  // Get the storage that holds the entity's value at compile time.
  virtual void *GlobalStorage(Epoch ep);

  // Get the name of the global variable that represents the entity's value
  // in the generated C++.
  virtual const std::string &GlobalStorageName();

  // Get resolution status of the entity.  Can only be used on fully resolved
  // entities.
  enum ResolutionState { rs_ok, rs_hasUnbound, rs_hasError };
  virtual ResolutionState GetResolutionState() = 0;

  // Add this entity to the closure, generating code as appropriate and
  // identifying other entities to add to the closure.
  virtual void AddToClosure(TranslateClosure *tc);
  virtual void FinalizeClosure(TranslateClosure *tc);

  // Mark this entity as executable at compile time; it and all of its
  // dependencies have been translated.
  virtual void SetExecutable();
};

namespace messageimpl {
  inline Location GetLocation(Entity *e) {
    return e->GetLocation();
  }
}
