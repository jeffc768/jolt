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

// A namespace entity is the representation of a Jolt namespace.

#pragma once

#include "Entity.h"
#include "parser/ParserDecls.h"
#include <unordered_map>

class AttributeList;
class Generic;
class Location;
class ModuleInflator;
class Specialization;
class Scope;
class Token;
class Value;

enum Epoch: uint8_t;

// Declares a static variable that registers a callback to populate one or more
// namespaces.
struct PopulateNamespace {
  using CB = void (*)();
  CB                  m_callback;

  PopulateNamespace  *m_next;

  PopulateNamespace(CB cb);
};

class Namespace: public Entity {
  DECLARE_ENTITY(Namespace)

  enum state_t: uint8_t {
    st_initial,
    st_resolved
  };

  state_t             m_state       = st_initial;

protected:
  void DeflateFields(Deflator &DF);
  Namespace(Inflator &IF);

  virtual void AppendToHash(SHA512 &hash);

public:
  Namespace(Namespace *parent, String *name);

  virtual EntityScoping Scoping() { return es_nonmember; }

  // Populate entity with declarations from a namespace syntax tree.
  void Populate(AST::SafeArray<AST::DeclInfo> decls, AttributeList *attrs);

  static void CreateModuleNamespace(bool genstd);
  static Namespace *ModuleNamespace();
  static Namespace *StdNamespace();
  static Namespace *ExportNamespace();
  static Namespace *InternalNamespace();

  // Add built-in entity to this namespace.
  void AddBuiltinEntity(Entity *e);

  // Add built-in generic entity to this namespace.
  Generic *AddBuiltinGeneric(Specialization *s);

  virtual void BindNames(SymbolTable &st);
  virtual Entity *LookupEntity(String *name);

  virtual void ResolveFully();

  virtual Node *AsValue(Location sl);
  virtual void *GlobalStorage(Epoch ep);
  virtual ResolutionState GetResolutionState();
  virtual void AddToClosure(TranslateClosure *tc);
  virtual void FinalizeClosure(TranslateClosure *tc);

  static Namespace *GetFromObject(Value *v);

  Scope *NamespaceScope() { return m_namespaceScope; }

  static Namespace *FindModule(String *name);
  const std::string &GetModuleFileName() { return m_moduleFileName; }
  String *GetModuleName() { return m_moduleName; }
  void SetModuleName(String *s) { m_moduleName = s; }
  ModuleInflator *GetModuleInflator() { return m_inflator; }

  template<class F> static void EnumerateImportedModules(F &&functor) {
    for (auto &I : s_importedModules)
      functor(I.second);
  }

private:
  // Helper function to map a namespace specifier to a namespace entity.
  Namespace *MapNameToEntity(AST::SafeArray<Token *> ns);

  // Helper function to link namespace to an imported module.
  void LinkToModule(AST::SafeArray<Token *> ns);
  Namespace *LinkToModule(Token *t, String *module);

  // The namespace scope.
  Scope              *m_namespaceScope  = nullptr;

  // Name of module to which this namespace is linked.  That includes the top
  // level export namespace.
  String             *m_moduleName      = nullptr;

  // File name of imported module.
  std::string         m_moduleFileName;

  ModuleInflator     *m_inflator        = nullptr;

  using ImportMap = std::unordered_map<String *, Namespace *>;
  static ImportMap s_importedModules;

  vector<AttributeList *> m_ubergroups;
  vector<AttributeList *> m_groups;
};
