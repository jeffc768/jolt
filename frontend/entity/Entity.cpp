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

#include "Entity.h"
#include "AttributeList.h"
#include "Namespace.h"
#include "Parameters.h"
#include "node/Expr.h"
#include "node/Ident.h"
#include "parser/Location.h"
#include "parser/SymbolTable.h"
#include "target/Target.h"
#include "util/InDeflator.h"
#include "util/SHA512.h"
#include "util/String.h"
#include <cctype>

Metadata Entity::s_metadata { &Object::s_metadata };

static Entity *g_tracked = nullptr;
static std::string g_moduleName;

Entity::Entity(Entity *parent, Location loc, StringHelper name)
    : Object(),
      m_parent(parent),
      m_location(loc),
      m_name(name.m_string) {
}

void Entity::DeflateFields(Deflator &DF) {
  DF << m_parent;
  DF << m_location;
  DF << m_name;
  DF << m_attributes;
  DF << m_externalName;
  DF << m_templateRoot;

  // FIXME: probably do not want to clone the attributes for each generic
  // derivation, as they do not participate in the genericity, but how do
  // we know which attributes need to be deflated and which do not?
}

void Entity::DeflateImportReference(Deflator &DF) {
  verify(m_module != nullptr);
  DF << m_module;
  DF << m_moduleOrd;
}

Entity::Entity(Inflator &IF)
    : Object(IF) {
  IF >> m_parent;
  IF >> m_location;
  IF >> m_name;
  IF >> m_attributes;
  IF >> m_externalName;
  IF >> m_templateRoot;

  // FIXME: we have to be careful.  For some entities, like Generic, the
  // concept of linkage is dubious, especially for entities nested within
  // a method body.

  // Note that when a generic entity is imported, its prestine copys is
  // inflated and re-deflated.  This will cause it to lose the import
  // linkage, which is the correct semantics, as new derivations need to
  // be translated to object code--even if other modules might have already
  // created that derivation.  As with C++, the linker sorts it out.
  m_linkage = IF.InModuleMode() ? lk_import : lk_once;
}

void Entity::SetImportReference(Namespace *ns, uint32_t ord) {
  m_module = ns;
  m_moduleOrd = ord;
  verify(ns);
}

Entity *Entity::InflateImportReference(Inflator &IF) {
  Namespace *ne;
  uint32_t ord;
  IF >> ne;
  IF >> ord;

  ModuleInflator &MIF = *ne->GetModuleInflator();
  Object *obj = MIF(ord);
  auto e = safe_cast<Entity *>(obj);
  void *p = ne->GlobalStorage(ep_compile);
  e->m_module = reinterpret_cast<Namespace *>(p);
  e->m_moduleOrd = ord;
  return e;
}

Entity *Entity::Parent() {
  return m_parent;
}

void Entity::SetParent(Entity *parent) {
  m_parent = parent;
  if (parent)
    m_linkage = parent->Linkage();
}

void Entity::SetNameAndParent(Entity *parent, String *name) {
  m_name = name;
  SetParent(parent);
}

void Entity::SetModuleName(const std::string &name) {
  g_moduleName = name;
}

void Entity::BindNames(SymbolTable &st) {
  if (!st.IsRedo())
    m_templateRoot = st.CurrentTemplate();
}

Parameters **Entity::GetTemplateRootHolder() {
  return &static_cast<Parameters *&>(m_templateRoot);
}

void Entity::TrackResolution() {
  // Ignore redundant tracking.  Note that even after removed from this list,
  // m_nextTracked is not reset.
  // FIXME: the tail of the list, of course, will have a null pointer.  This
  // *shouldn't* be a problem, as that entity will be a Namespace, and those
  // will never be redundantly added.
  if (!m_nextTracked) {
    m_nextTracked = g_tracked;
    g_tracked = this;
  }
}

Entity *Entity::GetNextTracked() {
  Entity *e = g_tracked;
  if (e)
    g_tracked = e->m_nextTracked;
  return e;
}

void Entity::SetParent(Entity *parent, AttributeList *attrGroup) {
  m_parent = parent;
  if (parent)
    m_linkage = parent->Linkage();

  if (m_attributes) {
    m_attributes->m_parent = attrGroup;
  } else {
    m_attributes = attrGroup;
  }
}

const std::string &Entity::ExternalName() {
  if (!m_externalName.empty())
    return m_externalName;

  // Internal entities have names that already contain type-based hashes.
  if (m_parent == Namespace::InternalNamespace()) {
    m_externalName = m_name->c_str();
    return m_externalName;
  }

  // The external name starts with the printable characters from the
  // entity name.
  for (const char *p = m_name->c_str(); *p; p++) {
    // FIXME: not all OSes, i.e. FreeBSD, can handle all printable chars!
    if (isprint(*p))
      m_externalName += *p;
    else
      m_externalName += '_';
  }

  m_externalName += '_';

  // Compute hash of full name and parent entity.
  SHA512 hash;
  m_name->AppendToHash(hash);
  if (m_parent) {
    hash.Append('P');
    hash.Append(m_parent->ExternalName());
  } else {
    hash.Append('M');
    hash.Append(g_moduleName);
  }

  // Add entity-specific stuff to hash and append to external name.
  AppendToHash(hash);
  m_externalName += hash;

  return m_externalName;
}

void Entity::BypassNameMangling() {
  m_externalName = m_name->c_str();
}

Entity *Entity::LookupEntity(String *n) {
  verify(false);
  return nullptr;
}

Node *Entity::AsValue(Location sl) {
  verify(0 && "Internal error");
  return nullptr;
}

void *Entity::GlobalStorage(Epoch ep) {
  verify(0 && "Internal error");
  return nullptr;
}

const std::string &Entity::GlobalStorageName() {
  verify(0 && "Internal error");
  static std::string name;
  return name;
}

void Entity::AddToClosure(TranslateClosure *tc) {
  verify(false); // Missing redefinition
  // Not all entity types can be added to closures, so don't force them all to
  // redefine this method by making it abstract virtual.
}

void Entity::FinalizeClosure(TranslateClosure *tc) {
  verify(false); // Missing redefinition
  // Ditto for this one too.
}

void Entity::SetExecutable() {
  // Default is a no-op.
}

void Entity::AppendToHash(SHA512 &hash) {
  verify(false);  // Must be implemented in relevant subclases.
}
