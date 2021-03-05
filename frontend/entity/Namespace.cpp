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

#include "Namespace.h"
#include "AttributeList.h"
#include "Class.h"
#include "Const.h"
#include "Field.h"
#include "Method.h"
#include "Scope.h"
#include "Specialization.h"
#include "node/Expr.h"
#include "node/Literal.h"
#include "parser/SymbolTable.h"
#include "parser/Token.h"
#include "target/Target.h"
#include "type/Type.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/SHA512.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_ENTITY(Namespace)

// Predefined namespaces.
static Namespace *g_ModuleNamespace;
static Namespace *g_StdNamespace;
static Namespace *g_ExportNamespace;
static Namespace *g_InternalNamespace;

// A mapping of all imported module names to their respective namespaces.
Namespace::ImportMap Namespace::s_importedModules;

static void AddPredefinedEntities();

static PopulateNamespace *g_pnCallbacks = nullptr;

PopulateNamespace::PopulateNamespace(CB cb)
    : m_callback(cb),
      m_next(g_pnCallbacks) {
  g_pnCallbacks = this;
}

Namespace::Namespace(Namespace *parent, String *name)
    : Entity(parent, { }, name),
      m_namespaceScope(new Scope()) {
  TrackResolution();

  if (parent) {
    SetLinkage(parent->Linkage());
    parent->m_namespaceScope->AddEntity(this);
  }
}

void Namespace::DeflateFields(Deflator &DF) {
  verify(m_state == st_resolved);

  // We musn't deflate the parent of the export namespace, as that will cause
  // *everything* to be exported.  This is OK, as upon importation its parent
  // will be reset to that of the namespace mapped to this module.
  if (this == g_ExportNamespace)
    SetParent(nullptr);

  // FIXME: *all* namespaces can be deflated, not just those under namespace
  // export.  Entites in other namespaces can be referenced by stuff that is
  // exported, and their parent chain will be deflated too.  Perhaps the scopes
  // of such namespaces ought not be deflated.
  //
  // BUT:  Until generics are captured in a partially resolved state--as far as
  // they can be resolved with unbound generic parameters--any namespace lookup
  // will fail if we don't deflate the scopes.  This, of course, will drag
  // *everything* into the exported module--though only that which is actually
  // used will be inflated.
  //
  // Even once generics are properly handled, there is still the case of
  // "a_namespace.P$", where P is a generic parameter, or an expression using
  // a generic parameter.  Perhaps this is exotic enough to be handled with a
  // compilation error.
  Entity::DeflateFields(DF);
  DF << m_namespaceScope;
  DF << m_moduleName;

  if (this == g_ExportNamespace)
    SetParent(g_ModuleNamespace);
}

Namespace::Namespace(Inflator &IF) : Entity(IF) {
  // FIXME: what about the predefined namespaces, such as jolt?  We want to use
  // the instance already in the importing module.  We also have to deal with
  // exported references to namespaces that were themselves imported, and might
  // have already been or will be imported as well.
  IF >> m_namespaceScope;
  IF >> m_moduleName;
}

void Namespace::Populate(AST::SafeArray<AST::DeclInfo> decls,
                         AttributeList *attrs) {
  // Populate namespace scope with new entities corresponding to the
  // declarations in its syntax tree.  This may be called multiple times on
  // a namespace, as namespaces are allowed to be broken up into multiple
  // syntactic units.

  // Namespaces are unique in that they do not have member attributes.  Any
  // member attributes syntactically associated with a namespace definition
  // are propagated to the member entities of the namespace.  An important
  // consequence is that namespaces are always built, and this is taken
  // advantage of.  For example, the members of a namespace are immediately
  // put onto the ready queue, and nested namespaces are immediately recursed
  // into.
  //
  // The global attributes initially are the member attributes of the current
  // syntactic unit of this namespace.
  AttributeList *attrgroup = attrs;
  if (attrs)
    m_ubergroups.push_back(attrs);

  // Process the declaration list.  Unlike with namespaces, these members
  // cannot be immediately placed onto the ready queue.  We must wait until
  // the class itself becomes ready.  Right now, for all we know, the class
  // has build(false).
  for (AST::DeclInfo &di : decls) {
    AttributeList *al = di.m_attrs;
    if (al && di.m_decltype != AST::DeclInfo::dt_attrgroup) {
      al->m_parent = attrgroup;
    } else {
      al = attrgroup;
    }

    switch (di.m_decltype) {
      case AST::DeclInfo::dt_empty:
        continue;

      case AST::DeclInfo::dt_normal: {
        Entity *e = di.m_normal;

        if (auto me = dyn_cast<Method *>(e)) {
          if (me->MethodKind() == mk_construct) {
            EmitError(me) << "Constructors may only be declared in classes.";
            continue;
          }

          if (me->MethodKind() == mk_destruct) {
            EmitError(me) << "Denstructors may only be declared in classes.";
            continue;
          }
        }

        // Add entity to the common scope.  Because there may be multiple
        // entities with the same name, and we don't know yet which (if any)
        // are going to be built, for now lump them altogether under an
        // Undetermined and deal with it later.  Likewise, we don't yet know
        // whether it may belong in the static or instance scope, so just dump
        // it in the common scope for now and sort it out later, after the
        // member attributes have been resolved.
        m_namespaceScope->AddUndetermined(this, e);
        e->SetParent(this, al);
        continue;
      }

      case AST::DeclInfo::dt_fields: {
        AST::SafeArray<Field *> ary { di.m_fields };
        for (Field *fe : ary) {
          m_namespaceScope->AddUndetermined(this, fe);
          fe->SetParent(this, al);
        }
        continue;
      }

      case AST::DeclInfo::dt_attrgroup:
        attrgroup = di.m_attrs;
        attrgroup->m_parent = attrs;
        m_groups.push_back(attrgroup);
        continue;

      case AST::DeclInfo::dt_namespace: {
        if (Namespace *ne = MapNameToEntity(di.m_namespace.m_names))
          ne->Populate(di.m_namespace.m_decls, al);
        continue;
      }

      case AST::DeclInfo::dt_import: {
        if (Namespace *ne = MapNameToEntity(di.m_import.m_namespaceNames))
          ne->LinkToModule(di.m_import.m_moduleNames);
        continue;
      }
    }
  }
}

void Namespace::CreateModuleNamespace(bool genstd) {
  verify(!g_ModuleNamespace);
  g_ModuleNamespace = new Namespace(nullptr, String::Get("<module>"));
  g_StdNamespace = new Namespace(g_ModuleNamespace, String::Get("std"));
  g_InternalNamespace = new Namespace(nullptr, String::Get("<internal>"));
  g_InternalNamespace->SetLinkage(lk_once);

  if (genstd) {
    // This compilation generates the std module.  Create the relevant
    // namespaces, and alias "export" to "std".
    g_ExportNamespace = g_StdNamespace;

    // Populate namespaces with predefined stuff.
    AddPredefinedEntities();
    Class::CreateWellKnownClasses();
  } else {
    // Import the std module, and create a separate export namespace.
    g_StdNamespace = g_StdNamespace->LinkToModule(nullptr, String::Get("std"));
    g_ExportNamespace = new Namespace(g_ModuleNamespace, String::Get("export"));
  }

  g_ExportNamespace->SetLinkage(lk_export);
}

Namespace *Namespace::ModuleNamespace() {
  return g_ModuleNamespace;
}

Namespace *Namespace::StdNamespace() {
  return g_StdNamespace;
}

Namespace *Namespace::ExportNamespace() {
  return g_ExportNamespace;
}

Namespace *Namespace::InternalNamespace() {
  return g_InternalNamespace;
}

void Namespace::AddBuiltinEntity(Entity *e) {
  m_namespaceScope->AddEntity(e);
  e->TrackResolution();
}

Generic *Namespace::AddBuiltinGeneric(Specialization *s) {
  s->TrackResolution();
  vector<Entity *> defs;
  defs.push_back(s);
  Generic *ge = new Generic(this, defs);
  AddBuiltinEntity(ge);
  return ge;
}

void Namespace::BindNames(SymbolTable &st) {
  // Note: no Entity::BindNames(), as namespaces can't be in a template.

  // Note: can't bind imported namespaces.
  if (m_state == st_resolved)
    return;

  for (auto g : m_ubergroups)
    g->BindNames(st);

  SymbolTable::PushScope scope(st);

  st.AddSymbols(m_namespaceScope);
  m_namespaceScope->BindNamesInAttributes(st);
  m_namespaceScope->BindNames(st);

  for (auto g : m_groups)
    g->BindNames(st);
}

Entity *Namespace::LookupEntity(String *name) {
  return m_namespaceScope->LookupEntity(name);
}

void Namespace::ResolveFully() {
  if (m_state == st_resolved)
    return;

  m_namespaceScope->HandleNameMacros();
  m_namespaceScope->ResolveFully();

  for (size_t i = m_namespaceScope->UsedNamespaceCount(); i > 0; ) {
    Namespace *ne = m_namespaceScope->UsedNamespace(--i);
    ne->ResolveFully();
  }

  m_state = st_resolved;
}

Node *Namespace::AsValue(Location sl) {
  // The value presentation of a namespace entity is a reference to the
  // associated (compile time only) Jolt namespace object.
  // FIXME: what level of resolution is required?  Not full, as that will cause
  // deadlock.
  void *p = GlobalStorage(ep_compile);
  return new Literal(sl, Value::NewPtr(Type::Namespace(), p));
}

void *Namespace::GlobalStorage(Epoch ep) {
  verify(ep == ep_compile);
  return reinterpret_cast<void *>(this);
}

Entity::ResolutionState Namespace::GetResolutionState() {
  verify(m_state == st_resolved);
  return rs_ok;
  // FIXME: do we need more?
}

void Namespace::AddToClosure(TranslateClosure *tc) {
  // No dependencies to add.
}

void Namespace::FinalizeClosure(TranslateClosure *tc) {
  // Nothing to generate.
}

Namespace *Namespace::GetFromObject(Value *v) {
  Namespace *ne = v->As<Namespace *>();
  return ne;
}

Namespace *Namespace::FindModule(String *name) {
  auto I = s_importedModules.find(name);

  // If the desired module hasn't been imported, do so now, linking it to a new
  // namespace that shall be anonymous.
  if (I != s_importedModules.end()) {
    return I->second;
  } else {
    // The name doesn't matter, as it'll be replaced anyway.
    Namespace *ne = new Namespace(g_ModuleNamespace, String::Get(wks_this));
    return ne->LinkToModule(nullptr, name);
  }
}

Namespace *Namespace::MapNameToEntity(AST::SafeArray<Token *> ns) {
  // FIXME: handle expr$ in specifier.
  Namespace *ne = this;

  for (Token *n : ns) {
    auto s = n->StringValue();

    // Lookup identifier in current scope.  If present, it must refer
    // exclusively to a namespace entity; otherwise, a new namespace entity
    // is created and inserted into the scope.

    Entity *e = ne->m_namespaceScope->RawGet(s);
    if (e) {
      if ((ne = dyn_cast<Namespace *>(e))) {
        if (ne->Linkage() == lk_import)  {
          EmitError(n) << s << " is an imported module.";
          return nullptr;
        }
      } else {
        EmitError(n) << s << " is not a namespace.";
        return nullptr;
      }
      continue;
    }

    ne = new Namespace(ne, s);
    ne->SetLocation(n->GetLocation());
  }

  return ne;
}

extern vector<std::string> g_moduleSearchPaths;

void Namespace::LinkToModule(AST::SafeArray<Token *> ns) {
  // FIXME: handle expr$ in specifier.
  // FIXME: disallow unusual characters, especially / and ..
  // FIXME: don't allow linking a namespace that's under export.
  std::string s(ns[0]->StringValue()->c_str());
  for (uint32_t i = 1; i < ns.size(); i++) {
    s += '.';
    s += ns[i]->StringValue()->c_str();
  }

  this->LinkToModule(ns[0], String::Get(s));
}

Namespace *Namespace::LinkToModule(Token *t, String *module) {
  // Search for the module in the defined locations.
  for (auto &path : g_moduleSearchPaths) {
    std::string fn = path + '/' + module->c_str() + ".jmod";
    if (FILE *f = fopen(fn.c_str(), "rb")) {
      ModuleInflator *IF = new ModuleInflator(f, fn);
      auto ne = safe_cast<Namespace *>((*IF)(0));

      String *name = Name();
      auto parent = safe_cast<Namespace *>(Parent());
      ne->SetNameAndParent(parent, name);
      ne->SetLinkage(lk_import);
      ne->m_moduleFileName = fn;
      ne->m_inflator = IF;
      verify(ne->m_moduleName == module);

      IF->SetLinkedNamespace(ne);
      ne->SetImportReference(ne, 0);

      // If no token, then this an indirect import triggered by another
      // imported module.  Leave the linked namespace inaccessible to Jolt
      // code.
      if (t)
        parent->NamespaceScope()->Replace(name, ne);

      s_importedModules[ne->m_moduleName] = ne;

      // FIXME: the contents of a module are highly indeterminant, which means
      // that whenever the Jolt compiler is rebuilt, the std module changes,
      // which means that *all* other modules must be rebuilt.  Clearly
      // unacceptable.  The deflation of unordered_maps are the main cause;
      // they need to be ordered when deflated.
      //
      // This doesn't avoid the problem when the std module itself changes,
      // however, even if only by the addition of something new that ought
      // have no effect on existing importers.

      fclose(f);
      return ne;
    }
  }

  if (t)
    EmitError(t) << "Module " << module << " cannot be found.";
  else
    EmitError() << "Module " << module << " cannot be found.";
  return nullptr;
}

/******************************************************************************/

static void AddNative(Namespace *ne, Scope *s, const char *name,
                      Method::NativeMethods nm, Type t) {
  Method *me = new Method(ne, name, nm, "operand", t);
  s->AddUndetermined(ne, me);
}

static void AddConst(Namespace *ne, const char *name, Type t) {
  Type tt = Type::JType();
  Const *ce = new Const(ne, name, tt, &t, sizeof(t));
  ne->AddBuiltinEntity(ce);
}

static void AddPredefinedEntities() {
  // These two native methods are useful hacks to get something printed.
  // The final overload relies on the hack that strings are typed as String.
  Namespace *ne = g_StdNamespace;
  Scope *s = ne->NamespaceScope();
  AddNative(ne, s, "Print", Method::nm_print_int, Type::Int());
  AddNative(ne, s, "Print", Method::nm_print_long, Type::Long());
  AddNative(ne, s, "Print", Method::nm_print_uint, Type::UInt());
  AddNative(ne, s, "Print", Method::nm_print_ulong, Type::ULong());
  AddNative(ne, s, "Print", Method::nm_print_bool, Type::Bool());
  AddNative(ne, s, "Print", Method::nm_print_string, Type::StringRef());
  AddNative(ne, s, "Print", Method::nm_print_char, Type::Char8());
  AddNative(ne, s, "Print", Method::nm_print_float, Type::Float());
  AddNative(ne, s, "Print", Method::nm_print_double, Type::Double());

  // Hack to define subrange types via .. until tuples are implemented.
  Method *me = new Method(ne, wks_op_range, Method::nm_signed_subrange,
                          Type::JType(),
                          "lb", Type::Long(), "ub", Type::Long());
  s->AddUndetermined(ne, me);

  me = new Method(ne, wks_op_range, Method::nm_unsigned_subrange, Type::JType(),
                  "lb", Type::ULong(), "ub", Type::ULong());
  s->AddUndetermined(ne, me);

  // Predefined operator new/delete methods.
  me = new Method(ne, wks_op_new, Method::nm_operator_new, Type::Address(),
                  "size", Type::SizeT());
  s->AddUndetermined(ne, me);

  me = new Method(ne, wks_op_new,
                  Method::nm_operator_placement_new, Type::Address(),
                  "ptr",  Type::Address(), "size", Type::SizeT());
  s->AddUndetermined(ne, me);

  AddNative(ne, s, "operator delete", Method::nm_operator_delete,
            Type::Address());

  me = new Method(ne, wks_op_delete, Method::nm_operator_placement_delete,
                  "ptr",  Type::Address(), "addr", Type::Address());
  s->AddUndetermined(ne, me);

  // Built-in type that should be declared here.
  AddConst(ne, "namespace", Type::Namespace());

  for (PopulateNamespace *pn = g_pnCallbacks; pn; pn = pn->m_next)
    (*pn->m_callback)();
}

void Namespace::AppendToHash(SHA512 &hash) {
  hash.Append('N');
}
