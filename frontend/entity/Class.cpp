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

#include "Class.h"
#include "Ambiguous.h"
#include "AttributeList.h"
#include "BaseSpecifier.h"
#include "Const.h"
#include "DeferredInflate.h"
#include "Field.h"
#include "Method.h"
#include "Namespace.h"
#include "OverloadSet.h"
#include "Scope.h"
#include "Specialization.h"
#include "Undetermined.h"
#include "node/Expr.h"
#include "node/GlobalAddr.h"
#include "node/Literal.h"
#include "parser/SymbolTable.h"
#include "parser/Token.h"
#include "target/Target.h"
#include "target/TranslateClosure.h"
#include "type/Type.h"
#include "util/InDeflator.h"
#include "util/Value.h"
#include "util/Message.h"
#include "util/SHA512.h"
#include "util/String.h"
#include <algorithm>
#include <unordered_set>

/******************************************************************************/

IMPLEMENT_ENTITY(Class)

static Class *g_WellKnownClasses[Class::wkc_not_well_known];

// Do not allow certain names to be defined, as they have special meaning
// as members of a class.
static bool ValidateName(Entity *e) {
  if (e->Name() == wks_class) {
    EmitError(e) << "The \"class\" member of classes cannot be "
                    "explicitly declared";
    return false;
  } else if (e->Name() == wks_this) {
    EmitError(e) << "The \"this\" member of classes cannot be "
                    "explicitly declared";
    return false;
  }

  return true;
}

Class::Class(Token *name, AST::SafeArray<AST::BaseInfo> bases,
             AST::SafeArray<AST::DeclInfo> decls)
    : Entity(nullptr, name->GetLocation(), name),
      m_commonScope(new Scope()),
      m_memberScope(new Scope()) {
  m_commonScope->SetClass(this);
  m_memberScope->SetClass(this);
  m_metaClass = new Class(this);

  // Add a definition for "this" in the common scope to refer to this class.
  String *s = String::Get(wks_this);
  Const *ce = new Const(this, s, this);
  m_commonScope->AddEntity(ce);

  // Run through the base classes.
  for (AST::BaseInfo &bi : bases)
    m_bases.push_back(new BaseSpecifier(bi));

  // We need to remember this, even if it turns out that all of the bases
  // have @build(false), for the symbol table.
  m_hadBaseSpecifiers = bases.size() > 0;

  // The global attributes that were passed in applied to this class entity.
  // We now need to track global attributes that are declared in the body of
  // the class declaration.  Initially, there are none.
  AttributeList *attrgroup = nullptr;

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

        if (!ValidateName(e))
          continue;

        // There may be multiple entities with the same name, and we don't
        // know yet which (if any) are going to be built.  For now lump them
        // altogether under an Undetermined and deal with it later.
        e->SetParent(this, al);
        AddUndetermined(e);
        continue;
      }

      case AST::DeclInfo::dt_fields: {
        AST::SafeArray<Field *> ary { di.m_fields };
        for (Field *fe : ary) {
          if (ValidateName(fe)) {
            fe->SetParent(this, al);
            AddUndetermined(fe);
          }
        }
        continue;
      }

      case AST::DeclInfo::dt_attrgroup:
        attrgroup = di.m_attrs;
        m_groups.push_back(attrgroup);
        continue;

      case AST::DeclInfo::dt_namespace: {
        Token *name = di.m_namespace.m_names[0];
        Location sl = name->GetLocation();
        EmitError(sl) << "Namespaces may be nested only within other "
                         "namespaces.";

        // FIXME: technically, we're leaking all the memory in the tree
        // rooted at di.m_namspace.
        continue;
      }

      case AST::DeclInfo::dt_import: {
        Token *name = di.m_import.m_namespaceNames[0];
        Location sl = name->GetLocation();
        EmitError(sl) << "Module imports may be nested only within namespaces.";

        // Don't leak memory.
        AST::SafeArray<Token *> n1(di.m_import.m_namespaceNames);
        AST::SafeArray<Token *> n2(di.m_import.m_moduleNames);
        continue;
      }
    }
  }
}

Class::Class(Class *ce)
    : Entity(ce->Parent(), ce->GetLocation(),
             ce->Name()->Concat(" metaclass", 10)),
      m_metaInstance(ce),
      m_bindings(ce->m_bindings),
      m_commonScope(ce->m_commonScope),
      m_memberScope(new Scope()),
      m_hasDestructor(NO) {
  m_memberScope->SetClass(this);
}

Class::Class(Entity *parent, WellKnownClassId id, const char *name, Class *base)
    : Entity(parent, { }, String::Get(name)),
      m_id(id),
      m_commonScope(new Scope()),
      m_memberScope(new Scope()) {
  m_commonScope->SetClass(this);
  m_memberScope->SetClass(this);
  m_metaClass = new Class(this);

  if (base)
    m_bases.push_back(new BaseSpecifier(base));

  g_WellKnownClasses[id] = this;
}

void Class::AddBase(Class *base) {
  m_bases.push_back(new BaseSpecifier(base));
}

void Class::AddUndetermined(Entity *e) {
  AttributeList *al = e->Attributes();
  bool isStatic = al && al->HasStatic();

  switch (e->Kind()) {
    case ek_Field: {
      Field *fe = static_cast<Field *>(e);
      fe->SetOrdinal(++m_fieldOrd);  // tentative, to preserve order
      if (isStatic) {
        fe->SetParent(m_metaClass);
        m_metaClass->m_memberScope->AddUndetermined(m_metaClass, e);
      } else {
        m_memberScope->AddUndetermined(this, e);
      }
      break;
    }

    case ek_Method: {
      Method *me = static_cast<Method *>(e);
      switch (me->MethodKind()) {
        case mk_construct:
          isStatic = true;
          break;
        case mk_destruct:
          isStatic = false;
          break;
        default:
          // FIXME: check for operator=, operator-> ??
          break;
      }

      if (isStatic) {
        me->MakeStatic(m_metaClass);
        m_metaClass->m_memberScope->AddUndetermined(m_metaClass, e);
      } else {
        me->MakeInstance(this);
        m_memberScope->AddUndetermined(this, e);
      }
      break;
    }

    default:
      m_commonScope->AddUndetermined(this, e);
      break;
  }
}

void Class::PreDestroy() {
  delete[] m_vtable;
  if (m_type)
    m_type.UpdateClass(nullptr);
}

void Class::DeflateFields(Deflator &DF) {
  Entity::DeflateFields(DF);
  DF << WX(m_state);
  DF << m_metaClass;
  DF << m_metaInstance;
  DF << m_bases;
  DF << m_commonScope;
  DF << m_memberScope;
  DF << m_fields;
  DF << m_hasDestructor;

  // Note: There may be a m_type set, but don't deflate it.  Inflation
  // creates a new class entity, which is it's own distinct type.

  if (m_state == st_resolved) {
    DF << m_id;
    DF << m_bindings;
    DF << m_size;
    DF << m_vtblSize;
    DF << m_vtblPrefixSize;
    DF << m_vtblLocation;
    DF << m_cppClassObject;

    // Note: m_vtable is not deflated, as it contains pointers to method
    // bytecodes and stuff and other vtables and stuff.  It will be regenerated
    // on inflation if necessary.
    //
    // Note: m_subclasses is not deflated.  It will also be regenerated on
    // inflation.  We don't want to implicitly deflate *every* subclass to
    // an exported module, after all.
    //
    // FIXME: what about the initialization of an imported metaclass object for
    // epoch compile?  For runtime, it's just an external reference...
  } else {
    // Note: For template deflatesd, metaclasses are never deflated as they are
    // created by the resolution process.
    DF << m_hadBaseSpecifiers;
    DF << m_inheritancePendingCnt;

    verify(m_state == st_initial);
    verify(!m_overloadSets.size());
    verify(m_id == wkc_not_well_known);
    verify(!m_subclasses.size());
    verify(!m_bindings);
    verify(m_size == -1);
    verify(!m_isTranslated);
    verify(!m_isExecutable);
    verify(!m_vtblSize);
    verify(!m_vtblPrefixSize);
    verify(!m_vtable);
    verify(!m_vtblMap.size());
    verify(m_cppClassObject.empty());
    verify(!m_runTarget);
    verify(!m_compileTarget);
  }
}

Class::Class(Inflator &IF)
    : Entity(IF),
      m_memberScope(new Scope()) {
  IF >> WX(m_state);
  IF >> m_metaClass;
  IF >> m_metaInstance;
  IF >> m_bases;
  IF >> m_commonScope;
  IF >> m_memberScope;
  IF >> m_fields;
  IF >> m_hasDestructor;

  if (m_state == st_resolved) {
    IF >> m_id;
    IF >> m_bindings;
    IF >> m_size;
    IF >> m_vtblSize;
    IF >> m_vtblPrefixSize;
    IF >> m_vtblLocation;
    IF >> m_cppClassObject;

    if (m_id < wkc_not_well_known)
      g_WellKnownClasses[m_id] = this;

    // Regenerate list of subclasses.
    // FIXME:  This may be unnecessary.  It's only used to populate the vtable
    // incrementally as the base classes resolve.
    size_t i = 0;
    for (BaseSpecifier *bs : m_bases)
      bs->m_baseClass->m_subclasses.emplace_back(this, i++);

    // Regenerate the vtable and vtablemap.
    if (m_vtblSize > 0)
      GenerateVtable();
  } else {
    IF >> m_hadBaseSpecifiers;
    IF >> m_inheritancePendingCnt;
  }
}

Class *Class::GetClassById(WellKnownClassId id) {
  verify(id < wkc_not_well_known);
  return g_WellKnownClasses[id];
}

void Class::CreateWellKnownClasses() {
  Namespace *ne = Namespace::StdNamespace();
  Class *cls;

  cls = new Class(ne, wkc_Attribute, "Attribute", nullptr);
  ne->AddBuiltinEntity(cls);

  cls = new Class(ne, wkc_MemberAttribute, "MemberAttribute",
                  g_WellKnownClasses[wkc_Attribute]);
  ne->AddBuiltinEntity(cls);

  cls = new Class(ne, wkc_ClassAttribute, "ClassAttribute",
                  g_WellKnownClasses[wkc_Attribute]);
  ne->AddBuiltinEntity(cls);

  cls = new Class(ne, wkc_StatementAttribute, "StatementAttribute",
                  g_WellKnownClasses[wkc_Attribute]);
  ne->AddBuiltinEntity(cls);

  cls = new Class(ne, wkc_ArgumentAttribute, "ArgumentAttribute",
                  g_WellKnownClasses[wkc_Attribute]);
  ne->AddBuiltinEntity(cls);

  cls = new Class(ne, wkc_BaseAttribute, "BaseAttribute",
                  g_WellKnownClasses[wkc_Attribute]);
  ne->AddBuiltinEntity(cls);

  cls = new Class(ne, wkc_VisibilityAttribute, "VisibilityAttribute",
                  g_WellKnownClasses[wkc_MemberAttribute]);
  ne->AddBuiltinEntity(cls);
  cls->AddBase(g_WellKnownClasses[wkc_BaseAttribute]);

  // FIXME: the above deferred attribute classes need to be marked /shared/
  // when that becomes possible (as well as /deferred/).

  /****************************** BuildAttribute ******************************/

  cls = new Class(ne, wkc_BuildAttribute, "BuildAttribute",
                  g_WellKnownClasses[wkc_MemberAttribute]);
  ne->AddBuiltinEntity(cls);
  cls->AddBase(g_WellKnownClasses[wkc_BaseAttribute]);
  cls->AddBase(g_WellKnownClasses[wkc_StatementAttribute]);
  cls->AddUndetermined(new Field(cls, "shallBuild", Type::Bool()));
  cls->AddUndetermined(new Method(cls, wks_op_apply, Method::nm_build_apply,
                                  cls->AsType(), "shallBuild", Type::Bool()));

  bool b = true;
  Const *ce = new Const(ne, "@build", cls->AsType(), &b, 1);
  ne->AddBuiltinEntity(ce);

  /****************************** StaticAttribute *****************************/

  cls = new Class(ne, wkc_StaticAttribute, "StaticAttribute",
                  g_WellKnownClasses[wkc_MemberAttribute]);
  ne->AddBuiltinEntity(cls);
  cls->AddUndetermined(new Field(cls, "isStatic", Type::Bool()));

  b = true;
  ce = new Const(ne, "@static", cls->AsType(), &b, 1);
  ne->AddBuiltinEntity(ce);

  /***************************** RedefineAttribute ****************************/

  cls = new Class(ne, wkc_RedefineAttribute, "RedefineAttribute",
                  g_WellKnownClasses[wkc_MemberAttribute]);
  ne->AddBuiltinEntity(cls);
  // FIXME: type should not be a raw ubyte.
  cls->AddUndetermined(new Field(cls, "type", Type::Byte()));

  unsigned char c = static_cast<unsigned char>(Class::rt_introduce);
  ce = new Const(ne, "@introduce", cls->AsType(), &c, 1);
  ne->AddBuiltinEntity(ce);

  c = static_cast<unsigned char>(Class::rt_redefine);
  ce = new Const(ne, "@override", cls->AsType(), &c, 1);
  ne->AddBuiltinEntity(ce);

  c = static_cast<unsigned char>(Class::rt_hide);
  ce = new Const(ne, "@hide", cls->AsType(), &c, 1);
  ne->AddBuiltinEntity(ce);

  /***************************** InheritAttribute *****************************/

  cls = new Class(ne, wkc_InheritAttribute, "InheritAttribute",
                  g_WellKnownClasses[wkc_MemberAttribute]);
  ne->AddBuiltinEntity(cls);
  // FIXME: type should not be a raw ubyte.
  cls->AddUndetermined(new Field(cls, "type", Type::Byte()));

  c = static_cast<unsigned char>(Class::it_final);
  ce = new Const(ne, "@final", cls->AsType(), &c, 1);
  ne->AddBuiltinEntity(ce);

  c = static_cast<unsigned char>(Class::it_deferred);
  ce = new Const(ne, "@deferred", cls->AsType(), &c, 1);
  ne->AddBuiltinEntity(ce);

  c = static_cast<unsigned char>(Class::it_auto_redefine);
  ce = new Const(ne, "@auto_override", cls->AsType(), &c, 1);
  ne->AddBuiltinEntity(ce);

  c = static_cast<unsigned char>(Class::it_no_inherit);
  ce = new Const(ne, "@no_inherit", cls->AsType(), &c, 1);
  ne->AddBuiltinEntity(ce);

  /****************************** VirtualAttribute ****************************/

  cls = new Class(ne, wkc_VirtualAttribute, "VirtualAttribute",
                  g_WellKnownClasses[wkc_MemberAttribute]);
  ne->AddBuiltinEntity(cls);
  cls->AddUndetermined(new Field(cls, "isVirtual", Type::Bool()));
  cls->AddUndetermined(new Method(cls, wks_op_apply, Method::nm_build_apply,
                                     cls->AsType(), "isVirtual", Type::Bool()));

  b = true;
  ce = new Const(ne, "@virtual", cls->AsType(), &b, 1);
  ne->AddBuiltinEntity(ce);

  /****************************** ExternCAttribute ****************************/

  cls = new Class(ne, wkc_ExternCAttribute, "ExternCAttribute",
                  g_WellKnownClasses[wkc_Attribute]); // FIXME
  ne->AddBuiltinEntity(cls);
  cls->AddUndetermined(new Field(cls, "isExternC", Type::Bool()));
  cls->AddUndetermined(new Method(cls, wks_op_apply, Method::nm_build_apply,
                                     cls->AsType(), "isExternC", Type::Bool()));

  b = true;
  ce = new Const(ne, "@extern_C", cls->AsType(), &b, 1);
  ne->AddBuiltinEntity(ce);
}

void Class::BindNames(SymbolTable &st) {
  Entity::BindNames(st);
  m_metaClass->Entity::BindNames(st);

  // See if inheritance is pending.  If so, nothing above us can be definitively
  // bound to anything below us.
  bool pending = false;
  m_inheritancePendingCnt = st.GetInheritancePendingCount();

  if (m_bases.size() > 0 && m_state < st_bases_known)
    pending = true;

  // The base classes do not see the members of the class; but they do see a
  // "this" declaration.
  {
    SymbolTable::PushScope scope(st);
    String *s = String::Get(wks_this);
    st.AddSymbol(s, m_commonScope->RawGet(s));

    for (auto bs : m_bases)
      bs->BindNames(st);
  }

  // Attributes do not see anything defined in the class itself (causes circular
  // dependencies--FIXME).
  m_commonScope->BindNamesInAttributes(st);
  m_memberScope->BindNamesInAttributes(st);
  m_metaClass->m_memberScope->BindNamesInAttributes(st);

  // Ditto for attribute groups.
  for (auto g : m_groups)
    g->BindNames(st);

  // Everything else inside the class sees the common members.
  SymbolTable::PushClass klass(st, this, pending);
  SymbolTable::PushScope commonScope(st);
  st.AddSymbols(m_commonScope);

  SymbolTable::PushScope staticScope(st);
  st.AddSymbols(m_metaClass->m_memberScope);

  // Common members may also (under the right conditions) see static members.
  m_commonScope->BindNames(st);

  // Static members also see other static members (constructors are handled
  // separately below).
  auto f1 = [](Entity *e) {
    if (e->Kind() == ek_Method) {
      auto me = safe_cast<Method *>(e);
      return me->MethodKind() != mk_construct;
    }
    return true;
  };

  m_metaClass->m_memberScope->BindNames(st, f1);

  // Instance members see everything.
  SymbolTable::PushScope instanceScope(st);
  st.AddSymbols(m_memberScope);
  m_memberScope->BindNames(st);

  // Constructors, though static, see and give priority to instance members.
  auto f2 = [](Entity *e) {
    if (e->Kind() == ek_Method) {
      auto me = safe_cast<Method *>(e);
      return me->MethodKind() == mk_construct;
    }
    return false;
  };

  m_metaClass->m_memberScope->BindNames(st, f2);
}

Entity *Class::LookupEntity(String *name) {
  Setup();
  auto e = m_commonScope->LookupEntity(name);
  if (!e)
    e = m_memberScope->LookupEntity(name);
  return e;
}

size_t Class::GetBaseCount() {
  verify(m_state >= st_bases_known);
  return m_bases.size();
}

BaseSpecifier *Class::GetBase(size_t i) {
  verify(m_state >= st_bases_known);
  return m_bases[i];
}

void Class::Setup() {
  if (m_state != st_initial)
    return;

  if (m_metaInstance) {
    m_metaInstance->Setup();
    m_state = st_build_started;
    return;
  }

  if (AttributeList *al = Attributes())
    al->ResolveFully();

  // Mark entities as needing possible auto-generation.
  m_memberScope->AddAuto(this, wkhs_destructor, &Class::AutoGenDestructors);
  m_memberScope->AddAuto(this, wks_op_assign, &Class::AutoGenOpAssign);
  m_metaClass->m_memberScope->AddAuto(m_metaClass, wks_op_apply,
                                      &Class::AutoGenConstructors);
  m_metaClass->SetParent(Parent());

  m_state = st_build_started;
}

bool Class::ResolveBases() {
  if (m_state >= st_bases_known)
    return false;

  if (m_metaInstance)
    return m_metaInstance->ResolveBases();

  Setup();

  size_t bias = 0;
  for (size_t i = 0; i < m_bases.size(); i++) {
    BaseSpecifier *bs = m_bases[i];
    bs->ResolveFully();

    if (bs->IsNotBuilt()) {
      bias++;
      continue;
    }

    // Squeeze out the holes from the non-built bases.
    if (bias > 0)
      m_bases[i - bias] = bs;
    bs->m_baseClass->m_subclasses.emplace_back(this, i - bias);
  }

  // Shrink bases array to reflect non-built bases.
  if (bias > 0)
    m_bases.resize(m_bases.size() - bias);

  // Propagate base classes to metaclass.
  m_metaClass->m_bases.reserve(m_bases.size());
  for (auto bs : m_bases)
    m_metaClass->m_bases.push_back(new BaseSpecifier(bs));

  // Prepare scopes for lookups.
  m_memberScope->HandleNameMacros();
  m_commonScope->HandleNameMacros();
  m_metaClass->m_memberScope->HandleNameMacros();

  // Prepare the scopes for inheritance.
  m_commonScope->ResolveFully();
  m_memberScope->ResolveFully();
  m_metaClass->m_memberScope->ResolveFully();

  // Gather up the fields and overload sets declared by the class, now that the
  // build(false) ones have been excluded.
  auto gather = [](Class *ce) {
    auto visit = [ce](String *name, Entity *e) {
      if (e->Kind() == ek_Field) {
        ce->m_fields.push_back(safe_cast<Field *>(e));
      } else if (e->Kind() == ek_OverloadSet) {
        ce->m_overloadSets.push_back(safe_cast<OverloadSet *>(e));
      }
    };

    ce->m_memberScope->EnumerateMembers(visit);

    // The tentative field ordinals now serve their purpose: to restore the
    // original declaration order.
    sort(ce->m_fields.begin(), ce->m_fields.end(),
         [](Field *a, Field *b) { return a->GetOrdinal() < b->GetOrdinal(); });

    // Unfortunately, an entity could be in the scope under multiple names;
    // weed out the duplicates.
    if (ce->m_fields.size() > 1) {
      size_t j = 0;
      for (size_t i = 1; i < ce->m_fields.size(); i++) {
        if (ce->m_fields[j] != ce->m_fields[i])
          ce->m_fields[++j] = ce->m_fields[i];
      }
      ce->m_fields.resize(j + 1);
    }
  };

  gather(this);
  gather(m_metaClass);

  // Now for inheritance!  Start by building a map of every entity present
  // in a base class, keyed by name.  We need to keep track of each type of
  // scope separately.
  struct ByName {
    InheritedEntities m_common;
    InheritedEntities m_instance;
    InheritedEntities m_static;
  };

  std::unordered_map<String *, ByName> inherited;

  for (size_t i = 0; i < m_bases.size(); i++) {
    Class *ce = m_bases[i]->m_baseClass;

    auto ose = [&inherited](String *name, Entity *value,
                  InheritedEntities ByName::*scope, size_t ord) {
      if (value->Kind() != ek_Nullified) {
        (inherited[name].*scope).emplace_back(value, ord);
      }
    };

    ce->m_commonScope->EnumerateMembers(ose, &ByName::m_common, i);
    ce->m_memberScope->EnumerateMembers(ose, &ByName::m_instance, i);
    ce->m_metaClass->m_memberScope->EnumerateMembers(ose, &ByName::m_static, i);
  }

  for (auto &I : inherited) {
    String *name = const_cast<String *>(I.first);

    // First instablish what presence the entity name has in this class.
    Entity *e_cs = m_commonScope->RawGet(name);
    if (e_cs && e_cs->Kind() == ek_Nullified)
      e_cs = nullptr;

    Entity *e_is = m_memberScope->RawGet(name);
    if (e_is && e_is->Kind() == ek_Nullified)
      e_is = nullptr;

    Entity *e_ss = m_metaClass->m_memberScope->RawGet(name);
    if (e_ss && e_ss->Kind() == ek_Nullified)
      e_ss = nullptr;

    // Convenient access to inherited stuff.
    InheritedEntities &i_cs = I.second.m_common;
    InheritedEntities &i_is = I.second.m_instance;
    InheritedEntities &i_ss = I.second.m_static;

    // If a name is declared in common and another scope, complain.
    if (e_cs) {
      if (e_is) {
          EmitError(e_is) << "Duplicate declaration of " << e_is << ".";
      } else if (e_ss) {
          EmitError(e_ss) << "Duplicate declaration of " << e_ss << ".";
      }

      // Regardless, no inheritance will take place for this name.
      continue;
    } else if (e_is || e_ss) {
      // Inform overload sets of overload sets they override.  They'll also do
      // error checking.
      auto notify = [](Entity *e, InheritedEntities &inherited) {
        // Obvious, a non-overload-set ignores all this.
        if (e->Kind() != ek_OverloadSet)
          return;

        // Filter out non-overload-sets from the base classes too.
        InheritedEntities defs;
        for (auto &J : inherited) {
          if (J.m_entity->Kind() == ek_OverloadSet)
            defs.push_back(J);
        }

        safe_cast<OverloadSet *>(e)->SetInherited(defs);
      };

      if (e_is)
        notify(e_is, i_is);

      if (e_ss)
        notify(e_ss, i_ss);

      // Nor can common be inherited if this name is declared as an instance
      // or static member.
      if (i_cs.size() > 0)
        continue;
    }

    // Lambda function to handle member inheritance.  Executed twice, once
    // for instance and once for static.
    auto propagate = [this, name, &i_cs](Scope *s, InheritedEntities &ie) {
      // If more than one base class wants to bequeath definitions for this
      // name, it's ambiguous.
      // FIXME: not if they're all the *same* definition.
      if (i_cs.size() + ie.size() > 1) {
        InheritedEntities defs;
        defs.insert(defs.end(), i_cs.begin(), i_cs.end());
        defs.insert(defs.end(), ie.begin(), ie.end());
        auto ae = new Ambiguous(this, const_cast<String *>(name), defs);
        ((i_cs.size() > 0) ? &*m_commonScope : s)->AddEntity(ae);
        return;
      }

      if (i_cs.size() > 0)  {
        m_commonScope->AddEntity(i_cs[0].m_entity);
      } else if (ie.size() > 0) {
        Entity *e = ie[0].m_entity;
        s->AddEntity(e);

        // When a field is inherited, track its inheritance bath.
        if (e->Kind() == ek_Field) {
          auto fe = safe_cast<Field *>(e);
          size_t ord = ie[0].m_baseOrd;
          Class *base = m_bases[ord]->m_baseClass;
          FieldPath fp = base->PathForField(fe);
          fp.insert(fp.begin(), ord);
          m_fieldPaths[fe] = fp;
        }
      }
    };

    if (!e_is)
      propagate(m_memberScope, i_is);

    if (!e_ss)
      propagate(m_metaClass->m_memberScope, i_ss);

    // FIXME: when a Nullified is replaced by something inherited, any other
    // Nullifieds with the same name in the other two scopes may need to be
    // purged.
  }

  // Now that inheritance has been done, revist name binding.
  if (m_hadBaseSpecifiers) {
    SymbolTable st(m_inheritancePendingCnt, true);
    BindNames(st);
  }

  // Note the presence of an explicit destructor.
  auto visit = [this](String *name, Entity *e) {
    if (e->Kind() == ek_OverloadSet) {
      auto ose = safe_cast<OverloadSet *>(e);
      if (ose->HasExplicitDestructor())
        m_hasDestructor = YES;
    }
  };

  m_memberScope->EnumerateMembers(visit);

  // We can finally let our metaclass start resolving, which begins with the
  // next state.
  m_metaClass->TrackResolution();
  m_metaClass->m_state = st_bases_known;

  m_state = st_bases_known;
  return true;
}

void Class::ResolveFields() {
  if (m_state >= st_fields_known)
    return;

  ResolveBases();

  // Assign field ordinals to all bases (which are pseudo-fields) and fields.
  // Metaclasses begin their state machine here.
  for (size_t i = 0; i < m_bases.size(); i++)
    m_bases[i]->m_ord = i;
  for (size_t i = 0; i < m_fields.size(); i++)
    m_fields[i]->SetOrdinal(i + m_bases.size());

  // If this class introduces a vtable (as opposed to extending an inherited
  // vtable), then add a field to hold its address.
  m_vtblLocation = -1;
  for (size_t i = 0; i < m_bases.size(); i++) {
    Class *ce = m_bases[i]->m_baseClass;

    // See if a base class already has a virtual table, and if so remember
    // which one--the left-most--it was.
    if (ce->m_vtblSize > 0) {
      m_vtblLocation = int(i);
      m_state = st_fields_known;
      return;
    }
  }

  // FIXME: This design is causing a certain amount of pain.  We need to have
  // all the fields identified *and* ordinal known before resolving the method
  // sets.  Unfortunately, one of those fields is dependent on whether any of
  // those methods are virtual.  Perhaps ordinal assignment should be pushed
  // to field entity resolution, meaning that a load node must refer to a field
  // and not an ordinal.  Note that ConstructorCentral is part of the depedency
  // circle: it holds up overload set resolution, but it needs to know the
  // fields.
  for (auto ose : m_overloadSets) {
    if (ose->HasVirtualMethods()) {
      // Insert the vtbl field in front of all others.  Must renumber the
      // existing fields first.
      for (size_t i = 0; i < m_fields.size(); i++)
        m_fields[i]->SetOrdinal(i + m_bases.size() + 1);

      auto *fe = new Field(this, String::Get(wkhs_vtbl), Type::Address());
      fe->SetOrdinal(m_bases.size());
      fe->TrackResolution();
      m_fields.insert(m_fields.begin(), fe);
      break;
    }
  }

  m_state = st_fields_known;
}

void Class::ResolveLayout() {
  if (m_state >= st_layout_known)
    return;

  ResolveFields();

  // For the layout to be determined, the types of all fields are needed.
  for (auto fe : m_fields)
    fe->ResolveType();

  // And all the types must be sufficiently resolved, naturally.
  for (auto fe : m_fields) {
    // FIXME: this doesn't work for an array of classes.  Should delegate
    // to Type.  Type::CheckResolution() almost works; it'll hang for pointers
    // to self.
    Type t = fe->GetType();
    if (t == tk_class) {
      Class *ce = t.Class();
      ce->ResolveLayout();
    }
  }

  m_state = st_layout_known;
}

void Class::ResolveVtable() {
  if (m_state >= st_vtable_known)
    return;

  ResolveLayout();

  // Resolve all member overload sets.
  for (auto ose : m_overloadSets)
    ose->ResolveFully();

  // Check if any inherited base class has a vtable.
  bool hasClassSlot = HasClassSlot();

  // If this does not inherit any vtables (and isn't a metaclass), reserve a
  // spot to hold the address of the metaclass object.  This reservation shall
  // be undone if it turns out this class has no vtable at all.
  if (hasClassSlot)
    m_vtblSize++;

  // Define the location of the base class vtables within our own vtable.
  for (auto bs : m_bases) {
    bs->m_vtblOffset = m_vtblSize;
    m_vtblSize += bs->m_baseClass->m_vtblSize;
    m_vtblMap.insert(m_vtblMap.end(), bs->m_baseClass->m_vtblMap.begin(),
                                      bs->m_baseClass->m_vtblMap.end());
    verify(m_vtblMap.size() == m_vtblSize - hasClassSlot);
  }

  // Assign vtable slots to introducing methods.
  m_vtblPrefixSize = m_vtblSize - hasClassSlot;
  for (auto ose : m_overloadSets)
    ose->AssignVtblSlots(m_vtblSize);

  // If no virtuals were inherited and none were introduced, then no vtable
  // for this class.
  if (m_vtblSize == unsigned(hasClassSlot)) {
    m_vtblSize = 0;
  } else {
    // Create the vtable.
    GenerateVtable();
  }

  m_state = st_vtable_known;
}

void Class::ResolveFully() {
  if (m_state >= st_resolved)
    return;

  ResolveVtable();

  // Wait for the used namespaces to be fully resolved.
  for (size_t i = 0; i < m_commonScope->UsedNamespaceCount(); i++) {
    Namespace *ne = m_commonScope->UsedNamespace(i);
    ne->ResolveFully();
  }

  // And, finally, the metaclass.
  if (m_metaClass)
    m_metaClass->ResolveFully();

  m_state = st_resolved;

  // FIXME: tasks to be performed by states to be added here:
  //   * Create vtable mappings for this class.
  //   * Provide default implementations for certain methods that weren't
  //     explicitly provided.
  //   * Squeeze genericity into here somewhere.
  //   * No doubt other stuff I've temporarily forgotten.
}

Node *Class::AsValue(Location sl) {
  // The value representation of a class entity is a reference to the associated
  // Jolt class object.
  // FIXME: what level of resolution is required?  Not full, as that will cause
  // deadlock.
  if (!IsMetaclass())
    return m_metaClass->AsValue(sl);

  return (new GlobalAddr(sl, this, AsType()))->Deref();
}

void *Class::GlobalStorage(Epoch ep) {
  // FIXME:  correct???
  if (IsMetaclass())
    return MetaInstance()->GetTarget(ep)->GlobalStorage(ep);
  return GetTarget(ep)->GlobalStorage(ep);
}

const std::string &Class::GlobalStorageName() {
  if (m_cppClassObject.empty()) {
    if (Class *meta = MetaInstance())
      m_cppClassObject = meta->ExternalName() + "_class";
    else
      m_cppClassObject = ExternalName() + "_class";
  }
  return m_cppClassObject;
}

Entity::ResolutionState Class::GetResolutionState() {
  verify(m_state == st_resolved);
  return rs_ok;
  // FIXME: do we need more?
}

void Class::AddToClosure(TranslateClosure *tc) {
  if (tc->GetEpoch() == ep_compile) {
    // If executable, then all dependencies, direct and indirect, are already
    // translated and executable.
    if (m_isExecutable)
      return;
    if (!m_isTranslated) {
      if (!m_compileTarget)
        m_compileTarget = Target::Get(ep_compile)->For(this);
    }
  } else {
    if (!m_runTarget)
      m_runTarget = Target::Get(ep_run)->For(this);
  }

  // All base classes are dependencies.
  for (auto bs : m_bases)
    tc->AddToClosure(bs->m_baseClass);

  // The metaclass is a dependency, as the address of its instance is placed
  // into vtables.
  if (m_metaClass)
    tc->AddToClosure(m_metaClass);
  else
    SetLinkage(m_metaInstance->Linkage());

  // If the class is being exported, then all methods must be added.
  if (Linkage() == lk_export) {
    auto func = [tc](String *, Entity *e) {
      if (auto ose = dyn_cast<OverloadSet *>(e))
        tc->AddToClosure(ose);
    };
    m_memberScope->EnumerateMembers(func);
  } else {
    // Not being exported, so we can afford to be more selective.

    // Any virtual methods in this class that are redefinitions of inherited
    // methods must be added to the closure.
    for (Method *me : m_vtblMap) {
      if (me && me->Parent() == this) {
        if (me->m_isRedefining && me->m_isVirtual)
          tc->AddToClosure(me);
      }
    }

    // Add destructor to the closure too.
    if (!IsMetaclass()) {
      String *s = String::Get(wkhs_destructor);
      auto e = m_memberScope->LookupEntity(s);
      auto ose = safe_cast<OverloadSet *>(e);
      Method *me = ose->Find();
      tc->AddToClosure(me);
    }
  }
}

void Class::FinalizeClosure(TranslateClosure *tc) {
  // Translate the class (but not methods of the class).
  if (tc->GetEpoch() == ep_compile) {
    if (!m_isTranslated) {
      m_compileTarget->Generate();
      m_isTranslated = true;
    }
  } else {
    m_runTarget->Generate();
  }
}

void Class::SetExecutable() {
  verify(m_isTranslated);
  m_isExecutable = true;
}

Class *Class::GetFromObject(Value *v) {
  void *storage = v->As<void *>();
  return GetFromObject(storage);
}

Class *Class::GetFromObject(void *storage) {
  char *p = reinterpret_cast<char *>(storage) - sizeof(void *);
  Class *ce = (*reinterpret_cast<Type *>(p)).Class();
  verify(ce->m_compileTarget->GlobalStorage(ep_compile) == storage);
  return ce;
}

vector<Field *> &Class::Fields() {
  verify(m_state >= st_fields_known);
  return m_fields;
}

Class::FieldPath &Class::PathForField(Field *f) {
  verify(m_state >= st_bases_known);

  auto I = m_fieldPaths.find(f);
  static FieldPath empty;
  return I != m_fieldPaths.end() ? I->second : empty;
}

bool Class::HasDestructor() {
  if (m_hasDestructor != MAYBE)
    return m_hasDestructor == YES;

  // First see if any fields have a destructor.
  verify(m_state >= st_layout_known);
  for (auto fe : m_fields) {
    Type t = fe->GetType();
    if (t.HasDestructor()) {
      m_hasDestructor = YES;
      return true;
    }
  }

  // We can return false.  If the class had an explicit destructor, then
  // m_hasDestructor would already have been set to YES.  If any base class
  // has a destructor, that would have been detected in the above loop.
  return false;
}

bool Class::HasOpDeref() {
  Entity *e = m_memberScope->LookupEntity(String::Get(wks_op_deref));
  if (!e || e->Kind() != ek_OverloadSet)
    return false;

  return safe_cast<OverloadSet *>(e)->HasEmptyArgListOverload();
}

Type Class::AsType() {
  if (!m_type)
    m_type = Type::GetClassType(this);
  return m_type;
}

size_t Class::StorageSize() {
  if (m_size < 0)
    ComputeLayout();
  return m_size;
}

size_t Class::FieldCount() {
  return m_bases.size() + m_fields.size();
}

size_t Class::OffsetOf(size_t ord) {
  if (m_size < 0)
    ComputeLayout();
  if (ord < m_bases.size())
    return m_bases[ord]->m_offset;
  ord -=  m_bases.size();
  verify(ord < m_fields.size());
  return m_fields[ord]->GetOffset();
}

Type Class::TypeOf(size_t ord) {
  if (ord < m_bases.size())
    return m_bases[ord]->m_baseClass->AsType();
  ord -=  m_bases.size();
  verify(ord < m_fields.size());
  return m_fields[ord]->GetType();
}

void Class::ComputeLayout() {
  m_size = 0;
  size_t nFields = FieldCount();
  if (nFields > 0) {
    vector<size_t> offsets { Target::GetStructLayout(AsType()) };
    m_size = offsets.back();
    for (size_t i = 0; i < m_bases.size(); i++)
      m_bases[i]->m_offset = offsets[i];
    for (size_t i = 0; i < m_fields.size(); i++)
      m_fields[i]->SetOffset(offsets[i + m_bases.size()]);
  }
}

YesNoMaybe Class::IsSubclassOf(Class *ce, vector<size_t> *path) {
  verify(m_state >= st_bases_known);
  if (ce == this)
    return YES;

  YesNoMaybe rv = NO;
  vector<size_t> *p = path;
  for (size_t i = 0; i < m_bases.size(); i++) {
    switch (m_bases[i]->m_baseClass->IsSubclassOf(ce, p)) {
      case YES:
        if (rv == NO) {
          // We found a path to ce; keep searching for an additional path.  Null
          // out p so that we don't record it if found.
          rv = YES;
          if (p) {
            p->push_back(i);
            p = nullptr;
          }
          break;
        }
        /* fall through */
      case MAYBE:
        // We know we have multiple paths to ce, and there's no point looking
        // for additional paths.  The left-most path has been recorded, so just
        // return.  p is non-null at this point only if this is the left-most
        // path, even though it is still ambiguous.
        if (p)
          p->push_back(i);
        return MAYBE;
      case NO:
        break;
    }
  }
  return rv;
}

const intptr_t *Class::Vtable() {
  verify(m_state >= st_vtable_known);
  return m_vtable;
}

size_t Class::GetVtableSize() {
  verify(m_state >= st_vtable_known);
  return m_vtblSize;
}

int Class::GetVtableLocation() {
  verify(m_state >= st_vtable_known);
  return m_vtblLocation;
}

bool Class::HasClassSlot() {
  // Only class instance vtables point to a metaclass instance.
  if (!m_metaClass)
    return false;

  // Only a class that introduces a vtable reserves a slot for the metaclass
  // instance.
  for (auto bs : m_bases) {
    if (bs->m_baseClass->m_vtblSize > 0)
      return false;
  }

  return true;
}

void Class::SetVtblSlot(size_t slot, intptr_t func, Method *me) {
  // If we haven't got as far as allocating a vtable yet, that's OK -- just
  // ignore this for now.  We'll ask all the methods to fill in their slots
  // when we get around to allocating the vtable.  If we don't have a vtable
  // yet, then neither does any subclass.
  if (!m_vtable)
    return;

  verify(slot < m_vtblSize);
  if (!m_vtblMap[slot] || me->Parent() == this) {
    // Creating initial vtable for class.
    m_vtable[slot] = func;
    m_vtblMap[slot] = me;
  } else if (m_vtblMap[slot] == me) {
    // Updating function pointer for method.
    m_vtable[slot] = func;
  } else {
    // We have reached a subclass that redefined the method.
    return;
  }

  // Propagate to all subclasses.
  for (auto &sc : m_subclasses) {

    // Don't propagate to subclasses that haven't gotten as far as laying out
    // their own vtable.  When they reach that point, they'll pick up the slots
    // inherited from us.
    if (sc.m_class->m_state >= st_vtable_known) {
      size_t offset = sc.m_class->GetBase(sc.m_index)->m_vtblOffset;
      sc.m_class->SetVtblSlot(slot + offset, func, me);
    }
  }
}

Method *Class::GetMethodForVtblSlot(size_t slot) {
  verify(m_state >= st_vtable_known && slot < m_vtblSize);
  return m_vtblMap[slot];
}

void Class::GenerateVtable() {
  m_vtable = new intptr_t[m_vtblSize] { };

  // Initialize mapping of slots to method entities.  First inherit mappings
  // from base classes.
  m_vtblMap.resize(m_vtblSize);
  for (auto bs : m_bases) {
    for (size_t i = 0; i < bs->m_baseClass->m_vtblSize; i++)
      VtblSlot(bs->m_vtblOffset + i) = bs->m_baseClass->VtblSlot(i);
  }

  // Then override mappings with our own methods.
  auto ose = [](String *, Entity *&value) {
    // FIXME: It would be nice if inflation could be avoided if the deflated
    // entity wasn't a OverloadSet, or even a OverloadSet without virtual
    // methods.
    if (value->Kind() == ek_DeferredInflate)
      value = safe_cast<DeferredInflate *>(value)->Inflate();
    if (value->Kind() == ek_OverloadSet)
      static_cast<OverloadSet *>(value)->SetVtblSlots();
  };

  m_memberScope->EnumerateMembers(ose);

  // And finally, insert pointer to our (compile time) class object if it has
  // been allocated (if not, it'll be inserted when it is).
  if (m_compileTarget && m_compileTarget->HasGlobalStorage(ep_compile))
    SetClassInVtable((intptr_t)m_compileTarget->GlobalStorage(ep_compile));
}

void Class::SetClassInVtable_(intptr_t clsobj, intptr_t *vtable) {
  verify(!IsMetaclass());
  if (HasClassSlot()) {
    if (m_vtblSize > 0)
      vtable[0] = clsobj;
    return;
  }

  for (size_t i = m_bases.size(); i > 0; ) {
    BaseSpecifier *bs = m_bases[--i];
    bs->m_baseClass->SetClassInVtable_(clsobj, vtable + bs->m_vtblOffset);
  }
}

bool Class::InheritsVtblSlot(size_t slot) {
  verify(m_state >= st_vtable_known && slot < m_vtblSize);
  return slot < m_vtblPrefixSize;
}

BaseSpecifier *Class::OwnsVtblSlot(size_t &slot) {
  verify(m_state >= st_vtable_known && slot < m_vtblPrefixSize);
  BaseSpecifier *bs = m_bases[0];

  for (size_t i = 1; i < m_bases.size(); i++) {
    BaseSpecifier *bs2 = m_bases[i];
    if (slot < bs2->m_vtblOffset) {
      slot -= bs->m_vtblOffset;
      return bs;
    }
    bs = bs2;
  }

  slot -= bs->m_vtblOffset;
  return bs;
}

TargetClass *Class::GetTarget(Epoch ep) {
  if (ep == ep_compile) {
    if (!m_compileTarget)
      m_compileTarget = Target::Get(ep_compile)->For(this);
    return m_compileTarget;
  } else {
    if (!m_runTarget)
      m_runTarget = Target::Get(ep_run)->For(this);
    return m_runTarget;
  }
}

void Class::AutoGenOpAssign(OverloadSet *ose) {
  Method *me;

  // Supply default assigner if none present.
  Type t1 = this->AsType().Const().LValueRef();
  me = ose->Find(t1);
  if (!me || me->Parent() != this) {
    // An inherited method with the same signature doesn't count;
    // we will redefine it.
    Expr *e = new Expr(new Literal({ }, Value::New(Type::Void())));
    me = new Method(this, ose->Name(),
                    mk_method, e,                       /* FIXME */
                    "that", t1);
    me->SetGenerated(Method::gm_assignment);
    me->MakeInstance(this);
    ose->AddGeneratedMethod(me);
  }
}

void Class::AutoGenConstructors(OverloadSet *ose) {
  Method *me;

  // Supply default constructor if none present.
  me = ose->Find();
  if (!me || me->Parent() != this) {
    // An inherited constructor with the same signature doesn't count;
    // we will redefine it.

    // The constructor starts with a basically empty body, lacking constructors
    // for the fields and base classes.  They will be inserted later, just as
    // they would for any user-supplied constructor lacking a full set of
    // construct statements.
    Expr *e = new Expr(new Literal({ }, Value::New(Type::Void())));
    me = new Method(m_metaInstance, ose->Name(), mk_construct, e);
    me->MakeStatic(this);
    me->SetGenerated(Method::gm_constructor);
    ose->AddGeneratedMethod(me);
  }

  // Also supply default copy constructor if none present.
  Type t1 = m_metaInstance->AsType().Const().LValueRef();
  me = ose->Find(t1);
  if (!me || me->Parent() != this) {
    // Ditto for copy constructors.
    Expr *e = new Expr(new Literal({ }, Value::New(Type::Void())));
    me = new Method(m_metaInstance, ose->Name(), mk_construct, e,
                    "that", t1);
    me->MakeStatic(this);
    me->SetGenerated(Method::gm_copy_constructor);
    ose->AddGeneratedMethod(me);
  }
}

void Class::AutoGenDestructors(OverloadSet *ose) {
  Method *me;

  // Supply default destructor if none present.
  me = ose->Find();
  if (!me || me->Parent() != this) {
    // An inherited destructor with the same signature doesn't count;
    // we will redefine it.

    // The destructor starts with a basically empty body, lacking destructors
    // for the fields and base classes.  They will be inserted later, just as
    // they would for any user-supplied destructor lacking a full set of
    // destruct statements.
    Expr *e = new Expr(new Literal({ }, Value::New(Type::Void())));
    me = new Method(this, ose->Name(), mk_destruct, e);
    me->MakeInstance(this);
    me->SetGenerated(Method::gm_destructor);
    ose->AddGeneratedMethod(me);
  }
}

void Class::AppendToHash(SHA512 &hash) {
  hash.Append(IsMetaclass() ? 'c' : 'C');
  if (!m_bindings)
    return;

  hash.Append('<');

  bool first = true;
  for (auto v : m_bindings->m_bindings) {
    if (!first)
      hash.Append(',');
    first = false;
    v->AppendToHash(hash);
  }

  hash.Append('>');
}
