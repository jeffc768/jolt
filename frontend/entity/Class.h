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

// A class entity describes a Jolt class.

#pragma once

#include "Entity.h"
#include "parser/ParserDecls.h"
#include "type/Type.h"
#include <unordered_map>

class BaseSpecifier;
class Field;
class Derivation;
class Method;
class OverloadSet;
class Scope;
class String;
class TargetClass;
class Undetermined;
class Value;

enum Epoch: uint8_t;

using std::unordered_map;

class Class: public Entity {
  DECLARE_ENTITY(Class)

  enum state_t: uint8_t {
    st_initial,
    st_build_started,
    st_bases_known, // f_Inheritance,
    st_fields_known, // f_Fields1,
    st_layout_known, // f_Fields2,
    st_vtable_known, // f_OverloadSets
    st_resolved
  };

  state_t             m_state             = st_initial;

public:
  enum WellKnownClassId {
    wkc_Attribute,
    wkc_MemberAttribute,
    wkc_VisibilityAttribute,
    wkc_ClassAttribute,
    wkc_StatementAttribute,
    wkc_ArgumentAttribute,
    wkc_BaseAttribute,
    wkc_BuildAttribute,
    wkc_StaticAttribute,
    wkc_RedefineAttribute,
    wkc_InheritAttribute,
    wkc_VirtualAttribute,
    wkc_ExternCAttribute,

    wkc_not_well_known // must be last
  };

  enum RedefineType {
    rt_introduce,
    rt_redefine,
    rt_hide
  };

  enum InheritType {
    it_final,
    it_deferred,
    it_auto_redefine,
    it_no_inherit
  };

protected:
  virtual void PreDestroy();

  void DeflateFields(Deflator &DF);
  Class(Inflator &IF);

  virtual void AppendToHash(SHA512 &hash);

public:
  WellKnownClassId GetId() { return m_id; }
  static Class *GetClassById(WellKnownClassId id);

  // Create "normal" class.
  Class(Token *name, AST::SafeArray<AST::BaseInfo> bases,
        AST::SafeArray<AST::DeclInfo> decls);

  static void CreateWellKnownClasses();

  virtual void BindNames(SymbolTable &st);
  virtual Entity *LookupEntity(String *name);
  virtual EntityScoping Scoping() { return es_nonmember; }

  void Setup();
  bool ResolveBases();
  void ResolveFields();
  void ResolveLayout();
  void ResolveVtable();
  virtual void ResolveFully();

  size_t GetBaseCount();
  BaseSpecifier *GetBase(size_t ord);

  virtual Node *AsValue(Location sl);
  virtual void *GlobalStorage(Epoch ep);
  virtual const std::string &GlobalStorageName();
  virtual ResolutionState GetResolutionState();
  virtual void AddToClosure(TranslateClosure *tc);
  virtual void FinalizeClosure(TranslateClosure *tc);
  virtual void SetExecutable();

  static Class *GetFromObject(Value *v);
  static Class *GetFromObject(void *storage);

  vector<Field *> &Fields();
  using FieldPath = vector<size_t>;
  FieldPath &PathForField(Field *f);
  bool HasDestructor();

  bool HasOpDeref();

  // Query type-related information.  Fields include base classes as well as
  // actual fields.  Field ordinals are assigned first to base classes then to
  // actual fields.
  Type AsType();
  size_t StorageSize();
  size_t FieldCount();
  size_t OffsetOf(size_t ord);
  Type TypeOf(size_t ord);
  void ComputeLayout();

  // If a subclass relationship exists and path is not null, the path through
  // the inheritance hierarchy is stored in path.  Maybe is returned if there
  // is more than one path, i.e. ce is inherited multiple times and an upcast
  // would be ambiguous; in this case, path returns the left-most path.  To
  // reach the superclass (ce) from the subclass (this), process the path vector
  // from the last element to the first.  Path is empty if ce == this.
  YesNoMaybe IsSubclassOf(Class *ce, vector<size_t> *path = nullptr);

  Class *Metaclass() {
    Class *ce = m_metaClass;
    return ce ? ce : this;
  }

  bool IsMetaclass() { return m_metaInstance != nullptr; }
  Class *MetaInstance() { return m_metaInstance; }

  const intptr_t *Vtable();
  size_t GetVtableSize();
  int GetVtableLocation();
  bool HasClassSlot();

  // Fill a vtable slot with a bytecode function and propagate to all
  // subclasses.  Only called from Method.
  void SetVtblSlot(size_t slot, intptr_t func, Method *me);

  // Low-level, checked m_vtable access.
  intptr_t &VtblSlot(size_t idx) {
    verify(idx < m_vtblSize && m_vtable);
    return m_vtable[idx];
  }

  // Find out which introducing method defines a slot.
  Method *GetMethodForVtblSlot(size_t slot);

  // Locate all root base classes and set our address into the vtable slot
  // designated for it.
  void SetClassInVtable(intptr_t clsobj) {
    if (m_vtable)
      SetClassInVtable_(clsobj, m_vtable);
  }

  // Does this class own the vtable slot, or is it inherited?
  bool InheritsVtblSlot(size_t slot);

  // From which class is the vtable slot inherited?  Adjusts slot ordinal
  // appropriately.
  BaseSpecifier *OwnsVtblSlot(size_t &slot);

  // Set generic parameters for a new derivation.
  void SetGenericBindings(Derivation *d) { m_bindings = d; }
  Derivation *GetGenericBindings() { return m_bindings; }

  // Get the target object for this class.
  TargetClass *GetTarget(Epoch ep);

private:
  // Create metaclass for a class.
  Class(Class *ce);

  // Create well-known class.
  Class(Entity *parent, WellKnownClassId id, const char *name, Class *base);

  // Add additional base class to well-known class.  Must be called right
  // after construction.
  void AddBase(Class *base);

  // Add Undetermined entity to well-known class.  Must be called right after
  // construction or AddBase().
  void AddUndetermined(Entity *e);

  // Autogenerate various missing methods.
  void AutoGenOpAssign(OverloadSet *ose);
  void AutoGenConstructors(OverloadSet *ose);
  void AutoGenDestructors(OverloadSet *ose);

  void GenerateVtable();

  // Helper for SetClassInVtable.
  void SetClassInVtable_(intptr_t clsobj, intptr_t *vtable);

  // Our class id, if this is one of the well-known classes.
  WellKnownClassId    m_id      = wkc_not_well_known;

  // The base classes of this class.
  vector<BaseSpecifier *> m_bases;

  // The subclasses of this class, including the position of this class within
  // the subclass's list of base specifiers.
  struct Subclass {
    Class            *m_class   = nullptr;
    size_t            m_index   = 0;

    Subclass() = default;
    Subclass(Class *cls, size_t idx) : m_class(cls), m_index(idx) { }

    void UpdateManagedAddresses() {
      *this = *this;
    }
  };

  vector<Subclass>    m_subclasses;

  // Our metaclass.  Everyone has one, including metaclasses, though not before
  // the base classes are resolved.
  Class              *m_metaClass         = nullptr;

  // If we are a metaclass, we have a singleton instance that is the class;
  // otherwise null.
  Class              *m_metaInstance      = nullptr;

  // The bound generic parameters of this derivation, or null if it isn't
  // generic.
  Derivation         *m_bindings          = nullptr;

  // The various scopes.
  Scope              *m_commonScope       = nullptr;
  Scope              *m_memberScope       = nullptr;

  // Set of fields in this class, in the order they are mapped to storage--
  // which is not necessarily the order in which they are contructed.
  vector<Field *>     m_fields;

  // For inherited fields, the path to the base class which declared them.
  unordered_map<Field *, FieldPath> m_fieldPaths;

  // Type object representing this class.
  Type                m_type;

  // Instance size; -1 if layout not yet determined.
  intptr_t            m_size              = -1;

  // Does the class have a destructor?
  YesNoMaybe          m_hasDestructor     = MAYBE;

  // Has the class been translated to bytecodes?  We only need to track
  // compile time, as a closure is computed for run time only once.
  bool                m_isTranslated      = false;

  // Is the compile time version executable?  In other words, has it and all
  // methods and classes it uses (directly or indirectly) been translated?
  bool                m_isExecutable      = false;

  // Size of this class's vtable.  Vtable sizes and offsets only directly apply
  // to compile time execution.
  size_t              m_vtblSize          = 0;

  // The size of the prefix of the vtable that maps to slots inherited from base
  // classes.  The number of slots introduced by this class is m_vtblSize -
  // m_vtblPrefixSize.
  size_t              m_vtblPrefixSize    = 0;

  // Compile time storage of the vtable.
  intptr_t           *m_vtable            = nullptr;

  // How to locate the vtable.  -1 means it's the first field of this class.  A
  // non-negative number means it's inherited from the indicated base class.
  // Meaningful only if m_vtblSize > 0.
  int                 m_vtblLocation;

  // Map slots back to the methods that fill them.
  vector<Method *>    m_vtblMap;

  // Name of C++ global holding the metaclass instance.
  std::string         m_cppClassObject;

  // The target objects for this class, one for each epoch.
  TargetClass        *m_runTarget         = nullptr;
  TargetClass        *m_compileTarget     = nullptr;

  bool                m_hadBaseSpecifiers = false;

  uint16_t            m_inheritancePendingCnt  = 0;

  unsigned            m_fieldOrd               = 0;

  vector<AttributeList *> m_groups;

  vector<OverloadSet *> m_overloadSets;
};
