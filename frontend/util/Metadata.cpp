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

#include "Metadata.h"
#include "DeflatedObject.h"
#include "Integer.h"
#include "Object.h"
#include "String.h"
#include "entity/Argument.h"
#include "entity/Attribute.h"
#include "entity/AttributeList.h"
#include "entity/BaseSpecifier.h"
#include "entity/Class.h"
#include "entity/Const.h"
#include "entity/Field.h"
#include "entity/FormalArguments.h"
#include "entity/Generic.h"
#include "entity/Method.h"
#include "entity/Namespace.h"
#include "entity/OverloadSet.h"
#include "entity/Parameter.h"
#include "entity/Parameters.h"
#include "entity/Scope.h"
#include "entity/Specialization.h"
#include "entity/Undetermined.h"
#include "entity/Var.h"
#include "node/AddrOf.h"
#include "node/Apply.h"
#include "node/Attributes.h"
#include "node/Binary.h"
#include "node/Block.h"
#include "node/BuildPointer.h"
#include "node/Call.h"
#include "node/CallBuiltin.h"
#include "node/Cast.h"
#include "node/Construct.h"
#include "node/Delete.h"
#include "node/Deref.h"
#include "node/Enum.h"
#include "node/Expr.h"
#include "node/ExtractAddress.h"
#include "node/ExtractDescriptor.h"
#include "node/FieldAddr.h"
#include "node/GlobalAddr.h"
#include "node/Ident.h"
#include "node/If.h"
#include "node/Index.h"
#include "node/Initializer.h"
#include "node/Label.h"
#include "node/List.h"
#include "node/Literal.h"
#include "node/LiteralAddr.h"
#include "node/Load.h"
#include "node/Member.h"
#include "node/MethodBody.h"
#include "node/New.h"
#include "node/Quote.h"
#include "node/Sequence.h"
#include "node/Shared.h"
#include "node/Store.h"
#include "node/Transfer.h"
#include "node/TypeOf.h"
#include "node/Unary.h"
#include "node/Union.h"
#include "node/VarAddr.h"
#include "node/VarDecl.h"
#include "node/Vtable.h"
#include "node/VtableCast.h"
#include "node/VtableSlot.h"
#include "node/While.h"
#include "parser/Location.h"
#include "parser/Token.h"
#include "util/InDeflator.h"
#include "util/Value.h"
#include <map>
#include <vector>

constexpr static const external_id_t max_external_id = 67;

// Data structure to track class inheritance.  Built by static constructors,
// then no longer needed once ordinals are assigned to the classes.
using hierarchy_t = std::map<Metadata *, vector<Metadata *>>;
static hierarchy_t *s_hierarchy;

using creator_func_t =  Metadata::creator_func_t;
static vector<creator_func_t> g_creators;

creator_func_t Metadata::GetInflator(external_id_t ord) {
  return g_creators[ord];
}

Metadata::Metadata(Metadata *base) {
  // Because this is a static constructor, we cannot use static construction to
  // initialize the hierarchy map.
  if (!s_hierarchy)
    s_hierarchy = new hierarchy_t;

  (*s_hierarchy)[base].push_back(this);
}

// Do a depth-first traversal of the managed class hierarchy in order to assign
// ordinals that support dynamic casting.
static void RecurseHierarchy(class_id_t &ord, Metadata *mmd) {
  mmd->m_ord = ++ord;

  auto I = s_hierarchy->find(mmd);
  if (I == s_hierarchy->end()) {
    mmd->m_ordOfLastSubclass = mmd->m_ord;
  } else {
    for (auto subclass : I->second)
      RecurseHierarchy(ord, subclass);
    mmd->m_ordOfLastSubclass = ord;
  }
}

// Set up object serialzation functions.
template<class T> void Metadata::Deflate(Deflator &DF, Object *self) {
  if (DF.m_forceReference) {
    verify(!DF.InModuleMode());
    DF.AppendExternal(self);
  } else {
    if (DF.StartObject(self))
      safe_cast<T *>(self)->DeflateFields(DF);
  }
}

template<class T> Object *Metadata::Inflate(Inflator &IF) {
  return new T(IF);
}

template<> Object *Metadata::Inflate<Value>(Inflator &IF) {
  return Value::Inflate(IF);
}

template<> Object *Metadata::Inflate<Location>(Inflator &IF) {
  // Don't do anything (?).  Locations are meaningless to importing modules.
  return nullptr;
}

template<> Object *Metadata::Inflate<DeflatedObject>(Inflator &IF) {
  return DeflatedObject::Inflate(IF);
}

template<> Object *Metadata::Inflate<Integer>(Inflator &IF) {
  return Integer::Inflate(IF);
}

template<> Object *Metadata::Inflate<String>(Inflator &IF) {
  return String::Inflate(IF);
}

template<external_id_t ord, class T>
static inline void SetupSerializableClass() {
  Metadata &mmd = T::s_metadata;
  verify(mmd.m_externalId == 0);
  mmd.m_externalId = ord;
  mmd.m_deflator = &Metadata::Deflate<T>;

  static_assert(ord <= max_external_id, "Need to increase max_external_id.");
  g_creators[ord] = &Metadata::Inflate<T>;
}

template<class T> void Metadata::Reference(Deflator &DF, Object *self) {
  if (DF.AppendExternal(self))
    safe_cast<T *>(self)->DeflateFields(DF);
}

template<external_id_t ord, class T> static inline void SetupReferencedClass() {
  Metadata &mmd = T::s_metadata;
  verify(mmd.m_externalId == 0);
  mmd.m_externalId = ord;
  mmd.m_deflator = &Metadata::Reference<T>;

  static_assert(ord <= max_external_id, "Need to increase max_external_id.");
  g_creators[ord] = &Metadata::Inflate<T>;
}

static void SetupHierarchy() {
  class_id_t ord = 0;
  RecurseHierarchy(ord, &Object::s_metadata);

  // The map has served its purpose.
  delete s_hierarchy;
  s_hierarchy = nullptr;
}

// Assign external ids to all serialzable classes.  These ids cannot change
// once assigned, as they exist in compiled modules.
static void SetupSerializations() {
  g_creators.resize(max_external_id + 1, creator_func_t { });

  SetupSerializableClass<   1,  Argument              >();
  SetupSerializableClass<   2,  Attribute             >();
  SetupSerializableClass<   3,  AttributeList         >();
  SetupSerializableClass<   4,  BaseSpecifier         >();
  SetupSerializableClass<   5,  Class                 >();
  SetupSerializableClass<   6,  Const                 >();
  SetupSerializableClass<   7,  Field                 >();
  SetupSerializableClass<   8,  FormalArguments       >();
  SetupSerializableClass<   9,  Generic               >();
  SetupSerializableClass<  10,  Method                >();
  SetupSerializableClass<  11,  Parameter             >();
  SetupSerializableClass<  12,  Scope                 >();
  SetupSerializableClass<  13,  Specialization        >();
  SetupSerializableClass<  14,  Undetermined          >();
  SetupSerializableClass<  15,  Var                   >();
  SetupSerializableClass<  16,  AddrOf                >();
  SetupSerializableClass<  17,  Apply                 >();
  SetupSerializableClass<  18,  Attributes            >();
  SetupSerializableClass<  19,  Block                 >();
  SetupSerializableClass<  20,  List                  >();
  SetupSerializableClass<  21,  Delete                >();
  SetupSerializableClass<  22,  Ident                 >();
  SetupSerializableClass<  23,  If                    >();
  SetupSerializableClass<  24,  Initializer           >();
  SetupSerializableClass<  25,  Label                 >();
  SetupSerializableClass<  26,  Member                >();
  SetupSerializableClass<  27,  MethodBody            >();
  SetupSerializableClass<  28,  New                   >();
  SetupSerializableClass<  29,  Enum                  >();
  SetupSerializableClass<  30,  Sequence              >();
  SetupSerializableClass<  31,  Shared                >();
  SetupSerializableClass<  32,  Expr                  >();
  SetupSerializableClass<  33,  Transfer              >();
  SetupSerializableClass<  34,  TypeOf                >();
  SetupSerializableClass<  35,  VarDecl               >();
  SetupSerializableClass<  36,  While                 >();
  SetupSerializableClass<  37,  Binary                >();
  SetupSerializableClass<  38,  BuildPointer          >();
  SetupSerializableClass<  39,  Call                  >();
  SetupSerializableClass<  40,  CallBuiltin           >();
  SetupSerializableClass<  41,  Cast                  >();
  SetupSerializableClass<  42,  Deref                 >();
  SetupSerializableClass<  43,  ExtractAddress        >();
  SetupSerializableClass<  44,  ExtractDescriptor     >();
  SetupSerializableClass<  45,  FieldAddr             >();
  SetupSerializableClass<  46,  GlobalAddr            >();
  SetupSerializableClass<  47,  Index                 >();
  SetupSerializableClass<  48,  LiteralAddr           >();
  SetupSerializableClass<  49,  Load                  >();
  SetupSerializableClass<  50,  Store                 >();
  SetupSerializableClass<  51,  Unary                 >();
  SetupSerializableClass<  52,  VarAddr               >();
  SetupSerializableClass<  53,  Vtable                >();
  SetupSerializableClass<  54,  VtableCast            >();
  SetupSerializableClass<  55,  VtableSlot            >();
  SetupSerializableClass<  56,  Quote                 >();
  SetupSerializableClass<  57,  Literal               >();
  SetupSerializableClass<  58,  Union                 >();
  SetupSerializableClass<  63,  OverloadSet           >();
  SetupSerializableClass<  64,  Derivation            >();
  SetupSerializableClass<  66,  Construct             >();
  SetupSerializableClass<  67,  Parameters            >();

  // The instances of these classes can be referenced by serialized objects,
  // but are not themselves serialized (except in module mode).
  SetupReferencedClass<    56,  Namespace             >();
  SetupReferencedClass<    59,  DeflatedObject        >();
  SetupReferencedClass<    60,  Integer               >();
  SetupReferencedClass<    61,  String                >();
  SetupReferencedClass<    62,  Value                 >();
}

void Metadata::SetupMetadata() {
  SetupHierarchy();
  SetupSerializations();
}
