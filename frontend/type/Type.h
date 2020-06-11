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

// Types are implemented as a collection of packed bit fields within a 32-bit
// value.  This appears to be the most efficient representation, as it avoids a
// combinatorial explosion of type objects for every possible mixture of const-
// ness and reference-ness, while still permitting type values to be efficiently
// passed around.
//
// Only a few type kinds are simple enough to be completely described by a 32-
// bit value.  For the others, an auxiliary info record is kept in a separate
// heap.  An offset into this heap is stored in the type value.  Note that the
// heap is re-allocated as needed as type information accumulates; it remains
// a contiguous block of memory to keep info records lookups efficient.  This
// storage is not garbage collected; it would be a waste of time, as types once
// created almost never become unused or inaccessible.  Info records may contain
// pointers to managed objects and these pointers must be kept up-to-date when
// GC takes place.
//
// Type values may be tested for equality by simply comparing their 32-bit
// reprentation.  They are equal if and only if the types are identical.  If
// a type value needs to be hashed, as when it is used as a key in a hash table,
// it is for this reason sufficient to hash only the 32-bit representation.

#pragma once

#include "util/Verify.h"

#include <stddef.h>
#include <stdint.h>
#include <string>
#include <vector>

class BufferWriter;
class Class;
class Integer;
class SHA512;
class String;
class Value;

using std::vector;

// Generally, MAYBE is used to signify assignable types as opposed to subtypes.
enum YesNoMaybe: uint8_t { NO, MAYBE, YES };

// Types are categorized by the following major type kinds.  The kind determines
// how the offset field is interpreted.
enum TypeKind: uint8_t {
  tk_unknown,
  tk_class,
  tk_pointer,
  tk_fatpointer,
  tk_void,
  tk_type,
  tk_namespace,
  tk_pseudo,
  tk_tuple,
  tk_union,
  tk_function,
  tk_valueless,
  tk_bool,
  tk_float,
  tk_char,
  tk_integer,
  tk_enum,
  tk_array,
  tk_set,
  tk_concept,
  tk_generic,
  tk_codefragment
  // tk_function
  // tk_lexicalscope
  // There are no builtin types for strings and maps
};

// All types have a reference property indicating how the value represented by
// the type is stored.
enum RefType: uint8_t {
  rt_rvalue,      // Computed value with no address; it cannot be assigned to
  rt_rvalueref,   // A temporary holding a computed value
  rt_lvalueref    // Value has an address; it can be assigned to
};

// All types have a constness property.
enum ConstType: uint8_t {
  ct_const,
  ct_mutable
};

enum VariadicType: uint8_t {
  vt_none,        // Fixed-length argument list
  vt_jolt,        // Jolt-style variadic argument list
  vt_c            // C-style variadic argument list
};

class Type {
  // Note: can't use enum bit fields, because the various compilers cannot
  // both avoid compilation errors or generate correct code.
  unsigned            m_offset : 24;
  unsigned            m_kind   :  5;
  unsigned            m_ref    :  2;
  unsigned            m_const  :  1;

  // Pseudo types are the types of literal values before they are adopted by a
  // "real" type.
  enum PseudoTypes {
    t_PseudoInteger,
    t_PseudoFloat,
    t_PseudoChar,
    t_PseudoString,
    t_PseudoList,
    t_PseudoEnum,
    t_MaxPseudo
  };

  // Valueless types are the types of expressions that cannot yield a value,
  // because they cannot complete execution or because they cannot even begin
  // execution for some reason.
  enum ValuelessTypes {
    t_Transfers,  // Flow of control unconditionally transfers elsewhere
    t_Unbound,    // Contains unbound generic parameter; no object code created
    t_Suppress,   // Compilation error occured; no object code created
    t_NotTyped,   // Failure to type an expression
    t_MaxValueless
  };

  enum FloatTypes {
    t_Float,
    t_Double,
    t_MaxFloat
  };

  constexpr Type(TypeKind k, unsigned o)
      : m_offset(o),
        m_kind(k),
        m_ref(rt_rvalue),
        m_const(ct_mutable) { }

  constexpr Type(Type t, ConstType ct)
      : m_offset(t.m_offset),
        m_kind(t.m_kind),
        m_ref(t.m_ref),
        m_const(ct) { }

  constexpr Type(Type t, RefType rt)
      : m_offset(t.m_offset),
        m_kind(t.m_kind),
        m_ref(rt),
        m_const(t.m_const) { }

  constexpr Type(Type t, RefType rt, ConstType ct)
      : m_offset(t.m_offset),
        m_kind(t.m_kind),
        m_ref(rt),
        m_const(ct) { }

public:
  constexpr Type()
      : m_offset(0),
        m_kind(tk_unknown),
        m_ref(rt_rvalue),
        m_const(ct_mutable) { }

  // Return the conceptless types.
  constexpr static Type Void()      { return Type(tk_void,      0); }
  constexpr static Type JType()     { return Type(tk_type,      0); }
  constexpr static Type Namespace() { return Type(tk_namespace, 0); }

  // Return common integer subrange types.
  static Type SByte();
  static Type Byte();
  static Type Short();
  static Type UShort();
  static Type Int();
  static Type UInt();
  static Type Long();
  static Type ULong();

  // Machine dependent aliases for the above subranges.
  static Type IntPtrT()  { return sizeof(intptr_t)  == 4 ? Int()  : Long(); }
  static Type UIntPtrT() { return sizeof(uintptr_t) == 4 ? UInt() : ULong(); }
  static Type SizeT()    { return sizeof(size_t)    == 4 ? Int()  : Long(); }
  static Type PtrDiffT() { return sizeof(ptrdiff_t) == 4 ? Int()  : Long(); }

  static Type Address() { return PointerTo(Void()); }

  // Return arbitrary integer subrange type.
  static Type IntegerSubrange(Integer *lower, Integer *upper,
                              bool is_signed = true);

  // Return character subrange type.
  static Type CharSubrange(uint32_t lower, uint32_t upper);

  // Return the pseudo types.
  constexpr static Type PseudoInteger()
    { return Type(tk_pseudo, t_PseudoInteger); }
  constexpr static Type PseudoFloat()
    { return Type(tk_pseudo, t_PseudoFloat); }
  constexpr static Type PseudoChar()
    { return Type(tk_pseudo, t_PseudoChar); }
  constexpr static Type PseudoString()
    { return Type(tk_pseudo, t_PseudoString); }
  constexpr static Type PseudoList()
    { return Type(tk_pseudo, t_PseudoList); }
  constexpr static Type PseudoEnum()
    { return Type(tk_pseudo, t_PseudoEnum); }

  // Return the valueless types.
  constexpr static Type Transfers() { return Type(tk_valueless, t_Transfers); }
  constexpr static Type Unbound()   { return Type(tk_valueless, t_Unbound); }
  constexpr static Type Suppress()  { return Type(tk_valueless, t_Suppress); }
  constexpr static Type NotTyped()  { return Type(tk_valueless, t_NotTyped); }

  // Return the bool type.
  constexpr static Type Bool()      { return Type(tk_bool, 0); }

  // Return the floating-point types.
  constexpr static Type Float()     { return Type(tk_float, t_Float); }
  constexpr static Type Double()    { return Type(tk_float, t_Double); }

  // Return the character types.
  static Type Char8();
  static Type Char16();
  static Type Char32();

  // Return a function type with the given attributes.
  // FIXME: how to handle static links?
  // FIXME: constructors? does that need to be known?
  static Type Function() { return Function(Type::Void(), { }); }
  static Type Function(Type rt, const vector<Type> &args,
                       bool method = false, bool throws = false,
                       VariadicType vt = vt_none);

  // Return a tuple type with the given collection of fields.
  static Type Tuple(const vector<Type> &fields);
  static Type Tuple(const vector<std::pair<String *, Type>> &fields);

  // Return a union type with the given set of variants.
  static Type Union(const vector<std::pair<String *, Type>> &variants);
  static Type Union(Type enumType);

  // Return an enum type with the given ordered list of identifiers and an
  // optional list of values.
  static Type Enum(const vector<String *> &idents);
  static Type Enum(const vector<String *> &idents, Type valueType,
                   const vector<Value *> &values);

  // Return a pointer to another (non-pseudo) type.  Forming a pointer to
  // Unbound returns Unbound.
  static Type PointerTo(Type base);
  static Type ThinPointerTo(Type base);

  // Return an array type with the given element type and (optional) index type.
  // An array without an index type is an adpatable array.
  static Type ArrayOf(Type element) { return ArrayOf(element, Type()); }
  static Type ArrayOf(Type element, Type index);

  // Return a set type with the given element type.  The cardinality of this
  // type determines the fixed size of the set.
  static Type SetOf(Type element);

  // Helper methods for constructing type of string literals, which are all
  // arrays.
  static Type StringRef();
  static Type StringRef(size_t len);
  bool IsString();

  // Return a concept type with the number and type of unbound generic
  // parameters supplied by a tuple type.
  static Type Concept(Type t) {
    verify(t.m_kind == tk_tuple);
    return Type(tk_concept, t.m_offset);
  }

  // Return a generic value type with the number and type of unbound generic
  // parameters supplied by a tuple type.
  static Type Generic(Type t) {
    verify(t.m_kind == tk_tuple);
    return Type(tk_generic, t.m_offset);
  }

  // Return a code fragment type.
  static Type CodeFragment(Type type = Type());

  // Return corresponding const or mutable version of the type.
  constexpr Type Const() const           { return Type(*this, ct_const); }
  constexpr Type Mutable() const         { return Type(*this, ct_mutable); }

  Type CopyConst(Type s) const { return Type(*this, s.ConstKind()); }
  ConstType ConstKind() const  { return static_cast<ConstType>(m_const); }

  // Return corresponding reference version of the type.
  constexpr Type RValue() const          { return Type(*this, rt_rvalue); }
  constexpr Type LValueRef() const       { return Type(*this, rt_lvalueref); }
  constexpr Type RValueRef() const       { return Type(*this, rt_rvalueref); }

  Type CopyRef(Type s) const   { return Type(*this, s.RefKind()); }
  RefType RefKind() const      { return static_cast<RefType>(m_ref); }

  // Return version of type minus any qualifiers.
  Type DropQualifiers() const  { return Type(RawKind(), m_offset); }

  // Query type kind.
  TypeKind Kind() const {
    return m_kind == tk_fatpointer ? tk_pointer : RawKind();
  }

  TypeKind RawKind() const { return static_cast<TypeKind>(m_kind); }

  // Are two types equivalent?
  bool operator==(Type t) const {
    return *reinterpret_cast<const unsigned *>(this) ==
           *reinterpret_cast<unsigned *>(&t);
  }

  bool operator!=(Type t) const {
    return *reinterpret_cast<const unsigned *>(this) !=
           *reinterpret_cast<unsigned *>(&t);
  }

  int Compare(Type t) const {
    return *reinterpret_cast<const unsigned *>(this) -
           *reinterpret_cast<unsigned *>(&t);
  }

  // Is this type of the specified kind?
  bool operator==(TypeKind tk) const { return Kind() == tk; }
  bool operator!=(TypeKind tk) const { return Kind() != tk; }

  // Is this type of the specified reference type?
  bool operator==(RefType rt) const { return m_ref == rt; }
  bool operator!=(RefType rt) const { return m_ref != rt; }

  // Is this type of the specified const type?
  bool operator==(ConstType ct) const { return m_const == ct; }
  bool operator!=(ConstType ct) const { return m_const != ct; }

  // Define ordering relationship between types.
  bool operator<(Type t) const {
    return *reinterpret_cast<const unsigned *>(this) <
           *reinterpret_cast<unsigned *>(&t);
  }

  bool IsKnown() const { return m_kind != tk_unknown; }

  // Return cardinality of type, e.g. the number of possible discrete values a
  // variable of this type can have.  Types for which cardinality is undefined
  // return -1.  If it is defined but is too large to fit in an intptr_t, -2 is
  // is returned.  Any type with a positive cardinality may be used as an index
  // type of an array or be the object of a set.
  intptr_t Cardinality() const;

  // Query other type attributes.  These assert if the query does not make sense
  // for the type kind.
  ::Class *Class() const; // Only valid on CLASS types.
  size_t StorageSize() const; // Not valid on pseudo types.
  unsigned Alignment() const; // Not valid on pseudo types.
  bool IsSigned() const;

  // This group is valid for CLASS, FATPOINTERs, UNIONs, and TUPLEs only.
  size_t FieldCount() const;          // also CONCEPTs and GENERICs
  size_t OffsetOf(size_t ord) const;
  Type TypeOf(size_t ord) const;      // also CONCEPTs and GENERICs

  // This group is valid for UNIONs only.
  Type TagType() const;

  // This group is valid for POINTERs, FATPOINTERs, and CODEFRAGMENTs only.
  Type BaseType() const;          // also ENUM and SETs
  bool IsThin() const;
  Type DescriptorType() const;    // FATPOINTER only

  // This group is valid for INTEGERs only.
  Integer *IntLowerBound() const;
  Integer *IntUpperBound() const;

  // This group is valud for CHARs only.
  uint32_t CharLowerBound() const;
  uint32_t CharUpperBound() const;

  // This group is valid for ENUMs only.
  Type AsSubrange() const;
  size_t IdentCount() const;
  bool HasExplicitValues() const;
  Value *ValueAt(size_t ord) const;

  // This group is valid for FUNCTIONs only.
  Type ReturnType() const;
  VariadicType Variadic() const;
  bool IsMethod() const;
  Type SetMethod(bool flag) const;
  bool Throws() const;
  Type SetThrows(bool flag) const;
  size_t ArgCount() const;
  Type ArgType(size_t ord) const;

  // This group is valid for ENUMs and TUPLEs.
  String *IdentAt(size_t ord) const;
  ssize_t OrdinalOf(String *ident) const;

  // This group is valid for ARRAYs only.
  Type ElementType() const;       // also ENUMs and SETs
  Type IndexType() const;
  Type SetIndexType(Type t) const;
  Type SetIndexType(size_t size) const;

  // Query whether this type is a subtype of another.  A response of MAYBE means
  // that a run-time check is called for or, if checks are disabled, means the
  // same as YES.
  YesNoMaybe IsSubtypeOf(Type t) const;

  // Similar to above, but applies to pointed-at types; e.g. two classes will
  // usually respond NO to IsSubtypeOf but may response YES to IsPtrSubtypeOf.
  // Should be used only by IsSubtypeOf.  Also highly unlikely to return MAYBE.
  YesNoMaybe IsPtrSubtypeOf(Type t) const;

  // Does the type map to a primitive type in C++?  This generally means types
  // such int or pointers, but could potentially include any literable class
  // whose instances are 8 bytes or less in size and has a trivial destructor.
  bool IsSimpleType() const;

  // Same as above, but excludes unit types (such as void) that do not have a
  // representation in object code.
  bool IsSimpleNonUnit() const;

  // Does the type have a destructor?  This is most interesting for classes,
  // which may or may not depending on the presence of an explicit destructor
  // or the types of its fields.
  bool HasDestructor() const;

  // Join two types together, returning the joined type.  If they cannot be
  // joined, NotTyped is returned.
  static Type Join(Type t1, Type t2);

  // Lower a type; for example, a reference to the implementing pointer type.
  Type Lower() const {
    if (m_ref == rt_rvalue)
      return *this;
    else
      return PointerTo(this->RValue());
  }

  // Adjust this type to pefectly forward a value of the supplied type.
  Type PerfectForward(Type t) const;

  // Return a reference to a piece of data that can be used by the run time
  // target for whatever purpose it wants.
  void *&TargetData();

  void AppendToHash(SHA512 &hash) const;

  // Dump printable representation of type to buffer.  Returns true if buffer
  // becomes full.
  bool Dump(BufferWriter &bw) const;

  // Variant called directly from within debuggers.
  const char *Dump() const;

  friend const char *LLDBFormatType(int o, int k, int r, int c);
  friend const char *FormatType(Type t);

private:
  // Create type for class entity -- only to be used by Class.
  friend class ::Class;
  static Type GetClassType(::Class *ce);

  // Update address of class entity for class type -- only to be used by Class
  // during garbage collection.
  void UpdateClass(::Class *ce);

  // Compute layout of fields in a tuple.
  void ComputeLayout() const;

  // Resolve any entities referred to by this type, directly or indirectly,
  // until the storage size is known.
  void ResolveEntities() const;
};

// For use by gdb and lldb.
const char *LLDBFormatType(int o, int k, int r, int c);
const char *FormatType(Type t);

namespace std {
  template<> struct hash<::Type> {
    size_t operator()(const ::Type &key) const {
      const uint32_t *p = reinterpret_cast<const uint32_t *>(&key);
      return hash<uint32_t>()(*p);
    }
  };
}
