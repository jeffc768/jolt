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

// A method entity describes a Jolt method.

#pragma once

#include "Entity.h"
#include "parser/ParserDecls.h"
#include "type/Type.h"
#include <vector>

class Apply;
class Argument;
class Class;
class Expr;
class Field;
class FormalArguments;
class OverloadSet;
class String;
class TargetMethod;
class Token;
class TranslateClosure;
class VarDecl;

enum ArgumentMechanism: uint8_t;
enum Epoch: uint8_t;

using std::vector;

enum MethodKind: uint8_t {
  mk_method,
  mk_macro,
  mk_construct,
  mk_destruct,

  // An conflicted method entity is created in a class that inherits conflict-
  // ing definitions from multiple base classes, *and* is not otherwise re-
  // defined in the inheriting class.  This is not an error so long as no
  // attempt is made to use it.  m_redefines lists the conflicting definitions
  // (which do not include deferred methods).
  mk_conflicted,

  // A placeholder method entity is created in a class which inherits at most
  // one implementation from the base classes, *and* is not redefined in the
  // inheriting class.  This definition can be found in m_redefines.  If all
  // inherited definitions are deferred, this placeholder is also deferred.
  mk_placeholder
};

class Method: public Entity {
  DECLARE_ENTITY(Method)

  enum state_t: uint8_t {
    st_initial,
    st_setup,
    st_signature,
    st_resolved
  };

  state_t             m_state           = st_initial;

  friend class Class;
  friend class OverloadSet;

public:
  enum NativeMethods: uint8_t {
    nm_none,
    nm_operator_new,    // FIXME: nothrow version as well?
    nm_operator_placement_new,
    nm_operator_delete,
    nm_operator_placement_delete,
    nm_print_int,
    nm_print_long,
    nm_print_uint,
    nm_print_ulong,
    nm_print_bool,
    nm_print_string,
    nm_print_char,
    nm_print_float,
    nm_print_double,
    nm_build_apply,
    nm_signed_subrange,
    nm_unsigned_subrange
  };

protected:
  Method(Entity *parent, String *name, ::MethodKind mk);

  struct NewArguments {
    Entity              *m_parent           = nullptr;
    String              *m_name             = nullptr;
    Expr                *m_body             = nullptr;
    ::MethodKind         m_kind             = mk_method;
    NativeMethods        m_nativeMethod     = nm_none;
    FormalArguments     *m_formals          = nullptr;

    NewArguments(Entity *parent);
    void SetReturnType(Type t);
    void SetName(const char *name);
    void SetName(WellKnownString name);
    void AppendIn(const char *name, Type t);
    void AppendOut(ArgumentMechanism m, const char *name, Type t);
    void Append(ArgumentMechanism m, const char *name, Type t);
  };

  Method(NewArguments &args);

  void DeflateFields(Deflator &DF);
  Method(Inflator &IF);

  using Entity::SetParent;
  virtual void SetParent(Entity *parent, AttributeList *attrGroup);

  void BuildBody();

  virtual void AppendToHash(SHA512 &hash);

public:
  Method(Location sl, ::MethodKind mk, Token *name,
         AST::SafeArray<AST::MemberItem> formals, AST::FuncInfo fi,
         AST::MemberItem &rt, AST::ProcBody &body);

  template<typename... ARGS>
  Method(Entity *parent, ARGS... args)
      : Method(NewHelper1(NewArguments(parent), args...)) { }

  static Method *NewConflicted(Entity *parent, String *name) {
    return new Method(parent, name, mk_conflicted);
  }

  static Method *NewPlaceholder(Entity *parent, String *name) {
    return new Method(parent, name, mk_placeholder);
  }

  virtual void BindNames(SymbolTable &st);
  virtual EntityScoping Scoping() { return es_member; }

  void Setup();
  void ResolveSignature();
  virtual void ResolveFully();

  virtual Node *AsValue(Location sl);
  virtual void *GlobalStorage(Epoch ep);
  virtual const std::string &GlobalStorageName();
  virtual ResolutionState GetResolutionState();
  virtual void AddToClosure(TranslateClosure *tc);
  virtual void FinalizeClosure(TranslateClosure *tc);
  virtual void SetExecutable();

  // Order this method with respect to the other.  Returns 0 if the two have
  // the same signature, otherwise 1 or -1 is returned based on which one is
  // considered "greater" or "lesser" (arbitrary, but must be consistent).
  int SignatureCompare(Method *other);

  ::MethodKind MethodKind() { return m_kind; }
  Expr *Body() { return m_body; }
  bool IsNative() { return m_nativeMethod != nm_none; }
  VariadicType Variadic();

  // Make this method instance or static.
  void MakeInstance(Class *cls);
  void MakeStatic(Class *metaclass);
  bool IsStatic() { return m_isStatic; }

  // Mark this method as being a certain kind of generated method that needs
  // special handling.
  enum GeneratedKind {
    gm_unmarked,
    gm_constructor,
    gm_copy_constructor,
    gm_destructor,
    gm_assignment
  };
  void SetGenerated(GeneratedKind gk);
  bool IsGenerated() { return m_generatedKind != gm_unmarked; }

  // Whether the method takes no arguments.
  bool HasNoArgs();

  // Lower the resolved method signature.
  FormalArguments *LowerFormals();

  // Query virtual properties of this method.
  bool IsVirtual() { return m_isVirtual; }
  unsigned GetVtblSlot();

  // Get the target object for this class.
  TargetMethod *GetTarget(Epoch ep);

  // Get the Main method of the program (null if one wasn't present).
  static Method *Main() { return s_main; }

private:
  // Helper methods to process variadic template arguments of New().
  // First up, NewHelper1 to do the method name.
  template<typename... ARGS>
  static NewArguments &NewHelper1(NewArguments &&newargs, const char *name,
                                  ARGS... args) {
    newargs.SetName(name);
    return NewHelper2(newargs, args...);
  }

  template<typename... ARGS>
  static NewArguments &NewHelper1(NewArguments &&newargs, WellKnownString name,
                                  ARGS... args) {
    newargs.SetName(name);
    return NewHelper2(newargs, args...);
  }

  template<typename... ARGS>
  static NewArguments &NewHelper1(NewArguments &&newargs, String *name,
                                  ARGS... args) {
    newargs.m_name = name;
    return NewHelper2(newargs, args...);
  }

  // Next, NewHelper2 does the method type and body.
  template<typename... ARGS>
  static NewArguments &NewHelper2(NewArguments &newargs, NativeMethods nm,
                                  ARGS... args) {
    verify(nm != nm_none);
    newargs.m_kind = mk_method;
    newargs.m_nativeMethod = nm;
    return NewHelper3(newargs, args...);
  }

  template<typename... ARGS>
  static NewArguments &NewHelper2(NewArguments &newargs, ::MethodKind kind,
                                  Expr *body, ARGS... args) {
    newargs.m_kind = kind;
    newargs.m_body = body;
    return NewHelper3(newargs, args...);
  }

  // Next, NewHelper3 does the return type.
  template<typename... ARGS>
  static NewArguments &NewHelper3(NewArguments &newargs, Type t, ARGS... args) {
    newargs.SetReturnType(t);
    return NewHelper4(newargs, args...);
  }

  template<typename... ARGS>
  static NewArguments &NewHelper3(NewArguments &newargs, ARGS... args) {
    return NewHelper4(newargs, args...);
  }

  // Finally, NewHelper4 recursively does all the arguments.
  template<typename... ARGS>
  static NewArguments &NewHelper4(NewArguments &newargs) {
    return newargs;
  }

  template<typename... ARGS>
  static NewArguments &NewHelper4(NewArguments &newargs, const char *name,
                                  Type t, ARGS... args) {
    newargs.AppendIn(name, t);
    return NewHelper4(newargs, args...);
  }

  template<typename... ARGS>
  static NewArguments &NewHelper4(NewArguments &newargs, ArgumentMechanism m,
                                  const char *name, Type t, ARGS... args) {
    newargs.Append(m, name, t);
    return NewHelper4(newargs, args...);
  }

  // Once the total ordering has been determined, insert implicit construct
  // and destruct statements.
  void InsertImplicitConstructs(vector<VarDecl *> &fields);
  Node *InitVtableAddresses(Node *obj, Class *ce, Node *vtbl);
  void InsertImplicitDestructs(vector<VarDecl *> &fields);

  // Create body of auto-generated operator= method.
  void InsertImplicitAssignments();

  ::MethodKind        m_kind;
  bool                m_isConst         = false;
  NativeMethods       m_nativeMethod    = nm_none;
  Expr               *m_body            = nullptr;
  FormalArguments    *m_formals         = nullptr;

  // Is this method static?  Only relevant for class members.
  bool                m_isStatic        = false;

  // Does this method redefine an inherited method?  There must be an inherited
  // definition, and this method uses the same vtable slot.
  bool                m_isRedefining    = false;

  // Does this method hide an inherited method?  There must be an inherited
  // definition, but this method is assigned a new vtable slot.
  bool                m_isHiding        = false;

  // Does this method introduce a new method?  There cannot be an inherited
  // definition, and obviously this method is assigned a new vtable slot.
  bool                m_isIntroducing   = false;

  // Is this method deferred?  It might still have a body.
  bool                m_isDeferred      = false;

  // Is this method final?  This may be set true if the compiler determines it
  // is never redefined, even if the attribute is not present.
  bool                m_isFinal         = false;

  // Is this method implementation not to be inherited?
  bool                m_isNoInherit     = false;

  // Is this method to be auto redefined in all subclasses?
  bool                m_isAutoRedefine  = false;

  // Is this method extern C?
  bool                m_isExternC       = false;

  struct InheritedMethod {
    // The inherited method.
    Method           *m_method          = nullptr;

    // Path through inheritance hierarchy.  The ints subscript the bases
    // array from the first inheriting class until the present one.
    vector<size_t>    m_path;

    InheritedMethod() { }

    InheritedMethod(Method *me) : m_method(me) { }

    InheritedMethod(Method *me, size_t baseIndex) : m_method(me) {
      m_path.push_back(baseIndex);
    }

    InheritedMethod(const InheritedMethod &that, size_t baseIndex)
        : m_method(that.m_method), m_path(that.m_path) {
      m_path.push_back(baseIndex);
    }

    InheritedMethod(const InheritedMethod &that) : m_method(that.m_method) {
      m_path.swap(const_cast<InheritedMethod &>(that).m_path);
    }

    void UpdateManagedAddresses() {
      m_method = m_method;
    }

    void Deflate(Deflator &DF) const;
    void Inflate(Inflator &IF);
  };

  friend void operator<<(Deflator &DF, const Method::InheritedMethod &ie);
  friend void operator>>(Inflator &IF, Method::InheritedMethod &ie);

  // Flag an auto-generated method.
  GeneratedKind       m_generatedKind     = Method::gm_unmarked;

  // The set of vtable slots covered by this method entity.  A slot is created
  // for each introducing method, and due to multiple inheritance a redefinition
  // may affect many slots.  Each covered slot is represented by the method
  // entity that introduced it.
  using InheritedMethods = vector<InheritedMethod>;
  InheritedMethods    m_coverage;

  // The set of directly inherited methods redefined by this method, potentially
  // one for each base class.  By definition, this set is empty for introducing
  // or hiding methods.
  InheritedMethods    m_redefines;

  // The set of methods in subclasses that directly redefine this method.
  vector<Method *>    m_redefinedBy;

  // Set m_coverage and m_redefines.  Deduce the argument list from them if
  // necessary.
  void SetCoverageAndRedefines(InheritedMethods &coverage,
                               InheritedMethods &redefines);

  // Vtable slot assigned this entity.  Applicable only if this is a new
  // signature and not a redefinition of an inherited signature.  A value of
  // -1 means a slot has not (yet) been assigned.  This value applies only to
  // compile time, as run time uses C++ structs to represent vtables (though
  // the slot # is used for member names).
  int                 m_vtblSlot        = -1;

  // Place ourselves into our assigned vtable slot(s).
  void SetVtblSlots();

  // Is this method declared virtual?
  bool                m_isVirtual       = false;

  // Is this method redefined in any subclass?  This can't be known until
  // whole program analysis is done (in other words, we assume it is during
  // compile time execution).
  bool                m_isRedefined     = false;

  // Has the function been added to the compile time closure?
  bool                m_inClosure       = false;

  // Has the function been translated to bytecodes?  We only need to track
  // compile time, as a closure is computed for run time only once.
  bool                m_isTranslated    = false;

  // Is the compile time version executable?  In other words, has it and all
  // methods and classes it uses (directly or indirectly) been translated?
  bool                m_isExecutable    = false;

  // Array of method and class entities this method's body uses and must be
  // translated in order for this method to be usable.  Computed at the time
  // the method is translated.
  vector<Entity *>    m_dependencies;

  // The target objects for this method, one for each epoch.
  TargetMethod       *m_runTarget       = nullptr;
  TargetMethod       *m_compileTarget   = nullptr;

  // The one and only main entry point to the program.
  static Method      *s_main;
};

inline void operator<<(Deflator &DF, const Method::InheritedMethod &ie) {
  ie.Deflate(DF);
}

inline void operator>>(Inflator &IF, Method::InheritedMethod &ie) {
  ie.Inflate(IF);
}
