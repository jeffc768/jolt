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

// Member node states related to members on type values.

#include "entity/Class.h"
#include "entity/Const.h"
#include "entity/Generic.h"
#include "entity/Method.h"
#include "entity/Namespace.h"
#include "node/Binary.h"
#include "node/BuildPointer.h"
#include "node/CallBuiltin.h"
#include "node/Cast.h"
#include "node/Expr.h"
#include "node/Node.h"
#include "node/ExtractAddress.h"
#include "node/Ident.h"
#include "node/List.h"
#include "node/Literal.h"
#include "node/Member.h"
#include "node/NativeOperator.h"
#include "target/Bytecode.h"
#include "target/Target.h"
#include "util/Integer.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"

namespace {
  class GenericType {
    Generic              *m_entity;
    WellKnownString       m_name;

  public:
    GenericType(WellKnownString name) : m_name(name) { }

    void Resolve() {
      if (!m_entity) {
        Namespace *ne = Namespace::StdNamespace();
        String *s = String::Get(m_name);
        auto e = ne->LookupEntity(s);
        m_entity = safe_cast<Generic *>(e);
      }

      m_entity->ResolveFully();
    }

    Node *AsValue(Location sl) {
      return m_entity->AsValue(sl);
    }
  };

  static GenericType g_ConformantArray(wks_ConformantArray);
  static GenericType g_FixedArray(wks_FixedArray);
  static GenericType g_Pointer(wks_Pointer);
  static GenericType g_Set(wks_Set);

  class TypePointerTo: public NativeOperator {
  public:
    TypePointerTo() : NativeOperator("TypePointerTo") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      Node *e = g_Pointer.AsValue(an->m_receiver->m_location);
      e = new Member(an->m_location, e, wkhs_op_generic, true);
      e = new Apply(e, nullptr, an->m_receiver);
      return e;
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = Type::JType();
      return an->m_formalTypes.size() == 0 ? Type::JType() : Type::Suppress();
    }

    static TypePointerTo s_macro;
  };

  TypePointerTo TypePointerTo::s_macro;

  class TypeSetOf: public NativeOperator {
  public:
    TypeSetOf() : NativeOperator("TypeSetOf") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      Node *e = g_Set.AsValue(an->m_receiver->m_location);
      e = new Member(an->m_location, e, wkhs_op_generic, true);
      e = new Apply(e, nullptr, an->m_receiver);
      return e;
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = Type::JType();
      return an->m_formalTypes.size() == 0 ? Type::JType() : Type::Suppress();
    }

    static TypeSetOf s_macro;
  };

  TypeSetOf TypeSetOf::s_macro;

  class ToConst: public BC::NativeFunction {
  public:
    ToConst() : BC::NativeFunction("ToConst") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra);
      Type t = args[0].AsType().Const();
      BC::Union rv;
      rv.SetType(t);
      return rv;
    }
  };

  static ToConst g_ToConst;

  class TypeToConst: public NativeOperator {
  public:
    TypeToConst() : NativeOperator("TypeToConst") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      CallBuiltin *cbn;
      cbn = new CallBuiltin(an->m_receiver->m_location,
                            Type::JType(), &g_ToConst);
      cbn->AppendArg(an->m_receiver);
      return cbn;
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = Type::JType();
      return an->m_formalTypes.size() == 0 ? Type::JType() : Type::Suppress();
    }

    static TypeToConst s_macro;
  };

  TypeToConst TypeToConst::s_macro;

  class ToMutable: public BC::NativeFunction {
  public:
    ToMutable() : BC::NativeFunction("ToMutable") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra);
      Type t = args[0].AsType().Mutable();
      BC::Union rv;
      rv.SetType(t);
      return rv;
    }
  };

  static ToMutable g_ToMutable;

  class TypeToMutable: public NativeOperator {
  public:
    TypeToMutable() : NativeOperator("TypeToMutable") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      CallBuiltin *cbn;
      cbn = new CallBuiltin(an->m_receiver->m_location,
                            Type::JType(), &g_ToMutable);
      cbn->AppendArg(an->m_receiver);
      return cbn;
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = Type::JType();
      return an->m_formalTypes.size() == 0 ? Type::JType() : Type::Suppress();
    }

    static TypeToMutable s_macro;
  };

  TypeToMutable TypeToMutable::s_macro;

  class LReferenceTo: public BC::NativeFunction {
  public:
    LReferenceTo() : BC::NativeFunction("LReferenceTo") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra);
      Type t = args[0].AsType().LValueRef();
      BC::Union rv;
      rv.SetType(t);
      return rv;
    }
  };

  static LReferenceTo g_LReferenceTo;

  class TypeLReferenceTo: public NativeOperator {
  public:
    TypeLReferenceTo() : NativeOperator("TypeLReferenceTo") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      CallBuiltin *cbn;
      cbn = new CallBuiltin(an->m_receiver->m_location,
                            Type::JType(), &g_LReferenceTo);
      cbn->AppendArg(an->m_receiver);
      return cbn;
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = Type::JType();
      return an->m_formalTypes.size() == 0 ? Type::JType() : Type::Suppress();
    }

    static TypeLReferenceTo s_macro;
  };

  TypeLReferenceTo TypeLReferenceTo::s_macro;

  class RReferenceTo: public BC::NativeFunction {
  public:
    RReferenceTo() : BC::NativeFunction("RReferenceTo") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra);
      Type t = args[0].AsType();
      if (t != rt_lvalueref)
        t = t.RValueRef();
      BC::Union rv;
      rv.SetType(t);
      return rv;
    }
  };

  static RReferenceTo g_RReferenceTo;

  class TypeRReferenceTo: public NativeOperator {
  public:
    TypeRReferenceTo() : NativeOperator("TypeRReferenceTo") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      CallBuiltin *cbn;
      cbn = new CallBuiltin(an->m_receiver->m_location,
                            Type::JType(), &g_RReferenceTo);
      cbn->AppendArg(an->m_receiver);
      return cbn;
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = Type::JType();
      return an->m_formalTypes.size() == 0 ? Type::JType() : Type::Suppress();
    }

    static TypeRReferenceTo s_macro;
  };

  TypeRReferenceTo TypeRReferenceTo::s_macro;

  class ArraySizeToType: public BC::NativeFunction {
  public:
    ArraySizeToType() : BC::NativeFunction("ArraySizeToType") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      // FIXME: look for uinreasonable values.  Subrange checking should catch
      // these too.
      verify(!extra);
      Integer *low = Integer::Get(wki_zero);
      Integer *high = Integer::Get(args[0].AsSizeT() - 1);
      Type t = Type::IntegerSubrange(low, high);
      BC::Union rv;
      rv.SetType(t);
      return rv;
    }
  };

  static ArraySizeToType g_ArraySizeToType;

  class TypeArrayOf: public NativeOperator {
    template<typename... T>
    static Node *FormArrayType(Apply *an, GenericType &gt, T... args) {
      Node *e = gt.AsValue(an->m_receiver->m_location);
      e = new Member(an->m_location, e, wkhs_op_generic, true);
      return new Apply(e, nullptr, args..., an->m_receiver);
    }

  public:
    TypeArrayOf() : NativeOperator("TypeArrayOf") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() == 0) {
        // No index type; run the element type through ConformantArray
        // generic type.
        return FormArrayType(an, g_ConformantArray);
      } else {
        // Index type present; run index and element types through
        // FixedArray generic type.
        Node *idx = an->m_arguments[0];

        // For convenience, the index type may be specified as a simple number,
        // which we'll convert to a subrange type.
        if (idx->m_type != tk_type) {
          CallBuiltin *cbn;
          cbn = new CallBuiltin(an->m_arguments[0]->m_location,
                                Type::JType(), &g_ArraySizeToType);
          cbn->AppendArg(idx);
          idx = cbn;
        }

        return FormArrayType(an, g_FixedArray, idx);
      }
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = Type::JType();
      if (an->m_formalTypes.size() == 0)
        return Type::JType();
      if (an->m_formalTypes.size() > 1)
        return Type::Suppress();

      // FIXME: handle other index types.
      Type it = an->m_arguments[0]->m_type.DropQualifiers();
      if (it == tk_type)
        an->m_formalTypes[0] = Type::JType(); // FIXME: restrict to cardinal types
      else if (it == Type::PseudoInteger() || it.IsSubtypeOf(Type::SizeT()))
        an->m_formalTypes[0] = Type::SizeT();
      else
        return Type::Suppress();
      return Type::JType();
    }

    static TypeArrayOf s_macro;
  };

  TypeArrayOf TypeArrayOf::s_macro;

  class ConstructFunctionType: public BC::NativeFunction {
  public:
    ConstructFunctionType() : BC::NativeFunction("ConstructFunctionType") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      Type rt = args[0].AsType();

      vector<Type> list;
      for (size_t i = 1; i < argcnt; i++)
        list.push_back(args[i].AsType());

      // FIXME: handle method signatures (first arg has parameter name "this").
      // FIXME: handle variadic.
      Type t = Type::Function(rt, list);
      BC::Union rv;
      rv.SetType(t);
      return rv;
    }
  };

  static ConstructFunctionType g_ConstructFunctionType;

  class TypeFunction: public NativeOperator {
  public:
    TypeFunction() : NativeOperator("TypeFunction") { }

    virtual ::MethodKind MethodKind() { return mk_construct; }

    virtual Node *Run(Apply *an, Context &ctx) {
      CallBuiltin *cbn;
      cbn = new CallBuiltin(an->m_receiver->m_location,
                            Type::JType(), &g_ConstructFunctionType);
      cbn->AppendArg(an->m_receiver);
      for (auto arg : an->m_arguments)
        cbn->AppendArg(arg);
      return cbn;
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = Type::JType();
      verify(an->m_arguments.size() >= 1);
      for (size_t i = 0; i < an->m_arguments.size(); i++) {
        Type t = an->m_arguments[i]->m_type.DropQualifiers();
        if (t.IsSubtypeOf(Type::JType()) == NO)
          return Type::Suppress();
        an->m_formalTypes[i] = Type::JType();
      }
      return Type::JType();
    }

    static TypeFunction s_macro;
  };

  TypeFunction TypeFunction::s_macro;

  class TypeEq: public NativeOperator {
  public:
    TypeEq() : NativeOperator("TypeEq") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      return new Binary(an->m_location, Binary::op_seteq, an->m_receiver,
                        an->m_arguments[0]);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();
      an->m_receiverType = Type::JType();
      an->m_formalTypes[0] = Type::JType();
      return Type::Bool();
    }

    static TypeEq s_macro;
  };

  TypeEq TypeEq::s_macro;

  class TypeNe: public NativeOperator {
  public:
    TypeNe() : NativeOperator("TypeNe") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      return new Binary(an->m_location, Binary::op_setne, an->m_receiver,
                        an->m_arguments[0]);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();
      an->m_receiverType = Type::JType();
      an->m_formalTypes[0] = Type::JType();
      return Type::Bool();
    }

    static TypeNe s_macro;
  };

  TypeNe TypeNe::s_macro;

  class TypeConstructSimple: public NativeOperator {
  public:
    TypeConstructSimple() : NativeOperator("TypeConstructSimple") { }

    virtual ::MethodKind MethodKind() { return mk_construct; }

    virtual Node *Run(Apply *an, Context &ctx) {
      Type t = an->m_type;
      if (an->m_formalTypes.size() == 0) {
        return new Literal(an->m_location, Value::New(t));
      } else {
        Node *e = an->m_arguments[0];
        if (e->m_type == t)
          return e;

        if (t == tk_float) {
          if (e->m_type == Type::PseudoInteger() ||
              e->m_type == Type::PseudoFloat()) {
            return e->LowerPseudos(t);
          } else if (e->IsNanOrInfinity()) {
            return e->LowerSpecialEnums(t);
          } else {
            e = e->LowerPseudos(Type());
            return new Cast(an->m_location, t, e);
          }
        }

        if (t == tk_integer) {
          if (e->m_type == Type::PseudoInteger()) {
            return e->LowerPseudos(t);
          } else if (e->IsMinOrMax()) {
            return e->LowerSpecialEnums(t);
          } else {
            e = e->LowerPseudos(Type());
            return new Cast(an->m_location, t, e);
          }
        }

        if (t == tk_char) {
          if (e->m_type == Type::PseudoChar()) {
            return e->LowerPseudos(t);
          } else if (e->IsMinOrMax()) {
            return e->LowerSpecialEnums(t);
          } else {
            e = e->LowerPseudos(Type());
            return new Cast(an->m_location, t, e);
          }
        }

        if (t == tk_pointer) {
          if (e->IsNull()) {
            return e->LowerSpecialEnums(t);
          } else {
            return new Cast(an->m_location, t, e);
          }
        }

        if (e->m_type != tk_pseudo)
          return new Cast(e->m_location, t, e);

        return e->LowerPseudos(t);
      }
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      Type t = an->m_receiver->EvaluateType();
      an->m_receiverType = Type::JType();
      if (an->m_formalTypes.size() == 0)
        return t;
      if (an->m_formalTypes.size() > 1)
        return Type::Suppress();
      // FIXME: handle casts.  This is basically allowing anything to be legal!
      Type u = an->m_arguments[0]->m_type.DropQualifiers();
      an->m_formalTypes[0] = u;
      return t;
    }

    static TypeConstructSimple s_macro;
  };

  TypeConstructSimple TypeConstructSimple::s_macro;

  class TypeConstructFromGeneric: public BC::NativeFunction {
  public:
    TypeConstructFromGeneric() : BC::NativeFunction("TypeConstructFromGeneric") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra);
      Derivation *d = reinterpret_cast<Derivation *>(args[0].AsAddr());
      BC::Union rv;
      switch (d->Kind()) {
        case gk_class:
          rv.SetType(d->CastToClassObject()->AsType());
          break;
        case gk_type: {
          // If this causes a fresh derivation, the new const entity will be
          // maximally unresolved and unable to yield a type value.  This can't
          // happen with class objects above, as even a maximally unresolved
          // class entity can yield its type.  For now, run a nested work unit
          // resolution loop until the const entity is resolved.  It is
          // questionable if this is the right way to do it, but the only
          // alternative is to abort the constant expression currently being
          // evaluated and retry later.  The framework needed to do that simply
          // doesn't exist yet.
          Const *ce = d->CastToTypeValue();
          ce->ResolveFully();
          verify(ce->m_object && ce->m_object->ObjectType() == Type::JType());
          rv.SetType(ce->m_object->AsType());
          break;
        }
        default:
          // Concept values have a different type and thus should never show up
          // here.
          verify(false);
      }
      return rv;
    }
  };

  static TypeConstructFromGeneric g_TypeConstructFromGeneric;

  class TypeConstructFromClass: public BC::NativeFunction {
  public:
    TypeConstructFromClass() : BC::NativeFunction("TypeConstructFromClass") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra);
      void *p = reinterpret_cast<void *>(args[0].AsAddr());
      Class *ce = Class::GetFromObject(p);
      BC::Union rv;
      rv.SetType(ce->AsType());
      return rv;
    }
  };

  static TypeConstructFromClass g_TypeConstructFromClass;

  class TypeConstructType: public NativeOperator {
  public:
    TypeConstructType() : NativeOperator("TypeConstructType") { }

    virtual ::MethodKind MethodKind() { return mk_construct; }

    virtual Node *Run(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() == 0) {
        Type t = Type::Void();
        return new Literal(an->m_location, Value::NewType(t));
      } else {
        if (an->m_formalTypes[0] == tk_type)
          return an->m_arguments[0];

        BC::NativeFunction *f = nullptr;
        if (an->m_formalTypes[0] == tk_generic)
          f = &g_TypeConstructFromGeneric;
        else if (an->m_formalTypes[0] == tk_class)
          f = &g_TypeConstructFromClass;
        else
          verify(false); // FIXME: handle Castable etc...

        CallBuiltin *cbn = new CallBuiltin(an->m_location, Type::JType(), f);
        cbn->AppendArg(an->m_arguments[0]);
        return cbn;
      }
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = an->m_receiverType.RValue();
      if (an->m_formalTypes.size() == 0)
        return Type::JType();
      if (an->m_formalTypes.size() > 1)
        return Type::Suppress();
      Type t = an->m_arguments[0]->m_type.DropQualifiers();
      if (t == Type::PseudoList()) {
        auto ln = safe_cast<List *>(an->m_arguments[0]);
        if (!ln->IsSubtypeOf(Type::JType()))
          return Type::Suppress();
        an->m_arguments[0] = ln->AsTupleType(an->m_location, ctx);
        t = Type::JType();
      } else if (t.IsSubtypeOf(Type::JType()) == NO) {
        return Type::Suppress();
      }
      an->m_formalTypes[0] = t;
      if (t == tk_class)
        an->m_formalTypes[0] = an->m_formalTypes[0].Const().LValueRef();
      return Type::JType();
    }

    static TypeConstructType s_macro;
  };

  TypeConstructType TypeConstructType::s_macro;

  class TypeConstructNamespace: public NativeOperator {
  public:
    TypeConstructNamespace() : NativeOperator("TypeConstructNamespace") { }

    virtual ::MethodKind MethodKind() { return mk_construct; }

    virtual Node *Run(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() == 0) {
        void *n = Namespace::StdNamespace()->GlobalStorage(ep_compile);
        return new Literal(an->m_location,
                           Value::NewRaw(Type::Namespace(), &n));
      } else {
        return an->m_arguments[0];
      }
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = an->m_receiverType.RValue();
      if (an->m_formalTypes.size() == 0)
        return Type::Namespace();
      if (an->m_formalTypes.size() > 1)
        return Type::Suppress();
      an->m_formalTypes[0] = Type::Namespace();
      return Type::Namespace();
    }

    static TypeConstructNamespace s_macro;
  };

  TypeConstructNamespace TypeConstructNamespace::s_macro;
}

namespace TC {
  extern NativeOperator *Array;
  extern NativeOperator *Pointer;
  extern NativeOperator *Set;
  extern NativeOperator *Tuple;
  extern NativeOperator *Void;
}

Node *Member::HandleTypes(Context &ctx) {
  // Handle special case of function type construction.
  if (m_expr && m_deref) {
    Apply *an;
    if (auto ln = dyn_cast<List *>(m_object))
      an = new Apply(m_location, &TypeFunction::s_macro, m_expr, &ln->m_values);
    else
      an = new Apply(m_location, &TypeFunction::s_macro, m_expr, m_object);
    return an->ResolveType(ctx);
  }

  // Go and get the member name.
  auto in = safe_cast<Ident *>(m_member);
  String *name = in->m_identifier;

  if (name == wkhs_op_ptr) {
    g_Pointer.Resolve();
    m_macro = &TypePointerTo::s_macro;
  } else if (name == wks_op_const) {
    m_macro = &TypeToConst::s_macro;
  } else if (name == wks_op_mutable) {
    m_macro = &TypeToMutable::s_macro;
  } else if (name == wkhs_op_ref) {
    m_macro = &TypeLReferenceTo::s_macro;
  } else if (name == wkhs_op_r_ref) {
    m_macro = &TypeRReferenceTo::s_macro;
  } else if (name == wks_op_index) {
    g_ConformantArray.Resolve();
    g_FixedArray.Resolve();
    m_macro = &TypeArrayOf::s_macro;
  } else if (name == wkhs_op_set) {
    g_Set.Resolve();
    m_macro = &TypeSetOf::s_macro;
  } else if (name == wks_op_eq) {
    m_macro = &TypeEq::s_macro;
  } else if (name == wks_op_ne) {
    m_macro = &TypeNe::s_macro;
  } else if (name == wks_op_apply) {
    goto construct_type;
  } else if (!m_operator || name == wkhs_op_generic) {
    goto cast_type;
  } else {
    m_reason = r_try_operator;
  }

  return this;

construct_type: {
    // We're not constructing a type, but rather a value of a type.  Before we
    // can go any further, we need to know the type.  So...
    {
      Context::PushVoid pv(ctx, false);
      m_object = m_object->ResolveFully(ctx);
    }
    Type t = m_object->EvaluateType();

    switch (t.Kind()) {
      case tk_bool:
      case tk_float:
      case tk_integer:
      case tk_char:
      case tk_enum:
        m_macro = &TypeConstructSimple::s_macro;
        break;
      case tk_type:
        m_macro = &TypeConstructType::s_macro;
        break;
      case tk_namespace:
        m_macro = &TypeConstructNamespace::s_macro;
        break;
      case tk_class:
        ReplaceObjectWithEntityRef(t.Class());
        m_state = st_initial;
        return this->ResolveFully(ctx);
      case tk_tuple:
        m_macro = TC::Tuple;
        break;
      case tk_pointer:
        m_macro = TC::Pointer;
        break;
      case tk_array:
        m_macro = TC::Array;
        break;
      case tk_set:
        m_macro = TC::Set;
        break;
      case tk_void:
        m_macro = TC::Void;
        break;
      case tk_valueless:
        m_reason = r_not_found;
        return this;
      default:
        verify(false);
    }

    auto in = safe_cast<Ident *>(m_member);
    if (in->m_identifier != wks_op_apply)
      m_reason = r_not_found;

    return this;
  }

cast_type: {
    // There are two possibilities here:
    //   1) The type is an enum type.  Do a member lookup.
    //   2) The type is a class type or a generic value.  Cast to a metaclass
    //      instance.
    // Regardless, the type's expression must be evaluated at compile time.
    {
      Context::PushVoid pv(ctx, false);
      m_object = m_object->ResolveFully(ctx);
    }
    Type t = m_object->EvaluateType();

    auto in = safe_cast<Ident *>(m_member);

    // Handle enum types.
    if (t == tk_enum) {
      size_t ord = t.OrdinalOf(in->m_identifier);
      if (ord != size_t(-1))
        return new Literal(m_location, Value::NewInt(t, ord));
      EmitError(this) << in->m_identifier << " is not an enum member.";
      ctx.m_error = true;
      m_type = Type::Suppress();
      return this;
    }

    // Handle tuple types.
    if (t == tk_tuple) {
      // Do member lookup.
      verify(false);
      return this;
    }

    // Otherwise, attempt cast to class object.
    if (t != tk_class) {
      EmitError(this) << "Illegal operation on types.";
      ctx.m_error = true;
      m_type = Type::Suppress();
      return this;
    }

    Class *ce = t.Class();
    if (in->m_identifier == wkhs_op_generic) {
      // For type<...>, the class must be a generic derivation.  Replace the
      // type with the base generic value.
      if (Derivation *d = ce->GetGenericBindings()) {
        d = d->m_entity->BaseGeneric();
        m_object = new Literal(m_location, Value::NewPtr(d->m_type, d));
      } else {
        EmitError(this) << "Illegal operation on types.";
        ctx.m_error = true;
        m_type = Type::Suppress();
        return this;
      }
    } else {
      // For type(...), retry member access on class object.
      ReplaceObjectWithEntityRef(ce);
    }

    // Redo with new object.
    m_state = st_initial;
    return this->ResolveFully(ctx);
  }
}

static void AddConst(Namespace *ne, const char *name, Type t) {
  Type tt = Type::JType();
  Const *ce = new Const(ne, name, tt, &t, sizeof(t));
  ne->AddBuiltinEntity(ce);
}

static void PopulateCB() {
  Namespace *ne = Namespace::StdNamespace();
  AddConst(ne, "type", Type::JType());
}

static PopulateNamespace g_pn(PopulateCB);
