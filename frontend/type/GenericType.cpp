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

// Member node states related to members on generic values.

#include "entity/Class.h"
#include "entity/Const.h"
#include "entity/Generic.h"
#include "node/Binary.h"
#include "node/CallBuiltin.h"
#include "node/Ident.h"
#include "node/Initializer.h"
#include "node/Member.h"
#include "node/NativeOperator.h"
#include "node/Store.h"
#include "util/CodeFragment.h"
#include "util/String.h"
#include "util/Value.h"

namespace {
  class GenericBindOp: public BC::NativeFunction {
  public:
    GenericBindOp() : BC::NativeFunction("GenericBindOp") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra);
      Derivation *d = reinterpret_cast<Derivation *>(args[0].AsAddr());

      // Convert the bound parameter values to Value.
      vector<Value *> jos;
      for (size_t i = 1; i < argcnt; i++) {
        Type t = d->m_type.TypeOf(jos.size());
        Value *v = nullptr;
        if (t == tk_array && !t.IndexType().IsKnown()) {
          void *argdata = args[i].AsAddr();
          size_t cnt = args[++i].AsSizeT();
          t = t.SetIndexType(cnt);
          verify(t.StorageSize() == cnt * t.ElementType().StorageSize());
          v = Value::NewRaw(t, argdata);
        } else {
          if (t.Lower() == tk_pointer && !t.Lower().IsThin()) {
            void *d[2] { args[i].AsAddr(), args[i + 1].AsAddr() };
            i++;
            verify(t.StorageSize() == 2 * sizeof(void *));
            v = Value::NewRaw(t, &d);
          } else if (t == tk_integer) {
            if (t.StorageSize() <= 4)
              v = Value::NewInt(t, args[i].m_i32);
            else
              v = Value::NewInt(t, args[i].m_i64);
          } else if (t == tk_char) {
            v = Value::NewInt(t, args[i].m_i32);
          } else if (t == tk_enum) {
            v = Value::NewInt(t, args[i].m_i32);
          } else if (t == tk_codefragment) {
            auto cf = reinterpret_cast<CodeFragment *>(args[i].AsAddr());
            CodeFragment::AddRef(cf);
            v = Value::NewPtr(t, cf);
          } else if (t == tk_void) {
            v = Value::New(Type::Void());
          } else {
            if (t.StorageSize() == 4)
              v = Value::NewRaw(t, &args[i].m_i32, 4);
            else if (t.StorageSize() == 8)
              v = Value::NewRaw(t, &args[i].m_i64, 8);
            else
              verify(false);
            // FIXME: tuple values will fail the above verify.
            // FIXME: large sets will fail
          }
        }
        jos.push_back(v);
      }

      // Ask Derivation to perform the binding.
      d = d->BindParameters(jos);

      BC::Union rv;
      rv.SetAddr(d);
      return rv;
    }
  };

  static GenericBindOp g_GenericBindOp;

  class GenericBind: public NativeOperator {
  public:
    GenericBind() : NativeOperator("GenericBind") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      Type rt = an->m_type;

      // Build CallBuiltin node.
      CallBuiltin *cbn = new CallBuiltin(an->m_location, rt, &g_GenericBindOp);
      cbn->AppendArg(an->m_receiver);
      for (size_t i = 0; i < an->m_formalTypes.size(); i++)
        cbn->AppendArg(an->m_arguments[i]);
      return cbn;
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      Type t = an->m_receiverType;
      if (t.FieldCount() < an->m_formalTypes.size())
        return Type::Suppress();
      an->m_receiverType = t.RValue();

      // Compute the type of the resulting generic value.
      // FIXME: handle re-ordering of remaining unbound parameters.
      vector<Type> ts;
      for (size_t i = an->m_formalTypes.size(); i < t.FieldCount(); i++)
        ts.push_back(t.TypeOf(i));
      Type rt = Type::Generic(Type::Tuple(ts));

      for (size_t i = 0; i < an->m_formalTypes.size(); i++) {
        Type u = t.TypeOf(i).Const();
        Type v = an->m_arguments[i]->m_type.DropQualifiers();
        if (v != tk_pseudo && v.IsSubtypeOf(u.DropQualifiers()) == NO)
          return Type::Suppress();

        // If the generic parameter is a conformant array, force the formal
        // type to a const reference.
        if (u == tk_array && !u.IndexType().IsKnown())
          u = u.LValueRef().Const();

        an->m_formalTypes[i] = u;
      }

      return rt;
    }

    static GenericBind s_macro;
  };

  GenericBind GenericBind::s_macro;

  static bool ValidateGenericArgTypes(Apply *an, bool allowType) {
    if (an->m_formalTypes.size() != 1)
      return false;

    Type t = an->m_arguments[0]->m_type.DropQualifiers();
    if (t != tk_generic) {
      if (!allowType || t != Type::JType())
        return false;
    }

    an->m_formalTypes[0] = t;
    return true;
  }

  static Node *GenCompare(Apply *an, Binary::Opcode op) {
    Type t = an->m_formalTypes[0];

    if (t == tk_generic)
      return new Binary(an->m_location, Type::Bool(), op,
                        an->m_receiver, an->m_arguments[0]);

    // We must cast the receiver to a type and retry the comparison.
    WellKnownString wks = op == Binary::op_seteq ? wks_op_eq : wks_op_ne;
    Node *e = new Initializer(an->m_location, Type::JType(), an->m_receiver,
                              false);
    Node *f = new Member(an->m_location, e, wks, true);
    return new Apply(f, e, an->m_arguments[0]);
  }

  class GenericEq: public NativeOperator {
  public:
    GenericEq() : NativeOperator("GenericEq") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      return GenCompare(an, Binary::op_seteq);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = an->m_receiverType.RValue();
      return ValidateGenericArgTypes(an, true) ? Type::Bool()
                                               : Type::Suppress();
    }

    static GenericEq s_macro;
  };

  GenericEq GenericEq::s_macro;

  class GenericNe: public NativeOperator {
  public:
    GenericNe() : NativeOperator("GenericNe") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      return GenCompare(an, Binary::op_setne);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = an->m_receiverType.RValue();
      return ValidateGenericArgTypes(an, true) ? Type::Bool()
                                               : Type::Suppress();
    }

    static GenericNe s_macro;
  };

  GenericNe GenericNe::s_macro;

  class GenericAssign: public NativeOperator {
  public:
    GenericAssign() : NativeOperator("GenericAssign") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      // FIXME: in non-void context, return the lvalue of receiver.
      return new Store(an->m_location, an->m_receiver->Deref(),
                       an->m_arguments[0]);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_receiverType == ct_const || an->m_receiverType == rt_rvalue)
        return Type::Suppress();
      if (ValidateGenericArgTypes(an, false))
        return an->m_receiverType.LValueRef();
      return Type::Suppress();
    }

    static GenericAssign s_macro;
  };

  GenericAssign GenericAssign::s_macro;
}

Node *Member::HandleGenerics(Context &ctx) {
  // Go and get the member name.
  auto in = safe_cast<Ident *>(m_member);
  String *name = in->m_identifier;

  if (name == wkhs_op_generic) {
    m_macro = &GenericBind::s_macro;
  } else if (name == wks_op_eq) {
    m_macro = &GenericEq::s_macro;
  } else if (name == wks_op_ne) {
    m_macro = &GenericNe::s_macro;
  } else if (name == wks_op_assign) {
    m_macro = &GenericAssign::s_macro;
  } else if (name == wks_op_apply) {
    goto cast_to_class;
  } else if (name == wks_op_const) {
    goto cast_to_class;
  } else if (name == wks_op_mutable) {
    goto cast_to_class;
  } else if (name == wkhs_op_ref) {
    goto cast_to_type;
  } else if (name == wkhs_op_r_ref) {
    goto cast_to_type;
  } else if (name == wkhs_op_ptr) {
    goto cast_to_type;
  } else if (name == wkhs_op_set) {
    goto cast_to_type;
  } else if (name == wks_op_index) {
    goto cast_to_type;
  } else if (!m_operator) {
    goto cast_to_class;
  } else {
    m_reason = r_try_operator;
  }

  return this;

cast_to_class: {
    // Implicitly cast the generic value to a class object, which will fail if
    // any unbound generic parameters lacking defaults exist.  The generic value
    // is an expression that must now be evaluated.
    {
      Context::PushVoid pv(ctx, false);
      m_object = m_object->ResolveFully(ctx);
    }
    Value *v = m_object->Evaluate();
    Derivation *d = v->As<Derivation *>();

    switch (d->Kind()) {
      case gk_class:
        // Easy case: the generic value represents a class.
        ReplaceObjectWithEntityRef(d->CastToClassObject());
        break;
      case gk_type:
        // Not so easy case:  it represents a type.  A double cast is needed:
        // first to a type, then to a class object (if the type is a class type,
        // otherwise an error)
        ReplaceObjectWithEntityRef(d->CastToTypeValue());
        break;
      default:
        // Concept values have a different type and thus should never show up
        // here.
        verify(false);
    }

    // Redo with new object.
    m_state = st_initial;
    return this->ResolveFully(ctx);
  }

cast_to_type: {
    // Implicitly cast the generic value to a type value, which will fail if
    // any unbound generic parameters lacking defaults exist.  Unlike with
    // casting to a class object, the generic value is *not* treated as a
    // constant/ expression and is not evaluated right now.
    {
      Context::PushVoid pv(ctx, false);
      m_object = new Initializer(m_object->m_location, Type::JType(), m_object,
                                 false);
    }

    // Redo with new object.
    m_state = st_initial;
    return this->ResolveFully(ctx);
  }
}
