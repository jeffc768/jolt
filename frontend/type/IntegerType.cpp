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

// Member node states related to members on integer values.
// FIXME: handle subranges largers than 8 bytes.

#include "entity/Const.h"
#include "entity/Namespace.h"
#include "entity/Specialization.h"
#include "node/Binary.h"
#include "node/Cast.h"
#include "node/Ident.h"
#include "node/Literal.h"
#include "node/Member.h"
#include "node/NativeOperator.h"
#include "node/Store.h"
#include "node/Unary.h"
#include "util/Integer.h"
#include "util/String.h"
#include "util/Value.h"

extern NativeOperator *MapOpToFloating(Binary::Opcode);

namespace {
  class IntegerMath: public NativeOperator {
    Unary::Opcode       m_unop;
    Binary::Opcode      m_binop;

  public:
    IntegerMath(const char  *name, Unary::Opcode u, Binary::Opcode b)
        : NativeOperator(name),
          m_unop(u),
          m_binop(b) { }

    virtual Node *Run(Apply *an, Context &ctx) {
      Type lt = an->m_receiverType.DropQualifiers();
      Node *le = an->m_receiver;

      if (lt == tk_float || lt == Type::PseudoFloat()) {
        NativeOperator *op = MapOpToFloating(m_binop);
        return op->Run(an, ctx);
      }

      if (an->m_arguments.size() == 0) {
        if (lt == tk_pseudo) {
          Integer *a = safe_cast<Literal *>(le)->m_value->AsInteger();
          Value *rv = nullptr;
          if (m_unop == Unary::op_neg)
            rv = Value::NewPseudo(Type::PseudoInteger(), a->Neg());
          else if (m_unop == Unary::op_not)
            verify(false);
          else
            verify(false);
          return new Literal(an->m_location, rv);
        }

        return new Unary(an->m_location, m_unop, an->m_receiver);
      }

      Node *re = an->m_arguments[0];

      if (le->m_type == tk_pseudo && re->m_type == tk_pseudo &&
          le->Kind() == nk_Literal && re->Kind() == nk_Literal) {
        Integer *a = safe_cast<Literal *>(le)->m_value->AsInteger();
        Integer *b = safe_cast<Literal *>(re)->m_value->AsInteger();
        Value *rv = nullptr;
        switch (m_binop) {
          case Binary::op_add:
            rv = Value::NewPseudo(Type::PseudoInteger(), a->Add(b));
            break;
          case Binary::op_sub:
            rv = Value::NewPseudo(Type::PseudoInteger(), a->Sub(b));
            break;
          case Binary::op_mul:
            rv = Value::NewPseudo(Type::PseudoInteger(), a->Mul(b));
            break;
          case Binary::op_div:
            rv = Value::NewPseudo(Type::PseudoInteger(), a->Div(b));
            break;
          case Binary::op_rem:
            verify(false);
          case Binary::op_and:
            verify(false);
          case Binary::op_or:
            verify(false);
          case Binary::op_xor:
            verify(false);
          case Binary::op_seteq:
            rv = Value::NewBool(a->Eq(b));
            break;
          case Binary::op_setne:
            rv = Value::NewBool(a->Ne(b));
            break;
          case Binary::op_setlt:
            rv = Value::NewBool(a->Lt(b));
            break;
          case Binary::op_setle:
            rv = Value::NewBool(a->Le(b));
            break;
          case Binary::op_setgt:
            rv = Value::NewBool(a->Gt(b));
            break;
          case Binary::op_setge:
            rv = Value::NewBool(a->Ge(b));
            break;
          default:
            verify(false);
        }
        return new Literal(an->m_location, rv);
      }

      return new Binary(an->m_location, m_binop, le, re);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      Type rt = an->m_receiverType.DropQualifiers();
      bool immediate = false;
      if (an->m_formalTypes.size() == 0) {
        if (m_unop == Unary::op_illegal)
          return Type::Suppress();
        if (an->m_receiverType == Type::PseudoInteger())
          immediate = true;
      } else if (an->m_formalTypes.size() == 1) {
        if (m_binop == Binary::op_illegal)
          return Type::Suppress();

        Node *rcvr = an->m_receiver;
        Node *arg = an->m_arguments[0];
        Type at = arg->m_type.DropQualifiers();

        if (at == tk_float || at == Type::PseudoFloat() ||
            arg->IsNanOrInfinity()) {
          if (an->m_receiverType == Type::PseudoInteger()) {
            NativeOperator *op = MapOpToFloating(m_binop);
            if (op)
              return op->ResolveTypes(an, ctx);
            // FIXME: operator & not declared would be a more consistent error.
          }
          return Type::Suppress();
        }

        if (rt == Type::PseudoInteger()) {
          if (at == Type::PseudoInteger()) {
            immediate = true;
          } else if (at == tk_integer) {
            rt = at;
          } else {
            // FIXME: this includes .min/.max, but that can't be handlded
            // here yet.
            return Type::Suppress();
          }
        } else if (rcvr->IsMinOrMax()) {
          if (at == tk_integer) {
            rt = at;
          } else {
            // FIXME: this includes integer literals, but that can't be handlded
            // here yet.
            return Type::Suppress();
          }
        } else if (rt == tk_integer) {
          if (at == Type::PseudoInteger() || arg->IsMinOrMax()) {
            at = rt;
          } else if (at == tk_integer) {
            if (rt.IsSigned() != at.IsSigned())
              return Type::Suppress();
          } else {
            return Type::Suppress();
          }
        } else {
          return Type::Suppress();
        }

        // We need to lower .min/.max now before we lose the types of the
        // receiver and argument.
        if (rcvr->IsMinOrMax()) {
          an->m_receiver = rcvr->LowerSpecialEnums(at);
        } else if (arg->IsMinOrMax()) {
          an->m_arguments[0] = arg->LowerSpecialEnums(rt);
        }

        rt = Type::Join(rt, at);
        an->m_formalTypes[0] = rt;
      } else {
        return Type::Suppress();
      }

      an->m_receiverType = Type::Join(rt, rt);
      if (immediate) {
        return Type();
      } else if (m_binop >= Binary::op_seteq) {
        return Type::Bool();
      } else {
        return rt;
      }
    }
  };

  class IntegerAssign: public NativeOperator {
  public:
    IntegerAssign() : NativeOperator("IntegerAssign") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      // FIXME: in non-void context, return the lvalue of receiver.
      return new Store(an->m_location, an->m_receiver->Deref(),
                       an->m_arguments[0]);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();
      Type t = an->m_receiverType;
      if (t == ct_const || t == rt_rvalue)
        return Type::Suppress();

      Node *arg = an->m_arguments[0];
      Type u = arg->m_type.DropQualifiers();
      if (u.IsSubtypeOf(t) == NO)
        return Type::Suppress();

      // We need to lower .min/.max now before we lose the types of the
      // receiver and argument.
      t = t.DropQualifiers();
      if (arg->IsMinOrMax())
        an->m_arguments[0] = arg->LowerSpecialEnums(t);

      an->m_formalTypes[0] = t;
      return t.LValueRef();
    }

    static IntegerAssign s_macro;
  };

  IntegerAssign IntegerAssign::s_macro;
}

static IntegerMath AddOp("IntegerAdd", Unary::op_illegal,  Binary::op_add);
static IntegerMath SubOp("IntegerSub", Unary::op_neg,      Binary::op_sub);
static IntegerMath MulOp("IntegerMul", Unary::op_illegal,  Binary::op_mul);
static IntegerMath DivOp("IntegerDiv", Unary::op_illegal,  Binary::op_div);
static IntegerMath RemOp("IntegerRem", Unary::op_illegal,  Binary::op_rem);
static IntegerMath AndOp("IntegerAnd", Unary::op_illegal,  Binary::op_and);
static IntegerMath OrOp ("IntegerOr",  Unary::op_illegal,  Binary::op_or);
static IntegerMath XorOp("IntegerXor", Unary::op_illegal,  Binary::op_xor);
static IntegerMath NotOp("IntegerNot", Unary::op_not,      Binary::op_illegal);
static IntegerMath EqOp ("IntegerEq",  Unary::op_illegal,  Binary::op_seteq);
static IntegerMath NeOp ("IntegerNe",  Unary::op_illegal,  Binary::op_setne);
static IntegerMath LtOp ("IntegerLt",  Unary::op_illegal,  Binary::op_setlt);
static IntegerMath LeOp ("IntegerLe",  Unary::op_illegal,  Binary::op_setle);
static IntegerMath GtOp ("IntegerGt",  Unary::op_illegal,  Binary::op_setgt);
static IntegerMath GeOp ("IntegerGe",  Unary::op_illegal,  Binary::op_setge);

Node *Member::HandleIntegers(Context &ctx) {
  // Go and get the member name.
  auto in = safe_cast<Ident *>(m_member);
  String *name = in->m_identifier;

  if (name == wks_op_add) {
    m_macro = &AddOp;
  } else if (name == wks_op_sub) {
    m_macro = &SubOp;
  } else if (name == wks_op_mul) {
    m_macro = &MulOp;
  } else if (name == wks_op_div) {
    m_macro = &DivOp;
  } else if (name == wks_op_rem) {
    m_macro = &RemOp;
  } else if (name == wks_op_bitand) {
    m_macro = &AndOp;
  } else if (name == wks_op_bitor) {
    m_macro = &OrOp;
  } else if (name == wks_op_bitxor) {
    m_macro = &XorOp;
  } else if (name == wks_op_bitnot) {
    m_macro = &NotOp;
  } else if (name == wks_op_eq) {
    m_macro = &EqOp;
  } else if (name == wks_op_ne) {
    m_macro = &NeOp;
  } else if (name == wks_op_lt) {
    m_macro = &LtOp;
  } else if (name == wks_op_le) {
    m_macro = &LeOp;
  } else if (name == wks_op_gt) {
    m_macro = &GtOp;
  } else if (name == wks_op_ge) {
    m_macro = &GeOp;
  } else if (name == wks_op_assign) {
    m_macro = &IntegerAssign::s_macro;
  } else {
    m_reason = r_try_operator;
  }

  return this;
}

NativeOperator *MapOpToInteger(Binary::Opcode op) {
  switch (op) {
    case Binary::op_add:      return &AddOp;
    case Binary::op_sub:      return &SubOp;
    case Binary::op_mul:      return &MulOp;
    case Binary::op_div:      return &DivOp;
    case Binary::op_rem:      return &RemOp;
    case Binary::op_and:      return &AndOp;
    case Binary::op_or:       return &OrOp;
    case Binary::op_xor:      return &XorOp;
    case Binary::op_illegal:  return &NotOp;  // hack, no binop equivalent
    case Binary::op_seteq:    return &EqOp;
    case Binary::op_setne:    return &NeOp;
    case Binary::op_setlt:    return &LtOp;
    case Binary::op_setle:    return &LeOp;
    case Binary::op_setgt:    return &GtOp;
    case Binary::op_setge:    return &GeOp;
    default:                  return nullptr;
  }
}

static Entity *IntegerSubrange(Derivation *d, bool is_signed) {
  auto v = safe_cast<Value *>(d->m_bindings[0]);
  Integer *lb = v->AsInteger();
  v = safe_cast<Value *>(d->m_bindings[1]);
  Integer *ub = v->AsInteger();
  Type t = Type::IntegerSubrange(lb, ub, is_signed);
  return new Const(nullptr, "T", Type::JType(), &t, sizeof(t));
}

namespace BG {
  Entity *SignedSubrange(Derivation *d) {
    return IntegerSubrange(d, true);
  }

  Entity *UnsignedSubrange(Derivation *d) {
    return IntegerSubrange(d, false);
  }
}

static void AddConst(Namespace *ne, const char *name, Type t) {
  Type tt = Type::JType();
  Const *ce = new Const(ne, name, tt, &t, sizeof(t));
  ne->AddBuiltinEntity(ce);
}

static void PopulateCB() {
  Namespace *ne = Namespace::StdNamespace();
  AddConst(ne, "sbyte", Type::SByte());
  AddConst(ne, "short", Type::Short());
  AddConst(ne, "int", Type::Int());
  AddConst(ne, "long", Type::Long());
  AddConst(ne, "byte", Type::Byte());
  AddConst(ne, "ushort", Type::UShort());
  AddConst(ne, "uint", Type::UInt());
  AddConst(ne, "ulong", Type::ULong());
  AddConst(ne, "intptr_t", Type::IntPtrT());
  AddConst(ne, "uintptr_t", Type::UIntPtrT());
  AddConst(ne, "size_t", Type::SizeT());
  AddConst(ne, "ptrdiff_t", Type::PtrDiffT());

  // FIXME: mark generic entity so it prohibits additional specializations??
  // FIXME: arbitrary precision integers when available
  vector<Type> ts { Type::Long(), Type::Long() };

  String *name = String::Get(wks_SignedSubrange);

  auto s = new Specialization(ne, gk_type, name, bg_SignedSubrange, ts);
  ne->AddBuiltinGeneric(s);

  name = String::Get(wks_UnsignedSubrange);
  s = new Specialization(ne, gk_type, name, bg_UnsignedSubrange, ts);
  ne->AddBuiltinGeneric(s);
}

static PopulateNamespace g_pn(PopulateCB);
