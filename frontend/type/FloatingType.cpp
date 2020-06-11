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

// Member node states related to members on floating-point values.

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
#include "parser/Token.h"
#include "util/Integer.h"
#include "util/String.h"
#include "util/Value.h"

namespace {
  class FloatingMath: public NativeOperator {
    Unary::Opcode       m_unop;
    Binary::Opcode      m_binop;

  public:
    FloatingMath(const char  *name, Unary::Opcode u, Binary::Opcode b)
        : NativeOperator(name),
          m_unop(u),
          m_binop(b) { }

    virtual Node *Run(Apply *an, Context &ctx) {
      Type lt = an->m_receiverType.DropQualifiers();
      Node *le = an->m_receiver;

      if (an->m_arguments.size() == 0) {
        if (lt == tk_pseudo) {
          double a = safe_cast<Literal *>(le)->m_value->AsDouble();
          Value *rv = nullptr;
          if (m_unop == Unary::op_neg)
            rv = Value::NewPseudo(Type::PseudoFloat(), new Float(-a));
          else
            verify(false);
          return new Literal(an->m_location, rv);
        }

        lt = Type::Join(lt, lt);
        return new Unary(an->m_location, lt, m_unop, an->m_receiver);
      }

      Node *re = an->m_arguments[0];

      if (le->m_type == tk_pseudo && re->m_type == tk_pseudo &&
          le->Kind() == nk_Literal && re->Kind() == nk_Literal) {
        double a = safe_cast<Literal *>(le)->m_value->AsDouble();
        double b = safe_cast<Literal *>(re)->m_value->AsDouble();
        Value *rv = nullptr;
        switch (m_binop) {
          case Binary::op_add:
            rv = Value::NewPseudo(Type::PseudoFloat(), new Float(a + b));
            break;
          case Binary::op_sub:
            rv = Value::NewPseudo(Type::PseudoFloat(), new Float(a - b));
            break;
          case Binary::op_mul:
            rv = Value::NewPseudo(Type::PseudoFloat(), new Float(a * b));
            break;
          case Binary::op_div:
            rv = Value::NewPseudo(Type::PseudoFloat(), new Float(a / b));
            break;
          case Binary::op_seteq:
            rv = Value::NewBool(a == b);
            break;
          case Binary::op_setne:
            rv = Value::NewBool(a != b);
            break;
          case Binary::op_setlt:
            rv = Value::NewBool(a < b);
            break;
          case Binary::op_setle:
            rv = Value::NewBool(a <= b);
            break;
          case Binary::op_setgt:
            rv = Value::NewBool(a > b);
            break;
          case Binary::op_setge:
            rv = Value::NewBool(a >= b);
            break;
          default:
            verify(false);
        }
        return new Literal(an->m_location, rv);
      }

      Type t = m_binop >= Binary::op_seteq ? Type::Bool() : lt;
      return new Binary(an->m_location, t, m_binop, le, re);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      Type rt = an->m_receiverType.DropQualifiers();
      bool immediate = false;
      if (an->m_formalTypes.size() == 0) {
        if (m_unop == Unary::op_illegal)
          return Type::Suppress();
        if (rt == Type::PseudoFloat()) {
          immediate = true;
        } else if (an->m_receiver->IsNanOrInfinity()) {
          immediate = true;
          rt = rt == Type::PseudoEnum() ? Type::PseudoFloat() : Type::Float();
        }
      } else if (an->m_formalTypes.size() == 1) {
        if (m_binop == Binary::op_illegal)
          return Type::Suppress();

        Node *arg = an->m_arguments[0];
        Type at = an->m_arguments[0]->m_type.DropQualifiers();

        if (rt == tk_float) {
          if (at == tk_float) {
            rt = Type::Join(rt, at);
          } else if (at != Type::PseudoInteger() && at != Type::PseudoFloat()
                     && !arg->IsNanOrInfinity()) {
            return Type::Suppress();
          }
        } else if (at == tk_float) {
          rt = at;
        } else if (at != Type::PseudoInteger() && at != Type::PseudoFloat()
                   && !arg->IsNanOrInfinity()) {
          return Type::Suppress();
        } else {
          immediate = true;
          if (at == tk_enum || rt == tk_enum)
            rt = Type::Float(); // FIXME:
          else
            rt = Type::PseudoFloat();
        }

        an->m_formalTypes[0] = rt;
      } else {
        return Type::Suppress();
      }

      an->m_receiverType = rt;
      if (immediate) {
        return Type();
      } else if (m_binop >= Binary::op_seteq) {
        return Type::Bool();
      } else {
        return rt;
      }
    }
  };

  class FloatingAssign: public NativeOperator {
  public:
    FloatingAssign() : NativeOperator("FloatingAssign") { }

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
      Type u = an->m_arguments[0]->m_type.DropQualifiers();
      if (u.IsSubtypeOf(t) == NO)
        return Type::Suppress();
      an->m_formalTypes[0] = t.DropQualifiers();
      return t.LValueRef();
    }

    static FloatingAssign s_macro;
  };

  FloatingAssign FloatingAssign::s_macro;
}

static FloatingMath AddOp("FloatingAdd", Unary::op_illegal,  Binary::op_add);
static FloatingMath SubOp("FloatingSub", Unary::op_neg,      Binary::op_sub);
static FloatingMath MulOp("FloatingMul", Unary::op_illegal,  Binary::op_mul);
static FloatingMath DivOp("FloatingDiv", Unary::op_illegal,  Binary::op_div);
static FloatingMath EqOp ("FloatingEq",  Unary::op_illegal,  Binary::op_seteq);
static FloatingMath NeOp ("FloatingNe",  Unary::op_illegal,  Binary::op_setne);
static FloatingMath LtOp ("FloatingLt",  Unary::op_illegal,  Binary::op_setlt);
static FloatingMath LeOp ("FloatingLe",  Unary::op_illegal,  Binary::op_setle);
static FloatingMath GtOp ("FloatingGt",  Unary::op_illegal,  Binary::op_setgt);
static FloatingMath GeOp ("FloatingGe",  Unary::op_illegal,  Binary::op_setge);

Node *Member::HandleFloats(Context &ctx) {
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
    m_macro = &FloatingAssign::s_macro;
  } else {
    m_reason = r_try_operator;
  }

  return this;
}

NativeOperator *MapOpToFloating(Binary::Opcode op) {
  switch (op) {
    case Binary::op_add:      return &AddOp;
    case Binary::op_sub:      return &SubOp;
    case Binary::op_mul:      return &MulOp;
    case Binary::op_div:      return &DivOp;
    case Binary::op_seteq:    return &EqOp;
    case Binary::op_setne:    return &NeOp;
    case Binary::op_setlt:    return &LtOp;
    case Binary::op_setle:    return &LeOp;
    case Binary::op_setgt:    return &GtOp;
    case Binary::op_setge:    return &GeOp;
    default:                  return nullptr;
  }
}

static void AddConst(Namespace *ne, const char *name, Type t) {
  Type tt = Type::JType();
  Const *ce = new Const(ne, name, tt, &t, sizeof(t));
  ne->AddBuiltinEntity(ce);
}

static void PopulateCB() {
  Namespace *ne = Namespace::StdNamespace();
  AddConst(ne, "float", Type::Float());
  AddConst(ne, "double", Type::Double());
}

static PopulateNamespace g_pn(PopulateCB);
