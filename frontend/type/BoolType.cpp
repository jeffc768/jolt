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

// Member node states related to members on bool values.

#include "entity/Const.h"
#include "entity/Namespace.h"
#include "node/Binary.h"
#include "node/Ident.h"
#include "node/Literal.h"
#include "node/Member.h"
#include "node/NativeOperator.h"
#include "node/Store.h"
#include "util/String.h"
#include "util/Value.h"

namespace {
  class NotOp: public NativeOperator {
  public:
    NotOp() : NativeOperator("NotOp") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      Literal *ln = new Literal(an->m_location, Value::NewBool(true));
      return new Binary(an->m_location, Type::Bool(), Binary::op_xor,
                        an->m_receiver, ln);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = Type::Bool();
      return an->m_formalTypes.size() == 0 ? Type::Bool() : Type::Suppress();
    }

    static NotOp s_macro;
  };

  NotOp NotOp::s_macro;

  class BoolBinOp: public NativeOperator {
    Binary::Opcode      m_binop;

  public:
    BoolBinOp(const char *name, Binary::Opcode b)
        : NativeOperator(name),
          m_binop(b) { }

    virtual Node *Run(Apply *an, Context &ctx) {
      return new Binary(an->m_location, Type::Bool(), m_binop,
                        an->m_receiver, an->m_arguments[0]);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();
      Type t = Type::Bool();
      if (an->m_arguments[0]->m_type.DropQualifiers().IsSubtypeOf(t) == NO)
        return Type::Suppress();

      an->m_receiverType = t;
      an->m_formalTypes[0] = t;
      return t;
    }
  };

  class Assign: public NativeOperator {
  public:
    Assign() : NativeOperator("Assign") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      // FIXME: in non-void context, return the lvalue of receiver.
      return new Store(an->m_location, an->m_receiver->Deref(),
                       an->m_arguments[0]);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();
      if (an->m_receiverType == ct_const || an->m_receiverType == rt_rvalue)
        return Type::Suppress();
      Type t = Type::Bool();
      if (an->m_arguments[0]->m_type.DropQualifiers().IsSubtypeOf(t) == NO)
        return Type::Suppress();
      an->m_formalTypes[0] = t;
      return t.LValueRef();
    }

    static Assign s_macro;
  };

  Assign Assign::s_macro;
}

static BoolBinOp AndOp("BoolAnd", Binary::op_and);
static BoolBinOp OrOp("BoolOr", Binary::op_or);
static BoolBinOp XorOp("BoolXor", Binary::op_xor);
static BoolBinOp EqOp("BoolEq", Binary::op_seteq);
static BoolBinOp NeOp("BoolNe", Binary::op_setne);
static BoolBinOp LtOp("BoolLt", Binary::op_setlt);
static BoolBinOp LeOp("BoolLe", Binary::op_setle);
static BoolBinOp GtOp("BoolGt", Binary::op_setgt);
static BoolBinOp GeOp("BoolGe", Binary::op_setge);

Node *Member::HandleBools(Context &ctx) {
  // Go and get the member name.
  auto in = safe_cast<Ident *>(m_member);
  String *name = in->m_identifier;

  if (name == wks_op_bitand) {
    m_macro = &AndOp;
  } else if (name == wks_op_bitor) {
    m_macro = &OrOp;
  } else if (name == wks_op_bitxor) {
    m_macro = &XorOp;
  } else if (name == wks_op_not) {
    m_macro = &NotOp::s_macro;
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
    m_macro = &Assign::s_macro;
  } else {
    m_reason = r_try_operator;
  }

  return this;
}

static void PopulateCB() {
  Namespace *ne = Namespace::StdNamespace();
  Type tt = Type::JType();
  Type t = Type::Bool();
  Const *ce = new Const(ne, "bool", tt, &t, sizeof(t));
  ne->AddBuiltinEntity(ce);

  Type tb = Type::Bool();
  bool b = true;
  verify(sizeof(bool) == 1);
  ce = new Const(ne, "true", tb, &b, sizeof(b));
  ne->AddBuiltinEntity(ce);

  b = false;
  ce = new Const(ne, "false", tb, &b, sizeof(b));
  ne->AddBuiltinEntity(ce);
}

static PopulateNamespace g_pn(PopulateCB);
