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

// Member node states related to members on void values.

#include "entity/Const.h"
#include "entity/Namespace.h"
#include "node/Binary.h"
#include "node/Cast.h"
#include "node/Node.h"
#include "node/Ident.h"
#include "node/Literal.h"
#include "node/Member.h"
#include "node/NativeOperator.h"
#include "node/Store.h"
#include "node/Sequence.h"
#include "node/Unary.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"

namespace {
  class VoidBinOp: public NativeOperator {
    Binary::Opcode      m_binop;

  public:
    VoidBinOp(const char *name, Binary::Opcode b)
        : NativeOperator(name),
          m_binop(b) { }

    virtual Node *Run(Apply *an, Context &ctx) {
      bool result = false;
      switch (m_binop) {
        case Binary::op_seteq:
        case Binary::op_setge:
        case Binary::op_setle:
          result = true;
          break;
        case Binary::op_setne:
        case Binary::op_setgt:
        case Binary::op_setlt:
          result = false;
          break;
        default:
          verify(false);
      }

      // Though the result is a constant, the arguments must still be
      // evaluated for potential side-effects.
      auto rv = new Literal(an->m_location, Value::NewBool(result));
      Node *e1 = an->m_receiver->LowerPseudos(Type());
      Node *e2 = an->m_arguments[0]->LowerPseudos(Type());
      auto effects = new Sequence(an->m_location, e1, e2, false);
      return new Sequence(an->m_location, effects, rv, false);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();
      Type rt = an->m_arguments[0]->m_type.DropQualifiers();
      if (rt != Type::Void())
        return Type::Suppress();
      an->m_formalTypes[0] = rt;
      an->m_receiverType = rt;
      return Type::Bool();
    }
  };

  class Assign: public NativeOperator {
  public:
    Assign() : NativeOperator("Assign") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      // FIXME: in non-void context, return the lvalue of receiver.
      // FIXME: uh... nothing to store?  But the argument must still be
      // evaluated for side-effects.
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
      if (u != Type::Void())
        return Type::Suppress();
      an->m_formalTypes[0] = u;
      return Type::Void().LValueRef();
    }

    static Assign s_macro;
  };

  Assign Assign::s_macro;

  class Construct: public NativeOperator {
  public:
    Construct() : NativeOperator("Construct") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      auto ln = new Literal(an->m_location, Value::New(Type::Void()));
      if (an->m_formalTypes.size() == 0)
        return ln;

      // though the value is void regardless, the argument must still be
      // evaluated for side-effects.  Create a Sequence node.
      Node *e = an->m_arguments[0]->LowerPseudos(Type());
      return new Sequence(an->m_location, e, ln, false);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() == 0)
        return Type::Void();
      if (an->m_formalTypes.size() > 1)
        return Type::Suppress();
      an->m_formalTypes[0] = an->m_arguments[0]->m_type;
      return Type::Void();
    }

    static Construct s_macro;
  };

  Construct Construct::s_macro;
}

static VoidBinOp EqOp("VoidEq", Binary::op_seteq);
static VoidBinOp NeOp("VoidNe", Binary::op_setne);
static VoidBinOp LtOp("VoidLt", Binary::op_setlt);
static VoidBinOp LeOp("VoidLe", Binary::op_setle);
static VoidBinOp GtOp("VoidGt", Binary::op_setgt);
static VoidBinOp GeOp("VoidGe", Binary::op_setge);

namespace TC {
  NativeOperator *Void = &Construct::s_macro;
}

Node *Member::HandleVoids(Context &ctx) {
  // Go and get the member name.
  auto in = safe_cast<Ident *>(m_member);
  String *name = in->m_identifier;

  if (name == wks_op_eq) {
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
  Type t = Type::Void();
  Const *ce = new Const(ne, "void", Type::JType(), &t, sizeof(t));
  ne->AddBuiltinEntity(ce);
}

static PopulateNamespace g_pn(PopulateCB);
