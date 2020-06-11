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

// Member node states related to members on character values.

#include "entity/Const.h"
#include "entity/Namespace.h"
#include "entity/Specialization.h"
#include "node/Binary.h"
#include "node/Ident.h"
#include "node/Literal.h"
#include "node/Member.h"
#include "node/NativeOperator.h"
#include "node/Store.h"
#include "util/Integer.h"
#include "util/String.h"
#include "util/Value.h"

namespace {
  class CharRel: public NativeOperator {
    Binary::Opcode      m_relop;

  public:
    CharRel(const char *name, Binary::Opcode b)
        : NativeOperator(name),
          m_relop(b) { }

    virtual Node *Run(Apply *an, Context &ctx) {
      Node *le = an->m_receiver;
      Node *re = an->m_arguments[0];

      if (le->m_type == tk_pseudo && re->m_type == tk_pseudo) {
        uint32_t a = safe_cast<Literal *>(le)->m_value->AsChar();
        uint32_t b = safe_cast<Literal *>(re)->m_value->AsChar();
        Value *rv = nullptr;
        switch (m_relop) {
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

      return new Binary(an->m_location, Type::Bool(), m_relop, le, re);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      Type rt = an->m_receiverType.DropQualifiers();
      Type t = Type::Bool();
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();

      Type at = an->m_arguments[0]->m_type.DropQualifiers();
      if (an->m_receiverType == Type::PseudoChar()) {
        if (at == Type::PseudoChar())
          t = Type(); // Force immediate expansion
        else if (at != Type::PseudoChar() && at != tk_char)
          return Type::Suppress();
        rt = at;
      } else if (at == Type::PseudoChar()) {
        at = rt;
      } else {
        if (at != tk_char)
          return Type::Suppress();
      }

      rt = Type::Join(rt, at);
      an->m_formalTypes[0] = rt;
      an->m_receiverType = rt;
      return t;
    }
  };

  class CharAssign: public NativeOperator {
  public:
    CharAssign() : NativeOperator("CharAssign") { }

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
      if (u != Type::PseudoChar() && u.IsSubtypeOf(t) == NO)
        return Type::Suppress();
      an->m_formalTypes[0] = t.DropQualifiers();
      return t.LValueRef();
    }

    static CharAssign s_macro;
  };

  CharAssign CharAssign::s_macro;
}

static CharRel EqOp("CharEq", Binary::op_seteq);
static CharRel NeOp("CharNe", Binary::op_setne);
static CharRel LtOp("CharLt", Binary::op_setlt);
static CharRel LeOp("CharLe", Binary::op_setle);
static CharRel GtOp("CharGt", Binary::op_setgt);
static CharRel GeOp("CharGe", Binary::op_setge);

Node *Member::HandleChars(Context &ctx) {
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
    m_macro = &CharAssign::s_macro;
  } else {
    m_reason = r_try_operator;
  }

  return this;
}

static Entity *CharacterSubrange(Derivation *d, bool is_signed) {
  auto v = safe_cast<Value *>(d->m_bindings[0]);
  uint32_t lb = v->AsChar();
  v = safe_cast<Value *>(d->m_bindings[1]);
  uint32_t ub = v->AsChar();
  Type t = Type::CharSubrange(lb, ub);
  return new Const(nullptr, "T", Type::JType(), &t, sizeof(t));
}

namespace BG {
  Entity *CharacterSubrange(Derivation *d) {
    return CharacterSubrange(d, false);
  }
}

static void AddConst(Namespace *ne, const char *name, Type t) {
  Type tt = Type::JType();
  Const *ce = new Const(ne, name, tt, &t, sizeof(t));
  ne->AddBuiltinEntity(ce);
}

static void PopulateCB() {
  Namespace *ne = Namespace::StdNamespace();
  AddConst(ne, "char", Type::Char8());
  AddConst(ne, "char8", Type::Char8());
  AddConst(ne, "char16", Type::Char16());
  AddConst(ne, "char32", Type::Char32());

  // FIXME: mark generic entity so it prohibits additional specializations??
  // FIXME: support codepage parameter.
  vector<Type> ts { Type::Char32(), Type::Char32() };
  String *name = String::Get(wks_CharacterSubrange);
  auto s = new Specialization(ne, gk_type, name, bg_CharacterSubrange, ts);
  ne->AddBuiltinGeneric(s);
}

static PopulateNamespace g_pn(PopulateCB);
