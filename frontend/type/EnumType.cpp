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

// Member node states related to members on enum values.

#include "entity/Const.h"
#include "entity/Namespace.h"
#include "node/Binary.h"
#include "node/Cast.h"
#include "node/Ident.h"
#include "node/Index.h"
#include "node/Literal.h"
#include "node/Load.h"
#include "node/Member.h"
#include "node/NativeOperator.h"
#include "node/Store.h"
#include "util/SHA512.h"
#include "util/String.h"
#include "util/Value.h"
#include <string.h>

extern NativeOperator *MapOpToInteger(Binary::Opcode);
extern NativeOperator *MapOpToFloating(Binary::Opcode);
extern NativeOperator *MapOpToPointer(Binary::Opcode);

namespace {
  class OrdMember: public NativeOperator {
  public:
    OrdMember() : NativeOperator("OrdMember") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      Type t = an->m_receiverType.AsSubrange();
      return new Cast(an->m_location, t, an->m_receiver);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = an->m_receiverType.RValue();
      return an->m_receiverType.AsSubrange();
    }

    static OrdMember s_macro;
  };

  OrdMember OrdMember::s_macro;

  class EnumBinOp: public NativeOperator {
    Binary::Opcode      m_binop;

  public:
    EnumBinOp(const char *name, Binary::Opcode b)
        : NativeOperator(name),
          m_binop(b) { }

    virtual Node *Run(Apply *an, Context &ctx) {
      Type lt = an->m_receiverType.DropQualifiers();

      if (lt == tk_integer || lt == Type::PseudoInteger()) {
        NativeOperator *op = MapOpToInteger(m_binop);
        return op->Run(an, ctx);
      }

      if (lt == tk_float || lt == Type::PseudoFloat()) {
        NativeOperator *op = MapOpToFloating(m_binop);
        return op->Run(an, ctx);
      }

      if (lt == tk_pointer) {
        NativeOperator *op = MapOpToPointer(m_binop);
        return op->Run(an, ctx);
      }

      return new Binary(an->m_location, Type::Bool(), m_binop,
                        an->m_receiver, an->m_arguments[0]);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      Node *le = an->m_receiver;

      if (an->m_formalTypes.size() == 0) {
        if (le->IsMinOrMax()) {
          NativeOperator *op = MapOpToInteger(m_binop);
          return op->ResolveTypes(an, ctx);
        } else if (le->IsNanOrInfinity()) {
          NativeOperator *op = MapOpToFloating(m_binop);
          return op->ResolveTypes(an, ctx);
        }
        return Type::Suppress();
      } else if (an->m_formalTypes.size() == 1) {
        Type at = an->m_arguments[0]->m_type.DropQualifiers();
        if (at == tk_pointer) {
          NativeOperator *op = MapOpToPointer(m_binop);
          if (op)
            return op->ResolveTypes(an, ctx);
          // FIXME: operator / not declared would be a more consistent error.
          return Type::Suppress();
        }

        bool isInt = at == tk_integer || at == Type::PseudoInteger();
        bool isFloat = at == tk_float || at == Type::PseudoFloat();

        if (le->IsMinOrMax() && isInt) {
          NativeOperator *op = MapOpToInteger(m_binop);
          return op->ResolveTypes(an, ctx);
        } else if (le->IsNanOrInfinity() &&
                   (isFloat || at == Type::PseudoInteger() ||
                    m_binop < Binary::op_seteq)) {
          NativeOperator *op = MapOpToFloating(m_binop);
          return op->ResolveTypes(an, ctx);
        }

        // FIXME: operator / not declared would be a more consistent error.
        if (m_binop < Binary::op_seteq)
          return Type::Suppress();

        // FIXME: not clear what to do when one is not a subtype of the other.
        Type rt = an->m_receiverType.DropQualifiers();
        if (rt.IsSubtypeOf(at) == YES)
          rt = at;
        else if (at.IsSubtypeOf(rt) == YES)
          ;
        else if (rt.IsSubtypeOf(at) == MAYBE)
          rt = at;
        else if (at.IsSubtypeOf(rt) == MAYBE)
          ;
        else
          return Type::Suppress();

        an->m_formalTypes[0] = rt;
        an->m_receiverType = rt;
        return Type::Bool();
      } else {
        return Type::Suppress();
      }
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
      Type t = an->m_receiverType;
      if (t == ct_const || t == rt_rvalue)
        return Type::Suppress();
      Type u = an->m_arguments[0]->m_type.DropQualifiers();
      if (u != Type::PseudoEnum() && u != t.DropQualifiers())
        return Type::Suppress(); // FIXME: handle subtyping
      an->m_formalTypes[0] = t.DropQualifiers();
      return t.LValueRef();
    }

    static Assign s_macro;
  };

  Assign Assign::s_macro;

  static std::unordered_map<Type, Const *> g_valueMap;

  class ValueOf: public NativeOperator {
  public:
    ValueOf() : NativeOperator("ValueOf") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      Type t = an->m_receiverType.DropQualifiers();
      Type u = t.ElementType();

      // Build an array that maps enum ordinals to their associated values and
      // wrap it in a Const entity.  Only do this once for a given enum type.
      auto I = g_valueMap.find(t);
      Const *ce;
      if (I != g_valueMap.end()) {
        ce = I->second;
      } else {
        size_t cnt = t.Cardinality();
        Value *v = Value::New(Type::ArrayOf(u).SetIndexType(cnt));
        uint8_t *p = (uint8_t *)v->Address();
        for (size_t i = 0; i < cnt; i++) {
          Value *v2 = t.ValueAt(i);
          memcpy(p, v2->Address(), u.StorageSize());
          p += u.StorageSize();
        }

        std::string name { "enum_values_" };
        SHA512 hash;
        t.AppendToHash(hash);
        name += hash;

        ce = new Const(Namespace::InternalNamespace(), String::Get(name), v);
        g_valueMap[t] = ce;
      }

      Node *e = ce->AsValue(an->m_location);
      e = new Index(an->m_location, e, an->m_receiver);
      if (u.IsSimpleType())
        return new Load(an->m_location, e->Deref());
      else
        return e->Deref();
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 0)
        return Type::Suppress();
      Type t = an->m_receiverType.ElementType();
      an->m_receiverType = an->m_receiverType.RValue();
      return t.IsSimpleType() ? t : t.Const().LValueRef();
    }

    static ValueOf s_macro;
  };

  ValueOf ValueOf::s_macro;
}

// FIXME: the flaws of this architecture is becoming all too clear.  Most of
// these are here only to support special enum literals that implicitly cast
//  to integer or floats.
static EnumBinOp AddOp("EnumAdd", Binary::op_add);
static EnumBinOp SubOp("EnumSub", Binary::op_sub);
static EnumBinOp MulOp("EnumMul", Binary::op_mul);
static EnumBinOp DivOp("EnumDiv", Binary::op_div);
static EnumBinOp RemOp("EnumRem", Binary::op_rem);
static EnumBinOp AndOp("EnumAnd", Binary::op_and);
static EnumBinOp OrOp ("EnumOr",  Binary::op_or);
static EnumBinOp XorOp("EnumXor", Binary::op_xor);
static EnumBinOp NotOp("EnumNot", Binary::op_illegal);
static EnumBinOp EqOp ("EnumEq",  Binary::op_seteq);
static EnumBinOp NeOp ("EnumNe",  Binary::op_setne);
static EnumBinOp LtOp ("EnumLt",  Binary::op_setlt);
static EnumBinOp LeOp ("EnumLe",  Binary::op_setle);
static EnumBinOp GtOp ("EnumGt",  Binary::op_setgt);
static EnumBinOp GeOp ("EnumGe",  Binary::op_setge);

Node *Member::HandleEnums(Context &ctx) {
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
    m_macro = &Assign::s_macro;
  } else if (name == wks_op_count) {
    m_macro = &ValueOf::s_macro;
  } else if (name == wks_ord) {
    Node *e = new Apply(m_location, &OrdMember::s_macro, m_object);
    return e->ResolveType(ctx);
  } else {
    m_reason = r_try_operator;
  }

  return this;
}

static void PopulateCB() {
  //Namespace *ne = Namespace::StdNamespace();
  //Type tt = Type::JType();
}

static PopulateNamespace g_pn(PopulateCB);
