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

// Member node states related to members on tuple values.

#include "entity/Const.h"
#include "entity/Method.h"
#include "entity/Namespace.h"
#include "node/Binary.h"
#include "node/Cast.h"
#include "node/Construct.h"
#include "node/FieldAddr.h"
#include "node/Ident.h"
#include "node/Index.h"
#include "node/Initializer.h"
#include "node/List.h"
#include "node/Literal.h"
#include "node/Load.h"
#include "node/Member.h"
#include "node/NativeOperator.h"
#include "node/Sequence.h"
#include "node/Store.h"
#include "node/TypeHolder.h"
#include "util/Message.h"
#include "util/SHA512.h"
#include "util/String.h"
#include "util/Value.h"

static Node *BuildSequence(Location sl, Type t, vector<Node *> &list) {
  if (list.empty())
    return new Literal(sl, Value::New(t));

  Node *e = list.back();
  list.pop_back();
  while (!list.empty()) {
    e = new Sequence(list.back(), e, false);
    list.pop_back();
  }

  return e;
}

namespace {
  class TupleRelOp: public NativeOperator {
    Binary::Opcode      m_binop;

  public:
    TupleRelOp(const char *name, Binary::Opcode b)
        : NativeOperator(name),
          m_binop(b) { }

    virtual Node *Run(Apply *an, Context &ctx) {
      return new Binary(an->m_location, m_binop, an->m_receiver,
                        an->m_arguments[0]);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      verify(false); // FIXME: implement
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();
      Type rt = an->m_arguments[0]->m_type.DropQualifiers();
      if (rt != an->m_receiverType.DropQualifiers())
        return Type::Suppress(); // FIXME: handle subtyping
      an->m_formalTypes[0] = rt;
      an->m_receiverType = an->m_receiverType.RValue();
      return Type::Bool();
    }
  };

  TupleRelOp TupleEqOp("TupleEq", Binary::op_seteq);
  TupleRelOp TupleNeOp("TupleNe", Binary::op_setne);
  TupleRelOp TupleLtOp("TupleLt", Binary::op_setlt);
  TupleRelOp TupleLeOp("TupleLe", Binary::op_setle);
  TupleRelOp TupleGtOp("TupleGt", Binary::op_setgt);
  TupleRelOp TupleGeOp("TupleGe", Binary::op_setge);

  class TupleConcat: public NativeOperator {
  public:
    TupleConcat() : NativeOperator("TupleConcat") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      // FIXME: in non-void context, return the lvalue of receiver.
      verify(false);
      return nullptr;
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      verify(false); // FIXME: implement
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();
      Type u = an->m_arguments[0]->m_type.DropQualifiers();
      if (u != tk_tuple && u != Type::PseudoList())
        return Type::Suppress();
      verify(false); // FIXME
      return Type();
    }

    static TupleConcat s_macro;
  };

  TupleConcat TupleConcat::s_macro;

  class TupleAssignList: public NativeOperator {
  public:
    TupleAssignList() : NativeOperator("TupleAssignList") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      // FIXME: in non-void context, return the lvalue of receiver.
      Type tt = an->m_type;
      Location sl = an->m_location;
      size_t count = tt.FieldCount();
      vector<Node *> list;

      for (size_t i = 0; i < count; i++) {
        Node *dst = new FieldAddr(sl, an->m_receiver->Deref(), i);
        Node *src = an->m_arguments[i];
        Node *f = new Member(sl, dst->Deref(), wks_op_assign, true);
        f = new Apply(f, dst->Deref(), src);
        list.push_back(f);
      }

      return BuildSequence(an->m_location, Type::Void(), list);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      // Should never get here, because Apply learns of us only after
      // ResolveTypes is called on TupleAssign.
      verify(false);
      return Type::Suppress();
    }

    static TupleAssignList s_macro;
  };

  TupleAssignList TupleAssignList::s_macro;

  class TupleAssign: public NativeOperator {
  public:
    TupleAssign() : NativeOperator("TupleAssign") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      // FIXME: in non-void context, return the lvalue of receiver.
      Type tt = an->m_type;
      Location sl = an->m_location;
      size_t count = tt.FieldCount();
      vector<Node *> list;

      for (size_t i = 0; i < count; i++) {
        Node *dst = new FieldAddr(sl, an->m_receiver->Deref(), i);
        Node *src = new FieldAddr(sl, an->m_arguments[0]->Deref(), i);
        Node *f = new Member(sl, dst->Deref(), wks_op_assign, true);
        f = new Apply(f, dst->Deref(), src->Deref());
        list.push_back(f);
      }

      return BuildSequence(an->m_location, Type::Void(), list);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();
      Type t = an->m_receiverType;
      if (t == ct_const || t == rt_rvalue)
        return Type::Suppress();
      Type u = an->m_arguments[0]->m_type.DropQualifiers();

      // FIXME: how to handle a list being the target, e.g. (a,b) = tuple
      verify(t != Type::PseudoList());

      if (u == Type::PseudoList()) {
        // FIXME: how to handle indirect lists, e.g. cond ? (list1) : (list2)
        // Probably by casting each one to the target type.
        verify(an->m_arguments[0]->Kind() == nk_List);
        if (!safe_cast<List *>(an->m_arguments[0])->IsSubtypeOf(t))
          return Type::Suppress();

        // We have to get rid of the List node now, otherwise Apply will
        // attempt to create a list-valued temp var (not a good thing).  Turn
        // its list of values into our arguments, and switch to a new native
        // operator that knows how to handle that.
        auto ln = safe_cast<List *>(an->m_arguments[0]);
        size_t cnt = ln->m_values.size();
        an->ResetNativeOperator(&TupleAssignList::s_macro, cnt);
        for (size_t i = 0; i < cnt; i++) {
          Node *e = ln->m_values[i];
          an->m_arguments[i] = e;
          an->m_formalTypes[i] = t.TypeOf(i).PerfectForward(e->m_type);
          an->m_passThrough[i] = true;
        }
        return t.LValueRef();
      } else {
        if (u.IsSubtypeOf(t) == NO)
          return Type::Suppress();
      }

      if (an->m_arguments[0]->m_type == rt_lvalueref)
        an->m_formalTypes[0] = u.Const().LValueRef();
      else
        an->m_formalTypes[0] = u.RValueRef();
      return t.LValueRef();
    }

    static TupleAssign s_macro;
  };

  TupleAssign TupleAssign::s_macro;

  class TupleCount: public NativeOperator {
    static void FlattenList(vector<Node *> &flat, vector<Node *> &list) {
      for (Node *e : list)
        if (auto ln = dyn_cast<List *>(e))
          FlattenList(flat, ln->m_values);
        else
          flat.push_back(e);
    }

  public:
    TupleCount() : NativeOperator("TupleCount") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      // Note: the operand must still be evaluated for side-effects.
      if (an->m_receiverType == tk_tuple) {
        auto v = Value::NewInt(Type::SizeT(), an->m_receiverType.FieldCount());
        auto ln = new Literal(an->m_location, v);
        return new Sequence(an->m_receiver, ln, false);
      }

      // FIXME: we can do better than this.  Replace all List nodes with the
      // field count?  Still must evaluated stuff for side-effects.
      auto ln = safe_cast<List *>(an->m_receiver);
      vector<Node *> list;
      FlattenList(list, ln->m_values);
      auto v = Value::NewInt(Type::SizeT(), ln->m_values.size());
      list.push_back(new Literal(an->m_location, v));
      return BuildSequence(an->m_location, Type::SizeT(), list);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 0)
        return Type::Suppress();
      return Type::SizeT();
    }

    static TupleCount s_macro;
  };

  TupleCount TupleCount::s_macro;

  class TupleSubscript: public NativeOperator {
  public:
    TupleSubscript() : NativeOperator("TupleSubscript") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      auto ln = safe_cast<Literal *>(an->m_arguments[0]);
      size_t ord = ln->m_value->As<size_t>();
      Node *rcvr = an->m_receiver->Deref();
      Node *addr = new FieldAddr(an->m_location, rcvr, ord);
      return addr->Deref();
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();
      Type t = Type::SizeT();
      Type u = an->m_arguments[0]->m_type.DropQualifiers();
      if (u != Type::PseudoInteger() && u.IsSubtypeOf(t) == NO)
        return Type::Suppress();

      Context::PushVoid pv(ctx, false);
      an->m_arguments[0] = an->m_arguments[0]->ResolveFully(ctx);
      Value *v = an->m_arguments[0]->Evaluate(t);
      if (v->ObjectType() != t)
        return Type::Suppress();

      ssize_t ord = v->As<ssize_t>();
      if (ord < 0 || (size_t)ord >= an->m_receiverType.FieldCount()) {
        EmitError(an) << "Subscript [" << ord << "] out of range; tuple has "
                      << an->m_receiverType.FieldCount() << " fields.";
        ctx.m_error = true;
        return Type::Suppress();
      }

      an->m_arguments[0] = new Literal(an->m_location, Value::NewInt(t, ord));
      an->m_formalTypes[0] = t;

      t = an->m_receiverType.TypeOf(ord).LValueRef();
      if (an->m_receiverType == ct_const)
        t = t.Const();
      an->m_receiverType = an->m_receiverType.LValueRef();
      return t;
     }

    static TupleSubscript s_macro;
  };

  TupleSubscript TupleSubscript::s_macro;

  class TupleDestruct: public NativeOperator {
  public:
    TupleDestruct() : NativeOperator("TupleDestruct") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      Type tt = an->m_receiverType;
      Location sl = an->m_location;
      size_t count = tt.FieldCount();
      vector<Node *> list;

      for (size_t i = count; i-- > 0; ) {
        Node *addr = new FieldAddr(sl, an->m_receiver->Deref(), i);
        Node *f = new Member(sl, addr->Deref(), wkhs_destructor, false);
        f = new Apply(f, addr->Deref());
        list.push_back(f);
      }

      return BuildSequence(an->m_location, Type::Void(), list);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 0)
        return Type::Suppress();
      return Type::Void();
    }

    static TupleDestruct s_macro;
  };

  TupleDestruct TupleDestruct::s_macro;

  static Node *FinishConstruct(Apply *an, vector<Node *> &list) {
    Node *e = BuildSequence(an->m_location, an->m_receiverType, list);
    return new TypeHolder(an->m_type, e);
  }

  class TupleConstructList: public NativeOperator {
  public:
    TupleConstructList() : NativeOperator("TupleConstructList") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      // FIXME: in non-void context, return the lvalue of receiver.
      Type tt = an->m_type;
      Location sl = an->m_location;
      size_t count = tt.FieldCount();
      vector<Node *> list;

      for (size_t i = 0; i < count; i++) {
        Type t = an->m_type.TypeOf(i);
        Node *arg = an->m_arguments[i];
        if (!an->m_passThrough[i] && an->m_formalTypes[i] != rt_rvalue)
          arg = arg->Deref();
        Node *in = new Initializer(sl, t, arg, false);
        Node *addr = new FieldAddr(sl, tt, i);
        Node *cn = new Construct(addr, in);
        list.push_back(cn);
      }

      return FinishConstruct(an, list);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      // Should never get here, because Apply learns of us only after
      // ResolveTypes is called on TupleConstruct.
      verify(false);
      return Type::Suppress();
    }

    static TupleConstructList s_macro;
  };

  TupleConstructList TupleConstructList::s_macro;

  class TupleConstruct: public NativeOperator {
  public:
    TupleConstruct() : NativeOperator("TupleConstruct") { }

    virtual ::MethodKind MethodKind() { return mk_construct; }

    virtual Node *Run(Apply *an, Context &ctx) {
      Type tt = an->m_type;
      Location sl = an->m_location;
      size_t count = tt.FieldCount();
      vector<Node *> list;

      if (an->m_arguments.size() == 0) {
        // Handle default construction.
        for (size_t i = 0; i < count; i++) {
          Type t = an->m_type.TypeOf(i);
          Node *in = new Initializer(sl, t, nullptr, false);
          Node *addr = new FieldAddr(sl, tt, i);
          Node *cn = new Construct(addr, in);
          list.push_back(cn);
        }
      } else {
        // Handle copy construction.
        for (size_t i = 0; i < count; i++) {
          Type t = an->m_type.TypeOf(i);
          Node *addr = new FieldAddr(sl, an->m_arguments[0]->Deref(), i);
          Node *in = new Initializer(sl, t, addr->Deref(), false);
          addr = new FieldAddr(sl, tt, i);
          Node *cn = new Construct(addr, in);
          list.push_back(cn);
        }
      }

      return FinishConstruct(an, list);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = an->m_receiverType.RValue();

      Context::PushVoid pv(ctx, false);
      an->m_receiver = an->m_receiver->ResolveFully(ctx);
      Type t = an->m_receiver->EvaluateType();
      if (t == rt_rvalueref)
        t = t.RValue();
      if (an->m_formalTypes.size() == 0)
        return t;

      // Copy construction (or possibly move construction; handled on a field-
      // by-field basis).
      Type u = an->m_arguments[0]->m_type.DropQualifiers();

      if (u == Type::PseudoList()) {
        // FIXME: how to handle indirect lists, e.g. cond ? (list1) : (list2)
        // Probably by casting each one to the target type.
        verify(an->m_arguments[0]->Kind() == nk_List);
        if (!safe_cast<List *>(an->m_arguments[0])->IsSubtypeOf(t))
          return Type::Suppress();

        // We have to get rid of the List node now, otherwise Apply will
        // attempt to create a list-valued temp var (not a good thing).  Turn
        // its list of values into our arguments, and switch to a new native
        // operator that knows how to handle that.
        auto ln = safe_cast<List *>(an->m_arguments[0]);
        size_t cnt = ln->m_values.size();
        an->ResetNativeOperator(&TupleConstructList::s_macro, cnt);
        for (size_t i = 0; i < cnt; i++) {
          Node *e = ln->m_values[i];
          an->m_arguments[i] = e;
          an->m_formalTypes[i] = t.TypeOf(i).PerfectForward(e->m_type);
          an->m_passThrough[i] = true;
        }
        return t;
      }

      // Look for copy construction from another suitable tuple.
      if (an->m_arguments.size() == 1 && u.IsSubtypeOf(t) != NO) {
        Type v = an->m_arguments[0]->m_type;
        if (v == rt_lvalueref || v == ct_const)
          an->m_formalTypes[0] = u.Const().LValueRef();
        else
          an->m_formalTypes[0] = u.RValueRef();
        return t;
      }

      // Copy construction from a list of values; effectively a pseudo list.
      size_t cnt = an->m_arguments.size();
      if (cnt != t.FieldCount())
        return Type::Suppress();

      for (size_t i = 0; i < cnt; i++) {
        Node *e = an->m_arguments[i];
        Type u = t.TypeOf(i);
        if (!List::ValueIsSubtypeOf(e, u))
          return Type::Suppress();
        an->m_formalTypes[i] = u.PerfectForward(e->m_type);
      }

      an->ResetNativeOperator(&TupleConstructList::s_macro, cnt);
      return t;
    }

    static TupleConstruct s_macro;
  };

  TupleConstruct TupleConstruct::s_macro;
}

namespace TC {
  NativeOperator *Tuple = &TupleConstruct::s_macro;
}

Node *Member::HandleTuples(Context &ctx) {
  // Go and get the member name.
  auto in = safe_cast<Ident *>(m_member);
  String *name = in->m_identifier;

  if (name == wks_op_eq) {
    m_macro = &TupleEqOp;
  } else if (name == wks_op_ne) {
    m_macro = &TupleNeOp;
  } else if (name == wks_op_lt) {
    m_macro = &TupleLtOp;
  } else if (name == wks_op_le) {
    m_macro = &TupleLeOp;
  } else if (name == wks_op_gt) {
    m_macro = &TupleGtOp;
  } else if (name == wks_op_ge) {
    m_macro = &TupleGeOp;
  } else if (name == wks_op_add) {
    m_macro = &TupleConcat::s_macro;
  } else if (name == wks_op_assign) {
    m_macro = &TupleAssign::s_macro;
  } else if (name == wks_op_count) {
    m_macro = &TupleCount::s_macro;
  } else if (name == wks_op_index) {
    m_macro = &TupleSubscript::s_macro;
  } else if (name == wkhs_destructor) {
    m_macro = &TupleDestruct::s_macro;
  } else if (ssize_t ord = m_object->m_type.OrdinalOf(name); ord >= 0) {
    Node *addr = m_object->ResolveFully(ctx);
    return (new FieldAddr(m_location, addr, ord))->Deref();
  } else {
    // FIXME: need a good error when an unknown tag is used as a member.
    // "Method signature not found" doesn't cut it.
    m_reason = r_try_operator;
  }

  return this;
}

static void PopulateCB() {
  //Namespace *ne = Namespace::StdNamespace();
  //Type tt = Type::JType();
}

static PopulateNamespace g_pn(PopulateCB);
