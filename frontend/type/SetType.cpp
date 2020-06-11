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

// Member node states related to members on set values.
// FIXME: this file is a stub cloned from elsewhere for now.

#include "entity/Const.h"
#include "entity/Method.h"
#include "entity/Namespace.h"
#include "entity/Specialization.h"
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
  class SetRelOp: public NativeOperator {
    Binary::Opcode      m_binop;

  public:
    SetRelOp(const char *name, Binary::Opcode b)
        : NativeOperator(name),
          m_binop(b) { }

    virtual Node *Run(Apply *an, Context &ctx) {
      verify(false); // FIXME
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      verify(false && m_binop); // FIXME: implement
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

  SetRelOp SetEqOp("SetEq", Binary::op_seteq);
  SetRelOp SetNeOp("SetNe", Binary::op_setne);
  SetRelOp SetLtOp("SetLt", Binary::op_setlt);
  SetRelOp SetLeOp("SetLe", Binary::op_setle);
  SetRelOp SetGtOp("SetGt", Binary::op_setgt);
  SetRelOp SetGeOp("SetGe", Binary::op_setge);

  class SetUnion: public NativeOperator {
  public:
    SetUnion() : NativeOperator("SetUnion") { }

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

    static SetUnion s_macro;
  };

  SetUnion SetUnion::s_macro;

  class SetAssignList: public NativeOperator {
  public:
    SetAssignList() : NativeOperator("SetAssignList") { }

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
      // ResolveTypes is called on SetAssign.
      verify(false);
      return Type::Suppress();
    }

    static SetAssignList s_macro;
  };

  SetAssignList SetAssignList::s_macro;

  class SetAssign: public NativeOperator {
  public:
    SetAssign() : NativeOperator("SetAssign") { }

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

      // FIXME: how to handle a list being the target, e.g. (a,b) = set
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
        an->ResetNativeOperator(&SetAssignList::s_macro, cnt);
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

    static SetAssign s_macro;
  };

  SetAssign SetAssign::s_macro;

  class SetCount: public NativeOperator {
    static void FlattenList(vector<Node *> &flat, vector<Node *> &list) {
      for (Node *e : list)
        if (auto ln = dyn_cast<List *>(e))
          FlattenList(flat, ln->m_values);
        else
          flat.push_back(e);
    }

  public:
    SetCount() : NativeOperator("SetCount") { }

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

    static SetCount s_macro;
  };

  SetCount SetCount::s_macro;

  class SetDestruct: public NativeOperator {
  public:
    SetDestruct() : NativeOperator("SetDestruct") { }

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

    static SetDestruct s_macro;
  };

  SetDestruct SetDestruct::s_macro;

  static Node *FinishConstruct(Apply *an, vector<Node *> &list) {
    Node *e = BuildSequence(an->m_location, an->m_receiverType, list);
    return new TypeHolder(an->m_type, e);
  }

  class SetConstructList: public NativeOperator {
  public:
    SetConstructList() : NativeOperator("SetConstructList") { }

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
      // ResolveTypes is called on SetConstruct.
      verify(false);
      return Type::Suppress();
    }

    static SetConstructList s_macro;
  };

  SetConstructList SetConstructList::s_macro;

  class SetConstruct: public NativeOperator {
  public:
    SetConstruct() : NativeOperator("SetConstruct") { }

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
        an->ResetNativeOperator(&SetConstructList::s_macro, cnt);
        for (size_t i = 0; i < cnt; i++) {
          Node *e = ln->m_values[i];
          an->m_arguments[i] = e;
          an->m_formalTypes[i] = t.TypeOf(i).PerfectForward(e->m_type);
          an->m_passThrough[i] = true;
        }
        return t;
      }

      // Look for copy construction from another suitable set.
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

      an->ResetNativeOperator(&SetConstructList::s_macro, cnt);
      return t;
    }

    static SetConstruct s_macro;
  };

  SetConstruct SetConstruct::s_macro;
}

namespace TC {
  NativeOperator *Set = &SetConstruct::s_macro;
}

Node *Member::HandleSets(Context &ctx) {
  // Go and get the member name.
  auto in = safe_cast<Ident *>(m_member);
  String *name = in->m_identifier;

  if (name == wks_op_eq) {
    m_macro = &SetEqOp;
  } else if (name == wks_op_ne) {
    m_macro = &SetNeOp;
  } else if (name == wks_op_lt) {
    m_macro = &SetLtOp;
  } else if (name == wks_op_le) {
    m_macro = &SetLeOp;
  } else if (name == wks_op_gt) {
    m_macro = &SetGtOp;
  } else if (name == wks_op_ge) {
    m_macro = &SetGeOp;
  } else if (name == wks_op_add) {
    m_macro = &SetUnion::s_macro;
  } else if (name == wks_op_sub) {
    //m_macro = &SetDifference::s_macro;
    verify(false);
  } else if (name == wks_op_mul) {
    //m_macro = &SetIntersection::s_macro;
    verify(false);
  } else if (name == wks_op_bitnot) {
    //m_macro = &SetComplement::s_macro;
    verify(false);
  } else if (name == wks_op_assign) {
    m_macro = &SetAssign::s_macro;
  } else if (name == wks_op_in) {
    //m_macro = &SetIn::s_macro;
    verify(false);
  } else if (name == wks_op_count) {
    m_macro = &SetCount::s_macro;
  } else if (name == wkhs_destructor) {
    m_macro = &SetDestruct::s_macro;
  } else {
    m_reason = r_try_operator;
  }

  return this;
}

namespace BG {
  Entity *Set(Derivation *d) {
    auto v = safe_cast<Value *>(d->m_bindings[0]);
    verify(v->ObjectType() == tk_type);
    Type t = Type::SetOf(v->AsType());
    return new Const(nullptr, "T", Type::JType(), &t, sizeof(t));
  }
}

static void PopulateCB() {
  Namespace *ne = Namespace::StdNamespace();

  // FIXME: mark generic entity so it prohibits additional specializations.
  String *name = String::Get(wks_Set);
  vector<Type> ts;
  ts.push_back(Type::JType());
  auto s = new Specialization(ne, gk_type, name, bg_Set, ts);
  ne->AddBuiltinGeneric(s);
}

static PopulateNamespace g_pn(PopulateCB);
