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

// Member node states related to members on array values.

// FIXME: need to support pseudolist, and in general follow what TupleType
// does.

#include "entity/Const.h"
#include "entity/Method.h"
#include "entity/Namespace.h"
#include "entity/Specialization.h"
#include "entity/Var.h"
#include "node/Binary.h"
#include "node/Construct.h"
#include "node/ExtractDescriptor.h"
#include "node/Ident.h"
#include "node/Index.h"
#include "node/Initializer.h"
#include "node/Literal.h"
#include "node/Load.h"
#include "node/Member.h"
#include "node/NativeOperator.h"
#include "node/Sequence.h"
#include "node/Store.h"
#include "node/TypeHolder.h"
#include "node/VarAddr.h"
#include "node/VarDecl.h"
#include "node/While.h"
#include "util/Integer.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"

static Node *GetArraySize(Location sl, Type t, Node *ary) {
  if (t.IndexType().IsKnown())
    return new Literal(sl, Value::NewInt(Type::SizeT(),
                                         t.IndexType().Cardinality()));
  else if (ary)
    return new ExtractDescriptor(ary);
  else
    return new ExtractDescriptor(sl, Type::SizeT());
}

static Node *GenerateLoopOverElements(Location sl, Type t, Node *sizeExpr,
                                      size_t first, Node *&iv, Node **&body) {
  // Start with induction variable.
  Var *ve = new Var(sl, Type::SizeT());
  ve->TrackResolution();
  Node *e = new Literal(sl, Value::NewInt(Type::SizeT(), first));
  VarDecl *vd = new VarDecl(ve, nullptr, e, dm_never);
  iv = (new VarAddr(sl, ve))->Deref();

  // Loop body wrapper.  Return pointer to first node of the sequence node for
  // our caller to insert the loop body.
  Node *e2 = new Store(sl, iv,
                       new Binary(sl, Type::SizeT(), Binary::op_add,
                                  new Load(sl, iv),
                                  new Literal(sl, Value::NewInt(Type::SizeT(),
                                                                1))));
  Sequence *sn = new Sequence(sl, nullptr, e2, false);
  body = &sn->m_first;

  // Finally, the loop itself.
  e = new While(sl,
                new Binary(sl, Type::Bool(), Binary::op_setlt,
                           new Load(sl, iv), sizeExpr),
                sn);

  vd->m_expr = e;
  return vd;
}

namespace {
  class ArraySubscript: public NativeOperator {
  public:
    ArraySubscript() : NativeOperator("ArraySubscript") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      return (new Index(an->m_location, an->m_receiver->Deref(),
                        an->m_arguments[0]))->Deref();
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();
      Type t = Type::Int();
      Type u = an->m_arguments[0]->m_type.DropQualifiers();
      if (u != Type::PseudoInteger() && u.IsSubtypeOf(t) == NO)
        return Type::Suppress();
      an->m_receiverType = an->m_receiverType.LValueRef();
      an->m_formalTypes[0] = t; // FIXME
      t = an->m_receiverType.ElementType();
      if (an->m_receiverType == ct_const)
        t = t.Const();
      return t.LValueRef();
    }

    static ArraySubscript s_macro;
  };

  ArraySubscript ArraySubscript::s_macro;

  class ArraySize: public NativeOperator {
  public:
    ArraySize() : NativeOperator("ArraySize") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      return GetArraySize(an->m_location, an->m_receiverType, an->m_receiver);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = an->m_receiverType.LValueRef().Const();
      return an->m_formalTypes.size() == 0 ? Type::SizeT() : Type::Suppress();
    }

    static ArraySize s_macro;
  };

  ArraySize ArraySize::s_macro;

  class ArrayDestruct: public NativeOperator {
  public:
    ArrayDestruct() : NativeOperator("ArrayDestruct") { }

    virtual ::MethodKind MethodKind() { return mk_destruct; }

    virtual Node *Run(Apply *an, Context &ctx) {
      Type t = an->m_receiverType;
      Location sl = an->m_location;

      // Generate a loop.
      Node *iv = nullptr;
      Node **body = nullptr;
      Node *sizeExpr = GetArraySize(sl, t, an->m_receiver);
      Node *loop = GenerateLoopOverElements(sl, t, sizeExpr, 0, iv, body);

      // Loop body.
      Node *addr = new Index(sl, an->m_receiver->Deref(), new Load(sl, iv));
      Node *f = new Member(sl, addr->Deref(), wkhs_destructor, false);
      *body = new Apply(f, addr->Deref());

      return loop;
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = an->m_receiverType.LValueRef();
      return an->m_formalTypes.size() == 0 ? Type::Void() : Type::Suppress();
    }

    static ArrayDestruct s_macro;
  };

  ArrayDestruct ArrayDestruct::s_macro;

  class ArrayConstruct: public NativeOperator {
  public:
    ArrayConstruct() : NativeOperator("ArrayConstruct") { }

    virtual ::MethodKind MethodKind() { return mk_construct; }

    virtual Node *Run(Apply *an, Context &ctx) {
      Type t = an->m_type;
      Type et = t.ElementType();
      Location sl = an->m_location;

      Node *size = nullptr;
      bool isDynamic = an->m_type == tk_array &&
                      !an->m_type.IndexType().IsKnown();
      if (isDynamic) {
        size = new ExtractDescriptor(sl, Type::SizeT());
      } else {
        size_t sz = static_cast<size_t>(t.IndexType().Cardinality());
        size = new Literal(an->m_location, Value::NewInt(Type::SizeT(), sz));
      }

      if (an->m_formalTypes.size() == 1) {
        Node *arg = an->m_arguments[0];
        if (arg->m_type == tk_pointer) {
          Type u = arg->m_type.BaseType();

          if (u == tk_array && u.ElementType().IsSubtypeOf(et) != NO) {
            // Do copy construction.
            // FIXME: handle dynamic array.
            sl = arg->m_location;
            verify(t.IndexType().IsKnown());  // FIXME: handle conformant source
            verify(u.IndexType().IsKnown());  // FIXME: handle conformant source

            // Generate a loop.
            Node *iv = nullptr;
            Node **body = nullptr;
            Node *loop = GenerateLoopOverElements(sl, t, size, 0, iv, body);

            // Loop body.
            Node *addr = new Index(sl, arg->Deref(), new Load(sl, iv));
            Initializer *in = new Initializer(sl, et, addr->Deref(), false);
            addr = new Index(in->m_location, t, new Load(sl, iv));
            *body = new Construct(addr, in);

            return loop;
          }
        }
      }

      vector<Node *> list;
      size_t argcnt = an->m_arguments.size();

      // FIXME: diagnose too many values.  This must be a runtime check when
      // it's a dynamic array.

      for (size_t i = 0; i < argcnt; i++) {
        Node *e = an->m_arguments[i];
        Node *addr = new Index(e->m_location, t,
                               new Literal(e->m_location,
                                           Value::NewInt(Type::SizeT(), i)));
        e = new Construct(addr, e);
        list.push_back(e);
      }

      // If not enough values have been supplied, then default construct the
      // remaining elements.
      if (isDynamic || argcnt < (size_t)t.IndexType().Cardinality()) {
        // Generate a loop.
        Node *iv = nullptr;
        Node **body = nullptr;
        Node *loop = GenerateLoopOverElements(sl, t, size, argcnt, iv, body);
        // Loop body.
        Initializer *in = new Initializer(sl, et, nullptr, false);
        Node *addr = new Index(in->m_location, t, new Load(sl, iv));
        *body = new Construct(addr, in);

        list.push_back(loop);
      }

      // Chain all element constructors together and return.
      if (list.empty()) {
        return new Literal(sl, Value::New(Type::Void()));
      } else {
        Node *e = list.back();
        list.pop_back();
        while (!list.empty()) {
          e = new Sequence(list.back(), e, false);
          list.pop_back();
        }

        return new TypeHolder(t, e);
      }
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = an->m_receiverType.RValue();

      Context::PushVoid pv(ctx, false);
      an->m_receiver = an->m_receiver->ResolveFully(ctx);
      Type t = an->m_receiver->EvaluateType();
      if (an->m_formalTypes.size() == 0)
        return t;
      Type et = t.ElementType();

      // Look for copy construction.
      if (an->m_formalTypes.size() == 1) {
        Type u = an->m_arguments[0]->m_type;
        if (u == tk_array && u.ElementType().IsSubtypeOf(et) != NO) {
          if (u.IndexType().IsKnown()) {
            if (t.Cardinality() != u.Cardinality())
              return Type::Suppress();
          }
          an->m_formalTypes[0] = u == rt_lvalueref ? u : u.RValueRef();
          return t;
        }
      }

      // Copy initialization.  Verify argument types.
      for (size_t i = 0; i < an->m_arguments.size(); i++) {
        Type u = an->m_arguments[i]->m_type.DropQualifiers();
        if (u != Type::PseudoInteger() &&
            u.IsSubtypeOf(et.DropQualifiers()) == NO)
          return Type::Suppress();
        an->m_formalTypes[i] = et;
      }

      // Verify argument count to extent possible.
      auto card = t.IndexType().Cardinality();
      auto size = an->m_arguments.size();
      if (card >= 0 && (size_t)card < size) {
        EmitError(an) << "Number of values (" << size
                      << ") exceeds size of array (" << card << ").";
        ctx.m_error = true;
      } else if (card == -1) {
        t = t.SetIndexType(size);
      }

      return t;
    }

    static ArrayConstruct s_macro;
  };

  ArrayConstruct ArrayConstruct::s_macro;
}

namespace TC {
  NativeOperator *Array = &ArrayConstruct::s_macro;
}

Node *Member::HandleArrays(Context &ctx) {
  // Go and get the member name.
  auto in = safe_cast<Ident *>(m_member);
  String *name = in->m_identifier;

  if (name == wks_op_index) {
    m_macro = &ArraySubscript::s_macro;
  } else if (name == wks_op_count) {
    m_macro = &ArraySize::s_macro;
  } else if (name == wkhs_destructor) {
    m_macro = &ArrayDestruct::s_macro;
  } else {
    m_reason = r_try_operator;
  }

  return this;
}

namespace BG {
  Entity *ConformantArray(Derivation *d) {
    auto v = safe_cast<Value *>(d->m_bindings[0]);
    verify(v->ObjectType() == tk_type);
    Type et = v->AsType();
    et = Type::ArrayOf(et);
    return new Const(nullptr, "T", Type::JType(), &et, sizeof(et));
  }

  Entity *FixedArray(Derivation *d) {
    auto v = safe_cast<Value *>(d->m_bindings[0]);
    verify(v->ObjectType() == tk_type);
    Type it = v->AsType();
    v = safe_cast<Value *>(d->m_bindings[1]);
    verify(v->ObjectType() == tk_type);
    Type et = v->AsType();
    et = Type::ArrayOf(et, it);
    return new Const(nullptr, "T", Type::JType(), &et, sizeof(et));
  }
}

static void PopulateCB() {
  Namespace *ne = Namespace::StdNamespace();

  // FIXME: mark generic entity so it prohibits additional specializations??
  String *name = String::Get(wks_ConformantArray);
  vector<Type> ts;
  ts.push_back(Type::JType());
  auto s = new Specialization(ne, gk_type, name, bg_ConformantArray, ts);
  ne->AddBuiltinGeneric(s);

  // FIXME: mark generic entity so it prohibits additional specializations??
  name = String::Get(wks_FixedArray);
  ts.push_back(Type::JType());
  s = new Specialization(ne, gk_type, name, bg_FixedArray, ts);
  ne->AddBuiltinGeneric(s);
}

static PopulateNamespace g_pn(PopulateCB);
