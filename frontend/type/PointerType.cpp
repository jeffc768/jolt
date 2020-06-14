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

// Member node states related to members on pointer values.

#include "entity/BaseSpecifier.h"
#include "entity/Class.h"
#include "entity/Const.h"
#include "entity/Method.h"
#include "entity/Namespace.h"
#include "entity/Specialization.h"
#include "entity/Var.h"
#include "node/AddrOf.h"
#include "node/Binary.h"
#include "node/BuildPointer.h"
#include "node/ExtractAddress.h"
#include "node/ExtractDescriptor.h"
#include "node/FieldAddr.h"
#include "node/Ident.h"
#include "node/Literal.h"
#include "node/Load.h"
#include "node/Member.h"
#include "node/NativeOperator.h"
#include "node/PointerCast.h"
#include "node/Sequence.h"
#include "node/Store.h"
#include "node/Unary.h"
#include "node/VarAddr.h"
#include "node/VarDecl.h"
#include "util/String.h"
#include "util/Value.h"

namespace {
  class PointerDeref: public NativeOperator {
  public:
    PointerDeref() : NativeOperator("PointerDeref") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      return an->m_receiver->Deref();
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      an->m_receiverType = an->m_receiverType.RValue();
      if (an->m_formalTypes.size())
        return Type::Suppress();
      return an->m_receiverType.BaseType().LValueRef();
    }

    static PointerDeref s_macro;
  };

  PointerDeref PointerDeref::s_macro;

  // Helper class used only by PointerAssign below.
  class PointerCaster {
    vector<size_t>    m_path;
    Class            *m_srcClass    = nullptr;
    Class            *m_dstClass    = nullptr;

  public:
    PointerCaster(Type srctype, Type dsttype) {
      if (srctype == tk_class)
        m_srcClass = srctype.Class();
      if (dsttype == tk_class)
        m_dstClass = dsttype.Class();
      if (m_srcClass != m_dstClass)
        m_srcClass->IsSubclassOf(m_dstClass, &m_path);
    }

    Node *PtrCast(Node *ptr) {
      // FIXME: handle ambiguous inheritance.
      Class *ce = m_srcClass;
      for (size_t i = m_path.size(); i-- > 0; ) {
        BaseSpecifier *bs = ce->GetBase(m_path[i]);
        ptr = new FieldAddr(ptr->m_location, ptr->Deref(), bs->m_ord);
        ce = bs->m_baseClass;
      }
      return ptr;
    }
  };

  class PointerCompare: public NativeOperator {
    Binary::Opcode      m_binop;

  public:
    PointerCompare(const char  *name, Binary::Opcode b)
        : NativeOperator(name),
          m_binop(b) { }

    virtual Node *Run(Apply *an, Context &ctx) {
      Type lt = an->m_receiverType.DropQualifiers();
      Node *le = an->m_receiver;
      Node *re = an->m_arguments[0];
      Location sl = an->m_location;

      if (lt.StorageSize() == sizeof(void *))
        return new Binary(sl, m_binop, le, re);

      // Comparing fat pointers.  First separate the pointer and descriptor.
      TempScope ts;
      auto [p1, d1] = Disect(le, ts);
      auto [p2, d2] = Disect(re, ts);

      // Start by comparing the descriptors.  The result won't be used unless
      // the pointers are equal.
      Node *c2 = new Binary(sl, m_binop, d1, d2);

      // Equality/inequality is simpler than other comparisons.
      if (m_binop == Binary::op_seteq || m_binop == Binary::op_setne) {
        Node *c1 = new Binary(sl, m_binop, p1, p2);
        Node *e = new Binary(sl, Binary::op_and, c1, c2);
        return ts.Get(e);
      } else {
        Var *ve1 = new Var(sl, p1->m_type);
        VarDecl *vd1 = new VarDecl(ve1, nullptr, p1, dm_leaves_scope);
        ts.AddTemp(vd1);
        ve1->TrackResolution();

        Var *ve2 = new Var(sl, p2->m_type);
        VarDecl *vd2 = new VarDecl(ve2, nullptr, p2, dm_leaves_scope);
        ts.AddTemp(vd2);
        ve2->TrackResolution();

        Binary::Opcode op = m_binop;
        if (op == Binary::op_setle)
          op = Binary::op_setlt;
        else if (op == Binary::op_setge)
          op = Binary::op_setgt;

        Node *e1 = new VarAddr(sl, ve1);
        e1 = new Load(sl, e1->Deref());
        Node *e2 = new VarAddr(sl, ve2);
        e2 = new Load(sl, e2->Deref());
        Node *c1a = new Binary(sl, op, e1, e2);

        e1 = new VarAddr(sl, ve1);
        e1 = new Load(sl, e1->Deref());
        e2 = new VarAddr(sl, ve2);
        e2 = new Load(sl, e2->Deref());
        Node *c1b = new Binary(sl, Binary::op_seteq, e1, e2);

        c1b = new Binary(sl, Binary::op_and, c2, c1b);
        c1a = new Binary(sl, Binary::op_or, c1a, c1b);
        return ts.Get(c1a);
      }
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();

      Type rt = an->m_receiverType.DropQualifiers();
      Type at = an->m_arguments[0]->m_type.DropQualifiers();

      if (an->m_receiver->IsNull()) {
        an->m_receiverType = at;
        an->m_formalTypes[0] = at;
        return Type::Bool();
      }

      if (!an->m_arguments[0]->IsNull()) {
        rt = Type::Join(rt, at);
        if (rt != tk_pointer)
          return Type::Suppress();
      }
      an->m_receiverType = rt;
      an->m_formalTypes[0] = rt;
      return Type::Bool();
    }

  private:
    std::pair<Node *, Node *> Disect(Node *n, TempScope &ts) {
      if (auto *bp = dyn_cast<BuildPointer *>(n)) {
        return { bp->m_pointer, bp->m_descriptor };
      }

      Location sl = n->m_location;
      Var *ve = new Var(sl, n->m_type.DropQualifiers());
      VarDecl *vd = new VarDecl(ve, nullptr, n, dm_leaves_scope);
      ts.AddTemp(vd);
      ve->TrackResolution();

      Node *e1 = new VarAddr(sl, ve);
      e1 = new Load(sl, e1->Deref());
      e1 = new ExtractAddress(e1);

      Node *e2 = new VarAddr(sl, ve);
      e2 = new Load(sl, e2->Deref());
      e2 = new ExtractDescriptor(e2);

      return { e1, e2 };
    }
  };

  class PointerAssign: public NativeOperator {
  public:
    PointerAssign() : NativeOperator("PointerAssign") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      bool srcThin = an->m_arguments[0]->m_type.IsThin();
      bool dstThin = an->m_receiverType.IsThin();

      Type st = an->m_arguments[0]->m_type.BaseType();
      Type dt = an->m_receiverType.BaseType();
      PointerCaster pc(st, dt);

      TempScope ts;
      ArgAddr dst_addr(ts, an->m_location, an->m_receiver,
                       an->m_receiver->m_type, dm_never);
      ArgAddr src_addr(ts, an->m_location, an->m_arguments[0],
                       an->m_arguments[0]->m_type, dm_never);

      Node *src = src_addr.GetAddr();
      Node *dst = dst_addr.GetLValue();

      Node *s1;
      if (dstThin) {
        if (srcThin) {
          s1 = new Store(an->m_location, dst, pc.PtrCast(src));
        } else {
          src = new ExtractAddress(src);
          s1 = new Store(an->m_location, dst, pc.PtrCast(src));
          // FIXME: runtime check to verify conformant array size.
        }
      } else {
        Node *ptr = nullptr;
        Node *dsc = nullptr;

        if (srcThin) {
          ptr = pc.PtrCast(src);
          (void)ptr;
          verify(0); // FIXME: generate conformant array descriptor
        } else {
          ptr = new ExtractAddress(src);
          ptr = pc.PtrCast(ptr);
          dsc = new ExtractDescriptor(src_addr.GetAddr());
        }

        s1 = new BuildPointer(an->m_receiverType.DropQualifiers(), ptr, dsc);
        s1 = new Store(an->m_location, dst, s1);
      }

      // Return destination as l-value as our value.
      if (!ctx.m_void)
        dst = new Sequence(s1, dst_addr.GetLValue(), false);
      return ts.Get(dst);
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

    static PointerAssign s_macro;
  };

  PointerAssign PointerAssign::s_macro;

  class PointerConstruct: public NativeOperator {
  public:
    PointerConstruct() : NativeOperator("PointerConstruct") { }

    virtual ::MethodKind MethodKind() { return mk_construct; }

    virtual Node *Run(Apply *an, Context &ctx) {
      Type t = an->m_type;
      if (an->m_formalTypes.size() == 0) {
        // Default constructor:  construct null pointer.
        if (t.IsThin()) {
          return new Literal(an->m_location, Value::NewPtr(t, nullptr));
        } else {
          return new BuildPointer(an->m_location, t);
        }
      } else {
        // Copy constructor.
        Node *rv = an->m_arguments[0];
        Type u = rv->m_type;

        // Ensure source and target are both thin or fat by converting the
        // source if necessary.
        if (!t.IsThin() && u.IsThin()) {
          rv = new AddrOf(rv->Deref(), true);
        } else if (t.IsThin() && !u.IsThin()) {
          rv = new ExtractAddress(rv);
          // FIXME: runtime check of conformant array size.
        } else if (!t.IsThin() && !u.IsThin()) {
        } else {
        }

        // Handle pointer upcasting.
        if (u.DropQualifiers() != t.DropQualifiers()) {
          verify((t.BaseType() == tk_class && u.BaseType() == tk_class) ||
                 (t.BaseType() == tk_array && u.BaseType() == tk_array &&
                  t.BaseType().ElementType() == u.BaseType().ElementType() &&
                  !t.BaseType().IndexType()) ||
                 t.BaseType() == u.BaseType().Const());
          rv = new PointerCast(t, rv);
        }

        return rv;
      }
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      Type t = an->m_receiver->EvaluateType();
      an->m_receiverType = an->m_receiverType.RValue();
      if (an->m_formalTypes.size() == 0)
        return t;
      if (an->m_formalTypes.size() > 1)
        return Type::Suppress();
      Type u = an->m_arguments[0]->m_type.DropQualifiers();
#if 0
      // FIXME: handle casts.  This doesn't do it.  The case that breaks it
      // is t == char[] const*+ and u == char[0..12] const*.  Causes infinite
      // recursion.
      if (u.IsSubtypeOf(t) == NO)
        return Type::Suppress();
      an->m_formalTypes[0] = t.DropQualifiers();
#else
      if (an->m_arguments[0]->IsNull()) {
        an->m_arguments[0] = an->m_arguments[0]->LowerSpecialEnums(t);
        u = t;
      } else if (u.IsSubtypeOf(t) == NO) {
        return Type::Suppress();
      }
      an->m_formalTypes[0] = u;
#endif
      return t;
    }

    static PointerConstruct s_macro;
  };

  PointerConstruct PointerConstruct::s_macro;
}

namespace TC {
  NativeOperator *Pointer = &PointerConstruct::s_macro;
}

static PointerCompare EqOp ("PointerEq",  Binary::op_seteq);
static PointerCompare NeOp ("PointerNe",  Binary::op_setne);
static PointerCompare LtOp ("PointerLt",  Binary::op_setlt);
static PointerCompare LeOp ("PointerLe",  Binary::op_setle);
static PointerCompare GtOp ("PointerGt",  Binary::op_setgt);
static PointerCompare GeOp ("PointerGe",  Binary::op_setge);

Node *Member::HandlePointers(Context &ctx) {
  // Go and get the member name.
  auto in = safe_cast<Ident *>(m_member);
  String *name = in->m_identifier;

  // Specially handle operator-> on class objects.
  bool special = false;
  if (auto an = dyn_cast<Apply *>(m_object)) {
    if (an->m_method && an->m_method->Name() == wks_op_deref) {
      special = true;
      Type t = an->m_type.BaseType();
      if (t == tk_class && t.Class()->HasOpDeref()) {
        auto mn = new Member(m_location, m_object->Deref(), m_member, true);
        return mn->ResolveType(ctx);
      }
    }
  }

  if (name == wks_op_add) {
    verify(false);
  } else if (name == wks_op_sub) {
    verify(false);
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
  } else if (name == wks_op_deref) {
    m_macro = &PointerDeref::s_macro;
  } else if (name == wks_op_index) {
    verify(false); //m_macro = &PointerSubscript::s_macro;
  } else if (name == wks_op_mul) {
    m_macro = &PointerDeref::s_macro;
  } else if (name == wks_op_assign) {
    m_macro = &PointerAssign::s_macro;
  } else if (special) {
    auto mn = new Member(m_location, m_object, m_member, true);
    return mn->ResolveType(ctx);
  } else {
    m_reason = r_try_operator;
  }

  return this;
}

NativeOperator *MapOpToPointer(Binary::Opcode bop) {
  switch (bop) {
    //case Binary::op_add:      return &AddOp;
    //case Binary::op_sub:      return &SubOp;
    case Binary::op_seteq:    return &EqOp;
    case Binary::op_setne:    return &NeOp;
    case Binary::op_setlt:    return &LtOp;
    case Binary::op_setle:    return &LeOp;
    case Binary::op_setgt:    return &GtOp;
    case Binary::op_setge:    return &GeOp;
    default:                  return nullptr;
  }
}

namespace BG {
  Entity *Pointer(Derivation *d) {
    auto v = safe_cast<Value *>(d->m_bindings[0]);
    verify(v->ObjectType() == tk_type);
    Type t = Type::PointerTo(v->AsType());
    return new Const(nullptr, "T", Type::JType(), &t, sizeof(t));
  }
}

static void PopulateCB() {
  Namespace *ne = Namespace::StdNamespace();

  String *name = String::Get(wks_Pointer);
  vector<Type> ts;
  ts.push_back(Type::JType());
  auto s = new Specialization(ne, gk_type, name, bg_Pointer, ts);
  ne->AddBuiltinGeneric(s);

  // FIXME: mark generic entity so it prohibits additional specializations.
  name = String::Get(wks_BuiltinPointer);
  s = new Specialization(ne, gk_type, name, bg_Pointer, ts);
  ne->AddBuiltinGeneric(s);

  name = String::Get(wks_null);
  Const *ce = new Const(ne, name, Type::PseudoEnum(), &name, sizeof(name));
  ne->AddBuiltinEntity(ce);
}

static PopulateNamespace g_pn(PopulateCB);
