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

// Member node states related to members on code fragment values.

#include "entity/Const.h"
#include "entity/Method.h"
#include "entity/Namespace.h"
#include "entity/Specialization.h"
#include "node/Binary.h"
#include "node/CallBuiltin.h"
#include "node/Ident.h"
#include "node/Literal.h"
#include "node/Member.h"
#include "node/NativeOperator.h"
#include "node/Sequence.h"
#include "node/Store.h"
#include "target/Bytecode.h"
#include "util/CodeFragment.h"
#include "util/String.h"
#include "util/Value.h"

namespace {
  class CodeFragmentRelOp: public NativeOperator {
    Binary::Opcode      m_binop;

  public:
    CodeFragmentRelOp(const char *name, Binary::Opcode b)
        : NativeOperator(name),
          m_binop(b) { }

    virtual Node *Run(Apply *an, Context &ctx) {
      return new Binary(an->m_location, m_binop, an->m_receiver,
                        an->m_arguments[0]);
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();
      Type rt = an->m_arguments[0]->m_type.DropQualifiers();
      if (rt != tk_codefragment)
        return Type::Suppress(); // FIXME: handle subtyping
      an->m_formalTypes[0] = rt;
      an->m_receiverType = an->m_receiverType.RValue();
      return Type::Bool();
    }
  };

  class CFAssign: public BC::NativeFunction {
  public:
    CFAssign() : BC::NativeFunction("CFAssign") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra);
      auto dst = reinterpret_cast<CodeFragment **>(args[0].AsAddr());
      auto src = reinterpret_cast<CodeFragment *>(args[1].AsAddr());
      CodeFragment *old = *dst;
      *dst = src;
      CodeFragment::AddRef(src);
      CodeFragment::DropRef(old);
      return args[0];
    }
  };

  static CFAssign g_CFAssign;

  class CodeFragmentAssign: public NativeOperator {
  public:
    CodeFragmentAssign() : NativeOperator("CodeFragmentAssign") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      auto cbn = new CallBuiltin(an->m_location,
                                 ctx.m_void ? Type::Void() : an->m_receiverType,
                                 &g_CFAssign);
      cbn->AppendArg(an->m_receiver);
      cbn->AppendArg(an->m_arguments[0]);
      return cbn;
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 1)
        return Type::Suppress();
      Type t = an->m_receiverType;
      if (t == ct_const || t == rt_rvalue)
        return Type::Suppress();
      Type u = an->m_arguments[0]->m_type.DropQualifiers();
      if (u.IsSubtypeOf(t) != YES)
        return Type::Suppress();

      an->m_formalTypes[0] = u;
      return t.LValueRef();
    }

    static CodeFragmentAssign s_macro;
  };

  CodeFragmentAssign CodeFragmentAssign::s_macro;

  class CodeFragmentInsert: public NativeOperator {
  public:
    CodeFragmentInsert() : NativeOperator("CodeFragmentInsert") { }

    virtual Node *Run(Apply *an, Context &ctx) {
      Context::PushVoid pv(ctx, false);
      an->m_receiver = an->m_receiver->ResolveFully(ctx);
      Value *v = an->m_receiver->Evaluate(an->m_receiverType);
      CodeFragment *cf = v->As<CodeFragment *>();
      return cf->Get();
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 0)
        return Type::Suppress();
      return an->m_receiverType.BaseType(); //code->m_type;
     }

    static CodeFragmentInsert s_macro;
  };

  CodeFragmentInsert CodeFragmentInsert::s_macro;

  class CFDropRef: public BC::NativeFunction {
  public:
    CFDropRef() : BC::NativeFunction("CFDropRef") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra);
      auto cf = reinterpret_cast<CodeFragment *>(args[0].AsAddr());
      CodeFragment::DropRef(cf);
      return { };
    }
  };

  static CFDropRef g_CFDropRef;

  class CodeFragmentDestruct: public NativeOperator {
  public:
    CodeFragmentDestruct() : NativeOperator("CodeFragmentDestruct") { }

    virtual ::MethodKind MethodKind() { return mk_destruct; }

    virtual Node *Run(Apply *an, Context &ctx) {
      auto cbn = new CallBuiltin(an->m_location, Type::Void(), &g_CFDropRef);
      cbn->AppendArg(an->m_arguments[0]);
      return cbn;
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() != 0)
        return Type::Suppress();
      return Type::Void();
    }

    static CodeFragmentDestruct s_macro;
  };

  CodeFragmentDestruct CodeFragmentDestruct::s_macro;

  class CFAddRef: public BC::NativeFunction {
  public:
    CFAddRef() : BC::NativeFunction("CFAddRef") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra);
      auto cf = reinterpret_cast<CodeFragment *>(args[0].AsAddr());
      CodeFragment::AddRef(cf);
      return args[0];
    }
  };

  static CFAddRef g_CFAddRef;

  class CodeFragmentConstruct: public NativeOperator {
  public:
    CodeFragmentConstruct() : NativeOperator("CodeFragmentConstruct") { }

    virtual ::MethodKind MethodKind() { return mk_construct; }

    virtual Node *Run(Apply *an, Context &ctx) {
      if (an->m_formalTypes.size() == 0)
        return new Literal(an->m_location, Value::New(an->m_receiverType));
      auto cbn = new CallBuiltin(an->m_location, an->m_receiverType,
                                 &g_CFAddRef);
      cbn->AppendArg(an->m_arguments[0]);
      return cbn;
    }

    virtual Type ResolveTypes(Apply *an, Context &ctx) {
      Type t = an->m_receiver->EvaluateType();
      an->m_receiverType = an->m_receiverType.RValue();
      if (an->m_formalTypes.size() == 0)
        return t;
      if (an->m_formalTypes.size() > 1)
        return Type::Suppress();
      Type u = an->m_arguments[0]->m_type.DropQualifiers();
      if (u.IsSubtypeOf(t) == NO)
        return Type::Suppress();
      an->m_formalTypes[0] = u;
      return t;
    }

    static CodeFragmentConstruct s_macro;
  };

  CodeFragmentConstruct CodeFragmentConstruct::s_macro;
}

namespace TC {
  NativeOperator *CodeFragment = &CodeFragmentConstruct::s_macro;
}

static CodeFragmentRelOp EqOp("CodeFragmentEq", Binary::op_seteq);
static CodeFragmentRelOp NeOp("CodeFragmentNe", Binary::op_setne);

Node *Member::HandleCodeFragments(Context &ctx) {
  // Go and get the member name.
  auto in = safe_cast<Ident *>(m_member);
  String *name = in->m_identifier;

  if (name == wks_op_eq) {
    m_macro = &EqOp;
  } else if (name == wks_op_ne) {
    m_macro = &NeOp;
  } else if (name == wks_op_assign) {
    m_macro = &CodeFragmentAssign::s_macro;
  } else if (name == wkhs_op_macro_insert) {
    m_macro = &CodeFragmentInsert::s_macro;
  } else if (name == wkhs_destructor) {
    m_macro = &CodeFragmentDestruct::s_macro;
  } else {
    m_reason = r_try_operator;
  }

  return this;
}

namespace BG {
  Entity *CodeFragment(Derivation *d) {
    auto v = safe_cast<Value *>(d->m_bindings[0]);
    verify(v->ObjectType() == tk_type);
    Type t = Type::CodeFragment(v->AsType());
    return new Const(nullptr, "T", Type::JType(), &t, sizeof(t));
  }
}

static void PopulateCB() {
  Namespace *ne = Namespace::StdNamespace();

  String *name = String::Get(wks_codefragment_t);
  vector<Type> ts;
  ts.push_back(Type::JType());
  auto s = new Specialization(ne, gk_type, name, bg_CodeFragment, ts);
  ne->AddBuiltinGeneric(s);
}

static PopulateNamespace g_pn(PopulateCB);
