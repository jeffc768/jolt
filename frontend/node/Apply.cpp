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

#include "Apply.h"
#include "Call.h"
#include "Expr.h"
#include "FieldAddr.h"
#include "Ident.h"
#include "Initializer.h"
#include "Load.h"
#include "Literal.h"
#include "Member.h"
#include "NativeOperator.h"
#include "OverloadSetRef.h"
#include "PointerCast.h"
#include "Sequence.h"
#include "VarAddr.h"
#include "VarDecl.h"
#include "VtableSlot.h"
#include "entity/Argument.h"
#include "entity/BaseSpecifier.h"
#include "entity/Class.h"
#include "entity/Method.h"
#include "entity/OverloadSet.h"
#include "entity/Var.h"
#include "parser/Token.h"
#include "target/Target.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_NODE(Apply)

Apply::Apply(Location sl, unop opcode, Node *arg)
    : Node(sl) {
  // FIXME: need to handle all unary operators!
  WellKnownString wks = wks_max_wks;
  switch (opcode) {
    case unop::bitnot:            wks = wks_op_bitnot;            break;
    case unop::boolnot:           wks = wks_op_not;               break;
    case unop::const_:            wks = wks_op_const;             break;
    case unop::count:             wks = wks_op_count;             break;
    case unop::deref:             wks = wks_op_mul;               break;
    case unop::global:            wks = wkhs_op_global;           break;
    case unop::lref:              wks = wkhs_op_ref;              break;
    case unop::macroinsert:       wks = wkhs_op_macro_insert;     break;
    case unop::macroiter:         wks = wkhs_op_macro_iter;       break;
    case unop::move:              wks = wkhs_op_move;             break;
    case unop::mutable_:          wks = wks_op_mutable;           break;
    case unop::neg:               wks = wks_op_sub;               break;
    case unop::pos:               wks = wks_op_add;               break;
    case unop::postdecr:          wks = wks_op_post_decr;         break;
    case unop::postincr:          wks = wks_op_post_incr;         break;
    case unop::predecr:           wks = wks_op_pre_decr;          break;
    case unop::preincr:           wks = wks_op_pre_incr;          break;
    case unop::ptr:               wks = wkhs_op_ptr;              break;
    case unop::rref:              wks = wkhs_op_r_ref;            break;
    case unop::set_:              wks = wkhs_op_set;              break;
  }

  // Handle the unary operators by turning them into the equivalent
  // method calls.  "~a" becomes "a.operator ~()" (or "operator ~(a)"
  // if no such method exists).
  m_functor = new Member(sl, arg, wks, true);
}

Apply::Apply(Location sl, binop opcode, Node *arg1, Node *arg2)
    : Node(sl) {
  // We handle the Jolt binary operators as well as the application of an
  // argument list to a functor.  We know it's the latter if the opcode is '(',
  // '[' or '<'.

  // Handle the binary operators by turning them into the equivalent
  // method calls.  "a + b" becomes "a.operator +(b)" (or "operator +(a, b)"
  // if no such method exists).
  m_arguments.resize(1);
  m_arguments[0] = arg2;

  // FIXME: need to handle all binary operators!
  WellKnownString wks = wks_max_wks;
  switch (opcode) {
    case binop::add:              wks = wks_op_add;               break;
    case binop::addassign:        wks = wks_op_add_assign;        break;
    case binop::apply:            verify(false);
    case binop::as:               verify(false);
    case binop::assign:           wks = wks_op_assign;            break;
    case binop::bitand_:          wks = wks_op_bitand;            break;
    case binop::bitandassign:     wks = wks_op_bitand_assign;     break;
    case binop::bitor_:           wks = wks_op_bitor;             break;
    case binop::bitorassign:      wks = wks_op_bitor_assign;      break;
    case binop::bitxor:           wks = wks_op_bitxor;            break;
    case binop::bitxorassign:     wks = wks_op_bitxor_assign;     break;
    case binop::booland:          verify(false);
    case binop::boolor:           verify(false);
    case binop::closure:          verify(false);
    case binop::derefstar:        verify(false);
    case binop::derive:           verify(false); // handled by other constructor
    case binop::div:              wks = wks_op_div;               break;
    case binop::divassign:        wks = wks_op_div_assign;        break;
    case binop::dotstar:          verify(false);
    case binop::eq:               wks = wks_op_eq;                break;
    case binop::forward:          verify(false);
    case binop::ge:               wks = wks_op_ge;                break;
    case binop::gt:               wks = wks_op_gt;                break;
    case binop::le:               wks = wks_op_le;                break;
    case binop::lshift:           wks = wks_op_lshift;            break;
    case binop::lshiftassign:     wks = wks_op_lshift_assign;     break;
    case binop::lt:               wks = wks_op_lt;                break;
    case binop::mod:              wks = wks_op_rem;               break;
    case binop::modassign:        wks = wks_op_rem_assign;        break;
    case binop::mul:              wks = wks_op_mul;               break;
    case binop::mulassign:        wks = wks_op_mul_assign;        break;
    case binop::ne:               wks = wks_op_ne;                break;
    case binop::range:            wks = wks_op_range;             break;
    case binop::rshift:           wks = wks_op_rshift;            break;
    case binop::rshiftassign:     wks = wks_op_rshift_assign;     break;
    case binop::select:           verify(false);
    case binop::sub:              wks = wks_op_sub;               break;
    case binop::subassign:        wks = wks_op_sub_assign;        break;
    case binop::subscript:        verify(false); // handled by other constructor

    case binop::in: {
      // FIXME: For this operator, the receiver is the right operand, not the
      // left.  Nonetheless, the left operand should probably be evaluated
      // first.
      wks = wks_op_in;
      m_arguments[0] = arg1;
      arg1 = arg2;
      break;
    }
  }

  m_functor = new Member(sl, arg1, wks, true);
}

Apply::Apply(Location sl, binop opcode, Node *functor,
             AST::SafeArray<AST::MemberItem> args)
    : Node(sl),
      m_functor(functor) {
  // Go and make sense of the argument list.
  if (uint32_t len = args.size()) {
    m_arguments.reserve(len);

    // Collect arguments.
    // FIXME: handle presence of names.
    for (auto &arg : args)
      m_arguments.push_back(arg.m_value);

    // FIXME: don't ignore argument attributes (though they have no use until
    // metaprogramming is fully supported).
    // FIXME: The above re-ordering doesn't obey left-to-right evaluation
    // ordering of argument evaluation.  Only a problem if Jolt requires it,
    // which it does not at this time.
  }

  // Convert [] into explicit operator[] calls.  Don't do this for () as one
  // does not call operator() on a method (well, you can but that would lead to
  // infinite recursion).
  if (opcode == binop::subscript)
    m_functor = new Member(sl, m_functor, wks_op_index, true);
  else if (opcode == binop::derive)
    m_functor = new Member(sl, m_functor, wkhs_op_generic, true);
}

Apply::Apply(Location sl, Node *functor, Node *rcvr, NativeOperator *no,
             ArgsHelper &ah)
    : Node(sl),
      m_functor(functor),
      m_receiver(rcvr),
      m_macro(no) {
  m_arguments.swap(ah.m_values);
}

void Apply::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_functor;
  DF << m_arguments;
}

Apply::Apply(Inflator &IF)
    : Node(IF) {
  IF >> m_functor;
  IF >> m_arguments;
}

Node *Apply::ResolveType_(Context &ctx) {
  // To resolve the type, we must first determine the method signature.  To do
  // this requires that we know the type of all the arguments.
  for (auto &arg : m_arguments)
    arg = arg->ResolveType(ctx);

  if (m_functor) {
    m_functor = m_functor->ResolveType(ctx);

    if (auto mn = dyn_cast<Member *>(m_functor)) {
      Context::PushVoid pv(ctx, false);
      m_functor = mn->ResolveFully(ctx);
    }
  }

  // Handle member functors; i.e., methods as opposed to functions.
  if (auto mn = dyn_cast<Member *>(m_functor)) {
    if (mn->m_reason == Member::r_try_operator) {
      // If the functor is a Member node that failed to find the member, and
      // that member came from an operator, then try again as a global function;
      // i.e., retry a.operator+(b) as operator+(a,b).

      // Take the receiver and insert it as the first argument.
      m_arguments.insert(m_arguments.begin(), mn->m_object);

      // FIXME: should consider static methods at the least.
      // FIXME: if the member name was not found, mn->m_global is null.
      m_functor = mn->m_global;

      // Redo type resolution.
      return ResolveType_(ctx);
    } else if (mn->m_reason == Member::r_builtin) {
      // The Member node resolved to a built-in operation on a non-class type.
      m_macro = mn->m_macro;
      m_receiver = mn->m_object;
      m_functor = nullptr;
      if (mn->m_type == tk_valueless)
        m_type = mn->m_type;
      // FIXME: should try global functions just like r_try_operator.
    } else {
      m_functor = mn;
    }
  }

  // See if the functor or any of the arguments are valueless.
  if (m_functor && m_functor->m_type == tk_valueless)
    m_type = m_functor->m_type;

  for (Node *e : m_arguments) {
    if (Type et = e->m_type; et == tk_valueless) {
      if (et == Type::NotTyped()) {
        et = Type::Suppress();
        ctx.m_error = true;
        EmitError(e) << "Argument cannot be typed.";
      }
      m_type = Type::Join(m_type, et);
    }
  }

  // If any are valueless, then propagate upwards.
  if (m_type.IsKnown()) {
    if (m_type == Type::Transfers()) {
      m_type = Type::Suppress();
      ctx.m_error = true;
      EmitError(this) << "Argument list has arguments that do not complete "
                         "execution.";
    }

    // Nothing more we can do; we're done.
    return this;
  }

  // If the functor isn't a function or method, implicitly apply operator() to
  // it; in other words, x(y) becomes x.operator()(y).
  if (m_functor) {
    m_functor = m_functor->ResolveFully(ctx);
    if (m_functor->Kind() != nk_OverloadSetRef) {
      Member *mn = new Member(m_functor->m_location, m_functor,
                              wks_op_apply, true);
      m_functor = mn;

      // Redo type resolution.
      return ResolveType_(ctx);
    }
  }

  // Gather up all the pseudo-valued Literal nodes passed directly or indirectly
  // as an argument.
  for (auto arg : m_arguments) {
    m_pseudoBases.push_back(m_pseudoLiterals.size());
    if (arg->m_type == tk_pseudo)
      arg->GatherPseudoLiterals(m_pseudoLiterals);
  }
  m_pseudoBases.push_back(m_pseudoLiterals.size());

  // Ask the OverloadSet for a Method with a matching signature.
  Method *me = nullptr;
  m_formalTypes.resize(m_arguments.size());
  m_passThrough.resize(m_arguments.size());

  if (m_functor) {
    auto osn = safe_cast<OverloadSetRef *>(m_functor);
    bool constReceiver = osn->m_receiver && osn->m_receiver->m_type == ct_const;
    me = osn->m_entity->
        Lookup(this, m_argEntities, m_returnValue, constReceiver);
    if (!me) {
      EmitError(this) << "Method signature not found.";
      goto error;
    } else if (me->MethodKind() == mk_conflicted) {
      EmitError(this) << "Method signature is conflicted.";
      // FIXME: need a way better message -- include list of inherited
      // methods, for example.
      goto error;
    }

    m_method = me;
    m_receiver = osn->m_receiver;
    osn->m_receiver = nullptr;

    if (m_method->MethodKind() == mk_macro)
      verify(false);

    // Retrieve information for receiver and arguments.
    if (m_receiver) {
      m_receiverType = m_argEntities[0]->m_type;
      m_receiverType = m_receiverType.CopyConst(m_receiver->m_type);
    }

    for (size_t i = 0; i < m_arguments.size(); i++) {
      size_t j = i + (m_receiver != nullptr);
      if (j >= m_argEntities.size()) {
        verify(me->Variadic() == vt_c);
        Type t = m_arguments[i]->m_type.RValue();
        if (t == tk_pseudo) {
          m_arguments[i] = m_arguments[i]->LowerPseudos(Type(), true);
          t = m_arguments[i]->m_type;
        } else if (t == Type::Float()) {
          t = Type::Double();
        }
        m_formalTypes[i] = Type::Join(t, t); // FIXME: should widen?
      } else {
        Argument *ae = m_argEntities[j];
        Type t = ae->m_type;
        if (ae->m_mechanism != am_in)
          t = t.LValueRef();
        m_formalTypes[i] = t;
      }
    }

    m_type = m_returnValue ? m_returnValue->m_type : Type::Void();

  } else if (m_macro) {
    if (m_receiver)
      m_receiverType = m_receiver->m_type;
    m_type = m_macro->ResolveTypes(this, ctx);

    // Check for immediate expansion; it's requested if the above did not set
    // a type for this node.
    if (!m_type.IsKnown()) {
      if (!WrapArguments(ctx, false))
        goto error;
      return m_macro->Run(this, ctx)->ResolveType(ctx);
    } else if (m_type == Type::Suppress()) {
      EmitError(this) << "Method signature not found.";
      goto error;
    }
  }

  m_pseudoLiterals.clear();
  m_pseudoBases.clear();
  return this;

error:
  m_pseudoLiterals.clear();
  m_pseudoBases.clear();
  m_type = Type::Suppress();
  ctx.m_error = true;
  return this;
}

static bool IsSimpleReceiver2(Node *e) {
  switch (e->Kind()) {
    case nk_GlobalAddr:
    case nk_VarAddr:
      return true;
    case nk_FieldAddr:
      return IsSimpleReceiver2(safe_cast<FieldAddr *>(e)->m_address);
    default:
      return false;
  }
}

static bool IsSimpleReceiver(Node *e) {
  if (e->Kind() != nk_Load)
    return false;
  return IsSimpleReceiver2(safe_cast<Load *>(e)->m_address);
}

static bool IsPolymorphic(Method *me, Node *rcvr) {
  auto in = safe_cast<Initializer *>(rcvr);
  rcvr = in->m_replacement;

  if (me && me->MethodKind() != mk_macro) {
    if (me->IsVirtual()) {
      // The method may be virtual, that doesn't mean we'll do a virtual call.
      // See if we statically know the type of the receiver.
      switch (rcvr->Kind()) {
        case nk_VarAddr:
        case nk_FieldAddr:
        case nk_GlobalAddr:
        case nk_Index:
          // The address is of a piece of memory of a specific size and type,
          // and hence cannot be polymorphic.
          return false;
          break;
        default:
          break;
      }
      // FIXME: also consider final attribute, on both method and class.
      // FIXME: also base class references, though that will probably use
      //        Field, and so is already covered above.

      return true;
    }
  }

  return false;
}

static Node *GetVtableForObject(Node *e) {
  Type t = e->m_type;
  verify(t == tk_pointer && t.BaseType() == tk_class);
  Class *ce = t.BaseType().Class();
  verify(ce->GetVtableSize() > 0);

  while (true) {
    int loc = ce->GetVtableLocation();
    if (loc < 0)
      break;

    BaseSpecifier *bs = ce->GetBase(loc);
    e = new FieldAddr(e->m_location, e->Deref(), bs->m_ord);
    ce = bs->m_baseClass;
  }

  e = new FieldAddr(e->m_location, e->Deref(), ce->GetBaseCount());
  return e->Deref();
}

Node *Apply::ResolveFully_(Context &ctx) {
  if (m_type == tk_valueless)
    return this;

  // We need to create a chain of VarDecls for the temporaries needed for
  // arguments.  These will eventually go above the Call node.
  // FIXME: these need to be placed at the "statement level", so that a
  // temporary returned as an rvalue reference remains valid long enough.
  vector<VarDecl *> temps;

  Context::PushVoid pv(ctx, false);

  // Prepare the functor.
  m_functor = nullptr;
  if (m_method && m_method->MethodKind() != mk_macro)
    m_functor = m_method->AsValue(m_location);

  if (!WrapArguments(ctx, true))
    return this;

  // Resolve method invocations.
  if (m_receiver) {
    bool isPoly = m_receiverType == tk_class &&
                  IsPolymorphic(m_method, m_receiver);
    if (isPoly)
      m_receiverType.Class()->ResolveVtable();

    // Unwrap Initializer, taking care of possible rvalue temp.
    auto in = safe_cast<Initializer *>(m_receiver);
    if (in->m_rvalueTemp) {
      temps.push_back(in->m_rvalueTemp);
      in->m_rvalueTemp = nullptr;
    }
    m_receiver = in->m_replacement;

    // Handle calling a method on a class.
    if (m_method &&
        m_method->MethodKind() != mk_macro &&
        m_receiverType == tk_class) {
      if (isPoly) {
        // If the receiver isn't a simple pointer expression, first save it
        // to a temp to avoid double evaluation.
        if (!IsSimpleReceiver(m_receiver)) {
          Var *newve = new Var(m_location, m_receiver->m_type);
          newve->TrackResolution();

          VarDecl *vd = new VarDecl(newve, nullptr, m_receiver,
                                    dm_leaves_full_expr);
          temps.push_back(vd);

          m_receiver = new VarAddr(m_location, newve);
          m_receiver = new Load(m_location, m_receiver->Deref());
        }

        // Fetch the method pointer from the vtable.
        Node *e = GetVtableForObject(m_receiver);
        m_functor = new VtableSlot(m_location, m_receiverType.Class(),
            e, m_method->GetVtblSlot());
        m_functor = m_functor->Deref();
      }

      // When calling an inherited method, the receiver must be upcast to the
      // correct pointer to class type.
      auto ce = safe_cast<Class *>(m_method->Parent());
      if (ce != m_receiverType.Class()) {
        vector<size_t> path;
        Class *r_ce = m_receiverType.Class();
        r_ce->IsSubclassOf(ce, &path);

        // FIXME: handle ambiguous inheritance.
        // FIXME: duplicate code in PointerCast node.
        for (size_t i = path.size(); i-- > 0; ) {
          size_t ord = path[i];
          BaseSpecifier *bs = r_ce->GetBase(ord);
          m_receiver = new FieldAddr(m_receiver->m_location,
                                     m_receiver->Deref(), bs->m_ord);
          r_ce = bs->m_baseClass;
        }
      }
    } else if (m_macro) {
    }
  }

  // Create temporaries as needed for arguments.
  for (size_t i = 0; i < m_arguments.size(); i++) {
    Node *&arg = m_arguments[i];
    auto mech = am_in;
    if (!m_macro) {
      size_t j = i + (m_receiver != nullptr);
      if (j < m_argEntities.size()) {
        Argument *ae = m_argEntities[j];
        mech = ae->m_mechanism;
      }
    }

    if (!m_passThrough[i]) {
      auto in = safe_cast<Initializer *>(arg);
      if (in->m_rvalueTemp) {
        temps.push_back(in->m_rvalueTemp);
        in->m_rvalueTemp = nullptr;
      }

      switch (mech) {
        case am_in:
          if (m_formalTypes[i].Lower().IsSimpleType()) {
            arg = in->m_replacement;
          } else {
            Var *ve = new Var(m_location, m_formalTypes[i].Lower());
            ve->TrackResolution();
            VarDecl *vd = new VarDecl(ve, nullptr, in, dm_leaves_full_expr);
            temps.push_back(vd);
            arg = new VarAddr(m_location, ve);
          }
          break;
        case am_construct:
          arg = in->m_replacement;
          break;
        default:
          verify(false);
      }
    }
  }

  Node *root = nullptr;
  if (m_macro) {
    // Handle native operators.
    root = m_macro->Run(this, ctx);
  } else if (m_method->MethodKind() == mk_macro) {
    verify(false);
  } else {
    // Handle regular functions, macros, and methods.
    if (m_returnValue) {
      if (m_returnValue->m_type == tk_class) {
        Class *ce = m_returnValue->m_type.Class();
        ce->ResolveLayout();
      }
    }

    // Lower to a Call node.  The argument list must be massaged to insert
    // implicit arguments.
    // FIXME: for now, do none of this.
    Call *cn = new Call(m_location, m_method, m_argEntities,
                        m_returnValue, m_functor->AddrOf(true));
    m_functor = nullptr;

    // Add "this" argument for class members.
    if (m_receiver) {
      cn->AppendArg(m_receiver);
      m_receiver = nullptr;
    }

    for (Node *e : m_arguments)
      cn->AppendArg(e);

    root = cn;
  }

  // Assemble the chain of tempoaries.
  for (size_t i = temps.size(); i > 0; ) {
    VarDecl *vd = temps[--i];
    vd->m_expr = root;
    root = vd;
  }

  return root->ResolveFully(ctx);
}

void Apply::ResetNativeOperator(NativeOperator *no, size_t argcnt) {
  m_macro = no;
  m_arguments.resize(argcnt);
  m_formalTypes.resize(argcnt);
  m_passThrough.resize(argcnt);
}

void Apply::VisitChildren(Visitor *v) {
  // The arguments are labeled ck_operand2 for now... once we know the
  // signature, we'll re-label them correctly.
  if (m_functor)
    v->Visit(m_functor, ck_operand1);
  for (size_t i = 0; i < m_arguments.size(); i++)
    if (m_arguments[i])
      v->Visit(m_arguments[i], ChildKind(ck_argument + i));
}

bool Apply::WrapArguments(Context &ctx, bool blockReplacement) {
  Context::PushVoid pv(ctx, false);
  bool err = ctx.m_error;

  // Wrap all arguments (and receiver) in Initializer nodes, so as to create
  // suitable values for the lowered call.
  for (size_t i = 0; i < m_arguments.size(); i++) {
    if (!m_passThrough[i]) {
      Node *e = m_arguments[i];
      e = new Initializer(e->m_location, m_formalTypes[i], e, blockReplacement);
      m_arguments[i] = e->ResolveFully(ctx);
    }
  }

  if (m_receiver) {
    Initializer *in = new Initializer(m_receiver->m_location, m_receiverType,
                                      m_receiver, blockReplacement);
    m_receiver = in->ResolveFully(ctx);
  }

  return !ctx.m_error || err;
}

bool Apply::Dump(BufferWriter &bw, int level, int maxlevel,
                 const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Apply", desc, descwidth))
    return true;

  if (m_method) {
    if (bw.Append(' '))
      return true;

    if (bw.Append(m_method->Name()))
      return true;
  } else if (m_macro) {
    if (bw.Append(" nm:", 4))
      return true;
    const char *name = m_macro->Name();
    if (bw.Append(name, strlen(name)))
      return true;
  }

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_functor && m_functor->Dump(bw, level, maxlevel, "func", 4))
      return true;
    if (m_receiver && m_receiver->Dump(bw, level, maxlevel, "rcvr", 4))
      return true;
    for (auto arg : m_arguments)
      if (arg && arg->Dump(bw, level, maxlevel, "arg", 4))
        return true;
  }

  return false;
}
