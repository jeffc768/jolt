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

#include "New.h"
#include "Apply.h"
#include "BuildPointer.h"
#include "Call.h"
#include "Cast.h"
#include "Construct.h"
#include "Expr.h"
#include "Ident.h"
#include "Literal.h"
#include "Load.h"
#include "Member.h"
#include "Sequence.h"
#include "VarAddr.h"
#include "VarDecl.h"
#include "entity/Class.h"
#include "entity/Var.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_NODE(New)

New::New(Location sl, AST::SafeArray<AST::MemberItem> args, Node *what)
    : Node(sl),
      m_what(what) {
  // Process arguments to opertor new method (these are the [args...]
  // immediately following the "new").
  for (auto &arg : args) {
    m_arguments.push_back(arg.m_value);
    verify(!arg.m_attrs);  // FIXME
  }

  // Set this up now, so that it can be bound to a global definition.  It might
  // not be used if it turns out there was a class member available.
  String *name = String::Get(wks_op_new);
  m_op_new_call = new Ident(m_location, name);

  // To avoid grammar ambiguities, everything following the "new" was parsed as
  // an expression.  There are two possibilites:
  //
  //   1) The expression yields a type value.  That is the type of the object
  //      that's allocated, and the type's default constructor will be invoked
  //      to initialize it.
  //
  //   2) The expression is a constructor call.  The type of the object being
  //      allocated is inferred from it.  That allocated memory is passed to
  //      the constructor call.
  //
  //   3) Everything else.  This is really no different from (2).  Instead of a
  //      constructor, there is a method whose return value will be copy con-
  //      structed into allocated memory.  The type of the return value is the
  //      type of the object being allocated.
  //
  // Which of the three it is cannot be known until "what" is resolved.
}

void New::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_what;
  DF << m_arguments;
  DF << m_op_new_call;

  verify(!m_size);
}

New::New(Inflator &IF)
    : Node(IF) {
  IF >> m_what;
  IF >> m_arguments;
  IF >> m_op_new_call;
}

Node *HandleDynamicArray(Node::Context &ctx, Node *e) {
  // Look for "type[size]" or "type[size](args)".  In either case, the root
  // of the expression must be an Apply.
  if (e->Kind() != nk_Apply)
    return nullptr;

  // Descend past explicit constructor invocation, if present.
  Apply *an = safe_cast<Apply *>(e);
  if (an->m_functor->Kind() == nk_Apply)
    an = safe_cast<Apply *>(an->m_functor);

  // Look for operator[].
  if (an->m_functor->Kind() != nk_Member)
    return nullptr;

  Member *mn = safe_cast<Member *>(an->m_functor);
  if (mn->m_member->Kind() != nk_Ident)
    return nullptr;

  if (safe_cast<Ident *>(mn->m_member)->m_identifier != wks_op_index)
    return nullptr;

  // The left-hand operand must yield a type value.
  mn->m_object = mn->m_object->ResolveType(ctx);
  if (!mn->m_object->m_type.IsSubtypeOf(Type::JType()))
    return nullptr;

  // And the right-hand operand must yield an integer value.
  if (an->m_arguments.size() != 1)
    return nullptr;
  Node *&size = an->m_arguments[0];
  size = size->ResolveType(ctx);
  if (size->m_type != tk_integer)
    return nullptr;

  // If the size is evaluable at compile-time, then it's not dynamic by
  // definition.  (Note: integer literals fail the test above, but that's
  // okay as by definition they'd fail this test anyway.
  Node::Context::PushVoid(ctx, false);
  size = size->ResolveFully(ctx);
  Expr *expr = new Expr(size);
  if (expr->IsEvaluable(false))
    return nullptr;

  // It's a dynamic array allocation.  Remove the size sub-expression from
  // the Apply node, turning it into a conformant array type constructor,
  // and return it here.
  Node *rv = size;
  an->m_arguments.clear();
  return rv;
}

Node *New::ResolveType_(Context &ctx) {
  // If a dynamically-sized array is being allocated, extract the size from
  // the type expression now, before doing anything else...
  m_size = HandleDynamicArray(ctx, m_what);

  // ...like typing what's being allocated.
  m_what = m_what->ResolveType(ctx);

  // If we're newing a type, evalaute it now.
  if (m_what->m_type.IsSubtypeOf(Type::JType())) {
    {
      Context::PushVoid pv(ctx, false);
      m_what = m_what->ResolveFully(ctx);
    }

    Expr *expr = new Expr(m_what);
    if (true /*FIXME*/ || expr->IsEvaluable(false)) {
      Type t = m_what->EvaluateType();
      auto rcvr = new Literal(m_location, Value::NewType(t));
      auto mn = new Member(m_location, rcvr, wks_op_apply, false);
      m_what = new Apply(mn, rcvr);
      m_what = m_what->ResolveType(ctx);
    } else {
      verify(false); // FIXME: compilation error
    }
  }

  m_type = m_what->m_type;
  if (m_type == tk_valueless) {
    // As we can't go any farther with new, just type the arguments and bail.
    // In the normal case, the Apply we replace ourselves with will type the
    // arguments.
    for (auto &arg : m_arguments) {
      arg = arg->ResolveType(ctx);
      if (arg->m_type == tk_valueless)
        m_type = Type::Join(m_type, arg->m_type);
    }

    return this;
  }

  // Prevent an explicit list of initial values from giving the array type a
  // fixed element count when it's a dynamic allocation.
  if (m_size) {
    m_type = Type::ArrayOf(m_type.ElementType());
    safe_cast<Apply *>(m_what)->m_type = m_type;
  }

  m_type = Type::PointerTo(m_type);
  return this;
}

Node *New::ResolveFully_(Context &ctx) {
  if (m_type == tk_valueless && m_type != Type::Transfers())
    return this;

  // Insert amount to allocate at the front of operator new's arguments.
  VarDecl *sizeTemp = nullptr;
  Type t = m_what->m_type;
  if (!m_size) {
    // As what is being new'ed has a statically known size, simply use the
    // storage size of the type (works for arrays also).
    m_size = new Literal(m_location, Value::NewInt(Type::SizeT(),
                                                   t.StorageSize()));
  } else {
    // Multiply the element size with the number of elements.
    size_t amt = t.ElementType().StorageSize();

    // The size needs to be used twice, once to compute the amount to allocate,
    // and again to create a fat pointer.  Define a temp to hold it.
    Var *ve = new Var(m_location, Type::SizeT());
    sizeTemp = new VarDecl(ve, nullptr, m_size, dm_never);
    ve->TrackResolution();
    m_size = new VarAddr(m_location, ve);
    m_size = new Load(m_location, m_size->Deref());

    if (amt >= 1) {
      Node *e = new Literal(m_location, Value::NewInt(Type::SizeT(), amt));
      Node *f = new Member(m_location, e, wks_op_mul, true);
      m_size = new Apply(f, e, m_size);
    }
  }

  // Size in bytes becomes first argument to operator new.
  m_arguments.insert(m_arguments.begin(), m_size);

  // Create call to operator new.  If a class instance or array of instances
  // is being allocated, check for the presence of an operator new method
  // in the metaclass.
#if 0
  if (m_constructor->m_type == tk_class) {
    // operator new must be static, so force lookup in metaclass.
    Class *ce = m_constructor->m_type.Class()->Metaclass();
    if (Entity *e = ce->GetEntity(name, ws_instance))
      verify(false); // FIXME: use static method we just found
  }
#endif

  // Use global operator new.
  m_op_new_call = new Apply(m_op_new_call, nullptr, &m_arguments);

  // TODO: generate code to do the following:
  //          * protect constructor with exception handler to free
  //            memory (once exceptions are implemented)

  Context::PushVoid pv(ctx, false);
  m_op_new_call = m_op_new_call->ResolveFully(ctx);

  if (m_type.IsThin()) {
    m_op_new_call = new Cast(m_what->m_location, m_type, m_op_new_call);
  } else {
    // Form fat pointer to conformant array.
    auto ve = safe_cast<Var *>(sizeTemp->m_entity);
    Node *size = new VarAddr(m_location, ve);
    size = new Load(m_location, size->Deref());
    Type tt = Type::ThinPointerTo(m_type.BaseType());
    m_op_new_call = new Cast(m_what->m_location, tt, m_op_new_call);
    m_op_new_call = new BuildPointer(m_type, m_op_new_call, size);
  }

  TempScope ts;
  ArgAddr addr(ts, m_op_new_call->m_location, m_op_new_call, m_type, dm_never);

  // Construct allocated memory.
  Node *e = new Construct(addr.GetAddr(), m_what);

  // Return pointer to allocated memory after performing construction.
  e = new Sequence(e, addr.GetAddr(), false);
  e = ts.Get(e);
  if (!m_type.IsThin()) {
    sizeTemp->m_expr = e;
    e = sizeTemp;
  }

  return e->ResolveFully(ctx);
}

void New::VisitChildren(Visitor *v) {
  v->Visit(m_what, ck_operand1);

  for (size_t i = 0; i < m_arguments.size(); i++)
    v->Visit(m_arguments[i], ChildKind(ck_argument + i));

  if (m_size)
    v->Visit(m_size, ck_operand1);

  v->Visit(m_op_new_call, ck_operand2);
}

bool New::Dump(BufferWriter &bw, int level, int maxlevel,
               const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "New", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_what->Dump(bw, level, maxlevel, "what", 4))
      return true;
    if (m_size && m_size->Dump(bw, level, maxlevel, "size", 4))
      return true;
    for (auto arg : m_arguments)
      if (arg->Dump(bw, level, maxlevel, "arg", 4))
        return true;
  }

  return false;
}
