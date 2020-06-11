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

#include "VarDecl.h"
#include "Apply.h"
#include "Construct.h"
#include "Deref.h"
#include "Expr.h"
#include "FieldAddr.h"
#include "Ident.h"
#include "Initializer.h"
#include "Literal.h"
#include "Member.h"
#include "VarAddr.h"
#include "entity/Argument.h"
#include "entity/AttributeList.h"
#include "entity/Field.h"
#include "entity/Var.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_NODE(VarDecl)

Node *VarDecl::DONT_INIT = reinterpret_cast<Node *>(1);
Node *VarDecl::USE_FIELD_INIT = reinterpret_cast<Node *>(1);

VarDecl::VarDecl(Node *field, Node *e, Node *init, DestructMode dm)
    : Node(field->m_location),
      m_expr(e),
      m_field(field),
      m_destructMode(dm) {
  if (dm == dm_is_destructor) {
    verify(!init);
    return;
  }

  if (init != USE_FIELD_INIT)
    SetInitExpr(init);
}

VarDecl::VarDecl(Node *field, Node *e, Ident *cons,
                 AST::SafeArray<AST::MemberItem> args, DestructMode dm)
    : Node(field->m_location),
      m_expr(e),
      m_field(field),
      m_destructMode(dm) {
  Location loc = cons ? cons->m_location : m_location;
  m_initializer = new Initializer(loc, cons, args);
  m_initexpr = m_initializer;
  m_initializer->BlockReplacement();
}

VarDecl::VarDecl(Var *ve, Node *e, Node *init, DestructMode dm)
    : Node(ve->GetLocation()),
      m_expr(e),
      m_entity(ve),
      m_destructMode(dm) {
  SetInitExpr(init);
  ve->m_vardecl = this;

  if (dm == dm_leaves_full_expr)
    ve->m_isTemp = true;

  if (ve->VarKind() == vk_local) {
    if (!ve->m_type.IsKnown() && !ve->GetTypeExpr()) {
      m_initializer->SetAutoType();
      m_autoType = true;
    }
  }
}

void VarDecl::SetInitExpr(Node *e) {
  // FIXME: just use void() as init expression.
  if (e == DONT_INIT) {
    m_initexpr = new Literal({ }, Value::New(Type::Void()));
    m_dontInitialize = true;
    return;
  }

  m_initializer = new Initializer(e ? e->m_location : m_location,
                                           nullptr, e);
  m_initexpr = m_initializer;
  m_initializer->BlockReplacement();
}

void VarDecl::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_initexpr;
  DF << m_expr;
  DF << m_field;
  DF << m_entity;
  DF << m_destructMode;
  DF << m_destructor;
  DF << m_constructorMode;
  DF << m_generated;
  DF << m_initializer;
  DF << m_dontInitialize;
  DF << m_autoType;

  // This should never be not-null by the time it's deflated, regardless of
  // being fully resolved or initial state.
  verify(!m_rvalueTemp);
}

VarDecl::VarDecl(Inflator &IF)
    : Node(IF) {
  IF >> m_initexpr;
  IF >> m_expr;
  IF >> m_field;
  IF >> m_entity;
  IF >> m_destructMode;
  IF >> m_destructor;
  IF >> m_constructorMode;
  IF >> m_generated;
  IF >> m_initializer;
  IF >> m_dontInitialize;
  IF >> m_autoType;

  if (Var *ve = dyn_cast<Var *>(m_entity))
    ve->m_vardecl = this;
}

Node *VarDecl::ResolveType_(Context &ctx) {
  if (m_field) {
    if (!m_constructorMode) {
      EmitError(this) << "Construct/destruct cannot be nested within "
                         "conditional or looping constructs, nor introduced "
                         "by a macro expansion.";
      ctx.m_error = true;
      return m_expr;
    }

    if (m_generated)
      if (Node *e = ResolveField(ctx); e != this)
        return e->ResolveType(ctx);

    if (!m_initexpr && m_destructMode != dm_is_destructor)
      m_initexpr = safe_cast<Field *>(m_entity)->GetInitializer();
  } else {
    if (AttributeList *al = m_entity->Attributes())
      if (!al->GetBuildValue())
        return m_expr->ResolveType(ctx);
  }

  if (m_initexpr) {
    m_initexpr = m_initexpr->ResolveType(ctx);
    if (m_initexpr->m_type == tk_valueless) {
      m_type = Type::Join(m_type, m_initexpr->m_type);
    } else if (m_autoType) {
      Context::PushVoid pv(ctx, false);
      m_initexpr = m_initexpr->ResolveFully(ctx);
      if (m_initexpr->Kind() == nk_Initializer)
        m_initializer = safe_cast<Initializer *>(m_initexpr);

      if (auto ve = dyn_cast<Var *>(m_entity))
        ve->m_type = m_initializer->m_deducedType;
      else
        verify(false);

      m_autoType = false;
    }
  }

  // Resolve type of statement list following this declaration.
  if (m_expr) {
    m_expr = m_expr->ResolveType(ctx);
    m_type = m_expr->m_type;
  } else {
    m_type = Type::Void();
  }

  return this;
}

Node *VarDecl::ResolveFully_(Context &ctx) {
  bool save_void = ctx.m_void;
  if (save_void)
    m_type = Type::Void();

  Context::PushVoid pv(ctx, false);

  // Wait for Var to resolve the type--but not for deduced return types.
  Type vt = Type::Void();
  if (auto ae = dyn_cast<Argument *>(m_entity)) {
    if (!ae->m_isDeducedType) {
      ae->ResolveFully();
      vt = VariableType();
    }
  } else {
    m_entity->ResolveFully();
    vt = VariableType();
  }

  if (vt == Type::Suppress())
    ctx.m_error = true;

  if (m_initializer && m_initializer->NeedsType())
    m_initializer->SetType(vt);

  if (vt == tk_valueless)
    m_type = Type::Join(m_type, vt);

  // Create destructor for use when this variable goes out of scope, for
  // whatever reason.
  if (vt.HasDestructor()) {
    Node *addr = m_field;
    if (!addr) {
      auto ve = safe_cast<Var *>(m_entity);
      addr = (new VarAddr(ve->GetLocation(), ve))->Deref();
    }
    Node *f = new Member(m_location, addr, wkhs_destructor, false);
    m_destructor = new Apply(f, addr);
    m_destructor = m_destructor->ResolveFully(ctx);
  }

  // For destructions, create an "initializer" that does the deed.
  if (!m_initexpr) {
    verify(m_destructMode == dm_is_destructor);

    if (vt.HasDestructor())
      m_initexpr = m_destructor;
    else
      m_initexpr = new Literal(m_location, Value::New(Type::Void()));
  }

  m_initexpr = m_initexpr->ResolveFully(ctx);
  if (m_initexpr->Kind() == nk_Initializer)
    m_initializer = safe_cast<Initializer *>(m_initexpr);

  if (m_initexpr->m_type == tk_valueless) {
    Type t = m_initexpr->m_type;
    if (t == Type::NotTyped()) {
      t = Type::Suppress();
      EmitError(m_initexpr) << "Cannot determine type of initializer.";
      ctx.m_error = true;
    }

    // If the initializer transfers, then force the VarDecl node to be
    // transfers.  The normal join rules favor a "real" type over transfers,
    // the only time a valueless type doesn't win over a "real" type.
    if (t == Type::Transfers() && m_type != tk_valueless)
      m_type = t;
    else
      t = Type::Join(t, m_type);
    // FIXME: must propagate valueless type from m_rvalueTemp, which is set
    // below.  Perhaps have Initializer node propagate it to m_deducedType.
  }

  {
    Context::PushVoid pv2(ctx, save_void);
    if (m_expr)
      m_expr = m_expr->ResolveFully(ctx);
  }

  // If this is a return value with a simple type, then the initializer is
  // simply returned as the result of the method.
  // FIXME: still needed?
  if (m_entity->Kind() == ek_Argument) {
    auto ae = safe_cast<Argument *>(m_entity);
    if (ae->m_mechanism == am_out) {
      if (ae->m_type.IsSimpleType()) {
        m_initializer = nullptr;
        return this;
      }
    }
  }

  Node *init = m_initexpr;

  // If the initializer needed to create a temp, handle it now.
  if (m_initializer)
    init = m_initializer->m_replacement;

  if (m_initializer && m_initializer->m_rvalueTemp) {
    // This is ugly.  If an rvalue was used to initialize a reference, then a
    // temp was created.  That temp must be promoted to have the same lifetime
    // as us, which means it must be constructed before us and destructed after
    // us.  Bottom line: that VarDecl must be hoisted above us in the expr
    // tree, and there's no pretty way to do that.
    //
    // So put the extra temp aside for now and do the hoisting later, just
    // before this expression tree is handed off to a target.
    m_rvalueTemp = m_initializer->m_rvalueTemp;
    m_rvalueTemp = safe_cast<VarDecl *>(m_rvalueTemp->ResolveFully(ctx));

    // FIXME: This should be a compilation error; a field of reference type
    // cannot be initialized with an rvalue.  There's no place to put the
    // temp.
    verify(!m_field);
  }

  if (init->m_type != tk_valueless) {
    Node *addr = m_field ? m_field->AddrOf(true)
                         : new VarAddr(m_location, safe_cast<Var *>(m_entity));
    m_initexpr = new Construct(addr, init);
    m_initexpr = m_initexpr->ResolveFully(ctx);
  }

  m_initializer = nullptr;
  return this;
}

void VarDecl::VisitChildren(Visitor *v) {
  if (m_initexpr)
    v->Visit(m_initexpr, ck_initializer);

  if (m_expr)
    v->Visit(m_expr, ck_operand1);

  if (m_entity && m_entity->Kind() == ek_Var) {
    auto ve = safe_cast<Var *>(m_entity);
    if (Expr *te = ve->GetTypeExpr())
      v->Visit(te->Root(), ck_type);
  }

  if (m_field)
    v->Visit(m_field, ck_operand2);

  if (m_destructor)
    v->Visit(m_destructor, ck_operand1);
}

void VarDecl::SetLegalConstructor() {
  verify(m_state == st_initial);
  m_constructorMode = true;
}

void VarDecl::SetGenerated() {
  m_generated = true;
  SetLegalConstructor();
}

Node *VarDecl::ResolveField(Context &ctx) {
  // FIXME: handle non-built fields and constructor statements.

  Context::PushVoid pv(ctx, false);
  m_field = m_field->ResolveFully(ctx);

  Node *e = nullptr;
  if (m_field->Kind() == nk_Deref)
    e = safe_cast<::Deref *>(m_field)->m_expr;

  Field *f = nullptr;
  if (e && e->Kind() == nk_FieldAddr) {
    f = safe_cast<FieldAddr *>(e)->m_entity;
  } else {
    // FIXME: verify field is a member of this class
    // FIXME: permit base classes
    EmitError(this) << "Attempt to construct/destruct something that is not a "
                       "field or base of this class.";
    ctx.m_error = true;
    return m_expr;
  }

  m_entity = f;
  m_entity->TrackResolution();
  return this;
}

Type VarDecl::VariableType() {
  switch (m_entity->Kind()) {
    case ek_Argument:
    case ek_Var:
      break;
    case ek_Field:
      return safe_cast<Field *>(m_entity)->GetType();
    default:
      verify(false);
      return Type();
  }

  Type t = safe_cast<Var *>(m_entity)->m_type;
  return t == rt_rvalueref ? t.LValueRef() : t;
}

bool VarDecl::Dump(BufferWriter &bw, int level, int maxlevel,
                   const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "VarDecl", desc, descwidth))
    return true;

  if (bw.Append(' '))
    return true;

  if (bw.Append(m_entity->Name()))
    return true;

  switch (m_destructMode) {
    case dm_invalid:          bw.Append(" [dm invalid]", 13);           break;
    case dm_leaves_scope:     bw.Append(" [dm leaves scope]", 18);      break;
    case dm_leaves_full_expr: bw.Append(" [dm leaves full expr]", 22);  break;
    case dm_not_on_return:    bw.Append(" [dm not on return]", 19);     break;
    case dm_is_destructor:    bw.Append(" [dm is destructor]", 19);     break;
    case dm_never:            bw.Append(" [dm never]", 11);             break;
  }

  if (m_constructorMode && bw.Append(" [constructor]", 14))
    return true;

  if (m_generated && bw.Append(" [generated]", 12))
    return true;

  if (m_dontInitialize && bw.Append(" [dont init]", 12))
    return true;

  if (m_autoType && bw.Append(" [auto type]", 12))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_initexpr && m_initexpr->Dump(bw, level, maxlevel, "init", 5))
      return true;
    if (m_destructor && m_destructor->Dump(bw, level, maxlevel, "destr", 5))
      return true;
    if (m_field && m_field->Dump(bw, level, maxlevel, "field", 5))
      return true;
    if (m_expr->Dump(bw, level, maxlevel, "expr", 5))
      return true;
  }

  return false;
}
