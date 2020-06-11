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

#include "MethodBody.h"
#include "Initializer.h"
#include "Label.h"
#include "Transfer.h"
#include "entity/Argument.h"
#include "entity/FormalArguments.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(MethodBody)

MethodBody::MethodBody(FormalArguments *formals, Node *body)
    : Node(body->m_location),
      m_formals(formals),
      m_body(body) { }

void MethodBody::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_body;
  DF << m_label;
  DF << m_formals;
}

MethodBody::MethodBody(Inflator &IF)
    : Node(IF) {
  IF >> m_body;
  IF >> m_label;
  IF >> m_formals;
}

Node *MethodBody::ResolveType_(Context &ctx) {
  m_body = m_body->ResolveType(ctx);

  if (m_formals->m_deducedReturnType) {
    Context::PushVoid pv(ctx, m_type == tk_void);
    m_body = m_body->ResolveFully(ctx);
    m_label->LowerPseudos({ });
    m_type = DetermineStatementType({ }, { &m_body }, m_label);
    // FIXME: may want lvalue in some cases
    m_formals->m_returnValue->SetType(m_type.RValue());
    m_formals->m_deducedReturnType = false;
  } else if (Argument *ae = m_formals->m_returnValue) {
    ae->ResolveFully();
    m_label->LowerPseudos(m_type);
    m_type = ae->m_type;
  } else {
    m_label->LowerPseudos(m_type);
    m_type = Type::Void();
  }

  return this;
}

Node *MethodBody::ResolveFully_(Context &ctx) {
  Context::PushVoid pv(ctx, m_type == tk_void);
  m_formals->ResolveFully();

  // Create Initializer nodes for everything, which will cast as needed.
  // Note: Initializer strips off a Deref when initializing a reference, so we
  // need to add it back.
  Type rt = m_formals->m_returnValue ? m_formals->m_returnValue->m_type
                                     : Type::Void();
  for (Transfer *tn : m_label->m_transfers) {
    tn->m_exprType = rt;
    tn->m_expr = new Initializer(tn->m_expr->m_location, rt, tn->m_expr,
                                 false, true);
    tn->m_expr = tn->m_expr->ResolveFully(ctx);
  }

  m_body = m_body->ResolveFully(ctx);
  return this;
}

// FIXME: if m_body has type Transfers, we may want to propagate it to
// callers, though that can get tricky and certainly can't be applied
// consistently.  Certainly must note Unbound.

void MethodBody::VisitChildren(Visitor *v) {
  v->Visit(m_body, ck_operand1);
}

bool MethodBody::Dump(BufferWriter &bw, int level, int maxlevel,
                      const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "MethodBody", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel)
    return m_body->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
