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

#include "While.h"
#include "Label.h"
#include "Literal.h"
#include "Transfer.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Value.h"

IMPLEMENT_NODE(While)

While::While(Location sl, Node *cond, Node *body, Node *els, Node *next)
    : Node(sl),
      m_condition(cond),
      m_body(body),
      m_next(next) {
  if (els)
    m_else = els;
  else
    m_else = new Literal(sl, Value::New(Type::Void()));
}

void While::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_condition;
  DF << m_body;
  DF << m_next;
  DF << m_else;
  DF << m_label;
}

While::While(Inflator &IF)
    : Node(IF) {
  IF >> m_condition;
  IF >> m_body;
  IF >> m_next;
  IF >> m_else;
  IF >> m_label;
}

Node *While::ResolveType_(Context &ctx) {
  m_body = m_body->ResolveType(ctx);
  m_else = m_else->ResolveType(ctx);
  m_condition = m_condition->UseCondition(ctx);

  if (m_next)
    m_next = m_next->ResolveType(ctx);

  ComputeType();
  return this;
}

Node *While::ResolveFully_(Context &ctx) {
  if (ctx.m_void)
    m_type = Type::Void();

  {
    Context::PushVoid pv(ctx, false);
    m_condition = m_condition->ResolveFully(ctx);
  }

  {
    Context::PushVoid pv(ctx, true);
    m_body = m_body->ResolveFully(ctx);

    if (m_next)
      m_next = m_next->ResolveFully(ctx);
  }

  {
    Context::PushVoid pv(ctx, m_type == tk_void);
    m_else = m_else->ResolveFully(ctx);
  }

  return this;
}

// FIXME: need to add new states to handle the lowering from Jolt semantics,
// in particular handling of types.

void While::ComputeType() {
  if (m_type == tk_void)
    return;

  // Our type is the join of the else clause and any exit transfers.
  // Propagate valueless types from m_condition.
  // FIXME: this can get very messy.. an exit in the condition is not the
  // same as an exit in the body.
  Type t = m_condition->m_type == tk_valueless ? m_condition->m_type : Type();

  m_type = DetermineStatementType(t, { &m_else }, m_label, true);
}

void While::VisitChildren(Visitor *v) {
  v->Visit(m_condition, ck_condition);
  v->Visit(m_body, ck_operand1);

  if (m_next)
    v->Visit(m_next, ck_operand1);

  v->Visit(m_else, ck_operand2);
}

bool While::Dump(BufferWriter &bw, int level, int maxlevel,
                 const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "While", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_condition->Dump(bw, level, maxlevel, "cond", 4))
      return true;
    if (m_body->Dump(bw, level, maxlevel, "body", 4))
      return true;
    if (m_next && m_next->Dump(bw, level, maxlevel, "next", 4))
      return true;
    if (m_else && m_else->Dump(bw, level, maxlevel, "else", 4))
      return true;
  }

  return false;
}
