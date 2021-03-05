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

#include "Transfer.h"
#include "Label.h"
#include "Literal.h"
#include "parser/ParserDecls.h"
#include "parser/Token.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_NODE(Transfer)

Transfer::Transfer(Location sl, TransferKind tk, Node *value, Token *lbl,
                   Node *guard)
    : Node(sl),
      m_kind(tk),
      m_expr(value ? value : new Literal(sl, Value::New(Type::Void()))),
      m_guard(guard) {
  if (lbl)
    m_labelName = lbl->StringValue();
}

Transfer::Transfer(TransferKind tk, Node *value)
    : Node(value->m_location),
      m_kind(tk),
      m_expr(value) {
  verify(value);
}

void Transfer::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_kind;
  DF << m_label;
  DF << m_labelName;
  DF << m_expr;
  DF << m_guard;
  DF << m_nonlocal;
  DF << m_exprType;
}

Transfer::Transfer(Inflator &IF)
    : Node(IF) {
  IF >> m_kind;
  IF >> m_label;
  IF >> m_labelName;
  IF >> m_expr;
  IF >> m_guard;
  IF >> m_nonlocal;
  IF >> m_exprType;
}

Node *Transfer::ResolveType_(Context &ctx) {
  m_expr = m_expr->ResolveType(ctx);
  if (m_guard)
    m_guard = m_guard->UseCondition(ctx);

  verify(m_label); // FIXME: proper error

  ComputeType();

  // Now we can register ourselves with the Label node.
  // FIXME: consider using Context for this.
  if (m_exprType != Type::Transfers())
    m_label->AddTransfer(this);

  return this;
}

Node *Transfer::ResolveFully_(Context &ctx) {
  Context::PushVoid pv(ctx, false);

  if (m_guard)
    m_guard = m_guard->ResolveFully(ctx);

  m_expr = m_expr->ResolveFully(ctx);
  return this;
}

void Transfer::ComputeType() {
  // Our type is the type of m_expr.  Note that we have two types, the one
  // seen by our direct parent (usually valueless) and the one seen by the
  // statement we are exiting.
  m_exprType = m_expr->m_type;

  if (!m_guard)
    m_type = Type::Transfers();
  else if (m_guard->m_type == Type::Transfers())
    m_type = Type::Transfers();
  else
    m_type = Type::Void();
}

void Transfer::BindNames(SymbolTable &st, Visitor *v) {
  auto lr = st.LookupLabel(m_labelName, m_kind);
  verify(!lr.m_label || !lr.m_nonlocal); // FIXME
  m_label = lr.m_label;

  VisitChildren(v);
}

void Transfer::VisitChildren(Visitor *v) {
  v->Visit(m_expr, ck_operand1);

  if (m_guard)
    v->Visit(m_guard, ck_operand2);
}

bool Transfer::Dump(BufferWriter &bw, int level, int maxlevel,
                    const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Transfer", desc, descwidth))
    return true;

  switch (m_kind) {
    case tk_exit:   bw.Append(" exit");   break;
    case tk_next:   bw.Append(" next");   break;
    case tk_return: bw.Append(" return"); break;
  }

  if (m_labelName) {
    if (bw.Append(' '))
      return true;
    if (bw.Append(m_labelName))
      return true;
  }

  if (m_nonlocal)
    if (bw.Append(" [nonlocal]", 11))
      return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_guard && m_guard->Dump(bw, level, maxlevel, "guard", 5))
      return true;
    if (m_expr->Dump(bw, level, maxlevel, "value", 5))
      return true;
  }

  return false;
}
