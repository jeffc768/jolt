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

#include "Label.h"
#include "Block.h"
#include "For.h"
#include "If.h"
#include "MethodBody.h"
#include "Transfer.h"
#include "VarDecl.h"
#include "While.h"
#include "entity/Parameters.h"
#include "parser/ParserDecls.h"
#include "parser/Token.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/String.h"

IMPLEMENT_NODE(Label)

Label::Label(Location sl, String *name, Node *e)
    : Node(sl),
      m_label(name),
      m_expr(e) {
  switch (e->Kind()) {
    case nk_Block:
    case nk_If:
    case nk_Switch:
      m_kind = lk_block;
      break;
    case nk_MethodBody:
      m_kind = lk_method;
      break;
    case nk_For:
    case nk_While:
      m_kind = lk_loop;
      break;
    default:
      verify(false);
  }
}

void Label::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_kind;
  DF << m_label;
  DF << m_expr;
  DF << m_templateRoot;
  DF << m_transfers;
}

Label::Label(Inflator &IF)
    : Node(IF) {
  IF >> m_kind;
  IF >> m_label;
  IF >> m_expr;
  IF >> m_templateRoot;
  IF >> m_transfers;
}

void Label::AddTransfer(Transfer *tn) {
  m_transfers.push_back(tn);
}

Node *Label::ResolveType_(Context &ctx) {
  // FIXME: consider handling this via Context
  switch (m_expr->Kind()) {
    case nk_Block:
      safe_cast<Block *>(m_expr)->m_label = this;
      break;
    case nk_MethodBody:
      safe_cast<MethodBody *>(m_expr)->m_label = this;
      break;
    case nk_For:
      safe_cast<For *>(m_expr)->m_label = this;
      break;
    case nk_If:
      safe_cast<If *>(m_expr)->m_label = this;
      break;
    case nk_While:
      safe_cast<While *>(m_expr)->m_label = this;
      break;
    default:
      verify(false);
  }

  m_expr = m_expr->ResolveType(ctx);
  m_type = m_expr->m_type;

  // A lowered for node will put vardecls above the resulting while node.
  // We must put ourselves below those vardecls, and next to the while node.
  if (m_expr->Kind() != nk_VarDecl)
    return this;

  Node *top = m_expr;
  VarDecl *above = nullptr;
  while (auto vd = dyn_cast<VarDecl *>(m_expr)) {
    above = vd;
    m_expr = vd->m_expr;
  }

  above->m_expr = this;
  return top;
}

Node *Label::ResolveFully_(Context &ctx) {
  if (ctx.m_void)
    m_type = Type::Void();

  m_expr = m_expr->ResolveFully(ctx);

  // Because the transfer value might be modified after the Transfer node has
  // fully resolved, due to type casting and r-value conversions, the
  // Transfer node can't wait for it to become resolved.  Must hanndle it here.
  for (auto tn : m_transfers)
    tn->m_expr = tn->m_expr->ResolveFully(ctx);

  return this;
}

void Label::VisitChildren(Visitor *v) {
  v->Visit(m_expr, ck_operand1);
}

Parameters **Label::GetTemplateRootHolder() {
  return &static_cast<Parameters *&>(m_templateRoot);
}

bool Label::Dump(BufferWriter &bw, int level, int maxlevel,
                 const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Label", desc, descwidth))
    return true;

  if (m_label) {
    if (bw.Append(' '))
      return true;

    if (bw.Append(m_label))
      return true;
  }

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel)
    return m_expr->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
