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

#include "Block.h"
#include "Label.h"
#include "Sequence.h"
#include "VarDecl.h"
#include "entity/Scope.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(Block)

Block::Block(Scope *s, Node *e)
    : Node(e->m_location),
      m_scope(s),
      m_expr(e) { }

void Block::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_scope;
  DF << m_label;
  DF << m_expr;
}

Block::Block(Inflator &IF)
    : Node(IF) {
  IF >> m_scope;
  IF >> m_label;
  IF >> m_expr;
}

Node *Block::ResolveType_(Context &ctx) {
  m_scope->HandleNameMacros();
  m_expr = m_expr->ResolveType(ctx);
  ComputeType();
  return this;
}

// FIXME: Consider switching to the Context to handle the hoisting.
static Node *Hoist(Node *e) {
  switch (e->Kind()) {
    case nk_Sequence: {
      auto sn = safe_cast<Sequence *>(e);
      sn->m_second = Hoist(sn->m_second);
      verify(sn->m_valueIs != Sequence::vi_first);
      return e;
    }
    case nk_VarDecl: {
      auto vn = safe_cast<VarDecl *>(e);
      vn->m_expr = Hoist(vn->m_expr);
      if (!vn->m_rvalueTemp)
        return e;
      VarDecl *temp = vn->m_rvalueTemp;
      vn->m_rvalueTemp = nullptr;
      temp->m_expr = vn;
      temp->m_type = vn->m_type;
      return temp;
    }
    default:
      return e;
  }
}

Node *Block::ResolveFully_(Context &ctx) {
  if (ctx.m_void)
    m_type = Type::Void();

  m_scope->ResolveFully();
  m_expr = m_expr->ResolveFully(ctx);

  // FIXME: if there are no nested methods or classes, this Block node can
  // remove itself from the tree.

  // Hoist any rvalue temps that might exist for the local variables scoped
  // to this block.
  m_expr = Hoist(m_expr);
  return this;
}

void Block::ComputeType() {
  if (m_type == tk_void)
    return;

  if (m_label)
    m_type = DetermineStatementType({ }, { &m_expr }, m_label);
  else
    m_type = m_expr->m_type;
}

void Block::VisitChildren(Visitor *v) {
  v->Visit(m_expr, ck_operand1);
}

bool Block::Dump(BufferWriter &bw, int level, int maxlevel,
                 const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Block", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel)
    return m_expr->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
