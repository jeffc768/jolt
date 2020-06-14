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

#include "Deref.h"
#include "AddrOf.h"
#include "ExtractAddress.h"
#include "FieldAddr.h"
#include "Index.h"
#include "VarAddr.h"
#include "entity/Var.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(Deref)

void Deref::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_expr;
}

Deref::Deref(Inflator &IF)
    : Node(IF) {
  IF >> m_expr;
}

static bool IsRvalueRef(Node *e) {
  if (!e)
    return false;

  switch (e->Kind()) {
    case nk_VarAddr:
      return safe_cast<VarAddr *>(e)->m_var->m_isTemp;
    case nk_FieldAddr:
      return IsRvalueRef(safe_cast<FieldAddr *>(e)->m_address);
    case nk_Index:
      return IsRvalueRef(safe_cast<Index *>(e)->m_address);
    default:
      return false;
  }
}

static Type ComputeType(Node *e) {
  Type t = e->m_type.BaseType().LValueRef();
  if (IsRvalueRef(e))
    t = t.RValueRef();
  return t;
}

Deref::Deref(Location sl, Node *e)
    : Node(sl),
      m_expr(e) {
  if (e->IsFullyResolved()) {
    m_type = ComputeType(m_expr);
    m_state = st_fully_resolved;
  }
}

Node *Deref::ResolveType_(Context &ctx) {
  m_expr = m_expr->ResolveType(ctx);
  m_type = ComputeType(m_expr);
  return this;
}

Node *Deref::ResolveFully_(Context &ctx) {
  m_expr = m_expr->ResolveFully(ctx);
  return this;
}

void Deref::VisitChildren(Visitor *v) {
  v->Visit(m_expr, ck_operand1);
}

bool Deref::Dump(BufferWriter &bw, int level, int maxlevel,
                 const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Deref", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel)
    return m_expr->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
