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

#include "Attributes.h"
#include "Expr.h"
#include "Literal.h"
#include "entity/Attribute.h"
#include "entity/AttributeList.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Value.h"

IMPLEMENT_NODE(Attributes)

Attributes::Attributes(AttributeList *al, Node *e)
    : Node(al->m_location),
      m_attributes(al),
      m_expr(e) { }

void Attributes::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_attributes;
  DF << m_expr;
}

Attributes::Attributes(Inflator &IF)
    : Node(IF) {
  IF >> m_attributes;
  IF >> m_expr;
}

Node *Attributes::ResolveType_(Context &ctx) {
  // Must resolve attributes now.  Nothing else happens if @build(false) is
  // present.
  m_attributes->ResolveFully();

  if (!m_attributes->GetBuildValue())
    return new Literal(m_expr->m_location, Value::New(Type::Void()));

  // FIXME: might want to push the attributes onto the context.
  m_expr = m_expr->ResolveType(ctx);
  m_type = m_expr->m_type;

  return this;
}

Node *Attributes::ResolveFully_(Context &ctx) {
  Context::PushVoid pv(ctx, false);
  // FIXME: might want to push the attributes onto the context.
  m_expr = m_expr->ResolveFully(ctx);
  return this;
}

void Attributes::VisitChildren(Visitor *v) {
  for (Attribute *a = m_attributes->m_first; a != 0; a = a->m_next) {
    Node *e = a->GetExpr()->Root();
    if (e)
      v->Visit(e, ck_operand2);
  }
  v->Visit(m_expr, ck_operand1);
}

bool Attributes::Dump(BufferWriter &bw, int level, int maxlevel,
                      const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Attributes", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  // FIXME: attributes list

  if (++level < maxlevel)
    return m_expr->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
