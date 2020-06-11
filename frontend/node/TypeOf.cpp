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

#include "TypeOf.h"
#include "Literal.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Value.h"

IMPLEMENT_NODE(TypeOf)

TypeOf::TypeOf(Location sl, Node *arg)
    : Node(sl),
      m_expr(arg) { }

void TypeOf::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_expr;
}

TypeOf::TypeOf(Inflator &IF)
    : Node(IF) {
  IF >> m_expr;
}

Node *TypeOf::ResolveType_(Context &ctx) {
  m_expr = m_expr->ResolveType(ctx);
  m_type = m_expr->m_type == tk_valueless ? m_expr->m_type : Type::JType();
  return this;
}

Node *TypeOf::ResolveFully_(Context &ctx) {
  Context::PushVoid pv(ctx, false);
  m_expr = m_expr->ResolveFully(ctx);

  if (m_type == tk_valueless)
    return this;

  // FIXME: is it correct to drop reference qualifiers?  If not, then $var will
  // always return an l-value type, which seems unexpected.
  Type t = m_expr->m_type.RValue();
  return new Literal(m_location, Value::NewType(t));
}

void TypeOf::VisitChildren(Visitor *v) {
  v->Visit(m_expr, ck_operand1);
}

bool TypeOf::Dump(BufferWriter &bw, int level, int maxlevel,
                  const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "TypeOf", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel)
    return m_expr->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
