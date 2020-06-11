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

#include "Construct.h"
#include "Store.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(Construct)

Construct::Construct(Node *addr, Node *init)
    : Node(addr->m_location, Type::Void()),
      m_addr(addr),
      m_init(init) {
  verify(init && addr->IsFullyResolved());
}

void Construct::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_addr;
  DF << m_init;
}

Construct::Construct(Inflator &IF)
    : Node(IF) {
  IF >> m_addr;
  IF >> m_init;
}

Node *Construct::ResolveType_(Context &ctx) {
  m_init = m_init->ResolveType(ctx);
  return this;
}

Node *Construct::ResolveFully_(Context &ctx) {
  Context::PushVoid pv(ctx, false);
  m_init = m_init->ResolveFully(ctx);

  // If the initializer is a void literal, then suppress the construction.
  if (m_init->Kind() == nk_Literal && m_init->m_type == tk_void)
    return m_init;

  Type t = m_addr->m_type.BaseType();
  if (!t.IsSimpleType())
    return this;

  // For simple types, the initializer is going to resolve to a first-class
  // rvalue.  Replace ourselves with code to take that value and store it.
  return new Store(m_location, m_addr->Deref(), m_init);
}

void Construct::VisitChildren(Visitor *v) {
  v->Visit(m_addr, ck_operand1);
  v->Visit(m_init, ck_operand2);
}

bool Construct::Dump(BufferWriter &bw, int level, int maxlevel,
                     const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Construct", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_addr->Dump(bw, level, maxlevel, "addr", 4))
      return true;
    if (m_init->Dump(bw, level, maxlevel, "init", 4))
      return true;
  }

  return false;
}
