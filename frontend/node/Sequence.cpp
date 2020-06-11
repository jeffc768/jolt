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

#include "Sequence.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(Sequence)

Sequence::Sequence(Node *e1, Node *e2, bool vif)
    : Node(e1->m_location),
      m_first(e1),
      m_second(e2),
      m_valueIs(vif ? vi_first : vi_second) { }

Sequence::Sequence(Location sl, Node *e1, Node *e2, bool vif)
    : Node(sl),
      m_first(e1),
      m_second(e2),
      m_valueIs(vif ? vi_first : vi_second) { }

void Sequence::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_first;
  DF << m_second;
  DF << WX(m_valueIs);
}

Sequence::Sequence(Inflator &IF)
    : Node(IF) {
  IF >> m_first;
  IF >> m_second;
  IF >> WX(m_valueIs);
}

Node *Sequence::ResolveType_(Context &ctx) {
  m_first = m_first->ResolveType(ctx);
  m_second = m_second->ResolveType(ctx);
  ComputeType();
  return this;
}

// FIXME: this should probably be moved to Node and made into a proper Visitor.
// This can only handle direct children that themselves have no Node children.
static bool IsEmpty(Node *e) {
  switch (e->Kind()) {
    case nk_GlobalAddr:
    case nk_Literal:
    case nk_LiteralAddr:
    case nk_VarAddr:
    case nk_Vtable:
      return true;
    default:
      return false;
  }
}

Node *Sequence::ResolveFully_(Context &ctx) {
  if (ctx.m_void) {
    m_type = Type::Void();
    m_valueIs = vi_neither;
  }

  {
    Context::PushVoid pv(ctx, m_valueIs != vi_first);
    m_first = m_first->ResolveFully(ctx);
  }

  {
    Context::PushVoid pv(ctx, m_valueIs != vi_second);
    m_second = m_second->ResolveFully(ctx);
  }

  if (m_type == Type::Transfers()) {
    // m_valueIs is irrelevant, as a value is never passed up to the
    // parent.  If the first transfers, the second never executes; discard it.
    // If it doesn't, we know the second does.  The first may have side-effects,
    // so it cannot be discarded unless it's empty.
    if (m_type == m_first->m_type)
      return m_first;
    else if (IsEmpty(m_first))
      return m_second;
    else
      return this;
  } else {
    // The one whose value is not used can be discarded if it's empty.
    if (m_valueIs == vi_first) {
      if (IsEmpty(m_second))
        return m_first;
    } else {
      if (IsEmpty(m_first))
        return m_second;
    }
  }

  return this;
}

void Sequence::ComputeType() {
  if (m_valueIs == vi_first) {
    m_type = m_first->m_type;
    if (m_second->m_type == Type::Transfers())
      m_type = Type::Transfers();
  } else if (m_valueIs == vi_second) {
    m_type = m_second->m_type;
    if (m_first->m_type == Type::Transfers())
      m_type = Type::Transfers();
  } else {
    if (m_first->m_type == Type::Transfers())
      m_type = Type::Transfers();
    else if (m_second->m_type == Type::Transfers())
      m_type = Type::Transfers();
    else
      m_type = Type::Void();
  }
}

void Sequence::VisitChildren(Visitor *v) {
  v->Visit(m_first, ck_operand1);
  v->Visit(m_second, ck_operand2);
}

bool Sequence::Dump(BufferWriter &bw, int level, int maxlevel,
                    const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Sequence", desc, descwidth))
    return true;

  if (m_valueIs == vi_first) {
    if (bw.Append(" [value-head]", 13))
      return true;
  } else if (m_valueIs == vi_second) {
    if (bw.Append(" [value-tail]", 13))
      return true;
  }

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_first->Dump(bw, level, maxlevel, "head", 4))
      return true;
    if (m_second->Dump(bw, level, maxlevel, "tail", 4))
      return true;
  }

  return false;
}
