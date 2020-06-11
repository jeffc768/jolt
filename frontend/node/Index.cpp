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

#include "Index.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(Index)

void Index::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_address;
  DF << m_index;
}

Index::Index(Inflator &IF)
    : Node(IF) {
  IF >> m_address;
  IF >> m_index;
}

Index::Index(Location sl, Node *addr, Node *idx)
    : Node(sl, Type::ThinPointerTo(addr->m_type.ElementType())),
      m_index(idx) {
  verify(addr->IsFullyResolved());
  verify(idx->IsFullyResolved());
  m_address = addr->AddrOf(true);
  verify(m_address->m_type == tk_pointer);
  if (addr->m_type == ct_const)
    m_type = Type::ThinPointerTo(m_type.BaseType().Const());
  m_state = st_fully_resolved;
}

Index::Index(Location sl, Type t, Node *idx)
    : Node(sl, Type::ThinPointerTo(t.ElementType())),
      m_index(idx) {
  verify(idx->IsFullyResolved());
  m_state = st_fully_resolved;
}

Node *Index::ResolveType_(Context &ctx) {
  verify(false); return this;
}

Node *Index::ResolveFully_(Context &ctx) {
  verify(false); return this;
}

void Index::VisitChildren(Visitor *v) {
  if (m_address)
    v->Visit(m_address, ck_operand1);

  v->Visit(m_index, ck_operand2);
}

bool Index::Dump(BufferWriter &bw, int level, int maxlevel,
                 const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Index", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_address && m_address->Dump(bw, level, maxlevel, "addr", 4))
      return true;
    if (m_index->Dump(bw, level, maxlevel, "idx", 4))
      return true;
  }

  return false;
}
