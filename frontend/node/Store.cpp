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

#include "Store.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(Store)

void Store::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_address;
  DF << m_operand;
}

Store::Store(Inflator &IF)
    : Node(IF) {
  IF >> m_address;
  IF >> m_operand;
}

Store::Store(Location sl, Node *addr, Node *e)
    : Node(sl, Type::Void()),
      m_operand(e) {
  verify(addr->IsFullyResolved());
  verify(e->IsFullyResolved());
  verify(addr->m_type != rt_rvalue);
  m_address = addr->AddrOf(true);
  m_state = st_fully_resolved;
}

Node *Store::ResolveType_(Context &ctx) {
  verify(false); return this;
}

Node *Store::ResolveFully_(Context &ctx) {
  verify(false); return this;
}

void Store::VisitChildren(Visitor *v) {
  v->Visit(m_address, ck_operand1);
  v->Visit(m_operand, ck_operand2);
}

bool Store::Dump(BufferWriter &bw, int level, int maxlevel,
                 const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Store", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_address->Dump(bw, level, maxlevel, "addr", 5))
      return true;
    if (m_operand->Dump(bw, level, maxlevel, "value", 5))
      return true;
  }

  return false;
}
