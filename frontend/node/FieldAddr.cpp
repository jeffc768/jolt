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

#include "FieldAddr.h"
#include "entity/Field.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(FieldAddr)

void FieldAddr::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_address;
  DF << m_entity;
  DF << m_ord;
}

FieldAddr::FieldAddr(Inflator &IF)
    : Node(IF) {
  IF >> m_address;
  IF >> m_entity;
  IF >> m_ord;
}

FieldAddr::FieldAddr(Location sl, Node *addr, size_t ord)
    : Node(sl, Type::ThinPointerTo(addr->m_type.TypeOf(ord))),
      m_ord(ord) {
  verify(addr->IsFullyResolved());
  m_address = addr->AddrOf(true);
  verify(m_address->m_type == tk_pointer);
  if (addr->m_type == ct_const)
    m_type = Type::ThinPointerTo(m_type.BaseType().Const());
  m_state = st_fully_resolved;
}

FieldAddr::FieldAddr(Location sl, Type t, size_t ord)
    : Node(sl, Type::ThinPointerTo(t.TypeOf(ord))),
      m_ord(ord) {
  m_state = st_fully_resolved;
}

FieldAddr::FieldAddr(Location sl, Field *fe, Node *addr)
    : Node(sl, Type::ThinPointerTo(fe->GetType())),
      m_entity(fe),
      m_ord(fe->GetOrdinal()) {
  verify(addr->IsFullyResolved());
  m_address = addr->AddrOf(true);
  verify(m_address->m_type == tk_pointer);
  if (addr->m_type == ct_const)
    m_type = Type::ThinPointerTo(m_type.BaseType().Const());
  m_state = st_fully_resolved;
}

Node *FieldAddr::ResolveType_(Context &ctx) {
  verify(false); return this;
}

Node *FieldAddr::ResolveFully_(Context &ctx) {
  verify(false); return this;
}

void FieldAddr::VisitChildren(Visitor *v) {
  if (m_address)
    v->Visit(m_address, ck_operand1);
}

bool FieldAddr::Dump(BufferWriter &bw, int level, int maxlevel,
                     const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "FieldAddr", desc, descwidth))
    return true;

  if (m_entity) {
    if (bw.Append(' '))
      return true;

    if (bw.Append(m_entity->Name()))
      return true;
  }

  char buf[128];
  int len = sprintf(buf, " [ord %d]", (int)m_ord);
  if (bw.Append(buf, len))
    return true;

  if (bw.Append('\n'))
    return true;

  if (m_address && ++level < maxlevel)
    return m_address->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
