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

#include "VtableSlot.h"
#include "Load.h"
#include "entity/Class.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(VtableSlot)

void VtableSlot::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_class;
  DF << m_vtable;
  DF << m_slot;
}

VtableSlot::VtableSlot(Inflator &IF)
    : Node(IF) {
  IF >> m_class;
  IF >> m_vtable;
  IF >> m_slot;
}

VtableSlot::VtableSlot(Location sl, Class *ce, Node *vtable, size_t slot)
    : Node(sl, Type::PointerTo(Type::Function())),
      m_class(ce),
      m_vtable(new Load(sl, vtable)),
      m_slot(slot) {
  verify(vtable->IsFullyResolved());
  m_state = st_fully_resolved;
}

void VtableSlot::VisitChildren(Visitor *v) {
  v->Visit(m_vtable, ck_operand1);
}

Node *VtableSlot::ResolveType_(Context &ctx) {
  verify(false); return this;
}

Node *VtableSlot::ResolveFully_(Context &ctx) {
  verify(false); return this;
}

bool VtableSlot::Dump(BufferWriter &bw, int level, int maxlevel,
                      const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "VtableSlot", desc, descwidth))
    return true;

  if (bw.Append(' '))
    return true;

  if (bw.Append(m_class->Name()))
    return true;

  char buf[64];
  int len = sprintf(buf, " [slot %d]", (int)m_slot);
  if (bw.Append(buf, len))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel)
    return m_vtable->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
