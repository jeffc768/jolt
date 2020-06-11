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

#include "BuildPointer.h"
#include "Literal.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Value.h"

IMPLEMENT_NODE(BuildPointer)

void BuildPointer::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_pointer;
  DF << m_descriptor;
}

BuildPointer::BuildPointer(Inflator &IF)
    : Node(IF) {
  IF >> m_pointer;
  IF >> m_descriptor;
}

BuildPointer::BuildPointer(Location sl, Type t)
    : Node(sl, t) {
  verify(t.BaseType() == tk_array);
  verify(!t.IsThin());
  Type tt = Type::ThinPointerTo(t.BaseType());
  m_pointer = new Literal(sl, Value::NewPtr(tt, nullptr));
  m_descriptor = new Literal(sl, Value::NewInt(t.DescriptorType(), 0));
  m_state = st_fully_resolved;
}

BuildPointer::BuildPointer(Type t, Node *ptr, Node *desc)
    : Node(ptr->m_location, t),
      m_pointer(ptr),
      m_descriptor(desc) {
  verify(t == tk_pointer && !t.IsThin());
  verify(ptr->IsFullyResolved());
  verify(desc->IsFullyResolved());
  m_state = st_fully_resolved;
}

Node *BuildPointer::ResolveType_(Context &ctx) {
  verify(false); return this;
}

Node *BuildPointer::ResolveFully_(Context &ctx) {
  verify(false); return this;
}

void BuildPointer::VisitChildren(Visitor *v) {
  v->Visit(m_pointer, ck_operand1);
  v->Visit(m_descriptor, ck_operand2);
}

bool BuildPointer::Dump(BufferWriter &bw, int level, int maxlevel,
                        const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "BuildPointer", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_pointer->Dump(bw, level, maxlevel, "ptr", 4))
      return true;
    if (m_descriptor->Dump(bw, level, maxlevel, "desc", 4))
      return true;
  }

  return false;
}
