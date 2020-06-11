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

#include "ExtractDescriptor.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(ExtractDescriptor)

void ExtractDescriptor::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_pointer;
}

ExtractDescriptor::ExtractDescriptor(Inflator &IF)
    : Node(IF) {
  IF >> m_pointer;
}

ExtractDescriptor::ExtractDescriptor(Node *ptr)
    : Node(ptr->m_location, ptr->m_type.DescriptorType()),
      m_pointer(ptr) {
  verify(ptr->m_type == tk_pointer && !ptr->m_type.IsThin());
  verify(ptr->IsFullyResolved());
  m_state = st_fully_resolved;
}

ExtractDescriptor::ExtractDescriptor(Location sl, Type t)
    : Node(sl, t),
      m_pointer(nullptr) {
  m_state = st_fully_resolved;
}

Node *ExtractDescriptor::ResolveType_(Context &ctx) {
  verify(false); return this;
}

Node *ExtractDescriptor::ResolveFully_(Context &ctx) {
  verify(false); return this;
}

void ExtractDescriptor::VisitChildren(Visitor *v) {
  if (m_pointer)
    v->Visit(m_pointer, ck_operand1);
}

bool ExtractDescriptor::Dump(BufferWriter &bw, int level, int maxlevel,
                             const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "ExtractDescriptor", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel)
    return m_pointer->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
