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

#include "Literal.h"
#include "util/BufferWriter.h"
#include "util/CodeFragment.h"
#include "util/InDeflator.h"
#include "util/Value.h"

IMPLEMENT_NODE(Literal)

Literal::Literal(Location sl, Value *value)
    : Node(sl, value->ObjectType()),
      m_value(value) {
  m_state = st_fully_resolved;
}

void Literal::DeflateFields(Deflator &DF) {
  // FIXME: if the type is of something that is a UnmovableObject, or a pointer
  // to a managed object, then it needs to be deflated as something other than
  // a raw sequence of bytes.
  Node::DeflateFields(DF);
  DF << m_value;
}

Literal::Literal(Inflator &IF)
    : Node(IF) {
  IF >> m_value;
}

Node *Literal::ResolveType_(Context &ctx) {
  verify(false); return this;
}

Node *Literal::ResolveFully_(Context &ctx) {
  verify(false); return this;
}

bool Literal::Dump(BufferWriter &bw, int level, int maxlevel,
                   const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Literal", desc, descwidth))
    return true;

  if (bw.Append(' '))
    return true;

  if (m_value->Dump(bw))
    return true;

  if (bw.Append('\n'))
    return true;

  if (m_value->ObjectType() == tk_codefragment) {
    auto cf = m_value->As<CodeFragment *>();
    return cf->Get()->Dump(bw, level + 1, maxlevel, "code", 4);
  }

  return false;
}
