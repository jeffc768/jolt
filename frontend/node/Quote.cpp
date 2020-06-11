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

#include "Quote.h"
#include "Literal.h"
#include "util/BufferWriter.h"
#include "util/CodeFragment.h"
#include "util/InDeflator.h"
#include "util/Value.h"

IMPLEMENT_NODE(Quote)

void Quote::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_code;
}

Quote::Quote(Inflator &IF)
    : Node(IF) {
  IF >> m_code;
}

Quote::Quote(Location sl, Node *e)
    : Node(sl, Type::CodeFragment()),
      m_code(e) {
  m_state = st_type_resolved;
}

Node *Quote::ResolveType_(Context &ctx) {
  verify(false); return this;
}

Node *Quote::ResolveFully_(Context &ctx) {
  // FIXME: this assumes that there can be no insertions present.
  auto cf = new CodeFragment(m_code);
  auto ln = new Literal(m_location, Value::NewPtr(Type::CodeFragment(), cf));
  return ln;
}

void Quote::VisitChildren(Visitor *v) {
  v->Visit(m_code, ck_operand1);
}

bool Quote::Dump(BufferWriter &bw, int level, int maxlevel,
                const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Quote", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel)
    return m_code->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
