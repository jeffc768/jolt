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

#include "CallBuiltin.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(CallBuiltin)

void CallBuiltin::DeflateFields(Deflator &DF) {
  // FIXME:  First, do these exist in fully-resolved code?  If so, then an
  // external representation is required for builtins.  But builtins tend to
  // be used for compiletime expressions that get evaluated during compilation.
  Node::DeflateFields(DF);
  verify(false);
  // DF << m_function;
  DF << m_operands;
}

CallBuiltin::CallBuiltin(Inflator &IF)
    : Node(IF) {
  verify(false);
  // IF >> m_function;
  IF >> m_operands;
}

CallBuiltin::CallBuiltin(Location sl, Type t, BC::NativeFunction *nf)
    : Node(sl, t),
      m_function(nf) {
  verify(t.IsKnown());
  m_state = st_fully_resolved;
}

void CallBuiltin::AppendArg(Node *e) {
  verify(e->IsFullyResolved());
  m_operands.push_back(e);
}

Node *CallBuiltin::ResolveType_(Context &ctx) {
  verify(false); return this;
}

Node *CallBuiltin::ResolveFully_(Context &ctx) {
  verify(false); return this;
}

void CallBuiltin::VisitChildren(Visitor *v) {
  for (size_t i = 0; i < m_operands.size(); i++)
    v->Visit(m_operands[i], ChildKind(ck_argument + i));
}

bool CallBuiltin::Dump(BufferWriter &bw, int level, int maxlevel,
                       const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "CallBuiltin", desc, descwidth))
    return true;

  if (bw.Append(' '))
    return true;

  const char *name = m_function->Name();
  if (bw.Append(name, strlen(name)))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    for (auto arg : m_operands)
      if (arg->Dump(bw, level, maxlevel, "arg", 4))
        return true;
  }

  return false;
}
