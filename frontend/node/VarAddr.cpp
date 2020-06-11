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

#include "VarAddr.h"
#include "entity/Var.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(VarAddr)

void VarAddr::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_var;
}

VarAddr::VarAddr(Inflator &IF)
    : Node(IF) {
  IF >> m_var;
}

static inline Type VarType(Var *ve) {
  Type t = ve->m_type;
  if (t == rt_rvalueref)
    t = t.LValueRef();
  Type u = Type::ThinPointerTo(t.Lower());
  if (ve->m_type != tk_pointer)
    if (u.BaseType() != tk_pointer)
      return u;
  return u;
}

VarAddr::VarAddr(Location sl, Var *ve)
    : Node(sl, VarType(ve)),
      m_var(ve) {
  m_state = st_fully_resolved;
}

Node *VarAddr::ResolveType_(Context &ctx) {
  verify(false); return this;
}

Node *VarAddr::ResolveFully_(Context &ctx) {
  verify(false); return this;
}

bool VarAddr::Dump(BufferWriter &bw, int level, int maxlevel,
                   const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "VarAddr", desc, descwidth))
    return true;

  if (bw.Append(' '))
    return true;

  if (bw.Append(m_var->Name()))
    return true;

  if (bw.Append('\n'))
    return true;

  return false;
}
