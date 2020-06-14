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

#include "Binary.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(Binary)

void Binary::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_opcode;
  DF << m_operand1;
  DF << m_operand2;
}

Binary::Binary(Inflator &IF)
    : Node(IF) {
  IF >> m_opcode;
  IF >> m_operand1;
  IF >> m_operand2;
}

static inline Type ComputeType(Binary::Opcode op, Node *e1, Node *e2) {
  if (op >= Binary::op_seteq)
    return Type::Bool();

  if (op == Binary::op_shl && op == Binary::op_shr)
    verify(e2->m_type == Type::Byte());
  else 
    verify(e1->m_type == e2->m_type);
  return e1->m_type;
}

Binary::Binary(Location sl, Opcode op, Node *e1, Node *e2)
    : Node(sl, ComputeType(op, e1, e2)),
      m_opcode(op),
      m_operand1(e1),
      m_operand2(e2) {
  verify(m_type == rt_rvalue);
  verify(e1->IsFullyResolved());
  verify(e2->IsFullyResolved());
  m_state = st_fully_resolved;
}

Node *Binary::ResolveType_(Context &ctx) {
  verify(false); return this;
}

Node *Binary::ResolveFully_(Context &ctx) {
  verify(false); return this;
}

void Binary::VisitChildren(Visitor *v) {
  v->Visit(m_operand1, ck_operand1);
  v->Visit(m_operand2, ck_operand2);
}

static const char g_opcodes[Binary::op_illegal+1][8] = {
  " add  \n",
  " sub  \n",
  " mul  \n",
  " div  \n",
  " rem  \n",
  " and  \n",
  " or   \n",
  " xor  \n",
  " shl  \n",
  " shr  \n",
  " seteq\n",
  " setne\n",
  " setlt\n",
  " setle\n",
  " setgt\n",
  " setge\n",
  " ???  \n"
};

bool Binary::Dump(BufferWriter &bw, int level, int maxlevel,
                  const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Binary", desc, descwidth))
    return true;

  if (bw.Append(g_opcodes[m_opcode], 7))
    return true;

  if (++level < maxlevel) {
    if (m_operand1->Dump(bw, level, maxlevel, "lhs", 3))
      return true;
    if (m_operand2->Dump(bw, level, maxlevel, "rhs", 3))
      return true;
  }

  return false;
}
