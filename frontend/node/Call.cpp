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

#include "Call.h"
#include "entity/Argument.h"
#include "entity/FormalArguments.h"
#include "entity/Method.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"

IMPLEMENT_NODE(Call)

void Call::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_function;
  DF << m_method;
  DF << m_arguments;
  DF << m_returnValue;
  DF << m_operands;
}

Call::Call(Inflator &IF)
    : Node(IF) {
  IF >> m_function;
  IF >> m_method;
  IF >> m_arguments;
  IF >> m_returnValue;
  IF >> m_operands;
}

Call::Call(Location sl, Method *me, vector<Argument *> &args,
           Argument *returnValue, Node *func)
    : Node(sl, returnValue ? returnValue->m_type : Type::Void()),
      m_function(func),
      m_method(me),
      m_returnValue(returnValue) {
  verify(func->IsFullyResolved());
  m_arguments.swap(args);
  m_state = st_fully_resolved;
}

void Call::AppendArg(Node *e) {
  verify(e->IsFullyResolved());
  m_operands.push_back(e);
}

void Call::GetSlotOrdering(vector<Slot> &args, Slot &rv) {
  // Reorder the arguments according to their assigned slots.  This will cause
  // the return value to be put back into the argument list if it isn't a
  // simple type.  Likewise, it will cause lowered return value to be removed
  // from the argument list if it's a simple type.
  auto formals = m_method->LowerFormals();
  args.resize(formals->m_nSlots);
  rv.m_arg = 0;
  for (size_t i = 0; i < m_arguments.size(); i++) {
    Argument *ae = m_arguments[i];
    bool isReturned = ae->m_isReturned;
    int slot = ae->m_slot;

    if (isReturned) {
      verify(!rv.m_arg);
      rv.m_arg = m_operands[i];
    } else if (slot >= 0) {
      verify(args[slot].m_arg == nullptr);
      args[slot].m_arg = m_operands[i];
    }
  }

  // Handle excess operands due to C-style variadic functions.
  for (size_t i = m_arguments.size(); i < m_operands.size(); i++)
    args.emplace_back(m_operands[i]);

  if (m_returnValue && m_returnValue->m_slot >= 0)
    verify(args[m_returnValue->m_slot].m_arg == nullptr);

#ifndef NDEBUG
  for (size_t i = 0; i < args.size(); i++)
    if (args[i].m_arg == nullptr)
      verify(m_returnValue && m_returnValue->m_slot == int(i));
#endif

  // FIXME: handle case where the lowered return value is an explicit out
  // argument--must pass back the l-value expression providing the target to
  // assign said value.
}

Node *Call::ResolveType_(Context &ctx) {
  verify(false); return this;
}

Node *Call::ResolveFully_(Context &ctx) {
  verify(false); return this;
}

void Call::VisitChildren(Visitor *v) {
  v->Visit(m_function, ck_operand1);
  for (size_t i = 0; i < m_operands.size(); i++)
    v->Visit(m_operands[i], ChildKind(ck_argument + i));
}

bool Call::Dump(BufferWriter &bw, int level, int maxlevel,
                  const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Call", desc, descwidth))
    return true;

  if (bw.Append(' '))
    return true;

  if (bw.Append(m_method->Name()))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_function->Dump(bw, level, maxlevel, "func", 4))
      return true;
    for (auto arg : m_operands)
      if (arg->Dump(bw, level, maxlevel, "arg", 4))
        return true;
  }

  return false;
}
