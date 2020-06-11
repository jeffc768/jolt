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

#include "OverloadSetRef.h"
#include "entity/OverloadSet.h"
#include "util/BufferWriter.h"

IMPLEMENT_NODE(OverloadSetRef)

OverloadSetRef::OverloadSetRef(Location sl, OverloadSet *ose, Node *receiver)
    : Node(sl),
      m_entity(ose),
      m_receiver(receiver) { }

Node *OverloadSetRef::ResolveType_(Context &ctx) {
  if (m_receiver)
    m_receiver = m_receiver->ResolveType(ctx);

  // A OverloadSetRef node doesn't really have a type, so we interpret type as
  // meaning that the OverloadSet can resolve an overloaded signature.
  m_entity->ResolveFully();

  // FIXME: need proper function types.
  m_type = Type::Function().LValueRef();
  return this;
}

Node *OverloadSetRef::ResolveFully_(Context &ctx) {
  Context::PushVoid pv(ctx, false);
  if (m_receiver)
    m_receiver = m_receiver->ResolveFully(ctx);
  return this;
}

void OverloadSetRef::VisitChildren(Visitor *v) {
  if (m_receiver)
    v->Visit(m_receiver, ck_operand1);
}

bool OverloadSetRef::Dump(BufferWriter &bw, int level, int maxlevel,
                          const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "OverloadSetRef", desc, descwidth))
    return true;

  if (bw.Append(' '))
    return true;

  if (bw.Append(m_entity->Name()))
    return true;

  if (bw.Append('\n'))
    return true;

  if (m_receiver && ++level < maxlevel)
    return m_receiver->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
