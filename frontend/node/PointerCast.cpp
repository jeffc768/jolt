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

#include "PointerCast.h"
#include "FieldAddr.h"
#include "entity/BaseSpecifier.h"
#include "entity/Class.h"
#include "util/BufferWriter.h"

IMPLEMENT_NODE(PointerCast)

PointerCast::PointerCast(Type t, Node *e)
    : Node(e->m_location, t),
      m_expr(e) { }

Node *PointerCast::ResolveType_(Context &ctx) {
  m_expr = m_expr->ResolveType(ctx);
  return this;
}

Node *PointerCast::ResolveFully_(Context &ctx) {
  Context::PushVoid pv(ctx, false);
  m_expr = m_expr->ResolveFully(ctx);

  // Check for no-op cast.
  if (m_type.BaseType() == m_expr->m_type.BaseType())
    return m_expr;

  // Check for cast from non-const to const.
  // FIXME: handle operator const overloads?
  if (m_type.BaseType() == m_expr->m_type.BaseType().Const()) {
    m_expr->m_type = m_type;
    return m_expr;
  }

  // Handle cast from non-conformant to conformant array pointer.
  if (m_type.BaseType() == tk_array) {
    m_expr->m_type = m_type;
    return m_expr;
  }

  // Otherwise, the cast must be a class pointer upcast.
  Class *t_ce = m_type.BaseType().Class();
  Class *s_ce = m_expr->m_type.BaseType().Class();

  // Obviously, if the two types are identical, nothing needs to be done.
  // Otherwise, follow the path up to the target inherited class in order to
  // offset the pointer as necessary.
  if (t_ce != s_ce) {
    vector<size_t> path;
    s_ce->IsSubclassOf(t_ce, &path);

    // FIXME: handle ambiguous inheritance.
    for (size_t i = path.size(); i-- > 0; ) {
      size_t ord = path[i];
      BaseSpecifier *bs = s_ce->GetBase(ord);
      m_expr = new FieldAddr(m_expr->m_location, m_expr->Deref(), bs->m_ord);
      s_ce = bs->m_baseClass;
    }

    // Redo to handle constness.
    return this->ResolveFully_(ctx);
  }

  return m_expr;
}

void PointerCast::VisitChildren(Visitor *v) {
  v->Visit(m_expr, ck_operand1);
}

bool PointerCast::Dump(BufferWriter &bw, int level, int maxlevel,
                       const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "PointerCast", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel)
    return m_expr->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
