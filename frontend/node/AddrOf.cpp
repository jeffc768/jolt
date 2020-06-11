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

#include "AddrOf.h"
#include "BuildPointer.h"
#include "Deref.h"
#include "Literal.h"
#include "Load.h"
#include "parser/Token.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Value.h"

IMPLEMENT_NODE(AddrOf)

AddrOf::AddrOf(Location sl, Node *arg)
    : Node(sl),
      m_expr(arg) { }

AddrOf::AddrOf(Node *e, bool forceFat)
    : Node(e->m_location),
      m_expr(e),
      m_forceFat(forceFat) { }

void AddrOf::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_expr;

  verify(!m_forceFat);
}

AddrOf::AddrOf(Inflator &IF)
    : Node(IF) {
  IF >> m_expr;
}

Node *AddrOf::ResolveType_(Context &ctx) {
  m_expr = m_expr->ResolveType(ctx);

  Type t = m_expr->m_type;

  // FIXME: diagnose r-values.
  verify(t != rt_rvalue);

  // FIXME: for now, fat pointers are forced to be non-simple.  Even though it
  // would fit in 64 bits on a 32-bit system, there isn't a good way to build a
  // single value from two addresses.
  m_type = Type::PointerTo(t);
  if (m_forceFat || !m_type.IsThin()) {
    if (t == tk_array) {
      m_type = Type::PointerTo(Type::ArrayOf(t.ElementType()));
    } else {
      verify(false);
    }
  }

  return this;
}

Node *AddrOf::ResolveFully_(Context &ctx) {
  Context::PushVoid pv(ctx, false);
  m_expr = m_expr->ResolveFully(ctx);
  if (ctx.m_error)
    return this;

  // Handle the simple case of producing a thin pointer.
  if (m_type.IsThin())
    return m_expr->AddrOf(true);

  // If we're taking the address of a dereferenced fat pointer, then the fat
  // pointer replaces us.
  if (auto dn = dyn_cast<::Deref *>(m_expr)) {
    if (auto ln = dyn_cast<Load *>(dn->m_expr))
      if (!ln->m_address->m_type.BaseType().IsThin())
        return ln;
  }

  intptr_t card = m_expr->m_type.IndexType().Cardinality();
  Node *e = m_expr->AddrOf(true);
  if (card >= 0) {
    auto lit = new Literal(m_location, Value::NewInt(Type::IntPtrT(), card));
    verify(e->m_type.IsThin());
    e = new BuildPointer(m_type, e, lit);
  }

  return e;
}

void AddrOf::VisitChildren(Visitor *v) {
  v->Visit(m_expr, ck_operand1);
}

bool AddrOf::Dump(BufferWriter &bw, int level, int maxlevel,
                  const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "AddrOf", desc, descwidth))
    return true;

  if (m_forceFat && bw.Append(" forcefat", 9))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel)
    return m_expr->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
