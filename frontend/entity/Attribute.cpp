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

#include "Attribute.h"
#include "node/Apply.h"
#include "node/Expr.h"
#include "node/Ident.h"
#include "util/InDeflator.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_OBJECT(Attribute)

Attribute::Attribute(Attribute *next, Expr *attr)
    : m_next(next),
      m_expr(attr) {
  // Must identify @build attribute now, while it's less messy to do so.
  Node *node = attr->Root();
  if (auto in = dyn_cast<Ident *>(node)) {
    if (in->m_identifier == wks_atbuild)
      m_isBuild = true;
  } else if (auto an = dyn_cast<Apply *>(node)) {
    if (auto in = dyn_cast<Ident *>(an->m_functor))
      if (in->m_identifier == wks_atbuild)
        m_isBuild = true;
  }
}

void Attribute::DeflateFields(Deflator &DF) {
  DF << m_next;
  DF << m_isBuild;
  DF << m_object;
  DF << m_expr;

  verify(!m_object);
}

Attribute::Attribute(Inflator &IF)
    : Object(IF) {
  IF >> m_next;
  IF >> m_isBuild;
  IF >> m_object;
  IF >> m_expr;
}

void Attribute::BindNames(SymbolTable &st) {
  m_expr->BindNames(st);

  if (m_next)
    m_next->BindNames(st);
}

void Attribute::ResolveFully() {
  if (m_object)
    return;

  m_object = m_expr->Evaluate();
  m_expr = nullptr;
  verify(m_object); // FIXME: handle entities
}
