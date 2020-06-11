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

#include "BaseSpecifier.h"
#include "AttributeList.h"
#include "Class.h"
#include "node/Expr.h"
#include "parser/Token.h"
#include "parser/SymbolTable.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"

/******************************************************************************/

IMPLEMENT_OBJECT(BaseSpecifier)

BaseSpecifier::BaseSpecifier(AST::BaseInfo &bi)
    : m_attributes(bi.m_attrs),
      m_baseClassExpr(new Expr(bi.m_type)) { }

BaseSpecifier::BaseSpecifier(BaseSpecifier *bs)
    : m_baseClass(bs->m_baseClass->Metaclass()),
      m_attributes(bs->m_attributes) { }

BaseSpecifier::BaseSpecifier(Class *ce)
    : m_baseClass(ce) { }

void BaseSpecifier::DeflateFields(Deflator &DF) {
  DF << m_attributes;
  DF << m_baseClass;
  DF << m_externalName;
  DF << m_offset;
  DF << m_ord;
  DF << m_vtblOffset;
  DF << m_baseClassExpr;

  // Base specifiers for metaclasses are never deflated, as they are created by
  // the resolution process.  Base specifiers for well-known classes are also
  // never deflated, as they cannot be part of generic classes or methods.
}

BaseSpecifier::BaseSpecifier(Inflator &IF)
    : Object(IF) {
  IF >> m_attributes;
  IF >> m_baseClass;
  IF >> m_externalName;
  IF >> m_offset;
  IF >> m_ord;
  IF >> m_vtblOffset;
  IF >> m_baseClassExpr;
}

void BaseSpecifier::BindNames(SymbolTable &st) {
  // Builtin classes are created with resolved bases.
  if (m_baseClassExpr)
    m_baseClassExpr->BindNames(st);

  if (m_attributes)
    m_attributes->BindNames(st);
}

void BaseSpecifier::ResolveFully() {
  if (!m_baseClassExpr) {
    verify(m_baseClass);
    return;
  }

  if (m_attributes) {
    m_attributes->ResolveFully();

    // Deal with the infamous build attributes.
    if (!m_attributes->GetBuildValue())
      return;
  }

  Type t = m_baseClassExpr->EvaluateType();
  if (t != tk_class) {
    EmitError(m_baseClassExpr) << "Cannot inherit from a non-class.";
    m_baseClassExpr = nullptr;
    return;
  }

  m_baseClass = t.Class();
  m_baseClass->ResolveFully();
  m_baseClassExpr = nullptr;
}

bool BaseSpecifier::IsNotBuilt() {
  return m_baseClass == nullptr;
}
