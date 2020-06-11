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

#include "AttributeList.h"
#include "Attribute.h"
#include "Class.h"
#include "node/Expr.h"
#include "node/Ident.h"
#include "util/InDeflator.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_OBJECT(AttributeList)

AttributeList::AttributeList(Location sl, AST::SafeArray<AST::AttrInfo> attrs)
      : m_location(sl) {
  for (uint32_t i = attrs.size(); i > 0; ) {
    --i;
    verify(attrs[i].m_attrtype == AST::AttrInfo::at_normal); // FIXME
    m_first = new Attribute(m_first, new Expr(attrs[i].m_expr));

    // We need to know if static is present before attributes are resolved, so
    // that class declarations can be assigned to their proper class.
    if (auto in = dyn_cast<Ident *>(attrs[i].m_expr)) {
      if (in->m_identifier == wks_atstatic)
        m_hasStatic = true;
    }
  }
}

void AttributeList::DeflateFields(Deflator &DF) {
  DF << m_location;
  DF << m_parent;
  DF << m_first;
  DF << m_hasStatic;
  DF << m_resolved;
  DF << m_bag;
}

AttributeList::AttributeList(Inflator &IF)
    : Object(IF) {
  IF >> m_location;
  IF >> m_parent;
  IF >> m_first;
  IF >> m_hasStatic;
  IF >> m_resolved;
  IF >> m_bag;
}

void AttributeList::BindNames(SymbolTable &st) {
  // Parents are not handled here; their scope could be quite different.
  // They're handled by the Class or Namespace in which they're located.
  if (m_first)
    m_first->BindNames(st);
}

void AttributeList::ResolveFully() {
  if (m_resolved)
    return;

  m_resolved = true;

  // Attributes are not evaluated if the parent list has a build(false) in it.
  // We therefore must resolve the parent list (if any).
  if (m_parent) {
    m_parent->ResolveFully();

    // If the parent list has build(false), we are now fully resolved.
    // Our attributes are never evaluated.
    Value *v = m_parent->GetAttribute(Class::wkc_BuildAttribute);
    if (v && !v->AsBool()) {
      Class *ce = Class::GetClassById(Class::wkc_BuildAttribute);
      m_bag[ce] = v;
      m_first = nullptr;
      return;
    }

    // Inherit parent's attributes.
    for (auto &I : m_parent->m_bag)
      m_bag[I.first] = I.second;
  }

  for (Attribute *a = m_first; a; a = a->m_next) {
    a->ResolveFully();
    Type t = a->m_object->ObjectType();
    // FIXME: check for Attribute base class
    // FIXME: do merge of similar attributes
    if (t != tk_valueless)
      m_bag[t.Class()] = a->m_object;
  }

  m_first = nullptr;
}

bool AttributeList::HasStatic() {
  if (m_hasStatic)
    return true;
  if (m_parent)
    return m_parent->HasStatic();
  return false;
}

bool AttributeList::GetBuildValue() {
  if (m_resolved) {
    Value *v = GetAttribute(Class::wkc_BuildAttribute);
    return v ? v->AsBool() : true;
  }

  // We need to determine a @build attribute even when the attribute list is
  // not resolved--important to avoid false circular dependencies.

  if (m_parent && !m_parent->GetBuildValue())
    return false;

  for (Attribute *a = m_first; a; a = a->m_next) {
    if (a->m_isBuild) {
      a->ResolveFully();
      return a->m_object->AsBool();
    }
  }

  return true;
}

Value *AttributeList::GetAttribute(int id) {
  verify(m_resolved);
  // Having id be a proper id in the first place would cause recursive header
  // file includes... one of the C++ problems Jolt fixes.
  Class *ce = Class::GetClassById(Class::WellKnownClassId(id));

  // If the wellknown class doesn't seem to have a class object, then it hasn't
  // yet been imported from the std module, which means no attribute of that
  // class can be present.
  if (!ce)
    return nullptr;

  auto I = m_bag.find(ce);
  return I == m_bag.end() ? nullptr : I->second;
}
