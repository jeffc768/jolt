// Copyright (c) 2018, Jeff Cohen
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

#include "Enum.h"
#include "Expr.h"
#include "Ident.h"
#include "entity/Attribute.h"
#include "entity/AttributeList.h"
#include "node/Literal.h"
#include "parser/Token.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_NODE(Enum)

// FIXME: evaluation of types and values should be deferred to a native
// method, so that their expressions may use function arguments when those
// functions are @epoch(compile).

Enum::Enum(Location sl, AST::SafeArray<AST::MemberItem> args, Node *avType)
    : Node(sl),
      m_avTypeExpr(avType) {
  for (auto &mi : args) {
    if (mi.m_value && !m_avTypeExpr) {
      EmitError(mi.m_value) << "No associated type specified - value ignored.";
      mi.m_value = nullptr;
    }

    auto name = safe_cast<String *>(mi.m_name->m_value);
    m_names.push_back(name);
    m_attrlists.push_back(mi.m_attrs);
    m_valueExprs.push_back(mi.m_value);

    if (!name) {
      EmitError(mi.m_value) << "No enum member name specified.";
    } else if (name->IsNameMacro()) {
      m_computedNames.push_back(name->AsNormal());
    } else {
      m_computedNames.push_back(nullptr);
    }
  }
}

void Enum::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_names;
  DF << m_attrlists;
  DF << m_computedNames;
  DF << m_avTypeExpr;
  DF << m_valueExprs;
}

Enum::Enum(Inflator &IF)
    : Node(IF) {
  IF >> m_names;
  IF >> m_attrlists;
  IF >> m_computedNames;
  IF >> m_avTypeExpr;
  IF >> m_valueExprs;
}

Node *Enum::ResolveType_(Context &ctx) {
  if (m_avTypeExpr) {
    m_avTypeExpr = m_avTypeExpr->ResolveType(ctx);
    if (m_avTypeExpr->m_type == tk_valueless)
      m_type = m_avTypeExpr->m_type;
  }

  // Determime which members are being built.
  for (size_t i = 0; i < m_attrlists.size(); ) {
    bool built = true;
    if (auto attr = m_attrlists[i])
      built = attr->GetBuildValue();

    if (built) {
      if (m_valueExprs[i])
        m_valueExprs[i] = m_valueExprs[i]->ResolveType(ctx);
      i++;
    } else {
      m_attrlists.erase(m_attrlists.begin() + i);
      m_names.erase(m_names.begin() + i);
      m_computedNames.erase(m_computedNames.begin() + i);
      m_valueExprs.erase(m_valueExprs.begin() + i);
    }
  }

  m_type = Type::JType();
  return this;
}

Node *Enum::ResolveFully_(Context &ctx) {
  Context::PushVoid pv(ctx, false);

  // Evaluate type of associated values.
  if (m_avTypeExpr) {
    m_avTypeExpr = m_avTypeExpr->ResolveFully(ctx);
    m_avType = m_avTypeExpr->EvaluateType();
  } else {
    m_avType = Type::Void();
  }

  // Evaluate enum tags which are name macros.
  for (size_t i = 0; i < m_computedNames.size(); i++) {
    if (String *name = m_computedNames[i]) {
      Node *e = new Ident(m_location, name);
      e = e->ResolveFully(ctx);
      Value *v = e->Evaluate(Type::StringRef());
      if (v->m_type == tk_valueless)
        m_type = Type::Join(m_type, v->m_type);
      else
        m_names[i] = v->AsString();
    }
  }

  // Evaluate associated values.
  if (m_avType != tk_void) {
    for (auto e : m_valueExprs) {
      verify(e); // FIXME: diagnose missing values.
      e = e->ResolveFully(ctx);
      Value *v = e->Evaluate(m_avType);
      m_values.push_back(v);

      Type t = v->ObjectType();
      if (t == tk_valueless)
        m_type = Type::Join(m_type, t);
    }
  }

  // Determine enum type.
  if (m_type != tk_valueless) {
    if (m_avType == tk_void)
      m_type = Type::Enum(m_names);
    else
      m_type = Type::Enum(m_names, m_avType, m_values);
  }

  return new Literal(m_location, Value::NewType(m_type));
}

void Enum::VisitChildren(Visitor *v) {
  if (m_avTypeExpr)
    v->Visit(m_avTypeExpr, ck_type);

  for (size_t i = 0; i < m_valueExprs.size(); i++) {
    if (auto p = m_valueExprs[i])
      v->Visit(p, ck_operand1);

    if (auto al = m_attrlists[i]) {
      for (Attribute *a = al->m_first; a != nullptr; a = a->m_next) {
        if (Expr *e = a->GetExpr())
          v->Visit(e->Root(), ck_operand2);
      }
    }
  }
}

bool Enum::Dump(BufferWriter &bw, int level, int maxlevel,
                const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Enum", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_avTypeExpr->Dump(bw, level, maxlevel, "type", 4))
      return true;
    for (auto e : m_valueExprs)
      if (e && e->Dump(bw, level, maxlevel, "val", 4))
        return true;
  }

  return false;
}
