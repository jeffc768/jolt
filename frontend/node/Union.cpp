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

#include "Union.h"
#include "Expr.h"
#include "Ident.h"
#include "entity/Attribute.h"
#include "entity/AttributeList.h"
#include "parser/Token.h"
#include "node/Literal.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_NODE(Union)

Union::Union(Location sl, AST::SafeArray<AST::MemberItem> args)
    : Node(sl) {
  for (auto &mi : args) {
    auto name = safe_cast<String *>(mi.m_name->m_value);
    m_names.push_back(name);
    m_attrlists.push_back(mi.m_attrs);
    m_typeExprs.push_back(mi.m_value);

    if (!name) {
      EmitError(mi.m_value) << "No union member name specified.";
    } else if (name->IsNameMacro()) {
      m_computedNames.push_back(name->AsNormal());
    } else {
      m_computedNames.push_back(nullptr);
    }
  }
}

void Union::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_names;
  DF << m_attrlists;
  DF << m_computedNames;
  DF << m_typeExprs;
}

Union::Union(Inflator &IF)
    : Node(IF) {
  IF >> m_names;
  IF >> m_attrlists;
  IF >> m_computedNames;
  IF >> m_typeExprs;
}

Node *Union::ResolveType_(Context &ctx) {
  // Determime which members are being built.
  for (size_t i = 0; i < m_attrlists.size(); ) {
    bool built = true;
    if (auto attr = m_attrlists[i])
      built = attr->GetBuildValue();

    if (built) {
      m_typeExprs[i] = m_typeExprs[i]->ResolveType(ctx);
      i++;
    } else {
      m_attrlists.erase(m_attrlists.begin() + i);
      m_names.erase(m_names.begin() + i);
      m_computedNames.erase(m_computedNames.begin() + i);
      m_typeExprs.erase(m_typeExprs.begin() + i);
    }
  }

  m_type = Type::JType();
  return this;
}

Node *Union::ResolveFully_(Context &ctx) {
  // Evaluate member tags which are name macros.
  Context::PushVoid pv(ctx, false);
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

  // Evaluate member types.
  vector<Type> types;
  for (auto e : m_typeExprs) {
    e = e->ResolveFully(ctx);
    Type t = e->EvaluateType();
    types.push_back(t);

    if (t == tk_valueless)
      m_type = Type::Join(m_type, t);
  }

  // Determine union type.
  if (m_type != tk_valueless) {
    vector<std::pair<String *, Type>> variants;
    variants.reserve(m_names.size());
    for (size_t i = 0; i < m_names.size(); i++)
      variants.emplace_back(m_names[i], types[i]);
    m_type = Type::Union(variants);
  }

  return new Literal(m_location, Value::NewType(m_type));
}

void Union::VisitChildren(Visitor *v) {
  for (size_t i = 0; i < m_names.size(); i++) {
    v->Visit(m_typeExprs[i], ck_operand1);

    if (auto al = m_attrlists[i]) {
      for (Attribute *a = al->m_first; a != nullptr; a = a->m_next) {
        if (Expr *e = a->GetExpr())
          v->Visit(e->Root(), ck_operand2);
      }
    }
  }
}

bool Union::Dump(BufferWriter &bw, int level, int maxlevel,
                 const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Union", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    for (size_t i = 0; i < m_typeExprs.size(); i++) {
      const char *name = m_names[i]->c_str();
      if (m_typeExprs[i]->Dump(bw, level, maxlevel, name, 0))
        return true;
    }
  }

  return false;
}
