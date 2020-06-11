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

#include "List.h"
#include "CallBuiltin.h"
#include "Expr.h"
#include "Ident.h"
#include "Initializer.h"
#include "Literal.h"
#include "entity/Attribute.h"
#include "entity/AttributeList.h"
#include "parser/Token.h"
#include "target/Bytecode.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_NODE(List)

List::List(Location sl, AST::SafeArray<AST::MemberItem> args)
    : Node(sl) {
  for (auto &mi : args) {
    auto tag = mi.m_name ? safe_cast<String *>(mi.m_name->m_value) : nullptr;
    m_tags.push_back(tag);
    m_attrlists.push_back(mi.m_attrs);
    m_values.push_back(mi.m_value);

    if (tag && tag->IsNameMacro())
      m_computedTags.push_back(tag->AsNormal());
    else
      m_computedTags.push_back(nullptr);
  }
}

void List::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_tags;
  DF << m_attrlists;
  DF << m_computedTags;
  DF << m_values;
}

List::List(Inflator &IF)
    : Node(IF) {
  IF >> m_tags;
  IF >> m_attrlists;
  IF >> m_computedTags;
  IF >> m_values;
}

Node *List::ResolveType_(Context &ctx) {
  // Determime which values are being built.
  for (size_t i = 0; i < m_attrlists.size(); ) {
    bool built = true;
    if (auto attr = m_attrlists[i])
      built = attr->GetBuildValue();

    if (built) {
      m_values[i] = m_values[i]->ResolveType(ctx);
      i++;
    } else {
      m_attrlists.erase(m_attrlists.begin() + i);
      m_tags.erase(m_tags.begin() + i);
      m_computedTags.erase(m_computedTags.begin() + i);
      m_values.erase(m_values.begin() + i);
    }
  }

  // Evaluate tags which are name macros.  This must be done now, as they are
  // part of a tuple's type (should this list be used as a type).
  m_type = Type::PseudoList();
  Context::PushVoid pv(ctx, false);
  for (size_t i = 0; i < m_computedTags.size(); i++) {
    if (String *tag = m_computedTags[i]) {
      Node *e = new Ident(m_location, tag);
      e = e->ResolveFully(ctx);
      Value *v = e->Evaluate(Type::StringRef());
      if (v->m_type == tk_valueless)
        m_type = Type::Join(m_type, v->m_type);
      else
        m_tags[i] = v->AsString();
    }
  }

  return this;
}

Node *List::ResolveFully_(Context &ctx) {
  for (auto &e : m_values)
    e = e->ResolveFully(ctx);
  return this;
}

bool List::ValueIsSubtypeOf(Node *e, Type t) {
  if (Type u = e->m_type; u != tk_pseudo) {
    // FIXME: this might be a bit more complicated; have to take
    // MAYBE into account?
    if (u.IsSubtypeOf(t) == NO)
      return false;
  } else if (u == Type::PseudoList()) {
    // Recursively handle nested lists.
    auto list = safe_cast<List *>(e);
    if (!list->IsSubtypeOf(t))
      return false;
  } else if (auto lit = dyn_cast<Literal *>(e)) {
    if (!lit->m_value->IsSubtypeOf(t))
      return false;
  } else {
    // FIXME: need to handle pseudo values that are not literals.
    verify(false);
  }

  return true;
}

bool List::IsSubtypeOf(Type t) {
  verify(m_state != st_initial);

  if (t == Type::JType()) {
    // Every value must be a type; this list represents a tuple type.
    for (auto e : m_values)
      if (auto ln = dyn_cast<List *>(e)) {
        if (!ln->IsSubtypeOf(t))
          return false;
      } else if (!e->m_type.IsSubtypeOf(Type::JType())) {
        return false;
      }

    return true;
  }

  if (t == tk_tuple) {
    // The number of values must must the tuple's field count.
    size_t cnt = t.FieldCount();
    if (cnt != m_values.size())
      return false;

    for (size_t i = 0; i < cnt; i++) {
      // The tag, or lack thereof, must match.
      if (t.IdentAt(i) != m_tags[i])
        return false;

      // Finally, check for legal subtyping.
      if (!ValueIsSubtypeOf(m_values[i], t.TypeOf(i)))
        return false;
    }

    return true;
  }

  // FIXME: handle arrays, classes, and maybe other things.
  verify(false);
  return false;
}

namespace {
  class BuildTupleType: public BC::NativeFunction {
  public:
    BuildTupleType() : BC::NativeFunction("BuildTupleType") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra);

      using member_t = std::pair<String *, Type>;
      vector<member_t> members;
      for (size_t i = 0; i < argcnt; i += 2) {
        String *s = reinterpret_cast<String *>(args[i].AsAddr());
        Type t = args[i + 1].AsType();
        members.emplace_back(s, t);
      }

      Type tt = Type::Tuple(members);

      BC::Union rv;
      rv.SetType(tt);
      return rv;
    }
  };

  static BuildTupleType g_BuildTupleType;
}

Node *List::AsTupleType(Location sl, Context &ctx) {
  verify(IsSubtypeOf(Type::JType()));
  Context::PushVoid pv(ctx, false);

  auto cbn = new CallBuiltin(sl, Type::JType(), &g_BuildTupleType);
  for (size_t i = 0; i < m_values.size(); i++) {
    Value *tag = Value::NewPseudo(Type::PseudoString(), m_tags[i]);
    cbn->AppendArg(new Literal(sl, tag));

    Node *&e = m_values[i];
    e = new Initializer(sl, Type::JType(), e, false);
    e = e->ResolveFully(ctx);
    cbn->AppendArg(e);
  }

  return cbn;
}

void List::VisitChildren(Visitor *v) {
  for (size_t i = 0; i < m_tags.size(); i++) {
    v->Visit(m_values[i], ck_operand1);

    if (auto al = m_attrlists[i]) {
      for (Attribute *a = al->m_first; a != nullptr; a = a->m_next) {
        if (Expr *e = a->GetExpr())
          v->Visit(e->Root(), ck_operand2);
      }
    }
  }
}

bool List::Dump(BufferWriter &bw, int level, int maxlevel,
                const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "List", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    for (size_t i = 0; i < m_values.size(); i++) {
      const char *tag = m_tags[i] ? m_tags[i]->c_str() : "";
      if (m_values[i]->Dump(bw, level, maxlevel, tag, 0))
        return true;
    }
  }

  return false;
}
