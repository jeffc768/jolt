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

#include "Initializer.h"
#include "AddrOf.h"
#include "Apply.h"
#include "Ident.h"
#include "List.h"
#include "Literal.h"
#include "Load.h"
#include "Member.h"
#include "VarAddr.h"
#include "VarDecl.h"
#include "entity/Var.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Integer.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_NODE(Initializer)

Initializer::Initializer(Location sl, Node *type, Node *value)
    : Node(sl),
      m_typeExpr(type) {
  if (value)
    m_values.push_back(value);
}

Initializer::Initializer(Location sl, Type type, Node *value, bool block,
                         bool dontLower)
    : Node(sl, type),
      m_blockReplacement(block),
      m_dontLower(dontLower) {
  if (value)
    m_values.push_back(value->LowerPseudos(type));
}

Initializer::Initializer(Location sl, Node *value, bool dummy)
    : Node(sl),
      m_values(1, value) { }

Initializer::Initializer(Location sl, Ident *name,
                         AST::SafeArray<AST::MemberItem> args)
    : Node(sl),
      m_name(name) {
  for (auto &mi : args)
    m_values.push_back(mi.m_value);  // FIXME: other fields of MemberItem
}

void Initializer::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_typeExpr;
  DF << m_name;
  DF << m_values;
  DF << m_autoType;
  DF << m_blockReplacement;
  DF << m_dontLower;
  verify(!m_deducedType.IsKnown());
  verify(!m_replacement);
  verify(!m_rvalueTemp);
}

Initializer::Initializer(Inflator &IF)
    : Node(IF) {
  IF >> m_typeExpr;
  IF >> m_name;
  IF >> m_values;
  IF >> m_autoType;
  IF >> m_blockReplacement;
  IF >> m_dontLower;
}

Node *Initializer::ResolveType_(Context &ctx) {
  // If we're just wrapping another, fully resolved Initializer, immediately
  // replace ourselves with it.
  if (m_values.size() == 1) {
    Node *e = m_values[0];
    if (e->Kind() == nk_Initializer && e->IsFullyResolved()) {
      verify(m_blockReplacement);
      return e;
    }
  }

  // Supply default constructor, if none explicitly named.
  if (!m_name)
    m_name = new Ident(m_location, wks_op_apply);

  // Resolve type, if one was supplied.
  if (m_type.IsKnown()) {
    return this;
  } else if (m_autoType) {
    return this;
  } else if (m_typeExpr) {
    Context::PushVoid pv(ctx, false);
    m_typeExpr = m_typeExpr->ResolveFully(ctx);
    m_type = m_typeExpr->EvaluateType();
  }

  // FIXME: for now, an Initializer node simply replaces itself with the
  // Apply node we'll eventually replace ourselves with.  Let that handle
  // name and values; indeed, the name can't begin resolution now as it needs
  // to be a member of the type, not in the current lexical scope!

  // The type of an Initializer node doesn't really have meaning at this point.
  return this;
}

static Type DeduceTupleType(List *ln) {
  using member_t = std::pair<String *, Type>;
  vector<member_t> fields;
  fields.reserve(ln->m_values.size());

  for (size_t i = 0; i < ln->m_values.size(); i++) {
    Node *e = ln->m_values[i];
    String *tag = ln->m_tags[i];
    if (Type t = e->m_type; t != tk_pseudo) {
      fields.emplace_back(tag, t.DropQualifiers());
    } else if (auto list = dyn_cast<List *>(e)) {
      fields.emplace_back(tag, DeduceTupleType(list));
    } else {
      e = e->LowerPseudos(Type());
      ln->m_values[i] = e;
      fields.emplace_back(tag, e->m_type);
    }
  }

  return Type::Tuple(fields);
}

Node *Initializer::ResolveFully_(Context &ctx) {
  // Note: the type may be set after ResolveType but before here.
  verify(m_autoType || m_type.IsKnown());

  Context::PushVoid pv(ctx, false);

  if (m_autoType) {
    if (m_values.size() == 0) {
      m_type = Type::Join(m_type, Type::Void());
    } else {
      Node *&e = m_values[0];
      e = e->ResolveType(ctx);
      Type t = e->m_type;
      if (t == tk_pseudo) {
        if (auto *ln = dyn_cast<List *>(e)) {
          t = DeduceTupleType(ln);
        } else {
          e = e->LowerPseudos(Type());
          t = e->m_type;
        }
      }
      m_type = Type::Join(m_type, t.DropQualifiers());
    }
  }

  for (auto &e : m_values) {
    e = e->ResolveFully(ctx);
    if (e->m_type == tk_valueless)
      m_type = Type::Join(m_type, e->m_type);
  }

  // FIXME: convert conformant array to fixed array when number of elements
  // is known.
  if (m_type == tk_array && !m_type.IndexType().IsKnown() &&
      m_type == rt_rvalue) {
    Integer *zero = Integer::Get(wki_zero);
    Integer *high = Integer::Get((long)m_values.size() - 1);
    m_type = m_type.SetIndexType(Type::IntegerSubrange(zero, high));
  }

  m_deducedType = m_type;

  // If we're initializing a reference type, we need to do some stuff.
  if (m_values.size() == 1 && m_type != rt_rvalue) {
    Node *&value = m_values[0];
    // If the initializing value is an rvalue, then we need to create a temp
    // to hold it.
    if (value->m_type == rt_rvalue) {
      // For non-class values, must cast the rvalue so that it matches the
      // reference type; class objects need no casting so long as it
      // derives from the relevant class.
      // FIXME: must be initializing a const reference!
      // FIXME: rvalue metaclass instances are a no-no.
      Type t = m_type.DropQualifiers();
      if (value->m_type == tk_class) {
        // FIXME: do subclassing check.
        t = value->m_type.DropQualifiers();
      }

      Var *ve = new Var(m_location, t);
      ve->TrackResolution();
	    m_rvalueTemp = new VarDecl(ve, nullptr, value, dm_leaves_full_expr);
      value = (new VarAddr(m_location, ve))->Deref();
    }

    // Switch to initializing a pointer type.
    if (!m_dontLower) {
      m_type = Type::PointerTo(m_type.RValue());
      value = new ::AddrOf(value, false);
      return this->ResolveFully_(ctx); // Redo with new value
    }
  } else if (m_values.size() == 1 && m_type == rt_rvalue) {
    Node *&value = m_values[0];
    // If an rvalue is being initialized with a reference, then, for simple
    // types, we need a Load node.
    Type t = value->m_type;
    if (t != rt_rvalue && t.RValue().IsSimpleType())
      value = new Load(value->m_location, value);
  } else {
    verify(m_type == rt_rvalue);  // FIXME: emit compilation error.
  }

  // If there's one expression in the list, and it has the exact type being
  // constructed, then replace ourselves with it.
  // FIXME: is this *always* the correct behavior?  Regardless, if we don't do
  // this, an infinite loop occurs.
  // FIXME: when adding or dropping constness, should apply operator const and
  // operator mutable for those classes that define it?
  if (m_values.size() == 1) {
    Node *value = m_values[0];
    if (value->m_type == m_type)
      m_replacement = value;
    else if (value->m_type.Const() == m_type)
      m_replacement = value;
    else if (m_type == rt_rvalue && value->m_type == m_type.Const())
      m_replacement = value;
  }

  if (!m_replacement) {
    if (m_type == Type::Suppress()) {
      m_replacement = new Literal(m_location, Value::New(m_type));
    } else {
      // For now, replace with a constructor invocation on the type.  No concepts
      // or comprehensions yet.
      auto rcvr = new Literal(m_location, Value::NewType(m_type));
      auto mn = new Member(m_location, rcvr, false, m_name);
      auto an = new Apply(mn, rcvr, &m_values);
      m_replacement = an;
    }
  }

  if (!m_blockReplacement) {
    // Since no one else is going to do it, put the VarDecl around our
    // replacement.
    if (m_rvalueTemp) {
      m_rvalueTemp->m_expr = m_replacement;
      m_replacement = m_rvalueTemp;
    }

    return m_replacement->ResolveFully(ctx);
  }

  m_replacement = m_replacement->ResolveFully(ctx);
  return this;
}

void Initializer::SetType(Type t) {
  verify(NeedsType());
  m_type = t;
}

void Initializer::SetAutoType() {
  verify(NeedsType());
  m_autoType = true;
}

void Initializer::BindNames(Visitor *v) {
  // Differs from VisitChildren() in that it doesn't visit m_name, which is a
  // member of the field being constructed and thus must not be bound by the
  // symbol table.
  if (m_typeExpr)
    v->Visit(m_typeExpr, ck_type);

  for (size_t i = 0; i < m_values.size(); i++)
    v->Visit(m_values[i], ChildKind(ck_argument + i));
}

void Initializer::VisitChildren(Visitor *v) {
  if (m_typeExpr)
    v->Visit(m_typeExpr, ck_type);

  if (m_name)
    v->Visit(m_name, ck_operand2);

  for (size_t i = 0; i < m_values.size(); i++)
    v->Visit(m_values[i], ChildKind(ck_argument + i));
}

bool Initializer::Dump(BufferWriter &bw, int level, int maxlevel,
                       const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Initializer", desc, descwidth))
    return true;

  if (m_deducedType.IsKnown()) {
    if (bw.Append(" dt:", 4))
      return true;
    if (m_deducedType.Dump(bw))
      return true;
  }

  if (m_autoType && bw.Append(" [autotype]", 11))
    return true;

  if (m_blockReplacement && bw.Append(" [block]", 8))
    return true;

  if (m_dontLower && bw.Append(" [dontlower]", 12))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_name && ((Node *)m_name)->Dump(bw, level, maxlevel, "ident", 5))
      return true;
    if (m_replacement && m_replacement->Dump(bw, level, maxlevel, "repl", 5))
      return true;
    if (m_typeExpr && m_typeExpr->Dump(bw, level, maxlevel, "type", 5))
      return true;
    for (auto val : m_values)
      if (val->Dump(bw, level, maxlevel, "value", 5))
        return true;
  }

  return false;
}
