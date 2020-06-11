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

#include "If.h"
#include "Label.h"
#include "Literal.h"
#include "Transfer.h"
#include "parser/Token.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Value.h"

IMPLEMENT_NODE(If)

If::If(AST::IfClause &main, AST::SafeArray<AST::IfClause> elseifs,
       AST::IfClause &els)
    : Node(main.m_keyword->GetLocation()),
      m_condition(main.m_cond),
      m_ifTrue(main.m_body) {
  // Do the else if clauses, building a series of If nodes chained through
  // the m_ifFalse field.
  If *parent = this;
  for (AST::IfClause& ic : elseifs) {
    If *in = new If(ic, parent);
    parent->m_ifFalse = in;
    parent = in;
  }

  // Finally, handle the optional else clause.
  if (els.m_keyword)
    parent->m_ifFalse = els.m_body;
  else
    parent->m_ifFalse = new Literal(m_location, Value::New(Type::Void()));
}

// else if clause
If::If(AST::IfClause &ic, If *parent)
    : Node(ic.m_keyword->GetLocation()),
      m_condition(ic.m_cond),
      m_ifTrue(ic.m_body),
      m_elseif(true) { }

// ? operator
If::If(Location sl, Node *cond, Node *t, Node *f)
    : Node(sl),
      m_condition(cond),
      m_ifTrue(t),
      m_ifFalse(f) { }

void If::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_condition;
  DF << m_ifTrue;
  DF << m_ifFalse;
  DF << m_label;
  DF << m_elseif;
}

If::If(Inflator &IF)
    : Node(IF) {
  IF >> m_condition;
  IF >> m_ifTrue;
  IF >> m_ifFalse;
  IF >> m_label;
  IF >> m_elseif;
}

Node *If::ResolveType_(Context &ctx) {
  // Walk down the else if chain.
  If *in = this;
  while (true) {
    in->m_condition = in->m_condition->UseCondition(ctx);
    in->m_ifTrue = in->m_ifTrue->ResolveType(ctx);

    if (auto els = dyn_cast<If *>(in->m_ifFalse); !els || !els->m_elseif)
      break;
    else
      in = els;
    in->m_state = st_type_resolved;
  }

  in->m_ifFalse = in->m_ifFalse->ResolveType(ctx);
  ComputeType();
  return this;
}

Node *If::ResolveFully_(Context &ctx) {
  if (ctx.m_void)
    m_type = Type::Void();

  {
    Context::PushVoid pv(ctx, false);
    m_condition = m_condition->ResolveFully(ctx);
  }

  {
    Context::PushVoid pv(ctx, m_type == tk_void);
    m_ifTrue = m_ifTrue->ResolveFully(ctx);
    m_ifFalse = m_ifFalse->ResolveFully(ctx);
  }

  return this;
}

void If::ComputeType() {
  // Do a join of all the clauses' types.  Note that the types of any
  // transfer nodes must have been resolved by now too.  Even though the type
  // of a Transfer node is not used by its immediate parent, it still must
  // determine it; and as type resolution is a bottom-up process, and all
  // Transfer nodes pointing to this node (parent Label node, actually) are
  // in a subtree... QED.
  if (m_type == tk_void)
    return;

  vector<Node **> parts;
  If *in = this;
  Type t;
  while (true) {
    // Propagate valueless types from m_condition.
    // FIXME: this can get very messy.. an exit in the condition is not the
    // same as an exit in the body.  Handles them wrong.
    if (in->m_condition->m_type == tk_valueless)
      t = Type::Join(t, in->m_condition->m_type);

    parts.push_back(&in->m_ifTrue);

    if (in->m_ifFalse->Kind() != nk_If)
      break;

    If *els = safe_cast<If *>(in->m_ifFalse);
    if (!els->m_elseif)
      break;

    in = els;
  }

  // Do final else clause.
  parts.push_back(&in->m_ifFalse);

  m_type = DetermineStatementType(t, parts, m_label);

  // Propagate the type to all the chained else if nodes.
  in = this;
  while (true) {
    in->m_type = m_type;
    if (in->m_ifFalse->Kind() != nk_If)
      break;

    If *els = safe_cast<If *>(in->m_ifFalse);
    if (!els->m_elseif)
      break;

    in = els;
  }
}

void If::VisitChildren(Visitor *v) {
  v->Visit(m_condition, ck_condition);
  v->Visit(m_ifTrue, ck_ifTrue);
  v->Visit(m_ifFalse, ck_ifFalse);
}

bool If::Dump(BufferWriter &bw, int level, int maxlevel,
              const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "If", desc, descwidth))
    return true;

  if (m_elseif && bw.Append(" [elseif]", 9))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_condition && m_condition->Dump(bw, level, maxlevel, "cond", 5))
      return true;
    if (m_ifTrue && m_ifTrue->Dump(bw, level, maxlevel, "true", 5))
      return true;
    if (m_ifFalse && m_ifFalse->Dump(bw, level, maxlevel, "false", 5))
      return true;
  }

  return false;
}
