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

/******************************************************************************/

// Note: only nodes that are created during AST translation put their children
// on the ready queue in the "created" state.  Other nodes, such as Binary node,
// are presume to be constructed with subexpressions that have already begun
// resolving.  Nodes are not immediately put on the ready queue so that any
// build attributes can be resolved first; build(false) prevents the affected
// subtree from ever getting onto the ready queue.

#include "Expr.h"
#include "Node.h"
#include "Label.h"
#include "Transfer.h"
#include "VarAddr.h"
#include "VarDecl.h"
#include "entity/Var.h"
#include "util/InDeflator.h"
#include <algorithm>

IMPLEMENT_OBJECT(Expr)

void Expr::DeflateFields(Deflator &DF) {
  DF << m_expr;
  DF << m_error;
}

Expr::Expr(Inflator &IF)
    : Object(IF) {
  IF >> m_expr;
  IF >> m_error;
}

Location Expr::GetLocation() {
  return m_expr->m_location;
}

Type Expr::GetType() {
  verify(m_expr->m_type.IsKnown());
  return m_expr->m_type;
}

Type Expr::ResolveType() {
  if (!m_expr->IsTypeResolved()) {
    Node::Context ctx;
    m_expr = m_expr->ResolveType(ctx);
    m_error = ctx.m_error | (m_expr->m_type == Type::Suppress());
  }
  return m_expr->m_type;
}

bool Expr::ResolveFully() {
  if (m_expr->IsFullyResolved())
    return !m_error;

  Node::Context ctx;
  m_expr = m_expr->ResolveFully(ctx);
  m_error |= ctx.m_error;
  return !m_error;
}

namespace {
  class EvaluableVisitor: public Node::Visitor {
    // Keep track of the locals and labels we see as we descend the
    // expression tree.  These may be used; use of any others will prevent
    // evaluability.
    vector<Entity *>   m_locals;
    vector<Label *>    m_labels;

    bool m_diagnose;

  public:
    // The answer is yes until we find evidence to the contrary.
    bool  m_result = true;

    EvaluableVisitor(bool diagnose) : m_diagnose(diagnose) { }

    virtual void Visit(Node *e, Node::ChildKind ck) {
      (void)m_diagnose; // FIXME: actually diagnose errors

      switch (e->Kind()) {
        case nk_GlobalAddr:
          // FIXME: For now, assume any reference to a global (including
          // functions) is evil.
          m_result = false;
          break;

        case nk_VarAddr:
          if (m_locals.end() == find(m_locals.begin(), m_locals.end(),
                                     &*safe_cast<VarAddr *>(e)->m_var))
            m_result = false;
          e->VisitChildren(this);
          break;

        case nk_VarDecl:
          m_locals.push_back(&*safe_cast<VarDecl *>(e)->m_entity);
          e->VisitChildren(this);
          m_locals.pop_back();
          break;

        case nk_Label:
          m_labels.push_back(safe_cast<Label *>(e));
          e->VisitChildren(this);
          m_labels.pop_back();
          break;

        case nk_Transfer:
          if (Label *lbl = safe_cast<Transfer *>(e)->m_label) {
            if (m_labels.end() == find(m_labels.begin(), m_labels.end(), lbl))
              m_result = false;
          } else {
            m_result = false; // FIXME: handle non-labeled transfer
          }
          e->VisitChildren(this);
          break;

        default:
          e->VisitChildren(this);
          break;
      }
    }
  };
}

bool Expr::IsEvaluable(bool diagnose) {
  if (m_evaluable)
    return true;

  if (!ResolveFully())
    return false;

  EvaluableVisitor v(diagnose);
  v.Visit(m_expr, Node::ck_noParent);

  if (v.m_result) {
    m_evaluable = true;
    return true;
  } else {
    m_error = true;
    return false;
  }
}

Value *Expr::Evaluate(Type t) {
  // FIXME: should call IsEvaluable, but can't until it can handle globals.
  if (ResolveFully())
    return m_expr->Evaluate(t);
  else
    return nullptr;
}

Type Expr::EvaluateType() {
  // FIXME: should call IsEvaluable, but can't until it can handle globals.
  if (ResolveFully())
    return m_expr->EvaluateType();
  else
    return Type::Suppress();
}

void Expr::BindNames(SymbolTable &st) {
  m_expr->BindNames(st);
}

namespace messageimpl {
  Location GetLocation(Expr *e) {
    return e->Root()->m_location;
  }
}
