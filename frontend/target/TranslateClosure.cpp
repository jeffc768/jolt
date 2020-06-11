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

#include "TranslateClosure.h"
#include "DependencyFinder.h"
#include "entity/Class.h"
#include "node/Expr.h"
#include "node/Node.h"
#include "target/Target.h"

TranslateClosure::TranslateClosure(Epoch ep)
    : m_epoch(ep) {
}

TranslateClosure::TranslateClosure(Epoch ep, Expr *e)
    : m_epoch(ep) {
  if (!e->ResolveFully()) {
    m_closureHasError = true;
    return;
  }

  // Check for other conditions that block translation.
  Type t = e->ResolveType();
  if (t == Type::Unbound()) {
    m_closureHasUnbound = true;
    return;
  } else if (t == Type::Suppress()) {
    m_closureHasError = true;
    return;
  } else if (t == Type::Transfers()) {
    m_closureHasError = true;
    return;
    // FIXME: some error message?
  }

  // Find the dependencies of the expression.
  vector<Entity *> dependencies;
  DependencyFinder df(dependencies);
  df.Visit(e->Root(), Node::ck_operand1);
  AddToClosure(dependencies);

  if (t == tk_class)
    AddToClosure(t.Class());
}

void TranslateClosure::Finish() {
  for (size_t i = 0; i < m_pending.size(); i++) {
    Entity *e = m_pending[i];
    e->ResolveFully();

    // Check for conditions that block translation.
    switch (e->GetResolutionState()) {
      case Entity::rs_hasUnbound:
        m_closureHasUnbound = true;
        continue;
      case Entity::rs_hasError:
        m_closureHasError = true;
        continue;
      case Entity::rs_ok:
        break;
    }

    e->AddToClosure(this);
  }

  if (m_closureHasUnbound || m_closureHasError) {
    // Closure failed to translate.
    return;
  }

  // Finalize all members of the closure (which generally means go and generate
  // code).  We can also mark them as executable if this is the epoch compile
  // closure.  We finalize in reverse order of discovery, though there isn't
  // really a good reason for doing so, but in general do finalize an entity
  // after its dependents have been finalized.
  for (size_t i = m_pending.size(); i > 0; ) {
    Entity *e = m_pending[--i];
    e->FinalizeClosure(this);
    if (m_epoch == ep_compile)
      e->SetExecutable();
  }

  // FIXME: What about run time closures?  There are two reasons to do this for
  // run time.  First, to decide what to emit into the C++ file, and second to
  // collect data for some optimizations regarding vtable and inlining and
  // stuff.
}

bool TranslateClosure::InClosure(Entity *e) {
  return m_seen.find(e) != m_seen.end();
}

bool TranslateClosure::AddToClosure(Entity *e) {
  verify(e);
  auto I = m_seen.insert(e);
  if (I.second)
    m_pending.push_back(e);
  return I.second;
}

void TranslateClosure::AddToClosure(vector<Entity *> &es) {
  for (Entity *e : es)
    AddToClosure(e);
}
