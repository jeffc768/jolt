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

#include "DependencyFinder.h"
#include "entity/Class.h"
#include "entity/Method.h"
#include "node/GlobalAddr.h"
#include "node/VarDecl.h"
#include "node/Vtable.h"
#include "node/VtableSlot.h"

DependencyFinder::DependencyFinder(vector<Entity *> &dependencies)
    : m_dependencies(dependencies) { }

void DependencyFinder::AddDependency(Entity *e) {
  verify(e);
  if (!m_seen.count(e)) {
    m_dependencies.push_back(e);
    m_seen[e] = true;
  }
}

void DependencyFinder::Visit(Node *e, Node::ChildKind ck) {
  // FIXME: we may need to handle nodes that require the size of a class
  // type, but probably not.  The use of the variable involves using
  // methods that will drag the class in anyway.
  switch (e->Kind()) {
    case nk_GlobalAddr: {
      auto gan = safe_cast<GlobalAddr *>(e);
      Entity *e = gan->m_entity;
      if (e->Kind() == ek_Class)
        e = static_cast<Class *>(e)->Metaclass();
      AddDependency(e);
      break;
    }

    case nk_VarDecl: {
      // Be sure that the class of a local variable is marked as a
      // dependency.
      auto vd = safe_cast<VarDecl *>(e);
      Type t = vd->VariableType().Lower();
      if (t == tk_class) {
        Class *ce = t.Class();
        AddDependency(ce);
      }
      break;
    }

    case nk_Vtable: {
       auto vn = safe_cast<Vtable *>(e);
       Class *ce = vn->m_class;
       AddDependency(ce);
       break;
    }

    case nk_VtableSlot: {
       auto vsn = safe_cast<VtableSlot *>(e);
       Class *ce = vsn->m_class;
       AddDependency(ce);
       Method *me = ce->GetMethodForVtblSlot(vsn->m_slot);
       AddDependency(me);
       break;
    }

    default:
      break;
  }
  e->VisitChildren(this);
}
