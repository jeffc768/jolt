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
//    // Used by Member instead of ResolveFully for member names.
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

#include "Ident.h"
#include "Apply.h"
#include "Member.h"
#include "OverloadSetRef.h"
#include "entity/Argument.h"
#include "entity/Class.h"
#include "entity/Field.h"
#include "entity/Method.h"
#include "entity/Namespace.h"
#include "entity/OverloadSet.h"
#include "entity/Scope.h"
#include "entity/Undetermined.h"
#include "parser/SymbolTable.h"
#include "parser/Token.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"
#include "util/Value.h"
#include <vector>

IMPLEMENT_NODE(Ident)

Ident::Ident(Token *name)
    : Node(name->GetLocation()),
      m_identifier(name->StringValue()) { }

Ident::Ident(Location sl, StringHelper name)
    : Node(sl),
      m_identifier(name.m_string) { }

Ident::Ident(Ident *macro)
    : Node(macro->m_location),
      m_identifier(macro->m_identifier->AsNormal()) {
  m_entity        = macro->m_entity;
  m_implicitThis  = macro->m_implicitThis;
  m_implicitObj   = macro->m_implicitObj;
  m_pendingClass  = macro->m_pendingClass;
  m_tentative     = macro->m_tentative;
}

void Ident::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_identifier;
  DF << m_entity;
  DF << m_implicitThis;
  DF << m_implicitObj;
  DF << m_pendingClass;
  DF << m_tentative;
  DF << m_expandMacro;
}

Ident::Ident(Inflator &IF)
    : Node(IF) {
  IF >> m_identifier;
  IF >> m_entity;
  IF >> m_implicitThis;
  IF >> m_implicitObj;
  IF >> m_pendingClass;
  IF >> m_tentative;
  IF >> m_expandMacro;
}

Node *Ident::ResolveType_(Context &ctx) {
  verify(!m_expandMacro);  // FIXME: compilation error?

  // We must wait until we got a definitive lookup.
  if (m_pendingClass)
    m_pendingClass->ResolveBases();

  // If no binding found, search used namespaces.
  if (!m_entity) {
    m_implicitThis = nullptr;
    m_implicitObj = nullptr;

    // Collect the scopes of the used namespaces.
    // FIXME: using namespace not implemented yet.
    vector<Namespace *>  usedNamespaces;
    usedNamespaces.push_back(Namespace::StdNamespace());

    for (auto ne : usedNamespaces) {
      if (auto e = ne->LookupEntity(m_identifier)) {
        if (m_entity) {
          // This is legal only if (1) the two entities are identical, or
          // (2) at least one is Nullified entity.
          if (m_entity != e) {
            if (m_entity->Kind() != ek_Nullified) {
              if (e->Kind() != ek_Nullified)
                verify(false); // FIXME: compilation error
            }
          }
        }
        m_entity = e;
      }
    }
  } else {
    // There is an entity; is it a field or method?
    if (m_entity->Kind() == ek_Undetermined) {
      auto ue = safe_cast<Undetermined *>(m_entity);
      ue->ResolveFully();
      m_entity = ue->GetReplacement();
    }

    if (m_implicitThis) {
      // Only fields and methods are implicitly qualified by "this" or
      // equivalent.
      if (auto fe = dyn_cast<Field *>(m_entity)) {
        auto ae = safe_cast<Argument *>(m_implicitThis);
        Type t = ae->m_type;
        auto pce = dyn_cast<Class *>(fe->Parent());
        if (t.Class() != pce) {
          t.Class()->ResolveBases();
          if (t.Class()->PathForField(fe).size() == 0) {
            m_implicitThis = nullptr;
          }
        }

        if (m_implicitObj) {
          ae = safe_cast<Argument *>(m_implicitObj);
          t = ae->m_type;
          auto pce = dyn_cast<Class *>(fe->Parent());
          if (t.Class() != pce) {
            t.Class()->ResolveBases();
            if (t.Class()->PathForField(fe).size() == 0) {
              m_implicitObj = nullptr;
            }
          }
          if (m_implicitObj)
            m_implicitThis = m_implicitObj;
        }
      } else if (auto ose = dyn_cast<OverloadSet *>(m_entity)) {
        (void) ose;
        // FIXME
      } else {
        m_implicitThis = nullptr;
        m_implicitObj = nullptr;
      }

      if (m_implicitThis) {
        Node *rcvr = m_implicitThis->AsValue(m_location);
        Member *mn = new Member(m_location, rcvr, m_identifier, false);
        return mn->ResolveType(ctx);
      }
    }
  }

resolveEntity:
  // Get an expression node that represents the entity.
  if (m_entity == nullptr) {
    EmitError(this) << m_identifier << " not declared.";
    m_type = Type::Suppress();
    ctx.m_error = true;
    return this;
  } else if (m_entity->Kind() == ek_Nullified) {
    EmitError(this) << m_identifier << " has no built definitions.";
    m_type = Type::Suppress();
    ctx.m_error = true;
    return this;
  }

  Node *e = nullptr;
  EntityKind ek = m_entity->Kind();
  if (ek == ek_OverloadSet || ek == ek_Field) {
    // Handle it like any other entity.
    e = m_entity->AsValue(m_location);

    // Remove implicitly generated application.
    if (e->Kind() == nk_Apply) {
      auto an = safe_cast<Apply *>(e);
      e = an->m_functor;
    }
  } else if (ek == ek_Undetermined) {
    auto ue = safe_cast<Undetermined *>(m_entity);
    ue->ResolveFully();
    m_entity = ue->GetReplacement();
    goto resolveEntity;
  } else {
    // Handle non-functions.  If it's a class object, setup our type so that
    // type expressions like ->Class within that class do not block on a
    // circular dependency (and shouldn't, as the class is being used as a
    // type).
    e = m_entity->AsValue(m_location);
#if 0
      if (!m_type) {
        if (m_entity->Kind() == ek_Class) {
          auto ce = safe_cast<Class *>(m_entity);
          m_type = ce->Metaclass()->AsType().LValueRef();
          verify(false); // move to ResolveFully()?
          return this;
        }
      }
#endif
  }

  m_entity = nullptr;
  return e->ResolveType(ctx);
}

Node *Ident::ResolveMember(Context &ctx) {
  // Only something to do for macro names.
  if (!m_expandMacro)
    return this;

  // We must wait until we got a definitive lookup.
  if (m_pendingClass)
    m_pendingClass->ResolveBases();

  verify(m_entity); // FIXME: need a compilation error

  if (m_entity->Kind() == ek_Undetermined) {
    auto ue = safe_cast<Undetermined *>(m_entity);
    ue->ResolveFully();
    m_entity = ue->GetReplacement();
  }

  Context::PushVoid pv(ctx, false);
  m_expr = new Ident(this);
  m_expr = m_expr->ResolveFully(ctx);
  Value *v = m_expr->Evaluate(Type::StringRef());
  m_identifier = v->AsString();
  m_expandMacro = false;
  return this;
}

Node *Ident::ResolveFully_(Context &ctx) {
  // An Ident node generally replaces itself with what it refers to during
  // type resolution, unless there was an error or it's a member.
  return this;
}

void Ident::BindNames(SymbolTable &st, ChildKind ck) {
  // Ignore future bind attempts from class inheritance if we've already been
  // bound to a definitive declaration.
  if (!m_tentative)
    return;

  // Let Member node worry about it if that's our parent--but this is the only
  // time name macros are expanded, and that must be done first.  Furthermore,
  // it might not *really* be a member but a function type construction, so do
  // the lookup anyway (which will be ignored if it really is a member).
  String *name = m_identifier;
  if (ck == ck_member) {
    if (m_identifier->IsNameMacro()) {
      m_expandMacro = true;
      name = name->AsNormal();
    }
  }

  auto lr = st.LookupSymbol(name);

  // Regardless of whether anything was found, take note if the lookup was
  // definitive.
  if (lr.m_tentative) {
    m_pendingClass = st.CurrentPendingClass();
  } else {
    m_tentative = false;
    m_pendingClass = nullptr;
  }

  // If not found, it might be supplied by class inheritance later on.  Or it
  // was already found in an outer scope earlier, and it wasn't overridden by
  // inheritance.
  if (!lr.m_entity)
    return;

  // Assume this is it.  Note that class inheritance may still override this.
  // so long as m_tentative is true.
  m_entity = lr.m_entity;
  m_implicitThis = nullptr;
  m_implicitObj = nullptr;

  // Done if it's not the member of a class.
  Entity *parent = lr.m_entity->Parent();
  if (!parent || parent->Kind() != ek_Class)
    return;

  // May need to replace ourselves with a Member node.  Which receiver depends
  // on whether it's an instance or static member.
  Method *me = st.CurrentMethod();

  if (me && me->Parent()->Kind() == ek_Class) {
    lr = st.LookupSymbol(String::Get(wks_this));
    m_implicitThis = lr.m_entity;

    if (me->MethodKind() == mk_construct) {
      lr = st.LookupSymbol(String::Get(wks_obj));
      m_implicitObj = lr.m_entity;
    }
  }
}

bool Ident::Dump(BufferWriter &bw, int level, int maxlevel,
                 const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Ident", desc, descwidth))
    return true;

  if (bw.Append(' '))
    return true;

  if (bw.Append(m_identifier))
    return true;

  if (m_expandMacro && bw.Append(" [macro]", 8))
    return true;

  if (m_tentative && bw.Append(" [tentative]", 12))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel && m_expr)
    return m_expr->Dump(bw, level, maxlevel, nullptr, 0);

  return false;
}
