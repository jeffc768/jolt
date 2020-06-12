// Copyright (c) 2015, Jeff Cohen
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

#include "Field.h"
#include "AttributeList.h"
#include "Class.h"
#include "node/Expr.h"
#include "node/GlobalAddr.h"
#include "node/Load.h"
#include "node/Initializer.h"
#include "node/Shared.h"
#include "node/VarDecl.h"
#include "parser/ParserDecls.h"
#include "parser/Token.h"
#include "target/Target.h"
#include "target/TranslateClosure.h"
#include "util/InDeflator.h"
#include "util/SHA512.h"
#include "util/String.h"

/******************************************************************************/

IMPLEMENT_ENTITY(Field)

Field::Field(AST::VarInfo &vi)
    : Entity(nullptr, vi.m_name[0].m_name->GetLocation(), vi.m_name[0].m_name),
      m_typeExpr(vi.m_type ? new Expr(vi.m_type) : nullptr) {
  verify(vi.m_name.size() == 1);
  Initializer *in;

  if (vi.m_init) {
    in = dyn_cast<Initializer *>(vi.m_init);
    if (!in) {
      Location sl = vi.m_init->m_location;
      if (!sl)
        sl = vi.m_name[0].m_name->GetLocation();
      in = new Initializer(sl, vi.m_init, true);
    }

    in->BlockReplacement();
    m_initexpr = new Expr(in);
  }
}

Field::Field(Entity *parent, StringHelper name, Type t)
    : Entity(parent, { }, name),
      m_type(t) {
  auto in = new Initializer({ }, t, nullptr, true);
  m_initexpr = new Expr(in);
}

void Field::DeflateFields(Deflator &DF) {
  Entity::DeflateFields(DF);

  DF << WX(m_state);
  DF << m_ord;

  if (m_state == st_resolved) {
    DF << m_type;
    DF << m_offset;

    // FIXME: What about namespace globals in epoch compile?  It's just an
    // external reference for epoch runtime...
  } else {
    DF << m_initexpr;
    DF << m_typeExpr;

    verify(m_state == st_initial);
    verify(!m_type);
    verify(!m_offset);
    verify(!m_runTarget);
    verify(!m_compileTarget);
    verify(!m_sharedInit);
  }
}

Field::Field(Inflator &IF)
    : Entity(IF) {
  IF >> WX(m_state);
  IF >> m_ord;

  if (m_state == st_resolved) {
    IF >> m_type;
    IF >> m_offset;
  } else {
    IF >> m_initexpr;
    IF >> m_typeExpr;
  }
}

void Field::BindNames(SymbolTable &st) {
  Entity::BindNames(st);

  if (m_typeExpr)
    m_typeExpr->BindNames(st);

  if (m_initexpr)
    m_initexpr->BindNames(st);
}

Node *Field::GetInitializer() {
  return m_sharedInit ? new Shared(m_initexpr) : m_initexpr->Root();
}

void Field::ResolveType() {
  if (m_state >= st_type_known)
    return;

  if (AttributeList *al = Attributes())
    al->ResolveFully();

  if (m_typeExpr)
    m_type = m_typeExpr->EvaluateType();

  if (m_initexpr) {
    if (auto in = dyn_cast<Initializer *>(m_initexpr->Root())) {
      if (in->NeedsType()) {
        if (m_typeExpr) {
          in->SetType(m_type);
        } else {
          in->SetAutoType();
          m_initexpr->ResolveFully();
          m_type = in->m_deducedType;
        }
      }
    }
  } else {
    Location sl = m_typeExpr->Root()->m_location;
    auto in = new Initializer(sl, m_type, nullptr, true);
    m_initexpr = new Expr(in);
  }

  m_state = st_type_known;
}

void Field::ResolveFully() {
  if (m_state == st_resolved)
    return;

  ResolveType();

  // Class member:
  //
  //   The resolved initializer will be picked up by construct statements,
  //   implicitly generated or explicit, and inserted into VarDecl nodes.
  //
  // Namespace member or shared static field:
  //
  //   If the type is literable, evaluate the initializer at compile time if
  //   possible and arrange for the field to be initialized at load time.
  //   Otherwise, arrange for the initializer to be executed at program
  //   startup.

  m_initexpr->ResolveFully();

  if (auto in = dyn_cast<Initializer *>(m_initexpr->Root())) {
    verify(m_type == in->m_deducedType);
    m_initexpr = new Expr(in->m_replacement);
    m_sharedInit = true;

    // FIXME: these should result in compilation errors.
    verify(!in->m_rvalueTemp);
  }

  m_state = st_resolved;
}

Node *Field::AsValue(Location sl) {
  verify(Parent()->Kind() == ek_Namespace);
  ResolveFully();
  if (m_type == rt_rvalue) {
    return (new GlobalAddr(sl, this, m_type))->Deref();
  } else {
    Node *e = new GlobalAddr(sl, this, Type::PointerTo(m_type.RValue()));
    e = new Load(sl, e);
    return e->Deref();
  }
}

void *Field::GlobalStorage(Epoch ep) {
  verify(Parent()->Kind() == ek_Namespace);
  return GetTarget(ep)->GlobalStorage(ep);
}

const std::string &Field::GlobalStorageName() {
  verify(Parent()->Kind() == ek_Namespace);
  return ExternalName();
}

Entity::ResolutionState Field::GetResolutionState() {
  verify(m_state == st_resolved);
  if (m_type == Type::Unbound())
    return rs_hasUnbound;
  else if (m_type == Type::Suppress())
    return rs_hasError;
  else
    return rs_ok;
}

void Field::AddToClosure(TranslateClosure *tc) {
  verify(Parent()->Kind() == ek_Namespace);
  if (tc->GetEpoch() == ep_compile) {
    if (!m_compileTarget)
      m_compileTarget = Target::Get(ep_compile)->For(this);
  } else {
    if (!m_runTarget)
      m_runTarget = Target::Get(ep_run)->For(this);
  }

  // Register our dependencies.
  if (m_type == tk_class)
    tc->AddToClosure(m_type.Class());
}

void Field::FinalizeClosure(TranslateClosure *tc) {
  if (tc->GetEpoch() == ep_compile) {
    m_compileTarget->Generate();
  } else {
    m_runTarget->Generate();
  }
}

size_t Field::GetOrdinal() {
  verify(Parent()->Kind() == ek_Class);
  return m_ord;
}

void Field::SetOrdinal(size_t ord) {
  verify(Parent()->Kind() == ek_Class);
  m_ord = ord;
}

size_t Field::GetOffset() {
  verify(Parent()->Kind() == ek_Class);
  return m_offset;
}

void Field::SetOffset(size_t offset) {
  verify(Parent()->Kind() == ek_Class);
  m_offset = offset;
}

TargetGlobal *Field::GetTarget(Epoch ep) {
  if (ep == ep_compile) {
    if (!m_compileTarget)
      m_compileTarget = Target::Get(ep_compile)->For(this);
    return m_compileTarget;
  } else {
    if (!m_runTarget)
      m_runTarget = Target::Get(ep_run)->For(this);
    return m_runTarget;
  }
}

void Field::AppendToHash(SHA512 &hash) {
  hash.Append('F');
  m_type.AppendToHash(hash);
}
