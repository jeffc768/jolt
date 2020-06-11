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

#include "Parameters.h"
#include "Const.h"
#include "Undetermined.h"
#include "parser/SymbolTable.h"
#include "util/InDeflator.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_OBJECT(Parameters)

Parameters::Parameters(Entity *body)
    : m_scope(new Scope()),
      m_body(body) { }

void Parameters::DeflateFields(Deflator &DF) {
  DF << m_scope;
  DF << m_body;
}

Parameters::Parameters(Inflator &IF) : Object(IF) {
  IF >> m_scope;
  IF >> m_body;
}

void Parameters::BindNames(SymbolTable &st) {
  SymbolTable::PushTemplate tmpl(st, this);
  SymbolTable::PushScope scope(st);
  st.AddSymbols(m_scope);

  m_scope->BindNames(st);
  m_body->BindNames(st);
}

bool Parameters::ReserveParameter(String *name) {
  Const *ce = new Const(nullptr, name, Value::New(Type::Void()));
  return m_scope->AddEntity(name, ce);
}

void Parameters::AddParameter(String *name, Object *value) {
  Entity *e = m_scope->RawGet(name);
  auto ce = safe_cast<Const *>(e);
  ce->UpdateValue(value);
}

void Parameters::Setup() {
  m_scope->HandleNameMacros();
  m_scope->ResolveFully();
  m_body->TrackResolution();
}
