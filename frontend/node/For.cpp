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

#include "For.h"
#include "Binary.h"
#include "ExtractDescriptor.h"
#include "Index.h"
#include "Label.h"
#include "Literal.h"
#include "Load.h"
#include "Sequence.h"
#include "Store.h"
#include "Transfer.h"
#include "VarDecl.h"
#include "While.h"
#include "entity/Var.h"
#include "parser/SymbolTable.h"
#include "parser/Token.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/String.h"
#include "util/Value.h"

IMPLEMENT_NODE(For)

For::For(Location sl, StringHelper kind, AST::SafeArray<AST::VarName> vars,
         Node *value, Node *extra, Node *body, Node *els)
    : Node(sl),
      m_kind(kind),
      m_value(value),
      m_extra(extra),
      m_body(body) {
  if (els)
    m_else = els;
  else
    m_else = new Literal(sl, Value::New(Type::Void()));

  // Prohibit use of currently unimplemented functionality.
  verify(!m_kind);
  verify(!m_extra);
  verify(vars.size() == 1);

  m_indvars.reserve(vars.size());
  for (auto &v : vars) {
    // Type of induction variables must be plugged in later.
    auto ve = new Var(v.m_name->GetLocation(), v.m_name, nullptr);
    verify(!ve->Name()->IsNameMacro()); // FIXME
    m_indvars.emplace_back(ve, v.m_isConst, v.m_isRef);
  }
}

void For::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_indvars;
  DF << m_kind;
  DF << m_value;
  DF << m_extra;
  DF << m_body;
  DF << m_else;
  DF << m_label;
}

void operator<<(Deflator &DF, const For::IndVar &iv) {
  DF << iv.m_var;
  DF << iv.m_isConst;
  DF << iv.m_isRef;
}

For::For(Inflator &IF)
    : Node(IF) {
  IF >> m_indvars;
  IF >> m_kind;
  IF >> m_value;
  IF >> m_extra;
  IF >> m_body;
  IF >> m_else;
  IF >> m_label;
}

void operator>>(Inflator &IF, For::IndVar &iv) {
  IF >> iv.m_var;
  IF >> iv.m_isConst;
  IF >> iv.m_isRef;
}

Node *For::ResolveType_(Context &ctx) {
  m_value = m_value->ResolveType(ctx);
  if (m_value->m_type == tk_valueless)
    m_type = m_value->m_type;

  if (m_extra) {
    m_extra = m_extra->ResolveType(ctx);
    if (m_extra->m_type == tk_valueless)
      m_type = Type::Join(m_type, m_extra->m_type);
  }

  if (m_value->m_type == tk_valueless)
    return HandleValueless(ctx);
  else if (m_value->m_type == tk_array)
    return HandleArrays(ctx);

  // Prohibit use of currently unimplemented functionality.
  verify(false);
}

Node *For::ResolveFully_(Context &ctx) {
  // We never get here unless an error occured, otherwise we'd be replaced by
  // a While node.
  verify(m_type == tk_valueless || m_type == tk_void);
  return this;
}

Node *For::HandleArrays(Context &ctx) {
  // Create induction variable.
  Type indtype = Type::SizeT();
  if (intptr_t card = m_value->m_type.IndexType().Cardinality(); card >= 0)
    if (card < 0x7fffffff)
      indtype = Type::Int();

  Var *indve = new Var(m_location, indtype);
  Node *zero = new Literal(m_location, Value::New(indtype));
  VarDecl *indvd = new VarDecl(indve, nullptr, zero, dm_never);

  // Grab reference to aggregate value.
  Type aggtype = m_value->m_type.LValueRef();
  Var *aggve = new Var(m_location, aggtype);
  VarDecl *aggvd = new VarDecl(aggve, indvd, m_value, dm_never);

  // Generate loop termination test.
  auto getsize = [&]() -> Node * {
    if (Type t = aggtype.IndexType(); t)
      return new Literal(m_location, Value::NewInt(indtype, t.Cardinality()));
    else
      return new ExtractDescriptor((aggve->AsValue(m_location))->AddrOf(false));
  };
  Node *ln1 = new Load(m_location, indve->AsValue(m_location));
  Node *sz = getsize();
  Node *cond = new Binary(m_location, Binary::op_setlt, ln1, sz);

  // Generate induction increment.
  Node *ln2 = new Load(m_location, indve->AsValue(m_location));
  Node *one = new Literal(m_location, Value::NewInt(indtype, 1));
  Node *add = new Binary(m_location, Binary::op_add, ln2, one);
  Node *next = new Store(m_location, indve->AsValue(m_location), add);

  // Provide access to current array element.
  Node *ln3 = new Load(m_location, indve->AsValue(m_location));
  Node *idx = (new Index(m_location, aggve->AsValue(m_location), ln3))->Deref();

  IndVar &elem = m_indvars.back();
  Type t = aggtype.ElementType();

  if (elem.m_isConst) {
    t = t.Const();
  } else {
    if (!elem.m_isRef)
      t = t.Mutable();
  }

  if (elem.m_isRef) {
    t = t.LValueRef();
    if (aggtype == ct_const)
      t = t.Const();
  }

  elem.m_var->m_type = t;
  Node *elemvd = new VarDecl(elem.m_var, m_body, idx, dm_leaves_scope);

  // Put it all together.
  indvd->m_expr = GenerateWhile(ctx, cond, elemvd, next);
  return aggvd->ResolveType(ctx);
}

Node *For::HandleValueless(Context &ctx) {
  // Something went wrong.  The induction variables cannot be typed.
  for (auto v : m_indvars)
    v.m_var->m_type = Type::Suppress();

  // Generate the while loop anyway, for the purposes of identifying
  // additional errors.
  Node *cond = new Literal(m_location, Value::NewBool(false));
  Node *w = GenerateWhile(ctx, cond, m_body, nullptr);
  return w->ResolveType(ctx);
}

Node *For::GenerateWhile(Context &ctx, Node *cond, Node *body, Node *next) {
  auto wn = new While(m_location, cond, body, m_else, next);
  wn->m_label = m_label;
  return wn;
}

void For::BindNames(Visitor *v, SymbolTable &st) {
  v->Visit(m_value, ck_forValue);

  if (m_extra)
    v->Visit(m_extra, ck_forExtra);

  if (m_else)
    v->Visit(m_else, ck_operand2);

  // Add the induction variables to the symbol table, but just for the loop
  // body.
  SymbolTable::PushScope scope(st);
  for (auto v : m_indvars)
    st.AddSymbol(v.m_var->Name(), v.m_var);

  v->Visit(m_body, ck_operand1);
}

void For::ComputeType() {
  verify(m_type == tk_valueless || m_type == tk_void);
}

void For::VisitChildren(Visitor *v) {
  v->Visit(m_value, ck_forValue);
  v->Visit(m_body, ck_operand1);

  if (m_extra)
    v->Visit(m_extra, ck_forExtra);

  if (m_else)
    v->Visit(m_else, ck_operand2);
}

bool For::Dump(BufferWriter &bw, int level, int maxlevel,
               const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "For", desc, descwidth))
    return true;

  if (bw.Append(' '))
    return true;

  if (m_kind) {
    if (bw.Append(m_kind))
      return true;
    if (bw.Append('('))
      return true;
  }

  bool first = true;
  for (auto &iv : m_indvars) {
    if (first) {
      if (bw.Append(','))
        return true;
      first = false;
    }

    if (bw.Append(iv.m_var->Name()))
      return true;
    if (iv.m_isConst && bw.Append(" const", 6))
      return true;
    if (iv.m_isRef && bw.Append('&'))
      return true;
  }

  if (m_kind && bw.Append(')'))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_value->Dump(bw, level, maxlevel, "value", 5))
      return true;
    if (m_extra && m_extra->Dump(bw, level, maxlevel, "extra", 5))
      return true;
    if (m_body->Dump(bw, level, maxlevel, "body", 5))
      return true;
    if (m_else && m_else->Dump(bw, level, maxlevel, "else", 5))
      return true;
  }

  return false;
}
