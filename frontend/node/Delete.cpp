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

#include "Delete.h"
#include "Apply.h"
#include "Call.h"
#include "Cast.h"
#include "ExtractAddress.h"
#include "Ident.h"
#include "Load.h"
#include "Member.h"
#include "Sequence.h"
#include "VarDecl.h"
#include "entity/Class.h"
#include "util/BufferWriter.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"

IMPLEMENT_NODE(Delete)

Delete::Delete(Location sl, AST::SafeArray<AST::MemberItem> args, Node *what)
    : Node(sl) {
  // Process arguments to opertor delete method (these are the [args...]
  // immediately following the "delete").
  for (uint32_t i = 0; i < args.size(); i++) {
    m_arguments.push_back(args[i].m_value);
    verify(!args[i].m_attrs);  // FIXME
  }

  m_object = what;

  // Set this up now, so that it can be bound to a global definition.  It might
  // not be used if it turns out there was a class member available.
  String *name = String::Get(wks_op_delete);
  m_op_delete = new Ident(m_location, name);
}

void Delete::DeflateFields(Deflator &DF) {
  Node::DeflateFields(DF);
  DF << m_object;
  DF << m_arguments;
  DF << m_op_delete;
}

Delete::Delete(Inflator &IF)
    : Node(IF) {
  IF >> m_object;
  IF >> m_arguments;
  IF >> m_op_delete;
}

Node *Delete::ResolveType_(Context &ctx) {
  m_object = m_object->ResolveType(ctx);
  if (m_object->m_type == tk_valueless) {
    m_type = m_object->m_type;

    // As we can't go any farther with delete, just type the arguments and bail.
    // In the normal case, the Apply we replace ourselves with will type the
    // arguments.
    for (auto &arg : m_arguments) {
      arg = arg->ResolveType(ctx);
      if (arg->m_type == tk_valueless)
        m_type = Type::Join(m_type, arg->m_type);
    }

    return this;
  }

  m_type = Type::Void();
  return this;
}

Node *Delete::ResolveFully_(Context &ctx) {
  if (m_type == tk_valueless && m_type != Type::Transfers())
    return this;

  Context::PushVoid pv(ctx, false);
  m_object = m_object->ResolveFully(ctx);

  if (m_object->m_type != tk_pointer) {
    verify(false); // FIXME: compilation error
  }

  bool hasDestructor = m_object->m_type.BaseType().HasDestructor();

  if (m_object->m_type != rt_rvalue)
    m_object = new Load(m_object->m_location, m_object);

  TempScope ts;
  ArgAddr addr(ts, m_object->m_location, m_object, m_object->m_type,
               dm_leaves_full_expr);

  // Insert object's address at front of operator delete's argument list.
  // FIXME: supposed to pass size to operator delete.  Have destructor return
  // it?
  Node *obj = hasDestructor ? addr.GetAddr() : static_cast<Node *>(m_object);
  if (!m_object->m_type.IsThin())
    obj = new ExtractAddress(obj);
  obj = new Cast(obj->m_location, Type::Address(), obj);
  m_arguments.insert(m_arguments.begin(), obj);

  // Generate call to operator delete.
  // FIXME: look in metaclass first.
  Node *op_delete = new Apply(m_op_delete, nullptr, &m_arguments);

  Node *e = nullptr;
  if (hasDestructor) {
    e = addr.GetAddr()->Deref();
    Member *f = new Member(m_location, e, wkhs_destructor, false);
    Node *destructor = new Apply(f, e);

    e = new Sequence(destructor, op_delete, false);
    e = ts.Get(e);
  } else {
    e = op_delete;
  }

  return e->ResolveFully(ctx);
}

void Delete::VisitChildren(Visitor *v) {
  if (m_object)
    v->Visit(m_object, ck_operand1);

  for (size_t i = 0; i < m_arguments.size(); i++)
    v->Visit(m_arguments[i], ChildKind(ck_argument + i));

  v->Visit(m_op_delete, ck_operand2);
}

bool Delete::Dump(BufferWriter &bw, int level, int maxlevel,
                  const char *desc, int descwidth) {
  if (DumpCommon(bw, level, "Delete", desc, descwidth))
    return true;

  if (bw.Append('\n'))
    return true;

  if (++level < maxlevel) {
    if (m_object->Dump(bw, level, maxlevel, "obj", 3))
      return true;
    for (auto arg : m_arguments)
      if (arg->Dump(bw, level, maxlevel, "arg", 3))
        return true;
  }

  return false;
}
