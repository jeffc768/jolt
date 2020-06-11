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

#include "Argument.h"
#include "FormalArguments.h"
#include "Method.h"
#include "parser/Location.h"
#include "parser/Token.h"
#include "util/InDeflator.h"
#include "util/Message.h"
#include "util/String.h"

IMPLEMENT_OBJECT(FormalArguments)

FormalArguments::FormalArguments(Location sl,
                                 AST::SafeArray<AST::MemberItem> formals,
                                 AST::MemberItem rettype,
                                 bool exprBody)
    : m_location(sl) {
  if (rettype.m_value) {
    m_returnValue = new Argument(rettype, true);
  } else if (exprBody) {
    m_deducedReturnType = true;
    m_returnValue = new Argument(rettype, true);
  }

  // Run through the arguments, appending them to their respective arrays.
  // FIXME: verify all have names.
  Token *ellipses = nullptr;
  for (auto& f : formals) {
    if (ellipses) {
      EmitError(ellipses) << "Ellipses must be at end of parameter list.";
      ellipses = nullptr;
    }

    if (f.m_ellipses) {
      ellipses = f.m_ellipses;
      continue;
    }

    // FIXME f.m_isArray;
    Argument *ae = new Argument(f, false);
    verify(ae->m_mechanism == am_in);
    m_arguments.push_back(ae);
  }

  if (ellipses)
    m_variadic = vt_c;
}

void FormalArguments::DeflateFields(Deflator &DF) {
  DF << m_location;
  DF << m_arguments;
  DF << m_returnValue;
  DF << m_type;
  DF << m_hasThis;
  DF << m_hasStaticLink;
  DF << m_variadic;
  DF << m_deducedReturnType;
  DF << m_nSlots;
}

FormalArguments::FormalArguments(Inflator &IF)
    : Object(IF) {
  IF >> m_location;
  IF >> m_arguments;
  IF >> m_returnValue;
  IF >> m_type;
  IF >> m_hasThis;
  IF >> m_hasStaticLink;
  IF >> m_variadic;
  IF >> m_deducedReturnType;
  IF >> m_nSlots;
}

void FormalArguments::AddImplicitArguments(MethodKind kind) {
  // For an argument whose type is the class that contains this method, that
  // type will have to be inserted later and will be null for now.  This is
  // because we are running before a pristine copy is made for generic classes.
  // The "this" member cannot be used because it will not be correct for meta-
  // classes.  The type is set in the MakeInstance/MakeStatic methods of
  // Method.

  if (kind == mk_construct) {
    Argument *ae = new Argument(m_location, am_construct, Type(),
                                String::Get(wks_obj));
    verify(!m_returnValue);
    m_returnValue = ae;

    // FIXME: add arguments for vtable and mostDerived.
  } else if (kind == mk_destruct) {
    // FIXME: add argument for mostDerived.
  }

  m_hasThis = true;
  Argument *ae = new Argument(m_location, am_in, Type(), String::Get(wks_this));
  m_arguments.insert(m_arguments.begin(), ae);
}

void FormalArguments::BindNames(SymbolTable &st) {
  for (auto arg : m_arguments)
    arg->BindNames(st);

  if (m_returnValue)
    m_returnValue->BindNames(st);
}

void FormalArguments::ResolveFully() {
  if (m_returnValue && m_returnValue->HasType()) {
    m_returnValue->ResolveFully();

    if (m_returnValue->m_type == tk_void)
      m_returnValue = nullptr;
  }

  for (auto arg : m_arguments)
    arg->ResolveFully();

  // Construct official function type.
  Type rt = m_returnValue ? m_returnValue->m_type : Type::Void();
  vector<Type> args;
  for (auto arg : m_arguments)
    args.push_back(arg->m_type);
  m_type = Type::Function(rt, args, m_hasThis);
}

int FormalArguments::Compare(FormalArguments *other) {
  // Note: while this function compares for equivalent signatures, it does not
  // check if one signature legally redefines another differing yet equivalent
  // signature; e.g., an argument has the same type in both signatures, but has
  // a different mechanism.  This is an illegal re-definition, but they are
  // still equivalent.  If they weren't equivalent, it wouldn't be a
  // redefinition in the first place.

  // First, compare number of "in" arguments.  "Out" arguments do not figure
  // in signature equivalence.
  if (m_arguments.size() < other->m_arguments.size())
    return -1;
  else if (m_arguments.size() > other->m_arguments.size())
    return 1;

  // Account for the implicit arguments that do not participate in the
  // signature, such as "this" arguments.
  size_t skip = ImplicitArgCount();
  verify(m_hasThis == other->m_hasThis);

  // Second, compare the types of the "in" arguments.
  for (size_t i = skip; i < m_arguments.size(); i++) {
    Argument *a1 = m_arguments[i];
    Argument *a2 = other->m_arguments[i];

    if (a1->m_type < a2->m_type)
      return -1;
    else if (a1->m_type != a2->m_type)
      return 1;
  }

  // They are equivalent.
  return 0;
}

size_t FormalArguments::ImplicitArgCount() {
  return unsigned(m_hasThis) + unsigned(m_hasStaticLink);
}
