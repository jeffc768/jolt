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

// An Apply node represents functional application, including methods calls and
// operators.  It drives argument resolution, selecting a signature based on the
// argument types, and finally replacing itself with the lowered equivalent.

#pragma once

#include "Node.h"
#include "parser/ParserDecls.h"
#include <string>
#include <vector>

class Argument;
class Method;
class NativeOperator;

enum class binop: char;
enum class unop: char;

using std::vector;

class Apply: public Node {
  DECLARE_NODE(Apply)

protected:
  struct ArgsHelper {
    vector<Node *>        m_values;
  };

  Apply(Location sl, Node *functor, Node *rcvr, NativeOperator *no,
        ArgsHelper &ah);

  void DeflateFields(Deflator &DF);
  Apply(Inflator &IF);

  virtual Node *ResolveType_(Context &ctx);
  virtual Node *ResolveFully_(Context &ctx);

  virtual bool Dump(BufferWriter &bw, int level, int maxlevel,
                    const char *desc, int descwidth);

public:
  Apply(Location sl, unop opcode, Node *arg);
  Apply(Location sl, binop opcode, Node *arg1, Node *arg2);
  Apply(Location sl, binop opcode, Node *functor,
        AST::SafeArray<AST::MemberItem> args);

  template<typename... ARGS>
  Apply(Node *functor, Node *rcvr, ARGS... args)
      : Apply(functor->m_location, functor, rcvr, nullptr,
              NewHelper(ArgsHelper(), args...)) { }

  template<typename... ARGS>
  Apply(Location sl, NativeOperator *no, Node *rcvr, ARGS... args)
      : Apply(sl, nullptr, rcvr, no, NewHelper(ArgsHelper(), args...)) { }

  void ResetNativeOperator(NativeOperator *no, size_t argcnt);

  virtual void VisitChildren(Visitor *v);

  // The argument list.  The m_argkinds array tells how to interpret the
  // corresponding element of m_arguments.
  Node               *m_functor         = nullptr;
  Node               *m_receiver        = nullptr;
  vector<Node *>      m_arguments;

  // These get filled in from different sources, depending on the nature of
  // the functor.
  vector<Type>        m_formalTypes;
  vector<bool>        m_passThrough;
  Method             *m_method          = nullptr;
  Type                m_receiverType;

  // Track all pseudo-valued Literal nodes that supply pseudo-valued types to
  // the arguments.  m_pseudoLiterals holds all of them, while m_pseudoBase
  // points at the first one for each argument.  m_pseudoBase has one more
  // value than the number of arguments, which is equal to the size of
  // m_pseudoLiterals.
  vector<Node *>      m_pseudoLiterals;
  vector<size_t>      m_pseudoBases;

private:
  static ArgsHelper &NewHelper(ArgsHelper &&ah) { return ah; }

  template<typename... ARGS>
  static ArgsHelper &NewHelper(ArgsHelper &&ah, const vector<Node *> *values,
                               ARGS... args) {
    ah.m_values.insert(ah.m_values.end(), values->begin(), values->end());
    return NewHelper(std::move(ah), args...);
  }

  template<typename... ARGS>
  static ArgsHelper &NewHelper(ArgsHelper &&ah, Node *value, ARGS... args) {
    ah.m_values.insert(ah.m_values.end(), value);
    return NewHelper(std::move(ah), args...);
  }

  // Helper method to wrap arguments (and receiver) in Initializer nodes.
  // Returns false if an error occurred.
  bool WrapArguments(Context &ctx, bool blockReplacement);

  NativeOperator     *m_macro           = nullptr;
  vector<Argument *>  m_argEntities;
  Argument           *m_returnValue     = nullptr;
};
