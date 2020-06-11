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

// A Call node represents a lowered function/method call.  The argument list is
// in ABI form, with implicit arguments made explicit.  The call has a result
// only when an out argument is left out of the argument list.  The callee may
// or may not be capable of raising an exception.

#pragma once

#include "Node.h"

class Argument;
class Method;

class Call: public Node {
  DECLARE_NODE(Call)

protected:
  void DeflateFields(Deflator &DF);
  Call(Inflator &IF);

  virtual Node *ResolveType_(Context &ctx);
  virtual Node *ResolveFully_(Context &ctx);

  virtual bool Dump(BufferWriter &bw, int level, int maxlevel,
                    const char *desc, int descwidth);

public:
  Call(Location sl, Method *me, vector<Argument *> &args,
       Argument *returnValue, Node *func);

  void AppendArg(Node *e);

  virtual void VisitChildren(Visitor *v);

  struct Slot {
    Node *m_arg = nullptr;

    Slot() = default;
    Slot(const Slot&) = default;
    Slot(Node *n) : m_arg(n) { }
  };

  void GetSlotOrdering(vector<Slot> &args, Slot &rv);

  Node                 *m_function          = nullptr;
  Method               *m_method            = nullptr;
  vector<Argument *>    m_arguments;
  Argument             *m_returnValue       = nullptr;
  vector<Node *>        m_operands;
};
