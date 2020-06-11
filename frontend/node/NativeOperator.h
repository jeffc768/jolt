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

// A NativeOperator implements an intrinsic operation, such as integer addition
// or array subscripting, as a macro that replaces itself with the appropriate,
// lowered code.

#pragma once

#include "Apply.h"

enum MethodKind: uint8_t;

class NativeOperator {
  const char          *m_name;

public:
  using Context = Node::Context;

  NativeOperator(const char *name) : m_name(name) { }
  virtual ~NativeOperator() { }

  virtual ::MethodKind MethodKind();
  const char *Name() { return m_name; }

  // Take the operands from the Apply node and return a new expression subtree
  // that will replace the Apply node.
  virtual Node *Run(Apply *an, Context &ctx) = 0;

  // Verify that the argument types are valid for the operator.  Returns the
  // type the expansion would have.  Also populates the m_arg*Type and
  // m_argIsLValue arrays in the passed Apply node.
  // Note: if the returned type is Type(), then Run() must be called ASAP; the
  // returned subexpression must then be typed.
  virtual Type ResolveTypes(Apply *an, Context &ctx) = 0;
};
