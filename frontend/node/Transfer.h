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

// A Transfer node represents a non-local transfer of control, namely an exit,
// next, return, or yield statement.  It references the Label node whose
// execution is being terminated.  Transfering to an enclosing method is
// permitted.

#pragma once

#include "Node.h"
#include "parser/SymbolTable.h"

class Token;

class Transfer: public Node {
  DECLARE_NODE(Transfer)

protected:
  void DeflateFields(Deflator &DF);
  Transfer(Inflator &IF);

  virtual Node *ResolveType_(Context &ctx);
  virtual Node *ResolveFully_(Context &ctx);

  virtual bool Dump(BufferWriter &bw, int level, int maxlevel,
                    const char *desc, int descwidth);

public:
  Transfer(Location sl, TransferKind tk, Node *value, Token *lbl, Node *guard);
  Transfer(TransferKind tk, Node *value);

  void ComputeType();

  void BindNames(SymbolTable &st, Visitor *v);

  virtual void VisitChildren(Visitor *v);

  TransferKind        m_kind        = tk_exit;
  String             *m_labelName   = nullptr; // null for unlabeled blocks
  Label              *m_label       = nullptr;
  Node               *m_expr        = nullptr;
  Type                m_exprType;
  Node               *m_guard       = nullptr;
  bool                m_nonlocal    = false;   // is target in enclosing method?
};
