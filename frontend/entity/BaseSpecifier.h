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

// A BaseSpecifier represents an inheritance from another class.

#pragma once

#include "util/Object.h"
#include <string>

namespace AST {
  struct BaseInfo;
}

class AttributeList;
class Class;
class Expr;
class String;
class SymbolTable;

class BaseSpecifier: public Object {
  DECLARE_OBJECT(BaseSpecifier)

protected:
  void DeflateFields(Deflator &DF);
  BaseSpecifier(Inflator &IF);

public:
  BaseSpecifier(AST::BaseInfo &bi);
  BaseSpecifier(BaseSpecifier *bs);
  BaseSpecifier(Class *ce);

  void BindNames(SymbolTable &st);

  void ResolveFully();
  bool IsNotBuilt();

  Class              *m_baseClass     = nullptr;
  AttributeList      *m_attributes    = nullptr;
  Expr               *m_baseClassExpr = nullptr;

  // Mangled C++ name for this base.
  std::string         m_externalName;

  // Offset of this base class relative to the start of the inheriting class.
  size_t              m_offset        = 0;

  // Ordinal of pseudo-field representing this base class within the
  // inheriting class.
  size_t              m_ord           = 0;

  // Offset of this class's vtable within the inheriting class's vtable.
  size_t              m_vtblOffset    = 0;
};
