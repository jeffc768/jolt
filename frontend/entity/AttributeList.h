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

// An attribute list represents a collection of metadata that annotates an
// entity or statement.

#pragma once

#include "parser/Location.h"
#include "parser/ParserDecls.h"
#include "util/Object.h"
#include <unordered_map>

class Attribute;
class Class;
class Deflator;
class Inflator;
class Location;
class SymbolTable;
class Value;

namespace AST {
  struct AttrInfo;
}

using std::unordered_map;

class AttributeList: public Object {
  DECLARE_OBJECT(AttributeList)

  bool                m_hasStatic   = false;
  bool                m_resolved    = false;

protected:
  void DeflateFields(Deflator &DF);
  AttributeList(Inflator &IF);

public:
  AttributeList(Location sl, AST::SafeArray<AST::AttrInfo> attrs);

  void BindNames(SymbolTable &st);

  void ResolveFully();

  bool HasStatic();
  bool IsFullyResolved() { return m_resolved; }
  bool GetBuildValue();

  Value *GetAttribute(int id);

  Location            m_location;
  AttributeList      *m_parent      = nullptr;
  Attribute          *m_first       = nullptr;
  unordered_map<Class *, Value *> m_bag;
};
