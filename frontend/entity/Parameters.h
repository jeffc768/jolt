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

// Parameters is a special-purpose scope that holds the generic parameter values
// when a generic entity derivation is expanded into a new set of work units.

#pragma once

#include "util/Object.h"

class Deflator;
class Entity;
class Inflator;
class Scope;
class String;
class SymbolTable;

class Parameters: public Object {
  DECLARE_OBJECT(Parameters)

protected:
  void DeflateFields(Deflator &DF);
  Parameters(Inflator &IF);

public:
  Parameters(Entity *body);

  void BindNames(SymbolTable &st);

  // Parameters must be reserved before deflated copy of template is made.
  bool ReserveParameter(String *name);
  void AddParameter(String *name, Object *value);

  Entity *GetBody() { return m_body; }

  void Setup();

private:
  Scope              *m_scope       = nullptr;
  Entity             *m_body        = nullptr;
};
