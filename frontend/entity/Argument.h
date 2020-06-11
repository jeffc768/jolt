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

// An argument entity describes a formal argument to a method.

#pragma once

#include "Var.h"

namespace AST {
  struct MemberItem;
}

enum ArgumentMechanism: uint8_t {
  am_in,
  am_out,
  am_construct
};

class Argument: public Var {
  DECLARE_ENTITY2(Argument, Var)

  // Argument is a specialization of Var, and as such isn't really
  // an entity either.
  // FIXME: there's too much duplicated code between here and Var.  Not
  // clear this subclass is really needed anymore.

protected:
  void DeflateFields(Deflator &DF);
  Argument(Inflator &IF);

public:
  Argument(AST::MemberItem &f, bool rv);

  Argument(Location sl, ArgumentMechanism m, Type t, StringHelper name);

  virtual void BindNames(SymbolTable &st);

  virtual void ResolveFully();

  // Used by Method to plug in the class containing the method.
  void SetType(Type t);

  // A type is known, even if not yet resolved.
  bool HasType();

  virtual VarKinds VarKind() { return vk_argument; }

  ArgumentMechanism   m_mechanism         = am_in;
  bool                m_isArray           = false;

  // Assigned slot in parent method signature.
  int                 m_slot              = -1;

  // Have we been selected to be the return value of the method signature?
  bool                m_isReturned        = false;

  // Is this a deduced return value type?
  bool                m_isDeducedType     = false;
};
