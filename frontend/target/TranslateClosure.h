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

// Tracks down all entities that used, directly or indirectly, starting from an
// expression or a specific method (usually Main), and ensures that they are all
// passed on to a target for translation.

#pragma once

#include <vector>
#include <unordered_set>
#include <stdint.h>

class Entity;
class Expr;

enum Epoch: uint8_t;

using std::vector;

class TranslateClosure {

public:
  TranslateClosure(Epoch ep);
  TranslateClosure(Epoch ep, Expr *e);

  Epoch GetEpoch() { return m_epoch; }

  bool InClosure(Entity *e);
  bool AddToClosure(Entity *e);
  void AddToClosure(vector<Entity *> &es);

  void Finish();

  bool HasUnbound() { return m_closureHasUnbound; }
  bool HasError() { return m_closureHasError; }

private:
  Epoch                        m_epoch;

  // Entities the expression is dependent on, directly or indirectly, that we
  // have encountered and have yet to become or be known to be executable.
  vector<Entity *>             m_pending;

  // All entities that have already been seen and appended to m_pending.
  std::unordered_set<Entity *> m_seen;

  bool                m_closureHasUnbound   = false;
  bool                m_closureHasError     = false;
};
