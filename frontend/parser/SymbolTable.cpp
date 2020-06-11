// Copyright (c) 2018, Jeff Cohen
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

#include "SymbolTable.h"
#include "entity/Class.h"
#include "entity/Scope.h"
#include "node/Label.h"
#include "util/String.h"
#include "util/Verify.h"

SymbolTable::SymbolTable(uint16_t ipcnt, bool redo)
      : m_inheritancePendingCnt(ipcnt),
        m_redo(redo) {
  m_bucketCnt = 4096;
  m_buckets = new uint32_t[m_bucketCnt];
  for (uint32_t i = 0; i < m_bucketCnt; i++)
    m_buckets[i] = EMPTY;

  m_entries.reserve(4096);
  m_hidden.reserve(4096);
}

SymbolTable::~SymbolTable() {
  delete[] m_buckets;
  m_buckets = nullptr;
}

void SymbolTable::AddSymbol(String *n, Entity *e) {
  if (auto r = HashName(n); r.second) {
    uint32_t idx = m_buckets[r.first];
    auto &ent = m_entries[idx];
    m_hidden.emplace_back(ent.m_entity, ent.m_inheritancePendingCnt, idx);
    ent.m_entity = e;
    ent.m_inheritancePendingCnt = m_inheritancePendingCnt;
  } else {
    if (m_buckets[r.first] == DELETED)
      m_deletedCnt--;

    m_buckets[r.first] = (uint32_t)m_entries.size();
    m_entries.emplace_back(n, e, m_inheritancePendingCnt);

    if (m_entries.size() > (m_bucketCnt >> 2))
      GrowHashTable(2);
    else if (m_deletedCnt >> (m_bucketCnt >> 3))
      GrowHashTable(1);
  }
}

void SymbolTable::AddSymbols(Scope *s) {
  s->EnumerateMembers([this](String *n, Entity *e) {
    AddSymbol(n, e);
  });
}

SymbolTable::LSResult SymbolTable::LookupSymbol(String *n) {
  if (auto r = HashName(n); r.second) {
    auto &e = m_entries[m_buckets[r.first]];
    unsigned ipcnt = m_inheritancePendingCnt - e.m_inheritancePendingCnt;
    return { e.m_entity, ipcnt > 0 };
  } else {
    return { nullptr, m_inheritancePendingCnt > 0 };
  }
}

SymbolTable::LLResult SymbolTable::LookupLabel(String *n, TransferKind tk) {
  bool nonlocal = false;

  for (size_t i = m_labels.size(); i > 0; ) {
    auto ln = m_labels[--i];
    bool found;

    if (n) {
      switch (tk) {
        case tk_exit:   found = ln->DefinesLabel(n); break;
        case tk_next:   found = ln->DefinesLabel(n); break;
        case tk_return: found = ln->IsMethod(n);     break;
      }
    } else {
      switch (tk) {
        case tk_exit:   found = ln->IsExitLegal();   break;
        case tk_next:   found = ln->IsNextLegal();   break;
        case tk_return: found = ln->IsReturnLegal(); break;
      }
    }

    if (found)
      return { ln, nonlocal };

    if (ln->IsReturnLegal())
      nonlocal = true;
  }

  return { nullptr, nonlocal };
}

inline std::pair<uint32_t, bool> SymbolTable::HashName(String *name) {
  verify(name);
  uint32_t mask = m_bucketCnt - 1;
  uintptr_t n = reinterpret_cast<uintptr_t>(name);
  uint32_t idx = mask & (uint32_t)(n ^ (n >> 17));

  while (m_buckets[idx] < EMPTY) {
    if (m_entries[m_buckets[idx]].m_name == name)
      return { idx, true };
    idx = (idx + 1) & mask;
  }

  if (m_buckets[idx] == EMPTY)
    return { idx, false };

  // Remember the first DELETED slot we see; that's what we'll return if we
  // don't find the name.
  uint32_t del = idx;

  while (true) {
    idx = (idx + 1) & mask;
    if (m_buckets[idx] == EMPTY)
      return { del, false };
    if (m_buckets[idx] != DELETED)
      if (m_entries[m_buckets[idx]].m_name == name)
        return { idx, true };
  };
}

void SymbolTable::popScope(size_t esz, size_t hsz) {
  // Restore declaration hidden by this scope.
  size_t sz = m_hidden.size();
  while (sz > hsz) {
    auto &h = m_hidden[--sz];
    auto &e = m_entries[h.m_index];
    e.m_entity = h.m_entity;
    e.m_inheritancePendingCnt = h.m_inheritancePendingCnt;
  }

  // Delete from hash table any names introduced by this scope.
  sz = m_entries.size();
  while (sz > esz) {
    auto &e = m_entries[--sz];
    auto r = HashName(e.m_name);
    verify(r.second && sz == m_buckets[r.first]);

    uint32_t idx = r.first;
    m_buckets[idx] = DELETED;
    m_deletedCnt++;

    // A DELETED followed by an EMPTY can be replaced by EMPTY.
    uint32_t mask = m_bucketCnt - 1;
    if (m_buckets[(idx + 1) & mask] == EMPTY) {
      do {
        m_buckets[idx] = EMPTY;
        m_deletedCnt--;
        idx = (idx - 1) & mask;
      } while (m_buckets[idx] == DELETED);
    }
  }

  // Restore symbol table to previous condition.
  m_hidden.resize(hsz);
  m_entries.resize(esz);
}

void SymbolTable::GrowHashTable(uint32_t factor) {
  m_deletedCnt = 0;

  if (factor > 1) {
    m_bucketCnt *= factor;
    delete[] m_buckets;
    m_buckets = new uint32_t[m_bucketCnt];
  }

  for (uint32_t i = 0; i < m_bucketCnt; i++)
    m_buckets[i] = EMPTY;

  for (size_t i = 0; i < m_entries.size(); i++) {
    auto r = HashName(m_entries[i].m_name);
    verify(!r.second);
    m_buckets[r.first] = uint32_t(i);
  }
}
