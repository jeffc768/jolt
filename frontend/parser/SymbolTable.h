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

/******************************************************************************/

// The symbol table keeps track of in-scope declarations as a parse tree
// is visited.

#pragma once

#include <cstddef>
#include <cstdint>
#include <vector>

class Class;
class Entity;
class Label;
class Method;
class Parameters;
class Scope;
class String;

enum TransferKind: uint8_t { tk_exit, tk_next, tk_return };

using std::vector;

class SymbolTable {
  SymbolTable(const SymbolTable &) = delete;
  SymbolTable &operator=(const SymbolTable &) = delete;

public:
  SymbolTable(uint16_t ipcnt = 0, bool redo = false);
  ~SymbolTable();

  // Is this symbol table in response to a class completing inheritance?
  // Note: if so, then the symbol table excludes anything above the class.
  bool IsRedo() { return m_redo; }

  // Add name/entity to symbol table, hiding any current declaratin for name.
  void AddSymbol(String *name, Entity *e);
  void AddSymbols(Scope *s);

  // Query symbol table.
  struct LSResult {
    // The found entity, if not null.  But if the lookup was tentative, then
    // this is the class whose inheritance must happen first.
    Entity                     *m_entity;

    // Whether this lookup is tentative because a class above us has yet to
    // do inheritance.
    bool                        m_tentative;
  };

  LSResult LookupSymbol(String *name);

  struct LLResult {
    Label                      *m_label;
    bool                        m_nonlocal;
  };

  LLResult LookupLabel(String *name, TransferKind tk);

  Class *CurrentClass() {
    return m_classes.empty() ? nullptr : m_classes.back();
  }

  Class *CurrentPendingClass() {
    return m_pendingClass;
  }

  uint16_t GetInheritancePendingCount() { return m_inheritancePendingCnt; }

  Method *CurrentMethod() {
    return m_methods.empty() ? nullptr : m_methods.back();
  }

  Parameters *CurrentTemplate() {
    return m_templates.empty() ? nullptr : m_templates.back();
  }

  // Begin new scope.  Anything changed after this point is undone when this
  // object destructs.
  class PushScope {
    SymbolTable                &m_symtab;

    // Curent sizes of m_entries and m_hidden at construction time.
    size_t                      m_entriesSize;
    size_t                      m_hiddenSize;

  public:
    PushScope(SymbolTable &st)
        : m_symtab(st),
          m_entriesSize(st.m_entries.size()),
          m_hiddenSize(st.m_hidden.size()) { }

    ~PushScope() {
      m_symtab.popScope(m_entriesSize, m_hiddenSize);
    }
  };

  class PushClass {
    SymbolTable                &m_symtab;
    Class                      *m_savedPenndingClass;
    bool                        m_inheritancePending;

  public:
    PushClass(SymbolTable &st, Class *ce, bool ip)
        : m_symtab(st),
          m_savedPenndingClass(st.m_pendingClass),
          m_inheritancePending(ip) {
      m_symtab.m_classes.push_back(ce);
      if (m_inheritancePending) {
        m_symtab.m_inheritancePendingCnt++;
        m_symtab.m_pendingClass = ce;
      }
    }

    ~PushClass() {
      m_symtab.m_classes.pop_back();
      m_symtab.m_inheritancePendingCnt -= m_inheritancePending;
      m_symtab.m_pendingClass = m_savedPenndingClass;
    }
  };

  // Currently nearest class with inheritance pending.
  Class                        *m_pendingClass    = nullptr;


  class PushMethod {
    SymbolTable                &m_symtab;

  public:
    PushMethod(SymbolTable &st, Method *me) : m_symtab(st) {
      m_symtab.m_methods.push_back(me);
    }

    ~PushMethod() {
      m_symtab.m_methods.pop_back();
    }
  };

  // Begin new label scope.
  class PushLabel {
    SymbolTable                &m_symtab;

  public:
    PushLabel(SymbolTable &st, Label *label)
        : m_symtab(st) {
      m_symtab.m_labels.push_back(label);
    }

    ~PushLabel() {
      m_symtab.m_labels.pop_back();
    }
  };

  // When deflating a template definition, any entities declared above this
  // will be excluded from the blob.
  class PushTemplate {
    SymbolTable                &m_symtab;

  public:
    PushTemplate(SymbolTable &st, Parameters *root)
          : m_symtab(st) {
      m_symtab.m_templates.push_back(root);
    }

    ~PushTemplate() {
      m_symtab.m_templates.pop_back();
    }
  };

private:
  struct HashEntry {
    String                     *m_name;
    Entity                     *m_entity;
    uint16_t                    m_inheritancePendingCnt;

    HashEntry()
        : m_name(nullptr), m_entity(nullptr), m_inheritancePendingCnt(0) { }
    HashEntry(String *n, Entity *e, uint16_t ipcnt)
        : m_name(n), m_entity(e), m_inheritancePendingCnt(ipcnt) { }
  };

  // The hash table buckets have indices into this vector.  When a name is added
  // (without hiding a previous declaration), it is pushed onto this vector; and
  // when it goes out of scope, it's popped.
  vector<HashEntry>             m_entries;

  // Keep track of all hidden declarations.
  struct Hidden {
    // Hidden declaration last associated with the name.
    Entity                     *m_entity;
    uint16_t                    m_inheritancePendingCnt;

    // Entity name's index into m_entries.
    uint32_t                    m_index;

    Hidden()
        : m_entity(nullptr), m_inheritancePendingCnt(0), m_index(0) { }
    Hidden(Entity *e, uint16_t ipcnt, uint32_t idx)
        : m_entity(e), m_inheritancePendingCnt(ipcnt), m_index(idx) { }
  };

  vector<Hidden>                m_hidden;

  // Hash table of all symbols currently in scope.
  uint32_t                     *m_buckets         = nullptr;
  uint32_t                      m_bucketCnt       = 0;
  uint32_t                      m_deletedCnt      = 0;

  // Special bucket values.  (EMPTY < DELETED must hold true.)
  constexpr static const uint32_t EMPTY     = 0xfffffffe;
  constexpr static const uint32_t DELETED   = 0xffffffff;

  std::pair<uint32_t, bool> HashName(String *n);
  void popScope(size_t esz, size_t hsz);
  void GrowHashTable(uint32_t factor);

  // Labels currently in scope.
  vector<Label *>               m_labels;

  // Class stack.
  vector<Class *>               m_classes;

  // Number of classes in the stack that may inherit and have yet to do so.
  uint16_t                      m_inheritancePendingCnt = 0;

  // Method stack.
  vector<Method *>              m_methods;

  // Template root stack.
  vector<Parameters *>          m_templates;

  // Is this symbol table in response to class completing inheritance?
  bool                          m_redo            = false;
};
