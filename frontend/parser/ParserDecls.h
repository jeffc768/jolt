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

// Declarations that need to be included before the Bison-generated union
// is seen.

#pragma once

#include "util/Verify.h"
#include <stdlib.h>
#include <stdint.h>
#include <utility>

class AttributeList;
class Entity;
class Node;
class Field;
class Token;

enum class unop: char {
  bitnot,
  boolnot,
  const_,
  count,
  deref,
  global,
  lref,
  macroinsert,
  macroiter,
  move,
  mutable_,
  neg,
  pos,
  postdecr,
  postincr,
  predecr,
  preincr,
  ptr,
  rref,
  set_
};

enum class binop: char {
  add,
  addassign,
  apply,
  as,
  assign,
  bitand_,
  bitandassign,
  bitor_,
  bitorassign,
  bitxor,
  bitxorassign,
  booland,
  boolor,
  closure,
  derefstar,
  derive,
  div,
  divassign,
  dotstar,
  eq,
  forward,
  ge,
  gt,
  in,
  le,
  lshift,
  lshiftassign,
  lt,
  mod,
  modassign,
  mul,
  mulassign,
  ne,
  range,
  rshift,
  rshiftassign,
  select,
  sub,
  subassign,
  subscript
};

namespace AST {
  // Values on Bison's parser stack are members of a union, constructors and
  // destructors not allowed; hence, this bastardized version of stl::vector.
  template<class T> struct BisonArray {
  protected:
    T                  *m_values;
    uint32_t            m_capacity;
    uint32_t            m_size;

  public:
    uint32_t size() { return m_size; }
    T &operator[](uint32_t idx) { verify(idx < m_size); return m_values[idx]; }

    T *begin() { return m_values; }
    T *end() { return m_values + m_size; }
    T &back() { return m_values[m_size - 1]; }

    static BisonArray Empty() {
      BisonArray ary;
      ary.m_values = nullptr;
      ary.m_capacity = 0;
      ary.m_size = 0;
      return ary;
    }

    static BisonArray New() {
      BisonArray ary;
      ary.m_capacity = 6;
      ary.m_values = (T *)malloc(sizeof(T) * ary.m_capacity);
      ary.m_size = 0;
      return ary;
    }

    template<class U> static BisonArray New(U&& init) {
      BisonArray ary;
      ary.m_capacity = 6;
      ary.m_values = (T *)malloc(sizeof(T) * ary.m_capacity);
      ary.m_size = 1;
      new(&ary.m_values[0]) T(std::forward<U>(init));
      return ary;
    }

    template<class U> BisonArray Append(U&& value) {
      BisonArray ary = *this;

      if (m_capacity == m_size) {
        ary.m_capacity *= 2;
        ary.m_values = (T *)malloc(sizeof(T) * ary.m_capacity);
        for (uint32_t i = 0; i < m_size; i++)
          new(&ary.m_values[i]) T(std::move(m_values[i]));
        release();
      }

      new(&ary.m_values[ary.m_size++]) T(std::forward<U>(value));
      return ary;
    }

    BisonArray Append(BisonArray &other) {
      BisonArray self = *this;
      for (uint32_t i = 0; i < other.size(); i++)
        self = self.Append(other[i]);
      other.release();
      return self;
    }

    void release() {
      for (uint32_t i = 0; i < m_size; i++)
        m_values[i].~T();
      free(m_values);
      m_values = nullptr;
      m_capacity = 0;
      m_size = 0;
    }
  };

  // A wrapper around BisonArray<T> that destructs.  Methods that take an
  // array use this so that cleanup is always done.
  template<class T> class SafeArray {
    BisonArray<T>   &m_array;

    static BisonArray<T> s_empty;

  public:
    SafeArray(BisonArray<T> &rhs) : m_array(rhs) { }
    SafeArray(std::nullptr_t) : m_array(s_empty) { }
    ~SafeArray() { m_array.release(); }

    uint32_t size() { return m_array.size(); }
    T &operator[](uint32_t idx) { return m_array[idx]; }

    T *begin() { return m_array.begin(); }
    T *end() { return m_array.end(); }

    const T *begin() const { return m_array.begin(); }
    const T *end() const { return m_array.end(); }
  };

  template<class T> BisonArray<T> SafeArray<T>::s_empty;

  // Value of a tuple member grammar rule, which is also used for parameter
  // lists, etc.
  struct MemberItem {
    Node               *m_value;
    Token              *m_name;
    AttributeList      *m_attrs;
    Token              *m_ellipses;
    bool                m_trailingComma;
    // FIXME: diagnose incorrect trailing stuff or ellpises as syntax errors

    static MemberItem New() {
      return { nullptr, nullptr, nullptr, nullptr, false };
    }
  };

  // Value of an ElseIf grammar rule.
  struct IfClause {
    Token              *m_keyword;
    Node               *m_cond;
    Node               *m_body;
  };

  // Value of a VarName grammar rule.
  struct VarName {
    Token              *m_name;
    bool                m_isConst;
    bool                m_isRef;
  };

  // Value of a VarDecl grammar rule.
  struct VarInfo {
    BisonArray<VarName> m_name;
    Node               *m_type;
    Node               *m_init;
  };

  // Value of a BaseSpecifier grammar rule.
  struct BaseInfo {
    Node               *m_type;
    AttributeList      *m_attrs;
  };

  // Value of a TemplateParam grammar rule.
  struct TemplateInfo {
    Node               *m_type;
    Token              *m_name;
    Node               *m_init;
    bool                m_isVariadic;
  };

  // Value of a TemplateClause grammar rule.
  struct TemplateClause {
    Token              *m_keyword;
    BisonArray<TemplateInfo> m_parms;
  };

  // Value of a ProcBody grammar rule.
  struct ProcBody {
    enum Keyword { pb_none, pb_expr, pb_deferred, pb_delete, pb_default};

    Keyword             m_keyword;
    Node               *m_body;
  };

  // Value of a NamespaceDecl grammar rule.
  struct DeclInfo;
  struct NamespaceInfo {
    BisonArray<Token *>    m_names;
    BisonArray<DeclInfo>   m_decls;
  };

  // Value of a ImportDecl grammar rule.
  struct ImportInfo {
    BisonArray<Token *>    m_namespaceNames;
    BisonArray<Token *>    m_moduleNames;
  };

  // Value of a Declaration grammar rule.
  struct DeclInfo {
    enum DeclType {
      dt_empty,
      dt_normal,
      dt_fields,
      dt_attrgroup,
      dt_namespace,
      dt_import
    };

    DeclType               m_decltype;
    AttributeList         *m_attrs;

    union {
      Entity              *m_normal;
      BisonArray<Field *>  m_fields;
      NamespaceInfo        m_namespace;
      ImportInfo           m_import;
    };

    static DeclInfo NewEmpty() {
      return { dt_empty, nullptr, { nullptr } };
    };

    static DeclInfo NewNormal(Entity *e) {
      DeclInfo di { dt_normal, nullptr, { nullptr } };
      di.m_normal = e;
      return di;
    }

    static DeclInfo NewFields(BisonArray<Field *> fs) {
      DeclInfo di { dt_fields, nullptr, { nullptr } };
      di.m_fields = fs;
      return di;
    }

    static DeclInfo NewAttrGroup() {
      DeclInfo di { dt_attrgroup, nullptr, { nullptr } };
      return di;
    }

    static DeclInfo NewNamespace(NamespaceInfo ni) {
      DeclInfo di { dt_namespace, nullptr, { nullptr } };
      di.m_namespace = ni;
      return di;
    }

    static DeclInfo NewImport(ImportInfo ii) {
      DeclInfo di { dt_import, nullptr, { nullptr } };
      di.m_import = ii;
      return di;
    }
  };

  struct AttrInfo {
    enum AttrType {
      at_normal,
      at_indirect,
      at_conditional
    };

    using sublist_t = BisonArray<AttrInfo>;

    AttrType               m_attrtype;
    uint32_t               m_truecount;
    Token                 *m_start;
    Node                  *m_expr;
    sublist_t              m_sublist;

    static AttrInfo NewNormal(Token *t, Node *e) {
      return { at_normal, 0, t, e, { } };
    }

    static AttrInfo NewIndirect(Token *t, Node *e) {
      return { at_indirect, 0, t, e, { } };
    }

    static AttrInfo NewConditional(Token *t, Node *e,
                                   sublist_t ts, sublist_t fs) {
      return { at_conditional, ts.size(), t, e, ts.Append(fs)};
    }
  };

  struct FuncInfo {
    enum throws_t : uint8_t { fi_none, fi_bool, fi_expr };
    enum ref_t : uint8_t { ref_unspec, ref_lvalue, ref_rvalue };

    bool                   m_const;
    ref_t                  m_ref;
    throws_t               m_throws;
    Node                  *m_throwsExpr;

    static FuncInfo New() { return { false, ref_unspec, fi_none, nullptr }; }
  };
}
