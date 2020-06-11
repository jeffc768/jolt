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

// A String represents an identifier or string literal from the lexer.  String
// objects are unique; two pointers to Strings are equal if and only if they
// refer to the exact same sequence of characters.

#pragma once

#include "Object.h"
#include <string>

class SHA512;

enum WellKnownString: int {
  wks_atbuild,
  wks_atstatic,
  wks_op_count,
  wks_op_rem,
  wks_op_rem_assign,
  wks_op_bitand,
  wks_op_bitand_assign,
  wks_op_apply,
  wks_op_mul,
  wks_op_mul_assign,
  wks_op_add,
  wks_op_post_incr,
  wks_op_pre_incr,
  wks_op_add_assign,
  wks_op_sub,
  wks_op_post_decr,
  wks_op_pre_decr,
  wks_op_sub_assign,
  wks_op_deref,
  wks_op_range,
  wks_op_div,
  wks_op_div_assign,
  wks_op_assign,
  wks_op_lt,
  wks_op_lshift,
  wks_op_lshift_assign,
  wks_op_le,
  wks_op_ne,
  wks_op_eq,
  wks_op_gt,
  wks_op_ge,
  wks_op_rshift,
  wks_op_rshift_assign,
  wks_op_index,
  wks_op_bitxor,
  wks_op_bitxor_assign,
  wks_op_const,
  wks_op_delete,
  wks_op_in,
  wks_op_mutable,
  wks_op_new,
  wks_op_not,
  wks_op_bitor,
  wks_op_bitor_assign,
  wks_op_bitnot,
  wks_class,
  wks_codefragment_t,
  wks_default,
  wks_infinity,
  wks_max,
  wks_min,
  wks_nan,
  wks_null,
  wks_obj,
  wks_ord,
  wks_that,
  wks_this,
  wks_type,
  wks_BuiltinPointer,
  wks_ConformantArray,
  wks_FixedArray,
  wks_Set,
  wks_SignedSubrange,
  wks_UnsignedSubrange,
  wks_CharacterSubrange,
  wks_Pointer,

  // Well known hidden strings.  These can never be referenced in Jolt code,
  // even using $-macros.
  wkhs_destructor,
  wkhs_op_generic,
  wkhs_op_global,
  wkhs_op_macro_insert,
  wkhs_op_macro_iter,
  wkhs_op_move,
  wkhs_op_ptr,
  wkhs_op_ref,
  wkhs_op_r_ref,
  wkhs_op_set,
  wkhs_vtbl,

  wks_max_wks,
  wkhs_first_wkhs = wkhs_destructor,
};

class String: public Object {
  // We need a custom version of this, unfortunately, so that the destructor
  // is accessible to HashTable<String>.
  //DECLARE_OBJECT(String)
  friend Metadata;

  enum category_t: uint8_t { cat_normal, cat_hidden, cat_namemacro };

  uint32_t            m_size;
  category_t          m_category;
  char                m_text[3];

protected:
  void DeflateFields(Deflator &DF);
  static String *Inflate(Inflator &IF);

  virtual size_t GetObjectSize();
  Object *GetObjectBase() { return this; }

  String() : m_size(0), m_category(cat_normal) { m_text[0] = 0; }
  String(const char *s, uint32_t len);
  String(uint32_t size) : m_size(size) { }

public:
  using base_t = Object;
  static Metadata s_metadata;
  virtual Metadata &GetMetadata();

  struct HashTrait;
  friend struct HashTrait;

  static String *Get(const char *s);
  static String *Get(const char *s, size_t size);
  static String *Get(const std::string &s);
  static String *Get(WellKnownString wks);

  String *AsNormal();
  String *AsNameMacro();
  bool IsNameMacro() { return m_category == cat_namemacro; }

  const char *c_str() const { return m_text; }
  uint32_t Length() { return m_size; }
  char operator[](uint32_t index) { return m_text[index]; }

  String *Concat(String *rhs);
  String *Concat(const char *s, uint32_t len);

  void AppendToHash(SHA512 &hash);
};


inline bool operator==(String *s, WellKnownString wks) {
  return s == String::Get(wks);
}

inline bool operator!=(String *s, WellKnownString wks) {
  return s != String::Get(wks);
}
