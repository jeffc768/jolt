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

#include "String.h"
#include "Deduplicator.h"
#include "InDeflator.h"
#include "SHA512.h"

// We need a custom version of this, unfortunately.
//IMPLEMENT_OBJECT_NOSIZE(String)

struct String::HashTrait {
  using ELEM = char;
  using SIZE = uint32_t;

  // Declared length of m_text, minus one for added zero terminator.
  static const int base = 2;

  static void New(String &data, const char *elem, uint32_t size) {
    new(&data) String(elem, size);
  }

  static size_t Size(String &data) { return data.Length(); }
  static const char *Data(String &data) { return data.c_str(); }
};

static Deduplicator<String> g_dedups[3];

Metadata String::s_metadata { &Object::s_metadata };

Metadata &String::GetMetadata() {
  return s_metadata;
}

size_t String::GetObjectSize() {
  // Allow for the fact that sizeof(String) includes space for three chars
  // and that m_size doesn't include zero terminator.

  return sizeof(String) + (m_size - 2);
}

void String::DeflateFields(Deflator &DF) {
  DF << m_size;
  DF << m_category;
  DF << std::make_pair(&m_text[0], m_size);
}

String *String::Inflate(Inflator &IF) {
  uint32_t size;
  IF >> size;

  category_t cat;
  IF >> cat;

  vector<char> value(size);
  IF >> std::make_pair(value.data(), size);

  String *obj = g_dedups[cat].FindOrAdd(value.data(), size);
  obj->m_category = cat;
  IF.RegisterObject(obj);
  return obj;
}

String::String(const char *s, uint32_t len)
    : m_size(len),
      m_category(cat_normal) {
  memcpy(m_text, s, m_size);
  m_text[len] = 0;
}

String *String::Get(const char *s) {
  return Get(s, uint32_t(strlen(s)));
}

String *String::Get(const char *s, size_t size) {
  String *str = g_dedups[cat_normal].FindOrAdd(s, uint32_t(size));
  str->m_category = cat_normal;
  return str;
}

String *String::Get(const std::string &s) {
  return Get(s.c_str(), s.size());
}

String *String::Concat(String *rhs) {
  if (rhs->m_size == 0)
    return this;
  else if (m_size == 0)
    return rhs;

  return Concat(rhs->c_str(), rhs->Length());
}

String *String::Concat(const char *p, uint32_t len) {
  if (len == 0)
    return this;

  char *q = reinterpret_cast<char *>(alloca(m_size + len + 1));
  memcpy(q, m_text, m_size);
  memcpy(q + m_size, p, len + 1);

  return Get(q, m_size + len);
}

/******************************************************************************/

static const char *g_wks[wks_max_wks] = {
  /* wks_atbuild          */   "@build",
  /* wks_atstatic         */   "@static",
  /* wks_op_count         */   "operator #",
  /* wks_op_rem           */   "operator %",
  /* wks_op_rem_assign    */   "operator %=",
  /* wks_op_bitand        */   "operator &",
  /* wks_op_bitand_assign */   "operator &=",
  /* wks_op_apply         */   "operator ()",
  /* wks_op_mul           */   "operator *",
  /* wks_op_mul_assign    */   "operator *=",
  /* wks_op_add           */   "operator +",
  /* wks_op_post_incr     */   "operator ++",
  /* wks_op_pre_incr      */   "operator ++!",
  /* wks_op_add_assign    */   "operator +=",
  /* wks_op_sub           */   "operator -",
  /* wks_op_post_decr     */   "operator --",
  /* wks_op_pre_decr      */   "operator --!",
  /* wks_op_sub_assign    */   "operator -=",
  /* wks_op_deref         */   "operator ->",
  /* wks_op_range         */   "operator ..",
  /* wks_op_div           */   "operator /",
  /* wks_op_div_assign    */   "operator /=",
  /* wks_op_assign        */   "operator =",
  /* wks_op_lt            */   "operator <",
  /* wks_op_lshift        */   "operator <<",
  /* wks_op_lshift_assign */   "operator <<=",
  /* wks_op_le            */   "operator <=",
  /* wks_op_ne            */   "operator !=",
  /* wks_op_eq            */   "operator ==",
  /* wks_op_gt            */   "operator >",
  /* wks_op_ge            */   "operator >=",
  /* wks_op_rshift        */   "operator >>",
  /* wks_op_rshift_assign */   "operator >>=",
  /* wks_op_index         */   "operator []",
  /* wks_op_bitxor        */   "operator ^",
  /* wks_op_bitxor_assign */   "operator ^=",
  /* wks_op_const         */   "operator const",
  /* wks_op_delete        */   "operator delete",
  /* wks_op_in            */   "operator in",
  /* wks_op_mutable       */   "operator mutable",
  /* wks_op_new           */   "operator new",
  /* wks_op_not           */   "operator !",
  /* wks_op_bitor         */   "operator |",
  /* wks_op_bitor_assign  */   "operator |=",
  /* wks_op_bitnot        */   "operator ~",
  /* wks_class            */   "class",
  /* wks_codefragment_t   */   "codefragment_t",
  /* wks_default          */   "default",
  /* wks_infinity         */   "infinity",
  /* wks_max              */   "max",
  /* wks_min              */   "min",
  /* wks_nan              */   "nan",
  /* wks_null             */   "null",
  /* wks_obj              */   "obj",
  /* wks_ord              */   "ord",
  /* wks_that             */   "that",
  /* wks_this             */   "this",
  /* wks_type             */   "type",
  /* wks_BuiltinPointer   */   "BuiltinPointer",
  /* wks_ConformantArray  */   "ConformantArray",
  /* wks_FixedArray       */   "FixedArray",
  /* wks_Set              */   "Set",
  /* wks_SignedSubrange   */   "SignedSubrange",
  /* wks_UnsignedSubrange */   "UnsignedSubrange",
  /* wks_CharacterSubrange*/   "CharacterSubrange",
  /* wks_Pointer          */   "Pointer",

  /* wkhs_destructor      */   "~this",
  /* wkhs_op_generic      */   "op generic",
  /* wkhs_op_global       */   "op global",
  /* wkhs_op_macro_insert */   "op macro_insert",
  /* wkhs_op_macro_iter   */   "op macro_iter",
  /* wkhs_op_move         */   "op move",
  /* wkhs_op_ptr          */   "op ptr",
  /* wkhs_op_ref          */   "op ref",
  /* wkhs_op_r_ref        */   "op r_ref",
  /* wkhs_op_set          */   "op set",
  /* wkhs_vtbl            */   "vtbl"
};

static String *g_wksobj[wks_max_wks];

String *String::Get(WellKnownString wks) {
  if (g_wksobj[wks])
    return g_wksobj[wks];

  const char *s = g_wks[wks];
  if (wks < wkhs_first_wkhs) {
    g_wksobj[wks] = Get(s);
  } else {
    uint32_t len = uint32_t(strlen(s));
    String *str = g_dedups[cat_hidden].FindOrAdd(s, len);
    str->m_category = cat_hidden;
    g_wksobj[wks] = str;
  }

  return g_wksobj[wks];
}

String *String::AsNormal() {
  if (m_category == cat_normal)
    return this;

  if (m_category == cat_namemacro) {
    m_text[m_size - 1] = 0;
    String *str = g_dedups[cat_normal].FindOrAdd(m_text, m_size - 1);
    m_text[m_size - 1] = '$';
    return str;
  }

  return g_dedups[cat_normal].FindOrAdd(m_text, m_size);
}

String *String::AsNameMacro() {
  verify(m_category == cat_normal);

  // Append a $.
  char *buf = reinterpret_cast<char *>(alloca(m_size + 2));
  memcpy(buf, m_text, m_size);
  buf[m_size] = '$';
  buf[m_size + 1] = 0;

  String *str = g_dedups[cat_namemacro].FindOrAdd(buf, m_size + 1);
  str->m_category = cat_namemacro;
  return str;
}

void String::AppendToHash(SHA512 &hash) {
  hash.AppendString(m_text, m_size);
}
