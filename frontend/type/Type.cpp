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

#include "Type.h"
#include "Config.h"
#include "entity/Class.h"
#include "target/Target.h"
#include "util/BufferWriter.h"
#include "util/Hash.h"
#include "util/Integer.h"
#include "util/SHA512.h"
#include "util/String.h"
#include "util/Value.h"
#include <algorithm>
#include <cassert>
#include <string.h>

using std::pair;
using std::string;
using std::vector;

static const string g_emptyString;

// Big enough to hold the m_offset field from a Type.
using Offset = uint32_t;

// All type information that doesn't fit into a 32-bit type value is kept in
// this heap.  An offset into the heap is stored in the type value.  When the
// heap fills up, it is simply re-allocated (which can be done efficiently using
// platform-specific APIs where available).
// FIXME:  do platform-specific versions.
static intptr_t *g_heap;
static Offset g_heapSize;
static Offset g_heapEnd = 1;  // Valid offsets exclude zero

template<class T>
static inline T *FromHeapOffset(Offset offset) {
  verify(offset < g_heapEnd);
  return reinterpret_cast<T *>(g_heap + offset);
}

static Offset TypeAlloc(unsigned n) {
  n = unsigned((n + sizeof(intptr_t) - 1) / sizeof(intptr_t));
  if (g_heapEnd + n > g_heapSize) {
    if (g_heapSize) {
      g_heapSize *= 2;
      g_heap = (intptr_t*)realloc(g_heap, g_heapSize * sizeof(intptr_t));
    } else {
      g_heapSize = 65536;
      g_heap = (intptr_t*)malloc(g_heapSize * sizeof(intptr_t));
    }
  }

  Offset rv = g_heapEnd;
  g_heapEnd += n;
  memset(g_heap + rv, 0, n * sizeof(intptr_t));
  return rv;
}

// Type information structs that are kept in a hash table must inherit from
// Hashable.
struct HashableInfo {
  Offset              m_next;
  uint32_t            m_hash;
  void               *m_targetData;
};

// Various structs defining the information kept in the above type heap.
struct ClassInfo {
  ::Class            *m_entity;
  void               *m_targetData;
};

struct ThinPointerInfo : public HashableInfo {
  using KEY = Type;

  Type                m_base;

  bool operator==(const KEY &key) const {
    return m_base == key;
  }

  static uint32_t Hash(const KEY &key) {
    return ::Hash(key);
  }

  static Offset Alloc(const KEY &key) {
    Offset rv = TypeAlloc(sizeof(ThinPointerInfo));
    auto tpi = FromHeapOffset<ThinPointerInfo>(rv);
    tpi->m_base = key;
    return rv;
  }
};

// Also steal this for code fragment types.
using CodeFragmentInfo = ThinPointerInfo;

struct FatPointerInfo : public HashableInfo {
  using KEY = Type;

  Type                m_key;
  Type                m_base;
  Type                m_descriptor;

  bool operator==(const KEY &key) const {
    return m_key == key;
  }

  static uint32_t Hash(const KEY &key) {
    return ::Hash(key);
  }

  static Offset Alloc(const KEY &key) {
    Offset rv = TypeAlloc(sizeof(FatPointerInfo));
    auto fpi = FromHeapOffset<FatPointerInfo>(rv);
    fpi->m_key = key;
    fpi->m_base = Type::ThinPointerTo(key);
    fpi->m_descriptor = Type::IntPtrT();
    return rv;
  }
};

struct FieldInfo {
  intptr_t            m_offset;    // Offset of field within struct
  String             *m_name;      // Optional name of field
  Type                m_type;      // Field type
};

struct FunctionInfo : public HashableInfo {
  struct KEY {
    Type              m_retType;
    Offset            m_nArgs;
    const Type       *m_argTypes;
    VariadicType      m_variadicType;
    bool              m_method;
    bool              m_throws;

    KEY(Type rt, Offset nargs, const Type *args, bool method, bool throws,
        VariadicType vt)
        : m_retType(rt), m_nArgs(nargs), m_argTypes(args), m_variadicType(vt),
          m_method(method), m_throws(throws) { }
  };

  Type                m_retType;
  VariadicType        m_variadicType;
  bool                m_method;
  bool                m_throws;
  Offset              m_nArgs;
  Type                m_argTypes[1];

  bool operator==(const KEY &key) const {
    if (key.m_retType != m_retType)
      return false;
    if (key.m_variadicType != m_variadicType)
      return false;
    if (key.m_method != m_method)
      return false;
    if (key.m_throws != m_throws)
      return false;
    if (key.m_nArgs != m_nArgs)
      return false;
    for (size_t i = 0; i < m_nArgs; i++)
      if (key.m_argTypes[i] != m_argTypes[i])
        return false;
    return true;
  }

  static uint32_t Hash(const KEY &key) {
    uint32_t hash = ::Hash(key.m_retType);
    hash ^= ::Hash((key.m_variadicType << 16) | (key.m_method << 8) |
                    key.m_throws);
    for (Offset i = 0; i < key.m_nArgs; i++)
      hash ^= ::Hash(key.m_argTypes[i]);
    return hash;
  }

  static Offset Alloc(const KEY &key) {
    // Factor in the number of arguments and allocate.
    size_t size = sizeof(FunctionInfo) + key.m_nArgs * sizeof(Type)
                                       - sizeof(Type);
    Offset rv = TypeAlloc(unsigned(size));
    auto fi = FromHeapOffset<FunctionInfo>(rv);

    // Initialize the FunctionInfo.
    fi->m_retType = key.m_retType;
    fi->m_variadicType = key.m_variadicType;
    fi->m_method = key.m_method;
    fi->m_throws = key.m_throws;
    fi->m_nArgs = key.m_nArgs;
    for (size_t i = 0; i < key.m_nArgs; i++)
      fi->m_argTypes[i] = key.m_argTypes[i];

    return rv;
  }
};

struct TupleInfo : public HashableInfo {
  using KEY = vector<std::pair<String *, Type>>;

  intptr_t            m_size;      // Tuple size; -1 if uncomputed
  Offset              m_nFields;   // Field count
  YesNoMaybe          m_hasDestructor;
  FieldInfo           m_fields[1]; // Array of field descriptors

  bool operator==(const KEY &key) const {
    if (key.size() != m_nFields)
      return false;
    for (size_t i = 0; i < m_nFields; i++) {
      if (m_fields[i].m_name != key[i].first)
        return false;
      if (m_fields[i].m_type != key[i].second)
        return false;
    }
    return true;
  }

  static uint32_t Hash(const KEY &key) {
    uint32_t hash = 0;
    for (auto &k : key) {
      hash ^= ::Hash(k.first);
      hash ^= ::Hash(k.second);
    }
    return hash;
  }

  static Offset Alloc(const KEY &key) {
    // Factor in the size of the fields and allocate.
    size_t size = sizeof(TupleInfo) + key.size() * sizeof(FieldInfo)
                                    - sizeof(FieldInfo);
    Offset rv = TypeAlloc(unsigned(size));
    auto ti = FromHeapOffset<TupleInfo>(rv);

    // Initialize the TupleInfo.
    ti->m_size = -1;
    ti->m_nFields = Offset(key.size());
    ti->m_hasDestructor = MAYBE;
    for (size_t i = 0; i < key.size(); i++) {
      ti->m_fields[i].m_offset = -1;
      ti->m_fields[i].m_name = key[i].first;
      ti->m_fields[i].m_type = key[i].second;
    }

    return rv;
  }
};

struct EnumInfo : public HashableInfo {
  struct KEY {
    const vector<String *>  &m_names;
    Type                     m_valueType;
    const vector<Value *>   &m_values;

    KEY(const vector<String *> &names, Type vt, const vector<Value *> &values)
        : m_names(names), m_valueType(vt), m_values(values) {
      verify((m_valueType == tk_void && m_values.size() == 0) ||
             (m_valueType != tk_void && m_values.size() == m_names.size()));
    }
  };

  uint32_t            m_count;     // Number of identifiers in enumeration
  Type                m_subrange;  // Equivalent integer subrange type
  Type                m_valueType; // Type of associated values (or void type)
  Offset              m_values;    // Offset to value array, if present
  String             *m_names[1];  // Enum identifiers
  // If this enumeration explicitly assigns values, then m_values is a non-zero
  // offset pointing to the start of an array of Value* that follows
  // m_names.

  Value **Values() {
    if (!m_count)
      return nullptr;

    return (Value **)(intptr_t(this) + m_values);
  }

  bool operator==(const KEY &key) const {
    if (key.m_names.size() != m_count)
      return false;
    if (key.m_valueType != m_valueType)
      return false;
    for (size_t i = 0; i < m_count; i++)
      if (m_names[i] != key.m_names[i])
        return false;
    if (m_valueType != tk_void) {
      uintptr_t addr = reinterpret_cast<uintptr_t>(this) + m_values;
      Value **values = reinterpret_cast<Value **>(addr);
      for (size_t i = 0; i < m_count; i++)
        if (values[i]->Compare(key.m_values[i]) != 0)
          return false;
    }
    return true;
  }

  static uint32_t Hash(const KEY &key) {
    uint32_t hash = ::Hash(&key.m_names[0], key.m_names.size());
    hash ^= ::Hash(key.m_valueType);
    if (key.m_valueType != tk_void) {
      for (Value *v : key.m_values)
        hash ^= v->Hash();
    }
    return hash;
  }

  static Offset Alloc(const KEY &key) {
    // Factor in the size of the other stuff and allocate.
    size_t size = sizeof(EnumInfo) + key.m_names.size() * sizeof(void *)
                                   - sizeof(void *);
    if (key.m_valueType != tk_void)
      size += key.m_values.size() * sizeof(void *);
    Offset rv = TypeAlloc(unsigned(size));
    auto ei = FromHeapOffset<EnumInfo>(rv);

    // Initialize the EnumInfo.
    ei->m_count = uint32_t(key.m_names.size());
    for (size_t i = 0; i < ei->m_count; i++)
      ei->m_names[i] = key.m_names[i];

    ei->m_valueType = key.m_valueType;
    if (key.m_valueType != tk_void) {
      ei->m_values = Offset((intptr_t)&ei->m_names[ei->m_count] - (intptr_t)ei);
      Value **values = ei->Values();
      for (size_t i = 0; i < ei->m_count; i++)
        values[i] = key.m_values[i];
    }

    Integer *lower = Integer::Get(wki_zero);
    Integer *upper = Integer::Get(int(ei->m_count) - 1);
    ei->m_subrange = Type::IntegerSubrange(lower, upper);
    return rv;
  }
};

struct UnionInfo : public HashableInfo {
  using KEY = Type;

  intptr_t            m_size;      // Union size; -1 if uncomputed
  intptr_t            m_offset;    // Offset of variants relative to start
  unsigned            m_alignment; // Alignment of union values
  Type                m_enum;      // Enum for variant tag

  bool operator==(const KEY &key) const {
    return key == m_enum;
  }

  static uint32_t Hash(const KEY &key) {
    return ::Hash(key);
  }

  static Offset Alloc(const KEY &key) {
    // Factor in the size of the fields and allocate.
    size_t size = sizeof(UnionInfo);
    Offset rv = TypeAlloc(unsigned(size));
    auto ui = FromHeapOffset<UnionInfo>(rv);

    // Initialize the UnionInfo.
    ui->m_size = -1;
    ui->m_offset = -1;
    ui->m_enum = key;

    return rv;
  }
};

struct IntegerSubrangeInfo : public HashableInfo {
  struct KEY {
    Integer          *m_lower;
    Integer          *m_upper;
    bool              m_signed;

    KEY(Integer *l, Integer *u, bool s)
        : m_lower(l),
          m_upper(u),
          m_signed(s) { }
  };

  // Interned lower/upper bounds (only pointer comparison needed).
  Integer            *m_lower;
  Integer            *m_upper;
  bool                m_signed;
  const char         *m_printable;

  // For pre-defined subranges, a custom printable name can be set by setting
  // this before calling FindOrAdd.
  static const char *s_name;

  bool operator==(const KEY &key) const {
    return m_lower == key.m_lower && m_upper == key.m_upper &&
           m_signed == key.m_signed;
  }

  static uint32_t Hash(const KEY &key) {
    HashState hs;
    hs.Hash(key.m_lower);
    hs.Hash(key.m_upper);
    uint32_t tmp = key.m_signed;
    hs.Hash(tmp);
    return hs.fmix32();
  }

  static Offset Alloc(const KEY &key) {
    Offset rv = TypeAlloc(sizeof(IntegerSubrangeInfo));
    auto isi = FromHeapOffset<IntegerSubrangeInfo>(rv);
    isi->m_lower = key.m_lower;
    isi->m_upper = key.m_upper;
    isi->m_signed = key.m_signed;
    isi->m_printable = s_name;
    s_name = nullptr;
    return rv;
  }
};

const char *IntegerSubrangeInfo::s_name = nullptr;

struct ArrayInfo : public HashableInfo {
  using KEY = pair<Type, Type>;

  Type                m_element;
  Type                m_index;

  bool operator==(const KEY &key) const {
    return m_element == key.first && m_index == key.second;
  }

  static uint32_t Hash(const KEY &key) {
    return ::Hash(key);
  }

  static Offset Alloc(const KEY &key) {
    Offset rv = TypeAlloc(sizeof(ArrayInfo));
    auto ai = FromHeapOffset<ArrayInfo>(rv);
    ai->m_element = key.first;
    ai->m_index = key.second;
    return rv;
  }
};

struct SetInfo : public HashableInfo {
  using KEY = Type;

  Type                m_element;
  Type                m_container;

  bool operator==(const KEY &key) const {
    return m_element == key;
  }

  static uint32_t Hash(const KEY &key) {
    return ::Hash(key);
  }

  static Offset Alloc(const KEY &key) {
    Offset rv = TypeAlloc(sizeof(SetInfo));
    auto si = FromHeapOffset<SetInfo>(rv);
    si->m_element = key;
    intptr_t cardinality = key.Cardinality();
    verify(cardinality >= 0 && cardinality <= 256); // FIXME
    if (cardinality == 0) {
      si->m_container = Type::Void();
    } else if (cardinality <= 8) {
      si->m_container = Type::Byte();
    } else if (cardinality <= 16) {
      si->m_container = Type::UShort();
    } else if (cardinality <= 32) {
      si->m_container = Type::UInt();
    } else if (cardinality <= 64) {
      si->m_container = Type::ULong();
    } else {
      size_t cnt = (cardinality + 63) / 64;
      si->m_container = Type::ArrayOf(Type::ULong());
      si->m_container.SetIndexType(cnt);
    }
    return rv;
  }
};

namespace {
  // Hash table for keeping track of select info structs above (the ones with
  // m_hash/m_next fields).

  template<class T> class HashTable {
  public:
    static const int NBUCKETS = 4096;   // must be power of two

    using KEY = typename T::KEY;

    Offset           *m_buckets   = new Offset[NBUCKETS];
    uint32_t          m_limit     = NBUCKETS - 1;
    uint32_t          m_count     = 0;

    HashTable() {
      // FIXME: did the new Offset[] already do this?
      memset(m_buckets, 0, NBUCKETS * sizeof(Offset));
    }

    // The new element is optimistically added to the type heap and passed to
    // us.  If it isn't found in the hash table, it remains in the heap and its
    // offset is returned; otherwise, the old heap end is restored, deleting
    // the new element, and the offset of the element already in the table is
    // returned.
    Offset FindOrAdd(const KEY &key) {
      uint32_t hash = T::Hash(key);

      // Search hash table.
      Offset *pHead = m_buckets + (hash & m_limit);
      for (Offset b = *pHead; b; ) {
        T *p = FromHeapOffset<T>(b);
        if (hash == p->m_hash && *p == key)
          return b;
        b = p->m_next;
      }

      // Not found -- go add it.
      Offset rv = T::Alloc(key);
      T *p = FromHeapOffset<T>(rv);
      p->m_hash = hash;
      p->m_next = *pHead;
      p->m_targetData = nullptr;
      *pHead = rv;

      // If the table has become too full, we need to double its size or else
      // collisions become too frequent.  But beware trying to keep the
      // collision rate too low, or the savings is wiped out by excessive
      // rehashing!
      if (++m_count > m_limit) {
        Offset newLimit = m_limit * 2 + 1;
        Offset *newHash = new Offset[newLimit + 1];
        memset(newHash, 0, (newLimit + 1) * sizeof(Offset));

        // Rehash entries to bigger table.
        for (Offset i = 0; i <= m_limit; i++) {
          Offset next = m_buckets[i];
          while (Offset n = next) {
            T *t = FromHeapOffset<T>(next);
            pHead = newHash + (t->m_hash & newLimit);
            next = t->m_next;
            t->m_next = *pHead;
            *pHead = n;
          }
        }

        // Finally, swap new for old.
        delete[] m_buckets;
        m_buckets = newHash;
        m_limit = newLimit;
      }

      return rv;
    }
  };
}

static HashTable<IntegerSubrangeInfo> g_integerSubrangeMap;

static inline void GetCommonIntegerSubrange(Offset &offset, const char *name,
                                            WellKnownInteger lower,
                                            WellKnownInteger upper) {
  if (!offset) {
    IntegerSubrangeInfo::s_name = name;
    Integer *l = Integer::Get(lower);
    Integer *u = Integer::Get(upper);
    IntegerSubrangeInfo::KEY key(l, u, l->IsNegative());
    offset = g_integerSubrangeMap.FindOrAdd(key);
  }
}

Type Type::SByte() {
  static Offset g_offset = 0;
  GetCommonIntegerSubrange(g_offset, "sbyte", wki_int8_min, wki_int8_max);
  return Type(tk_integer, g_offset);
}

Type Type::Byte() {
  static Offset g_offset = 0;
  GetCommonIntegerSubrange(g_offset, "byte", wki_zero, wki_uint8_max);
  return Type(tk_integer, g_offset);
}

Type Type::Short() {
  static Offset g_offset = 0;
  GetCommonIntegerSubrange(g_offset, "short", wki_int16_min, wki_int16_max);
  return Type(tk_integer, g_offset);
}

Type Type::UShort() {
  static Offset g_offset = 0;
  GetCommonIntegerSubrange(g_offset, "ushort", wki_zero, wki_uint16_max);
  return Type(tk_integer, g_offset);
}

Type Type::Int() {
  static Offset g_offset = 0;
  GetCommonIntegerSubrange(g_offset, "int", wki_int32_min, wki_int32_max);
  return Type(tk_integer, g_offset);
}

Type Type::UInt() {
  static Offset g_offset = 0;
  GetCommonIntegerSubrange(g_offset, "uint", wki_zero, wki_uint32_max);
  return Type(tk_integer, g_offset);
}

Type Type::Long() {
  static Offset g_offset = 0;
  GetCommonIntegerSubrange(g_offset, "long", wki_int64_min, wki_int64_max);
  return Type(tk_integer, g_offset);
}

Type Type::ULong() {
  static Offset g_offset = 0;
  GetCommonIntegerSubrange(g_offset, "ulong", wki_zero, wki_uint64_max);
  return Type(tk_integer, g_offset);
}

Type Type::IntegerSubrange(Integer *lower, Integer *upper, bool is_signed) {
  // Make sure all the common subranges are created first, so that they have
  // their correct printable descriptions.
  static bool g_first = true;
  if (g_first) {
    g_first = false;
    SByte();
    Byte();
    Short();
    UShort();
    Int();
    UInt();
    Long();
    ULong();
  }

  IntegerSubrangeInfo::KEY key(lower, upper, is_signed);
  Offset rv = g_integerSubrangeMap.FindOrAdd(key);
  return Type(tk_integer, rv);
}

struct CharSubrangeInfo : public HashableInfo {
  struct KEY {
    uint32_t          m_lower;
    uint32_t          m_upper;

    KEY(uint32_t l, uint32_t u)
        : m_lower(l),
          m_upper(u) { }
  };

  uint32_t            m_lower;
  uint32_t            m_upper;
  const char         *m_printable;

  // For pre-defined subranges, a custom printable name can be set by setting
  // this before calling FindOrAdd.
  static const char *s_name;

  bool operator==(const KEY &key) const {
    return m_lower == key.m_lower && m_upper == key.m_upper;
  }

  static uint32_t Hash(const KEY &key) {
    HashState hs;
    hs.Hash(key.m_lower);
    hs.Hash(key.m_upper);
    return hs.fmix32();
  }

  static Offset Alloc(const KEY &key) {
    Offset rv = TypeAlloc(sizeof(CharSubrangeInfo));
    auto csi = FromHeapOffset<CharSubrangeInfo>(rv);
    csi->m_lower = key.m_lower;
    csi->m_upper = key.m_upper;
    csi->m_printable = s_name;
    s_name = nullptr;
    return rv;
  }
};

const char *CharSubrangeInfo::s_name = nullptr;

static HashTable<CharSubrangeInfo> g_charSubrangeMap;

static inline void GetCommonCharSubrange(Offset &offset, const char *name,
                                         uint32_t lower, uint32_t upper) {
  if (!offset) {
    CharSubrangeInfo::s_name = name;
    CharSubrangeInfo::KEY key(lower, upper);
    offset = g_charSubrangeMap.FindOrAdd(key);
  }
}

Type Type::Char8() {
  static Offset g_offset = 0;
  GetCommonCharSubrange(g_offset, "char", 0, 0xff);
  return Type(tk_char, g_offset);
}

Type Type::Char16() {
  static Offset g_offset = 0;
  GetCommonCharSubrange(g_offset, "char16", 0, 0xffff);
  return Type(tk_char, g_offset);
}

Type Type::Char32() {
  static Offset g_offset = 0;
  GetCommonCharSubrange(g_offset, "char32", 0, 0xffffffff);
  return Type(tk_char, g_offset);
}

Type Type::CharSubrange(uint32_t lower, uint32_t upper) {
  // Make sure all the common subranges are created first, so that they have
  // their correct printable descriptions.
  static bool g_first = true;
  if (g_first) {
    g_first = false;
    Char8();
    Char16();
    Char32();
  }

  CharSubrangeInfo::KEY key(lower, upper);
  Offset rv = g_charSubrangeMap.FindOrAdd(key);
  return Type(tk_char, rv);
}

static HashTable<FunctionInfo> g_functionMap;

Type Type::Function(Type rt, const vector<Type> &args, bool method,
                    bool throws, VariadicType vt) {
  FunctionInfo::KEY key(rt, Offset(args.size()), &args[0], method, throws, vt);
  Offset rv = g_functionMap.FindOrAdd(key);
  return Type(tk_function, rv);
}

static HashTable<TupleInfo> g_tupleMap;

Type Type::Tuple(const vector<Type> &fields) {
  TupleInfo::KEY key;
  key.reserve(fields.size());
  for (Type t : fields)
    key.emplace_back(nullptr, t);
  return Tuple(key);
}

Type Type::Tuple(const vector<std::pair<String *, Type>> &fields) {
  Offset rv = g_tupleMap.FindOrAdd(fields);
  return Type(tk_tuple, rv);
}

static HashTable<UnionInfo> g_unionMap;

Type Type::Union(const vector<std::pair<String *, Type>> &variants) {
  // Sort by variant names for consistent enum type.
  vector<size_t> ords;
  for (size_t i = 0; i < variants.size(); i++)
    ords.push_back(i);
  std::sort(ords.begin(), ords.end(), [&variants](size_t a, size_t b) {
    return strcmp(variants[a].first->c_str(), variants[b].first->c_str());
  });

  vector<String *> names;
  vector<Value *> values;

  names.reserve(variants.size());
  values.reserve(variants.size());

  for (auto ord : ords) {
    auto &v = variants[ord];
    names.push_back(v.first);
    values.push_back(Value::NewType(v.second));
  }

  Type t = Type::Enum(names, Type::JType(), values);
  Offset rv = g_unionMap.FindOrAdd(t);
  return Type(tk_union, rv);
}

Type Type::Union(Type enumType) {
  // FIXME: validate enumType is a type, and that the members are sorted.
  Offset rv = g_unionMap.FindOrAdd(enumType);
  return Type(tk_union, rv);
}

static HashTable<EnumInfo> g_enumMap;

Type Type::Enum(const vector<String *> &idents) {
  EnumInfo::KEY key{idents, Type::Void(), {}};
  Offset rv = g_enumMap.FindOrAdd(key);
  return Type(tk_enum, rv);
}

Type Type::Enum(const vector<String *> &idents, Type valueType,
                const vector<Value *> &values) {
  EnumInfo::KEY key{idents, valueType, values};
  Offset rv = g_enumMap.FindOrAdd(key);
  return Type(tk_enum, rv);
}

void Type::ComputeLayout() const {
  verify(m_kind == tk_tuple || m_kind == tk_union);
  if (m_kind == tk_tuple) {
    auto ti = FromHeapOffset<TupleInfo>(m_offset);
    ti->m_size = 0;
    if (ti->m_nFields > 0) {
      vector<size_t> offsets { Target::GetStructLayout(*this) };
      ti->m_size = offsets.back();
      for (size_t i = 0; i < ti->m_nFields; i++)
        ti->m_fields[i].m_offset = offsets[i];
    }
  } else {
    auto ui = FromHeapOffset<UnionInfo>(m_offset);
    ui->m_size = ui->m_enum.StorageSize();
    unsigned max_alignment = ui->m_enum.Alignment();
    size_t max_size = 0;
    for (size_t i = 0; i < ui->m_enum.IdentCount(); i++) {
      Type t = ui->m_enum.ValueAt(i)->AsType();
      max_alignment = std::max(max_alignment, t.Alignment());
      max_size = std::max(max_size, t.StorageSize());
    }
    ui->m_alignment = max_alignment;
    ui->m_offset = (ui->m_size + max_alignment - 1) & (max_alignment - 1);
    ui->m_size = m_offset + max_size;
    ui->m_size = (ui->m_size + max_alignment - 1) & (max_alignment - 1);
  }
}

static HashTable<FatPointerInfo> g_fatPointerMap;

Type Type::PointerTo(Type base) {
  if (base == tk_class) {
    return ThinPointerTo(base);
  } else if (base == tk_array) {
    if (base.IndexType().IsKnown()) {
      return ThinPointerTo(base);
    } else {
      base = base.RValue();
      Offset rv = g_fatPointerMap.FindOrAdd(base);
      return Type(tk_fatpointer, rv);
    }
  } else {
    return ThinPointerTo(base);
  }
}

static HashTable<ThinPointerInfo> g_thinPointerMap;

Type Type::ThinPointerTo(Type base) {
  if (base == tk_valueless)
    return Type::Suppress();
  verify(base != tk_pseudo && base.IsKnown());
  base = base.RValue();
  Offset rv = g_thinPointerMap.FindOrAdd(base);
  return Type(tk_pointer, rv);
}

intptr_t Type::Cardinality() const {
  switch (m_kind) {
    case tk_tuple: {
      auto ti = FromHeapOffset<TupleInfo>(m_offset);
      intptr_t cardinality = 1;
      for (size_t i = 0; i < ti->m_nFields; i++) {
        intptr_t c = ti->m_fields[i].m_type.Cardinality();

        // If a field has no cardinality, then neither does the tuple.
        if (c == -1)
          return c;

        // If a field has cardinality zero or has too high a cardinality, then
        // so does the tuple (with zero having precedence).
        if (c <= 0) {
          if (cardinality != 0)
            cardinality = c;
        }

        // Compute product of all field cardinalities.  Check for overflow.
        if (cardinality > 0) {
          intptr_t x = cardinality * c;
          if (cardinality == x / c)
            cardinality = x;
          else
            cardinality = -2;
        }
      }
      return cardinality;
    }

    case tk_bool:
      return 2;

    case tk_char: {
      auto csi = FromHeapOffset<CharSubrangeInfo>(m_offset);
      return csi->m_upper - csi->m_lower + 1;
    }

    case tk_integer: {
      auto isi = FromHeapOffset<IntegerSubrangeInfo>(m_offset);
      Integer *one = Integer::Get(wki_one);
      Integer *cardinality = isi->m_upper->Sub(isi->m_lower)->Add(one);
      if (cardinality->GetSizeInBits() < sizeof(intptr_t) * 8)
        return cardinality->AsIntPtr();
      else
        return -2;
    }

    case tk_enum:
      return FromHeapOffset<EnumInfo>(m_offset)->m_subrange.Cardinality();

    case tk_array: {
      auto ai = FromHeapOffset<ArrayInfo>(m_offset);
      intptr_t index_c = ai->m_index.Cardinality();
      intptr_t element_c = ai->m_element.Cardinality();
      if (index_c == -1 || element_c == -1)
        return -1;
      if (index_c == 0 || element_c == 0)
        return 0;
      if (index_c == -2 || element_c == -2)
        return -2;
      intptr_t cardinality = 1;
      while (--index_c >= 0) {
        intptr_t x = cardinality * element_c;
        if (cardinality != x / element_c)
          return -2;
        cardinality = x;
      }
      return cardinality;
    }

    case tk_set: {
      auto ai = FromHeapOffset<SetInfo>(m_offset);
      intptr_t element_c = ai->m_element.Cardinality();
      if (element_c < 0)
        return element_c;
      if (element_c >= intptr_t(sizeof(intptr_t) * 8 - 1))
        return -2;
      return intptr_t(1) << element_c;
    }

    default:
      return -1;
  }
}

::Class *Type::Class() const {
  verify(m_kind == tk_class);
  auto ci = FromHeapOffset<ClassInfo>(m_offset);
  return ci->m_entity;
}

static HashTable<ArrayInfo> g_arrayMap;

Type Type::ArrayOf(Type element, Type index) {
  // FIXME: sanity checks on index type, including cardinality
  ArrayInfo::KEY key(element.RValue(), index.RValue());
  Offset rv = g_arrayMap.FindOrAdd(key);
  return Type(tk_array, rv);
}

static HashTable<SetInfo> g_setMap;

Type Type::SetOf(Type element) {
  // FIXME: sanity checks on element type, including cardinality
  SetInfo::KEY key(element.DropQualifiers());
  Offset rv = g_setMap.FindOrAdd(key);
  return Type(tk_set, rv);
}

Type Type::StringRef() {
  return ArrayOf(Char8()).Const().LValueRef();
}

Type Type::StringRef(size_t len) {
  Type t = StringRef();
  return t.SetIndexType(len);
}

bool Type::IsString() {
  if (m_kind != tk_array)
    return false;
  if (ElementType() != Char8())
    return false;
  return true;
}

static HashTable<CodeFragmentInfo> g_codeFragmentMap;

Type Type::CodeFragment(Type type) {
  if (type == tk_valueless)
    return Type::Suppress();
  verify(type != tk_pseudo);
  Offset rv = g_codeFragmentMap.FindOrAdd(type);
  return Type(tk_codefragment, rv);
}

size_t Type::StorageSize() const {
  if (m_ref != rt_rvalue)
    return Lower().StorageSize();

  switch (m_kind) {
    case tk_valueless:
    case tk_void:
      return 0;

    case tk_class: {
      auto ci = FromHeapOffset<ClassInfo>(m_offset);
      return ci->m_entity->StorageSize();
    }

    case tk_tuple: {
      auto ti = FromHeapOffset<TupleInfo>(m_offset);
      if (ti->m_size < 0)
        ComputeLayout();
      return ti->m_size;
    }

    case tk_union: {
      auto ui = FromHeapOffset<UnionInfo>(m_offset);
      if (ui->m_size < 0)
        ComputeLayout();
      return ui->m_size;
    }

    case tk_pointer:
      return sizeof(void *);

    case tk_fatpointer: {
      auto fpi = FromHeapOffset<FatPointerInfo>(m_offset);
      return fpi->m_base.StorageSize() + fpi->m_descriptor.StorageSize();
    }

    case tk_type:
      return sizeof(Type);

    case tk_namespace:
    case tk_concept:
    case tk_generic:
    case tk_pseudo:
    case tk_codefragment:
      return sizeof(void *);

    case tk_bool:
      return 1;

    case tk_float:
      return m_offset == t_Float ? 4 : 8;

    case tk_char: {
      auto csi = FromHeapOffset<CharSubrangeInfo>(m_offset);
      if (csi->m_upper <= 0xff)
        return 1;
      else if (csi->m_upper <= 0xffff)
        return 2;
      else
        return 4;
    }

    case tk_integer: {
      auto isi = FromHeapOffset<IntegerSubrangeInfo>(m_offset);
      int nbits = isi->m_upper->GetSizeInBits();
      if (isi->m_lower->IsNegative()) {
        int n = isi->m_lower->GetSizeInBits();
        if (n > nbits)
          nbits = n;
        nbits++; // account for sign bit
      }

      // FIXME: don't currently support sizes other than power of two.
      if (nbits <= 8)
        return 1;
      else if (nbits <= 16)
        return 2;
      else if (nbits <= 32)
        return 4;
      else
        return 8 * ((nbits + 63) / 64);
    }

    case tk_enum:
      return FromHeapOffset<EnumInfo>(m_offset)->m_subrange.StorageSize();

    case tk_array: {
      auto ai = FromHeapOffset<ArrayInfo>(m_offset);
      verify(ai->m_index.IsKnown());
      return ai->m_element.StorageSize() * ai->m_index.Cardinality();
    }

    case tk_set: {
      auto si = FromHeapOffset<SetInfo>(m_offset);
      return si->m_container.StorageSize();
    }

    default:
      verify(0 && "StorageSize() used on unsupported type.");
      return 0;
  }
}

unsigned Type::Alignment() const {
  if (m_ref != rt_rvalue)
    return (unsigned)Lower().StorageSize();

  switch (m_kind) {
    case tk_valueless:
    case tk_void:
      return 1;

    case tk_union: {
      auto ui = FromHeapOffset<UnionInfo>(m_offset);
      if (ui->m_size < 0)
        ComputeLayout();
      return ui->m_alignment;
    }

    case tk_pointer:
    case tk_bool:
    case tk_float:
    case tk_char:
    case tk_integer:
    case tk_tuple:
    case tk_class:
    case tk_fatpointer:
      return Target::GetAlignment(*this);

    case tk_type:
      return sizeof(Type);

    case tk_namespace:
    case tk_codefragment:
      return sizeof(void *);

    case tk_enum:
      return FromHeapOffset<EnumInfo>(m_offset)->m_subrange.Alignment();

    case tk_array: {
      auto ai = FromHeapOffset<ArrayInfo>(m_offset);
      return ai->m_element.Alignment();
    }

    case tk_set: {
      auto si = FromHeapOffset<SetInfo>(m_offset);
      return si->m_container.Alignment();
    }

    case tk_concept:
    case tk_generic:
      return sizeof(void *);

    default:
      verify(0 && "Alignment() used on unsupported type.");
      return 0;
  }
}

bool Type::IsSigned() const {
  if (m_kind == tk_integer)
    return FromHeapOffset<IntegerSubrangeInfo>(m_offset)->m_signed;
  if (m_kind == tk_float)
    return true;
  return false;
}

size_t Type::FieldCount() const {
  switch (m_kind) {
    case tk_class:
      return FromHeapOffset<ClassInfo>(m_offset)->m_entity->FieldCount();

    case tk_fatpointer:
      return 2;

    case tk_tuple:
    case tk_concept:
    case tk_generic:
      return FromHeapOffset<TupleInfo>(m_offset)->m_nFields;

    case tk_union:
      return FromHeapOffset<UnionInfo>(m_offset)->m_enum.IdentCount() + 1;

    default:
      verify(0 && "FieldCount() used on unsupported type.");
      return 0;
  }
}

size_t Type::OffsetOf(size_t ord) const {
  switch (m_kind) {
    case tk_class:
      return FromHeapOffset<ClassInfo>(m_offset)->m_entity->OffsetOf(ord);

    case tk_fatpointer: {
      auto fpi = FromHeapOffset<FatPointerInfo>(m_offset);
      verify(ord <= 1);
      return ord == 0 ? 0 : fpi->m_base.StorageSize();
    }

    case tk_tuple: {
      auto ti = FromHeapOffset<TupleInfo>(m_offset);
      if (ti->m_size < 0)
        ComputeLayout();
      verify(ord < ti->m_nFields);
      return ti->m_fields[ord].m_offset;
    }

    case tk_union: {
      auto ui = FromHeapOffset<UnionInfo>(m_offset);
      verify(ord <= ui->m_enum.IdentCount());
      return ord == 0 ? 0 : ui->m_enum.StorageSize();
      // FIXME: take alignment of variants into account!
    }

    default:
      verify(0 && "OffsetOf() used on unsupported type.");
      return 0;
  }
}

Type Type::TypeOf(size_t ord) const {
  switch (m_kind) {
    case tk_class:
      return FromHeapOffset<ClassInfo>(m_offset)->m_entity->TypeOf(ord);

    case tk_fatpointer: {
      auto fpi = FromHeapOffset<FatPointerInfo>(m_offset);
      verify(ord <= 1);
      return ord == 0 ? fpi->m_base : fpi->m_descriptor;
    }

    case tk_tuple:
    case tk_concept:
    case tk_generic: {
      auto ti = FromHeapOffset<TupleInfo>(m_offset);
      verify(ord < ti->m_nFields);
      return ti->m_fields[ord].m_type;
    }

    case tk_union: {
      auto ui = FromHeapOffset<UnionInfo>(m_offset);
      verify(ord < ui->m_enum.IdentCount());
      return ui->m_enum.ValueAt(ord)->AsType();
    }

    default:
      verify(m_kind == tk_valueless && m_offset == t_Suppress);
      return Suppress();
  }
}

Type Type::TagType() const {
  verify(m_kind == tk_union);
  auto ui = FromHeapOffset<UnionInfo>(m_offset);
  return ui->m_enum;
}

Type Type::BaseType() const {
  if (m_kind == tk_pointer) {
    auto tpi = FromHeapOffset<ThinPointerInfo>(m_offset);
    return tpi->m_base;
  } else if (m_kind == tk_fatpointer) {
    auto fpi = FromHeapOffset<FatPointerInfo>(m_offset);
    return fpi->m_base.BaseType();
  } else if (m_kind == tk_codefragment) {
    auto cfi = FromHeapOffset<CodeFragmentInfo>(m_offset);
    return cfi->m_base;
  } else if (m_kind == tk_enum) {
    auto ei = FromHeapOffset<EnumInfo>(m_offset);
    return ei->m_subrange;
  } else if (m_kind == tk_set) {
    auto si = FromHeapOffset<SetInfo>(m_offset);
    return si->m_container;
  }
  verify(m_kind == tk_valueless && m_offset == t_Suppress);
  return Suppress();
}

bool Type::IsThin() const {
  if (m_kind == tk_pointer)
    return true;
  else if (m_kind == tk_fatpointer)
    return false;
  else if (m_kind == tk_array)
    return IndexType().IsKnown();
  verify(m_kind == tk_valueless && m_offset == t_Suppress);
  return false;
}

Type Type::DescriptorType() const {
  if (m_kind == tk_fatpointer) {
    auto fpi = FromHeapOffset<FatPointerInfo>(m_offset);
    return fpi->m_descriptor;
  }
  verify(m_kind == tk_valueless && m_offset == t_Suppress);
  return Suppress();
}

Integer *Type::IntLowerBound() const {
  verify(m_kind == tk_integer);
  auto isi = FromHeapOffset<IntegerSubrangeInfo>(m_offset);
  return isi->m_lower;
}

Integer *Type::IntUpperBound() const {
  verify(m_kind == tk_integer);
  auto isi = FromHeapOffset<IntegerSubrangeInfo>(m_offset);
  return isi->m_upper;
}

uint32_t Type::CharLowerBound() const {
  verify(m_kind == tk_char);
  auto csi = FromHeapOffset<CharSubrangeInfo>(m_offset);
  return csi->m_lower;
}

uint32_t Type::CharUpperBound() const {
  verify(m_kind == tk_char);
  auto csi = FromHeapOffset<CharSubrangeInfo>(m_offset);
  return csi->m_upper;
}


Type Type::AsSubrange() const {
  verify(m_kind == tk_enum);
  return FromHeapOffset<EnumInfo>(m_offset)->m_subrange;
}

bool Type::HasExplicitValues() const {
  verify(m_kind == tk_enum);
  return FromHeapOffset<EnumInfo>(m_offset)->m_values != 0;
}

Type Type::ReturnType() const {
  verify(m_kind == tk_function);
  auto fi = FromHeapOffset<FunctionInfo>(m_offset);
  return fi->m_retType;
}

VariadicType Type::Variadic() const {
  verify(m_kind == tk_function);
  auto fi = FromHeapOffset<FunctionInfo>(m_offset);
  return fi->m_variadicType;
}

bool Type::IsMethod() const {
  verify(m_kind == tk_function);
  auto fi = FromHeapOffset<FunctionInfo>(m_offset);
  return fi->m_method;
}

Type Type::SetMethod(bool flag) const {
  verify(m_kind == tk_function);
  auto fi = FromHeapOffset<FunctionInfo>(m_offset);
  if (fi->m_method == flag)
    return *this;

  FunctionInfo::KEY key(fi->m_retType, fi->m_nArgs, fi->m_argTypes,
                        flag, fi->m_throws, fi->m_variadicType);
  Offset rv = g_functionMap.FindOrAdd(key);
  return Type(tk_function, rv);
}

bool Type::Throws() const {
  verify(m_kind == tk_function);
  auto fi = FromHeapOffset<FunctionInfo>(m_offset);
  return fi->m_throws;
}

Type Type::SetThrows(bool flag) const {
  verify(m_kind == tk_function);
  auto fi = FromHeapOffset<FunctionInfo>(m_offset);
  if (fi->m_throws == flag)
    return *this;

  FunctionInfo::KEY key(fi->m_retType, fi->m_nArgs, fi->m_argTypes,
                        fi->m_method, flag, fi->m_variadicType);
  Offset rv = g_functionMap.FindOrAdd(key);
  return Type(tk_function, rv);
}

size_t Type::ArgCount() const {
  verify(m_kind == tk_function);
  auto fi = FromHeapOffset<FunctionInfo>(m_offset);
  return fi->m_nArgs;
}

Type Type::ArgType(size_t ord) const {
  verify(m_kind == tk_function);
  auto fi = FromHeapOffset<FunctionInfo>(m_offset);
  verify(ord < fi->m_nArgs);
  return fi->m_argTypes[ord];
}

Value *Type::ValueAt(size_t ord) const {
  verify(m_kind == tk_enum);
  auto ei = FromHeapOffset<EnumInfo>(m_offset);
  verify(ord < ei->m_count && ei->m_valueType != tk_void);
  Value **values = (Value **)((char *)ei + ei->m_values);
  return values[ord];
}

String *Type::IdentAt(size_t ord) const {
  switch (m_kind) {
    case tk_enum: {
      auto ei = FromHeapOffset<EnumInfo>(m_offset);
      verify(ord < ei->m_count);
      return ei->m_names[ord];
    }
    case tk_tuple: {
      auto ti = FromHeapOffset<TupleInfo>(m_offset);
      verify(ord < ti->m_nFields);
      return ti->m_fields[ord].m_name;
    }
    default:
      verify(false);
  }
}

ssize_t Type::OrdinalOf(String *ident) const {
  // FIXME: should use hash table, not linear searches.
  switch (m_kind) {
    case tk_enum: {
      auto ei = FromHeapOffset<EnumInfo>(m_offset);
      for (size_t i = 0; i < ei->m_count; i++)
        if (ident == ei->m_names[i])
          return i;
      break;
    }
    case tk_tuple: {
      auto ti = FromHeapOffset<TupleInfo>(m_offset);
      for (size_t i = 0; i < ti->m_nFields; i++)
        if (ident == ti->m_fields[i].m_name)
          return i;
      break;
    }
    default:
      verify(false);
  }
  return -1;
}

size_t Type::IdentCount() const {
  switch (m_kind) {
    case tk_enum:
      return FromHeapOffset<EnumInfo>(m_offset)->m_count;
    case tk_union:
      return FromHeapOffset<UnionInfo>(m_offset)->m_enum.IdentCount();
    default:
      verify(false);
      return 0;
  }
}

Type Type::ElementType() const {
  verify(m_kind == tk_array || m_kind == tk_enum || m_kind == tk_set);
  if (m_kind == tk_array)
    return FromHeapOffset<ArrayInfo>(m_offset)->m_element;
  else if (m_kind == tk_set)
    return FromHeapOffset<SetInfo>(m_offset)->m_element;
  else
    return FromHeapOffset<EnumInfo>(m_offset)->m_valueType;
}

Type Type::IndexType() const {
  verify(m_kind == tk_array);
  return FromHeapOffset<ArrayInfo>(m_offset)->m_index;
}

Type Type::SetIndexType(Type t) const {
  verify(m_kind == tk_array);
  Type u = ArrayOf(ElementType(), t);
  u.m_ref = m_ref;
  u.m_const = m_const;
  return u;
}

Type Type::SetIndexType(size_t size) const {
  Integer *low = Integer::Get(wki_zero);
  Integer *high = Integer::Get(ssize_t(size) - 1);
  Type t = Type::IntegerSubrange(low, high);
  return SetIndexType(t);
}

YesNoMaybe Type::IsSubtypeOf(Type t) const {
  if (*this == ct_const && t == ct_mutable)
    return NO;
  if (DropQualifiers() == t.DropQualifiers())
    return YES;

  // FIXME: need to take Subtype and Assignable concepts into account.
  switch (m_kind) {
    case tk_bool:
      if (t == tk_integer) {
        auto si = FromHeapOffset<IntegerSubrangeInfo>(t.m_offset);
        Integer *zero = Integer::Get(wki_zero);
        Integer *one = Integer::Get(wki_one);
        if (si->m_lower->Le(zero) && si->m_upper->Ge(one))
          return YES;
        if (si->m_lower->Gt(one) || si->m_upper->Lt(zero))
          return NO;
        return MAYBE;
      } else if (t == tk_enum) {
        verify(false); // FIXME
        return NO;
      }
      return NO;

    case tk_pointer: {
      if (t != tk_pointer)
        return NO;
      auto tpi = FromHeapOffset<ThinPointerInfo>(m_offset);
      return tpi->m_base.IsPtrSubtypeOf(t.BaseType());
    }

    case tk_fatpointer:
      if (t != tk_pointer)
        return NO;
      return BaseType().IsPtrSubtypeOf(t.BaseType());

    case tk_class:
      return (t == tk_type) && Class()->IsMetaclass() ? YES : NO;

    case tk_generic:
      return t == tk_type ? YES : NO;

    case tk_integer: {
      if (t == tk_float) {
        return YES; // FIXME: look for potential loss of precision (i.e., MAYBE)
      } else if (t != tk_integer) {
        return NO;
      }
      auto si1 = FromHeapOffset<IntegerSubrangeInfo>(m_offset);
      auto si2 = FromHeapOffset<IntegerSubrangeInfo>(t.m_offset);
      if (si1->m_signed != si2->m_signed)
        return NO;
      if (si2->m_lower->Le(si1->m_lower) && si2->m_upper->Ge(si1->m_upper))
        return YES;
      if (si2->m_lower->Gt(si1->m_upper) || si2->m_upper->Lt(si1->m_lower))
        return NO;
      return MAYBE;
    }

    case tk_float:
      if (t == tk_float)
        return *this == Type::Float() || t == Type::Double() ? YES : MAYBE;
      return NO;

    case tk_enum:
      if (t == tk_integer || t == tk_char) {
        if (IdentCount() != 1)
          return NO;
        String *s = IdentAt(0);
        return s == String::Get(wks_min) || s == String::Get(wks_max)
            ? YES : NO;
      } else if (t == tk_float) {
        if (IdentCount() != 1)
          return NO;
        String *s = IdentAt(0);
        return s == String::Get(wks_nan) || s == String::Get(wks_infinity)
            ? YES : NO;
      } else if (t == tk_pointer) {
        if (IdentCount() != 1)
          return NO;
        return IdentAt(0) == String::Get(wks_null) ? YES : NO;
      } else {
        return NO; // FIXME
      }

    case tk_array: {
      if (t != tk_array)
        return NO;
      auto a1 = FromHeapOffset<ArrayInfo>(m_offset);
      auto a2 = FromHeapOffset<ArrayInfo>(t.m_offset);
      if (a1->m_element != a2->m_element)
        return NO;
      if (a1->m_index == a2->m_index)
        return YES;
      if (a2->m_index == tk_unknown)
        return  YES;
      return NO;
    }

    case tk_set: {
      if (t != tk_set)
        return NO;
      auto s1 = FromHeapOffset<SetInfo>(m_offset);
      auto s2 = FromHeapOffset<SetInfo>(t.m_offset);
      return s1->m_element.IsSubtypeOf(s2->m_element);
    }

    case tk_tuple:
      return this->DropQualifiers() == t.DropQualifiers() ? YES : NO; // FIXME

    case tk_pseudo:
      if (*this == Type::PseudoFloat()) {
        return t == tk_float? YES : NO;
      } else if (*this == Type::PseudoInteger()) {
        if (t == tk_float)
          return YES;
        if (t == tk_integer)
          return MAYBE;  // might be out of range
      } else if (*this == Type::PseudoEnum()) {
        if (t == tk_float)
          return MAYBE;  // .nam and .infinity are legal
        if (t == tk_integer)
          return MAYBE;  // .min and .max are legal
        if (t == tk_pointer)
          return MAYBE;  // .null is legal
        if (t == tk_enum)
          return MAYBE;  // might not be in actual enum type
      }
      return NO;

    default:
      return NO;
  }
}

YesNoMaybe Type::IsPtrSubtypeOf(Type t) const {
  if (*this == ct_const && t == ct_mutable)
    return NO;

  switch (m_kind) {
    case tk_class:
      if (t != tk_class) {
        return NO;
      } else {
        auto ce = FromHeapOffset<ClassInfo>(m_offset)->m_entity;
        YesNoMaybe rv = ce->IsSubclassOf(t.Class());
        return rv == NO ? NO : YES;
      }

    case tk_array:
      if (t != tk_array) {
        return NO;
      } else {
        auto a1 = FromHeapOffset<ArrayInfo>(m_offset);
        auto a2 = FromHeapOffset<ArrayInfo>(t.m_offset);
        if (a1->m_element != a2->m_element)
          return NO;
        if (a1->m_index == a2->m_index)
          return YES;
        if (a2->m_index == tk_unknown)
          return  YES;
        return NO;
      }

    default:
      if (DropQualifiers() == t.DropQualifiers())
        return YES;
      else
        return NO;
  }
}

bool Type::IsSimpleType() const {
  // References are just pointers.
  if (*this != rt_rvalue)
    return true;

  ResolveEntities();

  switch (m_kind) {
    case tk_integer:
    case tk_float:
    case tk_set:
      return StorageSize() <= 8;

    case tk_class: {
      // FIXME: only classes with trivial copy constructors (straightforward
      // byte copy) are simple.
      // FIXME: hard-code certain epoch compile classes as simple, otherwise
      // bad things happen with LLVM target.  The native functions assume they
      // return their object value on the stack.
      auto ce = FromHeapOffset<ClassInfo>(m_offset)->m_entity;
      if (ce->IsMetaclass())
        return false;
      if (ce->GetId() != ::Class::wkc_not_well_known)
        return StorageSize() <= 8;
      if (ce->HasDestructor())
        return false;

      // FIXME: return Target::IsSimpleType(ce->AsType());
      return false;
    }

    case tk_tuple:
      return Target::IsSimpleType(*this);

    case tk_union:
      return false;

    case tk_array:
      return false;

    default:
      return true;
  }
}

bool Type::IsSimpleNonUnit() const {
  if (!IsSimpleType())
    return false;
  return m_kind != tk_void;
  // FIXME: do other effectively unit types, such as integer subranges like
  // 10 .. 10.
}

bool Type::HasDestructor() const {
  ResolveEntities();

  // References have no destructors, because they're pointers under the covers.
  if (m_ref != rt_rvalue)
    return false;

  switch (m_kind) {
    case tk_concept:
    case tk_generic:
      // FIXME: probably false; only literable values should be generic values.
      return false;

    case tk_class: {
      auto ce = FromHeapOffset<ClassInfo>(m_offset)->m_entity;
      return ce->HasDestructor();
    }

    case tk_array:
      return ElementType().HasDestructor();

    case tk_tuple: {
      auto ti = FromHeapOffset<TupleInfo>(m_offset);
      if (ti->m_hasDestructor != MAYBE) {
        return ti->m_hasDestructor == YES;
      } else {
        ti->m_hasDestructor = YES;  // assume for now
        for (size_t i = 0; i < ti->m_nFields; i++)
          if (ti->m_fields[i].m_type.HasDestructor())
            return true;
        ti->m_hasDestructor = NO;
        return false;
      }
    }

    case tk_union:
      // FIXME: fall through for now

    default:
      return false;
  }
}

void Type::ResolveEntities() const {
  switch (m_kind) {
    case tk_class: {
      auto ce = FromHeapOffset<ClassInfo>(m_offset)->m_entity;
      // FIXME: whether class's simplicity can be determined must be factored
      // in as well.
      ce->ResolveLayout();
      return;
    }

    case tk_tuple:
    case tk_concept:
    case tk_generic: {
      auto ti = FromHeapOffset<TupleInfo>(m_offset);
      for (size_t i = 0; i < ti->m_nFields; i++)
        ti->m_fields[i].m_type.ResolveEntities();
      return;
    }

    case tk_union: {
      auto ui = FromHeapOffset<UnionInfo>(m_offset);
      size_t count = ui->m_enum.IdentCount();
      for (size_t i = 0; i < count; i++)
        ui->m_enum.ValueAt(i)->AsType().ResolveEntities();
      return;
    }

    case tk_array:
      ElementType().ResolveEntities();
      return;

    case tk_pointer:
    case tk_fatpointer: {
      Type t = BaseType();
      if (t == tk_class)
        t.Class()->ResolveBases();
      return;
    }

    default:
      return;
  }
}

Type Type::Join(Type t1, Type t2) {
  // FIXME: many type kinds not supported yet.
  // FIXME: need to take Join concept into account.

  // If one is known and the other in unknown, the known wins.
  if (t1 == tk_unknown)
    return t2;
  else if (t2 == tk_unknown)
    return t1;

  if (t1 == tk_valueless || t2 == tk_valueless) {
    Type t1dq = t1.DropQualifiers();
    Type t2dq = t2.DropQualifiers();

    // When one is Transfers and the other isn't, the other wins.  Suppress,
    // NotTyped and Unbound have priority.
    if (t1dq == Transfers())
      return t2;
    else if (t2dq == Transfers())
      return t1;

    // When one is Unbound and the other isn't, the Unbound wins.  Unbound has
    // priority over Suppress and NotTyped.
    if (t1dq == Unbound() || t2dq == Unbound())
      return Unbound();

    // When one is Suppress and the other isn't, the Suppress wins.
    if (t1dq == Suppress() || t2dq == Suppress())
      return Suppress();

    // When one is NotTyped and the other isn't, the NotTyped wins.
    if (t1dq == NotTyped() || t2dq == NotTyped())
      return NotTyped();
  }

  // Figure out the const and reference qualifiers of the joined type.
  ConstType constQual = (t1 == ct_const || t2 == ct_const)
                         ? ct_const : ct_mutable;
  RefType refQual;
  if (t1 == rt_rvalue || t2 == rt_rvalue)
    refQual = rt_rvalue;
  else if (t1 == rt_rvalueref || t2 == rt_rvalueref)
    refQual = rt_rvalueref;
  else
    refQual = rt_lvalueref;

  t1 = t1.DropQualifiers();
  t2 = t2.DropQualifiers();

  // Joining an integer and a pseudo integer yields a pseudo integer.
  // Eventually, the literals will be lowered to a concrete type.  We cannot
  // do that here, because we don't have access to the literal value.
  if (t1 == tk_integer && t2 == Type::PseudoInteger())
    return t2;
  else if (t1 == Type::PseudoInteger() && t2 == tk_integer)
    return t1;

  // Likewise for floats, except that a pseudo integer or enum is converted
  // into a pseudo float.
  if (t1 == tk_float) {
    if (t2 == Type::PseudoFloat())
      return t2;
    else if (t2 == Type::PseudoInteger())
      return Type::PseudoFloat();
  } else if (t2 == tk_float) {
    if (t1 == Type::PseudoFloat())
      return t1;
    else if (t1 == Type::PseudoInteger())
      return Type::PseudoFloat();
  }

  // Likewise, we don't have access to pseudo enum literal values.  Do the
  // best we can for now--which is to convert to the appropriate pseudo type.
  // Inappropriate literals will be caught and diagnosed later when lowered.
  if (t1 == Type::PseudoEnum() || t2 == Type::PseudoEnum()) {
    Type t = t1 == Type::PseudoEnum() ? t2 : t1;

    if (t == Type::PseudoInteger()) {
      return t;
    } else if (t == Type::PseudoFloat()) {
      return t;
    } else if (t == Type::PseudoEnum()) {
      return t;
    } else if (t == tk_integer) {
      return Type::PseudoInteger();
    } else if (t == tk_float) {
      return Type::PseudoFloat();
    } else if (t == tk_enum) {
      return Type::PseudoEnum();
    }

    return NotTyped();
  }

  // Integer joins do something, due to widening, that doesn't happen for
  // other types, especially when identical.
  if (t1 == tk_integer && t2 == tk_integer) {
    auto si1 = FromHeapOffset<IntegerSubrangeInfo>(t1.m_offset);
    auto si2 = FromHeapOffset<IntegerSubrangeInfo>(t2.m_offset);

    // Cannot join signed and usigned ranges.
    if (si1->m_signed != si2->m_signed)
      return NotTyped();

    Integer *lb = si1->m_lower->Min(si2->m_lower);
    Integer *ub = si1->m_upper->Max(si2->m_upper);

    // Promote the joined range to the first of the following that contains
    // the entire range.
    struct Range {
      WellKnownInteger lower;
      WellKnownInteger upper;
    };

    const static Range s_ranges[] = {
      { wki_int32_min,  wki_int32_max },
      { wki_int64_min,  wki_int64_max }
    };

    const static Range u_ranges[] = {
      { wki_zero,       wki_uint32_max },
      { wki_zero,       wki_uint64_max }
    };

    auto &ranges = si1->m_signed ? s_ranges : u_ranges;

    for (int i = 0; i < 2; i++) {
      Integer *lower = Integer::Get(ranges[i].lower);
      Integer *upper = Integer::Get(ranges[i].upper);
      if (lb->Ge(lower) && ub->Le(upper))
        return Type(IntegerSubrange(lower, upper, si1->m_signed),
                    refQual, constQual);
    }

    return NotTyped();
  }

  // The simple case:  types are identical apart from qualifiers.
  if (t1 == t2) {
    t1.m_const = constQual;
    t1.m_ref = refQual;
    return t1;
  }

  // Join array types.  When only the index types differ, the join is
  // conformant.
  if (t1 == tk_array && t2 == tk_array) {
    if (t1.ElementType() == t2.ElementType()) {
      // Types cannot be identifical, so index types must differ.
      Type t = Type::ArrayOf(t1.ElementType());
      t.m_const = constQual;
      t.m_ref = refQual;
      return t;
    }
    return NotTyped();
  }

  // Given that one of the two types needs casting, the result must now be an
  // rvalue.
  // FIXME:  is this true?  If pointers to these two types can be joined, as
  // with pointers to classes, can it remain an l-value?
  refQual = rt_rvalue;

  // If one is a subtype of the other, the join is the other.
  if (t1.IsSubtypeOf(t2) == YES) {
    return Type(t2, refQual, constQual);
  } else if (t2.IsSubtypeOf(t1) == YES) {
    return Type(t1, refQual, constQual);
  }

  // In general, different type kinds cannot be joined.
  if (t1.m_kind != t2.m_kind)
    return NotTyped();

  // In general, the same type kinds cannot be joined if the above cases do
  // not handle them.
  return NotTyped();
}

void *&Type::TargetData() {
  static void *s_pseudo[t_MaxPseudo];
  static void *s_valueless[t_MaxValueless];
  static void *s_float[t_MaxFloat];
  static void *s_void;
  static void *s_type;
  static void *s_namespace;
  static void *s_bool;
  static void *s_method;
  static void *s_error;

  switch (m_kind) {
    case tk_pseudo:
      return s_pseudo[m_offset];

    case tk_void:
      return s_void;

    case tk_type:
      return s_type;

    case tk_namespace:
      return s_namespace;

    case tk_bool:
      return s_bool;

    case tk_float:
      return s_float[m_offset];

    case tk_function:
      return s_method;

    case tk_valueless:
      return s_valueless[m_offset];

    case tk_pointer:
    case tk_integer:
    case tk_char:
    case tk_enum:
    case tk_fatpointer:
    case tk_tuple:
    case tk_array:
    case tk_concept:
    case tk_generic:
    case tk_union:
      return FromHeapOffset<HashableInfo>(m_offset)->m_targetData;

    case tk_class:
      return FromHeapOffset<ClassInfo>(m_offset)->m_targetData;

    case tk_codefragment: // FIXME: ???
    default:
      verify(false);
      return s_error;
  }
}

Type Type::PerfectForward(Type t) const {
  if (m_ref != rt_rvalue)
    return *this;
  if (IsSimpleType())
    return *this;
  if (t == rt_lvalueref)
    return Const().LValueRef();
  return RValueRef();
}

void Type::AppendToHash(SHA512 &hash) const {
  switch (m_kind) {
    case tk_class:
      hash.Append('C');
      hash.Append(FromHeapOffset<ClassInfo>(m_offset)->m_entity->ExternalName());
      break;

    case tk_pointer:
    case tk_fatpointer:
      BaseType().AppendToHash(hash);
      hash.Append('*');
      break;

    case tk_void:
      hash.Append('V');
      break;

    case tk_type:
      hash.Append('T');
      break;

    case tk_namespace:
      hash.Append('N');
      break;

    case tk_tuple: {
      hash.Append('t');
      auto ti = FromHeapOffset<TupleInfo>(m_offset);

      hash.Append('(');
      for (size_t i = 0; i < ti->m_nFields; i++) {
        if (i > 0)
          hash.Append(',');
        if (String *name = ti->m_fields[i].m_name)
          name->AppendToHash(hash);
        else
          hash.AppendString("", 0);
        ti->m_fields[i].m_type.AppendToHash(hash);
      }
      hash.Append(')');
      break;
    }

    case tk_concept:
      verify(false);
      break;

    case tk_generic: {
      hash.Append('g');
      auto ti = FromHeapOffset<TupleInfo>(m_offset);

      hash.Append('(');
      for (size_t i = 0; i < ti->m_nFields; i++) {
        if (i > 0)
          hash.Append(',');
        ti->m_fields[i].m_type.AppendToHash(hash);
      }
      hash.Append(')');
      break;
    }

    case tk_union:
      hash.Append('U');
      FromHeapOffset<UnionInfo>(m_offset)->m_enum.AppendToHash(hash);
      break;

    case tk_function:
      verify(false); // FIXME
      break;

    case tk_bool:
      hash.Append('B');
      break;

    case tk_float:
      hash.Append('R');
      switch (m_offset) {
        case t_Float:       hash.Append('4');         break;
        case t_Double:      hash.Append('8');         break;
      };
      break;

    case tk_char:
      if (*this == Char8()) {
        hash.Append('h');
        hash.Append('1');
      } else if (*this == Char16()) {
        hash.Append('h');
        hash.Append('2');
      } else if (*this == Char32()) {
        hash.Append('h');
        hash.Append('4');
      } else {
        hash.Append('H');
        auto csi = FromHeapOffset<CharSubrangeInfo>(m_offset);
        hash.Append(csi->m_lower);
        hash.Append(csi->m_upper);
      }
      break;

    case tk_integer:
      if (*this == SByte()) {
        hash.Append('s');
        hash.Append('1');
      } else if (*this == Byte()) {
        hash.Append('u');
        hash.Append('1');
      } else if (*this == Short()) {
        hash.Append('s');
        hash.Append('2');
      } else if (*this == UShort()) {
        hash.Append('u');
        hash.Append('2');
      } else if (*this == Int()) {
        hash.Append('s');
        hash.Append('4');
      } else if (*this == UInt()) {
        hash.Append('u');
        hash.Append('4');
      } else if (*this == Long()) {
        hash.Append('s');
        hash.Append('8');
      } else if (*this == ULong()) {
        hash.Append('u');
        hash.Append('8');
      } else {
        hash.Append('I');
        auto isi = FromHeapOffset<IntegerSubrangeInfo>(m_offset);
        isi->m_lower->AppendToHash(hash);
        isi->m_upper->AppendToHash(hash);
      }
      break;

    case tk_enum: {
      hash.Append('E');
      hash.Append('(');
      auto ei = FromHeapOffset<EnumInfo>(m_offset);
      for (size_t i = 0; i < ei->m_count; i++) {
        if (i > 0)
          hash.Append(',');
        ei->m_names[i]->AppendToHash(hash);
        if (ei->m_values)
          ei->Values()[i]->AppendToHash(hash, false);
      }
      hash.Append(')');
      break;
    }

    case tk_array:
      ElementType().AppendToHash(hash);
      hash.Append('[');
      if (Type t = IndexType(); t.IsKnown())
        t.AppendToHash(hash);
      hash.Append(']');
      break;

    case tk_set:
      hash.Append('S');
      hash.Append('<');
      ElementType().AppendToHash(hash);
      hash.Append('>');
      break;

    case tk_codefragment:
      hash.Append('{');
      if (Type t = BaseType(); t.IsKnown())
        t.AppendToHash(hash);
      hash.Append('}');
      break;

    default:
      verify(false);
  }

  if (m_const == ct_const)
    hash.Append('#');

  if (m_ref == rt_lvalueref) {
    hash.Append('&');
  } else if (m_ref == rt_rvalueref) {
    hash.Append('^');
  }
}

Type Type::GetClassType(::Class *ce) {
  Offset offset = TypeAlloc(sizeof(ClassInfo));
  auto ci = FromHeapOffset<ClassInfo>(offset);
  ci->m_entity = ce;
  ci->m_targetData = nullptr;
  return Type(tk_class, offset);
}

void Type::UpdateClass(::Class *ce) {
  class SHA512;
  verify(!ce || *this == ce->AsType());
  auto ci = FromHeapOffset<ClassInfo>(m_offset);
  ci->m_entity = ce;
}

bool Type::Dump(BufferWriter &bw) const {
  const char *n = "";

  // Note: the checks of m_offset against g_heapEnd are *not* redundant and
  // must not be removed.  This code executes inside Xcode, where it will be
  // asked to format uninitialized memory that has random junk in it.  A
  // verify failure in FromHeapOffset will require a force quit of Xcode!

  switch (m_kind) {
    case tk_unknown:
      n = "$unknown";
      break;

    case tk_class:
      if (m_offset >= g_heapEnd)
        break;
      n = FromHeapOffset<ClassInfo>(m_offset)->m_entity->Name()->c_str();
      break;

    case tk_pointer:
      if (m_offset >= g_heapEnd)
        break;
      if (FromHeapOffset<ThinPointerInfo>(m_offset)->m_base.Dump(bw))
        return true;
      if (bw.Append('*'))
        return true;
      break;

    case tk_fatpointer:
      if (m_offset >= g_heapEnd)
        break;
      if (FromHeapOffset<FatPointerInfo>(m_offset)->m_base.BaseType().Dump(bw))
        return true;
      if (bw.Append('*'))
        return true;
      if (bw.IsDebug() && bw.Append('+'))
        return true;
      break;

    case tk_void:
      n = "void";
      break;

    case tk_type:
      n = "type";
      break;

    case tk_namespace:
      n = "namespace";
      break;

    case tk_pseudo:
      switch (m_offset) {
        case t_PseudoInteger:  n = "$integer";        break;
        case t_PseudoFloat:    n = "$float";          break;
        case t_PseudoChar:     n = "$char";           break;
        case t_PseudoString:   n = "$string";         break;
        case t_PseudoList:     n = "$list";           break;
        case t_PseudoEnum:     n = "$enum";           break;
      }
      break;

    case tk_tuple: {
      if (m_offset >= g_heapEnd)
        break;
      if (bw.Append('('))
        return true;
      auto ti = FromHeapOffset<TupleInfo>(m_offset);
      for (size_t i = 0; i < ti->m_nFields; i++) {
        if (i > 0 && bw.Append(','))
          return true;
        if (String *name = ti->m_fields[i].m_name) {
          if (bw.Append(name))
            return true;
          if (bw.Append(':'))
            return true;
        }
        if (ti->m_fields[i].m_type.Dump(bw))
          return true;
      }
      if (ti->m_nFields == 1 && bw.Append(','))
        return true;
      if (bw.Append(')'))
        return true;
      break;
    }

    case tk_concept:
    case tk_generic: {
      if (m_offset >= g_heapEnd)
        break;
      if (m_kind == tk_concept) {
        if (bw.Append("concept(", 8))
          return true;
      } else /* tk_generic */ {
        if (bw.Append("generic(", 8))
          return true;
      }
      auto ti = FromHeapOffset<TupleInfo>(m_offset);
      for (size_t i = 0; i < ti->m_nFields; i++) {
        if (i > 0 && bw.Append(','))
          return true;
        if (ti->m_fields[i].m_type.Dump(bw))
          return true;
      }
      if (bw.Append(')'))
        return true;
      break;
    }

    case tk_union: {
      if (m_offset >= g_heapEnd)
        break;
      if (bw.Append("union(", 6))
        return true;
      auto ui = FromHeapOffset<UnionInfo>(m_offset);
      size_t count = ui->m_enum.IdentCount();
      for (size_t i = 0; i < count; i++) {
        if (i > 0 && bw.Append(','))
          return true;
        String *s = ui->m_enum.IdentAt(i);
        if (bw.Append(s))
          return true;
        if (bw.Append(':'))
          return true;
        if (ui->m_enum.ValueAt(i)->AsType().Dump(bw))
          return true;
      }
      if (bw.Append(')'))
        return true;
      break;
    }

    case tk_function: {
      if (m_offset >= g_heapEnd)
        break;
      auto fi = FromHeapOffset<FunctionInfo>(m_offset);
      if (bw.Append('('))
        return true;
      size_t j = 0;
      if (fi->m_method) {
        if (bw.Append('['))
          return true;
        if (fi->m_argTypes[0].Dump(bw))
          return true;
        if (bw.Append(']'))
          return true;
        j++;
      }
      for (size_t i = j; i < fi->m_nArgs; i++) {
        if (i > j && bw.Append(", ", 2))
          return true;
        if (fi->m_argTypes[i].Dump(bw))
          return true;
      }
      if (bw.Append(")->", 3))
        return true;
      if (fi->m_retType.Dump(bw))
        return true;
      if (fi->m_throws && bw.Append(" throws", 7))
        return true;
      // FIXME: variadic
      break;
    }

    case tk_valueless:
      switch (m_offset) {
        case t_Transfers:   n = "$transfers";         break;
        case t_Unbound:     n = "$unbound";           break;
        case t_Suppress:    n = "$suppress";          break;
        case t_NotTyped:    n = "$notTyped";          break;
      }
      break;

    case tk_bool:
      n = "bool";
      break;

    case tk_float:
      switch (m_offset) {
        case t_Float:       n = "float";              break;
        case t_Double:      n = "double";             break;
      };
      break;

    case tk_char: {
      if (m_offset >= g_heapEnd)
        break;
      auto csi = FromHeapOffset<CharSubrangeInfo>(m_offset);
      if (csi->m_printable) {
        n = csi->m_printable;
      } else {
        if (bw.Append('\''))
          return true;
        if (bw.Append(char(csi->m_lower)))
          return true;
        if (bw.Append("'..'", 4))
          return true;
        if (bw.Append(char(csi->m_upper)))
          return true;
        if (bw.Append('\''))
          return true;
      }
      break;
    }

    case tk_integer: {
      if (m_offset >= g_heapEnd)
        break;
      auto isi = FromHeapOffset<IntegerSubrangeInfo>(m_offset);
      if (isi->m_printable) {
        n = isi->m_printable;
      } else {
        if (bw.Append(isi->m_lower->AsString()))
          return true;
        if (!isi->m_signed)
          if (bw.Append('U'))
            return true;
        if (bw.Append("..", 2))
          return true;
        if (bw.Append(isi->m_upper->AsString()))
          return true;
        if (!isi->m_signed)
          if (bw.Append('U'))
            return true;
      }
      break;
    }

    case tk_enum: {
      if (m_offset >= g_heapEnd)
        break;
      auto ei = FromHeapOffset<EnumInfo>(m_offset);
      if (ei->m_valueType != tk_void) {
        ei->m_valueType.Dump(bw);
        if (bw.Append(' '))
          return true;
      }
      if (bw.Append("enum(", 5))
        return true;
      for (size_t i = 0; i < ei->m_count; i++) {
        if (i > 0 && bw.Append(','))
          return true;
        String *s = ei->m_names[i];
        if (bw.Append(s))
          return true;
      }
      if (bw.Append(')'))
        return true;
      break;
    }

    case tk_array: {
      if (m_offset >= g_heapEnd)
        break;
      auto ai = FromHeapOffset<ArrayInfo>(m_offset);
      ai->m_element.Dump(bw);
      if (bw.Append('['))
        return true;
      if (ai->m_index.IsKnown())
        ai->m_index.Dump(bw);
      if (bw.Append(']'))
        return true;
      break;
    }

    case tk_set: {
      if (m_offset >= g_heapEnd)
        break;
      auto si = FromHeapOffset<SetInfo>(m_offset);
      if (bw.Append("set<", 4))
        return true;
      if (si->m_element.Dump(bw))
        return true;
      if (bw.Append('>'))
        return true;
      break;
    }

    case tk_codefragment:
      if (m_offset >= g_heapEnd)
        break;
      if (bw.Append("codefragment_t<", 15))
        return true;
      if (Type t = FromHeapOffset<CodeFragmentInfo>(m_offset)->m_base;
          t.IsKnown()) {
        if (t.Dump(bw))
          return true;
      }
      if (bw.Append('>'))
        return true;
      break;
  }

  if (bw.Append(n, strlen(n)))
    return true;

  if (m_const == ct_const)
    if (bw.Append(" const", 6))
      return true;

  if (m_ref == rt_lvalueref) {
    if (bw.Append('&'))
      return true;
  } else if (m_ref == rt_rvalueref) {
    if (bw.Append("&&", 2))
      return true;
  }

  return false;
}

// FIXME:  There's GOT to be a better way of making this work!
const char *LLDBFormatType(int o, int k, int r, int c) {
  Type t;
  t.m_offset = o;
  t.m_kind = k;
  t.m_ref = r;
  t.m_const = c;

  return t.Dump();
}

const char *Type::Dump() const {
  DebugBuffer db;
  Dump(db);
  return db.Get();
}

const char *FormatType(Type t) {
  return t.Dump();
}
