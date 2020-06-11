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

// Utility hash table for deduplicating Strings and Integers.

#pragma once

#include "Hash.h"
#include <memory.h>
#include <stdint.h>

template<class T> class Deduplicator {
public:
  using HT   = typename T::HashTrait;
  using ELEM = typename HT::ELEM;
  using SIZE = typename HT::SIZE;

  static const int NBUCKETS = 4096;   // must be power of two

  struct HashBucket {
    HashBucket     *m_next;
    uint32_t        m_hash;
    T               m_data;
  };

  HashBucket      **m_buckets;
  uint32_t          m_limit;
  uint32_t          m_count;

  Deduplicator()
      : m_buckets(new HashBucket*[NBUCKETS]),
        m_limit(NBUCKETS - 1),
        m_count(0) {
    memset(m_buckets, 0, NBUCKETS * sizeof(HashBucket *));
  }

  ~Deduplicator() {
    for (size_t i = 0; i <= m_limit; i++) {
      HashBucket *b = m_buckets[i];
      while (b) {
        HashBucket *n = b->m_next;
        free(b);
        b = n;
      }
    }

    delete[] m_buckets;
  }

  T *FindOrAdd(const ELEM *elem, SIZE size) {
    uint32_t hash = Hash(elem, size);

    // Search hash table.
    HashBucket **pHead = m_buckets + (hash & m_limit);
    for (HashBucket *b = *pHead; b; b = b->m_next)
      if (hash == b->m_hash && size == HT::Size(b->m_data) &&
          !memcmp(elem, HT::Data(b->m_data), size * sizeof(ELEM)))
        return &b->m_data;

    // Not found -- go add it.
    HashBucket *p = reinterpret_cast<HashBucket *>(
        malloc(sizeof(HashBucket) + size * sizeof(ELEM)
                                  - HT::base * sizeof(ELEM)));
    HT::New(p->m_data, elem, size);
    p->m_hash = hash;
    p->m_next = *pHead;
    *pHead = p;

    // If the table has become too full, we need to double its size or else
    // collisions become too frequent.  But beware trying to keep the
    // collision rate too low, or the savings is wiped out by excessive
    // rehashing!
    if (++m_count > m_limit) {
      unsigned newLimit = m_limit * 2 + 1;
      HashBucket **newHash = new HashBucket*[newLimit + 1];
      memset(newHash, 0, (newLimit + 1) * sizeof(HashBucket *));

      // Rehash entries to bigger table.
      for (unsigned i = 0; i <= m_limit; i++) {
        HashBucket *next = m_buckets[i];
        while (HashBucket *t = next) {
          pHead = newHash + (t->m_hash & newLimit);
          next = t->m_next;
          t->m_next = *pHead;
          *pHead = t;
        }
      }

      // Finally, swap new for old.
      delete[] m_buckets;
      m_buckets = newHash;
      m_limit = newLimit;
    }

    return &p->m_data;
  }
};
