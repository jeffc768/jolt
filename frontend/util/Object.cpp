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

#include "Object.h"
#include "InDeflator.h"
#include "type/Type.h"
#include "util/DeflatedObject.h"
#include "util/Object.h"
#include <memory.h>

Metadata Object::s_metadata { nullptr };

Object::Object(Inflator &IF) {
  IF.RegisterObject(this);
}

Parameters **Object::GetTemplateRootHolder() {
  return nullptr;
}

namespace {
  struct Block {
    Block    *m_next;
    char     *m_cursor;
    unsigned  m_free;
    char      m_storage[4];

    static Block *New(unsigned size) {
      Block *b = reinterpret_cast<Block *>(malloc(size));
      b->m_next = nullptr;
      b->m_cursor = b->m_storage;
      b->m_free = size - sizeof(Block) + 4;
      return b;
    }
  };
}

static unsigned s_nblocks = 1;
static unsigned s_blocksize = 0x100000;
static Block   *s_firstblock = Block::New(s_blocksize);
static Block   *s_lastblock = s_firstblock;

/******************************************************************************/

static inline size_t AdjustSize(size_t size) {
  return (size + sizeof(void *) - 1) & (1 + ~sizeof(void *));
}

// Allocate space for a new object.
void *Object::operator new(size_t size) {
  size = AdjustSize(size);

  if (s_lastblock->m_free < size) {
    unsigned s = unsigned(size + sizeof(Block) - 4);
    s = s > s_blocksize ? s : s_blocksize;

    s_lastblock->m_next = Block::New(s);
    s_lastblock = s_lastblock->m_next;
    s_nblocks++;
  }

  void *p = reinterpret_cast<void *>(s_lastblock->m_cursor);
  s_lastblock->m_cursor += size;
  s_lastblock->m_free -= unsigned(size);
  return p;
}

/******************************************************************************/

// A program termination, clean up and release managed memory.  Do this mainly
// to make valgrind happy.

class Object::ReleaseAllMemory {
  public:
    ~ReleaseAllMemory() {
      while (s_firstblock) {
        Block *b = s_firstblock;
        s_firstblock = b->m_next;

        char *p = b->m_storage;
        while (p < b->m_cursor) {
          Object *obj = reinterpret_cast<Object *>(p);
          size_t size = AdjustSize(obj->GetObjectSize());
          obj->PreDestroy();
          obj->~Object();
          p += size;
        }

        free(b);
      }
    }
};

Object::ReleaseAllMemory Object::s_ram;
