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

// The bytecode interpreter provides for compile time evaluation of Jolt code.
// The following classes allow for the generation and execution of bytecodes.
//
//   BasicBlock
//     Represents a sequence of bytecodes that can only be entered at the
//     beginning and exited at the end.  Used only during bytecode generation.
//
//   Function
//     Abstract base class for functions that are callable from or implemented
//     as bytecodes.  A bytecode call instruction refers to a Function object,
//     not to the function's bytecodes, eliminating the need to backpatch
//     forward references.  Function objects are NOT managed and never move.
//
//   BytecodeFunction
//     Represents the bytecode representation of a function.  A BytecodeFunction
//     object is normally created before its bytecode is generated.
//
//   NativeFunction
//     Abstract base class for native functions implemented in C++ inside the
//     compiler.
//
//   Interpreter
//     Interpreter global state.

#pragma once

#include "type/Type.h"
#include <string>
#include <vector>

class Expr;
class Type;

using std::vector;

namespace BC {

class Interpreter;

enum Opcode {
  op_add32,
  op_add64,
  op_addf32,
  op_addf64,
  op_and32,
  op_and64,
  op_call,
  op_callfat,
  op_callval,
  op_convf32tof64,
  op_convf32toi32,
  op_convf32toi64,
  op_convf64tof32,
  op_convf64toi32,
  op_convf64toi64,
  op_convi32tof32,
  op_convi32tof64,
  op_convi64tof32,
  op_convi64tof64,
  op_convs32to64,
  op_convu32to64,
  op_conv64to32,
  op_divf32,
  op_divf64,
  op_divs32,
  op_divs64,
  op_divu32,
  op_divu64,
  op_dup,
  op_exts32,
  op_exts64,
  op_extz32,
  op_extz64,
  op_brfalse8,
  op_brfalse16,
  op_brfalse32,
  op_brtrue8,
  op_brtrue16,
  op_brtrue32,
  op_jump8,
  op_jump16,
  op_jump32,
  op_leaargs8,
  op_leaargs16,
  op_leaargs32,
  op_leaframe8,
  op_leaframe16,
  op_leaframe32,
  op_leaptr8,
  op_leaptr16,
  op_leaptr32,
  op_loads8,
  op_loadu8,
  op_loads16,
  op_loadu16,
  op_load32,
  op_load64,
  op_loadfat,
  op_mulf32,
  op_mulf64,
  op_muls32,
  op_muls64,
  op_mulu32,
  op_mulu64,
  op_neg32,
  op_neg64,
  op_negf32,
  op_negf64,
  op_not32,
  op_not64,
  op_or32,
  op_or64,
  op_pop,
  op_pop8,
  op_pop16,
  op_pushp8,
  op_pushn8,
  op_pushp16,
  op_pushn16,
  op_push32,
  op_push64,
  op_rems32,
  op_rems64,
  op_remu32,
  op_remu64,
  op_ret,
  op_retfat,
  op_retval,
  op_seteq32,
  op_seteq64,
  op_seteqf32,
  op_seteqf64,
  op_setgef32,
  op_setgef64,
  op_setges32,
  op_setges64,
  op_setgeu32,
  op_setgeu64,
  op_setgtf32,
  op_setgtf64,
  op_setgts32,
  op_setgts64,
  op_setgtu32,
  op_setgtu64,
  op_setlef32,
  op_setlef64,
  op_setles32,
  op_setles64,
  op_setleu32,
  op_setleu64,
  op_setltf32,
  op_setltf64,
  op_setlts32,
  op_setlts64,
  op_setltu32,
  op_setltu64,
  op_setne32,
  op_setne64,
  op_setnef32,
  op_setnef64,
  op_shl32,
  op_shl64,
  op_shrs32,
  op_shrs64,
  op_shru32,
  op_shru64,
  op_store8,
  op_store16,
  op_store32,
  op_store64,
  op_storefat,
  op_subf32,
  op_subf64,
  op_sub32,
  op_sub64,
  op_swap,
  op_swap8,
  op_swap16,
  op_xor32,
  op_xor64,

  // These instructions never occur in executable bytecodes.  op_exit and op_if
  // are temporary placeholders for other instructions.  op_fallthrough marks
  // a block that otherwise has no terminating instruction.
  op_exit,
  op_fallthrough,
  op_if,

  op_end_of_opcodes
};

// A value on the interpreter stack.  Note that this union cannot be used to
// implicitly cast a 64- to 32-bit integer; it won't work on big endian CPUs.
union Union {
  int32_t    m_i32;
  uint32_t   m_u32;
  int64_t    m_i64;
  uint64_t   m_u64;
  float      m_f32;
  double     m_f64;

  void *AsAddr() {
    if constexpr (sizeof(void *) == 4)
      return reinterpret_cast<void *>(intptr_t(m_u32));
    else
      return reinterpret_cast<void *>(intptr_t(m_u64));
  }

  void SetAddr(const void *p) {
    if constexpr (sizeof(void *) == 4)
      m_u32 = unsigned(reinterpret_cast<intptr_t>(p));
    else
      m_u64 = reinterpret_cast<intptr_t>(p);
  }

  Type AsType() {
    if constexpr (sizeof(void *) == 4)
      return *reinterpret_cast<Type *>(&m_u32);
    else
      return *reinterpret_cast<Type *>(&m_u64);
  }

  void SetType(Type t) {
    if constexpr (sizeof(Type) == 4)
      *reinterpret_cast<Type *>(&m_u32) = t;
    else
      *reinterpret_cast<Type *>(&m_u64) = t;
  }

  size_t AsSizeT() {
    if constexpr (sizeof(size_t) == 4)
      return size_t(m_u32);
    else
      return size_t(m_u64);
  }

  void SetSizeT(size_t s) {
    if constexpr (sizeof(size_t) == 4)
      m_u32 = uint32_t(s);
    else
      m_u64 = uint64_t(s);
  }

  void SetBool(bool v) {
    union {
      int  m_int;
      bool m_bools[sizeof(int)];
    } u;

    u.m_int = 0;
    u.m_bools[0] = v;
    m_i32 = u.m_int;
  }

  void SetBool(unsigned v) {
    verify(v == 0 || v == 1);
    SetBool(bool(v));
  }
};

class BasicBlock {
  friend class Expr;

private:
  // not allowed
  BasicBlock(const BasicBlock &that);
  BasicBlock &operator=(const BasicBlock &that);

public:
  using BasicBlockVector = vector<BasicBlock *>;
  BasicBlock(BasicBlock *chain);

  void Append(Opcode op);
  void AppendBlock(BasicBlock *target);
  void AppendByte(unsigned val);
  void AppendWord(unsigned val);
  void AppendDWord(uint32_t val);
  void AppendQWord(uint64_t val);
  void AppendLea(Opcode op, unsigned val);
  void AppendLoad(Type t);
  void AppendPush(int val);
  void AppendPush(unsigned val);
  void AppendPush(int64_t val);
  void AppendPush(uint64_t val);
  void AppendPush(const void *val);
  void AppendPush(float val);
  void AppendPush(double val);
  void AppendStore(Type t);

  // Mark this basic block as the target of non-local exits.  Any op_exit to
  // this block will pop as much stack as necessary to match the stack depth
  // at the start of base, with optional adjustment.  The keepValue flag
  // indicates whether the value at the top of the stack must be preserved;
  // the value is popped, the stack shrunk to match base + adjustment, and then
  // pushed back.
  void SetMerge(BasicBlock *base, int adjustment, bool keepValue);

  BasicBlock *GetChain() { return m_chain; }

  // Assemble a collection of basic blocks into a linearized bytecode blob.
  static uint8_t *AssembleBlocks(BasicBlock *entry, size_t &maxStack);

  static void DumpInstr(uint8_t *pc, uint8_t *base, int depth);

private:
  // Passes called by AssembleBlocks().
  static void DetermineStackDepths(BasicBlock *entry, size_t &maxStack);
  static void ConvertExitsToJumps(BasicBlock *entry);
  static void RemoveEmptyBlocks(BasicBlock *entry);
  static void OrderBlocks(BasicBlock *entry, BasicBlockVector &ordering);
  static size_t AssignOffsets(BasicBlockVector &ordering);
  static uint8_t *Linearize(BasicBlockVector &ordering, size_t imageSize);

  static void Dump(BasicBlock *entry, const char *title);

  // Get stack depth on entry, taking into account merge blocks.
  size_t GetStackOnEntry();

  // Chain of every basic block created by a state.
  BasicBlock         *m_chain;

  // The bytes of the block, excluding branch target.
  vector<uint8_t>     m_bytes;
  int                 m_offsetLastOpcode;

  // Successor blocks of this block.  The number depends on the terminating
  // instruction of this block.
  BasicBlockVector    m_successors;

  // Track the size of the operand stack.
  size_t              m_stackOnEntry;
  size_t              m_stackOnExit;

  // Information for merge blocks.
  BasicBlock         *m_baseBlock;    // Non-null only for merge blocks
  int                 m_adjustment;
  bool                m_keepValue;

  // For linearization of blocks.
  unsigned short      m_token;
  char                m_branchSize;
  size_t              m_offset;
  bool                m_fallThrough;
};

class Function {
protected:
  static const Union s_null;

public:
  // Be careful deleting Functions!  Be absolutely sure that no other function
  // references this one.
  virtual ~Function() { }

  // Call this function from inside the interpreter.
  virtual Union Call(Interpreter &I, size_t argcnt, Union *args,
                     Union *extra = nullptr) = 0;
};

class BytecodeFunction: public Function {
public:
  BytecodeFunction();
  virtual ~BytecodeFunction();

  // Execute function, with the return value stored in the rvsize bytes starting
  // at address rv.  Zeros can be passed for a function with no out arguments.
  void Execute(void *rv, size_t rvsize);

  // Set and query whether this function has a return value.  If the return
  // value is saved in a buffer whose address is passed as an argument, this is
  // false.
  void SetReturnsValue(bool f) { m_returnsValue = f; }
  bool ReturnsValue() { return m_returnsValue; }

  // Convert the provided collection of basic blocks into a linear blob and
  // keep the blob.
  void SetBytecode(BasicBlock *entry, size_t maxfs);

  virtual Union Call(Interpreter &I, size_t argcnt, Union *args,
                     Union *extra = nullptr);

private:
  // The bytecode implementation of this function.  May be null if it hasn't
  // been generated yet.
  uint8_t            *m_bytecode;

  // Does the function return its value on the stack or is the address of the
  // return value passed as an argument?
  bool                m_returnsValue;

  // The amount of stack space needed by this function for bytecode operands,
  // local variables and temporaries.
  size_t              m_maxStackSize;
  size_t              m_maxFrameSize;
};

class NativeFunction: public Function {
  const char         *m_name;

public:
  NativeFunction(const char *name) : m_name(name) { }

  virtual Union Call(Interpreter &I, size_t argcnt, Union *args,
                     Union *extra = nullptr) = 0;

  const char *Name() { return m_name; }
};

class Interpreter {
public:
  Interpreter() { }
};

}
