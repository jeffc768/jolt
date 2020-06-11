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

#include "Bytecode.h"
#include "Target.h"
#include "TranslateClosure.h"
#include "entity/Argument.h"
#include "entity/BaseSpecifier.h"
#include "entity/Class.h"
#include "entity/Const.h"
#include "entity/Field.h"
#include "entity/FormalArguments.h"
#include "entity/Method.h"
#include "entity/Scope.h"
#include "node/Attributes.h"
#include "node/Binary.h"
#include "node/Block.h"
#include "node/BuildPointer.h"
#include "node/Call.h"
#include "node/CallBuiltin.h"
#include "node/Cast.h"
#include "node/Construct.h"
#include "node/Deref.h"
#include "node/Expr.h"
#include "node/ExtractAddress.h"
#include "node/ExtractDescriptor.h"
#include "node/FieldAddr.h"
#include "node/GlobalAddr.h"
#include "node/If.h"
#include "node/Index.h"
#include "node/Label.h"
#include "node/Literal.h"
#include "node/LiteralAddr.h"
#include "node/Load.h"
#include "node/MethodBody.h"
#include "node/Sequence.h"
#include "node/Shared.h"
#include "node/Store.h"
#include "node/Transfer.h"
#include "node/Unary.h"
#include "node/VarAddr.h"
#include "node/VarDecl.h"
#include "node/Vtable.h"
#include "node/VtableCast.h"
#include "node/VtableSlot.h"
#include "node/While.h"
#include "type/Type.h"
#include "util/Integer.h"
#include "util/String.h"
#include "util/Value.h"
#include <map>
#include <stdio.h>
#include <string.h>

// Detect endianness of machine.
namespace {
  static struct EndianDetector {
    union {
      int  m_int;
      bool m_bools[sizeof(int)];
    } m_data;

    EndianDetector() { m_data.m_int = 1; }
  } g_littleEndian;
}

static bool IsLittleEndian() {
  return g_littleEndian.m_data.m_bools[0];
}

class BytecodeTarget: public Target {
public:
  BytecodeTarget() : Target("bytecode") { }
  ~BytecodeTarget() { }

  virtual void Setup() { }

  virtual TargetClass *For(Class *);
  virtual TargetConst *For(Const *);
  virtual TargetGlobal *For(Field *);
  virtual TargetMethod *For(Method *);
  virtual TargetMethod *For(Method *, int);

  virtual Value *Evaluate(TranslateClosure &tc, Value *result,  Expr *expr);

  virtual void WriteToFile(const char *fname) {
    verify(0 && "Bytecode interpreter cannot write to file.");
  }

  virtual bool IsSimpleType_(Type t) {
    verify(0 && "Bytecode interpreter is not a run target.");
    return false;
  }

  virtual vector<size_t> GetStructLayout_(Type t) {
    verify(0 && "Bytecode interpreter is not a run target.");
    return { };
  };

  virtual unsigned GetAlignment_(Type t) {
    verify(0 && "Bytecode interpreter is not a run target.");
    return 0;
  }
};

static BytecodeTarget BCI;

Target *Target::GetBytecodeInterpreter() {
  return &BCI;
}

static Target *g_targets = nullptr;       // Chain of all available targets.
Target *Target::s_runTarget = nullptr;

Target::Target(const char *name)
  : m_name(name),
    m_link(g_targets) {
  g_targets = this;
}

bool Target::SetTarget(const char *name) {
  verify(!s_runTarget);
  for (Target *t = g_targets; t; t = t->m_link) {
    if (t == &BCI)
      continue;
    if (strcmp(name, t->m_name) == 0) {
      s_runTarget = t;
      return true;
    }
  }
  return false;
}

bool Target::IsSimpleType(Type t) {
  verify(t == tk_tuple || t == tk_class);
  return s_runTarget->IsSimpleType_(t);
}

vector<size_t> Target::GetStructLayout(Type t) {
  verify(t == tk_tuple || t == tk_class);
  return s_runTarget->GetStructLayout_(t);
}

unsigned Target::GetAlignment(Type t) {
  return s_runTarget->GetAlignment_(t);
}

class BytecodeClass: public TargetClass {
  DECLARE_OBJECT(BytecodeClass)

public:
  BytecodeClass(Class *ce) : TargetClass(ce) { }

  virtual void Generate();
  virtual bool HasGlobalStorage(Epoch ep);
  virtual void *GlobalStorage(Epoch ep);

  void               *m_storage = nullptr;
};

class BytecodeConst: public TargetConst {
  DECLARE_OBJECT(BytecodeConst)

public:
  BytecodeConst(Const *ce) : TargetConst(ce) { }

  virtual void Generate();
  virtual void *GlobalStorage(Epoch ep);

  void               *m_storage = nullptr;
};

class BytecodeGlobal: public TargetGlobal {
  DECLARE_OBJECT(BytecodeGlobal)

public:
  BytecodeGlobal(Field *fe) : TargetGlobal(fe) { }

  virtual void Generate();
  virtual void *GlobalStorage(Epoch ep);

  void               *m_storage = nullptr;
};

class BytecodeMethod: public TargetMethod {
  DECLARE_OBJECT(BytecodeMethod)

public:
  BytecodeMethod(Method *me) : TargetMethod(me) { }
  BytecodeMethod(Method *me, int nm)
    : TargetMethod(me), m_function(GetNativeFunction(nm)) { }

  virtual void Generate();
  virtual void *GlobalStorage(Epoch ep);

  BC::Function *GetNativeFunction(int nm);

  // The bytecode representation of this method.  A non-null value does not mean
  // the bytecodes have been generated yet.
  BC::Function       *m_function = nullptr;
};

/******************************************************************************/

class ToBytecode {
public:
  ~ToBytecode() {
    while (m_chain) {
      BC::BasicBlock *b = m_chain;
      m_chain = b->GetChain();
      delete b;
    }
  }

  BC::BasicBlock *NewBlock() {
    m_chain = new BC::BasicBlock(m_chain);
    return m_chain;
  }

  // Translate an expression subtree into bytecodes.  Any instructions created
  // are appended to the basic block provided.  The value of an instruction, if
  // any, is left on the interpreter stack.
  //
  // A node that alters flow of control may also substitute a new basic block
  // for the one provided.  For example, an if node would create a new block
  // representing the merged flow of control from the two alternatives.  This
  // block would substitute for the original.  When code following the node is
  // unreachable, because the node causes a non-local transfer of control, it
  // substitutes null for the basic block pointer.  All callers MUST check for
  // this and handle it appropriately.
  //
  // A full expression marks the scope of temporary variables.  Regardless of
  // where they are initialized, they are not destructed until the end of the
  // nearest enclosing full expression.
  void TranslateFullExpr(BC::BasicBlock *&b, Node *e);
  void Translate(BC::BasicBlock *&b, Node *e);
  void Translate_(BC::BasicBlock *&b, Binary *bn);
  void Translate_(BC::BasicBlock *&b, Block *bn);
  void Translate_(BC::BasicBlock *&b, Call *cn);
  void Translate_(BC::BasicBlock *&b, CallBuiltin *cbn);
  void Translate_(BC::BasicBlock *&b, Cast *cn);
  void Translate_(BC::BasicBlock *&b, Construct *cn);
  void Translate_(BC::BasicBlock *&b, If *in);
  void Translate_(BC::BasicBlock *&b, Literal *ln);
  void Translate_(BC::BasicBlock *&b, LiteralAddr *lan);
  void Translate_(BC::BasicBlock *&b, MethodBody *mbn);
  void Translate_(BC::BasicBlock *&b, Transfer *tn);
  void Translate_(BC::BasicBlock *&b, Unary *un);
  void Translate_(BC::BasicBlock *&b, VarDecl *vd, bool fullexpr);
  void Translate_(BC::BasicBlock *&b, While *wn);

  // Translate list of statements (each of which is a full expression and may
  // have variable declarations).
  void TranslateBlock(BC::BasicBlock *&b, Node *stmts);

  // Translate a method body to bytecode.
  static void TranslateFunction(BC::BytecodeFunction *bf,
                                FormalArguments *formals,
                                void *target,
                                Node *e);

  // Emit code to push the address of a variable onto the stack.
  void EmitVarAddr(BC::BasicBlock *b, Var *ve);

  // Helper for translating MethodBody nodes.
  void GenerateArgumentLoad(BC::BasicBlock *&b, Type t);
  void GenerateArgumentPrologs(BC::BasicBlock *&b, FormalArguments *formals,
                               Argument *&rv);

  // Allocate space for a local variable or temporary.  The offset into the
  // stack frame for the allocated space, suitably aligned, is returned.
  unsigned GrowFrame(unsigned align, size_t amt) {
    verify(align == 0 || align == 1 || align == 2 || align == 4 || align == 8);
    if (align == 0) {
      if (amt <= 1)
        align = 1;
      else if (amt <= 2)
        align = 2;
      else if (amt <= 4)
        align = 4;
      else
        align = 8;
    }

    m_frameSize = (m_frameSize + align - 1) & ~(align - 1);
    unsigned newOffset = m_frameSize;

    m_frameSize += unsigned(amt);
    if (m_frameSize > m_maxFrameSize)
      m_maxFrameSize = m_frameSize;

    return newOffset;
  }

  // Release space used by a nested lexical scope that has ended.
  void ResetFrame(unsigned oldSize) {
    verify(oldSize <= m_frameSize);
    m_frameSize = oldSize;
  }

  // Unwind the stack, destructing locals and executing regardless clauses,
  // until the specified mark is reached.
  void UnwindStack(BC::BasicBlock *&b, int mark, bool inException);

  // Chain of every basic block created by this state.
  BC::BasicBlock     *m_chain         = nullptr;

  // Current and maximum stack frame size for holding local variables and
  // temporaries.
  unsigned            m_frameSize     = 0;
  unsigned            m_maxFrameSize  = 0;

  // Target location to hold value of object-valued expression.  Yields an
  // l-value.
  struct TargetTracker {
    ToBytecode       *m_parent;
    Node             *m_target;
    TargetTracker    *m_prev;

    TargetTracker(ToBytecode *tobc, Node *newTarget)
        : m_parent(tobc), m_target(newTarget), m_prev(tobc->m_targetTracker) {
      tobc->m_targetTracker = this;
    }
    ~TargetTracker() { m_parent->m_targetTracker = m_prev; }

    // If addr is null, translate current target instead.
    void Translate(BC::BasicBlock *&b, Node *addr, bool keepfat = false) {
      if (addr) {
        m_parent->Translate(b, addr);
        return;
      }

      // We have to temporarily pop the current target, in case the address
      // refers to a target as well, which will be the previous one.  This
      // happens when constructing an array, for example.
      m_parent->m_targetTracker = m_prev;
      m_parent->Translate(b, m_target);
      Type t = m_target->Kind() == nk_Deref
                   ? safe_cast<Deref *>(m_target)->m_expr->m_type
                   : m_target->m_type;
      if (!keepfat && !t.IsThin()) {
        if (b)
          b->Append(BC::op_pop);
      }
      m_parent->m_targetTracker = this;
    }
  };

  TargetTracker      *m_targetTracker = nullptr;

  // Formal argument list description.
  FormalArguments    *m_formals       = nullptr;

  // Map of VarDecls to their corresponding offsets against either the frame or
  // arguments base.
  enum VarBase: uint8_t { vb_unknown, vb_frame, vb_args };

  struct VarInfo {
    unsigned          m_offset      = 0;
    VarBase           m_base        = vb_unknown;
    bool              m_autoDeref   = false;
  };

  std::map<Var *, VarInfo> m_varMap;

  // Stack of nodes requiring action when they go out of scope, including
  // VarDecls and nodes related to exception handling.
  vector<VarDecl *> m_unwindStack;

  // Map of Label nodes to their reachable Transfer nodes.  Also mark the first
  // VarDecl to be outside the scope of this label.  This may differ for exit
  // and next transfers, hence inner and outer marks.
  struct TransferInfo {
    TransferInfo(BC::BasicBlock *b, TransferKind tk, bool hv)
        : m_fromBlock(b), m_kind(tk), m_hasValue(hv) { }

    BC::BasicBlock   *m_fromBlock;
    TransferKind      m_kind;
    bool              m_hasValue;
  };

  struct LabelInfo {
    vector<TransferInfo> m_transfers;
    int               m_innerMark   = -1;
    int               m_outerMark   = -1;
  };

  std::map<Label *, LabelInfo> m_labelToTransfersMap;

  // Map of node to its associated LabelInfo.
  Node               *m_nodeWithLabel     = nullptr;
  LabelInfo          *m_labelInfoForNode  = nullptr;

  // The LexicalScope class handles common bookkeeping with regards to entering
  // and exiting lexical scopes, namely tracking transfers to this scope and
  // what needs destructing as a result and freeing up allocated stack frame
  // space.
  class LexicalScope {
  public:
    LexicalScope(ToBytecode *self, Node *node)
        : m_self(self),
          m_savedFrameSize(self->GrowFrame(0, 0)) {
      m_labelInfo = self->m_nodeWithLabel == node ? self->m_labelInfoForNode
                                                  : &m_dummyLabelInfo;
      verify(m_labelInfo->m_innerMark == -1 && m_labelInfo->m_outerMark == -1);
      m_labelInfo->m_innerMark = int(m_self->m_unwindStack.size());
      m_labelInfo->m_outerMark = m_labelInfo->m_innerMark;
    }

    ~LexicalScope() {
      verify(int(m_self->m_unwindStack.size()) == m_labelInfo->m_outerMark);
      m_labelInfo->m_innerMark = -1;
      m_labelInfo->m_outerMark = -1;
      m_self->ResetFrame(m_savedFrameSize);
    }

    void push_back(BC::BasicBlock *b, TransferKind tk, bool hv) {
      m_labelInfo->m_transfers.push_back(TransferInfo(b, tk, hv));
    }

    size_t size() { return m_labelInfo->m_transfers.size(); }
    TransferInfo &operator[](size_t i) { return m_labelInfo->m_transfers[i]; }

  private:
    ToBytecode       *m_self;
    LabelInfo         m_dummyLabelInfo;
    LabelInfo        *m_labelInfo         = nullptr;
    unsigned          m_savedFrameSize;
  };
};

Value *BytecodeTarget::Evaluate(TranslateClosure &tc, Value *result,
                                Expr *expr) {
  // See if anything prevents execution.
  if (tc.HasUnbound())
    return Value::New(Type::Unbound());
  else if (tc.HasError())
    return Value::New(Type::Suppress());

  Node *e = expr->Root();
  if (e->m_type != rt_rvalue)
    e = e->AddrOf(true);

  size_t size = result->ObjectType().StorageSize();
  char *data = reinterpret_cast<char *>(result->Address());

  // Translate expression to bytecode.  The expression is wrapped in a
  // function.  This function has no arguments.  Any entities it refers to
  // must exist at compile time.
  BC::BytecodeFunction bf;
  ToBytecode::TranslateFunction(&bf, nullptr, data, e);
  bf.Execute(data, size);

  return result;
}

void ToBytecode::TranslateFullExpr(BC::BasicBlock *&b, Node *e) {
  int mark = int(m_unwindStack.size());

  Translate(b, e);

  if (b)
    UnwindStack(b, mark, false);
  m_unwindStack.resize(mark);
}

void ToBytecode::Translate(BC::BasicBlock *&b, Node *e) {
  switch (e->Kind()) {
    case nk_Attributes:
      Translate(b, safe_cast<Attributes *>(e)->m_expr);
      break;

    case nk_Binary:
      Translate_(b, safe_cast<Binary *>(e));
      break;

    case nk_Block:
      Translate_(b, safe_cast<Block *>(e));
      break;

    case nk_BuildPointer: {
      auto bpn = safe_cast<BuildPointer *>(e);
      Translate(b, bpn->m_pointer);
      if (b)
        Translate(b, bpn->m_descriptor);
      break;
    }

    case nk_Call:
      Translate_(b, safe_cast<Call *>(e));
      break;

    case nk_CallBuiltin:
      Translate_(b, safe_cast<CallBuiltin *>(e));
      break;

    case nk_Cast:
      Translate_(b, safe_cast<Cast *>(e));
      break;

    case nk_Construct:
      Translate_(b, safe_cast<Construct *>(e));
      break;

    case nk_Deref:
      Translate(b, safe_cast<Deref *>(e)->m_expr);
      break;

    case nk_ExtractAddress: {
      auto ean = safe_cast<ExtractAddress *>(e);
      Translate(b, ean->m_pointer);
      if (b)
        b->Append(BC::op_pop);
      break;
    }

    case nk_ExtractDescriptor: {
      auto edn = safe_cast<ExtractDescriptor *>(e);
      m_targetTracker->Translate(b, edn->m_pointer, true);
      if (b) {
        b->Append(BC::op_swap);
        b->Append(BC::op_pop);
      }
      break;
    }

    case nk_FieldAddr: {
      auto fan = safe_cast<FieldAddr *>(e);
      m_targetTracker->Translate(b, fan->m_address);
      if (b) {
        Node *addr = fan->m_address ? static_cast<Node *>(fan->m_address)
                                    : m_targetTracker->m_target;
        size_t offset = addr->m_type.BaseType().OffsetOf(fan->m_ord);
        b->AppendLea(BC::op_leaptr8, unsigned(offset));
      }
      break;
    }

    case nk_GlobalAddr: {
      auto gan = safe_cast<GlobalAddr *>(e);
      b->AppendPush(gan->m_entity->GlobalStorage(ep_compile));
      break;
    }

    case nk_If:
      Translate_(b, safe_cast<If *>(e));
      break;

    case nk_Index: {
      auto in = safe_cast<Index *>(e);
      m_targetTracker->Translate(b, in->m_address);
      if (b) {
        Translate(b, in->m_index);
        if (b) {
          b->AppendPush((unsigned)in->m_type.BaseType().StorageSize());
          b->Append(BC::op_muls32); // FIXME: make 64-bit friendly
          b->Append(BC::op_add32);
        }
      }
      break;
    }

    case nk_Label: {
      auto ln = safe_cast<Label *>(e);

      // Let the statement immediately under us find the label info.
      m_nodeWithLabel = ln->m_expr;
      m_labelInfoForNode = &m_labelToTransfersMap[ln];

      // Compound statements handle the label themselves.
      Translate(b, ln->m_expr);
      m_nodeWithLabel = nullptr;
      m_labelInfoForNode = nullptr;

      // FIXME: handle transfers from nested methods somehow.
      break;
    }

    case nk_Literal:
      Translate_(b, safe_cast<Literal *>(e));
      break;

    case nk_LiteralAddr:
      Translate_(b, safe_cast<LiteralAddr *>(e));
      break;

    case nk_Load: {
      auto ln = safe_cast<Load *>(e);
      Translate(b, ln->m_address);
      if (b)
        b->AppendLoad(ln->m_type);
      break;
    }

    case nk_MethodBody:
      Translate_(b, safe_cast<MethodBody *>(e));
      break;

    case nk_Sequence: {
      auto sn = safe_cast<Sequence *>(e);
      Translate(b, sn->m_first);
      if (b) {
        if (sn->m_valueIs != Sequence::vi_first &&
            sn->m_first->m_type.IsSimpleNonUnit())
          b->Append(BC::op_pop);
        Translate(b, sn->m_second);
        if (b) {
          if (sn->m_valueIs != Sequence::vi_second &&
              sn->m_second->m_type.IsSimpleNonUnit())
            b->Append(BC::op_pop);
        }
      }
      break;
    }

    case nk_Shared:
      Translate(b, safe_cast<Shared *>(e)->m_expr->Root());
      break;

    case nk_Store: {
      auto sn = safe_cast<Store *>(e);
      Translate(b, sn->m_address);
      if (b) {
        Translate(b, sn->m_operand);
        if (b)
          b->AppendStore(sn->m_operand->m_type);
      }
      break;
    }

    case nk_Transfer:
      Translate_(b, safe_cast<Transfer *>(e));
      break;

    case nk_Unary:
      Translate_(b, safe_cast<Unary *>(e));
      break;

    case nk_VarAddr:
      EmitVarAddr(b, safe_cast<VarAddr *>(e)->m_var);
      break;

    case nk_VarDecl:
      Translate_(b, safe_cast<VarDecl *>(e), false);
      break;

    case nk_Vtable: {
      auto vn = safe_cast<Vtable *>(e);
      b->AppendPush(vn->m_class->Vtable());
      break;
    }

    case nk_VtableCast: {
      auto vcn = safe_cast<VtableCast *>(e);
      BaseSpecifier *bs = vcn->m_class->GetBase(vcn->m_ord);
      Translate(b, vcn->m_vtbl);
      if (bs->m_vtblOffset > 0) {
        if (vcn->m_upcast) {
          b->AppendLea(BC::op_leaptr8, unsigned(bs->m_vtblOffset * 4));
        } else {
          if constexpr (sizeof(void *) == 4) {
            b->AppendPush(static_cast<unsigned>(bs->m_vtblOffset * 4));
            b->Append(BC::op_sub32);
          } else {
            b->AppendPush(static_cast<uint64_t>(bs->m_vtblOffset*8));
            b->Append(BC::op_sub64);
          }
        }
      }
      break;
    }

    case nk_VtableSlot: {
      auto vsn = safe_cast<VtableSlot *>(e);
      Translate(b, vsn->m_vtable);
      if (b) {
        if (vsn->m_slot > 0) {
          if constexpr (sizeof(void *) == 4) {
            b->AppendPush(static_cast<unsigned>(vsn->m_slot * 4));
            b->Append(BC::op_add32);
          } else {
            b->AppendPush(static_cast<uint64_t>(vsn->m_slot * 8));
            b->Append(BC::op_add64);
          }
        }

        b->AppendLoad(vsn->m_type);
      }
      break;
    }

    case nk_While:
      Translate_(b, safe_cast<While *>(e));
      break;

    default:
      verify(0 && "Expr node cannot be translated to bytecode.");
      break;
  }
}

void ToBytecode::Translate_(BC::BasicBlock *&b, Literal *ln) {
  // FIXME: provide for a larger variety of types.
  Value *v = ln->m_value;
  Type t = v->ObjectType();
  if (t == Type::SByte()) {
    b->AppendPush(v->As<int8_t>());
  } else if (t == Type::Short()) {
    b->AppendPush(v->As<int16_t>());
  } else if (t == Type::Int()) {
    b->AppendPush(v->As<int32_t>());
  } else if (t == Type::Long()) {
    b->AppendPush(v->As<int64_t>());
  } else if (t == Type::Address()) {
    b->AppendPush(v->As<void *>());
  } else if (t == Type::Float()) {
    b->AppendPush(v->As<float>());
  } else if (t == Type::Double()) {
    b->AppendPush(v->As<double>());
  } else {
    verify(t.IsSimpleType());
    switch (t.StorageSize()) {
      case 0: return;
      case 1: b->AppendPush(v->As<uint8_t>());  break;
      case 2: b->AppendPush(v->As<uint16_t>()); break;
      case 4: b->AppendPush(v->As<uint32_t>()); break;
      case 8: b->AppendPush(v->As<uint64_t>()); break;
      default: verify(false);
    }
  }
}

void ToBytecode::Translate_(BC::BasicBlock *&b, LiteralAddr *lan) {
  b->AppendPush(lan->m_data->Address());
}

void ToBytecode::Translate_(BC::BasicBlock *&b, Unary *un) {
  // Translate our operand to bytecode.  Propagate unreachability immediately.
  Translate(b, un->m_operand);
  if (!b)
    return;

  bool is64 = un->m_type.StorageSize() == 8;

  // Now that the operands are out of the way, go translate the unary
  // operation.
  switch (un->m_opcode) {
    case Unary::op_not:
      verify(un->m_type != tk_float);
      b->Append(is64 ? BC::op_not64 : BC::op_not32);
      break;
    case Unary::op_neg:
      if (un->m_type == tk_float)
        b->Append(is64 ? BC::op_negf64 : BC::op_negf32);
      else
        b->Append(is64 ? BC::op_neg64 : BC::op_neg32);
      break;
    default:
      verify(false);
  }
}

void ToBytecode::Translate_(BC::BasicBlock *&b, Binary *bn) {
  verify(bn->m_opcode != Binary::op_illegal);

  // Translate our operands to bytecode.  Propagate unreachability immediately.
  Translate(b, bn->m_operand1);
  if (!b)
    return;

  Translate(b, bn->m_operand2);
  if (!b)
    return;

  // Now that the operands are out of the way, go translate the binary
  // operation.
  using namespace BC;
  if (bn->m_type == tk_float) {
    static const Opcode opcodes[][2] = {
      /* op_add   */ { op_addf32,         op_addf64         },
      /* op_sub   */ { op_subf32,         op_subf64         },
      /* op_mul   */ { op_mulf32,         op_mulf64         },
      /* op_div   */ { op_divf32,         op_divf64         },
      /* op_rem   */ { op_end_of_opcodes, op_end_of_opcodes },
      /* op_and   */ { op_end_of_opcodes, op_end_of_opcodes },
      /* op_or    */ { op_end_of_opcodes, op_end_of_opcodes },
      /* op_xor   */ { op_end_of_opcodes, op_end_of_opcodes },
      /* op_shl   */ { op_end_of_opcodes, op_end_of_opcodes },
      /* op_shr   */ { op_end_of_opcodes, op_end_of_opcodes },
      /* op_seteq */ { op_seteqf32,       op_seteqf64       },
      /* op_setne */ { op_setnef32,       op_setnef64       },
      /* op_setlt */ { op_setltf32,       op_setltf64       },
      /* op_setle */ { op_setlef32,       op_setlef64       },
      /* op_setgt */ { op_setgtf32,       op_setgtf64       },
      /* op_setge */ { op_setgef32,       op_setgef64       }
    };

    static_assert(sizeof(Opcode) * Binary::op_illegal * 2 == sizeof(opcodes));

    Opcode op = opcodes[bn->m_opcode][bn->m_type == Type::Double()];
    verify(op != op_end_of_opcodes);
    b->Append(op);
  } else {
    static const Opcode opcodes[][4] = {
      /* op_add   */ { op_add32,    op_add64,    op_add32,    op_add64    },
      /* op_sub   */ { op_sub32,    op_sub64,    op_sub32,    op_sub64    },
      /* op_mul   */ { op_mulu32,   op_mulu64,   op_muls32,   op_muls64   },
      /* op_div   */ { op_divu32,   op_divu64,   op_divs32,   op_divs64   },
      /* op_rem   */ { op_remu32,   op_remu64,   op_rems32,   op_rems64   },
      /* op_and   */ { op_and32,    op_and64,    op_and32,    op_and64    },
      /* op_or    */ { op_or32,     op_or64,     op_or32,     op_or64     },
      /* op_xor   */ { op_xor32,    op_xor64,    op_xor32,    op_xor64    },
      /* op_shl   */ { op_shl32,    op_shl64,    op_shl32,    op_shl64    },
      /* op_shr   */ { op_shru32,   op_shru64,   op_shrs32,   op_shrs64   },
      /* op_seteq */ { op_seteq32,  op_seteq64,  op_seteq32,  op_seteq64  },
      /* op_setne */ { op_setne32,  op_setne64,  op_setne32,  op_setne64  },
      /* op_setlt */ { op_setltu32, op_setltu64, op_setlts32, op_setlts64 },
      /* op_setle */ { op_setleu32, op_setleu64, op_setles32, op_setles64 },
      /* op_setgt */ { op_setgtu32, op_setgtu64, op_setgts32, op_setgts64 },
      /* op_setge */ { op_setgeu32, op_setgeu64, op_setges32, op_setges64 }
    };

    static_assert(sizeof(Opcode) * Binary::op_illegal * 4 == sizeof(opcodes));

    int t = (bn->m_operand1->m_type.StorageSize() == 8) +
      2 * bn->m_operand1->m_type.IsSigned();

    b->Append(opcodes[bn->m_opcode][t]);
  }
}

void ToBytecode::Translate_(BC::BasicBlock *&b, Block *bn) {
  // FIXME: handle nested methods and classes.

  // Simple case of no label, and thus no exits.
  if (!bn->m_label)
    return TranslateBlock(b, bn->m_expr);

  LexicalScope ls(this, bn);

  BC::BasicBlock *exit = NewBlock();
  bool hasValue = bn->m_type.IsSimpleNonUnit();
  bool fallsThrough = false;

  // In order to keep track of stack depth on entry and exit, we need an
  // entry block.
  BC::BasicBlock *entry = b;
  b = NewBlock();
  entry->Append(BC::op_jump32);
  entry->AppendBlock(b);

  // Now translate the body of the block.
  Translate(b, bn->m_expr);
  if (b) {
    if (bn->m_expr->m_type.IsSimpleNonUnit() && !hasValue)
      b->Append(BC::op_pop);
    b->Append(BC::op_jump32);
    b->AppendBlock(exit);
    fallsThrough = true;
  }

  // Now we have to merge the flow of control.  We need yet another block to
  // which all the exits can branch and return as the new value of b.  But
  // only if there are any exits!
  if (ls.size() == 0 && !fallsThrough) {
    // Flow of control never reaches the end of the block and there are no
    // exits.
    b = nullptr;
    return;
  }

  b = exit;
  b->SetMerge(entry, 0, hasValue);
  for (size_t i = 0; i < ls.size(); i++) {
    BC::BasicBlock *from = ls[i].m_fromBlock;
    // FIXME: now that all paths are cast to the same type, there shouldn't
    // be a need for m_hasValue, or most of the tests for simple types.
    if (ls[i].m_hasValue && !hasValue)
      from->Append(BC::op_pop);
    from->Append(BC::op_exit);
    from->AppendBlock(b);
  }
}

void ToBytecode::Translate_(BC::BasicBlock *&b, Call *cn) {
  // Translate our operands to bytecode.  Propagate unreachability immediately.
  vector<Call::Slot> operands;
  Call::Slot rv;
  cn->GetSlotOrdering(operands, rv);
  if (rv.m_arg) {
    Translate(b, rv.m_arg);
    if (!b)
      return;
  }

  Translate(b, cn->m_function);
  if (!b)
    return;

  unsigned stackdelta = 0;
  for (size_t i = 0; i < operands.size(); i++) {
    Node *e = operands[i].m_arg;
    if (e) {
      Translate(b, e);
      Type t = e->m_type;
      if (t != tk_void)
        stackdelta += 1 + (t == rt_rvalue && t == tk_pointer && !t.IsThin());
    } else {
      stackdelta++;
      m_targetTracker->Translate(b, nullptr);
    }
    if (!b)
      return;
  }

  // FIXME: if our context needs to catch exceptions, use an invoke instruction
  // instead and generate the necessary exception handling.

  if (!cn->m_type.IsSimpleNonUnit() && !rv.m_arg) {
    b->Append(BC::op_call);
  } else {
    Type t = rv.m_arg ? rv.m_arg->m_type : cn->m_type;
    if (t == rt_rvalue && t == tk_pointer && !t.IsThin())
      b->Append(BC::op_callfat);
    else
      b->Append(BC::op_callval);
  }
  b->AppendByte(stackdelta);
  if (rv.m_arg)
    b->AppendStore(rv.m_arg->m_type);
}

void ToBytecode::Translate_(BC::BasicBlock *&b, CallBuiltin *cbn) {
  b->AppendPush(reinterpret_cast<void *>(cbn->m_function));

  // Translate our operands to bytecode.  Propagate unreachability immediately.
  unsigned stackdelta = 0;
  for (size_t i = 0; i < cbn->m_operands.size(); i++) {
    Node *e = cbn->m_operands[i];
    Type t = e->m_type;

    Translate(b, e);
    if (!b)
      return;

    stackdelta++;
    if (t == tk_void)
      b->AppendPush((int8_t)0);
    else if (t == rt_rvalue && t == tk_pointer && !t.IsThin())
      stackdelta++;
  }

  if (!cbn->m_type.IsSimpleNonUnit())
    b->Append(BC::op_call);
  else
    b->Append(BC::op_callval);
  b->AppendByte(stackdelta);
}

void ToBytecode::Translate_(BC::BasicBlock *&b, Cast *cn) {
  // Translate our operand to bytecode.  Propagate unreachability immediately.
  Translate(b, cn->m_operand);
  if (!b)
    return;

  if (cn->m_type == cn->m_operand->m_type)
    return;

  size_t ts = cn->m_type.StorageSize();
  size_t ss = cn->m_operand->m_type.StorageSize();

  // FIXME: conversions to and from unsigned ints?

  if (cn->m_operand->m_type == tk_float) {
     if (cn->m_type == tk_float) {
       if (ts > ss)
         b->Append(BC::op_convf32tof64);
       else if (ts < ss)
         b->Append(BC::op_convf64tof32);
     } else {
       if (ts > ss)
         b->Append(BC::op_convf32toi64);
       else if (ts < ss)
         b->Append(BC::op_convf64toi32);
       else if (ts == 8)
         b->Append(BC::op_convf64toi64);
       else
         b->Append(BC::op_convf32toi32);
     }
     return;
  } else if (cn->m_type == tk_float) {
     if (ts > ss)
       b->Append(BC::op_convi32tof64);
     else if (ts < ss)
       b->Append(BC::op_convi64tof32);
     else if (ts == 8)
       b->Append(BC::op_convi64tof64);
     else
       b->Append(BC::op_convi32tof32);
    return;
  }

  if (ts > ss) {
    // Extend the source until it is at least the same size as the target,
    // according to the signedness of the source.  As all values on the stack
    // are already extended to at least 32 bits, we only have to worry about
    // 64-bit targets.
    if (ts == 8)
      b->Append(cn->m_operand->m_type.IsSigned() ? BC::op_convs32to64
                                                 : BC::op_convu32to64);
  }

  if (ss == 8 && ts <= 4)
    b->Append(BC::op_conv64to32);

  if (ts < 4) {
    b->Append(cn->m_type.IsSigned() ? BC::op_exts32 : BC::op_extz32);
    b->AppendByte(unsigned(ts) * 8);
  }
}

static int ComputeArgumentOffset(int slot, Type t) {
  int offset = slot * sizeof(BC::Union);

  if (!IsLittleEndian()) {
    switch (t.StorageSize()) {
      case 1: offset += 3;  break;
      case 2: offset += 2;  break;
      default:              break;
    }
  }
  return offset;
}

void ToBytecode::GenerateArgumentLoad(BC::BasicBlock *&b, Type t) {
  // Ugly hack:  for 32-bit builds, fat pointer arguments must be specially
  // handled because the two halves are not consecutive on the caller's
  // stack.
  if (sizeof(void *) == 4 && t == tk_pointer && !t.IsThin()) {
    b->Append(BC::op_dup);
    b->AppendLoad(Type::Int());
    b->Append(BC::op_swap);
    b->AppendPush((unsigned)sizeof(BC::Union));
    b->Append(BC::op_add32);
    b->AppendLoad(Type::Int());
    return;
  }

  b->AppendLoad(t);
}

void ToBytecode::GenerateArgumentPrologs(BC::BasicBlock *&b,
                                         FormalArguments *formals,
                                         Argument *&rv) {
  // Slots may take either one or two entries on the stack.  Enumerate the
  // formals to build a map from slot to stack offset.
  vector<int> offset(formals->m_arguments.size() + 2);
  for (size_t i = 0; i < formals->m_arguments.size(); i++) {
    Argument *ae = formals->m_arguments[i];
    if (ae->m_slot >= 0) {
      Type t = ae->m_type.Lower();
      if (t == tk_void)
        offset[ae->m_slot] = 0;
      else
        offset[ae->m_slot] = 1 + (t == tk_pointer && !t.IsThin());
    }
  }

  if (Argument *ae = formals->m_returnValue) {
    if (ae->m_slot >= 0) {
      Type t = ae->m_type.Lower();
      if (t == tk_void)
        offset[ae->m_slot] = 0;
      else
        offset[ae->m_slot] = 1 + (t == tk_pointer && !t.IsThin());
    }
  }

  int sum = 0;
  for (size_t i = 0; i < offset.size(); i++) {
    int newsum = sum + offset[i];
    offset[i] = sum;
    sum = newsum;
  }

  for (size_t i = 0; i < formals->m_arguments.size(); i++) {
    Argument *ae = formals->m_arguments[i];
    int slot = ae->m_slot;

    // Add a VarDecl mapping to a location right now.  When the argument is
    // passed-by-value, this necessitates the creation of a variable for two
    // reasons.  First, even am_in arguments are mutable, just as in C/C++.
    // Second, even if they weren't, when used as the receiver of a method
    // call, a reference to the receiver is passed to the method.
    VarInfo &vi = m_varMap[ae];
    verify(vi.m_base == vb_unknown);
    verify(ae->m_mechanism == am_in);

    // If the argument is a simple value, create a local variable and
    // initialize it to the argument, and use the variable from this point
    // on; otherwise, we already have a pointer to storage we're free to
    // use as we please.
    Type t = ae->m_type.Lower();
    unsigned argoffset = ComputeArgumentOffset(offset[slot], t);
    if (t.IsSimpleType()) {
      vi.m_base = vb_frame;
      vi.m_offset = GrowFrame(0, t.StorageSize());
      if (t != tk_void) {
        b->AppendLea(BC::op_leaframe8, vi.m_offset);
        b->AppendLea(BC::op_leaargs8, argoffset);
        GenerateArgumentLoad(b, t);
        b->AppendStore(t);
      }
    } else {
      vi.m_base = vb_args;
      vi.m_offset = argoffset;
      vi.m_autoDeref = true;
    }
  }

  if (Argument *ae = formals->m_returnValue) {
    int slot = ae->m_slot;
    verify(ae->m_mechanism != am_in);

    if (ae->m_isReturned) {
      // This argument is returned as the value of the function.
      rv = ae;
    } else {
      // Storage for the argument has been passed to us.  Use it as the
      // address of the variable.
      VarInfo &vi = m_varMap[ae];
      verify(vi.m_base == vb_unknown);
      vi.m_base = vb_args;
      vi.m_offset = ComputeArgumentOffset(offset[slot], ae->m_type.Lower());
      vi.m_autoDeref = true;
    }
  }
}

void ToBytecode::Translate_(BC::BasicBlock *&b, MethodBody *mbn) {
  // Walk the formal argument list, priming the VarDecl map for the
  // arguments, and emitting whatever setup code is needed given the
  // circumstances for each argument.
  Argument *rv = nullptr; // Argument that's returned as function value
  GenerateArgumentPrologs(b, m_formals, rv);

  LexicalScope ls(this, mbn);
  TargetTracker tt(this, nullptr);

  // For object-valued method bodies, setup the target.
  if (m_formals->m_returnValue && mbn->m_type == rt_rvalue) {
    Argument *ae = m_formals->m_returnValue;
    if (!ae->m_isReturned)
      tt.m_target = (new VarAddr({ }, ae))->Deref();
  }

  // Translate the body to bytecode.
  Translate(b, mbn->m_body);

  if (rv) {
    BC::Opcode retOp =
      rv->m_type == tk_pointer && !rv->m_type.IsThin()
                    ? BC::op_retfat : BC::op_retval;
    // When the return type is simple, it's at the top of the stack; otherwise,
    // an address at which to construct the return value was passed an a hidden
    // argument and there's nothing to do here.
    for (size_t i = 0; i < ls.size(); i++) {
      verify(ls[i].m_hasValue);
      ls[i].m_fromBlock->Append(retOp);
    }
  } else {
    for (size_t i = 0; i < ls.size(); i++)
      ls[i].m_fromBlock->Append(BC::op_ret);
    if (b)
      b->Append(BC::op_ret);
  }

  // Naturally, control doesn't flow from here because we returned.
  b = nullptr;
}

void ToBytecode::Translate_(BC::BasicBlock *&b, While *wn) {
  LexicalScope ls(this, wn);

  // The layout of blocks generated for while loops is as follows:
  //
  //   header: jump test
  //
  //   test:   <condition>
  //           if body,else
  //
  //   body:   <body>
  //           jump next
  //
  //   next:   <next>
  //           jump test
  //
  //   else:   <else>
  //           jump exit
  //
  //   exit:
  //
  // The <next> block only exists for lowered For nodes.  For normal while
  // loops, <body> jumps directly to <test>.
  //
  // Note: unlike other nodes, we don't do a very good job of detecting
  // unreachable code here.  There are just too many permutations to look
  // for.  Anyway, it isn't really needed as unreachable basic blocks can
  // be detected and removed later; it's just nice to save it the effort
  // where possible (and save ourselves the trouble of translating it).

  // Miscellaneous.
  BC::BasicBlock *els2 = nullptr;
  bool hasValue = wn->m_type.IsSimpleNonUnit();

  // Insert an empty loop pre-header block so that stack depths can be correctly
  // determined for next transfers.
  BC::BasicBlock *header = NewBlock();
  b->Append(BC::op_jump32);
  b->AppendBlock(header);

  // Translate the condition to bytecode.
  BC::BasicBlock *test = NewBlock();
  BC::BasicBlock *test2 = test;
  TranslateFullExpr(test2, wn->m_condition);

  // Translate the body to bytecode.
  BC::BasicBlock *body = NewBlock();
  BC::BasicBlock *body2 = body;
  Translate(body2, wn->m_body);
  if (body2) {
    if (wn->m_body->m_type.IsSimpleNonUnit())
      body2->Append(BC::op_pop);
  }

  // Translate the iterator successor for lowered for loops.
  BC::BasicBlock *next = test;
  if (wn->m_next) {
    next = NewBlock();
    if (body2) {
      body2->Append(BC::op_jump32);
      body2->AppendBlock(next);
    }

    body2 = next;
    Translate(body2, wn->m_next);
    if (wn->m_next->m_type.IsSimpleNonUnit())
      body2->Append(BC::op_pop);

    next->SetMerge(header, 0, false);
  }

  if (body2) {
    body2->Append(BC::op_jump32);
    body2->AppendBlock(test);
  }

  // Create exit block.
  BC::BasicBlock *exit = NewBlock();

  // Finish up.
  test->SetMerge(header, 0, false);
  exit->SetMerge(header, 0, hasValue);
  BC::BasicBlock *els = NewBlock();
  els2 = els;
  Translate(els2, wn->m_else);
  if (els2) {
    if (wn->m_else->m_type.IsSimpleNonUnit() && !hasValue)
      els2->Append(BC::op_pop);
    els2->Append(BC::op_jump32);
    els2->AppendBlock(exit);
    ls.push_back(els2, tk_exit, hasValue);
  }
  if (test2) {
    test2->Append(BC::op_if);
    test2->AppendBlock(body);
    test2->AppendBlock(els);
  }
  header->Append(BC::op_jump32);
  header->AppendBlock(test);

  // Figure out if anything ever reaches the exit block.  While we're doing
  // that, append branches to the exit and next transfers.
  bool exitReached = false;
  for (size_t i = 0; i < ls.size(); i++) {
    BC::BasicBlock *from = ls[i].m_fromBlock;
    if (ls[i].m_kind == tk_exit) {
      // Block test2 or els2 already had a branch appended.
      if (from != test2 && from != els2) {
        if (ls[i].m_hasValue && !hasValue)
          from->Append(BC::op_pop);
        from->Append(BC::op_exit);
        from->AppendBlock(exit);
      }
      exitReached = true;
    } else {
      from->Append(BC::op_exit);
      from->AppendBlock(next);
    }
  }

  // If nothing reaches the exit, say so to our parent.
  b = exitReached ? exit : nullptr;
}

void ToBytecode::Translate_(BC::BasicBlock *&b, If *in) {
  LexicalScope ls(this, in);

  BC::BasicBlock *exit = NewBlock();
  BC::BasicBlock *cond = NewBlock();
  bool hasValue = in->m_type.IsSimpleNonUnit();
  bool fallsThrough = false;

  // Translate the condition to bytecode.
  b->Append(BC::op_jump32);
  b->AppendBlock(cond);
  b = cond;
  TranslateFullExpr(b, in->m_condition);

  if (b) {
    // Create a new basic block for each of the two possible paths, and then
    // insert a conditional branch from the previous block to these new blocks.
    BC::BasicBlock *tb = NewBlock();
    BC::BasicBlock *fb = NewBlock();
    b->Append(BC::op_if);
    b->AppendBlock(tb);
    b->AppendBlock(fb);

    // Now translate the true and false branches.
    Translate(tb, in->m_ifTrue);
    if (tb) {
      if (in->m_ifTrue->m_type.IsSimpleNonUnit() && !hasValue)
        tb->Append(BC::op_pop);
      tb->Append(BC::op_jump32);
      tb->AppendBlock(exit);
      fallsThrough = true;
    }

    Translate(fb, in->m_ifFalse);
    if (fb) {
      if (in->m_ifFalse->m_type.IsSimpleNonUnit() && !hasValue)
        fb->Append(BC::op_pop);
      fb->Append(BC::op_jump32);
      fb->AppendBlock(exit);
      fallsThrough = true;
    }
  }

  // Now we have to merge the flow of control.  We need yet another block to
  // which all the exits can branch and return as the new value of b.  But
  // only if there are any exits!
  if (ls.size() == 0 && !fallsThrough) {
    // Flow of control never reaches the end of the if statement.
    b = nullptr;
    return;
  }

  b = exit;
  b->SetMerge(cond, 0, hasValue);
  for (size_t i = 0; i < ls.size(); i++) {
    BC::BasicBlock *from = ls[i].m_fromBlock;
    // FIXME: now that all paths are cast to the same type, there shouldn't
    // be a need for m_hasValue, or most of the tests for simple types.
    if (ls[i].m_hasValue && !hasValue)
      from->Append(BC::op_pop);
    from->Append(BC::op_exit);
    from->AppendBlock(b);
  }
}

void ToBytecode::Translate_(BC::BasicBlock *&b, Transfer *tn) {
  // Do guard if present; if not present, any following code is unreachable.
  BC::BasicBlock *final = nullptr;
  if (tn->m_guard) {
    // Translate guard expression.
    Translate(b, tn->m_guard);
    if (!b)
      return;

    // Branch based on outcome.
    final = NewBlock();
    BC::BasicBlock *b2 = NewBlock();
    b->Append(BC::op_if);
    b->AppendBlock(b2);
    b->AppendBlock(final);
    b = b2;
  }

  // Translate the expression.  We do not return the resulting value to our
  // parent, who always sees a void result, but save it away for the relevant
  // statement node to use.
  Translate(b, tn->m_expr);
  if (!b)
    return;

  // Unwind until the target lexical scope is reached.
  LabelInfo &li = m_labelToTransfersMap[tn->m_label];
  if (tn->m_kind == tk_next)
    UnwindStack(b, li.m_innerMark, false);
  else
    UnwindStack(b, li.m_outerMark, false);

  // Register us with our Label node.  Note that if any of the expressions we
  // just translated don't reach here, the registration doesn't take place and
  // it will be as if this node didn't exist.
  bool hasValue = tn->m_expr->m_type.IsSimpleNonUnit();
  li.m_transfers.push_back(TransferInfo(b, tn->m_kind, hasValue));

  // Set what the parent node sees.
  b = final;
  return;
}

void ToBytecode::Translate_(BC::BasicBlock *&b, VarDecl *vd, bool fullexpr) {
  if (vd->m_entity->Kind() == ek_Argument) {
    // Do nothing.
  } else if (vd->m_entity->Kind() == ek_Var) {
    auto ve = safe_cast<Var *>(vd->m_entity);
    Type storageType = vd->VariableType();
    if (ve->VarKind() == vk_local) {
      // Allocate storage for local variables.
      VarInfo &vi = m_varMap[ve];
      vi.m_base = vb_frame;
      vi.m_offset = GrowFrame(0, storageType.StorageSize());
    } else {
      verify(false);
    }
  }

  // Translate the initializer to bytecode.  Propagate unreachability
  // immediately.
  if (fullexpr)
    TranslateFullExpr(b, vd->m_initexpr);
  else
    Translate(b, vd->m_initexpr);
  if (!b)
    return;

  // Translate the expression in which the variable is in scope, which is also
  // the value of this node.
  using func_t = void (ToBytecode::*)(BC::BasicBlock *&, Node *);
  func_t recurse = fullexpr ? &ToBytecode::TranslateBlock
                            : &ToBytecode::Translate;
  if (vd->m_destructMode == dm_leaves_scope ||
      vd->m_destructMode == dm_not_on_return) {
    m_unwindStack.push_back(vd);
    (this->*recurse)(b, vd->m_expr);
    if (b)
      UnwindStack(b, int(m_unwindStack.size()) - 1, false);
    m_unwindStack.pop_back();
  } else if (vd->m_destructMode == dm_leaves_full_expr) {
    m_unwindStack.push_back(vd);
    (this->*recurse)(b, vd->m_expr);
  } else {
    (this->*recurse)(b, vd->m_expr);
  }
}

void ToBytecode::Translate_(BC::BasicBlock *&b, Construct *cn) {
  // The object has a non-simple type, otherwise this Construct wouldn't be
  // here.  Set it up as the place to put the object created by the constructor.
  TargetTracker tt(this, cn->m_addr);

  // Translate the constructor call.
  Translate(b, cn->m_init);
}

void ToBytecode::TranslateBlock(BC::BasicBlock *&b, Node *stmts) {
  while (b && stmts->Kind() == nk_Sequence) {
    auto sn = safe_cast<Sequence *>(stmts);
    stmts = sn->m_second;
    TranslateFullExpr(b, sn->m_first);
  }

  if (b) {
    if (stmts->Kind() == nk_VarDecl)
      Translate_(b, safe_cast<VarDecl *>(stmts), true);
    else
      TranslateFullExpr(b, stmts);
  }
}

void ToBytecode::TranslateFunction(BC::BytecodeFunction *bf,
                                   FormalArguments *formals,
                                   void *target,
                                   Node *e) {
  ToBytecode tobc;
  TargetTracker tt(&tobc, nullptr);
  tobc.m_formals = formals;

  // If there are no formals, but the function has a non-simple value, go
  // and provide a target for the value.
  verify(int(formals == 0) + int(target == 0) == 1);
  if (target) {
    if (e->m_type == tk_void) {
      bf->SetReturnsValue(false);
    } else if (!e->m_type.IsSimpleType()) {
      verify(e->m_type == rt_rvalue); // FIXME
      Type t = Type::ThinPointerTo(e->m_type);
      tt.m_target = (new Literal({ }, Value::NewPtr(t, target)))->Deref();
      bf->SetReturnsValue(false);
    }
  }

  // Create the initial entry block.
  BC::BasicBlock *entry = tobc.NewBlock();
  BC::BasicBlock *b = entry;

  // Visit the expression nodes, translating them to bytecode.
  tobc.Translate(b, e);

  // Finally, add a return instruction.
  if (b) {
    if (bf->ReturnsValue()) {
      Type t = formals ? formals->m_returnValue->m_type.Lower()
                       : e->m_type;
      if (t == tk_pointer && !t.IsThin())
        b->Append(BC::op_retfat);
      else
        b->Append(BC::op_retval);
    } else {
      b->Append(BC::op_ret);
    }
  }

  bf->SetBytecode(entry, tobc.m_maxFrameSize);
}

void ToBytecode::EmitVarAddr(BC::BasicBlock *b, Var *ve) {
  // FIXME: this applies only for local variables in the function currently
  // being translated.  It does not apply to variables in outer functions in
  // which this function is nested.
  VarInfo &vi = m_varMap[ve];

  switch (vi.m_base) {
    case vb_args:
      b->AppendLea(BC::op_leaargs8, vi.m_offset);
      break;
    case vb_frame:
      b->AppendLea(BC::op_leaframe8, vi.m_offset);
      break;
    default:
      verify(false);
  }

  if (vi.m_autoDeref)
    b->AppendLoad(Type::Address());
}

void ToBytecode::UnwindStack(BC::BasicBlock *&b, int mark, bool inException) {
  verify(mark >= 0 && mark <= int(m_unwindStack.size()));
  for (int i = int(m_unwindStack.size()) - 1; i >= mark; i--) {
    VarDecl *vd = m_unwindStack[i];
    if (vd->NeedsDestructing(inException)) {
      Translate(b, vd->m_destructor);
      verify(b);
    }
  }
}

/******************************************************************************/

TargetClass *BytecodeTarget::For(Class *ce) {
  return new BytecodeClass(ce);
}

IMPLEMENT_OBJECT(BytecodeClass)

void BytecodeClass::Generate() {
  // FIXME: need to initialize all static fields.  Also need to create and
  // initialize internal static fields like the vtable.
}

bool BytecodeClass::HasGlobalStorage(Epoch ep) {
  return m_storage != nullptr;
}

void *BytecodeClass::GlobalStorage(Epoch ep) {
  if (!m_storage) {
    Class *meta = m_class->Metaclass();
    int ptrsz = sizeof(void *);
    m_storage = malloc(meta->StorageSize() + ptrsz);
    *reinterpret_cast<Type *>(m_storage) = m_class->AsType();
    m_storage = reinterpret_cast<char *>(m_storage) + ptrsz;
    m_class->SetClassInVtable(intptr_t(m_storage));
  }

  return m_storage;
  // FIXME: initialize storage... ought to be done by GenerateBytecodes().
}

/******************************************************************************/

TargetConst *BytecodeTarget::For(Const *fe) {
  return new BytecodeConst(fe);
}

IMPLEMENT_OBJECT(BytecodeConst)

void BytecodeConst::Generate() {
}

void *BytecodeConst::GlobalStorage(Epoch ep) {
  // FIXME: Const handles this itself... should it?
  verify(false);
  return nullptr;
}

/******************************************************************************/

TargetGlobal *BytecodeTarget::For(Field *fe) {
  return new BytecodeGlobal(fe);
}

IMPLEMENT_OBJECT(BytecodeGlobal)

void BytecodeGlobal::Generate() {
}

void *BytecodeGlobal::GlobalStorage(Epoch ep) {
  if (!m_storage) {
    size_t size = m_field->GetType().StorageSize();
    m_storage = malloc(size);
    memset(m_storage, 0, size);
  }
  return m_storage;
}

/******************************************************************************/

TargetMethod *BytecodeTarget::For(Method *me) {
  return new BytecodeMethod(me);
}

TargetMethod *BytecodeTarget::For(Method *me, int nm) {
  return new BytecodeMethod(me, nm);
}

IMPLEMENT_OBJECT(BytecodeMethod)

void BytecodeMethod::Generate() {
  // The function object probably exists, as for example it would be created
  // when this method was used in another method.  But not every method is
  // used internally, so...
  GlobalStorage(ep_compile);

  if (!m_method->IsNative()) {
    // Go populate it with code.
    FormalArguments *formals = m_method->LowerFormals();
    ToBytecode::TranslateFunction(
        static_cast<BC::BytecodeFunction *>(m_function), formals, nullptr,
        m_method->Body()->Root());
  }
}

void *BytecodeMethod::GlobalStorage(Epoch ep) {
  if (m_function)
    return m_function;

  FormalArguments *formals = m_method->LowerFormals();

  if (m_method->Body()) {
    BC::BytecodeFunction *bf = new BC::BytecodeFunction();
    bf->SetReturnsValue(formals->m_returnValue != nullptr);
    m_function = bf;
  } else {
    // We have a deferred method with no body.
    // FIXME: arrange to have a meaningful error reported if this gets
    // executed.
  }
  return m_function;
}

namespace {
  class OperatorNew: public BC::NativeFunction {
  public:
    OperatorNew() : BC::NativeFunction("OperatorNew") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      // FIXME: throw exception on failed malloc once exceptions are
      // implemented.
      verify(!extra && argcnt == 1);
      size_t size = args[0].AsSizeT();

      BC::Union result;
      result.SetAddr(malloc(size));
      return result;
    }
  };

  class OperatorPlacementNew: public BC::NativeFunction {
  public:
    OperatorPlacementNew() : BC::NativeFunction("OperatorPlacementNew") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 2);
      return args[0];
    }
  };

  class OperatorDelete: public BC::NativeFunction {
  public:
    OperatorDelete() : BC::NativeFunction("OperatorDelete") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 1);
      free(args[0].AsAddr());
      return s_null;
    }
  };

  class OperatorPlacementDelete: public BC::NativeFunction {
  public:
    OperatorPlacementDelete() : BC::NativeFunction("OperatorPlacementDelete") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 2);
      return s_null;
    }
  };

  // Method BuildAttribute BuildAttribute.operator()(bool shallBuild)
  // Note: also handles other attributes that wrap a bool.
  class BuildFunctor: public BC::NativeFunction {
  public:
    BuildFunctor() : BC::NativeFunction("BuildFunctor") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 2);

      BC::Union result;
      result.SetBool(args[1].m_u32);
      return result;
    }
  };

  // Method Type operator..(long lb, long ub)
  // FIXME: a hack until tuples are implemented
  class SignedSubrangeFunctor: public BC::NativeFunction {
  public:
    SignedSubrangeFunctor() : BC::NativeFunction("SignedSubrangeFunctor") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 2);
      verify(args[0].m_i64 < args[1].m_i64);

      Type t = Type::IntegerSubrange(Integer::Get(args[0].m_i64),
                                     Integer::Get(args[1].m_i64),
                                     true);
      BC::Union result;
      result.SetType(t);
      return result;
    }
  };

  class UnsignedSubrangeFunctor: public BC::NativeFunction {
  public:
    UnsignedSubrangeFunctor() : BC::NativeFunction("UnsignedSubrangeFunctor") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 2);
      verify(args[0].m_u64 < args[1].m_u64);

      Type t = Type::IntegerSubrange(Integer::Get(args[0].m_u64),
                                     Integer::Get(args[1].m_u64),
                                     false);
      BC::Union result;
      result.SetType(t);
      return result;
    }
  };

  class PrintInt: public BC::NativeFunction {
  public:
    PrintInt() : BC::NativeFunction("PrintInt") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 1);
      printf("%d", args->m_i32);
      return s_null;
    }
  };

  class PrintLong: public BC::NativeFunction {
  public:
    PrintLong() : BC::NativeFunction("PrintLong") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 1);
      // Note: can't use int64_t because platforms can't agree on it,
      // making it impossible to use a proper format string.
      printf("%s", std::to_string(args->m_i64).c_str());
      return s_null;
    }
  };

  class PrintUInt: public BC::NativeFunction {
  public:
    PrintUInt() : BC::NativeFunction("PrintUInt") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 1);
      printf("%u", args->m_u32);
      return s_null;
    }
  };

  class PrintULong: public BC::NativeFunction {
  public:
    PrintULong() : BC::NativeFunction("PrintULong") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 1);
      // Ditto for uint64_t.
      printf("%s", std::to_string(args->m_u64).c_str());
      return s_null;
    }
  };

  class PrintBool: public BC::NativeFunction {
  public:
    PrintBool() : BC::NativeFunction("PrintBool") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 1);
      printf(args->m_i32 ? "true" : "false");
      return s_null;
    }
  };

  class PrintStr: public BC::NativeFunction {
  public:
    PrintStr() : BC::NativeFunction("PrintStr") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 2);
      int len = args[1].m_i32;
      printf("%.*s", len, reinterpret_cast<char *>(args[0].AsAddr()));
      return s_null;
    }
  };

  class PrintChar: public BC::NativeFunction {
  public:
    PrintChar() : BC::NativeFunction("PrintChar") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 1);
      putchar(args->m_i32);
      return s_null;
    }
  };

  class PrintFloat: public BC::NativeFunction {
  public:
    PrintFloat() : BC::NativeFunction("PrintFloat") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 1);
      printf("%.4f", *reinterpret_cast<float *>(&args->m_u32));
      return s_null;
    }
  };

  class PrintDouble: public BC::NativeFunction {
  public:
    PrintDouble() : BC::NativeFunction("PrintDouble") { }

    virtual BC::Union Call(BC::Interpreter &I, size_t argcnt, BC::Union *args,
                           BC::Union *extra = nullptr) {
      verify(!extra && argcnt == 1);
      printf("%.4f", *reinterpret_cast<double *>(&args->m_u64));
      return s_null;
    }
  };
}

static OperatorNew g_OperatorNew;
static OperatorPlacementNew g_OperatorPlacementNew;
static OperatorDelete g_OperatorDelete;
static OperatorPlacementDelete g_OperatorPlacementDelete;
static BuildFunctor g_BuildFunctor;
static SignedSubrangeFunctor g_SignedSubrangeFunctor;
static UnsignedSubrangeFunctor g_UnsignedSubrangeFunctor;
static PrintInt g_PrintInt;
static PrintLong g_PrintLong;
static PrintUInt g_PrintUInt;
static PrintULong g_PrintULong;
static PrintBool g_PrintBool;
static PrintStr g_PrintStr;
static PrintChar g_PrintChar;
static PrintFloat g_PrintFloat;
static PrintDouble g_PrintDouble;

BC::Function *BytecodeMethod::GetNativeFunction(int nm) {
  switch (nm) {
    case Method::nm_operator_new:
      return &g_OperatorNew;
    case Method::nm_operator_placement_new:
      return &g_OperatorPlacementNew;
    case Method::nm_operator_delete:
      return &g_OperatorDelete;
    case Method::nm_operator_placement_delete:
      return &g_OperatorPlacementDelete;
    case Method::nm_print_int:
      return &g_PrintInt;
    case Method::nm_print_long:
      return &g_PrintLong;
    case Method::nm_print_uint:
      return &g_PrintUInt;
    case Method::nm_print_ulong:
      return &g_PrintULong;
    case Method::nm_print_bool:
      return &g_PrintBool;
    case Method::nm_print_string:
      return &g_PrintStr;
    case Method::nm_print_char:
      return &g_PrintChar;
    case Method::nm_print_float:
      return &g_PrintFloat;
    case Method::nm_print_double:
      return &g_PrintDouble;
    case Method::nm_build_apply:
      return &g_BuildFunctor;
    case Method::nm_signed_subrange:
      return &g_SignedSubrangeFunctor;
    case Method::nm_unsigned_subrange:
      return &g_UnsignedSubrangeFunctor;
    default:
      verify(false);
      return nullptr;
  }
}
