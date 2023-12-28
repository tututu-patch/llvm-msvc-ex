#include "ConstObfuscation.h"
#include "CryptoUtils.h"
#include "Utils.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FormatVariadic.h"

#include <cstdint>
#include <iomanip>
#include <set>
#include <sstream>
#include <vector>

using namespace llvm;

static cl::opt<bool>
    RunConstObfuscationPass("const-obfus", cl::init(false),
                            cl::desc("OLLVM - ConstObfuscationPass"));
static cl::opt<int>
    ObfConstTimes("const-times", cl::init(1),
                  cl::desc("Run ConstObfuscation pass <const-times> time(s)"));

namespace llvm {
// struct Pair {
//   unsigned int pos;
//   Value *val;
// };
// struct ConstEncryption {
//  ConstEncryption():CONTEXT(nullptr){}
//  LLVMContext *CONTEXT;
//  // void handleInstruction2(Function *f, Instruction *ii, unsigned int
//  // &count,std::string m_name) {
//  //   int pos = 0;
//  //   //errs()<<m_name<<"\r\n";
//  //   std::vector<Pair *> updates;
//  //   for (User::op_iterator opi = ii->op_begin(); opi != ii->op_end();
//  //        opi++, pos++) {
//  //     //printf"CONST OBFUS %s\r\n",ii->getName().data());
//  //     Value *v = *opi;
//  //     if (isa<ConstantInt>(*v)) {
//  //       //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //       ConstantInt *consts = (ConstantInt *)v;
//  //       Type *int8ty = Type::getInt8Ty(f->getContext());
//  //       Type *int16ty = Type::getInt16Ty(f->getContext());
//  //       Type *int32ty = Type::getInt32Ty(f->getContext());
//  //       Type *int64ty = Type::getInt64Ty(f->getContext());
//  //       std::string name = "global_const" + m_name +std::to_string(count);
//  //       /*errs()<<name<<"\r\n";
//  //       ii->print(errs());
//  //       errs() << "\r\n";*/
//  //       if (consts->getType() == int8ty) {
//  //         //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //         unsigned char data = (consts->getValue().getZExtValue()) & 0xFF;
//  //         unsigned char rr = (cryptoutils->get_uint32_t() & 0xFF);
//  //         unsigned char tt = data ^ rr;
//  //         Value *val1 = ConstantInt::get(int8ty, tt);
//  //         Value *val2 = ConstantInt::get(int8ty, rr);
//  //         GlobalVariable *g =
//  //             (GlobalVariable *)f->getParent()->getOrInsertGlobal(name,
//  //             int8ty);
//  //         g->setInitializer((Constant *)val1);
//  //         LoadInst *load = new LoadInst(int8ty, g, "", ii);
//  //         Value *vv = BinaryOperator::Create(Instruction::Xor, (Value
//  *)load,
//  //                                            (Value *)val2, "", ii);
//  //         Pair *node = (Pair *)malloc(sizeof(Pair));
//  //         node->pos = pos;
//  //         node->val = vv;
//  //         updates.push_back(node);
//  //         count++;
//  //       } else if (consts->getType() == int16ty) {
//  //         //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //         unsigned short data = (consts->getValue().getZExtValue()) &
//  0xFFFF;
//  //         unsigned short rr = (cryptoutils->get_uint32_t() & 0xFFFF);
//  //         unsigned short tt = data ^ rr;
//  //         Value *val1 = ConstantInt::get(int16ty, tt);
//  //         Value *val2 = ConstantInt::get(int16ty, rr);
//  //         GlobalVariable *g =
//  //             (GlobalVariable *)f->getParent()->getOrInsertGlobal(name,
//  //                                                                 int16ty);
//  //         g->setInitializer((Constant *)val1);
//  //         LoadInst *load = new LoadInst(int16ty, g, "", ii);
//  //         Value *vv = BinaryOperator::Create(Instruction::Xor, (Value
//  *)load,
//  //                                            (Value *)val2, "", ii);
//  //         Pair *node = (Pair *)malloc(sizeof(Pair));
//  //         node->pos = pos;
//  //         node->val = vv;
//  //         updates.push_back(node);
//  //         count++;
//  //       } else if (consts->getType() == int32ty) {
//  //         //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //         unsigned int data = (consts->getValue().getZExtValue()) &
//  //         0xFFFFFFFF; unsigned int rr = (cryptoutils->get_uint32_t() &
//  //         0xFFFFFFFF); unsigned int tt = data ^ rr; Value *val1 =
//  //         ConstantInt::get(int32ty, tt); Value *val2 =
//  //         ConstantInt::get(int32ty, rr); GlobalVariable *g =
//  //             (GlobalVariable *)f->getParent()->getOrInsertGlobal(name,
//  //                                                                 int32ty);
//  //         g->setInitializer((Constant *)val1);
//  //         LoadInst *load = new LoadInst(int32ty, g, "", ii);
//  //         Value *vv = BinaryOperator::Create(Instruction::Xor, (Value
//  *)load,
//  //                                            (Value *)val2, "", ii);
//  //         Pair *node = (Pair *)malloc(sizeof(Pair));
//  //         node->pos = pos;
//  //         node->val = vv;
//  //         updates.push_back(node);
//  //         count++;
//  //       } else if (consts->getType() == int64ty) {
//  //         //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //         unsigned long long data =
//  //             (consts->getValue().getZExtValue()) & 0xFFFFFFFFFFFFFFFF;
//  //         unsigned long long rr =
//  //             (cryptoutils->get_uint64_t() &
//  //              0xFFFFFFFFFFFFFFFF);
//  //         unsigned long long tt = data ^ rr;
//  //         Value *val1 = ConstantInt::get(int64ty, tt);
//  //         Value *val2 = ConstantInt::get(int64ty, rr);
//  //         GlobalVariable *g =
//  //             (GlobalVariable *)f->getParent()->getOrInsertGlobal(name,
//  //                                                                 int64ty);
//  //         g->setInitializer((Constant *)val1);
//  //         LoadInst *load = new LoadInst(int64ty, g, "", ii);
//  //         Value *vv = BinaryOperator::Create(Instruction::Xor, (Value
//  *)load,
//  //                                            (Value *)val2, "", ii);
//  //         Pair *node = (Pair *)malloc(sizeof(Pair));
//  //         node->pos = pos;
//  //         node->val = vv;
//  //         updates.push_back(node);
//  //         count++;
//  //       } else
//  //         continue;
//  //     }
//  //   }
//  //   for (const auto &update : updates)
//  //     ii->setOperand(update->pos, update->val);
//  // }
//  // void handleInstruction1(Function *f, Instruction *ii, unsigned int
//  &count)
//  // {
//  //   int pos = 0;
//  //   //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //   BasicBlock *bb = &f->getEntryBlock();
//  //   std::vector<Pair *> updates;
//  //   IRBuilder<> irb(&*bb->getFirstInsertionPt());
//  //   for (User::op_iterator opi = ii->op_begin(); opi != ii->op_end();
//  //        opi++, pos++) {
//  //     //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //     Value *v = *opi;
//  //     if (isa<ConstantInt>(*v)) {
//  //       ConstantInt *consts = (ConstantInt *)v;
//  //       Type *int8ty = Type::getInt8Ty(f->getContext());
//  //       Type *int16ty = Type::getInt16Ty(f->getContext());
//  //       Type *int32ty = Type::getInt32Ty(f->getContext());
//  //       Type *int64ty = Type::getInt64Ty(f->getContext());
//  //       std::string name = "global_const" + std::to_string(count);
//  //       if (consts->getType() == int8ty) {
//  //         //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //         unsigned char data = (consts->getValue().getZExtValue()) & 0xFF;
//  //         unsigned char rr = (cryptoutils->get_uint32_t() & 0xFF);
//  //         unsigned char tt = data ^ rr;
//  //         Value *val1 = ConstantInt::get(int8ty, tt);
//  //         Value *val2 = ConstantInt::get(int8ty, rr);
//  //         AllocaInst *g = irb.CreateAlloca(int8ty);
//  //         irb.CreateStore(val1, g);
//  //         LoadInst *load = new LoadInst(g->getAllocatedType(), g, "", ii);
//  //         Value *vv = BinaryOperator::Create(Instruction::Xor, (Value
//  *)load,
//  //                                            (Value *)val2, "", ii);
//  //         Pair *node = (Pair *)malloc(sizeof(Pair));
//  //         node->pos = pos;
//  //         node->val = vv;
//  //         updates.push_back(node);
//  //         count++;
//  //       } else if (consts->getType() == int16ty) {
//  //         //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //         unsigned short data = (consts->getValue().getZExtValue()) &
//  0xFFFF;
//  //         unsigned short rr = (cryptoutils->get_uint32_t() & 0xFFFF);
//  //         unsigned short tt = data ^ rr;
//  //         Value *val1 = ConstantInt::get(int16ty, tt);
//  //         Value *val2 = ConstantInt::get(int16ty, rr);
//  //         AllocaInst *g = irb.CreateAlloca(int16ty);
//  //         irb.CreateStore(val1, g);
//  //         LoadInst *load = new LoadInst(g->getAllocatedType(), g, "", ii);
//  //         Value *vv = BinaryOperator::Create(Instruction::Xor, (Value
//  *)load,
//  //                                            (Value *)val2, "", ii);
//  //         Pair *node = (Pair *)malloc(sizeof(Pair));
//  //         node->pos = pos;
//  //         node->val = vv;
//  //         updates.push_back(node);
//  //         count++;
//  //       } else if (consts->getType() == int32ty) {
//  //         //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //         unsigned int data = (consts->getValue().getZExtValue()) &
//  //         0xFFFFFFFF; unsigned int rr = (cryptoutils->get_uint32_t() &
//  //         0xFFFFFFFF); unsigned int tt = data ^ rr; Value *val1 =
//  //         ConstantInt::get(int32ty, tt); Value *val2 =
//  //         ConstantInt::get(int32ty, rr); AllocaInst *g =
//  //         irb.CreateAlloca(int32ty); irb.CreateStore(val1, g); LoadInst
//  *load
//  //         = new LoadInst(g->getAllocatedType(), g, "", ii); Value *vv =
//  //         BinaryOperator::Create(Instruction::Xor, (Value *)load,
//  //                                            (Value *)val2, "", ii);
//  //         Pair *node = (Pair *)malloc(sizeof(Pair));
//  //         node->pos = pos;
//  //         node->val = vv;
//  //         updates.push_back(node);
//  //         count++;
//  //       } else if (consts->getType() == int64ty) {
//  //         //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //         unsigned long long data =
//  //             (consts->getValue().getZExtValue()) & 0xFFFFFFFFFFFFFFFF;
//  //         unsigned long long rr =
//  //             (cryptoutils->get_uint64_t() &
//  //              0xFFFFFFFFFFFFFFFF);
//  //         unsigned long long tt = data ^ rr;
//  //         Value *val1 = ConstantInt::get(int64ty, tt);
//  //         Value *val2 = ConstantInt::get(int64ty, rr);
//  //         AllocaInst *g = irb.CreateAlloca(int64ty);
//  //         irb.CreateStore(val1, g);
//  //         LoadInst *load = new LoadInst(g->getAllocatedType(), g, "", ii);
//  //         Value *vv = BinaryOperator::Create(Instruction::Xor, (Value
//  *)load,
//  //                                            (Value *)val2, "", ii);
//  //         Pair *node = (Pair *)malloc(sizeof(Pair));
//  //         node->pos = pos;
//  //         node->val = vv;
//  //         updates.push_back(node);
//  //         count++;
//  //       } else
//  //         continue;
//  //     }
//  //   }
//  //   //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //   for (const auto &update : updates)
//  //     ii->setOperand(update->pos, update->val);
//  //   //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  // }
//  // void ReplaceConst(Module *M) {
//  //   unsigned int count = 0;
//  //   //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //   for (Function &func : *M) {
//  //     //printf"%s\r\n",func.getName().data());
//  //     std::vector<Instruction *> instr_list;
//  //     for (BasicBlock &bb : func)
//  //     {
//  //       for (Instruction &ii : bb)
//  //       {
//  //         auto BI = cast<BinaryOperator>(&ii);
//  //         auto opcode = BI->getOpcode();
//  //         //errs()<<"op = "<<opcode<<"\r\n";
//  //         {
//  //           for (User::op_iterator opi = ii.op_begin(); opi != ii.op_end();
//  //                opi++)
//  //           {
//  //             Value *v = *opi;
//  //             if (isa<ConstantInt>(*v))
//  //             {
//  //               //errs()<<"op = "<<opcode<<"\r\n";
//  //               instr_list.push_back(&ii);
//  //               break;
//  //             }
//  //           }
//  //         }
//  //       }
//  //     }
//  //     for (const auto &iter : instr_list) {
//  //       if (ConstUseGlobal) {
//  //           //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //           handleInstruction2(&func,iter,count,M->getName().str());
//  //       } else {
//  //         //printf"CONST OBFUS %d %s\r\n",__LINE__,__FUNCTION__);
//  //         handleInstruction1(&func, iter, count);
//  //       }
//  //       //
//  //     }
//  //     fixStack(func,true);
//  //   }
//  // }
//
//  // bool runOnModule(Module &M) {
//  //   ReplaceConst(&M);
//  //   return true;
//  // }
//  bool shouldEncryptConstant(Instruction *I);
//  void bitwiseSubstitute(Instruction *I, int i);
//  void linearSubstitute(Instruction *I, int i);
//  void substitute(Instruction *I);
//  bool runOnFunction(Function &F);
//};

bool ConstEncryption::shouldEncryptConstant(Instruction *I) {

  if (vm_flag) {
    if (isa<IntrinsicInst>(I) || isa<GetElementPtrInst>(I) || isa<PHINode>(I) ||
        I->isAtomic())
      return false;
  } else {
    if (isa<SwitchInst>(I) || isa<IntrinsicInst>(I) ||
        isa<GetElementPtrInst>(I) || isa<PHINode>(I) || I->isAtomic())
      return false;
  }
  if (AllocaInst *AI = dyn_cast<AllocaInst>(I))
    if (AI->isSwiftError())
      return false;
  return true;
}

void ConstEncryption::bitwiseSubstitute(Instruction *I, int i) {
  Module &M = *I->getModule();
  ConstantInt *val = cast<ConstantInt>(I->getOperand(i));
  IntegerType *type = cast<IntegerType>(val->getType());
  uint32_t width = type->getIntegerBitWidth();
  // ����λ��С��8�������������
  if (width < 8) {
    // �Ϳ���ֻ�ܽ��������滻
    // linearSubstitute(I,i);
    return;
  }
  uint32_t left = cryptoutils->get_uint32_t() % (width - 1) + 1;
  uint32_t right = width - left;
  // ������� x, y
  APInt mask = type->getMask();
  uint64_t randX = (cryptoutils->get_uint64_t() & mask).getZExtValue();
  uint64_t randY = (cryptoutils->get_uint64_t() & mask).getZExtValue();
  // ���� c = val ^ (x << left | y >> right)
  APInt c = val->getValue() ^ (randX << left | randY >> right);
  ConstantInt *constX = CONST(type, randX);
  ConstantInt *constY = CONST(type, randY);
  ConstantInt *constC = (ConstantInt *)CONST(type, c);
  // ����ȫ�ֱ��� x, y
  GlobalVariable *x = new GlobalVariable(
      M, type, false, GlobalValue::PrivateLinkage, constX, "x");
  GlobalVariable *y = new GlobalVariable(
      M, type, false, GlobalValue::PrivateLinkage, constY, "y");
  LoadInst *opX = new LoadInst(type, x, "", I);
  LoadInst *opY = new LoadInst(type, y, "", I);
  // ���� op = (x << left | y >> right) ^ c ����ʽ
  BinaryOperator *op1 =
      BinaryOperator::CreateShl(opX, CONST(type, left), "", I);
  BinaryOperator *op2 =
      BinaryOperator::CreateLShr(opY, CONST(type, right), "", I);
  BinaryOperator *op3 = BinaryOperator::CreateOr(op1, op2, "", I);
  BinaryOperator *op4 = BinaryOperator::CreateXor(op3, constC, "", I);
  // �ñ���ʽ (x << left | y >> right) ^ c �滻ԭ����������
  I->setOperand(i, op4);
}

void ConstEncryption::linearSubstitute(Instruction *I, int i) {
  Module &M = *I->getModule();
  ConstantInt *val = cast<ConstantInt>(I->getOperand(i));
  IntegerType *type = cast<IntegerType>(val->getType());
  // ������� x, y, a, b
  uint64_t randX = cryptoutils->get_uint64_t(),
           randY = cryptoutils->get_uint64_t();
  uint64_t randA = cryptoutils->get_uint64_t(),
           randB = cryptoutils->get_uint64_t();
  // ���� c = val - (ax + by)
  APInt c = val->getValue() - (randA * randX + randB * randY);
  ConstantInt *constX = CONST(type, randX);
  ConstantInt *constY = CONST(type, randY);
  ConstantInt *constA = CONST(type, randA);
  ConstantInt *constB = CONST(type, randB);
  ConstantInt *constC = (ConstantInt *)CONST(type, c);
  // ����ȫ�ֱ��� x, y
  GlobalVariable *x = new GlobalVariable(
      M, type, false, GlobalValue::PrivateLinkage, constX, "x");
  GlobalVariable *y = new GlobalVariable(
      M, type, false, GlobalValue::PrivateLinkage, constY, "y");
  LoadInst *opX = new LoadInst(type, x, "", I);
  LoadInst *opY = new LoadInst(type, y, "", I);
  // ���� op = ax + by + c ����ʽ
  BinaryOperator *op1 = BinaryOperator::CreateMul(opX, constA, "", I);
  BinaryOperator *op2 = BinaryOperator::CreateMul(opY, constB, "", I);
  BinaryOperator *op3 = BinaryOperator::CreateAdd(op1, op2, "", I);
  BinaryOperator *op4 = BinaryOperator::CreateAdd(op3, constC, "", I);
  // �ñ���ʽ ax + by + c �滻ԭ����������
  I->setOperand(i, op4);
}

void ConstEncryption::substitute(Instruction *I) {
  const int operand_num = I->getNumOperands();
  for (int i = 0; i < operand_num; i++) {
    if (isa<ConstantInt>(I->getOperand(i))) {
      switch (const uint32_t choice = cryptoutils->get_uint32_t() % 2) {
      case 0:
        linearSubstitute(I, i);
        break;
      case 1:
        bitwiseSubstitute(I, i);
        break;
      default:
        break;
      }
    }
  }
}

bool ConstEncryption::runOnFunction(Function &F, bool vm_fla) {
  CONTEXT = &F.getContext();
  this->vm_flag = vm_fla;
  for (int i = 0; i < ObfConstTimes; i++) {
    for (BasicBlock &BB : F) {
      std::vector<Instruction *> origInst;
      for (Instruction &I : BB) {
        origInst.push_back(&I);
      }
      for (Instruction *I : origInst) {
        if (shouldEncryptConstant(I)) {
          // I->print(errs());
          // errs()<<"\r\n";
          substitute(I);
        }
      }
    }
  }
  return true;
}
} // namespace llvm

PreservedAnalyses ConstObfuscationPass::run(Function &F,
                                            FunctionAnalysisManager &AM) {
  ConstEncryption str;
  if (toObfuscate(RunConstObfuscationPass, &F, "const-obfus")) {
    if (str.runOnFunction(F, false))
      return PreservedAnalyses::none();
  }
  return PreservedAnalyses::all();
}
