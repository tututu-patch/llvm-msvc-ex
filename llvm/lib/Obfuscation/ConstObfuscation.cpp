#include "ConstObfuscation.h"
#include "CryptoUtils.h"
#include "Utils.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/GlobalStatus.h"
#include "llvm/Transforms/Utils/LowerSwitch.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

#include <cstring>
#include <iomanip>
#include <map>
#include <set>
#include <sstream>
#include <stdint.h>
#include <vcruntime_string.h>
#include <vector>

using namespace llvm;

static cl::opt<bool>
    RunConstObfuscationPass("const-obfus", cl::init(false),
                            cl::desc("OLLVM - ConstObfuscationPass"));
static cl::opt<bool>
    ConstUseGlobal("const-glb", cl::init(false),
                   cl::desc("OLLVM - ConstObfuscationPass Global"));

namespace {
struct Pair {
  unsigned int pos;
  Value *val;
};
struct ConstEncryption {
  ConstEncryption() = default;
  void handleInstruction2(Function *f, Instruction *ii, unsigned int &count,std::string m_name) {
    int pos = 0;
    std::vector<Pair *> updates;
    for (User::op_iterator opi = ii->op_begin(); opi != ii->op_end();
         opi++, pos++) {
      Value *v = *opi;
      if (isa<ConstantInt>(*v)) {
        ConstantInt *consts = (ConstantInt *)v;
        Type *int8ty = Type::getInt8Ty(f->getContext());
        Type *int16ty = Type::getInt16Ty(f->getContext());
        Type *int32ty = Type::getInt32Ty(f->getContext());
        Type *int64ty = Type::getInt64Ty(f->getContext());
        std::string name = "global_const" + m_name +std::to_string(count);
        if (consts->getType() == int8ty) {
          unsigned char data = (consts->getValue().getZExtValue()) & 0xFF;
          unsigned char rr = (rand() & 0xFF);
          unsigned char tt = data ^ rr;
          Value *val1 = ConstantInt::get(int8ty, tt);
          Value *val2 = ConstantInt::get(int8ty, rr);
          GlobalVariable *g =
              (GlobalVariable *)f->getParent()->getOrInsertGlobal(name, int8ty);
          g->setInitializer((Constant *)val1);
          LoadInst *load = new LoadInst(int8ty, g, "", ii);
          Value *vv = BinaryOperator::Create(Instruction::Xor, (Value *)load,
                                             (Value *)val2, "", ii);
          Pair *node = (Pair *)malloc(sizeof(Pair));
          node->pos = pos;
          node->val = vv;
          updates.push_back(node);
          count++;
        } else if (consts->getType() == int16ty) {
          unsigned short data = (consts->getValue().getZExtValue()) & 0xFFFF;
          unsigned short rr = (rand() & 0xFFFF);
          unsigned short tt = data ^ rr;
          Value *val1 = ConstantInt::get(int16ty, tt);
          Value *val2 = ConstantInt::get(int16ty, rr);
          GlobalVariable *g =
              (GlobalVariable *)f->getParent()->getOrInsertGlobal(name,
                                                                  int16ty);
          g->setInitializer((Constant *)val1);
          LoadInst *load = new LoadInst(int16ty, g, "", ii);
          Value *vv = BinaryOperator::Create(Instruction::Xor, (Value *)load,
                                             (Value *)val2, "", ii);
          Pair *node = (Pair *)malloc(sizeof(Pair));
          node->pos = pos;
          node->val = vv;
          updates.push_back(node);
          count++;
        } else if (consts->getType() == int32ty) {
          unsigned int data = (consts->getValue().getZExtValue()) & 0xFFFFFFFF;
          unsigned int rr = (rand() & 0xFFFFFFFF);
          unsigned int tt = data ^ rr;
          Value *val1 = ConstantInt::get(int32ty, tt);
          Value *val2 = ConstantInt::get(int32ty, rr);
          GlobalVariable *g =
              (GlobalVariable *)f->getParent()->getOrInsertGlobal(name,
                                                                  int32ty);
          g->setInitializer((Constant *)val1);
          LoadInst *load = new LoadInst(int32ty, g, "", ii);
          Value *vv = BinaryOperator::Create(Instruction::Xor, (Value *)load,
                                             (Value *)val2, "", ii);
          Pair *node = (Pair *)malloc(sizeof(Pair));
          node->pos = pos;
          node->val = vv;
          updates.push_back(node);
          count++;
        } else if (consts->getType() == int64ty) {
          unsigned long long data =
              (consts->getValue().getZExtValue()) & 0xFFFFFFFFFFFFFFFF;
          unsigned long long rr =
              (((((unsigned long long)rand()) << 32) | rand()) &
               0xFFFFFFFFFFFFFFFF);
          unsigned long long tt = data ^ rr;
          Value *val1 = ConstantInt::get(int64ty, tt);
          Value *val2 = ConstantInt::get(int64ty, rr);
          GlobalVariable *g =
              (GlobalVariable *)f->getParent()->getOrInsertGlobal(name,
                                                                  int64ty);
          g->setInitializer((Constant *)val1);
          LoadInst *load = new LoadInst(int64ty, g, "", ii);
          Value *vv = BinaryOperator::Create(Instruction::Xor, (Value *)load,
                                             (Value *)val2, "", ii);
          Pair *node = (Pair *)malloc(sizeof(Pair));
          node->pos = pos;
          node->val = vv;
          updates.push_back(node);
          count++;
        } else
          continue;
      }
    }
    for (const auto &update : updates)
      ii->setOperand(update->pos, update->val);
  }
  void handleInstruction1(Function *f, Instruction *ii, unsigned int &count) {
    int pos = 0;
    BasicBlock *bb = &f->getEntryBlock();
    std::vector<Pair *> updates;
    IRBuilder<> irb(&*bb->getFirstInsertionPt());
    for (User::op_iterator opi = ii->op_begin(); opi != ii->op_end();
         opi++, pos++) {
      Value *v = *opi;
      if (isa<ConstantInt>(*v)) {
        ConstantInt *consts = (ConstantInt *)v;
        Type *int8ty = Type::getInt8Ty(f->getContext());
        Type *int16ty = Type::getInt16Ty(f->getContext());
        Type *int32ty = Type::getInt32Ty(f->getContext());
        Type *int64ty = Type::getInt64Ty(f->getContext());
        std::string name = "global_const" + std::to_string(count);
        if (consts->getType() == int8ty) {
          unsigned char data = (consts->getValue().getZExtValue()) & 0xFF;
          unsigned char rr = (rand() & 0xFF);
          unsigned char tt = data ^ rr;
          Value *val1 = ConstantInt::get(int8ty, tt);
          Value *val2 = ConstantInt::get(int8ty, rr);
          AllocaInst *g = irb.CreateAlloca(int8ty);
          irb.CreateStore(val1, g);
          LoadInst *load = new LoadInst(g->getAllocatedType(), g, "", ii);
          Value *vv = BinaryOperator::Create(Instruction::Xor, (Value *)load,
                                             (Value *)val2, "", ii);
          Pair *node = (Pair *)malloc(sizeof(Pair));
          node->pos = pos;
          node->val = vv;
          updates.push_back(node);
          count++;
        } else if (consts->getType() == int16ty) {
          unsigned short data = (consts->getValue().getZExtValue()) & 0xFFFF;
          unsigned short rr = (rand() & 0xFFFF);
          unsigned short tt = data ^ rr;
          Value *val1 = ConstantInt::get(int16ty, tt);
          Value *val2 = ConstantInt::get(int16ty, rr);
          AllocaInst *g = irb.CreateAlloca(int16ty);
          irb.CreateStore(val1, g);
          LoadInst *load = new LoadInst(g->getAllocatedType(), g, "", ii);
          Value *vv = BinaryOperator::Create(Instruction::Xor, (Value *)load,
                                             (Value *)val2, "", ii);
          Pair *node = (Pair *)malloc(sizeof(Pair));
          node->pos = pos;
          node->val = vv;
          updates.push_back(node);
          count++;
        } else if (consts->getType() == int32ty) {
          unsigned int data = (consts->getValue().getZExtValue()) & 0xFFFFFFFF;
          unsigned int rr = (rand() & 0xFFFFFFFF);
          unsigned int tt = data ^ rr;
          Value *val1 = ConstantInt::get(int32ty, tt);
          Value *val2 = ConstantInt::get(int32ty, rr);
          AllocaInst *g = irb.CreateAlloca(int32ty);
          irb.CreateStore(val1, g);
          LoadInst *load = new LoadInst(g->getAllocatedType(), g, "", ii);
          Value *vv = BinaryOperator::Create(Instruction::Xor, (Value *)load,
                                             (Value *)val2, "", ii);
          Pair *node = (Pair *)malloc(sizeof(Pair));
          node->pos = pos;
          node->val = vv;
          updates.push_back(node);
          count++;
        } else if (consts->getType() == int64ty) {
          unsigned long long data =
              (consts->getValue().getZExtValue()) & 0xFFFFFFFFFFFFFFFF;
          unsigned long long rr =
              (((((unsigned long long)rand()) << 32) | rand()) &
               0xFFFFFFFFFFFFFFFF);
          unsigned long long tt = data ^ rr;
          Value *val1 = ConstantInt::get(int64ty, tt);
          Value *val2 = ConstantInt::get(int64ty, rr);
          AllocaInst *g = irb.CreateAlloca(int64ty);
          irb.CreateStore(val1, g);
          LoadInst *load = new LoadInst(g->getAllocatedType(), g, "", ii);
          Value *vv = BinaryOperator::Create(Instruction::Xor, (Value *)load,
                                             (Value *)val2, "", ii);
          Pair *node = (Pair *)malloc(sizeof(Pair));
          node->pos = pos;
          node->val = vv;
          updates.push_back(node);
          count++;
        } else
          continue;
      }
    }
    for (const auto &update : updates)
      ii->setOperand(update->pos, update->val);
  }
  void ReplaceConst(Module *M) {
    unsigned int count = 0;

    for (Function &func : *M) {
      std::vector<Instruction *> instr_list;
      for (BasicBlock &bb : func)
        for (Instruction &ii : bb) {
          for (User::op_iterator opi = ii.op_begin(); opi != ii.op_end();
               opi++) {
            Value *v = *opi;
            if (isa<ConstantInt>(*v)) {
              instr_list.push_back(&ii);
              break;
            }
          }
        }
      for (const auto &iter : instr_list) {
        if (ConstUseGlobal) {
            handleInstruction2(&func,iter,count,M->getName().data());
        } else {
          handleInstruction1(&func, iter, count);
        }
      }
    }
  }

  bool runOnModule(Module &M) {
    ReplaceConst(&M);
    return true;
  }
};
} // namespace

PreservedAnalyses ConstObfuscationPass::run(Module &M,
                                            ModuleAnalysisManager &AM) {

  ConstEncryption str;
  if (RunConstObfuscationPass == true) {
    if (str.runOnModule(M))
      return PreservedAnalyses::none();
  }
  return PreservedAnalyses::all();
}