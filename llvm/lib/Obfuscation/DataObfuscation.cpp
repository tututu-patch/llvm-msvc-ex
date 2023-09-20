#include "DataObfuscation.h"
#include "CryptoUtils.h"
#include "Utils.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SetVector.h"
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
    RunDataObfuscationPass("data-obfus", cl::init(false),
                           cl::desc("OLLVM - DataObfuscationPass"));
namespace {
struct DataObfuscator {
  DataObfuscator() = default;
  void choose(int range, SetVector<int> *selected, SetVector<int> *unselect,
              int count);

  void removeSet(SetVector<int> &all, SetVector<int> &remove,
                 SetVector<int> &out);
  void collectAlloca(Function &f, SetVector<AllocaInst *> &allocaList);
  void dataFlowObfu(Function &f, SetVector<AllocaInst *> &allocaList);
  void doObfu(Function &f, SetVector<AllocaInst *> &varList, int size);
  void rubbishCode(IRBuilder<> &irb, SetVector<AllocaInst *> &array,
                   SetVector<int> &vars, int prob);
  bool runOnFunction(Function &f) {
    SetVector<AllocaInst *> allocaInsts;
    collectAlloca(f, allocaInsts);
    dataFlowObfu(f, allocaInsts);
    return false;
  }
};

void DataObfuscator::choose(int range, SetVector<int> *selected,
                            SetVector<int> *unselect, int count) {
  bool *used = (bool *)malloc(sizeof(bool) * range);
  memset(used, 0, sizeof(used));
  for (int i = 0; i < count; i++) {
    int id = rand() % range;
    while (used[id])
      id = rand() % range;
    used[id] = true;
    selected->insert(id);
  }
  if (unselect != nullptr) {
    for (int i = 0; i < range; i++)
      if (!used[i])
        unselect->insert(i);
  }
  free(used);
}

void DataObfuscator::removeSet(SetVector<int> &all, SetVector<int> &remove,
                               SetVector<int> &out) {
  for (int v : all) {
    bool ok = true;
    for (const int r : remove) {
      if (v == r) {
        ok = false;
        break;
      }
    }
    if (ok)
      out.insert(v);
  }
}

void DataObfuscator::collectAlloca(Function &f,
                                   SetVector<AllocaInst *> &allocaList) {
  for (BasicBlock &bb : f)
    for (Instruction &instr : bb)
      if (isa<AllocaInst>(instr))
        allocaList.insert(static_cast<AllocaInst *>(&instr));
}

void DataObfuscator::dataFlowObfu(Function &f,
                                  SetVector<AllocaInst *> &allocaList) {
  SetVector<AllocaInst *> charList, shortList, intList, longList;
  for (AllocaInst *a : allocaList) {
    Type *type = a->getAllocatedType();
    if (type->isIntegerTy()) {
      IntegerType *intType = (IntegerType *)type;
      if (intType->getBitWidth() == 8)
        charList.insert(a);
      else if (intType->getBitWidth() == 16)
        shortList.insert(a);
      else if (intType->getBitWidth() == 32)
        intList.insert(a);
      else if (intType->getBitWidth() == 64)
        longList.insert(a);
    }
  }
  doObfu(f, charList, charList.size() + 10);
  doObfu(f, shortList, shortList.size() + 10);
  doObfu(f, intList, intList.size() + 10);
  doObfu(f, longList, longList.size() + 10);
  for (AllocaInst *a : charList)
    a->eraseFromParent();
  for (AllocaInst *a : shortList)
    a->eraseFromParent();
  for (AllocaInst *a : intList)
    a->eraseFromParent();
  for (AllocaInst *a : longList)
    a->eraseFromParent();
}

void DataObfuscator::doObfu(Function &f, SetVector<AllocaInst *> &varList,
                            int size) {
  int prob = 100;
  MapVector<AllocaInst *, int> indexMap;
  if (varList.size() <= 0)
    return;
  Type *type = varList[0]->getAllocatedType();
  SetVector<int> freeId, usedId;
  choose(size, &usedId, &freeId, varList.size());
  IRBuilder<> irb(&*f.getEntryBlock().getFirstInsertionPt());
  SetVector<AllocaInst *> array;
  for (int i = 0; i < size; i++)
    array.insert(irb.CreateAlloca(type));
  BasicBlock::iterator entry = irb.GetInsertPoint();
  for (BasicBlock &bb : f) {
    BasicBlock::iterator iter = bb.begin();
    if (&bb == &f.getEntryBlock())
      iter = entry;
    while (iter != bb.end()) {
      irb.SetInsertPoint(&*iter);
      rubbishCode(irb, array, freeId, prob);
      iter++;
    }
  }
  int idx = 0;
  for (AllocaInst *a : varList) {
    int id = usedId[idx++];
    Value *zero = ConstantInt::get(Type::getInt32Ty(f.getContext()), 0);
    for (BasicBlock &bb : f)
      for (Instruction &inst : bb) {
        if (isa<LoadInst>(inst) && inst.getOperand(0) == a) {
          SetVector<int> used, unused;
          choose(freeId.size(), &used, &unused, 3);
          SetVector<int> rubbishVars;
          for (int v : unused)
            rubbishVars.insert(freeId[v]);
          Value *vr = ConstantInt::get(type, rand());
          irb.SetInsertPoint(&inst);
          Value *ga = array[freeId[used[0]]];
          Value *gb = array[freeId[used[1]]];
          Value *gc = array[freeId[used[2]]];
          if (rand() % 2) {
            irb.CreateStore(
                irb.CreateAdd(irb.CreateLoad(ga->getType(), ga), vr), gb);
            rubbishCode(irb, array, rubbishVars, prob);
            rubbishCode(irb, array, rubbishVars, prob);
            irb.CreateStore(
                irb.CreateAdd(irb.CreateLoad(ga->getType(), ga),
                              irb.CreateLoad(array[id]->getType(), array[id])),
                ga);
            rubbishCode(irb, array, rubbishVars, prob);
            rubbishCode(irb, array, rubbishVars, prob);
            irb.CreateStore(
                irb.CreateAdd(vr,
                              irb.CreateSub(irb.CreateLoad(ga->getType(), ga),
                                            irb.CreateLoad(gb->getType(), gb))),
                gc);
            rubbishCode(irb, array, rubbishVars, prob);
            rubbishCode(irb, array, rubbishVars, prob);
          } else {
            irb.CreateStore(
                irb.CreateXor(irb.CreateLoad(ga->getType(), ga), vr), gb);
            rubbishCode(irb, array, rubbishVars, prob);
            rubbishCode(irb, array, rubbishVars, prob);
            irb.CreateStore(
                irb.CreateXor(irb.CreateLoad(ga->getType(), ga),
                              irb.CreateLoad(array[id]->getType(), array[id])),
                ga);
            rubbishCode(irb, array, rubbishVars, prob);
            rubbishCode(irb, array, rubbishVars, prob);
            irb.CreateStore(
                irb.CreateXor(vr,
                              irb.CreateXor(irb.CreateLoad(ga->getType(), ga),
                                            irb.CreateLoad(gb->getType(), gb))),
                gc);
            rubbishCode(irb, array, rubbishVars, prob);
            rubbishCode(irb, array, rubbishVars, prob);
          }

          inst.setOperand(0, gc);
        } else if (isa<StoreInst>(inst) && inst.getOperand(1) == a) {
          Value *val = inst.getOperand(0);
          SetVector<int> used, unused;
          choose(freeId.size(), &used, &unused, 3);
          SetVector<int> rubbishVars;
          for (int v : unused)
            rubbishVars.insert(freeId[v]);
          Value *vr = ConstantInt::get(type, rand());
          irb.SetInsertPoint(&inst);
          Value *ga = array[freeId[used[0]]];
          Value *gb = array[freeId[used[1]]];
          Value *gc = array[freeId[used[2]]];
          if (rand() % 2) {
            irb.CreateStore(
                irb.CreateAdd(irb.CreateLoad(ga->getType(), ga), vr), gb);
            rubbishCode(irb, array, rubbishVars, prob);
            rubbishCode(irb, array, rubbishVars, prob);
            irb.CreateStore(
                irb.CreateAdd(irb.CreateLoad(ga->getType(), ga), val), ga);
            rubbishCode(irb, array, rubbishVars, prob);
            rubbishCode(irb, array, rubbishVars, prob);
            irb.CreateStore(
                irb.CreateAdd(vr,
                              irb.CreateSub(irb.CreateLoad(ga->getType(), ga),
                                            irb.CreateLoad(gb->getType(), gb))),
                gc);
            rubbishCode(irb, array, rubbishVars, prob);
            rubbishCode(irb, array, rubbishVars, prob);
          } else {
            irb.CreateStore(
                irb.CreateXor(irb.CreateLoad(ga->getType(), ga), vr), gb);
            rubbishCode(irb, array, rubbishVars, prob);
            rubbishCode(irb, array, rubbishVars, prob);
            irb.CreateStore(
                irb.CreateXor(irb.CreateLoad(ga->getType(), ga), val), ga);
            rubbishCode(irb, array, rubbishVars, prob);
            rubbishCode(irb, array, rubbishVars, prob);
            irb.CreateStore(
                irb.CreateXor(vr,
                              irb.CreateXor(irb.CreateLoad(ga->getType(), ga),
                                            irb.CreateLoad(gb->getType(), gb))),
                gc);
            rubbishCode(irb, array, rubbishVars, prob);
            rubbishCode(irb, array, rubbishVars, prob);
          }
          inst.setOperand(0, irb.CreateLoad(gc->getType(), gc));
          inst.setOperand(1, array[id]);
        } else {
          int c = 0;
          for (Value *ops : inst.operands()) {
            if (ops == a) {
              irb.SetInsertPoint(&inst);
              inst.setOperand(c, array[id]);
            }

            c++;
          }
        }
      }
  }
}

void DataObfuscator::rubbishCode(IRBuilder<> &irb,
                                 SetVector<AllocaInst *> &array,
                                 SetVector<int> &vars, int prob) {
  if (rand() % 100 > prob)
    return;
  SetVector<int> idx;
  choose(vars.size(), &idx, nullptr, 2);
  Value *l = (Value *)array[vars[idx[0]]];
  Value *r;
  int op = rand() % 4;
  int o = rand() % 2;
  if (o)
    r = ConstantInt::get(array[0]->getAllocatedType(), rand());
  else
    r = irb.CreateLoad(array[vars[idx[1]]]->getType(), array[vars[idx[1]]]);
  if (op == 0)
    irb.CreateStore(irb.CreateAdd(irb.CreateLoad(l->getType(), l), r), l);
  else if (op == 1)
    irb.CreateStore(irb.CreateSub(irb.CreateLoad(l->getType(), l), r), l);
  else if (op == 2)
    irb.CreateStore(irb.CreateMul(irb.CreateLoad(l->getType(), l), r), l);
  else
    irb.CreateStore(irb.CreateXor(irb.CreateLoad(l->getType(), l), r), l);
}

} // namespace

PreservedAnalyses DataObfuscationPass::run(Function &F,
                                           FunctionAnalysisManager &AM) {
  DataObfuscator s;
  if (toObfuscate(RunDataObfuscationPass, &F, "data-obfus")) {
    if (s.runOnFunction(F))
      return PreservedAnalyses::none();
  }
  return PreservedAnalyses::all();
}