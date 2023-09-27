#include "VMFlatten.h"
#include "CryptoUtils.h"
#include "Utils.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Transforms/Utils/LowerSwitch.h"


#include <cstring>
#include <iomanip>
#include <map>
#include <set>
#include <sstream>
#include <cstdint>
#include <vcruntime_string.h>
#include <vector>

// #define RUN_BLOCK 1
// #define JMP_BORING 2
// #define JMP_SELECT 3

using namespace llvm;
static cl::opt<bool>
    RunVmFlatObfuscationPass("vm-fla", cl::init(false),
                             cl::desc("OLLVM - VmFlattenObfuscationPass"));

static cl::opt<int> VmObfuProbRate(
    "vm-prob", cl::init(100),
    cl::desc("Choose the probability <vm-prob> for each basic blocks will "
             "be obfuscated by VmFlattenObfuscationPass"));

namespace {
struct VMFlat {
  struct Node {
    unsigned int value;
    Node *bb1, *bb2;
    BasicBlock *data;
  };
  struct VMInst {
    unsigned int type;
    unsigned int op1, op2;
  };
  unsigned int RUN_BLOCK;
  unsigned int JMP_BORING;
  unsigned int JMP_SELECT;
  std::vector<BasicBlock *> *getBlocks(Function *function,
                                       std::vector<BasicBlock *> *lists);

  unsigned int getUniqueNumber(const std::vector<unsigned int> *rand_list);

  Node *newNode(unsigned int value);

  VMInst *newInst(unsigned int type, unsigned int op1, unsigned int op2);

  void create_node_inst(std::vector<VMInst *> *all_inst,
                        std::map<Node *, unsigned int> *inst_map, Node *node);

  void gen_inst(std::vector<VMInst *> *all_inst,
                std::map<Node *, unsigned int> *inst_map, const Node *node);

  Node *findBBNode(const BasicBlock *bb, const std::vector<Node *> *all_node);

  void dump_inst(const std::vector<VMInst *> *all_inst) const;
  bool DoFlatten(Function *f, int seed);
  bool runVmFlaOnFunction(Function &function);
};

std::vector<BasicBlock *> *VMFlat::getBlocks(Function *function,
                                             std::vector<BasicBlock *> *lists) {
  lists->clear();
  for (BasicBlock &basicBlock : *function)
    lists->push_back(&basicBlock);
  return lists;
}

unsigned VMFlat::getUniqueNumber(const std::vector<unsigned> *rand_list) {
  unsigned int num = cryptoutils->get_uint32_t();
  while (true) {
    bool state = true;
    for (const unsigned int &n : *rand_list)
      if (n == num) {
        state = false;
        break;
      }
    if (state)
      break;
    num = cryptoutils->get_uint32_t();
  }
  return num;
}

VMFlat::Node *VMFlat::newNode(unsigned value) {
  const auto node = static_cast<Node *>(malloc(sizeof(Node)));
  node->value = value;
  node->bb1 = node->bb2 = nullptr;
  return node;
}

VMFlat::VMInst *VMFlat::newInst(unsigned type, unsigned op1, unsigned op2) {
  const auto code = static_cast<VMInst *>(malloc(sizeof(VMInst)));
  code->type = type;
  code->op1 = op1;
  code->op2 = op2;
  return code;
}

void VMFlat::create_node_inst(std::vector<VMInst *> *all_inst,
                              std::map<Node *, unsigned> *inst_map,
                              Node *node) {
  VMInst *code = newInst(RUN_BLOCK, node->value, 0);
  all_inst->push_back(code);
  inst_map->insert(
      std::map<Node *, unsigned int>::value_type(node, all_inst->size() - 1));
}

void VMFlat::gen_inst(std::vector<VMInst *> *all_inst,
                      std::map<Node *, unsigned> *inst_map, const Node *node) {
  // assert(!(node->bb1==NULL && node->bb2!=NULL));
  if (node->bb1 != nullptr && node->bb2 == nullptr) {
    if (inst_map->count(node->bb1) == 0) {
      create_node_inst(all_inst, inst_map, node->bb1);
      gen_inst(all_inst, inst_map, node->bb1);
    } else {
      const unsigned int addr = inst_map->find(node->bb1)->second * 3;
      VMInst *code = newInst(JMP_BORING, addr, 0);
      all_inst->push_back(code);
    }
  } else if (node->bb2 != nullptr) {
    VMInst *code = newInst(JMP_SELECT, 0, 0);
    all_inst->push_back(code);
    if (inst_map->count(node->bb1) == 0) {
      create_node_inst(all_inst, inst_map, node->bb1);
      gen_inst(all_inst, inst_map, node->bb1);
    }
    if (inst_map->count(node->bb2) == 0) {
      create_node_inst(all_inst, inst_map, node->bb2);
      gen_inst(all_inst, inst_map, node->bb2);
    }
    code->op1 = (*inst_map->find(node->bb1)).second * 3;
    code->op2 = (*inst_map->find(node->bb2)).second * 3;
  } else
    return;
}

VMFlat::Node *VMFlat::findBBNode(const BasicBlock *bb,
                                 const std::vector<Node *> *all_node) {
  for (const auto &i : *all_node) {
    if (bb == i->data)
      return i;
  }
  return nullptr;
}

void VMFlat::dump_inst(const std::vector<VMInst *> *all_inst) const {
  unsigned int x = 0;
  for (const auto c : *all_inst) {
    printf("0x%02x: ", x++);
    if (c->type == RUN_BLOCK)
      printf("RUN_BLOCK 0x%02x\n", c->op1);
    if (c->type == JMP_BORING)
      printf("JMP_BORING 0x%02x\n", c->op1);
    if (c->type == JMP_SELECT)
      printf("JMP_SELECT 0x%02x 0x%02x\n", c->op1, c->op2);
  }
}

bool VMFlat::DoFlatten(Function *f, int seed) {
  //errs() << f->getName().data() << "\r\n";

  
  std::string new_name_pre = std::to_string(seed);
  std::vector<BasicBlock *> orig_bb;
  getBlocks(f, &orig_bb);
  if (orig_bb.size() <= 1) {
    return false;
  }
  //errs() << "Count 1 = " << orig_bb.size() << "\r\n";
  //  unsigned int rand_val = seed;
  auto tmp = f->begin();
  BasicBlock *old_entry = &*tmp;
  orig_bb.erase(orig_bb.begin());
  BranchInst *first_br = nullptr;
  if (isa<BranchInst>(old_entry->getTerminator())) {
    first_br = cast<BranchInst>(old_entry->getTerminator());
  }
  BasicBlock *firstbb = old_entry->getTerminator()->getSuccessor(0);
  if ((first_br != nullptr && first_br->isConditional()) ||
      old_entry->getTerminator()->getNumSuccessors() >
          2) { // Split the first basic block
    auto iter = old_entry->end();
    --iter;
    if (old_entry->size() > 1) {
      --iter;
    }
    BasicBlock *splited = old_entry->splitBasicBlock(iter, Twine(new_name_pre+"FirstBB"));
    firstbb = splited;
    orig_bb.insert(orig_bb.begin(), splited);
  }
  std::vector<Node *> all_node;
  // unsigned int val=0;
  std::vector<unsigned int> rand_list;
  for (auto &i : orig_bb) {
    unsigned int num = getUniqueNumber(&rand_list);
    rand_list.push_back(num);
    Node *tmp1 = newNode(num);
    all_node.push_back(tmp1);
    tmp1->data = i;
  }

  //errs() << "Count = " << all_node.size() << "\r\n";

  for (auto i = all_node.begin(); i != all_node.end(); ++i) {
    Node *tmp = *i;
    BasicBlock *bb = tmp->data;
    if (bb->getTerminator()->getNumSuccessors() == 2) {
      BasicBlock *bb1 = bb->getTerminator()->getSuccessor(0),
                 *bb2 = bb->getTerminator()->getSuccessor(1);
      Node *n1 = findBBNode(bb1, &all_node), *n2 = findBBNode(bb2, &all_node);
      tmp->bb1 = n1;
      tmp->bb2 = n2;
    } else if (bb->getTerminator()->getNumSuccessors() == 1) {
      BasicBlock *bb1 = bb->getTerminator()->getSuccessor(0);
      Node *n = findBBNode(bb1, &all_node);
      tmp->bb1 = n;
    } else {
      continue;
    }
    // for(std::vector<Node*>::iterator
    // j=all_node.begin();j!=all_node.end();j++)
  }

  Node *start = findBBNode(firstbb, &all_node);
  Node *fake = newNode(0x7FFFFFFF);
  std::vector<VMInst *> all_inst;
  std::map<Node *, unsigned int> inst_map;
  fake->bb1 = start;
  //errs() << "begin gen ins\r\n";
  gen_inst(&all_inst, &inst_map, fake);
  //errs() << "end gen ins\r\n";
  //dump_inst(&all_inst);
  std::vector<Constant *> opcodes;
  [[maybe_unused]] auto type_int32ty = Type::getInt32Ty(f->getContext());
  auto type_int64ty = Type::getInt64Ty(f->getContext());
  for (auto inst : all_inst) {
    opcodes.push_back(ConstantInt::get(type_int64ty, inst->type));
    opcodes.push_back(ConstantInt::get(type_int64ty, inst->op1));
    opcodes.push_back(ConstantInt::get(type_int64ty, inst->op2));
  }
  //errs() << "inst ok\r\n";

  ArrayType *at = ArrayType::get(type_int64ty, opcodes.size());
  Constant *opcode_array =
      ConstantArray::get(at, ArrayRef<Constant *>(opcodes));
  auto oparr_var = new GlobalVariable(*(f->getParent()), at, false,
                                      GlobalValue::LinkageTypes::PrivateLinkage,
                                      opcode_array, new_name_pre+"opcodes");
  // 去除第一个基本块末尾的跳转
  old_entry->getTerminator()->eraseFromParent();
  auto vm_pc = new AllocaInst(type_int64ty, 0, Twine(new_name_pre+"VMpc"), old_entry);
  ConstantInt *init_pc = ConstantInt::get(type_int64ty, 0);
  new StoreInst(init_pc, vm_pc, old_entry);
  auto vm_flag = new AllocaInst(type_int64ty, 0, Twine(new_name_pre+"VMJmpFlag"), old_entry);
  BasicBlock *vm_entry =
      BasicBlock::Create(f->getContext(), Twine(new_name_pre+"VMEntry"), f, firstbb);

  BranchInst::Create(vm_entry, old_entry);
  IRBuilder<> IRB(vm_entry);
  Value *zero = ConstantInt::get(type_int64ty, 0);

  Value *op1_offset =
      IRB.CreateAdd(IRB.CreateLoad(vm_pc->getAllocatedType(), vm_pc),
                    ConstantInt::get(type_int64ty, 1));
  Value *op2_offset =
      IRB.CreateAdd(IRB.CreateLoad(vm_pc->getAllocatedType(), vm_pc),
                    ConstantInt::get(type_int64ty, 2));

  auto optype_gep =
      IRB.CreateGEP(oparr_var->getValueType(), oparr_var,
                    {zero, IRB.CreateLoad(vm_pc->getAllocatedType(), vm_pc)});
  Value *optype = IRB.CreateLoad(optype_gep->getType(), optype_gep);
  auto op1_gep =
      IRB.CreateGEP(oparr_var->getValueType(), oparr_var, {zero, op1_offset});
  Value *op1 = IRB.CreateLoad(op1_gep->getType(), op1_gep);
  auto op2_gep =
      IRB.CreateGEP(oparr_var->getValueType(), oparr_var, {zero, op2_offset});
  Value *op2 = IRB.CreateLoad(op2_gep->getType(), op2_gep);

  IRB.CreateStore(
      IRB.CreateAdd(IRB.CreateLoad(vm_pc->getAllocatedType(), vm_pc),
                    ConstantInt::get(type_int64ty, 3)),
      vm_pc);
  BasicBlock *run_block =
      BasicBlock::Create(f->getContext(), new_name_pre+"RunBlock", f, firstbb);
  BasicBlock *jmp_boring =
      BasicBlock::Create(f->getContext(), new_name_pre+"JmpBoring", f, firstbb);
  BasicBlock *jmp_select =
      BasicBlock::Create(f->getContext(), new_name_pre+"JmpSelect", f, firstbb);
  BasicBlock *defaultCase =
      BasicBlock::Create(f->getContext(), new_name_pre+"Default", f, firstbb);
  BranchInst::Create(vm_entry, defaultCase);
  SwitchInst *switch1 = IRB.CreateSwitch(optype, defaultCase, 0);
  switch1->addCase(ConstantInt::get(type_int64ty, RUN_BLOCK), run_block);
  switch1->addCase(ConstantInt::get(type_int64ty, JMP_BORING), jmp_boring);
  switch1->addCase(ConstantInt::get(type_int64ty, JMP_SELECT), jmp_select);

  // create run_block's basicblock
  // the first choice
  IRB.SetInsertPoint(run_block);
#if 0
    std::vector<Constant *> bb_addrs;
    for (std::vector<BasicBlock *>::iterator b = origBB.begin();
         b != origBB.end(); b++) {
        BasicBlock *block = *b;
        bb_addrs.push_back(BlockAddress::get(block));
    }
    ArrayType *AT_ =
        ArrayType::get(Type::getInt8PtrTy(f->getContext()), bb_addrs.size());
    Constant *addr_array =
        ConstantArray::get(AT_, ArrayRef<Constant *>(bb_addrs));
    GlobalVariable *address_arr_var = new GlobalVariable(
        *(f->getParent()), AT_, false,
        GlobalValue::LinkageTypes::PrivateLinkage, addr_array, "address_table");
    auto load_gep =IRB.CreateGEP(address_arr_var->getValueType(),address_arr_var, {zero, op1});
    Value *load =
        IRB.CreateLoad(load_gep->getType(),load_gep, "address");
    IndirectBrInst *indirBr =
        IndirectBrInst::Create(load, bb_addrs.size(), run_block);
    for (std::vector<BasicBlock *>::iterator b = origBB.begin();
         b != origBB.end(); b++) {
        BasicBlock *block = *b;
        indirBr->addDestination(block);
    }
#endif
  // the seconde choice
  SwitchInst *switch2 = IRB.CreateSwitch(op1, defaultCase, 0);
  //errs() << "the seconde choice start\r\n";
  for (std::vector<BasicBlock *>::iterator b = orig_bb.begin();
       b != orig_bb.end(); b++) {
    BasicBlock *block = *b;
    block->moveBefore(defaultCase);
    Node *t = findBBNode(block, &all_node);
    // ConstantInt *numCase =
    // cast<ConstantInt>(ConstantInt::get(switch2->getCondition()->getType(),
    // t->value));
    switch2->addCase(ConstantInt::get(type_int64ty, t->value), block);
  }
  for (auto block : orig_bb) { // Handle successors
    if (block->getTerminator()->getNumSuccessors() == 1) {
      //  ////errs() << "\033[1;32mThis block has 1 successor\033[0m\n";
      [[maybe_unused]] BasicBlock *succ =
          block->getTerminator()->getSuccessor(0);
      block->getTerminator()->eraseFromParent();
      BranchInst::Create(defaultCase, block);
    } else if (block->getTerminator()->getNumSuccessors() == 2) {
      // ////errs() << "\033[1;32mThis block has 2 successors\033[0m\n";
      auto old_br = cast<BranchInst>(block->getTerminator());
      SelectInst *select = SelectInst::Create(
          old_br->getCondition(), ConstantInt::get(type_int64ty, 1),
          ConstantInt::get(type_int64ty, 0), "", block->getTerminator());
      new StoreInst(select, vm_flag, block->getTerminator());
      block->getTerminator()->eraseFromParent();
      BranchInst::Create(defaultCase, block);
    } else {
      continue;
    }
  }
  //errs() << "the seconde choice end\r\n";
  IRB.SetInsertPoint(jmp_boring);
  IRB.CreateStore(op1, vm_pc);
  IRB.CreateBr(vm_entry);

  IRB.SetInsertPoint(jmp_select);
  BasicBlock *select_true =
      BasicBlock::Create(f->getContext(), new_name_pre+"JmpSelectTrue", f, firstbb);
  BasicBlock *select_false =
      BasicBlock::Create(f->getContext(), new_name_pre+"JmpSelectFalse", f, firstbb);
  IRB.CreateCondBr(
      IRB.CreateICmpEQ(IRB.CreateLoad(vm_flag->getAllocatedType(), vm_flag),
                       ConstantInt::get(type_int64ty, 1)),
      select_true, select_false);
  IRB.SetInsertPoint(select_true);
  IRB.CreateStore(op1, vm_pc);
  IRB.CreateBr(vm_entry);
  IRB.SetInsertPoint(select_false);
  IRB.CreateStore(op2, vm_pc);
  IRB.CreateBr(vm_entry);

  fixStack(*f, false);
  // std::vector<PHINode *> tmpPhi;
  // std::vector<Instruction *> tmpReg;
  // BasicBlock *bbEntry = &*f->begin();
#if 0
   ////errs()<<"the PHI start\r\n";

    //do{
    //  ////errs()<<"Fix Stack\r\n";
    //    tmpPhi.clear();
    //    tmpReg.clear();
    //    for (Function::iterator i = f->begin(); i != f->end(); i++){
    //        for (BasicBlock::iterator j = i->begin(); j != i->end(); j++){
    //            if (isa<PHINode>(j)){
    //                PHINode *phi = cast<PHINode>(j);
    //                tmpPhi.push_back(phi);
    //                continue;
    //            }
    //            if (!(isa<AllocaInst>(j) && j->getParent() == bbEntry) && (valueEscapes(&*j) || j->isUsedOutsideOfBlock(&*i))){
    //                tmpReg.push_back(&*j);
    //                continue;
    //            }
    //        }
    //    }
    //    for (unsigned int i = 0; i < tmpReg.size(); i++){
    //        DemoteRegToStack(*tmpReg.at(i));
    //    }
    //    for (unsigned int i = 0; i < tmpPhi.size(); i++){
    //        DemotePHIToStack(tmpPhi.at(i));
    //    }
    //} while (tmpReg.size() != 0 || tmpPhi.size() != 0);
   ////errs()<<"PHI end\r\n";
#endif
  return true;
}

bool VMFlat::runVmFlaOnFunction(Function &function) {


  if (!((VmObfuProbRate > 0) && (VmObfuProbRate <= 100))) {
    //errs() << "VmFlattenObfuscationPass application basic blocks percentage "
              "-vm_prob=x must be 0 < x <= 100";
    return false;
  }

  if(function.getName().startswith("??")) {
    return false;
  }
  if(function.getName().contains("std@")) {
    return false;
  }
  if(function.hasCXXEH()||function.hasCXXSEH()) {
    return false;
  }

  bool changed = false;
    RUN_BLOCK = cryptoutils->get_uint32_t();
    JMP_BORING = RUN_BLOCK + 1;
    JMP_SELECT = RUN_BLOCK + 2;
    if (static_cast<int32_t>(cryptoutils->get_range(100)) <= VmObfuProbRate) {
      changed |=
          DoFlatten(&function, static_cast<int>(cryptoutils->get_uint32_t()));
    }
  if (changed)
    turnOffOptimization(&function);
  // DoSplit(&function,4);
  return true;
}
} // namespace

PreservedAnalyses VmFlatObfuscationPass::run(Function &F,
                                             FunctionAnalysisManager &AM) {

  if (toObfuscate(RunVmFlatObfuscationPass, &F, "vm-fla")) {
    LowerSwitchPass lower;
    lower.run(F, AM);
    VMFlat vm;
    if (vm.runVmFlaOnFunction(F))
      return PreservedAnalyses::none();
  }
  return PreservedAnalyses::all();
}