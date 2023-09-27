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
#include "llvm/Transforms/Utils/ValueMapper.h"

#include <cstdint>
#include <cstring>
#include <iomanip>
#include <map>
#include <set>
#include <sstream>
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
  std::string new_name_pre;
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
  bool DoFlatten(Function *f);
  //bool DoFlattenEx(Function *f);
  //bool isPHINodeBranchInst(Instruction &insn);
  //// 构建不透明谓词
  //// dst原本是src的后继
  //bool insert_opaque_predicate(BasicBlock *src, BasicBlock *dst);
  ///* createAlteredBasicBlock
  // *
  // * This function return a basic block similar to a given one.
  // * It's inserted just after the given basic block.
  // * The instructions are similar but junk instructions are added between
  // * the cloned one. The cloned instructions' phi nodes, metadatas, uses and
  // * debug locations are adjusted to fit in the cloned basic block and
  // * behave nicely.
  // */
  //BasicBlock *createAlteredBasicBlock(BasicBlock *basicBlock,
  //                                    const Twine &Name = "gen");

  //// 一元二次方程 ax^2 + bx + c = 0有解的前提是b^2 - 4ac > 0
  //// 生成不满足该条件的a,b,c
  //void get_a_b_c(int &a, int &b, int &c);
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

bool VMFlat::DoFlatten(Function *f) {
  // errs() << f->getName().data() << "\r\n";
  if (f->getName().startswith("??")) {
    return false;
  }
  if (f->getName().contains("std@")) {
    return false;
  }
  if (f->hasCXXEH() || f->hasCXXSEH()) {
    return false;
  }
  RUN_BLOCK = cryptoutils->get_uint32_t();
  JMP_BORING = RUN_BLOCK + 1;
  JMP_SELECT = RUN_BLOCK + 2;

  std::vector<BasicBlock *> orig_bb;
  getBlocks(f, &orig_bb);
  if (orig_bb.size() <= 1) {
    return false;
  }
  // errs() << "Count 1 = " << orig_bb.size() << "\r\n";
  //   unsigned int rand_val = seed;
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
    BasicBlock *splited =
        old_entry->splitBasicBlock(iter, Twine(new_name_pre + "FirstBB"));
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

  // errs() << "Count = " << all_node.size() << "\r\n";

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
  // errs() << "begin gen ins\r\n";
  gen_inst(&all_inst, &inst_map, fake);
  // errs() << "end gen ins\r\n";
  // dump_inst(&all_inst);
  std::vector<Constant *> opcodes;
  [[maybe_unused]] auto type_int32ty = Type::getInt32Ty(f->getContext());
  auto type_int64ty = Type::getInt64Ty(f->getContext());
  for (auto inst : all_inst) {
    opcodes.push_back(ConstantInt::get(type_int64ty, inst->type));
    opcodes.push_back(ConstantInt::get(type_int64ty, inst->op1));
    opcodes.push_back(ConstantInt::get(type_int64ty, inst->op2));
  }
  // errs() << "inst ok\r\n";

  ArrayType *at = ArrayType::get(type_int64ty, opcodes.size());
  Constant *opcode_array =
      ConstantArray::get(at, ArrayRef<Constant *>(opcodes));
  auto oparr_var = new GlobalVariable(*(f->getParent()), at, false,
                                      GlobalValue::LinkageTypes::PrivateLinkage,
                                      opcode_array, new_name_pre + "opcodes");
  // 去除第一个基本块末尾的跳转
  old_entry->getTerminator()->eraseFromParent();
  auto vm_pc =
      new AllocaInst(type_int64ty, 0, Twine(new_name_pre + "VMpc"), old_entry);
  ConstantInt *init_pc = ConstantInt::get(type_int64ty, 0);
  new StoreInst(init_pc, vm_pc, old_entry);
  auto vm_flag = new AllocaInst(type_int64ty, 0,
                                Twine(new_name_pre + "VMJmpFlag"), old_entry);
  BasicBlock *vm_entry = BasicBlock::Create(
      f->getContext(), Twine(new_name_pre + "VMEntry"), f, firstbb);

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
  BasicBlock *run_block = BasicBlock::Create(
      f->getContext(), new_name_pre + "RunBlock", f, firstbb);
  BasicBlock *jmp_boring = BasicBlock::Create(
      f->getContext(), new_name_pre + "JmpBoring", f, firstbb);
  BasicBlock *jmp_select = BasicBlock::Create(
      f->getContext(), new_name_pre + "JmpSelect", f, firstbb);
  BasicBlock *defaultCase =
      BasicBlock::Create(f->getContext(), new_name_pre + "Default", f, firstbb);
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
  // errs() << "the seconde choice start\r\n";
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
  // errs() << "the seconde choice end\r\n";
  IRB.SetInsertPoint(jmp_boring);
  IRB.CreateStore(op1, vm_pc);
  IRB.CreateBr(vm_entry);

  IRB.SetInsertPoint(jmp_select);
  BasicBlock *select_true = BasicBlock::Create(
      f->getContext(), new_name_pre + "JmpSelectTrue", f, firstbb);
  BasicBlock *select_false = BasicBlock::Create(
      f->getContext(), new_name_pre + "JmpSelectFalse", f, firstbb);
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

//bool VMFlat::DoFlattenEx(Function *f) {
//
//  if (f->isDeclaration()) {
//    return false;
//  }
//
//  // Check external linkage
//  if (f->hasAvailableExternallyLinkage() != 0) {
//    return false;
//  }
//
//  errs() << f->getName().data() << "\r\n";
//
//
//  BasicBlock *fn_new_entry_bb =
//      BasicBlock::Create(f->getContext(), Twine(new_name_pre + "fn_entry"), f,
//                         &f->getEntryBlock());
//
//  std::vector<BasicBlock *> toearse_bbs;
//  int count = 0;
//  for (BasicBlock &origin_bb : *f) {
//    std::string name = new_name_pre +"OriginBB" + std::to_string(count++);
//    origin_bb.setName(name);
//    errs() << origin_bb << "\n";
//    if (!origin_bb.empty()) {
//      // PHI NODE肯定位于block的第一条件指令
//      Instruction &insn = *(origin_bb.begin());
//      if (isa<PHINode>(insn)) {
//        errs() << "Processing bb has PHINODE\n";
//        continue;
//      }
//
//      if (origin_bb.size() <= 2) {
//        errs() << "BB size less than 2\n";
//        continue;
//      }
//    }
//
//    // exception handler block不处理
//    if (origin_bb.empty() || origin_bb.isEHPad())
//      continue;
//
//    LLVMContext &context = origin_bb.getContext();
//    IRBuilder<> builder(context);
//
//    // VMInterpreterBody
//    BasicBlock *VMInterpreterbody_bb =
//        BasicBlock::Create(context, "", f, &origin_bb);
//    // VMInterpreter
//    BasicBlock *VMInterpreter_bb =
//        BasicBlock::Create(context, "", f, VMInterpreterbody_bb);
//    // 先创建初始化向量表的block
//    BasicBlock *entry_bb =
//        BasicBlock::Create(context, "", f, VMInterpreter_bb);
//
//    std::vector<BasicBlock *> handlerbb_list;
//    // PC向量表
//    std::vector<ConstantInt *> switch_elems;
//    std::vector<Constant *> const_array_elems;
//    // 为解决变量生命周期问题，为每一条指令都申请一个变量
//    std::vector<Value *> var_declare;
//    size_t split_bb_num = 0;
//
//    while (!origin_bb.empty()) {
//      auto first_insn = origin_bb.begin();
//      unsigned int insn_opcode = first_insn->getOpcode();
//      if (insn_opcode == Instruction::Alloca) // 变量声明不混淆，放在entry
//      {
//        fn_new_entry_bb->getInstList().splice(
//            fn_new_entry_bb->end(), origin_bb.getInstList(), first_insn);
//        errs()<<"found Alloca"<<"\r\n";
//        continue;
//      }
//
//      // 对于跳转到PHINODE的指令，不切割成一个单独的bb，放到前一个指令的bb
//      if (isPHINodeBranchInst(*first_insn)) {
//        BasicBlock *bb = *handlerbb_list.rbegin();
//        // 移除上一次添加的br
//        bb->getTerminator()->eraseFromParent();
//        bb->getInstList().splice(bb->end(), origin_bb.getInstList(),
//                                 first_insn);
//        bb->replaceSuccessorsPhiUsesWith(&origin_bb, bb);
//      } else {
//        ++split_bb_num;
//        BasicBlock *new_bb =
//            BasicBlock::Create(context, "", f, &origin_bb);
//        new_bb->getInstList().splice(new_bb->end(), origin_bb.getInstList(),
//                                     first_insn);
//
//        if (!new_bb->begin()->isTerminator()) {
//          builder.SetInsertPoint(new_bb, new_bb->end());
//          builder.CreateBr(VMInterpreterbody_bb);
//        }
//        // else
//        //{
//        //     new_bb->replaceSuccessorsPhiUsesWith(&originBB, new_bb);
//        // }
//        int code = cryptoutils->get_uint32_t();
//        switch_elems.push_back(
//            ConstantInt::get(Type::getInt32Ty(context), code));
//        const_array_elems.push_back(
//            ConstantInt::get(Type::getInt32Ty(context), code));
//        handlerbb_list.push_back(new_bb);
//      }
//    }
//
//    for (size_t i = 0; i < handlerbb_list.size(); ++i) {
//      BasicBlock *bb = handlerbb_list[i];
//      for (Instruction &insn : *bb) {
//        //获取指令返回值
//        Value *returnval = llvm::cast<Value>(&insn);
//        // 指令返回值下面有引用
//        if (returnval->hasNUsesOrMore(1)) {
//          std::vector<BasicBlock *> returnval_users;
//          for (auto user : returnval->users()) {
//            // 找到引用此变量的指令
//            Instruction *insn = llvm::cast<Instruction>(user);
//            // 如果该指令不是PHINODE
//            if (!isa<PHINode>(*insn)) {
//              BasicBlock *that_bb = insn->getParent();
//              // 找出不在当前bb的引用
//              if (that_bb != bb) {
//                returnval_users.push_back(that_bb);
//              }
//            }
//          }
//
//          if (!returnval_users.empty()) {
//            // 在entry新声明一个变量
//            builder.SetInsertPoint(fn_new_entry_bb, fn_new_entry_bb->end());
//            errs()<<returnval->getType()->getTypeID()<<"\r\n";
//            Value *tmpPtr = builder.CreateAlloca(returnval->getType());
//            // 在new_bb中对此变量赋值, 并将该指令返回值的所有使用处替换为该变量
//            BasicBlock::iterator p = bb->end();
//            --p;
//            builder.SetInsertPoint(bb, p);
//            builder.CreateStore(returnval, tmpPtr);
//
//            for (BasicBlock *ele_bb : returnval_users) {
//              builder.SetInsertPoint(ele_bb, ele_bb->begin());
//              Value *replace = builder.CreateLoad(tmpPtr->getType(), tmpPtr);
//
//              // 获取ele_bb的位置
//              int ele_bb_id = -1;
//              for (size_t j = 0; j < handlerbb_list.size(); ++j) {
//                if (handlerbb_list[j] == ele_bb) {
//                  ele_bb_id = j;
//                  break;
//                }
//              }
//
//              returnval->replaceUsesWithIf(
//                  replace, [handlerbb_list, ele_bb_id](Use &U) {
//                    auto *I = dyn_cast<Instruction>(U.getUser());
//                    if (I == nullptr)
//                      return true;
//                    // 仅替换当前bb后面bb引用的变量，否则产生BUG!!
//                    for (size_t j = ele_bb_id;
//                         ele_bb_id > 0 && j < handlerbb_list.size(); ++j) {
//                      if (handlerbb_list[j] == I->getParent()) {
//                        return true;
//                      }
//                    }
//                    return false;
//                  });
//            }
//          }
//        }
//
//        // 每次循环都把所有的block打印一遍
//        /*
//        errs() << "=======================================\n";
//        for(size_t j = 0; j < handlerbb_list.size(); ++j)
//        {
//            errs() << * handlerbb_list[j] << "\n";
//        }
//        errs() << "+++++++++++++++++++++++++++++++++++++++\n";
//        */
//      }
//    }
//
//    toearse_bbs.push_back(&origin_bb);
//
//    ArrayType *array_type =
//        ArrayType::get(Type::getInt32Ty(context), split_bb_num);
//    GlobalVariable *opcodes = new GlobalVariable(
//        *f->getParent(),
//        /*Type=*/array_type,
//        /*isConstant=*/true,
//        /*Linkage=*/GlobalValue::PrivateLinkage,
//        /*Initializer=*/0, // has initializer, specified below
//        /*Name=*/Twine(new_name_pre + "opcodes"));
//    opcodes->setAlignment(MaybeAlign(4));
//    opcodes->setInitializer(ConstantArray::get(array_type, const_array_elems));
//    //errs() << *opcodes << "\n";
//
//    // alloca集中放在入口
//    builder.SetInsertPoint(fn_new_entry_bb, fn_new_entry_bb->end());
//    Value *opcodesPtr = builder.CreateAlloca(
//        Type::getInt32Ty(context)->getPointerTo(), nullptr, Twine(new_name_pre + "opcodesPtr"));
//    Value *i_alloc =
//        builder.CreateAlloca(Type::getInt32Ty(context), nullptr, Twine(new_name_pre + "i_alloc"));
//
//    // entry
//    builder.SetInsertPoint(entry_bb, entry_bb->end());
//    Value *opcodesGVCast = builder.CreateBitCast(
//        opcodes, Type::getInt32Ty(context)->getPointerTo(), Twine(new_name_pre + "opcodesGVCast"));
//    builder.CreateStore(opcodesGVCast, opcodesPtr);
//    builder.CreateBr(VMInterpreter_bb);
//    // 替换originBB前驱后继为entry_bb
//    origin_bb.replaceAllUsesWith(entry_bb);
//
//    // VMInterpreter
//    builder.SetInsertPoint(VMInterpreter_bb);
//    // 创建变量i并创始化为0
//    Value *con0 = ConstantInt::get(Type::getInt32Ty(context), 0);
//    builder.CreateStore(con0, i_alloc);
//    builder.CreateBr(VMInterpreterbody_bb);
//
//    // VMInterperterBody
//    builder.SetInsertPoint(VMInterpreterbody_bb);
//    Value *loaded_i =
//        builder.CreateLoad(Type::getInt32Ty(context), i_alloc, Twine(new_name_pre + "load_i"));
//    Value *con1 = ConstantInt::get(Type::getInt32Ty(context), 1);
//    Value *increased_i = builder.CreateAdd(loaded_i, con1, Twine(new_name_pre + "increased_i"));
//    builder.CreateStore(increased_i, i_alloc);
//    Value *loadedOpcodePtr = builder.CreateLoad(opcodesPtr->getType(),
//                                                opcodesPtr, Twine(new_name_pre + "loadedOpcodePtr"));
//    Value *opcodesIdx = builder.CreateGEP(
//        Type::getInt32Ty(context), loadedOpcodePtr, loaded_i, Twine(new_name_pre + "opcodesIdx"));
//    Value *loadedOpcode =
//        builder.CreateLoad(opcodesIdx->getType(), opcodesIdx, Twine(new_name_pre + "loadedOpcode"));
//    // 创建switch语句
//    SwitchInst *switch_inst =
//        builder.CreateSwitch(loadedOpcode, VMInterpreterbody_bb, split_bb_num);
//    for (size_t i = 0; i < split_bb_num; ++i) {
//      switch_inst->addCase(switch_elems[i], handlerbb_list[i]);
//    }
//
//    // errs() << *entry_bb << "\n";
//    // errs() << *VMInterpreter_bb << "\n";
//    // errs() << *VMInterpreterbody_bb << "\n";
//  }
//
//  for (auto &bb : toearse_bbs) {
//    bb->eraseFromParent();
//  }
//
//  // 将新entry串进去
//  {
//    IRBuilder<> builder(f->getContext());
//    builder.SetInsertPoint(fn_new_entry_bb, fn_new_entry_bb->end());
//    builder.CreateBr(fn_new_entry_bb->getNextNode());
//  }
//
//  std::vector<BasicBlock *> all_bbs;
//  for (BasicBlock &bb : *f) {
//    all_bbs.push_back(&bb);
//  }
//
//  for (auto *bb : all_bbs) {
//    if (cryptoutils->get_uint8_t() % 2 == 0) {
//      if (bb->getTerminator()->getNumSuccessors() == 1) {
//        insert_opaque_predicate(bb, bb->getSingleSuccessor());
//      }
//    }
//  }
//
//  return true;
//}
//
//bool VMFlat::isPHINodeBranchInst(Instruction &insn) {
//  if (isa<BranchInst>(insn)) {
//    const auto bran_inst = cast<BranchInst>(&insn);
//    for (auto *succ : bran_inst->successors()) {
//      auto first_insn = succ->begin();
//      if (isa<PHINode>(*first_insn))
//        return true;
//    }
//  }
//  return false;
//}
//
//bool VMFlat::insert_opaque_predicate(BasicBlock *src, BasicBlock *dst) {
//  if (src == nullptr || dst == nullptr || dst->empty())
//    return false;
//
//  auto *terminator = src->getTerminator();
//  if (isa<BranchInst>(terminator)) {
//    if (const auto inst = cast<BranchInst>(terminator); inst->isConditional()) {
//      return false;
//    }
//  }
//
//  // dst如果是phinode也不插
//  if (const auto first = dst->begin(); isa<PHINode>(&(*first))) {
//    return false;
//  }
//
//  LLVMContext &context = src->getContext();
//  IRBuilder<> builder(context);
//
//  // 清除结尾br
//  terminator->eraseFromParent();
//  // 创建junk bb
//  BasicBlock *junk_bb = createAlteredBasicBlock(dst);
//
//  // 构造不透明谓词
//  builder.SetInsertPoint(src, src->end());
//  int a, b, c;
//  get_a_b_c(a, b, c);
//  Value *con_a = ConstantInt::get(Type::getInt32Ty(context), a);
//  Value *con_b = ConstantInt::get(Type::getInt32Ty(context), b);
//  Value *con_c = ConstantInt::get(Type::getInt32Ty(context), c);
//  Value *con_0 = ConstantInt::get(Type::getInt32Ty(context), 0);
//  // a*x^2+b*x+c==0
//  Value *x = builder.CreateAlloca(Type::getInt32Ty(context));
//  builder.CreateLifetimeStart(x);
//  Value *x_load = builder.CreateLoad(Type::getInt32Ty(context), x);
//  Value *xx = builder.CreateMul(x_load, x_load);
//  Value *axx = builder.CreateMul(xx, con_a);
//  Value *bx = builder.CreateMul(x_load, con_b);
//  Value *add1 = builder.CreateAdd(axx, bx);
//  Value *add2 = builder.CreateAdd(add1, con_c);
//
//  Value *cmp = builder.CreateCmp(CmpInst::Predicate::ICMP_NE, add2, con_0);
//  builder.CreateLifetimeEnd(x);
//  builder.CreateCondBr(cmp, dst, junk_bb);
//  return true;
//}
//
//BasicBlock *VMFlat::createAlteredBasicBlock(BasicBlock *basicBlock,
//                                            const Twine &Name) {
//  // Useful to remap the informations concerning instructions.
//  ValueToValueMapTy VMap;
//  BasicBlock *alteredBB = BasicBlock::Create(
//      basicBlock->getContext(), "", basicBlock->getParent(), basicBlock);
//  alteredBB->moveAfter(basicBlock);
//
//  for (auto &insn : *basicBlock) {
//    Instruction *clone_insn = insn.clone();
//    alteredBB->getInstList().push_back(clone_insn);
//    VMap[&insn] = clone_insn;
//  }
//
//  // Remap operands.
//  auto ji = basicBlock->begin();
//  for (auto i = alteredBB->begin(), e = alteredBB->end();
//       i != e; ++i) {
//    // Loop over the operands of the instruction
//    for (User::op_iterator opi = i->op_begin(), ope = i->op_end(); opi != ope;
//         ++opi) {
//      // get the value for the operand
//      Value *v = MapValue(*opi, VMap, RF_None, nullptr);
//      if (v != nullptr) {
//        *opi = v;
//      }
//    }
//    // Remap phi nodes' incoming blocks.
//    if (PHINode *pn = dyn_cast<PHINode>(i)) {
//      for (unsigned j = 0, e = pn->getNumIncomingValues(); j != e; ++j) {
//        Value *v = MapValue(pn->getIncomingBlock(j), VMap, RF_None, nullptr);
//        if (v != nullptr) {
//          pn->setIncomingBlock(j, cast<BasicBlock>(v));
//        }
//      }
//    }
//    // Remap attached metadata.
//    SmallVector<std::pair<unsigned, MDNode *>, 4> MDs;
//    i->getAllMetadata(MDs);
//    // important for compiling with DWARF, using option -g.
//    i->setDebugLoc(ji->getDebugLoc());
//    ji++;
//  } // The instructions' informations are now all correct
//
//  // add random instruction in the middle of the bloc. This part can be improve
//  for (auto i = alteredBB->begin(), e = alteredBB->end();
//       i != e; ++i) {
//    // in the case we find binary operator, we modify slightly this part by
//    // randomly insert some instructions
//    if (i->isBinaryOp()) { // binary instructions
//      const unsigned opcode = i->getOpcode();
//      BinaryOperator *op, *op1 = NULL;
//      const Twine *var = new Twine("_");
//      // treat differently float or int
//      // Binary int
//      if (opcode == Instruction::Add || opcode == Instruction::Sub ||
//          opcode == Instruction::Mul || opcode == Instruction::UDiv ||
//          opcode == Instruction::SDiv || opcode == Instruction::URem ||
//          opcode == Instruction::SRem || opcode == Instruction::Shl ||
//          opcode == Instruction::LShr || opcode == Instruction::AShr ||
//          opcode == Instruction::And || opcode == Instruction::Or ||
//          opcode == Instruction::Xor) {
//        for (auto random = cryptoutils->get_uint32_t() % 10; random < 10; ++random) {
//          switch (cryptoutils->get_uint32_t() % 4) { // to improve
//          case 0:                       // do nothing
//            break;
//          case 1:
//            op = BinaryOperator::CreateNeg(i->getOperand(0), *var, &*i);
//            op1 = BinaryOperator::Create(Instruction::Add, op, i->getOperand(1),
//                                         "gen", &*i);
//            break;
//          case 2:
//            op1 = BinaryOperator::Create(Instruction::Sub, i->getOperand(0),
//                                         i->getOperand(1), *var, &*i);
//            op = BinaryOperator::Create(Instruction::Mul, op1, i->getOperand(1),
//                                        "gen", &*i);
//            break;
//          case 3:
//            op = BinaryOperator::Create(Instruction::Shl, i->getOperand(0),
//                                        i->getOperand(1), *var, &*i);
//            break;
//          }
//        }
//      }
//      // Binary float
//      if (opcode == Instruction::FAdd || opcode == Instruction::FSub ||
//          opcode == Instruction::FMul || opcode == Instruction::FDiv ||
//          opcode == Instruction::FRem) {
//        for (int random = cryptoutils->get_uint32_t() % 10; random < 10; ++random) {
//          switch (cryptoutils->get_uint32_t() % 3) { // can be improved
//          case 0:                       // do nothing
//            break;
//          case 1:
//            op = BinaryOperator::CreateFDiv(i->getOperand(0), i->getOperand(1),
//                                            *var, &*i);
//            op1 = BinaryOperator::Create(Instruction::FAdd, op,
//                                         i->getOperand(1), "gen", &*i);
//            break;
//          case 2:
//            op = BinaryOperator::Create(Instruction::FSub, i->getOperand(0),
//                                        i->getOperand(1), *var, &*i);
//            op1 = BinaryOperator::Create(Instruction::FMul, op,
//                                         i->getOperand(1), "gen", &*i);
//            break;
//          }
//        }
//      }
//      if (opcode == Instruction::ICmp) { // Condition (with int)
//        const auto currentI = reinterpret_cast<ICmpInst *>(&i);
//        switch (cryptoutils->get_uint32_t() % 3) { // must be improved
//        case 0:                       // do nothing
//          break;
//        case 1:
//          currentI->swapOperands();
//          break;
//        case 2: // randomly change the predicate
//          switch (cryptoutils->get_uint32_t() % 10) {
//          case 0:
//            currentI->setPredicate(ICmpInst::ICMP_EQ);
//            break; // equal
//          case 1:
//            currentI->setPredicate(ICmpInst::ICMP_NE);
//            break; // not equal
//          case 2:
//            currentI->setPredicate(ICmpInst::ICMP_UGT);
//            break; // unsigned greater than
//          case 3:
//            currentI->setPredicate(ICmpInst::ICMP_UGE);
//            break; // unsigned greater or equal
//          case 4:
//            currentI->setPredicate(ICmpInst::ICMP_ULT);
//            break; // unsigned less than
//          case 5:
//            currentI->setPredicate(ICmpInst::ICMP_ULE);
//            break; // unsigned less or equal
//          case 6:
//            currentI->setPredicate(ICmpInst::ICMP_SGT);
//            break; // signed greater than
//          case 7:
//            currentI->setPredicate(ICmpInst::ICMP_SGE);
//            break; // signed greater or equal
//          case 8:
//            currentI->setPredicate(ICmpInst::ICMP_SLT);
//            break; // signed less than
//          case 9:
//            currentI->setPredicate(ICmpInst::ICMP_SLE);
//            break; // signed less or equal
//          }
//          break;
//        }
//      }
//      if (opcode == Instruction::FCmp) { // Conditions (with float)
//        const auto currentI = reinterpret_cast<FCmpInst *>(&i);
//        switch (cryptoutils->get_uint32_t() % 3) { // must be improved
//        case 0:                       // do nothing
//          break;
//        case 1:
//          currentI->swapOperands();
//          break;
//        case 2: // randomly change the predicate
//          switch (cryptoutils->get_uint32_t() % 10) {
//          case 0:
//            currentI->setPredicate(FCmpInst::FCMP_OEQ);
//            break; // ordered and equal
//          case 1:
//            currentI->setPredicate(FCmpInst::FCMP_ONE);
//            break; // ordered and operands are unequal
//          case 2:
//            currentI->setPredicate(FCmpInst::FCMP_UGT);
//            break; // unordered or greater than
//          case 3:
//            currentI->setPredicate(FCmpInst::FCMP_UGE);
//            break; // unordered, or greater than, or equal
//          case 4:
//            currentI->setPredicate(FCmpInst::FCMP_ULT);
//            break; // unordered or less than
//          case 5:
//            currentI->setPredicate(FCmpInst::FCMP_ULE);
//            break; // unordered, or less than, or equal
//          case 6:
//            currentI->setPredicate(FCmpInst::FCMP_OGT);
//            break; // ordered and greater than
//          case 7:
//            currentI->setPredicate(FCmpInst::FCMP_OGE);
//            break; // ordered and greater than or equal
//          case 8:
//            currentI->setPredicate(FCmpInst::FCMP_OLT);
//            break; // ordered and less than
//          case 9:
//            currentI->setPredicate(FCmpInst::FCMP_OLE);
//            break; // ordered or less than, or equal
//          }
//          break;
//        }
//      }
//    }
//  }
//  return alteredBB;
//}
//
//void VMFlat::get_a_b_c(int &a, int &b, int &c) {
//  b = static_cast<uint16_t>(cryptoutils->get_uint32_t()&0xFFFF);
//  const long bb = b * b;
//  long ac4;
//  do {
//    a = static_cast<uint16_t>(cryptoutils->get_uint32_t()&0xFFFF);
//    c = static_cast<uint16_t>(cryptoutils->get_uint32_t()&0xFFFF);
//    ac4 = 4 * a * c;
//  } while (bb >= ac4);
//}

bool VMFlat::runVmFlaOnFunction(Function &function) {

  if (!((VmObfuProbRate > 0) && (VmObfuProbRate <= 100))) {
    // errs() << "VmFlattenObfuscationPass application basic blocks percentage "
    "-vm_prob=x must be 0 < x <= 100";
    return false;
  }
  new_name_pre = std::to_string(cryptoutils->get_uint32_t());
  bool changed = false;
  if (static_cast<int32_t>(cryptoutils->get_range(100)) <= VmObfuProbRate) {
    changed = DoFlatten(&function);
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