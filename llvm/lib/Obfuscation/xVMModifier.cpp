#include "xVMModifier.h"

#include "llvm/IR/IRBuilder.h"

using namespace  llvm;
void VMModifier::run() {

    #ifdef VMMODIFIER_DEBUG
    errs() << "[*] modifying function: \n";
    errs() << "\t" << F->getName().str() << "\n";
    #endif

    // remove old function body
    llvm::GlobalValue::LinkageTypes linkagetype = F->getLinkage();
    F->deleteBody();
    F->setLinkage(linkagetype);

    // create new basicblock
    BasicBlock* ret_bbl = BasicBlock::Create(this->Mod->getContext(), "ret", F);
    BasicBlock* body_bbl = BasicBlock::Create(this->Mod->getContext(), "body", F, ret_bbl);
    IRBuilder<> irbuilder(body_bbl);

    // cannot handle var arg function
    assert(!F->isVarArg());

    // collect all callinst args
    std::vector<std::pair<Value*, int>> args_map;
    int arg_offset = 0;
    // if return not void
    if (!F->getReturnType()->isVoidTy()) {
        arg_offset += modDataLayout->getTypeAllocSize(F->getReturnType());
    }

    for (auto arg = F->arg_begin(); arg != F->arg_end(); arg++) {
        Value *tmparg = &*arg;

        Value *paramPtr = irbuilder.CreateAlloca(tmparg->getType());
        irbuilder.CreateStore(tmparg, paramPtr);
        Value *currvalue = irbuilder.CreateLoad(paramPtr->getType(),paramPtr);

        // insert to args_map
        args_map.push_back(std::pair<Value *, int>(currvalue, arg_offset));
        arg_offset += modDataLayout->getTypeAllocSize(tmparg->getType());

        #ifdef VMMODIFIER_DEBUG
        errs() << "[*] Arg: " << tmparg->getName() << "\n";
        errs() << "\t size: " << modDataLayout->getTypeAllocSize(tmparg->getType()) << "\n\n";
        #endif
    }


    // store args to data_seg->vm_args
    // IRBuilder<> irbuilder(inst);

    // setup global variables
    // store global variables to data_seg
    for (auto p: *gv_value_map) {
        GlobalVariable *gv = p.first;
        int offset = p.second;

        errs() << "[*] storing GlobalVariable: " << *gv << "\t" << offset << "\n";

        // convert pointer to int64
        Value * gv_addr_int = irbuilder.CreatePtrToInt(gv,Type::getInt64Ty(Mod->getContext()));

        // create gep: data_seg + offset
        ConstantInt *Zero = ConstantInt::get(Type::getInt64Ty(Mod->getContext()), 0);
        Value * offset_value = ConstantInt::get(Type::getInt64Ty(Mod->getContext()), offset);
        Value * gepinst = irbuilder.CreateGEP( vm_store->data_seg_type,vm_store->gv_data_seg, {Zero, offset_value}, "");

        // convert gep i8* to i64*
        PointerType * target_ptr_type = PointerType::get(gv_addr_int->getType(), cast<PointerType>(gepinst->getType())->getAddressSpace());
        Value * ptr = irbuilder.CreatePointerCast(gepinst, target_ptr_type);

        // store gv_addr_int to data_seg+offset
        irbuilder.CreateStore(gv_addr_int, ptr);
    }


    // store args to data_seg
    for (auto p:args_map) {
        Value * value = p.first;
        int offset = p.second;

        errs() << "[*] storing value: " << *value << "\t" << offset << "\n";

        // GEP get ptr point to offset
        ConstantInt *Zero = ConstantInt::get(Type::getInt64Ty(F->getContext()), 0);
        Value * const_curr_value_offset = ConstantInt::get(Type::getInt64Ty(F->getContext()), offset);
        Value * gepinst = irbuilder.CreateGEP( vm_store->data_seg_type,vm_store->gv_data_seg, {Zero, const_curr_value_offset}, "");

        // cast gep_ptr to value->type
        PointerType * target_ptr_type = PointerType::get(value->getType(), cast<PointerType>(gepinst->getType())->getAddressSpace());
        Value * ptr = irbuilder.CreatePointerCast(gepinst, target_ptr_type);

        // store value to data_seg+offset
        irbuilder.CreateStore(value, ptr);

    }
    // if value is a ptr type, cast gep also can handle it.

    // store gv_data_seg and gv_code_seg to data_seg_addr, code_seg_addr
    Value * data_seg_ptr2int = irbuilder.CreatePtrToInt(vm_store->gv_data_seg, Type::getInt64Ty(Mod->getContext()));
    irbuilder.CreateStore(data_seg_ptr2int, vm_store->data_seg_addr);
    Value * code_seg_ptr2int = irbuilder.CreatePtrToInt(vm_store->gv_code_seg, Type::getInt64Ty(Mod->getContext()));
    irbuilder.CreateStore(code_seg_ptr2int, vm_store->code_seg_addr);

    // replace callinst callee to VM_interpreter
    // CallInst *resultvalue = CallInst::Create(VM_interpreter, "", inst);
    CallInst *resultvalue = irbuilder.CreateCall(vm_store->vm_interpreter);

    if (!F->getReturnType()->isVoidTy()) {
        unsigned return_value_size = modDataLayout->getTypeAllocSize(F->getReturnType());
        // load return value from data_seg

        // GEP get ptr point to offset
        ConstantInt *Zero = ConstantInt::get(Type::getInt64Ty(F->getContext()), 0);
        Value * gepinst = irbuilder.CreateGEP( vm_store->data_seg_type,vm_store->gv_data_seg, {Zero, Zero}, "");
        // cast gep_ptr to value->type
        PointerType * target_ptr_type = PointerType::get(F->getReturnType(), cast<PointerType>(gepinst->getType())->getAddressSpace());
        Value * ptr = irbuilder.CreatePointerCast(gepinst, target_ptr_type);
        // load return value
        Value * retval = irbuilder.CreateLoad(ptr->getType(),ptr);

        ReturnInst *inst_ret = ReturnInst::Create(this->Mod->getContext(), retval, ret_bbl);
    }
    else {
        ReturnInst *inst_ret = ReturnInst::Create(this->Mod->getContext(), ret_bbl);
    }

    irbuilder.CreateBr(ret_bbl);


    // not support not void return value
    // assert(F->getReturnType()->isVoidTy());

}