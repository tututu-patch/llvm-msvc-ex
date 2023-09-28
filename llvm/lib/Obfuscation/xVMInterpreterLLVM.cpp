#include "xVMInterpreterLLVM.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/ValueMapper.h"

#include <set>
using  namespace  llvm;

#define IS_INLINE_FUNC

// memcpy functions: for points to and taint propagation.
const std::set<std::string> interpreter_function_names{
#ifndef IS_INLINE_FUNC
                                                        "xorshift32",
                                                        "get_byte_code",
                                                        "get_xorshift_seed",
                                                        "unpack_code",
                                                        "unpack_data",
                                                        "unpack_addr",
                                                        "pack_store_addr",
                                                        "get_value_with_size",
                                                        "get_value",
                                                        "alloca_handler",
                                                        "load_handler",
                                                        "store_handler",
                                                        "binaryOperator_handler",
                                                        "gep_handler",
                                                        "cmp_handler",
                                                        "cast_handler",
                                                        "br_handler",
                                                        "return_handler",
                                                        "get_opcode",
#endif
                                                        "vm_interpreter",
                                                        "vm_interpreter_callinst_dispatch"      // only for check annotation

                                                        };



// check targetfunction is c-implement functions
        bool is_interpreter_function(Function *targetFunction) {
            if(!targetFunction->isDeclaration() && targetFunction->hasName()) {
                std::string func_name = targetFunction->getName().str();
                // errs() << "is_interpreter_function: " << func_name << "\n";
                for (const std::string &curr_func: interpreter_function_names) {
                    if (func_name.find(curr_func.c_str()) != std::string::npos) {
                    // if (targetFunction->getName().str() == curr_func.c_str()) {
                        return true;
                    }
                }
            }
            return false;
        }
        std::string get_vm_function_name(Function *targetFunction) {
            if(!targetFunction->isDeclaration() && targetFunction->hasName()) {
                std::string func_name = targetFunction->getName().str();
                // errs() << "is_interpreter_function: " << func_name << "\n";
                for (const std::string &curr_func: interpreter_function_names) {
                    size_t pos = func_name.find(curr_func.c_str());
                    if (pos != std::string::npos) {
                        if (func_name.length() <= pos+curr_func.length()) {
                            // just vm function self
                            return func_name;
                        } else {
                            return func_name.substr(pos+curr_func.length()+1);
                        }
                    }
                }
            }
            return "";
        }


/* ********************************************************************
*   VMInterpreterLLVM_CPP
***********************************************************************
*/

void VMInterpreterLLVM::construct_gv() {
    // opcode_xorshift32_state      32bit
    Constant *opcode_xorshift32_state_initGV = ConstantInt::get(Type::getInt32Ty(Mod->getContext()), 0);
    opcode_xorshift32_state = new GlobalVariable(*Mod, Type::getInt32Ty(Mod->getContext()),
                false,  GlobalValue::InternalLinkage,
                opcode_xorshift32_state_initGV, "opcode_xorshift32_state_"+F->getName());

    // vm_code_state                32bit
    Constant *vm_code_state_initGV = ConstantInt::get(Type::getInt32Ty(Mod->getContext()), 0);
    vm_code_state = new GlobalVariable(*Mod, Type::getInt32Ty(Mod->getContext()),
                false,  GlobalValue::InternalLinkage,
                vm_code_state_initGV, "vm_code_state_"+F->getName());
}

// Function *govm_interpreter;

void VMInterpreterLLVM::run() {

    // Module *interpreter_module = llvm_parse_bitcode();
    Module *interpreter_module = llvm_parse_bitcode_from_string();
    assert(interpreter_module);
    // interpreter_module->dump();

    // replace GlobalVariable
    std::vector<std::string> gv_list = {"ip",  "data_seg_addr", "code_seg_addr", "opcode_xorshift32_state", "vm_code_state"};
    std::vector<GlobalVariable *> new_gv_list = {vm_store->ip,  vm_store->data_seg_addr, vm_store->code_seg_addr, opcode_xorshift32_state, vm_code_state};
    for (unsigned i = 0; i < gv_list.size(); i++) {
        errs() << "[*] Replacing GlobalVariable: " << *new_gv_list[i] << "\n";
        GlobalVariable *old_gv = interpreter_module->getGlobalVariable(gv_list[i]);
        // GlobalVariable *new_gv = Mod->getGlobalVariable(name);
        GlobalVariable *new_gv = new_gv_list[i];

        errs() << "old_gv->getType(): " << *old_gv->getType() << "\n";
        errs() << "new_gv->getType(): " << *new_gv->getType() << "\n";

        old_gv->replaceAllUsesWith(new_gv);
    }

    // replace call_handler
    Function *old_func = interpreter_module->getFunction("call_handler");
    errs() << "[*] Replacing function: " << old_func->getName().str() << "\n";
    old_func->replaceAllUsesWith(callinst_handler);



    // clone functions
    for(auto Func = interpreter_module->begin();Func!=interpreter_module->end();++Func)
        {

            Function *fun = &*Func;

            // if(F->getName().str() == "unpack_code"){
            if(is_interpreter_function(fun)) {
                FunctionCallee tmp = Mod->getOrInsertFunction(fun->getName(), fun->getFunctionType());
                Function *NewF = cast<Function>(tmp.getCallee());
                NewF->setLinkage(llvm::GlobalValue::LinkageTypes::InternalLinkage);


                // setup VMap
                ValueToValueMapTy VMap;
                SmallVector<ReturnInst*, 8> returns;

                Function::arg_iterator DestI = NewF->arg_begin();

                for (const Argument & I : fun->args())
                    if (VMap.count(&I) == 0) {     // Is this argument preserved?
                        DestI->setName(I.getName()); // Copy the name over...
                        VMap[&I] = &*DestI++;        // Add mapping to VMap
                    }



                CloneFunctionInto(NewF, fun, VMap, CloneFunctionChangeType::LocalChangesOnly, returns);

                // set a new name
                NewF->setName(fun->getName()+"_"+F->getName());

                errs() << "[*] Function: " << fun->getName().str() << " Clone finished!\n";


                // collect all references
                std::vector<CallInst *> F_users;
                for (User *U : fun->users()) {
                    if (CallInst *CI = dyn_cast<CallInst>(U)) {
                        F_users.push_back(CI);
                    }
                }

                // replace references
                for (CallInst *CI: F_users) {
                    errs() << "[*] Replacing references: " << *CI << "\n";
                    CI->setCalledFunction(NewF);
                }

            }


        }


    // remove all function of interpreter_module
    while(true) {
        bool flag = true;
        for(auto Func = interpreter_module->begin(), Funcend = interpreter_module->end();Func!=Funcend;++Func) {

            Function *fun = dyn_cast<Function>(&*Func);

            if(fun->use_empty()) {
                errs() << "[*] Removing function: " << fun->getName().str() << "\n";
                flag = false;
                fun->eraseFromParent();
                break;
            }
        }
        if (flag)
            break;
    }


    vm_store->vm_interpreter = Mod->getFunction("vm_interpreter_"+F->getName().str());

}