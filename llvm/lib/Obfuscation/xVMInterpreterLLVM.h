#ifndef LLVM_VMInterpreterLLVM_OBFUSCATION_H
#define LLVM_VMInterpreterLLVM_OBFUSCATION_H

#include "xVMP.h"

#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"

namespace llvm {
class VMInterpreterLLVM {

public:
  VMInterpreterLLVM(Function &F, Function *callinst_handler,
                vmprotect_store *store) {
    this->Mod = F.getParent();
    this->F = &F;
    this->modDataLayout = new DataLayout(this->Mod);
    this->callinst_handler = callinst_handler;
    this->vm_store = store;
    construct_gv();
  }

  Module *Mod;
  Function *F;
  DataLayout *modDataLayout;

  Function *callinst_handler;
  vmprotect_store *vm_store;
  GlobalVariable *opcode_xorshift32_state;
  GlobalVariable *vm_code_state;

  virtual void run();
  virtual void construct_gv();

  Module *llvm_parse_bitcode_from_string() {
    binary_ir.resize(binary_ir_length);
    int binary_ir_idx = 0;
    for (auto s : binary_ir_vector) {
      for (int i = 0; i < s.size(); i++) {
        binary_ir[binary_ir_idx++] = s[i];
      }
    }

    StringRef str_ref(binary_ir);
    MemoryBufferRef buf_ref = MemoryBufferRef(str_ref, str_ref);

    SMDiagnostic Err;
    LLVMContext *LLVMCtx = &Mod->getContext();
    std::unique_ptr<Module> M = parseIR(buf_ref, Err, *LLVMCtx);
    return M.release();
  }

  Module *llvm_parse_bitcode() {
    SMDiagnostic Err;
    // LLVMContext *LLVMCtx = new LLVMContext();
    LLVMContext *LLVMCtx = &Mod->getContext(); // match Mod context
    std::unique_ptr<Module> M =
        parseIRFile("../c-implement/govm.bc", Err, *LLVMCtx);
    return M.release();
  }
#include "vm.h"
};
} // namespace llvm
#endif
