#ifndef LLVM_VMP_OBFUSCATION_H
#define LLVM_VMP_OBFUSCATION_H

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/PassManager.h"
//#include <Obfuscation/PassRegistry.h>
// Opcode
#define ALLOCA_OP           0x01
#define LOAD_OP             0x02
#define STORE_OP            0x03
#define BinaryOperator_OP   0x04
#define GEP_OP              0x05
#define CMP_OP              0x06
#define CAST_OP             0x07
#define BR_OP               0x08
#define Call_OP             0x09
#define Ret_OP              0x0A

#define OP_TOTAL            0x0A

// pointer size
#define POINTER_SIZE 8



namespace llvm {
struct vmprotect_store {
  GlobalVariable *gv_code_seg;
  GlobalVariable *gv_data_seg;
  GlobalVariable *ip;
  GlobalVariable *data_seg_addr;
  GlobalVariable *code_seg_addr;
  ArrayType * data_seg_type;
  ArrayType * code_seg_type;
  Function *vm_interpreter;
};

class VmProtectPass : public PassInfoMixin<VmProtectPass> {
public:
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);

  static bool isRequired() { return true; }
};

} // namespace llvm

#endif