#ifndef LLVM_VMPROTECT_OBFUSCATION_H
#define LLVM_VMPROTECT_OBFUSCATION_H

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/PassManager.h"
//#include <Obfuscation/PassRegistry.h>

namespace llvm {

class xvmPass : public PassInfoMixin<xvmPass> {
public:
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);

  static bool isRequired() { return true; }
};

} // namespace llvm

#endif