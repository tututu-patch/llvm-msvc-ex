#ifndef LLVM_CONST_OBFUSCATION_H
#define LLVM_CONST_OBFUSCATION_H

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/PassManager.h"
//#include <Obfuscation/PassRegistry.h>

namespace llvm {

class ConstObfuscationPass : public PassInfoMixin<ConstObfuscationPass> {
public:
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);

  static bool isRequired() { return true; }
};

} // namespace llvm

#endif