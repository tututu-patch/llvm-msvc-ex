#ifndef LLVM_COMBINE_FUNCTION_OBFUSCATION_H
#define LLVM_COMBINE_FUNCTION_OBFUSCATION_H

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/PassManager.h"


namespace llvm {

class CombineFunctionsPass : public PassInfoMixin<CombineFunctionsPass> {
public:
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);

  static bool isRequired() { return true; }
};

} // namespace llvm

#endif 