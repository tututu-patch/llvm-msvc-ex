#ifndef LLVM_INDGV_OBFUSCATION_H
#define LLVM_INDGV_OBFUSCATION_H

#include "llvm/IR/Constants.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/PassManager.h"

#include <map>
//#include <Obfuscation/PassRegistry.h>

namespace llvm {

struct IndirectGlobalVariable {
  std::map<GlobalVariable *, unsigned> GVNumbering;
  std::vector<GlobalVariable *> GlobalVariables;
  void NumberGlobalVariable(Function & F);
  GlobalVariable * get_indirect_global_variables(Function &F, ConstantInt *EncKey);
  bool runOnFunction(Function &F);
};

class IngvObfuscationPass : public PassInfoMixin<IngvObfuscationPass> {
public:
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);

  static bool isRequired() { return true; }
};

} // namespace llvm

#endif