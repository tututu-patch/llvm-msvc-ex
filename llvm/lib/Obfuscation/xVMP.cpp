#include "xVMP.h"
#include "CryptoUtils.h"
#include "Utils.h"
#include "xVMInterpreterLLVM.h"
#include "xVMModifier.h"
#include "xVMTranslator.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Transforms/Utils/LowerSwitch.h"

#include <cstdint>
#include <iomanip>
#include <set>
#include <sstream>
#include <vector>

using namespace llvm;
static cl::opt<bool> RunVmProtectPass("vmprotect", cl::init(false),
                                      cl::desc("OLLVM - VmProtectPass"));

namespace llvm {
struct VmProtect {
  vmprotect_store vm_store;
  VmProtect():vm_store({}){}
  bool runOnFunction(Function &F);
};

bool VmProtect::runOnFunction(Function &F) {
  if (F.isVarArg())
    return false;
  if(F.getName().starts_with("vm_interpreter"))
    return false;

  auto translator = new VMTranslator(F,&this->vm_store);
  translator->run();

  auto interpreter = new VMInterpreterLLVM(F, translator->get_callinst_handler(),&this->vm_store);
  interpreter->run();

  auto modifier = new VMModifier(F, translator->get_gv_value_map(),&this->vm_store);
  modifier->run();

  return true;
}
}; // namespace llvm
PreservedAnalyses VmProtectPass::run(Function &F, FunctionAnalysisManager &AM) {

  if (toObfuscate(RunVmProtectPass, &F, "vmprotect")) {
    VmProtect vm;
    if (vm.runOnFunction(F))
    {
        turnOffOptimization(&F);
      return PreservedAnalyses::none();
    }
  }
  return PreservedAnalyses::all();
}
