#include "BogusControlFlow.h"
#include "Flattening.h"
#include "SplitBasicBlock.h"
#include "Substitution.h"
#include "MBAObfuscation.h"
#include "StringObfuscation.h"
#include "IndirectCall.h"
#include "ConstObfuscation.h"
#include "DataObfuscation.h"
#include "VMFlatten.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

using namespace llvm;

llvm::PassPluginLibraryInfo getObfuscationPluginInfo() {
  return {
      LLVM_PLUGIN_API_VERSION, "Obfuscation", LLVM_VERSION_STRING,
      [](PassBuilder &PB) {
        srand(time(nullptr));
        PB.registerPipelineStartEPCallback([](llvm::ModulePassManager &MPM,
                                              OptimizationLevel Level) {
          MPM.addPass(createModuleToFunctionPassAdaptor(SplitBasicBlockPass()));
          MPM.addPass(
              createModuleToFunctionPassAdaptor(BogusControlFlowPass()));
          MPM.addPass(createModuleToFunctionPassAdaptor(FlatteningPass()));
          MPM.addPass(createModuleToFunctionPassAdaptor(MBAObfuscationPass()));
          
          //MPM.addPass(createModuleToFunctionPassAdaptor(IndirectCallPass()));
          MPM.addPass(ConstObfuscationPass());
         
        });
        PB.registerOptimizerEarlyEPCallback([](llvm::ModulePassManager &MPM,
                                              OptimizationLevel Level) {
           MPM.addPass(StringObfuscationPass());
           MPM.addPass(createModuleToFunctionPassAdaptor(DataObfuscationPass()));
           //MPM.addPass(createModuleToFunctionPassAdaptor(VmFlatObfuscationPass()));
            //MPM.addPass(ConstObfuscationPass());
          });

        PB.registerOptimizerLastEPCallback([](llvm::ModulePassManager &MPM,
                                              OptimizationLevel Level) {
          MPM.addPass(createModuleToFunctionPassAdaptor(SubstitutionPass()));
         
          //MPM.addPass(ConstObfuscationPass());
          MPM.addPass(createModuleToFunctionPassAdaptor(IndirectCallPass()));
          MPM.addPass(createModuleToFunctionPassAdaptor(MBAObfuscationPass()));
          //MPM.addPass(createModuleToFunctionPassAdaptor(VmFlatObfuscationPass()));
          //MPM.addPass(createModuleToFunctionPassAdaptor(DataObfuscationPass()));
        });
        PB.registerVectorizerStartEPCallback([](FunctionPassManager& FPM, OptimizationLevel Level) {
                //FPM.addPass(IndirectCallPass());
              //FPM.addPass(MBAObfuscationPass());
            //FPM.addPass(VmFlatObfuscationPass());
                });
      }};
}

#ifndef LLVM_OBFUSCATION_LINK_INTO_TOOLS
extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return getObfuscationPluginInfo();
}
#endif
