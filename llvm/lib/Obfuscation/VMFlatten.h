  /*
  Entry:
......
alloca vmpc
alloca vmflag
br VMEntry


VMEntry:
optype=load addr
tmp1=vmpc+1
addr1=GEP opcode tmp1
op1=load addr1
tmp2=vmpc+2
addr2=GEP opcode tmp2
op2=load addr2
vmpct=vmpc+3
store vmpct,vmpc
switch optype (RUN_BLOCK,JMP_BORING,JMP_SELECT,default)

RUN_BLOCK:
switch op1 (xx,xx,xx,xx,default)
JMP_BORING:
store vmpc,op1
JMP_SELECT:
val=select cond (op1,op2)
store vmpc,val
br default

xx:
.....
br default

default:
br VMEntry
  */

#ifndef LLVM_VMFLAT_OBFUSCATION_H
#define LLVM_VMFLAT_OBFUSCATION_H

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/PassManager.h"
//#include <Obfuscation/PassRegistry.h>

namespace llvm {

class VmFlatObfuscationPass : public PassInfoMixin<VmFlatObfuscationPass> {
public:
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);

  static bool isRequired() { return true; }
};

} // namespace llvm

#endif