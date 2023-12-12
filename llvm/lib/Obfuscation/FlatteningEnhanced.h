#ifndef _FLATTENINGENHANCED_H_
#define _FLATTENINGENHANCED_H_

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/AbstractCallSite.h"
#include "llvm/Pass.h"
#include <list>
#include <vector>

// User libs

#include "CryptoUtils.h"
#include "Utils.h"

#include "llvm/IR/PassManager.h"

using namespace std;
using namespace llvm;

namespace llvm{ // 平坦化控制流增强版
class FlatteningEnhanced : public PassInfoMixin<FlatteningEnhanced> {
public:
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
  static bool isRequired() { return true; } // 直接返回true即可
};
}
#endif