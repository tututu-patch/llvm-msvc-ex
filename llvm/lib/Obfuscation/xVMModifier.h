#ifndef LLVM_VMModifier_OBFUSCATION_H
#define LLVM_VMModifier_OBFUSCATION_H

#include "xVMP.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include <map>

namespace llvm {
class VMModifier {

    public:
        VMModifier(Function &F, std::map<GlobalVariable *, int> *gv_value_map,vmprotect_store *store) {
            this->Mod = F.getParent();
            this->F = &F;
            this->modDataLayout = new DataLayout(this->Mod);
            this->gv_value_map = gv_value_map;
            this->vm_store = store;
        }

        Module * Mod;
        Function * F;
        DataLayout * modDataLayout;
        vmprotect_store *vm_store;
        std::map<GlobalVariable *, int> *gv_value_map;


        virtual void run ();


};
}
#endif
