#include <utility>

#include "llvm/Pass.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

namespace {
    struct SkeletonPass : public FunctionPass {
        static char ID;
        SkeletonPass() : FunctionPass(ID) {}

        Function *print_msg;

        /*
         * Creates a global variable holding the specified string.
         */
        Constant *getConstantArray(Module *module, std::string str) {
            auto i8 = Type::getInt8Ty(module->getContext());

            // create a null-terminated string constant
            std::vector<Constant *> chars;
            for (int i = 0; i < str.size(); i++) {
                chars.push_back(ConstantInt::get(i8, str[i]));
            }
            chars.push_back(ConstantInt::get(i8, '\0'));

            // initialize the array
            Constant *initializer = ConstantArray::get(ArrayType::get(i8, 0), chars);

            // store the array in a global variable
            GlobalVariable *var = new GlobalVariable(*module, initializer->getType(), true,
                                                     GlobalVariable::ExternalLinkage, initializer, str);

            // convert to char *
            return ConstantExpr::getBitCast(var, i8->getPointerTo());
        }

        virtual bool runOnFunction(Function &func) {
            if (!print_msg) {
                print_msg = &func;
            } else {
                // type of the hacky print_msg function
                ArrayRef<Type *> arg_types = {ArrayType::get(Type::getInt8Ty(func.getContext()), 0)};
                FunctionType *type = FunctionType::get(Type::getInt32Ty(func.getContext()), arg_types, false);
                
                for (auto &bblock : func) {
                    // keep track of print_msg instructions to insert
                    // first is the place to insert, second is the message to print
                    std::vector<std::pair<Instruction *, llvm::Constant *>> to_insert;
                    
                    for (auto &instr : bblock) {
                        if (auto *call = dyn_cast<CallBase>(&instr)) {
                            // we print a message for entering and leaving

                            std::string name = call->getCalledFunction()->getName();
                            
                            auto *enterMsg = getConstantArray(func.getParent(), "entering function: " + name);
                            auto *leaveMsg = getConstantArray(func.getParent(), "leaving function: " + name);

                            to_insert.push_back(std::make_pair(call, enterMsg));
                            to_insert.push_back(std::make_pair(call->getNextNode(), leaveMsg));
                        }
                    }

                    // we need to add the instructions after we finish traversing the instruction graph
                    for (auto pair : to_insert) {
                        IRBuilder<>(pair.first).CreateCall(type, print_msg, {pair.second});
                    }
                }
            }

            return false;
        }
    };
}

char SkeletonPass::ID = 0;

// Automatically enable the pass.
// http://adriansampson.net/blog/clangpass.html
static void registerSkeletonPass(const PassManagerBuilder &,
                                 legacy::PassManagerBase &PM) {
    PM.add(new SkeletonPass());
}
static RegisterStandardPasses
RegisterMyPass(PassManagerBuilder::EP_EarlyAsPossible,
               registerSkeletonPass);
