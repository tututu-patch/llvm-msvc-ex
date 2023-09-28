#ifndef LLVM_VMTranslator_OBFUSCATION_H
#define LLVM_VMTranslator_OBFUSCATION_H

#include "xVMP.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"

#include <map>

namespace llvm {

/***
 * The main class that modify the callsite of vm-function.
 */
class VMTranslator {

public:
  VMTranslator(Function &F,vmprotect_store *store) {
    this->Mod = F.getParent();
    this->F = &F;
    this->modDataLayout = new DataLayout(this->Mod);
    this->vm_store = store;
    // construct function and global variables
    init();
  }

  vmprotect_store *vm_store;
  Module *Mod;
  Function *F;
  DataLayout *modDataLayout;

  // construct callinst_handler to interprete callinst
  Function *callinst_handler;
  BasicBlock *callinst_handler_conBBL;
  Value *targetfunc_id;
  unsigned callinst_handler_curr_idx;
  std::map<Function *, unsigned> function_id_map;

  // hex code
  std::vector<uint8_t> vm_code;

  // map
  std::map<Value *, int> value_map;
  std::map<BasicBlock *, int> basicblock_map;
  std::vector<std::pair<int, BasicBlock *>> br_map; // ?? successor basicblock???
  std::map<CallInst *, long long> callinst_map;

  // collect global variable
  std::map<GlobalVariable *, int> gv_value_map;

  // current offset in data_seg
  int curr_data_offset = 0;

  virtual void run();
  virtual void handle_inst(Instruction *);
  virtual void construct_gv();

  // create callinst_handler function, add instructions to callinst_handler and
  // create ret basicblock to callinst_handler
  virtual void setup_callinst_handler();
  virtual void handle_callinst(CallInst *inst, long long curr_func_id);
  virtual void finish_callinst_handler();

// get a NULL value
#define GET_NULL_VALUE() std::vector<uint8_t>(2 + POINTER_SIZE)

// pack a value
#define GET_PACK_VALUE(value) (packValue(value, &value_map))

  // construct function and global variables
  void init() {
    // construct_gv();
    setup_callinst_handler();

    // encrypt opcode
    init_xorshift32();
  }

  Function *get_callinst_handler() { return this->callinst_handler; }

  // insert arg into res.end
  template <typename T, typename Arg> void vector_appender(T &res, Arg arg) {
    res.insert(res.end(), arg.begin(), arg.end());
  }

  // combine multiple vector, result in res
  template <typename T, typename... Args> void ins_to_hex(T &res, Args... arg) {
    int wrapper[] = {(vector_appender(res, arg), 0)...};
  }

  std::map<GlobalVariable *, int> *get_gv_value_map() { return &gv_value_map; }

  // insert a value to value_map
  void insert_to_value_map(std::map<Value *, int> *value_map, Value *value,
                           int offset) {
    value_map->insert(std::pair<Value *, int>(value, offset));
  }

  void dump_vector(std::vector<uint8_t> v) {
    for (auto i : v) {
      errs() << int(i) << " ";
    }
    errs() << "\n";
  }

  // pack a int to vector<uint8_t>(4)
  std::vector<uint8_t> p32(int int32) {
    std::vector<uint8_t> tmp;
    tmp.push_back(int32 & 0xFF);
    tmp.push_back((int32 >> 8) & 0xFF);
    tmp.push_back((int32 >> 16) & 0xFF);
    tmp.push_back((int32 >> 24) & 0xFF);
    return tmp;
  }

  // pack a int to vector<uint8_t>(1-8)
  std::vector<uint8_t> pack(long long int int_n, int size) {

    if (size > 8) {
      // long long is 64bit
      assert(0);
    }

    std::vector<uint8_t> tmp;
    while (size > 0) {
      tmp.push_back((int_n)&0xFF);
      size--;
      int_n = int_n >> 8;
    }
    return tmp;
  }

  // encrypt opcode, use xorshift
  uint32_t xorshift32_state = 0;
  uint32_t xorshift32_seed = 0;

  /* encrypt vm_code */
  // mark seed for each basicblock
  std::map<uint32_t, std::pair<uint32_t, uint32_t>> vm_code_seed_map;

  void init_xorshift32() { srand(time(0)); }

  uint32_t gen_xorshift32_seed() {
    for (int _ = 0; _ < 10; _++) {
      xorshift32_seed ^= rand();
    }
    return xorshift32_seed;
  }

  /* The state word must be initialized to non-zero */
  uint32_t xorshift32(uint32_t *state) {
    /* Algorithm "xor" from p. 4 of Marsaglia, "Xorshift RNGs" */
    uint32_t x = *state;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    return *state = x;
  }

  uint32_t vm_code_seed_setup() {
    uint32_t res = gen_xorshift32_seed();

    std::vector<uint8_t> hex_code;
    ins_to_hex(hex_code, pack(res, sizeof(uint32_t)));
    vm_code.insert(vm_code.end(), hex_code.begin(), hex_code.end());

    return res;
  }

  uint32_t opcode_seed_setup() {
    xorshift32_seed = gen_xorshift32_seed();
    xorshift32_state = xorshift32_seed;

    std::vector<uint8_t> hex_code;
    ins_to_hex(hex_code, pack(xorshift32_seed, sizeof(uint32_t)));
    vm_code.insert(vm_code.end(), hex_code.begin(), hex_code.end());

    return xorshift32_seed;
  }

  void encrypt_vm_code() {
    for (auto p : vm_code_seed_map) {
      uint32_t vm_code_seed = p.first;
      for (uint32_t addr = p.second.first; addr < p.second.second; addr++) {
        vm_code[addr] ^= (xorshift32(&vm_code_seed) & 0xFF);
      }
    }
  }

  // pack one byte opcode
  std::vector<uint8_t> pack_op(uint8_t op) {
    uint8_t res = 0;
    std::vector<uint8_t> his;
    for (int i = 0; i < op; i++) {
      uint8_t tmp = xorshift32(&xorshift32_state) & 0xFF;
      // privent xorshift32&0xFF conflict
      if (find(his.begin(), his.end(), tmp) == his.end()) {
        his.push_back(tmp);
        res = tmp;
      } else {
        i--;
      }
    }
    return pack(res, 1);
  }

  // pack a Constant to a vector
  std::vector<uint8_t> pack_const_value(Value *const_value) {
    std::vector<uint8_t> res;
    Type *type = const_value->getType();
    int type_size = modDataLayout->getTypeAllocSize(type);
    int64_t value = 0;

    // unsupport float
    //
    // if(type->isDoubleTy()){
    //     ConstantFP* FP = dyn_cast<ConstantFP>(const_value);
    //     value = FP->getValueAPF().bitcastToAPInt();
    // }
    // else if(type->isFloatTy()){
    //     ConstantFP* FP = dyn_cast<ConstantFP>(const_value);
    //     value = FP->getValueAPF().bitcastToAPInt();
    // }

    if (type->isIntegerTy()) {
      ConstantInt *CI = dyn_cast<ConstantInt>(const_value);
      if (CI->getBitWidth() <= 64) {
        value = CI->getSExtValue();
      }
    } else if (ConstantPointerNull *CPN =
                   dyn_cast<ConstantPointerNull>(const_value)) {
      // handle i8* null
      value = 0;
    } else {
      errs() << "Unsport const value: " << *const_value << "\n";
      assert(0);
    }

    res = pack(value, type_size);

    return res;
  }

  // pack type to a vector(2)
  // {size, TypeID}
  std::vector<uint8_t> type_to_hex(Type *type) {
    std::vector<uint8_t> res;
    res.push_back(modDataLayout->getTypeAllocSize(type));
    res.push_back(type->getTypeID());
    return res;
  }

  // pack a value
  std::vector<uint8_t> packValue(Value *value,
                                 std::map<Value *, int> *value_map) {
    std::vector<uint8_t> res;
    std::vector<uint8_t> packed;
    std::vector<uint8_t> packType = type_to_hex(value->getType());
    if (ConstantData *CD = dyn_cast<ConstantData>(value)) {
      packed = pack_const_value(value);
    } else {
      // if value not in map
      if (value_map->find(value) == value_map->end()) {
        // check value is not a GlobalVariable
        if (GlobalVariable *gv = dyn_cast<GlobalVariable>(value)) {
          // is a GlobalVariable and not in value_map
          // put it into value_map
          insert_to_value_map(value_map, value, curr_data_offset);

          // also put it into gv_value_map
          gv_value_map.insert(
              std::pair<GlobalVariable *, int>(gv, curr_data_offset));

          int res_size = modDataLayout->getTypeAllocSize(gv->getType());
          curr_data_offset += res_size;
        } else {
          assert(value_map->find(value) != value_map->end());
        }
      }

      packed = pack((*value_map)[value], POINTER_SIZE);
      // variableï¼Œpacktype->TypeID=0
      packType[1] = 0;
    }

    res.insert(res.end(), packType.begin(), packType.end());
    res.insert(res.end(), packed.begin(), packed.end());

    return res;
  }
};
} // namespace llvm

#endif