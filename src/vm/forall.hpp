#include "lmntal.h"
#include "react_context.hpp"

#ifndef FORALL_HPP
    
struct LmnForall {
  LmnRuleInstr exists_instr;
  LmnRegisterArray initial_reg;
  std::vector<size_t> changed_reg_indices;
  std::vector<std::map<size_t, LmnRegister>> preserved_reg;
  private:
  unsigned int step;

  public:
  LmnForall(LmnRuleInstr exists_instr) :
    exists_instr(exists_instr), step(0) {};

  // forallpushループ開始時のレジスタを雑に突っ込むメソッド
  // 1回目: initial_regに保存する
  // 2回目: initial_regとの差分を把握して何番のレジスタが変化するのかを把握してchanged_reg_indicesに記録。
  //       その差分だけreserved_regに突っ込む
  // n(n>2)回目: changed_reg_indicesのレジスタをそのままreserved_regに突っ込む
  void push_registers(LmnRegisterArray regarray) {
    if (step == 0) {
      step++;
      initial_reg = LmnRegisterArray(regarray.size());
      size_t i;
      // copy regarray to initial_reg
      for (i = 0; i < regarray.size(); i++) {
        initial_reg[i] = regarray[i];
      }
    } else if (step == 1) {
      step++;
      size_t i;
      changed_reg_indices = std::vector<size_t>();
      // record changed_reg_indices
      for (i = 0; i < regarray.size(); i++) {
        if (regarray[i].wt != initial_reg[i].wt) {
          changed_reg_indices.push_back(i);
        }
      }
      preserved_reg = std::vector<std::map<size_t, LmnRegister>>();
      std::map<size_t, LmnRegister> map = std::map<size_t, LmnRegister>();
      for (auto i: changed_reg_indices) {
        LmnRegister reg_clone = regarray[i];
        map.insert(std::make_pair(i, reg_clone));
      }
      preserved_reg.push_back(map);
    } else {
      // refer to changed_reg_indices
      std::map<size_t, LmnRegister> map = std::map<size_t, LmnRegister>();
      for (auto i: changed_reg_indices) {
        LmnRegister reg_clone = regarray[i];
        map.insert(std::make_pair(i, reg_clone));
      }
      preserved_reg.push_back(map);
    }
  }
};

typedef LmnForall* LmnForallRef;

#endif // FORALL_HPP