#ifndef LMN_MEMBRANE_HPP
#define LMN_MEMBRANE_HPP
/*----------------------------------------------------------------------
 * Membrane
 */

struct LmnMembrane {
  AtomSet atomset;
  ProcessID id;
  unsigned int max_functor;
  unsigned int atomset_size;
  unsigned int atom_symb_num; /* # of symbol atom except proxy */
  unsigned int atom_data_num;
  lmn_interned_str name;
  BOOL is_activated;
  LmnMembraneRef parent;
  LmnMembraneRef child_head;
  LmnMembraneRef prev, next;
  struct Vector rulesets;
#ifdef USE_FIRSTCLASS_RULE
  Vector *firstclass_rulesets;
#endif
};

#endif
