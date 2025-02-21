/* QLMNtal */
#ifndef CARDINALITY_HPP

#define LMN_ATTR_IS_CARD(ATTR) ((ATTR) == LMN_CARD_ATTR)

typedef std::pair<size_t, LmnRegister> CardPair;
typedef std::vector<CardPair> CardMap;
typedef std::vector<CardMap> CardQueue;

struct LmnCard {
  CardQueue queue;
  std::vector<LmnRegister> included_list;
  int pop_index;

  public:
  LmnCard(CardQueue queue) :
    queue(queue),
    included_list(std::vector<LmnRegister>()),
    pop_index(0) {};
  
  CardQueue get_queue() {
    return queue;
  }
  
  void reset_queue() {
    queue = CardQueue();
    included_list = std::vector<LmnRegister>();
    pop_index = 0;
  }
  
  void push_map(CardMap map) {
    queue.push_back(map);
  }

  CardMap pop_map() {
    CardMap map = queue[pop_index];
    pop_index++;
    return map;
  }

  std::vector<LmnRegister> get_included_list() {
    return included_list;
  }

  void concat_included_list(std::vector<LmnRegister> included) {
    included_list.insert(included_list.end(), included.begin(), included.end());
  }

  int get_pop_index() {
    return pop_index;
  }

  void set_pop_index(int i) {
    pop_index = i;
  }
  
  std::vector<LmnRegister> calc_picked_maps_regs(size_t max) {
    LmnCard* card;
    card = new LmnCard(CardQueue());
    auto picked_maps_regs = std::vector<LmnRegister>{{(LmnWord)card, LMN_CARD_ATTR, TT_OTHER}};
    for (CardMap map: queue) {
      auto map_included_list = std::vector<LmnRegister>();
      for (CardPair pair: map) {
        if (pair.second.register_tt() == TT_ATOM || pair.second.register_tt() == TT_MEM) {
          map_included_list.push_back(pair.second);
        } else if (LMN_ATTR_IS_CARD(pair.second.register_at())) {
          std::vector<LmnRegister> queue_reg_included_list = ((LmnCard*)(pair.second.register_wt()))->get_included_list();
          map_included_list.insert(map_included_list.end(), queue_reg_included_list.begin(), queue_reg_included_list.end());
        }
      }
      auto new_picked_maps_regs = std::vector<LmnRegister>();
      for (LmnRegister reg: picked_maps_regs) {
        new_picked_maps_regs.push_back(reg);
        if (max > 0 && ((LmnCard*)(reg.register_wt()))->get_queue().size() < max) {
          std::vector<LmnRegister> queue_included_list = ((LmnCard*)(reg.register_wt()))->get_included_list();
          BOOL overlap = FALSE;
          for (LmnRegister queue_included: queue_included_list) {
            for (LmnRegister map_included: map_included_list) {
              if (queue_included.register_tt() == TT_ATOM && map_included.register_tt() == TT_ATOM) {
                if (queue_included.register_wt() == map_included.register_wt()){
                  overlap = TRUE;
                }
              } else if (queue_included.register_tt() == TT_MEM && map_included.register_tt() == TT_MEM) {
                if (queue_included.register_wt() == map_included.register_wt()){
                  overlap = TRUE;
                }
              }
              if (overlap == TRUE) {
                break;
              }
            }
            if (overlap == TRUE) {
              break;
            }
          }
          if (!overlap) {
            card = new LmnCard(((LmnCard*)(reg.register_wt()))->get_queue());
            card->concat_included_list(queue_included_list);
            card->push_map(map);
            card->concat_included_list(map_included_list);
            new_picked_maps_regs.push_back({(LmnWord)card, LMN_CARD_ATTR, TT_OTHER});
          }
        }
      }
      picked_maps_regs = new_picked_maps_regs;
    }
    return picked_maps_regs;
  }
};

typedef LmnCard* LmnCardRef;

#endif // CARDINALITY_HPP

BOOL mapneqatom(size_t atom1, std::vector<size_t> atom2list, LmnReactCxt *rc, LmnRegister queue_reg){
  CardQueue queue = ((LmnCardRef)(queue_reg.register_wt()))->get_queue();
  if (atom2list.size() == 1) {
    for (CardMap map: queue) {
      for (CardPair pair: map) {
        if (atom2list[0] == pair.first) {
          if (!(LMN_ATTR_IS_DATA(rc->at(atom1)) || LMN_ATTR_IS_DATA(pair.second.register_at()) ||
            LMN_SATOM(rc->wt(atom1)) != LMN_SATOM(pair.second.register_wt()))) {
              return FALSE;
            }
        }
      }
    }
  } else {
    size_t atom2list_head = atom2list[0];
    atom2list.erase(atom2list.begin());
    for (CardMap map: queue) {
      for (CardPair pair: map) {
        if (atom2list_head == pair.first) {
          if (!mapneqatom(atom1, atom2list, rc, pair.second)) {
            return FALSE;
          }
        }
      }
    }
  }
  return TRUE;
}

BOOL mapneqmem(size_t mem1, std::vector<size_t> mem2list, LmnReactCxt *rc, LmnRegister queue_reg){
  CardQueue queue = ((LmnCardRef)(queue_reg.register_wt()))->get_queue();
  if(mem2list.size() == 1) {
    for (CardMap map: queue) {
      for (CardPair pair: map) {
        if(mem2list[0] == pair.first) {
          if (rc->wt(mem1) == pair.second.register_wt()) {
              return FALSE;
            }
        }
      }
    }
  } else {
    size_t mem2list_head = mem2list[0];
    mem2list.erase(mem2list.begin());
    for (CardMap map: queue) {
      for (CardPair pair: map) {
        if(mem2list_head == pair.first) {
          if (!mapneqmem(mem1, mem2list, rc, pair.second)) {
            return FALSE;
          }
        }
      }
    }
  }
  return TRUE;
}

void slim::vm::interpreter::anyatom(LmnMembrane *mem, size_t reg){
  std::map<LmnFunctor, AtomListEntry *> atomlists = mem->atom_lists();
  auto atom_regs = std::vector<LmnRegister>();
  for (std::pair<LmnFunctor, AtomListEntry *> atomlist: atomlists) { 
    auto iter = std::begin(*atomlist.second);
    auto end = std::end(*atomlist.second);
    if (iter == end || atomlist.first == 0 || atomlist.first == 1)
      continue;
    auto v = std::vector<LmnRegister>(atomlist.second->size());
    std::transform(iter, end, v.begin(), [](LmnSymbolAtomRef atom) {
      return LmnRegister({(LmnWord)atom, LMN_ATTR_MAKE_LINK(0), TT_ATOM});
    });
    atom_regs.insert(atom_regs.end(), v.begin(), v.end());
  }
  if(lmn_env.shuffle_atom) {
    std::random_shuffle(atom_regs.begin(), atom_regs.end());
  }
  this->false_driven_enumerate(reg, std::move(atom_regs));
}

BOOL eqmaps_sub(CardMap map1, CardMap map2);

BOOL eqmaps(CardQueue queue1, CardQueue queue2) {
  if (queue1.size() != queue2.size()){
    return FALSE;
  }
  for (CardMap map1: queue1) {
    BOOL hit = FALSE;
    auto new_queue2 = CardQueue();
    for (CardMap map2: queue2) {
      if (!hit & eqmaps_sub(map1, map2)) {
        hit = TRUE;
      } else {
        new_queue2.push_back(map2);
      }
    }
    if (!hit) {
      return FALSE;
    }
    queue2 = new_queue2;
  }
  return TRUE;
}

BOOL eqmaps_sub(CardMap map1, CardMap map2){
  if (map1.size() != map2.size()) {
    return FALSE;
  }
  for (CardPair pair1: map1) {
    int index1 = pair1.first;
    LmnRegister reg1 = pair1.second;
    BOOL hit = FALSE;
    auto new_map2 = CardMap();
    for (CardPair pair2: map2) {
      int index2 = pair2.first;
      LmnRegister reg2 = pair2.second;
      if (!hit & reg1.register_tt() == reg2.register_tt()) {
        if (reg1.register_tt() == TT_ATOM) {
          if (!(LMN_ATTR_IS_DATA(reg1.register_at()) || LMN_ATTR_IS_DATA(reg2.register_at()) ||
          LMN_SATOM(reg1.register_wt()) != LMN_SATOM(reg2.register_wt()))) {
            hit = TRUE;
          } else {
            new_map2.push_back(pair2);
          }
        } else if (pair1.second.register_tt() == TT_MEM) {
          if (reg1.register_wt() == reg2.register_wt()) {
            hit = TRUE;
          } else {
            new_map2.push_back(pair2);
          }
        } else if(LMN_ATTR_IS_CARD(pair1.second.register_at())) {
          CardQueue queue1 = ((LmnCardRef)(pair1.second.register_wt()))->get_queue();
          CardQueue queue2 = ((LmnCardRef)(pair2.second.register_wt()))->get_queue();
          if (eqmaps(queue1, queue2)) {
            hit = TRUE;
          } else {
            new_map2.push_back(pair2);
          }
        }
      } else {
        new_map2.push_back(pair2);
      }
    }
    if (!hit) {
      return FALSE;
    }
    map2 = new_map2;
  }
  return TRUE;
}

int replace_in_card_by_tbl(ProcessTableRef p, LmnCardRef card, LmnWord *value) {
  CardQueue queue = card->get_queue();
  CardQueue new_queue = CardQueue();
  for (CardMap map: queue) {
    auto new_map = CardMap();
    for (CardPair pair: map) {
      LmnWord t;
      if (pair.second.register_tt() == TT_ATOM) {
        if (LMN_ATTR_IS_DATA(pair.second.register_at())) {
          new_map.push_back(CardPair{pair.first, {lmn_copy_data_atom(pair.second.register_wt(), pair.second.register_at()), pair.second.register_at(), TT_ATOM}});
        } else {
          if (proc_tbl_get_by_atom(p, (LmnSymbolAtomRef)pair.second.register_wt(), &t)) {
            new_map.push_back(CardPair{pair.first, {t, pair.second.register_at(), TT_ATOM}});
          } else {
            return 0;
          }
        }
      } else if (pair.second.register_tt() == TT_MEM) {
        if (proc_tbl_get_by_mem(p, (LmnMembraneRef)pair.second.register_wt(), &t)) {
          new_map.push_back(CardPair{pair.first, {t, pair.second.register_at(), TT_MEM}});
        } else {
          return 0;
        }
      } else if (LMN_ATTR_IS_CARD(pair.second.register_at())) {
        if (replace_in_card_by_tbl(p, (LmnCardRef)pair.second.register_wt(), &t)) {
          new_map.push_back(CardPair{pair.first, {t, pair.second.register_at(), TT_OTHER}});
        } else {
          return 0;
        }
      }
    }
    new_queue.push_back(new_map);
  }
  LmnCardRef new_card;
  new_card = new LmnCard(new_queue);
  *value = (LmnWord)new_card;
  return 1;
}