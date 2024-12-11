/* QLMNtal */
#ifndef CARDINALITY_HPP

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
    auto picked_maps_regs = std::vector<LmnRegister>{{(LmnWord)card, 0, TT_CARD}};
    for (CardMap map: queue) {
      auto map_included_list = std::vector<LmnRegister>();
      for (CardPair pair: map) {
        if (pair.second.register_tt() == TT_ATOM || pair.second.register_tt() == TT_MEM) {
          map_included_list.push_back(pair.second);
        } else if (pair.second.register_tt() == TT_CARD) {
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
            new_picked_maps_regs.push_back({(LmnWord)card, 0, TT_CARD});
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