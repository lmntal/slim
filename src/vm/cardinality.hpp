#include <iostream> // 後で消す

/* QLMNtal */
#ifndef CARDINALITY_HPP

typedef std::pair<size_t, LmnRegister> CardPair;
typedef std::vector<CardPair> CardMap;
typedef std::vector<CardMap> CardQueue;

struct LmnCard {
  CardQueue queue;
  std::vector<LmnRegister> included_list;

  public:
  LmnCard(CardQueue queue) :
    queue(queue),
    included_list(std::vector<LmnRegister>()) {};
  
  CardQueue get_queue(){
    return queue;
  }
  
  void reset_queue(){
    queue = CardQueue();
  }
  
  void push_map(CardMap map){
    queue.push_back(map);
  }

  CardMap pop_map(){
    CardMap map = queue[0];
    queue.erase(queue.begin());
    return map;
  }

  std::vector<LmnRegister> get_included_list(){
    return included_list;
  }

  void add_included(std::vector<LmnRegister> included){
    included_list.insert(included_list.end(), included.begin(), included.end());
  }
  
  std::vector<LmnRegister> get_queue_combination(size_t max){
    LmnCard* card;
    card = new LmnCard(CardQueue());
    std::vector<LmnRegister> queue_combination = std::vector<LmnRegister>{{(LmnWord)card, 0, TT_CARD}};
    for (CardMap map: queue) {
      std::vector<LmnRegister> map_included_list = std::vector<LmnRegister>();
      for (CardPair pair: map) {
        if (TT_ATOM == pair.second.register_tt() || TT_MEM == pair.second.register_tt()) {
          map_included_list.push_back(pair.second);
        } else if (TT_CARD == pair.second.register_tt()) {
          std::vector<LmnRegister> queue_reg_included_list = ((LmnCard*)(pair.second.register_wt()))->get_included_list();
          map_included_list.insert(map_included_list.end(), queue_reg_included_list.begin(), queue_reg_included_list.end());
        }
      }
      std::vector<LmnRegister> new_queue_combination = std::vector<LmnRegister>();
      for (LmnRegister queue_combination_element: queue_combination){
        new_queue_combination.push_back(queue_combination_element);
        if (max > 0 && ((LmnCard*)(queue_combination_element.register_wt()))->get_queue().size() < max) {
          std::vector<LmnRegister> queue_included_list = ((LmnCard*)(queue_combination_element.register_wt()))->get_included_list();
          BOOL duplication = FALSE;
          for (LmnRegister queue_included: queue_included_list){
            for (LmnRegister map_included: map_included_list){
              if (TT_ATOM == queue_included.register_tt() && TT_ATOM == map_included.register_tt()) {
                // if (LMN_SATOM(queue_included.register_wt()) == LMN_SATOM(map_included.register_wt())){
                if (queue_included.register_wt() == map_included.register_wt()){
                  duplication = TRUE;
                }
              } else if (TT_MEM == queue_included.register_tt() && TT_MEM == map_included.register_tt()) {
                if (queue_included.register_wt() == map_included.register_wt()){
                  duplication = TRUE;
                }
              }
            }
          }
          if (!duplication) {
            card = new LmnCard(((LmnCard*)(queue_combination_element.register_wt()))->get_queue());
            card->add_included(queue_included_list);
            card->push_map(map);
            card->add_included(map_included_list);
            new_queue_combination.push_back({(LmnWord)card, 0, TT_CARD});
          }
        }
      }
      queue_combination = new_queue_combination;
    }
    return queue_combination;
  }

};

typedef LmnCard* LmnCardRef;

#endif // CARDINALITY_HPP