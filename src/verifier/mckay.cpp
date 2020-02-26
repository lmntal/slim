#include "mckay.hpp"
#include "trie.hpp"
#include "runtime_status.h"
#include "union_find.hpp"
#include <iostream>
#include <string>
propagation_list::iterator firstNonTrivialCell(propagation_list &pList) {

  for (auto it = pList.begin(); it != pList.end(); ++it) {
    if (it->size() > 1) {
      return it;
    }
  }
  return pList.end();
}

void freePreserveDiscreteProapgationList(vertex_list *pdpList) {
  for (auto &v : *pdpList) {
    if (slim::element::holds_alternative<InheritedVertex>(v)) {
      delete (
          slim::element::get<InheritedVertex>(v).conventionalPropagationMemo);
    }
  }

  delete pdpList;
}

bool insertDiscretePropagationListOfInheritedVerticesWithAdjacentLabelToTable(
    discrete_propagation_lists
        &discretePropagationListsOfInheritedVerticesWithAdjacentLabels,
    propagation_list &dpList, ConvertedGraph *cAfterGraph,
    int gapOfGlobalRootMemID, UnionFind &u) {
  bool isExisting = true;
  putLabelsToAdjacentVertices(dpList);
  propagation_list *preserveDPList = new propagation_list(dpList);

  auto &key = *preserveDPList;

  auto seniorDPList =
      discretePropagationListsOfInheritedVerticesWithAdjacentLabels.find(key);

  if (seniorDPList ==
      discretePropagationListsOfInheritedVerticesWithAdjacentLabels.end()) {
    discretePropagationListsOfInheritedVerticesWithAdjacentLabels.insert(
        std::make_pair(key, preserveDPList));
    isExisting = false;
  } else {
    auto itp = preserveDPList->begin();
    auto its = seniorDPList->second->begin();
    for (; itp != preserveDPList->end(); itp++, its++) {
      u.merge(itp->front(), its->front());
    }
#ifdef DIFFISO_DEB
    std::cout << "EXIST" << std::endl;
#endif
  }
  return isExisting;
}

void discretePropagationListDump(vertex_list *dpList) {
  std::cout << *dpList;
  fprintf(stdout, "\n");
  fprintf(stdout, "\n");

  return;
}

Bool isNewSplit(vertex_list::iterator sentinelCell,
                vertex_list::iterator splitCell) {
  for (auto iteratorCell = std::next(sentinelCell, 1);
       iteratorCell != splitCell; iteratorCell = std::next(iteratorCell, 1)) {
    auto &splitIVertex = slim::element::get<InheritedVertex>(*splitCell);
    auto &iteratorIVertex = slim::element::get<InheritedVertex>(*iteratorCell);

    if (isInSameDisjointSetForest(
            splitIVertex.equivalenceClassOfIsomorphism,
            iteratorIVertex.equivalenceClassOfIsomorphism)) {
      return FALSE;
    }
  }

  return TRUE;
}

bool listMcKayInner(
    propagation_list &propagationListOfInheritedVertices,
    ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID,
    discrete_propagation_lists
        &discretePropagationListsOfInheritedVerticesWithAdjacentLabels,
    UnionFind &u) {
  bool isUsefulBranch = true;
  auto stabilizer = propagation_list(propagationListOfInheritedVertices);
#ifdef DIFFISO_DEB
  std::cout << "###### before stabilizer ######" << std::endl;
  std::cout << stabilizer << std::endl;

  printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
  refineConventionalPropagationListByPropagation(stabilizer);
#ifdef DIFFISO_DEB
  std::cout << "###### after stable refinement ######" << std::endl;
  std::cout << stabilizer << std::endl;
#endif
  auto beginSentinel = firstNonTrivialCell(stabilizer);

  if (beginSentinel == stabilizer.end()) {
    isUsefulBranch =
        !insertDiscretePropagationListOfInheritedVerticesWithAdjacentLabelToTable(
            discretePropagationListsOfInheritedVerticesWithAdjacentLabels,
            stabilizer, cAfterGraph, gapOfGlobalRootMemID, u);
  } else {
    bool isFirstLoop = true;
    for (auto i = 0; i < beginSentinel->size(); i++) {
      bool isSame = false;
      for (auto j = 0; j < i; j++) {
        if (u.issame(*std::next(beginSentinel->begin(), i),
                     *std::next(beginSentinel->begin(), j))) {
          isSame = true;
          break;
        }
      }
      if (isSame)
        continue;
      auto new_l = stabilizer.emplace(beginSentinel,
                                      std::list<ConvertedGraphVertex *>());
      new_l->splice(new_l->begin(), *beginSentinel,
                    std::next(beginSentinel->begin(), i),
                    std::next(std::next(beginSentinel->begin(), i)));
      listMcKayInner(
          stabilizer, cAfterGraph, gapOfGlobalRootMemID,
          discretePropagationListsOfInheritedVerticesWithAdjacentLabels, u);
      beginSentinel->splice(std::next(beginSentinel->begin(), i), *new_l,
                            new_l->begin(), std::next(new_l->begin()));
      stabilizer.erase(new_l);
      if(isFirstLoop) {
	isFirstLoop = false;
	if(!isUsefulBranch) {
	  isUsefulBranch = false;
	  break;
	} else {
	  isUsefulBranch = true;
	}
      }
    }
  }
  return isUsefulBranch;
}

propagation_list listMcKay(propagation_list &propagationList,
                           ConvertedGraph *cAfterGraph,
                           int gapOfGlobalRootMemID) {
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__LISTMCKAY);
  }
#endif
  propagation_list canonicalDiscreteRefinement;
  if (propagationList.empty()) {
    canonicalDiscreteRefinement = propagation_list(propagationList);
    return canonicalDiscreteRefinement;
  } else {
    discrete_propagation_lists
        discretePropagationListsOfInheritedVerticesWithAdjacentLabels;
    UnionFind u(propagationList);

#ifdef DIFFISO_DEB
    std::cout << "+++++ start classify +++++" << std::endl;
    classifyWithAttribute(propagationList, cAfterGraph, gapOfGlobalRootMemID);
    std::cout << "###### after attribute classifying ######" << std::endl;
    std::cout << propagationList << std::endl;
#endif
    listMcKayInner(
        propagationList, cAfterGraph, gapOfGlobalRootMemID,
        discretePropagationListsOfInheritedVerticesWithAdjacentLabels, u);

    propagation_list canonicalDiscreteRefinement = propagation_list();
    for (auto &v :
         *discretePropagationListsOfInheritedVerticesWithAdjacentLabels.begin()
              ->second)
      canonicalDiscreteRefinement.push_back(v);

    // std::cout
    //     << "########### candidates of canonical discrete refinement##########
    //     #"
    //     << std::endl;
    // std::cout <<
    // discretePropagationListsOfInheritedVerticesWithAdjacentLabels
    //           << std::endl;
    //   for (auto &v :
    //   *discretePropagationListsOfInheritedVerticesWithAdjacentLabels)
    //     freePreserveDiscreteProapgationList(v.second);
    //   delete discretePropagationListsOfInheritedVerticesWithAdjacentLabels;
    for (auto &v : discretePropagationListsOfInheritedVerticesWithAdjacentLabels) {
      delete v.second;
    }
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_finish_timer(PROFILE_TIME__LISTMCKAY);
    }
#endif
    return canonicalDiscreteRefinement;
  }
  // printf("%s:%d\n", __FUNCTION__, __LINE__);
  // return canonicalDiscreteRefinement;
}

Bool checkIsomorphismValidity(unbound_vector<vertex_list *> *slimKeyCollection,
                              key_collection *McKayKeyCollection,
                              vertex_list *canonicalDiscreteRefinement,
                              long stateID) {
  Bool isValid = TRUE;

  if (stateID != 0) {
    auto &key = *canonicalDiscreteRefinement;
    auto it = McKayKeyCollection->find(key);
    if (it != std::end(*McKayKeyCollection)) {
      CollectionInt seniorID = it->second - 1;
      if (stateID != seniorID) {
        fprintf(stdout, "stateID is wrong.\n");
        fprintf(stdout, "juniorStateID is %ld\n", stateID);
        fprintf(stdout, "seniorStateID is %ld\n", seniorID);
        isValid = FALSE;
        return isValid;
      }
    } else {
      McKayKeyCollection->insert(std::make_pair(key, (stateID + 1)));
    }

    vertex_list *seniorDiscreteRefinement = slimKeyCollection->read(stateID);
    if (seniorDiscreteRefinement != NULL) {
      if (*canonicalDiscreteRefinement != *seniorDiscreteRefinement) {
        printf("adjacency list is wrong.\n");
        isValid = FALSE;
        return isValid;
      } else {
        freePreserveDiscreteProapgationList(canonicalDiscreteRefinement);
      }
    } else {
      slimKeyCollection->write(stateID, canonicalDiscreteRefinement);
    }
  }

  return isValid;
}

bool propagationList_is_discrete(propagation_list &p) {
  bool f = true;
  for (auto &list : p) {
    if (list.size() != 1)
      f = false;
  }
  return f;
}

std::vector<std::vector<std::string>> trieMcKay(Trie *trie, DiffInfo *diffInfo,
                                                Graphinfo *cAfterGraph,
                                                Graphinfo *cBeforeGraph,
                                                std::map<int, int> &id_map, bool forward_f) {
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__TRIEMCKAY);
  }
#endif
  std::vector<std::vector<std::string>> canonical_label;
  int gapOfGlobalRootMemID =
      cBeforeGraph->globalRootMemID - cAfterGraph->globalRootMemID;
  int stepOfPropagation;
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__TRIEPROPAGATE);
  }
#endif
  Bool verticesAreCompletelySorted =
      trie->propagate(diffInfo, cAfterGraph, cBeforeGraph, gapOfGlobalRootMemID,
                      &stepOfPropagation, id_map);
#ifdef PROFILE
  if (lmn_env.profile_level >= 3)   {
    profile_finish_timer(PROFILE_TIME__TRIEPROPAGATE);
  }
#endif
  if (IS_DIFFERENCE_APPLICATION_MODE && verticesAreCompletelySorted && false) {
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_finish_timer(PROFILE_TIME__TRIEMCKAY);
    }
#endif
    return canonical_label;
  } else {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    // for (auto i = cAfterGraph->cv->atoms.begin();
    //      i != cAfterGraph->cv->atoms.end(); ++i)
    //   std::cout << *(i->second->correspondingVertexInTrie) << std::endl;
    propagation_list propagationList;
    trie->conventionalPropagationList(trie->body, propagationList);
#ifdef DIFFISO_DEB
    std::cout << "###### before list propagate ######" << std::endl;
    std::cout << propagationList << std::endl;
#endif
    propagation_list canonicalDiscreteRefinement;
    if (propagationList_is_discrete(propagationList)) {
      canonicalDiscreteRefinement = propagationList;
    } else {
      canonicalDiscreteRefinement = listMcKay(propagationList, cAfterGraph->cv, gapOfGlobalRootMemID);      
    }


#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << *cAfterGraph->cv << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    std::map<ConvertedGraphVertex *, int> m;
    int counter = 0;
    for (auto &v : canonicalDiscreteRefinement) {
      auto cv = v.begin();
      m[(*cv)] = counter;
      counter++;
    }
    for (auto &v : canonicalDiscreteRefinement) {
      auto cv = v.begin();
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << *(*cv) << std::endl;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << (*cv)->correspondingVertexInTrie->canonicalLabel.first << std::endl;
#endif
      std::vector<std::string> l;
      std::string s = (*cv)->name +  std::to_string(m[*cv]);
#ifdef DIFFISO_DEB
      std::cout << s << std::endl;
#endif
      l.push_back(s);
      for (auto &link : (*cv)->links) {
#ifdef DIFFISO_DEB
        std::cout << link << std::endl;
#endif
        auto &attr = link.attr;
        if (attr == INTEGER_ATTR) {
#ifdef DIFFISO_DEB
          std::cout << link.data.integer << std::endl;
#endif
          l.push_back(std::to_string(link.data.integer));
        } else if (attr == GLOBAL_ROOT_MEM_ATTR) {
#ifdef DIFFISO_DEB
          std::cout << "GR" << std::endl;
#endif
        } else if (attr < 128) {
#ifdef DIFFISO_DEB
	  printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
	  auto adjVertex = cAfterGraph->cv->atoms[link.data.ID];
#ifdef DIFFISO_DEB
	  std::cout << *adjVertex << std::endl;
#endif
	  l.push_back(adjVertex->name + std::to_string(m[adjVertex]));
        } else {
          std::cout << "unexpected attr" << std::endl;
        }
      }
      canonical_label.push_back(l);
    }
#ifdef DIFFISO_DEB
    std::cout << "!!CANONICAL LABEL!!" << std::endl;
    std::cout << "[";
    for (auto &l : canonical_label) {
      std::cout << "[";
      for (auto &v : l) {
        std::cout << v << ", ";
      }
      std::cout << "]";
    }
    std::cout << "]" << std::endl;
    std::cout << "!!!!!!!!!!!!!!!!!!!" << std::endl;
#endif
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_finish_timer(PROFILE_TIME__TRIEMCKAY);
    }
#endif
    return canonical_label;
  }
}
