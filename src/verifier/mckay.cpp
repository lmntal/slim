#include "mckay.hpp"
#include "trie.hpp"

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
    int gapOfGlobalRootMemID) {
  bool isExisting = true;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  putLabelsToAdjacentVertices(dpList);
  propagation_list *preserveDPList = new propagation_list(dpList);

  auto &key = *preserveDPList;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  auto seniorDPList =
      discretePropagationListsOfInheritedVerticesWithAdjacentLabels.find(key);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  if (seniorDPList ==
      discretePropagationListsOfInheritedVerticesWithAdjacentLabels.end()) {
    std::cout << "NEW" << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    discretePropagationListsOfInheritedVerticesWithAdjacentLabels.insert(
        std::make_pair(key, preserveDPList));
    isExisting = false;
    return isExisting;
  } else {
    std::cout << "EXIST" << std::endl;
  }

  // else {
  //   auto iteratorCell = std::begin(*preserveDPList);
  //   auto iteratorCellSenior = std::begin(*seniorDPList->second);

  //   while (iteratorCell != std::end(*preserveDPList)) {
  //     if (*iteratorCell != CLASS_SENTINEL) {
  //       auto &iVertex = slim::element::get<InheritedVertex>(*iteratorCell);
  //       auto &iVertexSenior =
  //           slim::element::get<InheritedVertex>(*iteratorCellSenior);

  //       unionDisjointSetForest(iVertex.equivalenceClassOfIsomorphism,
  //                              iVertexSenior.equivalenceClassOfIsomorphism);
  //     }

  //     iteratorCell = std::next(iteratorCell, 1);
  //     iteratorCellSenior = std::next(iteratorCellSenior, 1);
  //   }

  //   freePreserveDiscreteProapgationList(preserveDPList);

  //   isExisting = TRUE;
  //   return isExisting;
  // }
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
        &discretePropagationListsOfInheritedVerticesWithAdjacentLabels) {
  bool isUsefulBranch = true;
  auto stabilizer = propagation_list(propagationListOfInheritedVertices);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  refineConventionalPropagationListByPropagation(stabilizer);

  std::cout << "###### after stable refinement ######" << std::endl;
  std::cout << stabilizer << std::endl;

  auto beginSentinel = firstNonTrivialCell(stabilizer);

  if (beginSentinel == stabilizer.end()) {
    isUsefulBranch =
        !insertDiscretePropagationListOfInheritedVerticesWithAdjacentLabelToTable(
            discretePropagationListsOfInheritedVerticesWithAdjacentLabels,
            stabilizer, cAfterGraph, gapOfGlobalRootMemID);
  } else {
    for (auto i = 0; i < beginSentinel->size(); i++) {
      auto new_l = stabilizer.emplace(beginSentinel,
                                      std::list<ConvertedGraphVertex *>());
      new_l->splice(new_l->begin(), *beginSentinel,
                    std::next(beginSentinel->begin(), i),
                    std::next(std::next(beginSentinel->begin(), i)));
      listMcKayInner(
          stabilizer, cAfterGraph, gapOfGlobalRootMemID,
          discretePropagationListsOfInheritedVerticesWithAdjacentLabels);
      beginSentinel->splice(std::next(beginSentinel->begin(), i), *new_l,
                            new_l->begin(), std::next(new_l->begin()));
      stabilizer.erase(new_l);
    }
  }
  return isUsefulBranch;
}

propagation_list listMcKay(propagation_list &propagationList,
                           ConvertedGraph *cAfterGraph,
                           int gapOfGlobalRootMemID) {
  propagation_list canonicalDiscreteRefinement;
  if (propagationList.empty()) {
    canonicalDiscreteRefinement = propagation_list(propagationList);
    return canonicalDiscreteRefinement;
  } else {
    discrete_propagation_lists
        discretePropagationListsOfInheritedVerticesWithAdjacentLabels;

    std::cout << "+++++ start classify +++++" << std::endl;
    classifyWithAttribute(propagationList, cAfterGraph, gapOfGlobalRootMemID);
    std::cout << "###### after attribute classifying ######" << std::endl;
    std::cout << propagationList << std::endl;
    listMcKayInner(
        propagationList, cAfterGraph, gapOfGlobalRootMemID,
        discretePropagationListsOfInheritedVerticesWithAdjacentLabels);

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
                                                std::map<int, int> &id_map) {
  std::vector<std::vector<std::string>> canonical_label;
  int gapOfGlobalRootMemID =
      cBeforeGraph->globalRootMemID - cAfterGraph->globalRootMemID;
  int stepOfPropagation;
  Bool verticesAreCompletelySorted =
      trie->propagate(diffInfo, cAfterGraph, cBeforeGraph, gapOfGlobalRootMemID,
                      &stepOfPropagation, id_map);
  if (IS_DIFFERENCE_APPLICATION_MODE && verticesAreCompletelySorted && false) {
    return canonical_label;
  } else {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    // for (auto i = cAfterGraph->cv->atoms.begin();
    //      i != cAfterGraph->cv->atoms.end(); ++i)
    //   std::cout << *(i->second->correspondingVertexInTrie) << std::endl;
    propagation_list propagationList;
    trie->conventionalPropagationList(trie->body, propagationList);
    std::cout << "###### before list propagate ######" << std::endl;
    std::cout << propagationList << std::endl;
    propagation_list canonicalDiscreteRefinement;
    if (propagationList_is_discrete(propagationList)) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      canonicalDiscreteRefinement = propagationList;
    } else {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      canonicalDiscreteRefinement = listMcKay(propagationList, cAfterGraph->cv, gapOfGlobalRootMemID);      
    }



    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << *cAfterGraph->cv << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::map<ConvertedGraphVertex *, int> m;
    int counter = 0;
    for (auto &v : canonicalDiscreteRefinement) {
      auto cv = v.begin();
      m[(*cv)] = counter;
      counter++;
    }
    for (auto &v : canonicalDiscreteRefinement) {
      auto cv = v.begin();

      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << *(*cv) << std::endl;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << (*cv)->correspondingVertexInTrie->canonicalLabel.first << std::endl;
      std::vector<std::string> l;
      std::string s = (*cv)->name +  std::to_string(m[*cv]);
      std::cout << s << std::endl;
      l.push_back(s);
      for (auto &link : (*cv)->links) {
        std::cout << link << std::endl;
        auto &attr = link.attr;
        if (attr == INTEGER_ATTR) {
          std::cout << link.data.integer << std::endl;
          l.push_back(std::to_string(link.data.integer));
        } else if (attr == GLOBAL_ROOT_MEM_ATTR) {
          std::cout << "GR" << std::endl;
        } else if (attr < 128) {
	  printf("%s:%d\n", __FUNCTION__, __LINE__);
	  auto adjVertex = cAfterGraph->cv->atoms[link.data.ID];
	  std::cout << *adjVertex << std::endl;
	  l.push_back(adjVertex->name + std::to_string(m[adjVertex]));
        } else {
          std::cout << "unexpected attr" << std::endl;
        }
      }
      canonical_label.push_back(l);
    }

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

    return canonical_label;
  }
}
