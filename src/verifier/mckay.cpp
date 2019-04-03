#include "mckay.hpp"
#include "trie.hpp"

#include <iostream>
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
    ;

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

propagation_list trieMcKay(Trie *trie, DiffInfo *diffInfo,
                           Graphinfo *cAfterGraph, Graphinfo *cBeforeGraph) {
  int gapOfGlobalRootMemID =
      cBeforeGraph->globalRootMemID - cAfterGraph->globalRootMemID;
  int stepOfPropagation;
  Bool verticesAreCompletelySorted =
      trie->propagate(diffInfo, cAfterGraph, cBeforeGraph, gapOfGlobalRootMemID,
                      &stepOfPropagation);
  if (IS_DIFFERENCE_APPLICATION_MODE && verticesAreCompletelySorted && false) {
    return propagation_list();
  } else {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    for (auto i = cAfterGraph->cv->atoms.begin();
         i != cAfterGraph->cv->atoms.end(); ++i)
      std::cout << *(i->second->correspondingVertexInTrie) << std::endl;
    propagation_list propagationList;
    trie->conventionalPropagationList(trie->body, propagationList);
    std::cout << "###### before list propagate ######" << std::endl;
    std::cout << propagationList << std::endl;

    auto canonicalDiscreteRefinement =
        listMcKay(propagationList, cAfterGraph->cv, gapOfGlobalRootMemID);

    return canonicalDiscreteRefinement;
  }
}
