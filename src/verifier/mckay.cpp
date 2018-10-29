#include "mckay.hpp"
#include "trie.hpp"

template <typename List>
void initializeDisjointSetForestsOfPropagationList(List *pList) {
  for (auto iteratorCell = pList->sentinel->next;
       iteratorCell != pList->sentinel; iteratorCell = iteratorCell->next) {
    if (iteratorCell->value != CLASS_SENTINEL) {
      InheritedVertex *iVertex = (InheritedVertex *)iteratorCell->value;
      initializeDisjointSetForest(iVertex->equivalenceClassOfIsomorphism);
    }
  }

  return;
}

#define IS_DISCRETE_LIST (NULL)

vertex_list::iterator firstNonTrivialCell(vertex_list *pList) {
  auto beginSentinel = std::begin(*pList);
  auto endSentinel = beginSentinel;

  do {
    endSentinel = getNextSentinel(beginSentinel);

    if (std::next(beginSentinel, 2) != endSentinel) {
      return beginSentinel;
    }
    beginSentinel = endSentinel;
  } while (endSentinel != pList->sentinel);

  return IS_DISCRETE_LIST;
}

Order compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabelsInner(
    InheritedVertex *iVertexA, InheritedVertex *iVertexB) {
  if (iVertexA == CLASS_SENTINEL && iVertexB == CLASS_SENTINEL) {
    return EQ;
  } else if (iVertexA == CLASS_SENTINEL && iVertexB != CLASS_SENTINEL) {
    CHECKER("CLASS_SENTINEL is invalid\n");
    exit(EXIT_FAILURE);
  } else if (iVertexA != CLASS_SENTINEL && iVertexB == CLASS_SENTINEL) {
    CHECKER("CLASS_SENTINEL is invalid\n");
    exit(EXIT_FAILURE);
  } else if (iVertexA->type < iVertexB->type) {
    return LT;
  } else if (iVertexA->type > iVertexB->type) {
    return GT;
  } else if (strcmp(iVertexA->name, iVertexB->name) < 0) {
    return LT;
  } else if (strcmp(iVertexA->name, iVertexB->name) > 0) {
    return GT;
  } else if (numStack(iVertexA->conventionalPropagationMemo) <
             numStack(iVertexB->conventionalPropagationMemo)) {
    return LT;
  } else if (numStack(iVertexA->conventionalPropagationMemo) >
             numStack(iVertexB->conventionalPropagationMemo)) {
    return GT;
  } else {
    int degree = numStack(iVertexA->conventionalPropagationMemo);
    int i;
    std::vector<int> *iStackA = iVertexA->conventionalPropagationMemo;
    std::vector<int> *iStackB = iVertexB->conventionalPropagationMemo;

    for (i = 0; i < degree; i++) {
      if (readStack(iStackA, i) < readStack(iStackB, i)) {
        return LT;
      } else if (readStack(iStackA, i) > readStack(iStackB, i)) {
        return GT;
      }
    }

    return EQ;
  }
}

Order compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabelsInnerCaster(
    void *iVertexA, void *iVertexB) {
  return compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabelsInner(
      (InheritedVertex *)iVertexA, (InheritedVertex *)iVertexB);
}

void initializeInheritedVertexAdjacentLabels(InheritedVertex *iVertex) {
  if (iVertex == CLASS_SENTINEL) {
    return;
  } else {
    iVertex->conventionalPropagationMemo->clear();

    return;
  }
}

void initializeInheritedVertexAdjacentLabelsCaster(void *iVertex) {
  initializeInheritedVertexAdjacentLabels((InheritedVertex *)iVertex);

  return;
}

void freeInheritedVertexOfPreserveDiscretePropagationList(
    InheritedVertex *iVertex) {
  if (iVertex != CLASS_SENTINEL) {
    freeStack(iVertex->conventionalPropagationMemo);
    free(iVertex);
  }

  return;
}

void freeInheritedVertexOfPreserveDiscretePropagationListCaster(void *iVertex) {
  freeInheritedVertexOfPreserveDiscretePropagationList(
      (InheritedVertex *)iVertex);

  return;
}

template <typename List>
void freePreserveDiscreteProapgationList(List *pdpList) {
  freeListWithValues(
      pdpList, freeInheritedVertexOfPreserveDiscretePropagationListCaster);

  return;
}

void freePreserveDiscreteProapgationListCaster(void *pdpList) {
  freePreserveDiscreteProapgationList((List__<void *> *)pdpList);

  return;
}

Bool insertDiscretePropagationListOfInheritedVerticesWithAdjacentLabelToTable(
    RedBlackTree__<KeyContainer__<vertex_list *>, vertex_list *>
        *discretePropagationListsOfInheritedVerticesWithAdjacentLabels,
    vertex_list *dpList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID) {
  Bool isExisting;

  putLabelsToAdjacentVertices(dpList, cAfterGraph, gapOfGlobalRootMemID);
  vertex_list *preserveDPList = copyListWithValues(dpList, copyInheritedVertexCaster);
  forEachValueOfList(dpList, initializeInheritedVertexAdjacentLabelsCaster);

  auto key = makeDiscretePropagationListKey(preserveDPList);
  auto seniorDPList = (vertex_list *)searchRedBlackTree(
      discretePropagationListsOfInheritedVerticesWithAdjacentLabels, key);

  if (seniorDPList == NULL) {
    insertRedBlackTree(
        discretePropagationListsOfInheritedVerticesWithAdjacentLabels, key,
        preserveDPList);
    isExisting = FALSE;
    return isExisting;
  } else {
    auto iteratorCell = preserveDPList->sentinel->next;
    auto iteratorCellSenior = seniorDPList->sentinel->next;

    while (iteratorCell != preserveDPList->sentinel) {
      if (iteratorCell->value != CLASS_SENTINEL) {
        InheritedVertex *iVertex = (InheritedVertex *)iteratorCell->value;
        InheritedVertex *iVertexSenior =
            (InheritedVertex *)iteratorCellSenior->value;

        unionDisjointSetForest(iVertex->equivalenceClassOfIsomorphism,
                               iVertexSenior->equivalenceClassOfIsomorphism);
      }

      iteratorCell = iteratorCell->next;
      iteratorCellSenior = iteratorCellSenior->next;
    }

    freePreserveDiscreteProapgationList(preserveDPList);

    isExisting = TRUE;
    return isExisting;
  }
}

template <typename List> void discretePropagationListDump(List *dpList) {
  listDump(dpList, inheritedVertexDumpCaster);
  fprintf(stdout, "\n");
  fprintf(stdout, "\n");

  return;
}

void discretePropagationListDumpCaster(void *dpList) {
  discretePropagationListDump((List__<void *> *)dpList);

  return;
}

Bool isNewSplit(vertex_list::iterator sentinelCell,
                vertex_list::iterator splitCell) {
  for (auto iteratorCell = std::next(sentinelCell, 1); iteratorCell != splitCell;
       iteratorCell = std::next(iteratorCell, 1)) {
    InheritedVertex *splitIVertex = (InheritedVertex *)*splitCell;
    InheritedVertex *iteratorIVertex = (InheritedVertex *)*iteratorCell;

    if (isInSameDisjointSetForest(
            splitIVertex->equivalenceClassOfIsomorphism,
            iteratorIVertex->equivalenceClassOfIsomorphism)) {
      return FALSE;
    }
  }

  return TRUE;
}

Bool listMcKayInner(
    vertex_list *propagationListOfInheritedVertices, ConvertedGraph *cAfterGraph,
    int gapOfGlobalRootMemID,
    RedBlackTree__<KeyContainer__<vertex_list *>, vertex_list *>
        *discretePropagationListsOfInheritedVerticesWithAdjacentLabels) {
  Bool isUsefulBranch = TRUE;

  auto stabilizer = copyList(propagationListOfInheritedVertices);
  getStableRefinementOfConventionalPropagationList(stabilizer, cAfterGraph,
                                                   gapOfGlobalRootMemID);

  /*
  CHECKER("###### after stable refinement ######\n");
  listDump(stabilizer,inheritedVertexDumpCaster),fprintf(stdout,"\n");
  //*/

  auto beginSentinel = firstNonTrivialCell(stabilizer);

  if (beginSentinel == IS_DISCRETE_LIST) {
    isUsefulBranch =
        !insertDiscretePropagationListOfInheritedVerticesWithAdjacentLabelToTable(
            discretePropagationListsOfInheritedVerticesWithAdjacentLabels,
            stabilizer, cAfterGraph, gapOfGlobalRootMemID);
  } else {
    Bool isFirstLoop = TRUE;

    auto endSentinel = getNextSentinel(beginSentinel);
    auto sentinelCell = vertex_list::iterator(CLASS_SENTINEL);
    insertNextCell(beginSentinel, sentinelCell);

    for (auto iteratorCell = sentinelCell; std::next(iteratorCell, 1) != endSentinel;
         iteratorCell = std::next(iteratorCell, 1)) {
      auto splitCell = std::next(iteratorCell, 1);

      if (isNewSplit(sentinelCell, splitCell)) {
        cutCell(splitCell);
        insertNextCell(beginSentinel, splitCell);

        Bool isUsefulChild = listMcKayInner(
            stabilizer, cAfterGraph, gapOfGlobalRootMemID,
            discretePropagationListsOfInheritedVerticesWithAdjacentLabels);

        cutCell(splitCell);
        insertNextCell(iteratorCell, splitCell);

        if (isFirstLoop) {
          isFirstLoop = FALSE;
          if (!isUsefulChild) {
            isUsefulBranch = FALSE;
            break;
          } else {
            isUsefulBranch = TRUE;
          }
        }
      }
    }
  }

  freeList(stabilizer);

  return isUsefulBranch;
}

vertex_list *listMcKay(vertex_list *propagationListOfInheritedVertices,
                ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID) {
  if (propagationListOfInheritedVertices->empty()) {
    vertex_list *canonicalDiscreteRefinement =
        copyList(propagationListOfInheritedVertices);
    return canonicalDiscreteRefinement;
  } else {
    initializeDisjointSetForestsOfPropagationList(
        propagationListOfInheritedVertices);
    auto discretePropagationListsOfInheritedVerticesWithAdjacentLabels =
        new RedBlackTree__<KeyContainer__<vertex_list *>, vertex_list *>();

    classifyConventionalPropagationListWithAttribute(
        propagationListOfInheritedVertices, cAfterGraph, gapOfGlobalRootMemID);

    /*
    CHECKER("###### after attribute classifying ######\n");
    listDump(propagationListOfInheritedVertices,inheritedVertexDumpCaster),fprintf(stdout,"\n");
    //*/

    listMcKayInner(
        propagationListOfInheritedVertices, cAfterGraph, gapOfGlobalRootMemID,
        discretePropagationListsOfInheritedVerticesWithAdjacentLabels);

    vertex_list *canonicalDiscreteRefinement = (vertex_list *)copyListWithValues(
        minimumElementOfRedBlackTree(
            discretePropagationListsOfInheritedVerticesWithAdjacentLabels),
        copyInheritedVertexCaster);

    /*
    CHECKER("########### candidates of canonical discrete refinement
    ###########\n");
    redBlackTreeValueDump(discretePropagationListsOfInheritedVerticesWithAdjacentLabels,discretePropagationListDumpCaster);
    //*/

    freeRedBlackTreeWithValue(
        discretePropagationListsOfInheritedVerticesWithAdjacentLabels,
        freePreserveDiscreteProapgationList);

    return canonicalDiscreteRefinement;
  }
}

template <typename List>
Bool checkIsomorphismValidity(unbound_vector<List *> *slimKeyCollection,
                              RedBlackTree *McKayKeyCollection,
                              List *canonicalDiscreteRefinement, int stateID) {
  Bool isValid = TRUE;

  if (stateID != 0) {
    auto key = makeDiscretePropagationListKey(canonicalDiscreteRefinement);
    CollectionInt seniorID =
        (CollectionInt)searchRedBlackTree(McKayKeyCollection, key) - 1;
    if (seniorID != -1) {
      if (stateID != seniorID) {
        fprintf(stdout, "stateID is wrong.\n");
        fprintf(stdout, "juniorStateID is %d\n", stateID);
        fprintf(stdout, "seniorStateID is %d\n", seniorID);
        isValid = FALSE;
        return isValid;
      }
    } else {
      insertRedBlackTree(McKayKeyCollection, key, (void *)(stateID + 1));
    }

    List *seniorDiscreteRefinement = slimKeyCollection->read(stateID);
    if (seniorDiscreteRefinement != NULL) {
      if (compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabels(
              canonicalDiscreteRefinement, seniorDiscreteRefinement) != EQ) {
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

List__<void *> *trieMcKay(Trie *trie, DiffInfo *diffInfo, Graphinfo *cAfterGraph,
                Graphinfo *cBeforeGraph) {
  int gapOfGlobalRootMemID =
      cBeforeGraph->globalRootMemID - cAfterGraph->globalRootMemID;
  int stepOfPropagation;
  Bool verticesAreCompletelySorted =
      triePropagate(trie, diffInfo, cAfterGraph, cBeforeGraph,
                    gapOfGlobalRootMemID, &stepOfPropagation);
  if (IS_DIFFERENCE_APPLICATION_MODE && verticesAreCompletelySorted) {
    /* printf("%s:%d\n", __FUNCTION__, __LINE__); */
    return new List__<void *>();
  } else {

    List__<void *> *propagationListOfInheritedVertices =
        makeConventionalPropagationList(trie, stepOfPropagation);

    /*
       CHECKER("###### before list propagate ######\n");

    //*/
    /* printf("%s:%d\n", __FUNCTION__, __LINE__); */
    /* listDump(propagationListOfInheritedVertices,inheritedVertexDumpCaster),fprintf(stdout,"\n");
     */
    List__<void *> *canonicalDiscreteRefinement =
        listMcKay(propagationListOfInheritedVertices, cAfterGraph->cv,
                  gapOfGlobalRootMemID);

    /*
       CHECKER("###### after list propagate ######\n");
       listDump(canonicalDiscreteRefinement,inheritedVertexDumpCaster),fprintf(stdout,"\n");
    //*/

    freeList(propagationListOfInheritedVertices);

    return canonicalDiscreteRefinement;
  }
}
