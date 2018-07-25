#include "mckay.hpp"
#include "trie.hpp"

void initializeDisjointSetForestsOfPropagationList(List *pList){
  ListBody *iteratorCell;

  for(iteratorCell=pList->sentinel->next;iteratorCell!=pList->sentinel;iteratorCell=iteratorCell->next){
    if(iteratorCell->value != CLASS_SENTINEL){
      InheritedVertex *iVertex = (InheritedVertex *)iteratorCell->value;
      initializeDisjointSetForest(iVertex->equivalenceClassOfIsomorphism);
    }
  }

  return;
}

#define IS_DISCRETE_LIST (NULL)

ListBody *firstNonTrivialCell(List *pList){
  ListBody *beginSentinel = pList->sentinel;
  ListBody *endSentinel = beginSentinel;

  do{
    endSentinel = getNextSentinel(beginSentinel);

    if(beginSentinel->next->next != endSentinel){
      return beginSentinel;
    }
    beginSentinel = endSentinel;
  }while(endSentinel != pList->sentinel);

  return IS_DISCRETE_LIST;
}

Order compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabelsInner(InheritedVertex *iVertexA,InheritedVertex *iVertexB){
  if(iVertexA == CLASS_SENTINEL && iVertexB == CLASS_SENTINEL){
    return EQ;
  }else if(iVertexA == CLASS_SENTINEL && iVertexB != CLASS_SENTINEL){
    CHECKER("CLASS_SENTINEL is invalid\n");
    exit(EXIT_FAILURE);
  }else if(iVertexA != CLASS_SENTINEL && iVertexB == CLASS_SENTINEL){
    CHECKER("CLASS_SENTINEL is invalid\n");
    exit(EXIT_FAILURE);
  }else if(iVertexA->type < iVertexB->type){
    return LT;
  }else if(iVertexA->type > iVertexB->type){
    return GT;
  }else if(strcmp(iVertexA->name,iVertexB->name) < 0){
    return LT;
  }else if(strcmp(iVertexA->name,iVertexB->name) > 0){
    return GT;
  }else if(numIntStack(iVertexA->conventionalPropagationMemo) < numIntStack(iVertexB->conventionalPropagationMemo)){
    return LT;
  }else if(numIntStack(iVertexA->conventionalPropagationMemo) > numIntStack(iVertexB->conventionalPropagationMemo)){
    return GT;
  }else{
    int degree = numIntStack(iVertexA->conventionalPropagationMemo);
    int i;
    IntStack *iStackA = iVertexA->conventionalPropagationMemo;
    IntStack *iStackB = iVertexB->conventionalPropagationMemo;

    for(i=0;i<degree;i++){
      if(readIntStack(iStackA,i) < readIntStack(iStackB,i)){
        return LT;
      }else if(readIntStack(iStackA,i) > readIntStack(iStackB,i)){
        return GT;
      }
    }

    return EQ;
  }
}

Order compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabelsInnerCaster(void *iVertexA,void *iVertexB){
  return compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabelsInner((InheritedVertex *)iVertexA,(InheritedVertex *)iVertexB);
}

Order compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabels(List *listA,List *listB){
  return compareList(listA,listB,compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabelsInnerCaster);
}

void initializeInheritedVertexAdjacentLabels(InheritedVertex *iVertex){
  if(iVertex == CLASS_SENTINEL){
    return;
  }else{
    setIntStackEmpty(iVertex->conventionalPropagationMemo);

    return;
  }
}

void initializeInheritedVertexAdjacentLabelsCaster(void *iVertex){
  initializeInheritedVertexAdjacentLabels((InheritedVertex *)iVertex);

  return;
}

void freeInheritedVertexOfPreserveDiscretePropagationList(InheritedVertex *iVertex){
  if(iVertex != CLASS_SENTINEL){
    freeIntStack(iVertex->conventionalPropagationMemo);
    free(iVertex);
  }

  return;
}

void freeInheritedVertexOfPreserveDiscretePropagationListCaster(void *iVertex){
  freeInheritedVertexOfPreserveDiscretePropagationList((InheritedVertex *)iVertex);

  return;
}

void freePreserveDiscreteProapgationList(List *pdpList){
  freeListWithValues(pdpList,freeInheritedVertexOfPreserveDiscretePropagationListCaster);

  return;
}

void freePreserveDiscreteProapgationListCaster(void *pdpList){
  freePreserveDiscreteProapgationList((List *)pdpList);

  return;
}

Bool insertDiscretePropagationListOfInheritedVerticesWithAdjacentLabelToTable(RedBlackTree *discretePropagationListsOfInheritedVerticesWithAdjacentLabels,List *dpList,ConvertedGraph *cAfterGraph,int gapOfGlobalRootMemID){
  Bool isExisting;

  putLabelsToAdjacentVertices(dpList,cAfterGraph,gapOfGlobalRootMemID);
  List *preserveDPList = copyListWithValues(dpList,copyInheritedVertexCaster);
  forEachValueOfList(dpList,initializeInheritedVertexAdjacentLabelsCaster);

  KeyContainer key = makeDiscretePropagationListKey(preserveDPList);
  List *seniorDPList = (List *)searchRedBlackTree(discretePropagationListsOfInheritedVerticesWithAdjacentLabels,key);

  if(seniorDPList == NULL){
    insertRedBlackTree(discretePropagationListsOfInheritedVerticesWithAdjacentLabels,key,preserveDPList);
    isExisting = FALSE;
    return isExisting;
  }else{
    ListBody *iteratorCell = preserveDPList->sentinel->next;
    ListBody *iteratorCellSenior = seniorDPList->sentinel->next;

    while(iteratorCell != preserveDPList->sentinel){
      if(iteratorCell->value != CLASS_SENTINEL){
        InheritedVertex *iVertex = (InheritedVertex *)iteratorCell->value;
        InheritedVertex *iVertexSenior = (InheritedVertex *)iteratorCellSenior->value;

        unionDisjointSetForest(iVertex->equivalenceClassOfIsomorphism,iVertexSenior->equivalenceClassOfIsomorphism);
      }

      iteratorCell = iteratorCell->next;
      iteratorCellSenior = iteratorCellSenior->next;
    }

    freePreserveDiscreteProapgationList(preserveDPList);

    isExisting = TRUE;
    return isExisting;
  }
}

void discretePropagationListDump(List *dpList){
  listDump(dpList,inheritedVertexDumpCaster);
  fprintf(stdout,"\n");
  fprintf(stdout,"\n");

  return;
}

void discretePropagationListDumpCaster(void *dpList){
  discretePropagationListDump((List *)dpList);

  return;
}

Bool isNewSplit(ListBody *sentinelCell,ListBody *splitCell){
  ListBody *iteratorCell;

  for(iteratorCell=sentinelCell->next;iteratorCell!=splitCell;iteratorCell=iteratorCell->next){
    InheritedVertex *splitIVertex = (InheritedVertex *)splitCell->value;
    InheritedVertex *iteratorIVertex = (InheritedVertex *)iteratorCell->value;

    if(isInSameDisjointSetForest(splitIVertex->equivalenceClassOfIsomorphism,iteratorIVertex->equivalenceClassOfIsomorphism)){
      return FALSE;
    }
  }

  return TRUE;
}

Bool listMcKayInner(List *propagationListOfInheritedVertices,ConvertedGraph *cAfterGraph,int gapOfGlobalRootMemID,RedBlackTree *discretePropagationListsOfInheritedVerticesWithAdjacentLabels){
  Bool isUsefulBranch = TRUE;

  List *stabilizer = copyList(propagationListOfInheritedVertices);
  getStableRefinementOfConventionalPropagationList(stabilizer,cAfterGraph,gapOfGlobalRootMemID);

  /*
  CHECKER("###### after stable refinement ######\n");
  listDump(stabilizer,inheritedVertexDumpCaster),fprintf(stdout,"\n");
  //*/

  ListBody *beginSentinel = firstNonTrivialCell(stabilizer);

  if(beginSentinel == IS_DISCRETE_LIST){
    isUsefulBranch = !insertDiscretePropagationListOfInheritedVerticesWithAdjacentLabelToTable(discretePropagationListsOfInheritedVerticesWithAdjacentLabels,stabilizer,cAfterGraph,gapOfGlobalRootMemID);
  }else{
    Bool isFirstLoop = TRUE;

    ListBody *endSentinel = getNextSentinel(beginSentinel);
    ListBody *sentinelCell = makeCell(CLASS_SENTINEL);
    insertNextCell(beginSentinel,sentinelCell);
    ListBody *iteratorCell;

    for(iteratorCell=sentinelCell;iteratorCell->next!=endSentinel;iteratorCell=iteratorCell->next){
      ListBody *splitCell = iteratorCell->next;

      if(isNewSplit(sentinelCell,splitCell)){
        cutCell(splitCell);
        insertNextCell(beginSentinel,splitCell);

        Bool isUsefulChild = listMcKayInner(stabilizer,cAfterGraph,gapOfGlobalRootMemID,discretePropagationListsOfInheritedVerticesWithAdjacentLabels);

        cutCell(splitCell);
        insertNextCell(iteratorCell,splitCell);

        if(isFirstLoop){
          isFirstLoop = FALSE;
          if(!isUsefulChild){
            isUsefulBranch = FALSE;
            break;
          }else{
            isUsefulBranch = TRUE;
          }
        }
      }
    }
  }

  freeList(stabilizer);

  return isUsefulBranch;
}

List *listMcKay(List *propagationListOfInheritedVertices,ConvertedGraph *cAfterGraph,int gapOfGlobalRootMemID){
  if(isEmptyList(propagationListOfInheritedVertices)){
    List *canonicalDiscreteRefinement = copyList(propagationListOfInheritedVertices);
    return canonicalDiscreteRefinement;
  }else{
    initializeDisjointSetForestsOfPropagationList(propagationListOfInheritedVertices);
    RedBlackTree *discretePropagationListsOfInheritedVerticesWithAdjacentLabels = makeRedBlackTree();

    classifyConventionalPropagationListWithAttribute(propagationListOfInheritedVertices,cAfterGraph,gapOfGlobalRootMemID);

    /*
    CHECKER("###### after attribute classifying ######\n");
    listDump(propagationListOfInheritedVertices,inheritedVertexDumpCaster),fprintf(stdout,"\n");
    //*/

    listMcKayInner(propagationListOfInheritedVertices,cAfterGraph,gapOfGlobalRootMemID,discretePropagationListsOfInheritedVerticesWithAdjacentLabels);

    List *canonicalDiscreteRefinement = (List *)copyListWithValues((List *)minimumElementOfRedBlackTree(discretePropagationListsOfInheritedVerticesWithAdjacentLabels),copyInheritedVertexCaster);

    /*
    CHECKER("########### candidates of canonical discrete refinement ###########\n");
    redBlackTreeValueDump(discretePropagationListsOfInheritedVerticesWithAdjacentLabels,discretePropagationListDumpCaster);
    //*/

    freeRedBlackTreeWithValue(discretePropagationListsOfInheritedVerticesWithAdjacentLabels,freePreserveDiscreteProapgationListCaster);

    return canonicalDiscreteRefinement;
  }
}

Bool checkIsomorphismValidity(DynamicArray *slimKeyCollection,RedBlackTree *McKayKeyCollection,List *canonicalDiscreteRefinement,int stateID){
  Bool isValid = TRUE;

  if(stateID != 0){
    KeyContainer key = makeDiscretePropagationListKey(canonicalDiscreteRefinement);
    CollectionInt seniorID = (CollectionInt)searchRedBlackTree(McKayKeyCollection,key) - 1;
    if(seniorID != -1){
      if(stateID != seniorID){
        fprintf(stdout,"stateID is wrong.\n");
        fprintf(stdout,"juniorStateID is %d\n",stateID);
        fprintf(stdout,"seniorStateID is %d\n",seniorID);
        isValid = FALSE;
        return isValid;
      }
    }else{
      insertRedBlackTree(McKayKeyCollection,key,(void *)(stateID+1));
    }

    List *seniorDiscreteRefinement = (List *)readDynamicArray(slimKeyCollection,stateID);
    if(seniorDiscreteRefinement != NULL){
      if(compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabels(canonicalDiscreteRefinement,seniorDiscreteRefinement) != EQ){
        printf("adjacency list is wrong.\n");
        isValid = FALSE;
        return isValid;
      }else{
        freePreserveDiscreteProapgationList(canonicalDiscreteRefinement);
      }
    }else{
      writeDynamicArray(slimKeyCollection,stateID,canonicalDiscreteRefinement);
    }
  }

  return isValid;
}

void trieMcKay(Trie *trie,DiffInfo *diffInfo,Graphinfo *cAfterGraph,Graphinfo *cBeforeGraph) {
  int gapOfGlobalRootMemID = cBeforeGraph->globalRootMemID - cAfterGraph->globalRootMemID;
  int stepOfPropagation;
  Bool verticesAreCompletelySorted = triePropagate(trie,diffInfo,cAfterGraph,cBeforeGraph,gapOfGlobalRootMemID,&stepOfPropagation);
  // if(IS_DIFFERENCE_APPLICATION_MODE && verticesAreCompletelySorted){
  //   if(measure){
  //     countOfSortedInTrie++;
  //   }
  //   /* printf("%s:%d\n", __FUNCTION__, __LINE__); */
  //   return makeList();
  // }else{
  //   if(measure){
  //     countOfNotSortedInTrie++;
  //   }

  //   beforeTime = get_dtime();

  //   List *propagationListOfInheritedVertices = makeConventionalPropagationList(trie,stepOfPropagation);

  //   /*
  //      CHECKER("###### before list propagate ######\n");

  //   //*/
  //   /* printf("%s:%d\n", __FUNCTION__, __LINE__); */
  //   /* listDump(propagationListOfInheritedVertices,inheritedVertexDumpCaster),fprintf(stdout,"\n"); */
  //   List *canonicalDiscreteRefinement = listMcKay(propagationListOfInheritedVertices,cAfterGraph,gapOfGlobalRootMemID);

  //   /*
  //      CHECKER("###### after list propagate ######\n");
  //      listDump(canonicalDiscreteRefinement,inheritedVertexDumpCaster),fprintf(stdout,"\n");
  //   //*/

  //   freeList(propagationListOfInheritedVertices);

  //   afterTime = get_dtime();
  //   if(measure){
  //     listMcKayTime += afterTime - beforeTime;
  //   }

  //   return canonicalDiscreteRefinement;
  // }
}
