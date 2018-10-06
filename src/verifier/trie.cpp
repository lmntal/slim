#include "trie.hpp"
#include <iterator>
#include <stack>
#include <vector>

HashString *makeHashString() {
  HashString *ret = (HashString *)malloc(sizeof(HashString));

  ret->creditIndex = 0;
  ret->body = makeDynamicArray();

  return ret;
}

void freeHashString(HashString *hashString) {
  freeDynamicArrayAndValues(hashString->body, free);
  free(hashString);

  return;
}

Hash stringHashValue(char *string) {
  Hash ret = OFFSET_BASIS;
  int i;

  for (i = 0; string[i] != '\0'; i++) {
    ret ^= (Hash)string[i];
    ret *= FNV_PRIME;
  }

  return ret;
}

Hash doubleHashValue(double dbl) {
  union {
    double d;
    Hash h;
  } uni;

  uni.d = dbl;

  return uni.h;
}

Hash integerHashValue(int i) {
  union {
    int i;
    Hash h;
  } uni;

  uni.i = i;

  return uni.h;
}

Hash initialHashValue(ConvertedGraphVertex *cVertex) {
  Hash ret = OFFSET_BASIS;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  convertedGraphVertexDump(cVertex);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  switch (cVertex->type) {
  case convertedAtom:
    ret *= FNV_PRIME;
    ret ^= cVertex->type;
    ret *= FNV_PRIME;
    ret ^= stringHashValue(cVertex->name);
    ret *= FNV_PRIME;
    ret ^= numStack(cVertex->links);
    ret *= FNV_PRIME;

    return ret;
    break;
  case convertedHyperLink:
    ret *= FNV_PRIME;
    ret ^= cVertex->type;
    ret *= FNV_PRIME;
    ret ^= stringHashValue(cVertex->name);
    ret *= FNV_PRIME;

    return ret;
    break;
  default:
    CHECKER("unexpected type\n");
    exit(EXIT_FAILURE);
    break;
  }
}

template <typename S>
Hash linkHashValue(LMNtalLink *link, int index, ConvertedGraph *cGraph,
                   int gapOfGlobalRootMemID, S *fixCreditIndexStack) {
  Hash ret;

  switch (link->attr) {
  case INTEGER_ATTR:
    ret = OFFSET_BASIS;
    ret *= FNV_PRIME;
    ret ^= link->attr;
    ret *= FNV_PRIME;
    ret ^= link->data.ID;
    return ret;
    break;
  case HYPER_LINK_ATTR:
    ret = OFFSET_BASIS;
    ret *= FNV_PRIME;
    ret ^= link->attr;
    ret *= FNV_PRIME;
    ret ^=
        callHashValue(((ConvertedGraphVertex *)readDynamicArray(
                           cGraph->hyperlinks, link->data.ID))
                          ->correspondingVertexInTrie,
                      index, cGraph, gapOfGlobalRootMemID, fixCreditIndexStack);
    return ret;
    break;
  case GLOBAL_ROOT_MEM_ATTR:
    ret = OFFSET_BASIS;
    ret *= FNV_PRIME;
    ret ^= link->attr;
    ret *= FNV_PRIME;
    ret ^= link->data.ID;
    return ret;
    break;
  case DOUBLE_ATTR: // LMNtalLink非対応
  case STRING_ATTR: // LMNtalLink非対応
  default:
    if (link->attr < 128) {
      ret = OFFSET_BASIS;
      ret *= FNV_PRIME;
      ret ^= link->attr;
      ret *= FNV_PRIME;
      ret ^= callHashValue(((ConvertedGraphVertex *)readDynamicArray(
                                cGraph->atoms, link->data.ID))
                               ->correspondingVertexInTrie,
                           index, cGraph, gapOfGlobalRootMemID,
                           fixCreditIndexStack);
      return ret;
    } else {
      CHECKER("unexpected type\n");
      exit(EXIT_FAILURE);
    }
    break;
  }
}

template <typename S>
Hash adjacentHashValue(ConvertedGraphVertex *cVertex, int index,
                       ConvertedGraph *cGraph, int gapOfGlobalRootMemID,
                       S *fixCreditIndexStack) {
  Hash ret;
  Hash sum, mul;
  Hash tmp;
  int i;

  switch (cVertex->type) {
  case convertedAtom:
    ret = OFFSET_BASIS;
    for (i = 0; i < numStack(cVertex->links); i++) {
      ret *= FNV_PRIME;
      ret ^=
          linkHashValue((LMNtalLink *)readStack(cVertex->links, i), index - 1,
                        cGraph, gapOfGlobalRootMemID, fixCreditIndexStack);
    }

    return ret;
    break;
  case convertedHyperLink:
    sum = ADD_INIT;
    mul = MUL_INIT;
    for (i = 0; i < numStack(cVertex->links); i++) {
      tmp = linkHashValue((LMNtalLink *)readStack(cVertex->links, i), index - 1,
                          cGraph, gapOfGlobalRootMemID, fixCreditIndexStack);
      sum += tmp;
      mul *= (tmp * 2 + 1);
    }
    ret = sum ^ mul;

    return ret;
    break;
  default:
    CHECKER("unexpected type\n");
    exit(EXIT_FAILURE);
    break;
  }
}

template <typename S>
void pushInheritedVertexIntoFixCreditIndexStackWithoutOverlap(
    S *fixCreditIndexStack, InheritedVertex *iVertex) {
  if (!iVertex->isPushedIntoFixCreditIndex) {
    pushStack(fixCreditIndexStack, iVertex);
    iVertex->isPushedIntoFixCreditIndex = TRUE;
  }

  return;
}

template <typename S>
InheritedVertex *popInheritedVertexFromFixCreditIndexStackWithoutOverlap(
    S *fixCreditIndexStack) {
  InheritedVertex *iVertex = (InheritedVertex *)popStack(fixCreditIndexStack);
  iVertex->isPushedIntoFixCreditIndex = FALSE;

  return iVertex;
}

template <typename S>
void fixCreditIndex(S *fixCreditIndexStack, ConvertedGraph *cAfterGraph,
                    int gapOfGlobalRootMemID) {
  while (!fixCreditIndexStack->empty()) {
    InheritedVertex *iVertex =
        popInheritedVertexFromFixCreditIndexStackWithoutOverlap(
            fixCreditIndexStack);
    TrieBody *ownerNode = iVertex->ownerNode;
    HashString *hashString = iVertex->hashString;

    hashString->creditIndex = ownerNode->depth;
  }

  return;
}

template <typename S>
Hash callHashValue(InheritedVertex *iVertex, int index,
                   ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID,
                   S *fixCreditIndexStack) {
  HashString *hashString = iVertex->hashString;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  if (index < 0) {
    return 0;
  } else if (index < hashString->creditIndex) {
    return ((KeyContainer *)readDynamicArray(hashString->body, index))->u.ui32;
  } else if (index == 0) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    Hash tmp = initialHashValue(correspondingVertexInConvertedGraph(
        iVertex, cAfterGraph, gapOfGlobalRootMemID));
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    KeyContainer *old =
        (KeyContainer *)readDynamicArray(hashString->body, index);
    writeDynamicArray(hashString->body, index, (void *)allocKey(makeUInt32Key(tmp)));
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    if (old != NULL) {
      free(old);
    }
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    hashString->creditIndex = 1;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    pushInheritedVertexIntoFixCreditIndexStackWithoutOverlap(
        fixCreditIndexStack, iVertex);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    return tmp;
  } else {
    Hash prevMyHash = callHashValue(iVertex, index - 1, cAfterGraph,
                                    gapOfGlobalRootMemID, fixCreditIndexStack);
    Hash adjacentHash = adjacentHashValue(
        correspondingVertexInConvertedGraph(iVertex, cAfterGraph,
                                            gapOfGlobalRootMemID),
        index, cAfterGraph, gapOfGlobalRootMemID, fixCreditIndexStack);
    Hash newMyHash = (FNV_PRIME * prevMyHash) ^ adjacentHash;
    KeyContainer *old =
        (KeyContainer *)readDynamicArray(hashString->body, index);
    writeDynamicArray(hashString->body, index,
                      (void *)allocKey(makeUInt32Key(newMyHash)));
    if (old != NULL) {
      free(old);
    }
    hashString->creditIndex = index + 1;
    pushInheritedVertexIntoFixCreditIndexStackWithoutOverlap(
        fixCreditIndexStack, iVertex);
    return newMyHash;
  }
}

ConvertedGraphVertex *
getConvertedVertexFromGraphAndIDAndType(ConvertedGraph *cGraph, int ID,
                                        ConvertedGraphVertexType type) {
  switch (type) {
  case convertedAtom:
    return (ConvertedGraphVertex *)readDynamicArray(cGraph->atoms, ID);
    break;
  case convertedHyperLink:
    return (ConvertedGraphVertex *)readDynamicArray(cGraph->hyperlinks, ID);
    break;
  default:
    CHECKER("unexpected vertex type\n");
    exit(EXIT_FAILURE);
    break;
  }
}

ConvertedGraphVertex *
correspondingVertexInConvertedGraph(InheritedVertex *iVertex,
                                    ConvertedGraph *cAfterGraph,
                                    int gapOfGlobalRootMemID) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  int afterID = iVertex->beforeID + gapOfGlobalRootMemID;

  switch (iVertex->type) {
  case convertedAtom:
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    convertedGraphDump(cAfterGraph);
    printf("afterID=%d\n", afterID);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    convertedGraphVertexDump(
        (ConvertedGraphVertex *)readDynamicArray(cAfterGraph->atoms, afterID));
    return (ConvertedGraphVertex *)readDynamicArray(cAfterGraph->atoms,
                                                    afterID);
    break;
  case convertedHyperLink:
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    return (ConvertedGraphVertex *)readDynamicArray(cAfterGraph->hyperlinks,
                                                    afterID);
    break;
  default:
    CHECKER("unexpected vertex type\n");
    exit(EXIT_FAILURE);
    break;
  }
}

template <typename S1, typename S2>
void getNextDistanceConvertedVertices(S1 BFSStack,
                                      S2 initializeConvertedVerticesStack,
                                      ConvertedGraph *cAfterGraph) {
  S1 nextBFSStack = new std::vector<ConvertedGraphVertex *>();

  while (!BFSStack->empty()) {
    ConvertedGraphVertex *cVertex = (ConvertedGraphVertex *)popStack(BFSStack);

    int i;
    for (i = 0; i < numStack(cVertex->links); i++) {
      LMNtalLink *link = (LMNtalLink *)readStack(cVertex->links, i);
      ConvertedGraphVertex *adjacentVertex;

      switch (link->attr) {
      case INTEGER_ATTR:
        break;
      case DOUBLE_ATTR:
        break;
      case STRING_ATTR:
        break;
      case HYPER_LINK_ATTR:
        adjacentVertex = (ConvertedGraphVertex *)readDynamicArray(
            cAfterGraph->hyperlinks, link->data.ID);
        if (!adjacentVertex->isVisitedInBFS) {
          pushStack(nextBFSStack, adjacentVertex);
          adjacentVertex->isVisitedInBFS = TRUE;
        }
        break;
      case GLOBAL_ROOT_MEM_ATTR:
        break;
      default:
        if (link->attr < 128) {
          adjacentVertex = (ConvertedGraphVertex *)readDynamicArray(
              cAfterGraph->atoms, link->data.ID);
          if (!adjacentVertex->isVisitedInBFS) {
            pushStack(nextBFSStack, adjacentVertex);
            adjacentVertex->isVisitedInBFS = TRUE;
          }
        } else {
          CHECKER("unexpected vertex type\n");
          exit(EXIT_FAILURE);
        }
        break;
      }
    }

    pushStack(initializeConvertedVerticesStack, cVertex);
  }

  swapStack(nextBFSStack, BFSStack);
  freeStack(nextBFSStack);

  return;
}

int compareTrieLeaves(TrieBody *a, TrieBody *b) {
  if (a == b) {
    return 0;
  } else if (a->key.u.ui32 < b->key.u.ui32) {
    return -1;
  } else if (a->key.u.ui32 > b->key.u.ui32) {
    return 1;
  } else {
    int depthA = a->depth;
    int depthB = b->depth;
    HashString *hStringA =
        ((InheritedVertex *)a->inheritedVertices->front())->hashString;
    HashString *hStringB =
        ((InheritedVertex *)b->inheritedVertices->front())->hashString;

    int i;
    for (i = 0;; i++) {
      if (i >= depthA || i >= depthB) {
        CHECKER("unexpected case\n");
        exit(EXIT_FAILURE);
      }

      Hash hashA =
          ((KeyContainer *)readDynamicArray(hStringA->body, i))->u.ui32;
      Hash hashB =
          ((KeyContainer *)readDynamicArray(hStringB->body, i))->u.ui32;

      if (hashA < hashB) {
        return -1;
      } else if (hashA > hashB) {
        return 1;
      } else {
        CHECKER("unexpected case\n");
        exit(EXIT_FAILURE);
      }
    }
  }
}

void freeInheritedVertex(InheritedVertex *iVertex) {
  freeHashString(iVertex->hashString);
  freeStack(iVertex->conventionalPropagationMemo);
  freeDisjointSetForest(iVertex->equivalenceClassOfIsomorphism);
  free(iVertex);

  return;
}

TrieBody *makeTrieBody() {
  TrieBody *ret = (TrieBody *)malloc(sizeof(TrieBody));
  ret->key.type = key_null;
  ret->inheritedVertices = makeList();
  ret->parent = NULL;
  ret->children = makeRedBlackTree();
  ret->depth = -1;
  ret->isInfinitedDepth = FALSE;
  ret->isPushedIntoGoAheadStack = FALSE;

  return ret;
}

TerminationConditionInfo *makeTerminationConditionInfo() {
  TerminationConditionInfo *ret =
      (TerminationConditionInfo *)malloc(sizeof(TerminationConditionInfo));
  ret->distribution = makeOmegaArray();
  ret->increase = makeOmegaArray();

  return ret;
}

Trie *makeTrie() {
  Trie *ret = (Trie *)malloc(sizeof(Trie));
  ret->body = makeTrieBody();
  ret->body->depth = -1;
  ret->info = makeTerminationConditionInfo();

  return ret;
}

void freeTrieInnerCaster(void *body);

void freeTrieInner(TrieBody *body) {
  freeList(body->inheritedVertices);
  freeRedBlackTreeWithValue(body->children, freeTrieInnerCaster);
  free(body);

  return;
}

void freeTrieInnerCaster(void *body) {
  freeTrieInner((TrieBody *)body);

  return;
}

void freeTrieBody(TrieBody *body) {
  freeTrieInner(body);

  return;
}

void freeTerminationConditionInfo(TerminationConditionInfo *info) {
  freeOmegaArray(info->distribution);
  freeOmegaArray(info->increase);
  free(info);

  return;
}

void freeTrie(Trie *trie) {
  freeTrieInner(trie->body);
  freeTerminationConditionInfo(trie->info);
  free(trie);

  return;
}

void deleteTrieBody(TrieBody *body) {
  deleteRedBlackTree(body->parent->children, body->key);
  freeTrieBody(body);

  return;
}

void deleteTrieDescendantsAndItself(TrieBody *body);

void deleteTrieDescendantsAndItselfCaster(void *body) {
  deleteTrieDescendantsAndItself((TrieBody *)body);

  return;
}

void deleteTrieDescendantsAndItself(TrieBody *body) {
  if (body != NULL) {
    freeRedBlackTreeWithValue(body->children,
                              deleteTrieDescendantsAndItselfCaster);
    free(body);
  }

  return;
}

void deleteTrieDescendants(TrieBody *body) {
  freeRedBlackTreeWithValueInner(body->children->body,
                                 deleteTrieDescendantsAndItselfCaster);
  body->children->body = NULL;

  return;
}

template <typename S>
void pushTrieBodyIntoGoAheadStackWithoutOverlap(S *stack, TrieBody *body) {
  if (body != NULL) {
    if (!body->isPushedIntoGoAheadStack) {
      stack->push(body);
      body->isPushedIntoGoAheadStack = TRUE;
    }
  }
  return;
}

template <typename S>
TrieBody *popTrieBodyFromGoAheadStackWithoutOverlap(S *stack) {
  TrieBody *ret = popStack(stack);

  ret->isPushedIntoGoAheadStack = FALSE;

  return ret;
}

template <typename S>
void goBackProcessInnerManyCommonPrefixVertices(
    typename List::iterator targetCell, TrieBody *currentNode, S *goAheadStack,
    TerminationConditionInfo *tInfo, int targetDepth) {
  if (targetDepth == currentNode->depth) {
    pushCell(currentNode->inheritedVertices, targetCell);
    ((InheritedVertex *)targetCell->value)->ownerNode = currentNode;
    ((InheritedVertex *)targetCell->value)->hashString->creditIndex =
        currentNode->depth;
    pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, currentNode);
  } else {
    TrieBody *parent = currentNode->parent;

    goBackProcessInnerManyCommonPrefixVertices(targetCell, parent, goAheadStack,
                                               tInfo, targetDepth);
  }
}

template <typename S>
void goBackProcessInnerDoubleCommonPrefixVertices(
    typename List::iterator targetCell, typename List::iterator brotherCell,
    TrieBody *currentNode, TrieBody *prevNode, S *goAheadStack,
    TerminationConditionInfo *tInfo, int targetDepth) {
  if (targetDepth == currentNode->depth) {
    pushCell(currentNode->inheritedVertices, targetCell);
    ((InheritedVertex *)targetCell->value)->ownerNode = currentNode;
    ((InheritedVertex *)targetCell->value)->hashString->creditIndex =
        currentNode->depth;
    pushCell(prevNode->inheritedVertices, brotherCell);
    ((InheritedVertex *)brotherCell->value)->ownerNode = prevNode;
    ((InheritedVertex *)brotherCell->value)->hashString->creditIndex =
        prevNode->depth;
    incrementOmegaArray(tInfo->distribution, prevNode->depth);
    ((InheritedVertex *)brotherCell->value)->canonicalLabel.first =
        prevNode->key.u.ui32;

    pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, currentNode);
  } else if (isSingletonRedBlackTree(currentNode->children)) {
    TrieBody *parent = currentNode->parent;

    deleteTrieBody(prevNode);
    goBackProcessInnerDoubleCommonPrefixVertices(
        targetCell, brotherCell, parent, currentNode, goAheadStack, tInfo,
        targetDepth);
  } else {
    TrieBody *parent = currentNode->parent;

    pushCell(prevNode->inheritedVertices, brotherCell);
    ((InheritedVertex *)brotherCell->value)->ownerNode = prevNode;
    ((InheritedVertex *)brotherCell->value)->hashString->creditIndex =
        prevNode->depth;
    incrementOmegaArray(tInfo->distribution, prevNode->depth);
    ((InheritedVertex *)brotherCell->value)->canonicalLabel.first =
        prevNode->key.u.ui32;

    goBackProcessInnerManyCommonPrefixVertices(targetCell, parent, goAheadStack,
                                               tInfo, targetDepth);
  }
}

template <typename S>
void goBackProcessInnerSingleCommonPrefixVertex(
    typename List::iterator targetCell, TrieBody *currentNode, S *goAheadStack,
    TerminationConditionInfo *tInfo, int targetDepth) {
  if (targetDepth == currentNode->depth) {
    pushCell(currentNode->inheritedVertices, targetCell);
    ((InheritedVertex *)targetCell->value)->ownerNode = currentNode;
    ((InheritedVertex *)targetCell->value)->hashString->creditIndex =
        currentNode->depth;
    pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, currentNode);
  } else if (isSingletonRedBlackTree(currentNode->children) &&
             isSingletonList(((TrieBody *)(currentNode->children->body->value))
                                 ->inheritedVertices)) {
    TrieBody *childNode = (TrieBody *)currentNode->children->body->value;
    typename List::iterator brother = popCell(childNode->inheritedVertices);

    decrementOmegaArray(tInfo->distribution, childNode->depth);

    goBackProcessInnerDoubleCommonPrefixVertices(
        targetCell, brother, currentNode, childNode, goAheadStack, tInfo,
        targetDepth);
  } else {
    TrieBody *parent = currentNode->parent;

    goBackProcessInnerManyCommonPrefixVertices(targetCell, parent, goAheadStack,
                                               tInfo, targetDepth);
  }

  return;
}

// trie is minimal for uniqueness!!
template <typename S>
void goBackProcess(typename List::iterator targetCell, TrieBody *currentNode,
                   S *goAheadStack, TerminationConditionInfo *tInfo,
                   int targetDepth) {
  if (targetDepth < currentNode->depth) {
    if (currentNode->inheritedVertices->empty()) {
      TrieBody *parent = currentNode->parent;

      decrementOmegaArray(tInfo->distribution, currentNode->depth);
      if (parent->depth >= 0 && !isSingletonRedBlackTree(parent->children)) {
        decrementOmegaArray(tInfo->increase, parent->depth);
      }

      deleteTrieBody(currentNode);

      goBackProcessInnerSingleCommonPrefixVertex(
          targetCell, parent, goAheadStack, tInfo, targetDepth);
    } else if (isSingletonList(currentNode->inheritedVertices)) {
      typename List::iterator brother = popCell(currentNode->inheritedVertices);
      TrieBody *parent = currentNode->parent;

      decrementOmegaArray(tInfo->distribution, OMEGA);
      decrementOmegaArray(tInfo->distribution, OMEGA);

      goBackProcessInnerDoubleCommonPrefixVertices(targetCell, brother, parent,
                                                   currentNode, goAheadStack,
                                                   tInfo, targetDepth);
    } else {
      TrieBody *parent = currentNode->parent;

      decrementOmegaArray(tInfo->distribution, OMEGA);

      goBackProcessInnerManyCommonPrefixVertices(
          targetCell, parent, goAheadStack, tInfo, targetDepth);
    }
  } else {
    pushCell(currentNode->inheritedVertices, targetCell);
  }
}

template <typename S1, typename S2>
void goBackProcessOfCurrentConvertedVertices(S1 *BFSStack, S2 *goAheadStack,
                                             TerminationConditionInfo *tInfo,
                                             int targetDepth) {
  int i;

  for (i = 0; i < numStack(BFSStack); i++) {
    ConvertedGraphVertex *cVertex = readStack(BFSStack, i);
    InheritedVertex *iVertex = cVertex->correspondingVertexInTrie;
    TrieBody *currentNode = iVertex->ownerNode;
    typename List::iterator targetCell = iVertex->ownerCell;
    cutCell(targetCell);

    goBackProcess(targetCell, currentNode, goAheadStack, tInfo, targetDepth);
  }

  return;
}

template <typename S1, typename S2>
void goAheadProcess(TrieBody *targetNode, S1 *goAheadStack,
                    S2 *fixCreditIndexStack, TerminationConditionInfo *tInfo,
                    ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID) {
  List *inheritedVerticesList = targetNode->inheritedVertices;
  RedBlackTree *children = targetNode->children;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  if (isSingletonList(inheritedVerticesList) && isEmptyRedBlackTree(children) &&
      targetNode->depth != -1) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    incrementOmegaArray(tInfo->distribution, targetNode->depth);
    ((InheritedVertex *)inheritedVerticesList->front())->canonicalLabel.first =
        targetNode->key.u.ui32;
  } else {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    while (!inheritedVerticesList->empty()) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      typename List::iterator tmpCell = popCell(inheritedVerticesList);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      KeyContainer key = makeUInt32Key(callHashValue(
          ((InheritedVertex *)tmpCell->value), targetNode->depth, cAfterGraph,
          gapOfGlobalRootMemID, fixCreditIndexStack));
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      TrieBody *nextNode = (TrieBody *)searchRedBlackTree(children, key);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      if (nextNode == NULL) {
        if (!isEmptyRedBlackTree(children)) {
          incrementOmegaArray(tInfo->increase, targetNode->depth);
        }
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        nextNode = makeTrieBody();
        insertRedBlackTree(children, key, nextNode);
        nextNode->key = key;
        nextNode->parent = targetNode;
        nextNode->depth = targetNode->depth + 1;
        printf("%s:%d\n", __FUNCTION__, __LINE__);
      }
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      if (!nextNode->isPushedIntoGoAheadStack &&
          !nextNode->inheritedVertices->empty()) {
        if (isSingletonList(nextNode->inheritedVertices)) {
          decrementOmegaArray(tInfo->distribution, nextNode->depth);
        } else {
          for (auto iterator = std::begin(*nextNode->inheritedVertices);
               iterator != std::end(*nextNode->inheritedVertices);
               iterator = std::next(iterator, 1)) {
            decrementOmegaArray(tInfo->distribution, OMEGA);
          }
        }
      }
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      pushCell(nextNode->inheritedVertices, tmpCell);
      ((InheritedVertex *)tmpCell->value)->ownerNode = nextNode;
      pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, nextNode);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
    }
  }
}

template <typename S1, typename S2>
void goAheadProcessOfCurrentTrieNodes(S1 *goAheadStack, S2 *fixCreditIndexStack,
                                      TerminationConditionInfo *tInfo,
                                      ConvertedGraph *cAfterGraph,
                                      int gapOfGlobalRootMemID) {
  S1 *nextGoAheadStack = new std::stack<TrieBody *>();

  while (!goAheadStack->empty()) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    TrieBody *targetNode =
        popTrieBodyFromGoAheadStackWithoutOverlap(goAheadStack);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    goAheadProcess(targetNode, nextGoAheadStack, fixCreditIndexStack, tInfo,
                   cAfterGraph, gapOfGlobalRootMemID);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  }

  swapStack(nextGoAheadStack, goAheadStack);
  freeStack(nextGoAheadStack);

  return;
}

template <typename S1, typename S2>
void deleteInheritedVerticesFromTrie(Trie *trie, S1 *deletedVertices,
                                     S2 *goAheadStack) {
  while (!deletedVertices->empty()) {
    ConvertedGraphVertex *targetCVertex =
        popConvertedVertexFromDiffInfoStackWithoutOverlap(deletedVertices);
    // convertedGraphVertexDump(targetCVertex);

    InheritedVertex *targetIVertex = targetCVertex->correspondingVertexInTrie;

    printf("%s:%d\n", __FUNCTION__, __LINE__);
    typename List::iterator targetCell = targetIVertex->ownerCell;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    cutCell(targetCell);
    TrieBody *currentNode = targetIVertex->ownerNode;

    goBackProcess(targetCell, currentNode, goAheadStack, trie->info, -1);

    cutCell(targetCell);
    free(targetCell);
    freeInheritedVertex(targetIVertex);
  }
}

InheritedVertex *
wrapAfterConvertedVertexInInheritedVertex(ConvertedGraphVertex *cVertex,
                                          int gapOfGlobalRootMemID) {
  InheritedVertex *iVertex = (InheritedVertex *)malloc(sizeof(InheritedVertex));
  iVertex->type = cVertex->type;
  strcpy(iVertex->name, cVertex->name);
  iVertex->canonicalLabel.first = 0;
  iVertex->canonicalLabel.second = 0;
  iVertex->hashString = makeHashString();
  iVertex->isPushedIntoFixCreditIndex = FALSE;
  iVertex->beforeID = cVertex->ID - gapOfGlobalRootMemID;
  cVertex->correspondingVertexInTrie = iVertex;
  iVertex->ownerNode = NULL;
  iVertex->ownerCell = NULL;
  iVertex->conventionalPropagationMemo = new std::vector<int>();
  iVertex->equivalenceClassOfIsomorphism = makeDisjointSetForest();

  return iVertex;
}

template <typename S1, typename S2, typename S3>
void addInheritedVerticesToTrie(Trie *trie, S1 *addedVertices,
                                S2 *initializeConvertedVerticesStack,
                                S3 *goAheadStack, Graphinfo *cAfterGraph,
                                int gapOfGlobalRootMemID) {
  if (!addedVertices->empty()) {
    pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, trie->body);
  }

  while (!addedVertices->empty()) {
    ConvertedGraphVertex *targetCVertex =
        popConvertedVertexFromDiffInfoStackWithoutOverlap(addedVertices);
    InheritedVertex *targetIVertex = wrapAfterConvertedVertexInInheritedVertex(
        targetCVertex, gapOfGlobalRootMemID);

    trie->body->inheritedVertices->push_front(targetIVertex);
    targetIVertex->ownerCell = std::begin(*trie->body->inheritedVertices);
    targetCVertex->isVisitedInBFS = TRUE;
    pushStack(initializeConvertedVerticesStack, targetCVertex);
  }

  return;
}

template <typename S1, typename S2, typename S3>
void moveInheritedRelinkedVerticesToBFSStack(
    S1 *relinkedVertices, S2 *initializeConvertedVerticesStack, S3 *BFSStack) {
  while (!relinkedVertices->empty()) {
    ConvertedGraphVertex *cVertex =
        popConvertedVertexFromDiffInfoStackWithoutOverlap(relinkedVertices);
    BFSStack->push_back(cVertex);
    cVertex->isVisitedInBFS = TRUE;
  }

  return;
}

template <typename S>
void initializeConvertedVertices(S *initializeConvertedVerticesStack) {
  while (!initializeConvertedVerticesStack->empty()) {
    ConvertedGraphVertex *cVertex =
        (ConvertedGraphVertex *)popStack(initializeConvertedVerticesStack);
    cVertex->isVisitedInBFS = FALSE;
  }

  return;
}

Bool isEmptyTrie(Trie *trie) {
  return isEmptyRedBlackTree(trie->body->children);
}

template <typename S>
Bool isDescreteTrie(S *goAheadStack, TerminationConditionInfo *tInfo,
                    int depth) {
  return maxIndex(tInfo->distribution) == depth && goAheadStack->empty();
}

Bool isRefinedTrie(TerminationConditionInfo *tInfo, int step) {
  return readOmegaArray(tInfo->increase, step) != 0;
}

template <typename S>
Bool triePropagationIsContinued(S *goAheadStack,
                                TerminationConditionInfo *tInfo, int step) {
  return isRefinedTrie(tInfo, step) &&
         !isDescreteTrie(goAheadStack, tInfo, step);
}

template <typename S>
void pushInftyDepthTrieNodesIntoGoAheadStackInner(
    TrieBody *body, S *goAheadStack, TerminationConditionInfo *tInfo,
    int depth);

template <typename S>
void pushInftyDepthTrieNodesIntoGoAheadStackInnerInner(
    RedBlackTreeBody *trieChildrenBody, S *goAheadStack,
    TerminationConditionInfo *tInfo, int depth) {
  if (trieChildrenBody != NULL) {
    pushInftyDepthTrieNodesIntoGoAheadStackInnerInner(
        trieChildrenBody->children[LEFT], goAheadStack, tInfo, depth);
    pushInftyDepthTrieNodesIntoGoAheadStackInner(
        (TrieBody *)trieChildrenBody->value, goAheadStack, tInfo, depth);
    pushInftyDepthTrieNodesIntoGoAheadStackInnerInner(
        trieChildrenBody->children[RIGHT], goAheadStack, tInfo, depth);
  }

  return;
}

void collectDescendantConvertedVerticesInner(TrieBody *ancestorBody,
                                             RedBlackTreeBody *rbtb);

template <typename S>
void pushInftyDepthTrieNodesIntoGoAheadStackInner(
    TrieBody *body, S *goAheadStack, TerminationConditionInfo *tInfo,
    int targetDepth) {
  if (body->depth == targetDepth) {
    if (!body->isPushedIntoGoAheadStack) {
      collectDescendantConvertedVerticesInner(body, body->children->body);
      deleteTrieDescendants(body);
      body->isInfinitedDepth = FALSE;

      if (!isSingletonList(body->inheritedVertices)) {
        pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, body);

        for (auto iterator = std::begin(*body->inheritedVertices);
             iterator != std::end(*body->inheritedVertices);
             iterator = std::next(iterator, 1)) {
          decrementOmegaArray(tInfo->distribution, OMEGA);
        }
      }
    }
  } else {
    pushInftyDepthTrieNodesIntoGoAheadStackInnerInner(
        body->children->body, goAheadStack, tInfo, targetDepth);
  }

  return;
}

template <typename S>
void pushInftyDepthTrieNodesIntoGoAheadStack(Trie *trie, S *goAheadStack,
                                             int targetDepth) {
  pushInftyDepthTrieNodesIntoGoAheadStackInner(trie->body, goAheadStack,
                                               trie->info, targetDepth);

  return;
}

template <typename S1, typename S2, typename S3, typename S4>
void triePropagateInner(Trie *trie, S1 *BFSStack,
                        S2 *initializeConvertedVerticesStack, S3 *goAheadStack,
                        S4 *fixCreditIndexStack,
                        TerminationConditionInfo *tInfo, int stepOfPropagation,
                        ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID) {
  if (maxIndex(tInfo->distribution) == OMEGA &&
      maxIndex(tInfo->increase) == stepOfPropagation - 1) {
    pushInftyDepthTrieNodesIntoGoAheadStack(trie, goAheadStack,
                                            stepOfPropagation);
  }

  goBackProcessOfCurrentConvertedVertices(BFSStack, goAheadStack, tInfo,
                                          stepOfPropagation);
  goAheadProcessOfCurrentTrieNodes(goAheadStack, fixCreditIndexStack, tInfo,
                                   cAfterGraph, gapOfGlobalRootMemID);

  getNextDistanceConvertedVertices(BFSStack, initializeConvertedVerticesStack,
                                   cAfterGraph);

  return;
}

void collectDescendantConvertedVertices(TrieBody *ancestorBody,
                                        TrieBody *descendantBody);

void collectDescendantConvertedVerticesInner(TrieBody *ancestorBody,
                                             RedBlackTreeBody *rbtb) {
  if (rbtb != NULL) {
    collectDescendantConvertedVerticesInner(ancestorBody, rbtb->children[LEFT]);
    collectDescendantConvertedVertices(ancestorBody, (TrieBody *)rbtb->value);
    collectDescendantConvertedVerticesInner(ancestorBody,
                                            rbtb->children[RIGHT]);
  }

  return;
}

void collectDescendantConvertedVertices(TrieBody *ancestorBody,
                                        TrieBody *descendantBody) {
  if (isEmptyRedBlackTree(descendantBody->children)) {
    while (!descendantBody->inheritedVertices->empty()) {
      typename List::iterator targetCell =
          popCell(descendantBody->inheritedVertices);
      pushCell(ancestorBody->inheritedVertices, targetCell);
      ((InheritedVertex *)targetCell->value)->ownerNode = ancestorBody;
      ((InheritedVertex *)targetCell->value)->hashString->creditIndex =
          ancestorBody->depth;

      ((InheritedVertex *)targetCell->value)->canonicalLabel.first =
          ancestorBody->key.u.ui32;
    }
  } else {
    collectDescendantConvertedVerticesInner(ancestorBody,
                                            descendantBody->children->body);
  }

  return;
}

void makeTrieMinimumInner(TrieBody *body, TerminationConditionInfo *tInfo,
                          int stepOfPropagation);

void makeTrieMinimumInnerInner(RedBlackTreeBody *rbtb,
                               TerminationConditionInfo *tInfo,
                               int stepOfPropagation) {
  if (rbtb != NULL) {
    makeTrieMinimumInnerInner(rbtb->children[LEFT], tInfo, stepOfPropagation);
    makeTrieMinimumInner((TrieBody *)rbtb->value, tInfo, stepOfPropagation);
    makeTrieMinimumInnerInner(rbtb->children[RIGHT], tInfo, stepOfPropagation);
  }

  return;
}

void makeTrieMinimumInner(TrieBody *body, TerminationConditionInfo *tInfo,
                          int stepOfPropagation) {
  if (body->depth == stepOfPropagation + 1) {
    if (body->isPushedIntoGoAheadStack) {
      for (auto iterator = std::begin(*body->inheritedVertices);
           iterator != std::end(*body->inheritedVertices);
           iterator = std::next(iterator, 1)) {
        incrementOmegaArray(tInfo->distribution, OMEGA);
        ((InheritedVertex *)iterator->value)->canonicalLabel.first =
            body->key.u.ui32;
      }
    }

    if (!isEmptyRedBlackTree(body->children)) {
      collectDescendantConvertedVerticesInner(body, body->children->body);
      deleteTrieDescendants(body);
    }

    body->isInfinitedDepth = TRUE;
  } else {
    makeTrieMinimumInnerInner(body->children->body, tInfo, stepOfPropagation);
  }

  return;
}

void makeTrieMinimum(Trie *trie, int stepOfPropagation) {
  TerminationConditionInfo *tInfo = trie->info;

  makeTrieMinimumInner(trie->body, tInfo, stepOfPropagation);

  while (tInfo->distribution->maxFiniteIndex > stepOfPropagation) {
    decrementOmegaArray(tInfo->distribution,
                        tInfo->distribution->maxFiniteIndex);
    incrementOmegaArray(tInfo->distribution, OMEGA);
  }

  while (tInfo->increase->maxFiniteIndex > stepOfPropagation) {
    decrementOmegaArray(tInfo->increase, tInfo->increase->maxFiniteIndex);
  }

  return;
}

template <typename List>
void makeConventionalPropagationListInner(TrieBody *body, List *list,
                                          int stepOfPropagation);

template <typename List>
void makeConventionalPropagationListInnerInner(RedBlackTreeBody *body,
                                               List *list,
                                               int stepOfPropagation) {
  if (body != NULL) {
    makeConventionalPropagationListInnerInner(body->children[LEFT], list,
                                              stepOfPropagation);
    makeConventionalPropagationListInner((TrieBody *)body->value, list,
                                         stepOfPropagation);
    makeConventionalPropagationListInnerInner(body->children[RIGHT], list,
                                              stepOfPropagation);
  }

  return;
}

template <typename List>
void makeConventionalPropagationListInner(TrieBody *body, List *list,
                                          int stepOfPropagation) {
  if (isEmptyRedBlackTree(body->children)) {
    if (!list->empty()) {
      list->push_front(CLASS_SENTINEL);
    }

    for (auto iterator = std::begin(*body->inheritedVertices);
         iterator != std::end(*body->inheritedVertices);
         iterator = std::next(iterator, 1)) {
      list->push_front(iterator->value);
    }
  } else {
    makeConventionalPropagationListInnerInner(body->children->body, list,
                                              stepOfPropagation);
  }

  return;
}

List *makeConventionalPropagationList(Trie *trie, int stepOfPropagation) {
  List *ret = makeList();

  makeConventionalPropagationListInner(trie->body, ret, stepOfPropagation);

  return ret;
}

typename List::iterator getNextSentinel(typename List::iterator beginSentinel) {
  typename List::iterator endSentinel;

  for (endSentinel = beginSentinel->next; endSentinel->value != CLASS_SENTINEL;
       endSentinel = endSentinel->next) {
  }

  return endSentinel;
}

Bool putClassesWithPriority(typename List::iterator beginSentinel,
                            typename List::iterator endSentinel,
                            std::priority_queue<std::pair<int, ListBody *>> *cellPQueue) {
  Bool isRefined = FALSE;
  auto prevWrapper = cellPQueue->top();

  while (!cellPQueue->empty()) {
    auto tmpWrapper = cellPQueue->top();
    cellPQueue->pop();

    if (tmpWrapper.first < prevWrapper.first) {
      typename List::iterator classSentinel = makeCell(CLASS_SENTINEL);
      insertNextCell(beginSentinel, classSentinel);

      isRefined = TRUE;
    }

    insertNextCell(beginSentinel, (ListBody *)tmpWrapper.second);

    prevWrapper = tmpWrapper;
  }

  return isRefined;
}

template <typename List>
Bool classifyConventionalPropagationList(
    List *pList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID,
    Bool classifyConventionalPropagationListInner(ListBody *, ListBody *,
                                                  ConvertedGraph *, int,
                                                  std::priority_queue<std::pair<int, ListBody *>> *)) {
  if (pList->empty()) {
    return FALSE;
  } else {
    Bool isRefined = FALSE;
    auto cellPQueue = std::priority_queue<std::pair<int, ListBody *>>();

    typename List::iterator beginSentinel;
    typename List::iterator endSentinel;

    endSentinel = pList->sentinel;
    beginSentinel = endSentinel;

    do {
      endSentinel = getNextSentinel(beginSentinel);

      if (classifyConventionalPropagationListInner(
              beginSentinel, endSentinel, cAfterGraph, gapOfGlobalRootMemID,
              &cellPQueue)) {
        isRefined = TRUE;
      }
      beginSentinel = endSentinel;
    } while (endSentinel != pList->sentinel);

    return isRefined;
  }
}

Bool classifyConventionalPropagationListWithTypeInner(
    typename List::iterator beginSentinel, typename List::iterator endSentinel,
    ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID,
    std::priority_queue<std::pair<int, ListBody *>> *cellPQueue) {
  while (beginSentinel->next != endSentinel) {
    typename List::iterator tmpCell = beginSentinel->next;
    cutCell(tmpCell);

    int tmpPriority = ((InheritedVertex *)(tmpCell->value))->type;
    cellPQueue->emplace(tmpPriority, tmpCell);
  }

  Bool isRefined =
      putClassesWithPriority(beginSentinel, endSentinel, cellPQueue);

  return isRefined;
}

Bool classifyConventionalPropagationListWithDegreeInner(
    typename List::iterator beginSentinel, typename List::iterator endSentinel,
    ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID,
    std::priority_queue<std::pair<int, ListBody *>> *cellPQueue) {
  while (beginSentinel->next != endSentinel) {
    typename List::iterator tmpCell = beginSentinel->next;
    cutCell(tmpCell);

    int tmpPriority = numStack(correspondingVertexInConvertedGraph(
                                   ((InheritedVertex *)(tmpCell->value)),
                                   cAfterGraph, gapOfGlobalRootMemID)
                                   ->links);
    cellPQueue->emplace(tmpPriority, tmpCell);
  }

  Bool isRefined =
      putClassesWithPriority(beginSentinel, endSentinel, cellPQueue);

  return isRefined;
}

Bool classifyConventionalPropagationListWithNameLengthInner(
    typename List::iterator beginSentinel, typename List::iterator endSentinel,
    ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID,
    std::priority_queue<std::pair<int, ListBody *>> *cellPQueue) {
  while (beginSentinel->next != endSentinel) {
    typename List::iterator tmpCell = beginSentinel->next;
    cutCell(tmpCell);

    int tmpPriority = strlen(correspondingVertexInConvertedGraph(
                                 ((InheritedVertex *)(tmpCell->value)),
                                 cAfterGraph, gapOfGlobalRootMemID)
                                 ->name);
    cellPQueue->emplace(tmpPriority, tmpCell);
  }

  Bool isRefined =
      putClassesWithPriority(beginSentinel, endSentinel, cellPQueue);

  return isRefined;
}

Bool classifyConventionalPropagationListWithNameCharactersInnerInner(
    typename List::iterator beginSentinel, typename List::iterator endSentinel,
    ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID, int index,
    std::priority_queue<std::pair<int, ListBody *>> *cellPQueue) {
  while (beginSentinel->next != endSentinel) {
    typename List::iterator tmpCell = beginSentinel->next;
    cutCell(tmpCell);

    int tmpPriority = (correspondingVertexInConvertedGraph(
                           ((InheritedVertex *)(tmpCell->value)), cAfterGraph,
                           gapOfGlobalRootMemID)
                           ->name)[index];
    cellPQueue->emplace(tmpPriority, tmpCell);
  }

  Bool isRefined =
      putClassesWithPriority(beginSentinel, endSentinel, cellPQueue);

  return isRefined;
}

Bool classifyConventionalPropagationListWithNameCharactersInner(
    typename List::iterator beginSentinel, typename List::iterator endSentinel,
    ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID,
    std::priority_queue<std::pair<int, ListBody *>> *cellPQueue) {
  Bool isRefined = FALSE;

  typename List::iterator innerBeginSentinel;
  typename List::iterator innerEndSentinel;

  int nameLength = strlen(correspondingVertexInConvertedGraph(
                              ((InheritedVertex *)(beginSentinel->next->value)),
                              cAfterGraph, gapOfGlobalRootMemID)
                              ->name);

  int i;
  for (i = 0; i < nameLength; i++) {
    innerEndSentinel = beginSentinel;
    innerBeginSentinel = beginSentinel;

    do {
      innerEndSentinel = getNextSentinel(innerBeginSentinel);

      isRefined =
          classifyConventionalPropagationListWithNameCharactersInnerInner(
              innerBeginSentinel, innerEndSentinel, cAfterGraph,
              gapOfGlobalRootMemID, i, cellPQueue) ||
          isRefined;

      innerBeginSentinel = innerEndSentinel;
    } while (innerEndSentinel != endSentinel);
  }

  return isRefined;
}

template <typename List>
Bool classifyConventionalPropagationListWithType(List *pList,
                                                 ConvertedGraph *cAfterGraph,
                                                 int gapOfGlobalRootMemID) {
  return classifyConventionalPropagationList(
      pList, cAfterGraph, gapOfGlobalRootMemID,
      classifyConventionalPropagationListWithTypeInner);
}

template <typename List>
Bool classifyConventionalPropagationListWithDegree(List *pList,
                                                   ConvertedGraph *cAfterGraph,
                                                   int gapOfGlobalRootMemID) {
  return classifyConventionalPropagationList(
      pList, cAfterGraph, gapOfGlobalRootMemID,
      classifyConventionalPropagationListWithDegreeInner);
}

template <typename List>
Bool classifyConventionalPropagationListWithName(List *pList,
                                                 ConvertedGraph *cAfterGraph,
                                                 int gapOfGlobalRootMemID) {
  Bool isRefined = FALSE;
  isRefined = classifyConventionalPropagationList(
                  pList, cAfterGraph, gapOfGlobalRootMemID,
                  classifyConventionalPropagationListWithNameLengthInner) ||
              isRefined;
  isRefined = classifyConventionalPropagationList(
                  pList, cAfterGraph, gapOfGlobalRootMemID,
                  classifyConventionalPropagationListWithNameCharactersInner) ||
              isRefined;

  return isRefined;
}

template <typename List>
Bool classifyConventionalPropagationListWithAttribute(
    List *pList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID) {
  Bool isRefined = FALSE;
  isRefined = classifyConventionalPropagationListWithType(
                  pList, cAfterGraph, gapOfGlobalRootMemID) ||
              isRefined;
  isRefined = classifyConventionalPropagationListWithDegree(
                  pList, cAfterGraph, gapOfGlobalRootMemID) ||
              isRefined;
  isRefined = classifyConventionalPropagationListWithName(
                  pList, cAfterGraph, gapOfGlobalRootMemID) ||
              isRefined;

  return isRefined;
}

template <typename List>
void putLabelsToAdjacentVertices(List *pList, ConvertedGraph *cAfterGraph,
                                 int gapOfGlobalRootMemID) {
  if (pList->empty()) {
    return;
  }

  int tmpLabel = 0;

  typename List::iterator beginSentinel = pList->sentinel;
  typename List::iterator endSentinel = beginSentinel;

  do {
    endSentinel = getNextSentinel(beginSentinel);

    int tmpDegree =
        numStack(correspondingVertexInConvertedGraph(
                     ((InheritedVertex *)(beginSentinel->next->value)),
                     cAfterGraph, gapOfGlobalRootMemID)
                     ->links);
    ConvertedGraphVertexType tmpType =
        correspondingVertexInConvertedGraph(
            ((InheritedVertex *)(beginSentinel->next->value)), cAfterGraph,
            gapOfGlobalRootMemID)
            ->type;

    int i;
    for (i = 0; i < tmpDegree; i++) {
      typename List::iterator iteratorCell;
      for (iteratorCell = beginSentinel->next; iteratorCell != endSentinel;
           iteratorCell = iteratorCell->next) {
        LMNtalLink *tmpLink = (LMNtalLink *)readStack(
            correspondingVertexInConvertedGraph(
                ((InheritedVertex *)(iteratorCell->value)), cAfterGraph,
                gapOfGlobalRootMemID)
                ->links,
            i);
        ConvertedGraphVertex *adjacentVertex;

        switch (tmpLink->attr) {
        case INTEGER_ATTR:
          writeStack(((InheritedVertex *)(iteratorCell->value))
                         ->conventionalPropagationMemo,
                     i, tmpLink->data.integer * 256 + INTEGER_ATTR);
          break;
        // case DOUBLE_ATTR:
        // break;
        // case STRING_ATTR:
        // break;
        case HYPER_LINK_ATTR:
          adjacentVertex = getConvertedVertexFromGraphAndIDAndType(
              cAfterGraph, tmpLink->data.ID, convertedHyperLink);
          pushStack(adjacentVertex->correspondingVertexInTrie
                        ->conventionalPropagationMemo,
                    tmpLabel * 256 + i);
          break;
        case GLOBAL_ROOT_MEM_ATTR:
          break;
        default:
          if (tmpLink->attr < 128) {
            adjacentVertex = getConvertedVertexFromGraphAndIDAndType(
                cAfterGraph, tmpLink->data.ID, convertedAtom);
            switch (tmpType) {
            case convertedAtom:
              writeStack(adjacentVertex->correspondingVertexInTrie
                             ->conventionalPropagationMemo,
                         tmpLink->attr, tmpLabel * 256 + i);
              break;
            case convertedHyperLink:
              writeStack(adjacentVertex->correspondingVertexInTrie
                             ->conventionalPropagationMemo,
                         tmpLink->attr, tmpLabel * 256 + HYPER_LINK_ATTR);
              break;
            default:
              CHECKER("unexpected vertex type\n");
              exit(EXIT_FAILURE);
              break;
            }
          } else {
            CHECKER("unexpected vertex type\n");
            exit(EXIT_FAILURE);
          }
          break;
        }
      }
    }

    tmpLabel++;

    beginSentinel = endSentinel;
  } while (endSentinel != pList->sentinel);

  return;
}

Bool classifyConventionalPropagationListWithAdjacentLabelsInnerInner(
    typename List::iterator beginSentinel, typename List::iterator endSentinel,
    std::priority_queue<std::pair<int, ListBody *>> *cellPQueue) {
  while (beginSentinel->next != endSentinel) {
    typename List::iterator tmpCell = beginSentinel->next;
    cutCell(tmpCell);

    int tmpPriority = popStack(
        ((InheritedVertex *)(tmpCell->value))->conventionalPropagationMemo);
    cellPQueue->emplace(tmpPriority, tmpCell);
  }

  Bool isRefined =
      putClassesWithPriority(beginSentinel, endSentinel, cellPQueue);

  return isRefined;
}

Bool classifyConventionalPropagationListWithAdjacentLabelsInner(
    typename List::iterator beginSentinel, typename List::iterator endSentinel,
    ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID,
    std::priority_queue<std::pair<int, ListBody *>> *cellPQueue) {
  Bool isRefined = FALSE;

  typename List::iterator innerBeginSentinel;
  typename List::iterator innerEndSentinel;

  int degree = numStack(correspondingVertexInConvertedGraph(
                            ((InheritedVertex *)(beginSentinel->next->value)),
                            cAfterGraph, gapOfGlobalRootMemID)
                            ->links);

  int i;
  for (i = 0; i < degree; i++) {
    innerEndSentinel = beginSentinel;
    innerBeginSentinel = beginSentinel;

    do {
      innerEndSentinel = getNextSentinel(innerBeginSentinel);

      isRefined =
          classifyConventionalPropagationListWithAdjacentLabelsInnerInner(
              innerBeginSentinel, innerEndSentinel, cellPQueue) ||
          isRefined;

      innerBeginSentinel = innerEndSentinel;
    } while (innerEndSentinel != endSentinel);
  }

  return isRefined;
}

template <typename List>
Bool classifyConventionalPropagationListWithAdjacentLabels(
    List *pList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID) {
  return classifyConventionalPropagationList(
      pList, cAfterGraph, gapOfGlobalRootMemID,
      classifyConventionalPropagationListWithAdjacentLabelsInner);
}

template <typename List>
Bool refineConventionalPropagationListByPropagation(List *pList,
                                                    ConvertedGraph *cAfterGraph,
                                                    int gapOfGlobalRootMemID) {
  Bool isRefined = FALSE;

  putLabelsToAdjacentVertices(pList, cAfterGraph, gapOfGlobalRootMemID);

  isRefined = classifyConventionalPropagationListWithAdjacentLabels(
                  pList, cAfterGraph, gapOfGlobalRootMemID) ||
              isRefined;

  return isRefined;
}

template <typename List>
Bool getStableRefinementOfConventionalPropagationList(
    List *pList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID) {
  Bool isRefined = FALSE;

  while (refineConventionalPropagationListByPropagation(pList, cAfterGraph,
                                                        gapOfGlobalRootMemID)) {
    isRefined = TRUE;
  }

  return isRefined;
}

InheritedVertex *copyInheritedVertex(InheritedVertex *iVertex) {
  if (iVertex == CLASS_SENTINEL) {
    return CLASS_SENTINEL;
  } else {
    InheritedVertex *ret = (InheritedVertex *)malloc(sizeof(InheritedVertex));

    ret->type = iVertex->type;
    strcpy(ret->name, iVertex->name);
    ret->canonicalLabel = iVertex->canonicalLabel;
    ret->hashString = iVertex->hashString;
    ret->isPushedIntoFixCreditIndex = iVertex->isPushedIntoFixCreditIndex;
    ret->beforeID = iVertex->beforeID;
    ret->ownerNode = iVertex->ownerNode;
    ret->ownerCell = iVertex->ownerCell;
    ret->conventionalPropagationMemo = new std::vector<int>();
    int i;
    for (i = 0; i < numStack(iVertex->conventionalPropagationMemo); i++) {
      int tmp = readStack(iVertex->conventionalPropagationMemo, i);
      writeStack(ret->conventionalPropagationMemo, i, tmp);
    }
    ret->equivalenceClassOfIsomorphism = iVertex->equivalenceClassOfIsomorphism;

    return ret;
  }
}

void *copyInheritedVertexCaster(void *iVertex) {
  return copyInheritedVertex((InheritedVertex *)iVertex);
}

void assureReferenceFromConvertedVerticesToInheritedVertices(
    ConvertedGraph *cAfterGraph, ConvertedGraph *cBeforeGraph,
    int gapOfGlobalRootMemID) {
  int i;
  for (i = 0; i < cAfterGraph->atoms->size(); i++) {
    ConvertedGraphVertex *cAfterVertex =
        (ConvertedGraphVertex *)readDynamicArray(cAfterGraph->atoms, i);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    if (cAfterVertex != NULL) {
      if (cAfterVertex->correspondingVertexInTrie == NULL) {
        convertedGraphVertexDump(cAfterVertex);
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        ConvertedGraphVertex *cBeforeVertex =
            getConvertedVertexFromGraphAndIDAndType(
                cBeforeGraph, cAfterVertex->ID - gapOfGlobalRootMemID,
                cAfterVertex->type);
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        cAfterVertex->correspondingVertexInTrie =
            cBeforeVertex->correspondingVertexInTrie;
        cAfterVertex->correspondingVertexInTrie->beforeID = cBeforeVertex->ID;
      }
    }
  }

  for (i = 0; i < cAfterGraph->hyperlinks->size(); i++) {
    ConvertedGraphVertex *cAfterVertex =
        (ConvertedGraphVertex *)readDynamicArray(cAfterGraph->hyperlinks, i);
    if (cAfterVertex != NULL) {
      if (cAfterVertex->correspondingVertexInTrie == NULL) {
        ConvertedGraphVertex *cBeforeVertex =
            (ConvertedGraphVertex *)getConvertedVertexFromGraphAndIDAndType(
                cBeforeGraph, cAfterVertex->ID - gapOfGlobalRootMemID,
                cAfterVertex->type);
        cAfterVertex->correspondingVertexInTrie =
            cBeforeVertex->correspondingVertexInTrie;
        cAfterVertex->correspondingVertexInTrie->beforeID = cBeforeVertex->ID;
      }
    }
  }

  return;
}

void initializeReferencesFromConvertedVerticesToInheritedVertices(
    ConvertedGraph *cBeforeGraph) {
  int i;
  for (i = 0; i < cBeforeGraph->atoms->size(); i++) {
    ConvertedGraphVertex *cBeforeVertex =
        (ConvertedGraphVertex *)readDynamicArray(cBeforeGraph->atoms, i);
    if (cBeforeVertex != NULL) {
      cBeforeVertex->correspondingVertexInTrie = NULL;
    }
  }

  for (i = 0; i < cBeforeGraph->hyperlinks->size(); i++) {
    ConvertedGraphVertex *cBeforeVertex =
        (ConvertedGraphVertex *)readDynamicArray(cBeforeGraph->hyperlinks, i);
    if (cBeforeVertex != NULL) {
      cBeforeVertex->correspondingVertexInTrie = NULL;
    }
  }

  return;
}

Bool triePropagate(Trie *trie, DiffInfo *diffInfo, Graphinfo *cAfterGraph,
                   Graphinfo *cBeforeGraph, int gapOfGlobalRootMemID,
                   int *stepOfPropagationPtr) {
  std::stack<TrieBody *> goAheadStack;
  std::vector<ConvertedGraphVertex *> BFSStack;
  std::vector<ConvertedGraphVertex *> initializeConvertedVerticesStack;
  std::stack<InheritedVertex *> fixCreditIndexStack;
  TerminationConditionInfo *tInfo = trie->info;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  deleteInheritedVerticesFromTrie(trie, diffInfo->deletedVertices,
                                  &goAheadStack);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  addInheritedVerticesToTrie(trie, diffInfo->addedVertices,
                             &initializeConvertedVerticesStack, &goAheadStack,
                             cAfterGraph, gapOfGlobalRootMemID);
  //実際のSLIMでは起きない操作
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  assureReferenceFromConvertedVerticesToInheritedVertices(
      cAfterGraph->cv, cBeforeGraph->cv, gapOfGlobalRootMemID);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  moveInheritedRelinkedVerticesToBFSStack(
      diffInfo->relinkedVertices, &initializeConvertedVerticesStack, &BFSStack);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  int stepOfPropagation = -1;
  goAheadProcessOfCurrentTrieNodes(&goAheadStack, &fixCreditIndexStack, tInfo,
                                   cAfterGraph->cv, gapOfGlobalRootMemID);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  stepOfPropagation = 0;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  goAheadProcessOfCurrentTrieNodes(&goAheadStack, &fixCreditIndexStack, tInfo,
                                   cAfterGraph->cv, gapOfGlobalRootMemID);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  while (triePropagationIsContinued(&goAheadStack, tInfo, stepOfPropagation)) {
    stepOfPropagation++;
    triePropagateInner(trie, &BFSStack, &initializeConvertedVerticesStack,
                       &goAheadStack, &fixCreditIndexStack, tInfo,
                       stepOfPropagation, cAfterGraph->cv,
                       gapOfGlobalRootMemID);
  }

  Bool verticesAreCompletelySorted =
      isDescreteTrie(&goAheadStack, tInfo, stepOfPropagation) ||
      isEmptyTrie(trie);
  if (!verticesAreCompletelySorted) {
    makeTrieMinimum(trie, stepOfPropagation);

    while (!goAheadStack.empty()) {
      popTrieBodyFromGoAheadStackWithoutOverlap(&goAheadStack);
    }
  }

  initializeConvertedVertices(&initializeConvertedVerticesStack);
  initializeConvertedVertices(&BFSStack);
  fixCreditIndex(&fixCreditIndexStack, cAfterGraph->cv, gapOfGlobalRootMemID);

  //実際のSLIMでは起きない操作
  initializeReferencesFromConvertedVerticesToInheritedVertices(
      cBeforeGraph->cv);

  *stepOfPropagationPtr = stepOfPropagation;

  return verticesAreCompletelySorted;
}

void spacePrinter(int length) {
  int i;
  for (i = 0; i < length; i++) {
    printf("    ");
  }

  return;
}

void inheritedVertexDump(InheritedVertex *iVertex) {
  fprintf(stdout, "<");

  switch (iVertex->type) {
  case convertedAtom:
    fprintf(stdout, "SYMBOLATOM,");
    break;
  case convertedHyperLink:
    fprintf(stdout, " HYPERLINK,");
    break;
  default:
    fprintf(stderr, "This is unexpected vertex type\n");
    exit(EXIT_FAILURE);
    break;
  }

  fprintf(stdout, "BEFORE_ID=%d,", iVertex->beforeID);
  // fprintf(stdout,"LABEL=(%08X,%d),",iVertex->canonicalLabel.first,iVertex->canonicalLabel.second);
  // fprintf(stdout,"CREDIT=%d,",iVertex->hashString->creditIndex);
  fprintf(stdout, "NAME:\"%s\"", iVertex->name);

  fprintf(stdout, ">");

  // intStackDump(iVertex->conventionalPropagationMemo);
  return;
}

void inheritedVertexDumpCaster(void *iVertex) {
  if (iVertex == CLASS_SENTINEL) {
    fprintf(stdout, "CLASS_SENTINEL\n");
  } else {
    inheritedVertexDump((InheritedVertex *)iVertex);
  }

  return;
}

void trieDumpInner(TrieBody *body);

void trieDumpInnerCaster(void *body) {
  trieDumpInner((TrieBody *)body);

  return;
}

void trieDumpInner(TrieBody *body) {
  if (body->isPushedIntoGoAheadStack) {
    fprintf(stdout, "\x1b[33m");
  }

  spacePrinter(body->depth);
  fprintf(stdout, "KEY:");
  keyDump(body->key);
  fprintf(stdout, "\n");

  spacePrinter(body->depth);
  fprintf(stdout, "VERTICES:");
  listDump(body->inheritedVertices, inheritedVertexDumpCaster);
  fprintf(stdout, "\n");

  if (body->isPushedIntoGoAheadStack) {
    fprintf(stdout, "\x1b[39m");
  }

  redBlackTreeValueDump(body->children, trieDumpInnerCaster);

  return;
}

void terminationConditionInfoDump(TerminationConditionInfo *tInfo) {
  fprintf(stdout, "DISTRIBUTION:"), omegaArrayDump(tInfo->distribution),
      fprintf(stdout, "\n");
  fprintf(stdout, "INCREASE    :"), omegaArrayDump(tInfo->increase),
      fprintf(stdout, "\n");

  return;
}

void makeTerminationConditionMemoInner(TrieBody *tBody,
                                       OmegaArray *distributionMemo,
                                       OmegaArray *increaseMemo);

void makeTerminationConditionMemoInnerInner(RedBlackTreeBody *rBody,
                                            OmegaArray *distributionMemo,
                                            OmegaArray *increaseMemo) {
  if (rBody != NULL) {
    makeTerminationConditionMemoInnerInner(rBody->children[LEFT],
                                           distributionMemo, increaseMemo);
    makeTerminationConditionMemoInner((TrieBody *)rBody->value,
                                      distributionMemo, increaseMemo);
    makeTerminationConditionMemoInnerInner(rBody->children[RIGHT],
                                           distributionMemo, increaseMemo);
  }

  return;
}

void makeTerminationConditionMemoInner(TrieBody *tBody,
                                       OmegaArray *distributionMemo,
                                       OmegaArray *increaseMemo) {
  if (!tBody->isPushedIntoGoAheadStack) {
    if (isSingletonList(tBody->inheritedVertices)) {
      incrementOmegaArray(distributionMemo, tBody->depth);
    } else {
      for (auto iterator = std::begin(*tBody->inheritedVertices);
           iterator != std::end(*tBody->inheritedVertices);
           iterator = std::next(iterator, 1)) {
        incrementOmegaArray(distributionMemo, OMEGA);
      }
    }
  }

  if (tBody->depth != 0) {
    incrementOmegaArray(increaseMemo, tBody->depth - 1);
  }

  makeTerminationConditionMemoInnerInner(tBody->children->body,
                                         distributionMemo, increaseMemo);

  if (!isEmptyRedBlackTree(tBody->children)) {
    decrementOmegaArray(increaseMemo, tBody->depth);
  }

  return;
}

void makeTerminationConditionMemo(Trie *trie, OmegaArray *distributionMemo,
                                  OmegaArray *increaseMemo) {
  makeTerminationConditionMemoInnerInner(trie->body->children->body,
                                         distributionMemo, increaseMemo);

  return;
}

void trieDump(Trie *trie) {
  OmegaArray *distributionMemo = makeOmegaArray();
  OmegaArray *increaseMemo = makeOmegaArray();

  setvbuf(stdout, NULL, _IONBF, BUFSIZ);
  terminationConditionInfoDump(trie->info);
  redBlackTreeValueDump(trie->body->children, trieDumpInnerCaster);

  makeTerminationConditionMemo(trie, distributionMemo, increaseMemo);

  if (!isEqualOmegaArray(distributionMemo, trie->info->distribution) ||
      !isEqualOmegaArray(increaseMemo, trie->info->increase)) {
    fprintf(
        stderr,
        "WRONG TerminationConditionInfo "
        "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
    fprintf(stderr, "CORRECT DISTRIBUTION:"), omegaArrayDump(distributionMemo),
        fprintf(stderr, "\n");
    fprintf(stderr, "CORRECT INCREASE    :"), omegaArrayDump(increaseMemo),
        fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
  }

  freeOmegaArray(distributionMemo);
  freeOmegaArray(increaseMemo);
  printf("\n");
  return;
}

void omegaArrayDumpExperiment(OmegaArray *oArray) {
  fprintf(stdout, "[");

  int i;
  for (i = 0; i <= oArray->maxFiniteIndex; i++) {
    fprintf(stdout, "%2d,", readOmegaArray(oArray, i));
  }

  fprintf(stdout, " 0, 0, 0,...,%2d]", readOmegaArray(oArray, OMEGA));

  return;
}

void terminationConditionInfoDumpExperiment(TerminationConditionInfo *tInfo) {
  fprintf(stdout, "DISTRIBUTION:\n"),
      omegaArrayDumpExperiment(tInfo->distribution), fprintf(stdout, "\n");
  fprintf(stdout, "INCREASE    :\n"), omegaArrayDumpExperiment(tInfo->increase),
      fprintf(stdout, "\n");

  return;
}

void terminationConditionInfoDumpExperimentFromTrie(Trie *trie) {
  setvbuf(stdout, NULL, _IONBF, BUFSIZ);
  terminationConditionInfoDumpExperiment(trie->info);

  return;
}
Trie *gen_tmp_trie_from_originaltrie_and_gi(Trie *org_trie, Graphinfo *org_gi,
                                            Graphinfo *tmp_gi) {
  Trie *trie = new Trie();
  trieDump(org_trie);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  listDump(org_trie->body->inheritedVertices, inheritedVertexDumpCaster);
  printf("\n");
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  return trie;
}
