#include "trie.hpp"
#include <algorithm>
#include <iostream>
#include <iterator>
#include <stack>
#include <tuple>
#include <vector>

HashString *makeHashString() {
  HashString *ret = (HashString *)malloc(sizeof(HashString));

  ret->creditIndex = 0;
  ret->body = new std::vector<uint32_t *>();
  return ret;
}

void freeHashString(HashString *hashString) {
  for (auto v : *hashString->body)
    free(v);
  free(hashString->body);
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
    ret ^= numStack(&cVertex->links);
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
    ret ^= callHashValue(
        cGraph->hyperlinks[link->data.ID]->correspondingVertexInTrie, index,
        cGraph, gapOfGlobalRootMemID, fixCreditIndexStack);
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
      ret ^= callHashValue(
          cGraph->atoms[link->data.ID]->correspondingVertexInTrie, index,
          cGraph, gapOfGlobalRootMemID, fixCreditIndexStack);
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
    for (i = 0; i < numStack(&cVertex->links); i++) {
      ret *= FNV_PRIME;
      ret ^= linkHashValue(&cVertex->links[i], index - 1, cGraph,
                           gapOfGlobalRootMemID, fixCreditIndexStack);
    }

    return ret;
    break;
  case convertedHyperLink:
    sum = ADD_INIT;
    mul = MUL_INIT;
    for (i = 0; i < numStack(&cVertex->links); i++) {
      tmp = linkHashValue(&cVertex->links[i], index - 1, cGraph,
                          gapOfGlobalRootMemID, fixCreditIndexStack);
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
  InheritedVertex *iVertex = popStack(fixCreditIndexStack);
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
  std::cout << *iVertex << std::endl;
  std::cout << index << std::endl;
  if (index < 0) {
    return 0;
  } else if (index < hashString->creditIndex) {
    return *(*hashString->body)[index];
  } else if (index == 0) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    Hash tmp = initialHashValue(correspondingVertexInConvertedGraph(
        iVertex, cAfterGraph, gapOfGlobalRootMemID));
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    if(hashString->body->size() > 0) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      auto old = hashString->body->at(index);
      if (old != NULL) {
	free(old);
      }
    }
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    hashString->body->push_back(new uint32_t(tmp));
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
    auto old = hashString->body->at(index);
    (*hashString->body)[index] = new uint32_t(newMyHash);
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
    return cGraph->atoms[ID];
  case convertedHyperLink:
    return cGraph->hyperlinks[ID];
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
  std::cout<< "beforeID=" << iVertex->beforeID << std::endl;
  switch (iVertex->type) {
  case convertedAtom:
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    convertedGraphDump(cAfterGraph);
    printf("afterID=%d\n", afterID);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    convertedGraphVertexDump(cAfterGraph->atoms[afterID]);
    return cAfterGraph->atoms[afterID];
  case convertedHyperLink:
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    return cAfterGraph->hyperlinks[afterID];
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
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  S1 nextBFSStack = new std::vector<ConvertedGraphVertex *>();

  while (!BFSStack->empty()) {
    ConvertedGraphVertex *cVertex = (ConvertedGraphVertex *)popStack(BFSStack);

    int i;
    for (i = 0; i < numStack(&cVertex->links); i++) {
      auto &link = cVertex->links[i];
      ConvertedGraphVertex *adjacentVertex;

      switch (link.attr) {
      case INTEGER_ATTR:
        break;
      case DOUBLE_ATTR:
        break;
      case STRING_ATTR:
        break;
      case HYPER_LINK_ATTR:
        adjacentVertex = cAfterGraph->hyperlinks[link.data.ID];
        if (!adjacentVertex->isVisitedInBFS) {
          pushStack(nextBFSStack, adjacentVertex);
          adjacentVertex->isVisitedInBFS = TRUE;
        }
        break;
      case GLOBAL_ROOT_MEM_ATTR:
        break;
      default:
        if (link.attr < 128) {
          adjacentVertex = cAfterGraph->atoms[link.data.ID];
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
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  return;
}

int compareTrieLeaves(TrieBody *a, TrieBody *b) {
  if (a == b) {
    return 0;
  } else if (a->key < b->key) {
    return -1;
  } else if (a->key > b->key) {
    return 1;
  } else {
    int depthA = a->depth;
    int depthB = b->depth;
    HashString *hStringA =
        slim::element::get<InheritedVertex>(a->inheritedVertices->front())
            .hashString;
    HashString *hStringB =
        slim::element::get<InheritedVertex>(b->inheritedVertices->front())
            .hashString;

    int i;
    for (i = 0;; i++) {
      if (i >= depthA || i >= depthB) {
        CHECKER("unexpected case\n");
        exit(EXIT_FAILURE);
      }

      Hash hashA = *(*hStringA->body)[i];
      Hash hashB = *(*hStringB->body)[i];

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
  ret->inheritedVertices = new vertex_list();
  ret->parent = NULL;
  ret->children = new trie_body_map();
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
  delete (body->inheritedVertices);
  for (auto &v : *body->children)
    freeTrieInner(v.second);
  delete body->children;
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
  body->parent->children->erase(body->key);
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
    for (auto &v : *body->children)
      deleteTrieDescendantsAndItself(v.second);
    delete body->children;
    free(body);
  }

  return;
}

void deleteTrieDescendants(TrieBody *body) {
  for (auto &v : *body->children)
    deleteTrieDescendantsAndItself(v.second);
  body->children->clear();

  return;
}

template <typename S>
void pushTrieBodyIntoGoAheadStackWithoutOverlap(S *stack, TrieBody *body) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  if (body != NULL) {
    if (!body->isPushedIntoGoAheadStack) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
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
void goBackProcessInnerManyCommonPrefixVertices(InheritedVertex &target,
                                                TrieBody *currentNode,
                                                S *goAheadStack,
                                                TerminationConditionInfo *tInfo,
                                                int targetDepth) {
  auto targetCell = target.ownerCell;
  if (targetDepth == currentNode->depth) {
    currentNode->inheritedVertices->splice(
        std::begin(*currentNode->inheritedVertices), *target.ownerList,
        targetCell);
    slim::element::get<InheritedVertex>(*targetCell).ownerNode = currentNode;
    slim::element::get<InheritedVertex>(*targetCell).hashString->creditIndex =
        currentNode->depth;
    pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, currentNode);
  } else {
    goBackProcessInnerManyCommonPrefixVertices(
        target, currentNode->parent, goAheadStack, tInfo, targetDepth);
  }
}

template <typename S>
void goBackProcessInnerDoubleCommonPrefixVertices(
    InheritedVertex &target, InheritedVertex &brother, TrieBody *currentNode,
    TrieBody *prevNode, S *goAheadStack, TerminationConditionInfo *tInfo,
    int targetDepth) {
  auto targetCell = target.ownerCell;
  auto brotherCell = brother.ownerCell;
  if (targetDepth == currentNode->depth) {
    currentNode->inheritedVertices->splice(
        std::begin(*currentNode->inheritedVertices), *target.ownerList,
        targetCell);
    slim::element::get<InheritedVertex>(*targetCell).ownerNode = currentNode;
    slim::element::get<InheritedVertex>(*targetCell).hashString->creditIndex =
        currentNode->depth;
    prevNode->inheritedVertices->splice(
        std::begin(*prevNode->inheritedVertices), *brother.ownerList,
        brotherCell);
    slim::element::get<InheritedVertex>(*brotherCell).ownerNode = prevNode;
    slim::element::get<InheritedVertex>(*brotherCell).hashString->creditIndex =
        prevNode->depth;
    incrementOmegaArray(tInfo->distribution, prevNode->depth);
    slim::element::get<InheritedVertex>(*brotherCell).canonicalLabel.first =
        prevNode->key;

    pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, currentNode);
  } else if (currentNode->children->size() == 1) {
    TrieBody *parent = currentNode->parent;

    deleteTrieBody(prevNode);
    goBackProcessInnerDoubleCommonPrefixVertices(
        target, brother, parent, currentNode, goAheadStack, tInfo, targetDepth);
  } else {
    TrieBody *parent = currentNode->parent;

    prevNode->inheritedVertices->splice(
        std::begin(*prevNode->inheritedVertices), *brother.ownerList,
        brotherCell);
    slim::element::get<InheritedVertex>(*brotherCell).ownerNode = prevNode;
    slim::element::get<InheritedVertex>(*brotherCell).hashString->creditIndex =
        prevNode->depth;
    incrementOmegaArray(tInfo->distribution, prevNode->depth);
    slim::element::get<InheritedVertex>(*brotherCell).canonicalLabel.first =
        prevNode->key;

    goBackProcessInnerManyCommonPrefixVertices(target, parent, goAheadStack,
                                               tInfo, targetDepth);
  }
}

template <typename S>
void goBackProcessInnerSingleCommonPrefixVertex(InheritedVertex &ivertex,
                                                TrieBody *currentNode,
                                                S *goAheadStack,
                                                TerminationConditionInfo *tInfo,
                                                int targetDepth) {
  auto targetCell = ivertex.ownerCell;
  if (targetDepth == currentNode->depth) {
    currentNode->inheritedVertices->splice(
        std::begin(*currentNode->inheritedVertices), *ivertex.ownerList,
        targetCell);
    slim::element::get<InheritedVertex>(*targetCell).ownerNode = currentNode;
    slim::element::get<InheritedVertex>(*targetCell).hashString->creditIndex =
        currentNode->depth;
    pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, currentNode);
  } else if (currentNode->children->size() == 1 &&
             isSingletonList(
                 currentNode->children->begin()->second->inheritedVertices)) {
    TrieBody *childNode = (TrieBody *)currentNode->children->begin()->second;
    auto brother = std::begin(*childNode->inheritedVertices);

    decrementOmegaArray(tInfo->distribution, childNode->depth);

    goBackProcessInnerDoubleCommonPrefixVertices(
        ivertex, slim::element::get<InheritedVertex>(*brother), currentNode,
        childNode, goAheadStack, tInfo, targetDepth);
  } else {
    TrieBody *parent = currentNode->parent;

    goBackProcessInnerManyCommonPrefixVertices(ivertex, parent, goAheadStack,
                                               tInfo, targetDepth);
  }

  return;
}

// trie is minimal for uniqueness!!
template <typename S>
void goBackProcess(InheritedVertex &ivertex, TrieBody *currentNode,
                   S *goAheadStack, TerminationConditionInfo *tInfo,
                   int targetDepth) {
  if (targetDepth < currentNode->depth) {
    if (currentNode->inheritedVertices->empty()) {
      TrieBody *parent = currentNode->parent;

      decrementOmegaArray(tInfo->distribution, currentNode->depth);
      if (parent->depth >= 0 && parent->children->size() != 1) {
        decrementOmegaArray(tInfo->increase, parent->depth);
      }

      deleteTrieBody(currentNode);

      goBackProcessInnerSingleCommonPrefixVertex(ivertex, parent, goAheadStack,
                                                 tInfo, targetDepth);
    } else if (isSingletonList(currentNode->inheritedVertices)) {
      auto brother = std::begin(*currentNode->inheritedVertices);
      TrieBody *parent = currentNode->parent;

      decrementOmegaArray(tInfo->distribution, OMEGA);
      decrementOmegaArray(tInfo->distribution, OMEGA);

      goBackProcessInnerDoubleCommonPrefixVertices(
          ivertex, slim::element::get<InheritedVertex>(*brother), parent,
          currentNode, goAheadStack, tInfo, targetDepth);
    } else {
      TrieBody *parent = currentNode->parent;

      decrementOmegaArray(tInfo->distribution, OMEGA);

      goBackProcessInnerManyCommonPrefixVertices(ivertex, parent, goAheadStack,
                                                 tInfo, targetDepth);
    }
  } else {
    currentNode->inheritedVertices->splice(
        std::begin(*currentNode->inheritedVertices), *ivertex.ownerList,
        ivertex.ownerCell);
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
    goBackProcess(*iVertex, currentNode, goAheadStack, tInfo, targetDepth);
  }

  return;
}

template <typename S1, typename S2>
void goAheadProcess(TrieBody *targetNode, S1 *goAheadStack,
                    S2 *fixCreditIndexStack, TerminationConditionInfo *tInfo,
                    ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID) {
  auto inheritedVerticesList = targetNode->inheritedVertices;
  auto children = targetNode->children;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  if (isSingletonList(inheritedVerticesList) && children->empty() &&
      targetNode->depth != -1) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    incrementOmegaArray(tInfo->distribution, targetNode->depth);
    slim::element::get<InheritedVertex>(inheritedVerticesList->front())
        .canonicalLabel.first = targetNode->key;
  } else {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    while (!inheritedVerticesList->empty()) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      auto tmpCell = std::begin(*inheritedVerticesList);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << targetNode->depth << std::endl;
      auto key = callHashValue(&slim::element::get<InheritedVertex>(*tmpCell),
                               targetNode->depth, cAfterGraph,
                               gapOfGlobalRootMemID, fixCreditIndexStack);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << key <<std::endl;
      auto it = children->find(key);
      TrieBody *nextNode;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      if (it == std::end(*children)) {
        if (!children->empty()) {
          incrementOmegaArray(tInfo->increase, targetNode->depth);
        }
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        nextNode = makeTrieBody();
        children->insert(std::make_pair(key, nextNode));
        nextNode->key = key;
        nextNode->parent = targetNode;
        nextNode->depth = targetNode->depth + 1;
        printf("%s:%d\n", __FUNCTION__, __LINE__);
      } else {
	printf("%s:%d\n", __FUNCTION__, __LINE__);
        nextNode = it->second;
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
      nextNode->inheritedVertices->splice(
          std::begin(*nextNode->inheritedVertices), *inheritedVerticesList,
          tmpCell);
      slim::element::get<InheritedVertex>(*tmpCell).ownerNode = nextNode;
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
    auto targetCVertex =
        popConvertedVertexFromDiffInfoStackWithoutOverlap(deletedVertices);
    // convertedGraphVertexDump(targetCVertex);

    InheritedVertex *targetIVertex = targetCVertex->correspondingVertexInTrie;

    printf("%s:%d\n", __FUNCTION__, __LINE__);
    auto targetCell = targetIVertex->ownerCell;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    TrieBody *currentNode = targetIVertex->ownerNode;

    goBackProcess(*targetIVertex, currentNode, goAheadStack, trie->info, -1);

    targetIVertex->ownerList->erase(targetIVertex->ownerCell);
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
  iVertex->ownerList = nullptr;
  iVertex->ownerCell = vertex_list::iterator();
  iVertex->conventionalPropagationMemo = new std::vector<int>();
  iVertex->equivalenceClassOfIsomorphism = makeDisjointSetForest();

  return iVertex;
}

void addInheritedVerticesToTrie(
    Trie *trie, std::vector<ConvertedGraphVertex *> *addedVertices,
    std::vector<ConvertedGraphVertex *> *initializeConvertedVerticesStack,
    std::stack<TrieBody *> *goAheadStack, Graphinfo *cAfterGraph,
    int gapOfGlobalRootMemID) {
  if (!addedVertices->empty()) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, trie->body);
  }

  while (!addedVertices->empty()) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    ConvertedGraphVertex *targetCVertex =
        popConvertedVertexFromDiffInfoStackWithoutOverlap(addedVertices);
    // convertedGraphVertexDump(targetCVertex);
    InheritedVertex *targetIVertex = wrapAfterConvertedVertexInInheritedVertex(
        targetCVertex, gapOfGlobalRootMemID);
    std::cout<< *(targetIVertex) << std::endl;
    trie->body->inheritedVertices->push_front(*targetIVertex);
    targetIVertex->ownerList = trie->body->inheritedVertices;
    targetIVertex->ownerCell = std::begin(*trie->body->inheritedVertices);
    targetCVertex->isVisitedInBFS = TRUE;
    delete targetIVertex;
    pushStack(initializeConvertedVerticesStack, targetCVertex);
  }
  std::cout<< *(trie->body->inheritedVertices) <<std::endl;

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

Bool isEmptyTrie(Trie *trie) { return trie->body->children->empty(); }

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

void collectDescendantConvertedVertices(TrieBody *ancestorBody,
                                        TrieBody *descendantBody);

void pushInftyDepthTrieNodesIntoGoAheadStackInner(
    TrieBody *body, std::stack<TrieBody *> *goAheadStack,
    TerminationConditionInfo *tInfo, int targetDepth) {
  if (body->depth == targetDepth) {
    if (!body->isPushedIntoGoAheadStack) {
      for (auto &v : *body->children)
        collectDescendantConvertedVertices(body, v.second);
      deleteTrieDescendants(body);
      body->isInfinitedDepth = FALSE;

      if (!isSingletonList(body->inheritedVertices)) {
        pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, body);

        for (auto &v : *body->inheritedVertices) {
          decrementOmegaArray(tInfo->distribution, OMEGA);
        }
      }
    }
  } else {
    for (auto &v : *body->children)
      pushInftyDepthTrieNodesIntoGoAheadStackInner(v.second, goAheadStack,
                                                   tInfo, targetDepth);
  }
}

void pushInftyDepthTrieNodesIntoGoAheadStack(
    Trie *trie, std::stack<TrieBody *> *goAheadStack, int targetDepth) {
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
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    pushInftyDepthTrieNodesIntoGoAheadStack(trie, goAheadStack,
                                            stepOfPropagation);
  }
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  goBackProcessOfCurrentConvertedVertices(BFSStack, goAheadStack, tInfo,
                                          stepOfPropagation);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  goAheadProcessOfCurrentTrieNodes(goAheadStack, fixCreditIndexStack, tInfo,
                                   cAfterGraph, gapOfGlobalRootMemID);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  getNextDistanceConvertedVertices(BFSStack, initializeConvertedVerticesStack,
                                   cAfterGraph);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  return;
}

void collectDescendantConvertedVertices(TrieBody *ancestorBody,
                                        TrieBody *descendantBody) {
  if (descendantBody->children->empty()) {
    while (!descendantBody->inheritedVertices->empty()) {
      auto targetCell = std::begin(*descendantBody->inheritedVertices);
      ancestorBody->inheritedVertices->splice(
          std::begin(*ancestorBody->inheritedVertices),
          *descendantBody->inheritedVertices, targetCell);
      slim::element::get<InheritedVertex>(*targetCell).ownerNode = ancestorBody;
      slim::element::get<InheritedVertex>(*targetCell).hashString->creditIndex =
          ancestorBody->depth;
      slim::element::get<InheritedVertex>(*targetCell).canonicalLabel.first =
          ancestorBody->key;
    }
  } else {
    for (auto &v : *descendantBody->children)
      collectDescendantConvertedVertices(ancestorBody, v.second);
  }
}

void makeTrieMinimumInner(TrieBody *body, TerminationConditionInfo *tInfo,
                          int stepOfPropagation) {
  if (body->depth == stepOfPropagation + 1) {
    if (body->isPushedIntoGoAheadStack) {
      for (auto &v : *body->inheritedVertices) {
        incrementOmegaArray(tInfo->distribution, OMEGA);
        slim::element::get<InheritedVertex>(v).canonicalLabel.first = body->key;
      }
    }

    if (!body->children->empty()) {
      for (auto &v : *body->children)
        collectDescendantConvertedVertices(body, v.second);
      deleteTrieDescendants(body);
    }

    body->isInfinitedDepth = TRUE;
  } else {
    for (auto &v : *body->children)
      makeTrieMinimumInner(v.second, tInfo, stepOfPropagation);
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

void makeConventionalPropagationListInner(TrieBody *body, vertex_list *list,
                                          int stepOfPropagation) {
  if (body->children->empty()) {
    if (!list->empty()) {
      list->push_front(CLASS_SENTINEL);
    }

    for (auto &v : *body->inheritedVertices) {
      list->push_front(v);
    }
  } else {
    for (auto &v : *body->children)
      makeConventionalPropagationListInner(v.second, list, stepOfPropagation);
  }
  return;
}

vertex_list *makeConventionalPropagationList(Trie *trie,
                                             int stepOfPropagation) {
  auto ret = new vertex_list();
  makeConventionalPropagationListInner(trie->body, ret, stepOfPropagation);
  return ret;
}

vertex_list::iterator getNextSentinel(vertex_list::iterator beginSentinel) {
  auto endSentinel = std::next(beginSentinel, 1);
  for (; *endSentinel != CLASS_SENTINEL;
       endSentinel = std::next(endSentinel, 1)) {
  }
  return endSentinel;
}

vertex_list::iterator
getNextSentinel(vertex_list &list, const vertex_list::iterator beginSentinel) {
  return std::find(beginSentinel, std::end(list), CLASS_SENTINEL);
}

using vertex_queue = std::priority_queue<std::pair<int, InheritedVertex *>>;

bool putClassesWithPriority(vertex_list &list,
                            vertex_list::iterator beginSentinel,
                            vertex_list::iterator endSentinel,
                            vertex_queue *cellPQueue) {
  bool isRefined = false;
  int prev_priority;
  InheritedVertex *prev_vert;
  std::tie(prev_priority, prev_vert) = cellPQueue->top();

  while (!cellPQueue->empty()) {
    int priority;
    InheritedVertex *vert;

    std::tie(priority, vert) = cellPQueue->top();
    cellPQueue->pop();

    if (priority < prev_priority) {
      list.insert(std::next(beginSentinel, 1), CLASS_SENTINEL);
      isRefined = true;
    }

    list.splice(std::next(beginSentinel, 1), *vert->ownerList, vert->ownerCell);
    std::tie(prev_priority, prev_vert) = std::tie(priority, vert);
  }

  return isRefined;
}

using classifier = Bool(vertex_list &, vertex_list::iterator,
                        vertex_list::iterator, ConvertedGraph *, int,
                        vertex_queue *);

Bool classifyConventionalPropagationList(
    vertex_list *pList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID,
    classifier classifyConventionalPropagationListInner) {
  if (pList->empty())
    return FALSE;

  Bool isRefined = FALSE;
  auto cellPQueue = vertex_queue();
  auto endSentinel = std::end(*pList);
  auto beginSentinel = endSentinel;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout<< *pList << std::endl;
  do {
    endSentinel = std::find(beginSentinel, std::end(*pList), CLASS_SENTINEL);
    if(endSentinel == pList->end()) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
    }
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    if (classifyConventionalPropagationListInner(
            *pList, beginSentinel, endSentinel, cAfterGraph,
            gapOfGlobalRootMemID, &cellPQueue)) {
      isRefined = TRUE;
    }
    beginSentinel = endSentinel;
  } while (endSentinel != std::end(*pList));

  return isRefined;
}

Bool classifyConventionalPropagationListWithTypeInner(
    vertex_list &list, vertex_list::iterator beginSentinel,
    vertex_list::iterator endSentinel, ConvertedGraph *cAfterGraph,
    int gapOfGlobalRootMemID, vertex_queue *cellPQueue) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  while (std::next(beginSentinel, 1) != endSentinel) {
    auto tmpCell = std::next(beginSentinel, 1);
    // std::cout << (*tmpCell) <<std::endl;
    int tmpPriority = slim::element::get<InheritedVertex>(*tmpCell).type;
    cellPQueue->emplace(tmpPriority,
                        &slim::element::get<InheritedVertex>(*tmpCell));
  }

  Bool isRefined =
      putClassesWithPriority(list, beginSentinel, endSentinel, cellPQueue);

  return isRefined;
}

Bool classifyConventionalPropagationListWithDegreeInner(
    vertex_list &list, vertex_list::iterator beginSentinel,
    vertex_list::iterator endSentinel, ConvertedGraph *cAfterGraph,
    int gapOfGlobalRootMemID, vertex_queue *cellPQueue) {
  while (std::next(beginSentinel, 1) != endSentinel) {
    auto tmpCell = std::next(beginSentinel, 1);

    int tmpPriority =
        numStack(&correspondingVertexInConvertedGraph(
                      &slim::element::get<InheritedVertex>(*tmpCell),
                      cAfterGraph, gapOfGlobalRootMemID)
                      ->links);
    cellPQueue->emplace(tmpPriority,
                        &slim::element::get<InheritedVertex>(*tmpCell));
  }

  Bool isRefined =
      putClassesWithPriority(list, beginSentinel, endSentinel, cellPQueue);

  return isRefined;
}

Bool classifyConventionalPropagationListWithNameLengthInner(
    vertex_list &list, vertex_list::iterator beginSentinel,
    vertex_list::iterator endSentinel, ConvertedGraph *cAfterGraph,
    int gapOfGlobalRootMemID, vertex_queue *cellPQueue) {
  while (std::next(beginSentinel, 1) != endSentinel) {
    auto tmpCell = std::next(beginSentinel, 1);

    int tmpPriority = strlen(correspondingVertexInConvertedGraph(
                                 &slim::element::get<InheritedVertex>(*tmpCell),
                                 cAfterGraph, gapOfGlobalRootMemID)
                                 ->name);
    cellPQueue->emplace(tmpPriority,
                        &slim::element::get<InheritedVertex>(*tmpCell));
  }

  Bool isRefined =
      putClassesWithPriority(list, beginSentinel, endSentinel, cellPQueue);

  return isRefined;
}

Bool classifyConventionalPropagationListWithNameCharactersInnerInner(
    vertex_list &list, vertex_list::iterator beginSentinel,
    vertex_list::iterator endSentinel, ConvertedGraph *cAfterGraph,
    int gapOfGlobalRootMemID, int index, vertex_queue *cellPQueue) {
  while (std::next(beginSentinel, 1) != endSentinel) {
    auto tmpCell = std::next(beginSentinel, 1);

    int tmpPriority = (correspondingVertexInConvertedGraph(
                           &slim::element::get<InheritedVertex>(*tmpCell),
                           cAfterGraph, gapOfGlobalRootMemID)
                           ->name)[index];
    cellPQueue->emplace(tmpPriority,
                        &slim::element::get<InheritedVertex>(*tmpCell));
  }

  Bool isRefined =
      putClassesWithPriority(list, beginSentinel, endSentinel, cellPQueue);

  return isRefined;
}

Bool classifyConventionalPropagationListWithNameCharactersInner(
    vertex_list &list, vertex_list::iterator beginSentinel,
    vertex_list::iterator endSentinel, ConvertedGraph *cAfterGraph,
    int gapOfGlobalRootMemID, vertex_queue *cellPQueue) {
  Bool isRefined = FALSE;

  int nameLength = strlen(
      correspondingVertexInConvertedGraph(
          &slim::element::get<InheritedVertex>(*(std::next(beginSentinel, 1))),
          cAfterGraph, gapOfGlobalRootMemID)
          ->name);

  int i;
  for (i = 0; i < nameLength; i++) {
    auto innerEndSentinel = beginSentinel;
    auto innerBeginSentinel = beginSentinel;

    do {
      innerEndSentinel =
          std::find(innerBeginSentinel, std::end(list), CLASS_SENTINEL);

      isRefined =
          classifyConventionalPropagationListWithNameCharactersInnerInner(
              list, innerBeginSentinel, innerEndSentinel, cAfterGraph,
              gapOfGlobalRootMemID, i, cellPQueue) ||
          isRefined;

      innerBeginSentinel = innerEndSentinel;
    } while (innerEndSentinel != endSentinel);
  }

  return isRefined;
}

Bool classifyConventionalPropagationListWithType(vertex_list *pList,
                                                 ConvertedGraph *cAfterGraph,
                                                 int gapOfGlobalRootMemID) {
  return classifyConventionalPropagationList(
      pList, cAfterGraph, gapOfGlobalRootMemID,
      classifyConventionalPropagationListWithTypeInner);
}

Bool classifyConventionalPropagationListWithDegree(vertex_list *pList,
                                                   ConvertedGraph *cAfterGraph,
                                                   int gapOfGlobalRootMemID) {
  return classifyConventionalPropagationList(
      pList, cAfterGraph, gapOfGlobalRootMemID,
      classifyConventionalPropagationListWithDegreeInner);
}

Bool classifyConventionalPropagationListWithName(vertex_list *pList,
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

Bool classifyConventionalPropagationListWithAttribute(
    vertex_list *pList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID) {
  Bool isRefined = FALSE;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  isRefined = classifyConventionalPropagationListWithType(
                  pList, cAfterGraph, gapOfGlobalRootMemID) ||
              isRefined;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  isRefined = classifyConventionalPropagationListWithDegree(
                  pList, cAfterGraph, gapOfGlobalRootMemID) ||
              isRefined;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  isRefined = classifyConventionalPropagationListWithName(
                  pList, cAfterGraph, gapOfGlobalRootMemID) ||
              isRefined;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  return isRefined;
}

void putLabelsToAdjacentVertices(vertex_list *pList,
                                 ConvertedGraph *cAfterGraph,
                                 int gapOfGlobalRootMemID) {
  if (pList->empty()) {
    return;
  }

  int tmpLabel = 0;

  auto beginSentinel = std::end(*pList);
  auto endSentinel = beginSentinel;

  do {
    endSentinel = std::find(beginSentinel, std::end(*pList), CLASS_SENTINEL);

    int tmpDegree = numStack(
        &correspondingVertexInConvertedGraph(
             &slim::element::get<InheritedVertex>(*std::next(beginSentinel, 1)),
             cAfterGraph, gapOfGlobalRootMemID)
             ->links);
    ConvertedGraphVertexType tmpType = correspondingVertexInConvertedGraph(
                                           &slim::element::get<InheritedVertex>(
                                               *(std::next(beginSentinel, 1))),
                                           cAfterGraph, gapOfGlobalRootMemID)
                                           ->type;

    int i;
    for (i = 0; i < tmpDegree; i++) {
      for (auto iteratorCell = std::next(beginSentinel, 1);
           iteratorCell != endSentinel;
           iteratorCell = std::next(iteratorCell, 1)) {
        auto &tmpLink = correspondingVertexInConvertedGraph(
                            &slim::element::get<InheritedVertex>(*iteratorCell),
                            cAfterGraph, gapOfGlobalRootMemID)
                            ->links[i];
        ConvertedGraphVertex *adjacentVertex;

        switch (tmpLink.attr) {
        case INTEGER_ATTR:
          writeStack(slim::element::get<InheritedVertex>(*(iteratorCell))
                         .conventionalPropagationMemo,
                     i, tmpLink.data.integer * 256 + INTEGER_ATTR);
          break;
        // case DOUBLE_ATTR:
        // break;
        // case STRING_ATTR:
        // break;
        case HYPER_LINK_ATTR:
          adjacentVertex = getConvertedVertexFromGraphAndIDAndType(
              cAfterGraph, tmpLink.data.ID, convertedHyperLink);
          pushStack(adjacentVertex->correspondingVertexInTrie
                        ->conventionalPropagationMemo,
                    tmpLabel * 256 + i);
          break;
        case GLOBAL_ROOT_MEM_ATTR:
          break;
        default:
          if (tmpLink.attr < 128) {
            adjacentVertex = getConvertedVertexFromGraphAndIDAndType(
                cAfterGraph, tmpLink.data.ID, convertedAtom);
            switch (tmpType) {
            case convertedAtom:
              writeStack(adjacentVertex->correspondingVertexInTrie
                             ->conventionalPropagationMemo,
                         tmpLink.attr, tmpLabel * 256 + i);
              break;
            case convertedHyperLink:
              writeStack(adjacentVertex->correspondingVertexInTrie
                             ->conventionalPropagationMemo,
                         tmpLink.attr, tmpLabel * 256 + HYPER_LINK_ATTR);
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
  } while (endSentinel != std::end(*pList));

  return;
}

Bool classifyConventionalPropagationListWithAdjacentLabelsInnerInner(
    vertex_list &list, vertex_list::iterator beginSentinel,
    vertex_list::iterator endSentinel, vertex_queue *cellPQueue) {
  while (std::next(beginSentinel, 1) != endSentinel) {
    auto tmpCell = std::next(beginSentinel, 1);

    int tmpPriority = popStack(slim::element::get<InheritedVertex>(*tmpCell)
                                   .conventionalPropagationMemo);
    cellPQueue->emplace(tmpPriority,
                        &slim::element::get<InheritedVertex>(*tmpCell));
  }

  Bool isRefined =
      putClassesWithPriority(list, beginSentinel, endSentinel, cellPQueue);

  return isRefined;
}

Bool classifyConventionalPropagationListWithAdjacentLabelsInner(
    vertex_list &list, vertex_list::iterator beginSentinel,
    vertex_list::iterator endSentinel, ConvertedGraph *cAfterGraph,
    int gapOfGlobalRootMemID, vertex_queue *cellPQueue) {
  Bool isRefined = FALSE;

  int degree = numStack(
      &correspondingVertexInConvertedGraph(
           &slim::element::get<InheritedVertex>(*(std::next(beginSentinel, 1))),
           cAfterGraph, gapOfGlobalRootMemID)
           ->links);

  int i;
  for (i = 0; i < degree; i++) {
    auto innerEndSentinel = beginSentinel;
    auto innerBeginSentinel = beginSentinel;

    do {
      innerEndSentinel =
          std::find(innerBeginSentinel, std::end(list), CLASS_SENTINEL);

      isRefined =
          classifyConventionalPropagationListWithAdjacentLabelsInnerInner(
              list, innerBeginSentinel, innerEndSentinel, cellPQueue) ||
          isRefined;

      innerBeginSentinel = innerEndSentinel;
    } while (innerEndSentinel != endSentinel);
  }

  return isRefined;
}

Bool classifyConventionalPropagationListWithAdjacentLabels(
    vertex_list *pList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID) {
  return classifyConventionalPropagationList(
      pList, cAfterGraph, gapOfGlobalRootMemID,
      classifyConventionalPropagationListWithAdjacentLabelsInner);
}

Bool refineConventionalPropagationListByPropagation(vertex_list *pList,
                                                    ConvertedGraph *cAfterGraph,
                                                    int gapOfGlobalRootMemID) {
  Bool isRefined = FALSE;

  putLabelsToAdjacentVertices(pList, cAfterGraph, gapOfGlobalRootMemID);

  isRefined = classifyConventionalPropagationListWithAdjacentLabels(
                  pList, cAfterGraph, gapOfGlobalRootMemID) ||
              isRefined;

  return isRefined;
}

Bool getStableRefinementOfConventionalPropagationList(
    vertex_list *pList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID) {
  Bool isRefined = FALSE;

  while (refineConventionalPropagationListByPropagation(pList, cAfterGraph,
                                                        gapOfGlobalRootMemID)) {
    isRefined = TRUE;
  }

  return isRefined;
}

void assureReferenceFromConvertedVerticesToInheritedVertices(
    ConvertedGraph *cAfterGraph, ConvertedGraph *cBeforeGraph,
    int gapOfGlobalRootMemID) {
  for (auto &v : cAfterGraph->atoms) {
    auto cAfterVertex = v.second;
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

  for (auto &v : cAfterGraph->hyperlinks) {
    auto cAfterVertex = v.second;
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
  for (auto &v : cBeforeGraph->atoms) {
    auto cBeforeVertex = v.second;
    if (cBeforeVertex != NULL) {
      cBeforeVertex->correspondingVertexInTrie = NULL;
    }
  }

  for (auto &v : cBeforeGraph->hyperlinks) {
    auto cBeforeVertex = v.second;
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
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  Bool verticesAreCompletelySorted =
      isDescreteTrie(&goAheadStack, tInfo, stepOfPropagation) ||
      isEmptyTrie(trie);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  if (!verticesAreCompletelySorted) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    makeTrieMinimum(trie, stepOfPropagation);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    while (!goAheadStack.empty()) {
      popTrieBodyFromGoAheadStackWithoutOverlap(&goAheadStack);
    }
  }
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  initializeConvertedVertices(&initializeConvertedVerticesStack);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  initializeConvertedVertices(&BFSStack);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  fixCreditIndex(&fixCreditIndexStack, cAfterGraph->cv, gapOfGlobalRootMemID);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  //実際のSLIMでは起きない操作
  initializeReferencesFromConvertedVerticesToInheritedVertices(
      cBeforeGraph->cv);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  *stepOfPropagationPtr = stepOfPropagation;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
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

#include <iostream>

void trieDumpInner(TrieBody *body) {
  if (body->isPushedIntoGoAheadStack) {
    fprintf(stdout, "\x1b[33m");
  }

  spacePrinter(body->depth);
  fprintf(stdout, "KEY:");
  fprintf(stdout, "%08X", body->key);
  fprintf(stdout, "\n");

  spacePrinter(body->depth);
  fprintf(stdout, "VERTICES:");
  std::cout << *body->inheritedVertices << "\n";

  if (body->isPushedIntoGoAheadStack) {
    fprintf(stdout, "\x1b[39m");
  }

  std::cout << *body->children;

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

  for (auto &v : *tBody->children)
    makeTerminationConditionMemoInner(v.second, distributionMemo, increaseMemo);

  if (!tBody->children->empty()) {
    decrementOmegaArray(increaseMemo, tBody->depth);
  }

  return;
}

void makeTerminationConditionMemo(Trie *trie, OmegaArray *distributionMemo,
                                  OmegaArray *increaseMemo) {
  for (auto &v : *trie->body->children)
    makeTerminationConditionMemoInner(v.second, distributionMemo, increaseMemo);

  return;
}

void trieDump(Trie *trie) {
  OmegaArray *distributionMemo = makeOmegaArray();
  OmegaArray *increaseMemo = makeOmegaArray();
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  setvbuf(stdout, NULL, _IONBF, BUFSIZ);
  terminationConditionInfoDump(trie->info);
  std::cout << *trie->body->children;

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
  std::cout << *org_trie->body->inheritedVertices << "\n";
  printf("\n");
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  return trie;
}
