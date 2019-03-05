#include "trie.hpp"
#include <algorithm>
#include <iostream>
#include <iterator>
#include <stack>
#include <tuple>
#include <vector>
#include <iomanip>
slim::element::conditional_ostream debug_log(std::cout);

struct hash_generator {
  ConvertedGraph *cGraph;
  int gapOfGlobalRootMemID;
  std::stack<InheritedVertex *> *fixCreditIndexStack;

  hash_generator(ConvertedGraph *cGraph, int gapOfGlobalRootMemID,
                 std::stack<InheritedVertex *> *fixCreditIndexStack)
      : cGraph(cGraph), gapOfGlobalRootMemID(gapOfGlobalRootMemID),
        fixCreditIndexStack(fixCreditIndexStack) {}

  Hash hash(InheritedVertex *iVertex, int index) {
    HashString *hashString = iVertex->hashString;
    debug_log << __FUNCTION__ << ":" << __LINE__ << *iVertex << std::endl;
    debug_log << index << std::endl;

    if (index < 0) {
      return 0;
    } else if (index < hashString->creditIndex) {
      return (*hashString->body)[index];
    } else if (index == 0) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      Hash tmp = initialHashValue(cGraph->at(*iVertex, gapOfGlobalRootMemID));
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      if (hashString->body->size() > 0) {
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        auto old = hashString->body->at(index);
        //  if (old != NULL) {
        //   free(old);
        // }
      }
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      hashString->body->push_back(uint32_t(tmp));
      hashString->creditIndex = 1;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      fixCreditIndexStack->push(iVertex);
      iVertex->isPushedIntoFixCreditIndex = true;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      return tmp;
    } else {
      Hash prevMyHash = hash(iVertex, index - 1);
      Hash adjacentHash =
          hash(cGraph->at(*iVertex, gapOfGlobalRootMemID), index);
      Hash newMyHash = (FNV_PRIME * prevMyHash) ^ adjacentHash;
      auto old = hashString->body->at(index);
      (*hashString->body)[index] = uint32_t(newMyHash);
      // if (old != NULL) {
      //   free(old);
      // }
      hashString->creditIndex = index + 1;
      fixCreditIndexStack->push(iVertex);
      iVertex->isPushedIntoFixCreditIndex = true;
      return newMyHash;
    }
  }

  Hash hash(ConvertedGraphVertex *cVertex, int index) {
    Hash ret;
    Hash sum, mul;
    Hash tmp;
    int i;

    switch (cVertex->type) {
    case convertedAtom:
      ret = OFFSET_BASIS;
      for (i = 0; i < cVertex->links.size(); i++) {
        ret *= FNV_PRIME;
        ret ^= hash(&cVertex->links[i], index - 1);
      }

      return ret;
      break;
    case convertedHyperLink:
      sum = ADD_INIT;
      mul = MUL_INIT;
      for (i = 0; i < cVertex->links.size(); i++) {
        tmp = hash(&cVertex->links[i], index - 1);
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

  Hash hash(LMNtalLink *link, int index) {
    Hash ret;

    ret = OFFSET_BASIS;
    ret *= FNV_PRIME;
    ret ^= link->attr;
    ret *= FNV_PRIME;
    switch (link->attr) {
    case INTEGER_ATTR:
      ret ^= link->data.ID;
      return ret;
      break;
    case HYPER_LINK_ATTR:
      ret ^= hash(cGraph->hyperlinks[link->data.ID]->correspondingVertexInTrie,
                  index);
      return ret;
      break;
    case GLOBAL_ROOT_MEM_ATTR:
      ret ^= link->data.ID;
      return ret;
      break;
    case DOUBLE_ATTR: // LMNtalLink非対応
    case STRING_ATTR: // LMNtalLink非対応
    default:
      if (link->attr < 128) {
        ret ^= hash(cGraph->atoms[link->data.ID]->correspondingVertexInTrie,
                    index);
        return ret;
      } else {
        CHECKER("unexpected type\n");
        exit(EXIT_FAILURE);
      }
      break;
    }
  }

  Hash initialHashValue(ConvertedGraphVertex *cVertex) {
    Hash ret = OFFSET_BASIS;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << cVertex;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    ret *= FNV_PRIME;
    ret ^= cVertex->type;
    ret *= FNV_PRIME;
    ret ^= stringHash(cVertex->name);
    ret *= FNV_PRIME;

    switch (cVertex->type) {
    case convertedAtom:
      ret ^= cVertex->links.size();
      ret *= FNV_PRIME;
      return ret;
      break;
    case convertedHyperLink:
      return ret;
      break;
    default:
      CHECKER("unexpected type\n");
      exit(EXIT_FAILURE);
      break;
    }
  }

  Hash stringHash(const char *string) {
    Hash ret = OFFSET_BASIS;
    int i;

    for (i = 0; string[i] != '\0'; i++) {
      ret ^= (Hash)string[i];
      ret *= FNV_PRIME;
    }

    return ret;
  }
};

void fixCreditIndex(std::stack<InheritedVertex *> *fixCreditIndexStack) {
  while (!fixCreditIndexStack->empty()) {
    InheritedVertex *iVertex = fixCreditIndexStack->top();
    fixCreditIndexStack->pop();
    iVertex->isPushedIntoFixCreditIndex = FALSE;
    iVertex->hashString->creditIndex = iVertex->ownerNode->depth;
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
  switch (iVertex->type) {
  case convertedAtom:
    return cAfterGraph->atoms[afterID];
  case convertedHyperLink:
    return cAfterGraph->hyperlinks[afterID];
    break;
  default:
    CHECKER("unexpected vertex type\n");
    exit(EXIT_FAILURE);
    break;
  }
}

void getNextDistanceConvertedVertices(
    std::vector<ConvertedGraphVertex *> *BFSStack,
    std::vector<ConvertedGraphVertex *> *initializeConvertedVerticesStack,
    ConvertedGraph *cAfterGraph) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  auto nextBFSStack = new std::vector<ConvertedGraphVertex *>();

  while (!BFSStack->empty()) {
    ConvertedGraphVertex *cVertex = BFSStack->back();
    BFSStack->pop_back();

    int i;
    for (i = 0; i < cVertex->links.size(); i++) {
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
          nextBFSStack->push_back(adjacentVertex);
          adjacentVertex->isVisitedInBFS = TRUE;
        }
        break;
      case GLOBAL_ROOT_MEM_ATTR:
        break;
      default:
        if (link.attr < 128) {
          adjacentVertex = cAfterGraph->atoms[link.data.ID];
          if (!adjacentVertex->isVisitedInBFS) {
            nextBFSStack->push_back(adjacentVertex);
            adjacentVertex->isVisitedInBFS = TRUE;
          }
        } else {
          CHECKER("unexpected vertex type\n");
          exit(EXIT_FAILURE);
        }
        break;
      }
    }

    initializeConvertedVerticesStack->push_back(cVertex);
  }

  nextBFSStack->swap(*BFSStack);
  delete (nextBFSStack);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  return;
}

void freeInheritedVertex(InheritedVertex *iVertex) {
  delete (iVertex->hashString);
  delete (iVertex->conventionalPropagationMemo);
  freeDisjointSetForest(iVertex->equivalenceClassOfIsomorphism);
  free(iVertex);

  return;
}

TerminationConditionInfo *makeTerminationConditionInfo() {
  TerminationConditionInfo *ret =
      (TerminationConditionInfo *)malloc(sizeof(TerminationConditionInfo));
  ret->distribution = new OmegaArray();
  ret->increase = new OmegaArray();

  return ret;
}

void freeTerminationConditionInfo(TerminationConditionInfo *info) {
  delete (info->distribution);
  delete (info->increase);
  free(info);

  return;
}

template <typename S>
void pushTrieBodyIntoGoAheadStackWithoutOverlap(S *stack, TrieBody *body) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  if (body != NULL) {
    if (!body->isPushedIntoGoAheadStack) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      stack->push(body);
      debug_log << __FUNCTION__ << ":" << __LINE__ << std::endl;
      body->isPushedIntoGoAheadStack = TRUE;
    }
  }
  return;
}

TrieBody *
popTrieBodyFromGoAheadStackWithoutOverlap(std::stack<TrieBody *> *stack) {
  TrieBody *ret = stack->top();
  stack->pop();

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
    (*tInfo->distribution)[prevNode->depth]++;
    slim::element::get<InheritedVertex>(*brotherCell).canonicalLabel.first =
        prevNode->key;

    pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, currentNode);
  } else if (currentNode->children->size() == 1) {
    TrieBody *parent = currentNode->parent;

    prevNode->parent->children->erase(prevNode->key);
    delete (prevNode);
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
    (*tInfo->distribution)[prevNode->depth]++;
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

             currentNode->children->begin()
                     ->second->inheritedVertices->size() == 1) {
    TrieBody *childNode = (TrieBody *)currentNode->children->begin()->second;
    auto brother = std::begin(*childNode->inheritedVertices);

    (*tInfo->distribution)[childNode->depth]--;

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

      (*tInfo->distribution)[currentNode->depth]--;
      if (parent->depth >= 0 && parent->children->size() != 1) {
        (*tInfo->increase)[parent->depth]--;
      }

      currentNode->parent->children->erase(currentNode->key);
      delete (currentNode);

      goBackProcessInnerSingleCommonPrefixVertex(ivertex, parent, goAheadStack,
                                                 tInfo, targetDepth);
    } else if (currentNode->inheritedVertices->size() == 1) {
      auto brother = std::begin(*currentNode->inheritedVertices);
      TrieBody *parent = currentNode->parent;

      (*tInfo->distribution)[omega_array::OMEGA]--;
      (*tInfo->distribution)[omega_array::OMEGA]--;

      goBackProcessInnerDoubleCommonPrefixVertices(
          ivertex, slim::element::get<InheritedVertex>(*brother), parent,
          currentNode, goAheadStack, tInfo, targetDepth);
    } else {
      TrieBody *parent = currentNode->parent;

      (*tInfo->distribution)[omega_array::OMEGA]--;

      goBackProcessInnerManyCommonPrefixVertices(ivertex, parent, goAheadStack,
                                                 tInfo, targetDepth);
    }
  } else {
    currentNode->inheritedVertices->splice(
        std::begin(*currentNode->inheritedVertices), *ivertex.ownerList,
        ivertex.ownerCell);
  }
}

template <typename S1>
void goBackProcessOfCurrentConvertedVertices(
    S1 *BFSStack, std::stack<TrieBody *> *goAheadStack,
    TerminationConditionInfo *tInfo, int targetDepth) {
  int i;

  for (i = 0; i < BFSStack->size(); i++) {
    ConvertedGraphVertex *cVertex = BFSStack->at(i);
    InheritedVertex *iVertex = cVertex->correspondingVertexInTrie;
    TrieBody *currentNode = iVertex->ownerNode;
    goBackProcess(*iVertex, currentNode, goAheadStack, tInfo, targetDepth);
  }

  return;
}

void goAheadProcess(TrieBody *targetNode, std::stack<TrieBody *> *goAheadStack,
                    TerminationConditionInfo *tInfo, hash_generator gen) {
  auto inheritedVerticesList = targetNode->inheritedVertices;
  auto children = targetNode->children;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  if (inheritedVerticesList->size() == 1 && children->empty() &&
      targetNode->depth != -1) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    (*tInfo->distribution)[targetNode->depth]++;
    slim::element::get<InheritedVertex>(inheritedVerticesList->front())
        .canonicalLabel.first = targetNode->key;
  } else {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    while (!inheritedVerticesList->empty()) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      auto tmpCell = std::begin(*inheritedVerticesList);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << targetNode->depth << std::endl;
      auto key = gen.hash(&slim::element::get<InheritedVertex>(*tmpCell),
                          targetNode->depth);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << key << std::endl;
      auto it = children->find(key);
      TrieBody *nextNode;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      if (it == std::end(*children)) {
        if (!children->empty()) {
          (*tInfo->increase)[targetNode->depth]++;
        }
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        nextNode = new TrieBody();
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
        auto size = nextNode->inheritedVertices->size();
        if (size == 1) {
          (*tInfo->distribution)[nextNode->depth]--;
        } else {
          for (auto i = 0; i < size; i++) {
            (*tInfo->distribution)[omega_array::OMEGA]--;
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

void goAheadProcessOfCurrentTrieNodes(std::stack<TrieBody *> *goAheadStack,
                                      TerminationConditionInfo *tInfo,
                                      hash_generator gen) {
  auto nextGoAheadStack = std::stack<TrieBody *>();

  while (!goAheadStack->empty()) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    TrieBody *targetNode =
        popTrieBodyFromGoAheadStackWithoutOverlap(goAheadStack);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    goAheadProcess(targetNode, &nextGoAheadStack, tInfo, gen);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  }

  nextGoAheadStack.swap(*goAheadStack);
}

template <typename S1, typename S2>
void deleteInheritedVerticesFromTrie(Trie *trie, S1 *deletedVertices,
                                     S2 *goAheadStack) {
  while (!deletedVertices->empty()) {
    auto targetCVertex =
        popConvertedVertexFromDiffInfoStackWithoutOverlap(deletedVertices);
    // std::cout << *targetCVertex;

    InheritedVertex *targetIVertex = targetCVertex->correspondingVertexInTrie;

    printf("%s:%d\n", __FUNCTION__, __LINE__);
    auto targetCell = targetIVertex->ownerCell;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    TrieBody *currentNode = targetIVertex->ownerNode;

    goBackProcess(*targetIVertex, currentNode, goAheadStack, trie->info, -1);

    targetIVertex->ownerList->erase(targetIVertex->ownerCell);
    delete targetIVertex;
  }
}

void addInheritedVerticesToTrie(
    Trie *trie, std::vector<ConvertedGraphVertex *> *addedVertices,
    std::vector<ConvertedGraphVertex *> *initializeConvertedVerticesStack,
    std::stack<TrieBody *> *goAheadStack, Graphinfo *cAfterGraph,
    int gapOfGlobalRootMemID) {
  if (!addedVertices->empty()) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, trie->body);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  }
  while (!addedVertices->empty()) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    ConvertedGraphVertex *targetCVertex =
        popConvertedVertexFromDiffInfoStackWithoutOverlap(addedVertices);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    trie->body->inheritedVertices->push_front(
        InheritedVertex(targetCVertex, gapOfGlobalRootMemID));
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    InheritedVertex *targetIVertex = &slim::element::get<InheritedVertex>(
        trie->body->inheritedVertices->front());
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    targetCVertex->correspondingVertexInTrie = targetIVertex;
    targetIVertex->ownerList = trie->body->inheritedVertices;
    targetIVertex->ownerCell = std::begin(*trie->body->inheritedVertices);
    targetCVertex->isVisitedInBFS = TRUE;
    initializeConvertedVerticesStack->push_back(targetCVertex);
  }
  std::cout << *(trie->body->inheritedVertices) << std::endl;

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

void initializeConvertedVertices(
    std::vector<ConvertedGraphVertex *> *initializeConvertedVerticesStack) {
  while (!initializeConvertedVerticesStack->empty()) {
    ConvertedGraphVertex *cVertex = initializeConvertedVerticesStack->back();
    initializeConvertedVerticesStack->pop_back();
    cVertex->isVisitedInBFS = FALSE;
  }

  return;
}

Bool isEmptyTrie(Trie *trie) { return trie->body->children->empty(); }

template <typename S>
Bool isDescreteTrie(S *goAheadStack, TerminationConditionInfo *tInfo,
                    int depth) {
  return omega_array::maxIndex(*tInfo->distribution) == depth &&
         goAheadStack->empty();
}

Bool isRefinedTrie(TerminationConditionInfo *tInfo, int step) {
  printf("%s:%d:step=%d\n", __FUNCTION__, __LINE__, step);
  int v;
  try {
    v = tInfo->increase->at(step);
  }
  catch(std::out_of_range&) {
    v = 0;
  }
  return v != 0;
}

template <typename S>
Bool triePropagationIsContinued(S *goAheadStack,
                                TerminationConditionInfo *tInfo, int step) {
  return isRefinedTrie(tInfo, step) &&
         !isDescreteTrie(goAheadStack, tInfo, step);
}

void TrieBody::pushInftyDepthTrieNodesIntoGoAheadStackInner(
    std::stack<TrieBody *> *goAheadStack, TerminationConditionInfo *tInfo,
    int targetDepth) {
  if (this->depth == targetDepth) {
    if (!this->isPushedIntoGoAheadStack) {
      for (auto &v : *this->children)
        this->collectDescendantConvertedVertices(v.second);
      this->clearDescendants();
      this->isInfinitedDepth = FALSE;

      if (inheritedVertices->size() != 1) {
        pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, this);

        for (auto &v : *this->inheritedVertices) {
          (*tInfo->distribution)[omega_array::OMEGA]--;
        }
      }
    }
  } else {
    for (auto &v : *this->children)
      v.second->pushInftyDepthTrieNodesIntoGoAheadStackInner(
          goAheadStack, tInfo, targetDepth);
  }
}

void pushInftyDepthTrieNodesIntoGoAheadStack(
    Trie *trie, std::stack<TrieBody *> *goAheadStack, int targetDepth) {
  trie->body->pushInftyDepthTrieNodesIntoGoAheadStackInner(
      goAheadStack, trie->info, targetDepth);

  return;
}

template <typename S1, typename S2, typename S3>
void triePropagateInner(Trie *trie, S1 *BFSStack,
                        S2 *initializeConvertedVerticesStack, S3 *goAheadStack,
                        TerminationConditionInfo *tInfo, int stepOfPropagation,
                        hash_generator data) {
  if (omega_array::maxIndex(*tInfo->distribution) == omega_array::OMEGA &&
      omega_array::maxIndex(*tInfo->increase) == stepOfPropagation - 1) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    pushInftyDepthTrieNodesIntoGoAheadStack(trie, goAheadStack,
                                            stepOfPropagation);
  }
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  goBackProcessOfCurrentConvertedVertices(BFSStack, goAheadStack, tInfo,
                                          stepOfPropagation);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  goAheadProcessOfCurrentTrieNodes(goAheadStack, tInfo, data);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  getNextDistanceConvertedVertices(BFSStack, initializeConvertedVerticesStack,
                                   data.cGraph);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  return;
}

void TrieBody::collectDescendantConvertedVertices(TrieBody *descendantBody) {
  if (descendantBody->children->empty()) {
    while (!descendantBody->inheritedVertices->empty()) {
      auto targetCell = std::begin(*descendantBody->inheritedVertices);
      this->inheritedVertices->splice(std::begin(*this->inheritedVertices),
                                      *descendantBody->inheritedVertices,
                                      targetCell);
      slim::element::get<InheritedVertex>(*targetCell).ownerNode = this;
      slim::element::get<InheritedVertex>(*targetCell).hashString->creditIndex =
          this->depth;
      slim::element::get<InheritedVertex>(*targetCell).canonicalLabel.first =
          this->key;
    }
  } else {
    for (auto &v : *descendantBody->children)
      collectDescendantConvertedVertices(v.second);
  }
}

void TrieBody::makeTrieMinimumInner(TerminationConditionInfo *tInfo,
                                    int stepOfPropagation) {
  if (this->depth == stepOfPropagation + 1) {
    if (this->isPushedIntoGoAheadStack) {
      for (auto &v : *this->inheritedVertices) {
        (*tInfo->distribution)[omega_array::OMEGA]++;
        slim::element::get<InheritedVertex>(v).canonicalLabel.first = this->key;
      }
    }

    if (!this->children->empty()) {
      for (auto &v : *this->children)
        this->collectDescendantConvertedVertices(v.second);
      this->clearDescendants();
    }

    this->isInfinitedDepth = TRUE;
  } else {
    for (auto &v : *this->children)
      v.second->makeTrieMinimumInner(tInfo, stepOfPropagation);
  }

  return;
}

void makeTrieMinimum(Trie *trie, int stepOfPropagation) {
  TerminationConditionInfo *tInfo = trie->info;
  trie->body->makeTrieMinimumInner(tInfo, stepOfPropagation);
  omega_array::move_to_omega_larger_than(*tInfo->distribution,
                                         stepOfPropagation);
  omega_array::clear_finite_larger_than(*tInfo->increase, stepOfPropagation);
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
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  bool isRefined = false;
  int prev_priority;
  InheritedVertex *prev_vert;
  std::tie(prev_priority, prev_vert) = cellPQueue->top();
  std::cout << *prev_vert << std::endl;
  while (!cellPQueue->empty()) {
    int priority;
    InheritedVertex *vert;
    std::tie(priority, vert) = cellPQueue->top();
    cellPQueue->pop();
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    if (priority < prev_priority) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      list.insert(std::next(beginSentinel, 1), CLASS_SENTINEL);
      isRefined = true;
    }
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << list << std::endl;
    // list.insert(list.begin(), *vert);
    // list.insert(std::next(beginSentinel, 1), *vert);
    std::cout << list << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
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
  std::cout << *pList << std::endl;
  do {
    endSentinel =
        std::find(next(beginSentinel, 1), std::end(*pList), CLASS_SENTINEL);
    if (endSentinel == pList->end()) printf("%s:%d\n", __FUNCTION__, __LINE__);
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
  std::cout<<(*(std::next(beginSentinel, 1)))<<std::endl;
  while (std::next(beginSentinel, 1) != endSentinel) {
    auto tmpCell = std::next(beginSentinel, 1);
    std::cout << (*tmpCell) << std::endl;
    list.erase(tmpCell);
    std::cout << (*tmpCell) << std::endl;
    std::cout << list << std::endl;
    int tmpPriority = slim::element::get<InheritedVertex>(*tmpCell).type;
    cellPQueue->emplace(tmpPriority,
                        &slim::element::get<InheritedVertex>(*tmpCell));
  }
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  Bool isRefined =
      putClassesWithPriority(list, beginSentinel, endSentinel, cellPQueue);

  return isRefined;
}

Bool classifyConventionalPropagationListWithDegreeInner(
    vertex_list &list, vertex_list::iterator beginSentinel,
    vertex_list::iterator endSentinel, ConvertedGraph *cAfterGraph,
    int gapOfGlobalRootMemID, vertex_queue *cellPQueue) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  while (std::next(beginSentinel, 1) != endSentinel) {
    auto tmpCell = std::next(beginSentinel, 1);

    int tmpPriority = correspondingVertexInConvertedGraph(
                          &slim::element::get<InheritedVertex>(*tmpCell),
                          cAfterGraph, gapOfGlobalRootMemID)
                          ->links.size();
    cellPQueue->emplace(tmpPriority,
                        &slim::element::get<InheritedVertex>(*tmpCell));
    list.erase(tmpCell);
  }

  Bool isRefined =
      putClassesWithPriority(list, beginSentinel, endSentinel, cellPQueue);

  return isRefined;
}

Bool classifyConventionalPropagationListWithNameLengthInner(
    vertex_list &list, vertex_list::iterator beginSentinel,
    vertex_list::iterator endSentinel, ConvertedGraph *cAfterGraph,
    int gapOfGlobalRootMemID, vertex_queue *cellPQueue) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  while (std::next(beginSentinel, 1) != endSentinel) {
    auto tmpCell = std::next(beginSentinel, 1);

    int tmpPriority =
        strlen(cAfterGraph
                   ->at(slim::element::get<InheritedVertex>(*tmpCell),
                        gapOfGlobalRootMemID)
                   ->name);
    list.erase(tmpCell);
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
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  while (std::next(beginSentinel, 1) != endSentinel) {
    auto tmpCell = std::next(beginSentinel, 1);

    int tmpPriority = (cAfterGraph
                           ->at(slim::element::get<InheritedVertex>(*tmpCell),
                                gapOfGlobalRootMemID)
                           ->name)[index];
    list.erase(tmpCell);
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
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  Bool isRefined = FALSE;

  int nameLength = strlen(cAfterGraph
                              ->at(slim::element::get<InheritedVertex>(
                                       *(std::next(beginSentinel, 1))),
                                   gapOfGlobalRootMemID)
                              ->name);

  int i;
  for (i = 0; i < nameLength; i++) {
    auto innerEndSentinel = beginSentinel;
    auto innerBeginSentinel = beginSentinel;

    do {
      innerEndSentinel = std::find(std::next(innerBeginSentinel, 1),
                                   std::end(list), CLASS_SENTINEL);

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
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  Bool isRefined = FALSE;
  isRefined = classifyConventionalPropagationList(
                  pList, cAfterGraph, gapOfGlobalRootMemID,
                  classifyConventionalPropagationListWithNameLengthInner) ||
              isRefined;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  isRefined = classifyConventionalPropagationList(
                  pList, cAfterGraph, gapOfGlobalRootMemID,
                  classifyConventionalPropagationListWithNameCharactersInner) ||
              isRefined;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  return isRefined;
}

void classifyConventionalPropagationListWithAttribute(
    vertex_list *pList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  classifyConventionalPropagationListWithType(pList, cAfterGraph, gapOfGlobalRootMemID);

  printf("%s:%d\n", __FUNCTION__, __LINE__);
  classifyConventionalPropagationListWithDegree(pList, cAfterGraph, gapOfGlobalRootMemID);

  printf("%s:%d\n", __FUNCTION__, __LINE__);
  classifyConventionalPropagationListWithName(pList, cAfterGraph, gapOfGlobalRootMemID);

  printf("%s:%d\n", __FUNCTION__, __LINE__);
}

void putLabelsToAdjacentVertices(vertex_list *pList,
                                 ConvertedGraph *cAfterGraph,
                                 int gapOfGlobalRootMemID) {
  if (pList->empty()) {
    return;
  }
  debug_log << __FUNCTION__ << ":" << __LINE__ << std::endl;
  int tmpLabel = 0;

  auto beginSentinel = std::end(*pList);
  auto endSentinel = beginSentinel;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  do {
    endSentinel =
        std::find(next(beginSentinel, 1), std::end(*pList), CLASS_SENTINEL);

    int tmpDegree =
        correspondingVertexInConvertedGraph(
            &slim::element::get<InheritedVertex>(*std::next(beginSentinel, 1)),
            cAfterGraph, gapOfGlobalRootMemID)
            ->links.size();
    ConvertedGraphVertexType tmpType = correspondingVertexInConvertedGraph(
                                           &slim::element::get<InheritedVertex>(
                                               *(std::next(beginSentinel, 1))),
                                           cAfterGraph, gapOfGlobalRootMemID)
                                           ->type;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << *(std::next(beginSentinel, 1)) << std::endl;
    std::cout << tmpDegree << std::endl;

    int i;
    for (i = 0; i < tmpDegree; i++) {
      for (auto iteratorCell = std::next(beginSentinel, 1);
           iteratorCell != endSentinel;
           iteratorCell = std::next(iteratorCell, 1)) {

        printf("%s:%d\n", __FUNCTION__, __LINE__);
        std::cout << *(std::next(beginSentinel, 1)) << std::endl;
        auto &tmpLink = correspondingVertexInConvertedGraph(
                            &slim::element::get<InheritedVertex>(*iteratorCell),
                            cAfterGraph, gapOfGlobalRootMemID)
                            ->links[i];
        ConvertedGraphVertex *adjacentVertex;
        std::cout << __FUNCTION__ << ":" << __LINE__ << std::endl;
        std::cout << tmpLink.attr << std::endl;

        switch (tmpLink.attr) {
        case INTEGER_ATTR:
          printf("%s:%d\n", __FUNCTION__, __LINE__);
          slim::element::get<InheritedVertex>(*(iteratorCell))
              .conventionalPropagationMemo->push_back(
                  tmpLink.data.integer * 256 + INTEGER_ATTR);
          break;
        // case DOUBLE_ATTR:
        // break;
        // case STRING_ATTR:
        // break;
        case HYPER_LINK_ATTR:
          printf("%s:%d\n", __FUNCTION__, __LINE__);
          adjacentVertex = getConvertedVertexFromGraphAndIDAndType(
              cAfterGraph, tmpLink.data.ID, convertedHyperLink);
          adjacentVertex->correspondingVertexInTrie->conventionalPropagationMemo
              ->push_back(tmpLabel * 256 + i);
          break;
        case GLOBAL_ROOT_MEM_ATTR:
          printf("%s:%d\n", __FUNCTION__, __LINE__);
          break;
        default:
          if (tmpLink.attr < 128) {
            printf("%s:%d\n", __FUNCTION__, __LINE__);
            adjacentVertex = cAfterGraph->at(tmpLink.data.ID, convertedAtom);
            debug_log << __FUNCTION__ << ":" << __LINE__ << std::endl;
            debug_log << adjacentVertex << std::endl;

            std::cout << (*adjacentVertex);
            std::cout << adjacentVertex->correspondingVertexInTrie << std::endl;
            debug_log << "name"
                      << ":" << adjacentVertex->correspondingVertexInTrie->name
                      << std::endl;
            debug_log << "before_ID"
                      << ":"
                      << adjacentVertex->correspondingVertexInTrie->beforeID
                      << std::endl;
            switch (tmpType) {
            case convertedAtom:
              printf("%s:%d\n", __FUNCTION__, __LINE__);
              std::cout << *(adjacentVertex->correspondingVertexInTrie)
                        << std::endl;
              std::cout << "numStack(Memo) = "
                        << adjacentVertex->correspondingVertexInTrie
                               ->conventionalPropagationMemo->size()
                        << std::endl;
              adjacentVertex->correspondingVertexInTrie
                  ->conventionalPropagationMemo->push_back(tmpLabel * 256 + i);
              printf("%s:%d\n", __FUNCTION__, __LINE__);
              break;
            case convertedHyperLink:
              printf("%s:%d\n", __FUNCTION__, __LINE__);
              adjacentVertex->correspondingVertexInTrie
                  ->conventionalPropagationMemo->push_back(tmpLabel * 256 +
                                                           HYPER_LINK_ATTR);
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
          printf("%s:%d\n", __FUNCTION__, __LINE__);
          break;
        }
        printf("%s:%d\n", __FUNCTION__, __LINE__);
      }
    }

    tmpLabel++;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    beginSentinel = endSentinel;
  } while (endSentinel != std::end(*pList));

  return;
}

Bool classifyConventionalPropagationListWithAdjacentLabelsInnerInner(
    vertex_list &list, vertex_list::iterator beginSentinel,
    vertex_list::iterator endSentinel, vertex_queue *cellPQueue) {
  while (std::next(beginSentinel, 1) != endSentinel) {
    auto tmpCell = std::next(beginSentinel, 1);

    auto &memo = *slim::element::get<InheritedVertex>(*tmpCell)
                      .conventionalPropagationMemo;
    int tmpPriority = memo.back();
    memo.pop_back();
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

  int degree =
      correspondingVertexInConvertedGraph(
          &slim::element::get<InheritedVertex>(*(std::next(beginSentinel, 1))),
          cAfterGraph, gapOfGlobalRootMemID)
          ->links.size();

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
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  Bool isRefined = FALSE;

  putLabelsToAdjacentVertices(pList, cAfterGraph, gapOfGlobalRootMemID);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  isRefined = classifyConventionalPropagationListWithAdjacentLabels(
                  pList, cAfterGraph, gapOfGlobalRootMemID) ||
              isRefined;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  return isRefined;
}

Bool getStableRefinementOfConventionalPropagationList(
    vertex_list *pList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID) {
  Bool isRefined = FALSE;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
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
        std::cout << *cAfterVertex;
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        ConvertedGraphVertex *cBeforeVertex = cBeforeGraph->at(
            cAfterVertex->ID - gapOfGlobalRootMemID, cAfterVertex->type);
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
        auto cBeforeVertex = cBeforeGraph->at(
            cAfterVertex->ID - gapOfGlobalRootMemID, cAfterVertex->type);
        cAfterVertex->correspondingVertexInTrie =
            cBeforeVertex->correspondingVertexInTrie;
        cAfterVertex->correspondingVertexInTrie->beforeID = cBeforeVertex->ID;
      }
    }
  }

  return;
}

bool Trie::propagate(DiffInfo *diffInfo, Graphinfo *cAfterGraph,
                     Graphinfo *cBeforeGraph, int gapOfGlobalRootMemID,
                     int *stepOfPropagationPtr) {
  std::stack<TrieBody *> goAheadStack;
  std::vector<ConvertedGraphVertex *> BFSStack;
  std::vector<ConvertedGraphVertex *> initializeConvertedVerticesStack;
  std::stack<InheritedVertex *> fixCreditIndexStack;
  TerminationConditionInfo *tInfo = this->info;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  deleteInheritedVerticesFromTrie(this, diffInfo->deletedVertices,
                                  &goAheadStack);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  addInheritedVerticesToTrie(this, diffInfo->addedVertices,
                             &initializeConvertedVerticesStack, &goAheadStack,
                             cAfterGraph, gapOfGlobalRootMemID);
  auto hash_gen = hash_generator(cAfterGraph->cv, gapOfGlobalRootMemID,
                                 &fixCreditIndexStack);
  printf("%s:%d\n", __FUNCTION__, __LINE__);

  for (auto i = cAfterGraph->cv->atoms.begin();
       i != cAfterGraph->cv->atoms.end(); ++i) {
    std::cout << *i->second;
    debug_log << i->second << std::endl;
    // std::cout << *(i->second->correspondingVertexInTrie) << std::endl;
  }

  //実際のSLIMでは起きない操作
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  assureReferenceFromConvertedVerticesToInheritedVertices(
      cAfterGraph->cv, cBeforeGraph->cv, gapOfGlobalRootMemID);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  moveInheritedRelinkedVerticesToBFSStack(
      diffInfo->relinkedVertices, &initializeConvertedVerticesStack, &BFSStack);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  int stepOfPropagation = -1;
  goAheadProcessOfCurrentTrieNodes(&goAheadStack, tInfo, hash_gen);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  stepOfPropagation = 0;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  goAheadProcessOfCurrentTrieNodes(&goAheadStack, tInfo, hash_gen);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  while (triePropagationIsContinued(&goAheadStack, tInfo, stepOfPropagation)) {
    stepOfPropagation++;
    triePropagateInner(this, &BFSStack, &initializeConvertedVerticesStack,
                       &goAheadStack, tInfo, stepOfPropagation,
                       hash_generator(cAfterGraph->cv, gapOfGlobalRootMemID,
                                      &fixCreditIndexStack));
  }
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  Bool verticesAreCompletelySorted =
      isDescreteTrie(&goAheadStack, tInfo, stepOfPropagation) ||
      isEmptyTrie(this);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  if (!verticesAreCompletelySorted) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    makeTrieMinimum(this, stepOfPropagation);
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
  fixCreditIndex(&fixCreditIndexStack);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  //実際のSLIMでは起きない操作
  cBeforeGraph->cv->clearReferencesFromConvertedVerticesToInheritedVertices();
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  *stepOfPropagationPtr = stepOfPropagation;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  this->dump();
  return verticesAreCompletelySorted;
}

void spacePrinter(int length) {
  int i;
  for (i = 0; i < length; i++) {
    printf("    ");
  }

  return;
}

#include <iostream>

void terminationConditionInfoDump(TerminationConditionInfo *tInfo) {
  fprintf(stdout, "DISTRIBUTION:");
  std::cout << (*tInfo->distribution) << "\n";
  fprintf(stdout, "INCREASE    :");
  std::cout << (*tInfo->increase) << "\n";

  return;
}

void TrieBody::makeTerminationConditionMemoInner(OmegaArray *distributionMemo,
                                                 OmegaArray *increaseMemo) {
  if (!this->isPushedIntoGoAheadStack) {
    if (inheritedVertices->size() != 1) {
      (*distributionMemo)[depth]++;
    } else {
      for (auto iterator = std::begin(*this->inheritedVertices);
           iterator != std::end(*this->inheritedVertices);
           iterator = std::next(iterator, 1)) {
        (*distributionMemo)[omega_array::OMEGA]++;
      }
    }
  }

  if (this->depth != 0) {
    (*increaseMemo)[depth - 1]++;
  }

  for (auto &v : *this->children)
    v.second->makeTerminationConditionMemoInner(distributionMemo, increaseMemo);

  if (!this->children->empty()) {
    (*increaseMemo)[depth]--;
  }

  return;
}

void makeTerminationConditionMemo(Trie *trie, OmegaArray *distributionMemo,
                                  OmegaArray *increaseMemo) {
  for (auto &v : *trie->body->children)
    v.second->makeTerminationConditionMemoInner(distributionMemo, increaseMemo);

  return;
}

inline std::ostream &operator<<(std::ostream &os, const TrieBody &body) {
  if(body.depth!=-1) {
    if (body.isPushedIntoGoAheadStack)
      os << "\x1b[33m";

    for (int i = 0; i < body.depth; i++)
      os << "    ";
    os << "KEY:";
    os <<std::uppercase<<std::setw(8)<<std::setfill('0')<< std::hex << body.key;
    os << "\n";

    for (int i = 0; i < body.depth; i++)
      os << "    ";
    os << "VERTICES:";
    os << *body.inheritedVertices << "\n";

    if (body.isPushedIntoGoAheadStack) {
      os << "\x1b[39m";
    }
  }
  for (auto i : *body.children) {
    os << *i.second;
  }
  return os;
}

void Trie::dump() {
  OmegaArray *distributionMemo = new OmegaArray();
  OmegaArray *increaseMemo = new OmegaArray();
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  setvbuf(stdout, NULL, _IONBF, BUFSIZ);
  terminationConditionInfoDump(this->info);
  std::cout << *(this->body);
  makeTerminationConditionMemo(this, distributionMemo, increaseMemo);

  /*
   * termination condition memoの作成がうまくいっていないので現在コメントアウト
   */
  // if (*distributionMemo != *info->distribution ||
  //     *increaseMemo != *info->increase) {
  //   fprintf(
  //       stderr,
  //       "WRONG TerminationConditionInfo "
  //       "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
  //   fprintf(stderr, "CORRECT DISTRIBUTION:");
  //   std::cerr << *distributionMemo << "\n";
  //   fprintf(stderr, "CORRECT INCREASE    :");
  //   std::cerr << *increaseMemo << "\n";
  //   exit(EXIT_FAILURE);
  // }

  delete (distributionMemo);
  delete (increaseMemo);
  printf("\n");
  return;
}

void terminationConditionInfoDumpExperiment(TerminationConditionInfo *tInfo) {
  fprintf(stdout, "DISTRIBUTION:\n");
  std::cout << *tInfo->distribution << "\n";
  fprintf(stdout, "INCREASE    :\n");
  std::cout << *tInfo->increase << "\n";

  return;
}

void terminationConditionInfoDumpExperimentFromTrie(Trie *trie) {
  setvbuf(stdout, NULL, _IONBF, BUFSIZ);
  terminationConditionInfoDumpExperiment(trie->info);

  return;
}
Trie *Trie::gen_tmp_trie_from_originaltrie_and_gi(Graphinfo *org_gi,
                                                  Graphinfo *tmp_gi) {
  Trie *trie = new Trie();
  this->dump();
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << *this->body->inheritedVertices << "\n";
  printf("\n");
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  return trie;
}
