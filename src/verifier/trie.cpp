#include "trie.hpp"
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <stack>
#include <tuple>
#include <vector>
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
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << "index=" << index<< std::endl;
    std::cout << "iVertex:" << *iVertex << std::endl;
    std::cout << "hashString:" << *hashString << std::endl;


    if (index < 0) {
      return 0;
    } else if (index < hashString->creditIndex) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      return (hashString->body)[index];
    } else if (index == 0) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      Hash tmp = initialHashValue(cGraph->at(*iVertex, gapOfGlobalRootMemID));
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      if (hashString->body.size() > 0) {
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        // auto old = hashString->body->at(index);
        //  if (old != NULL) {
        //   free(old);
        // }
      }
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      hashString->body.push_back(uint32_t(tmp));
      hashString->creditIndex = 1;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      fixCreditIndexStack->push(iVertex);
      iVertex->isPushedIntoFixCreditIndex = true;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      return tmp;
    } else {

      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << "index=" << index<< std::endl;
      Hash prevMyHash = hash(iVertex, index - 1);
      std::cout << "prevMyHash=" << prevMyHash << std::endl;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << "index=" << index<< std::endl;
      ConvertedGraphVertex *cv = iVertex->correspondingVertex;
      std::cout << *cv << std::endl;
      Hash adjacentHash =
          hash(cv, index);
      std::cout << "adjacentHash=" << adjacentHash << std::endl;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << "index=" << index<< std::endl;
      Hash newMyHash = (FNV_PRIME * prevMyHash) ^ adjacentHash;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << "newMyHash=" << newMyHash << std::endl;
      std::cout << "hashString=" <<*hashString<< std::endl;
      std::cout << "index=" << index << std::endl;
      //auto old = hashString->body.at(index);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      (hashString->body)[index] = uint32_t(newMyHash);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      // if (old != NULL) {
      //   free(old);
      // }
      hashString->creditIndex = index + 1;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      fixCreditIndexStack->push(iVertex);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      iVertex->isPushedIntoFixCreditIndex = true;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
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
    std::cout << *cVertex << std::endl;
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
correspondingVertexInConvertedGraph(const InheritedVertex *iVertex,
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
                                                int targetDepth,
						vertex_list *tmp_delete_lst,
						bool del_f) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  auto targetCell = target.ownerCell;
  if (targetDepth == currentNode->depth) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    if(!del_f) {
      currentNode->inheritedVertices->splice(
					     std::begin(*currentNode->inheritedVertices), *tmp_delete_lst,
					     std::begin(*tmp_delete_lst));
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      target.ownerList = currentNode->inheritedVertices;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      slim::element::get<InheritedVertex>(*targetCell).ownerNode = currentNode;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      slim::element::get<InheritedVertex>(*targetCell).hashString->creditIndex =
        currentNode->depth;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, currentNode);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
    }
  } else {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    goBackProcessInnerManyCommonPrefixVertices(
					       target, currentNode->parent, goAheadStack, tInfo, targetDepth, tmp_delete_lst, del_f);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  }
  printf("%s:%d\n", __FUNCTION__, __LINE__);
}

template <typename S>
void goBackProcessInnerDoubleCommonPrefixVertices(
    InheritedVertex &target, InheritedVertex &brother, TrieBody *currentNode,
    TrieBody *prevNode, S *goAheadStack, TerminationConditionInfo *tInfo,
    int targetDepth, vertex_list *tmp_delete_lst, bool del_f) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  auto targetCell = target.ownerCell;
  auto brotherCell = brother.ownerCell;
  if (targetDepth == currentNode->depth) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    if (!del_f){
      currentNode->inheritedVertices->splice(
					     std::begin(*currentNode->inheritedVertices), *tmp_delete_lst,
					     std::begin(*tmp_delete_lst));
      target.ownerList = currentNode->inheritedVertices;
      slim::element::get<InheritedVertex>(*targetCell).ownerNode = currentNode;
      slim::element::get<InheritedVertex>(*targetCell).hashString->creditIndex =
        currentNode->depth;
    }
    prevNode->inheritedVertices->splice(
        std::begin(*prevNode->inheritedVertices), *brother.ownerList,
        brotherCell);
    brother.ownerList = prevNode->inheritedVertices;
    slim::element::get<InheritedVertex>(*brotherCell).ownerNode = prevNode;
    slim::element::get<InheritedVertex>(*brotherCell).hashString->creditIndex =
        prevNode->depth;
    (*tInfo->distribution)[prevNode->depth]++;
    slim::element::get<InheritedVertex>(*brotherCell).canonicalLabel.first =
        prevNode->key;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, currentNode);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  } else if (currentNode->children->size() == 1) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    TrieBody *parent = currentNode->parent;

    prevNode->parent->children->erase(prevNode->key);
    delete (prevNode);
    goBackProcessInnerDoubleCommonPrefixVertices(
						 target, brother, parent, currentNode, goAheadStack, tInfo, targetDepth, tmp_delete_lst, del_f);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  } else {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    TrieBody *parent = currentNode->parent;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    prevNode->inheritedVertices->splice(
        std::begin(*prevNode->inheritedVertices), *brother.ownerList,
        brotherCell);
    brother.ownerList = prevNode->inheritedVertices;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    slim::element::get<InheritedVertex>(*brotherCell).ownerNode = prevNode;
    slim::element::get<InheritedVertex>(*brotherCell).hashString->creditIndex =
        prevNode->depth;
    (*tInfo->distribution)[prevNode->depth]++;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    slim::element::get<InheritedVertex>(*brotherCell).canonicalLabel.first =
        prevNode->key;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    goBackProcessInnerManyCommonPrefixVertices(target, parent, goAheadStack,
                                               tInfo, targetDepth, tmp_delete_lst, del_f);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  }
}

template <typename S>
void goBackProcessInnerSingleCommonPrefixVertex(InheritedVertex &ivertex,
                                                TrieBody *currentNode,
                                                S *goAheadStack,
                                                TerminationConditionInfo *tInfo,
                                                int targetDepth,
						vertex_list *tmp_delete_lst,
						bool del_f) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  auto targetCell = ivertex.ownerCell;
  std::cout << *targetCell << std::endl;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  if (targetDepth == currentNode->depth) {
    if(!del_f) {
      currentNode->inheritedVertices->splice(
					     std::begin(*currentNode->inheritedVertices), *tmp_delete_lst,
					     std::begin(*tmp_delete_lst));
      ivertex.ownerList = currentNode->inheritedVertices;
      slim::element::get<InheritedVertex>(*targetCell).ownerNode = currentNode;
      slim::element::get<InheritedVertex>(*targetCell).hashString->creditIndex =
        currentNode->depth;
      pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, currentNode);      
    }
  } else if (currentNode->children->size() == 1 &&

             currentNode->children->begin()
                     ->second->inheritedVertices->size() == 1) {
    TrieBody *childNode = (TrieBody *)currentNode->children->begin()->second;
    auto brother = std::begin(*childNode->inheritedVertices);

    (*tInfo->distribution)[childNode->depth]--;

    goBackProcessInnerDoubleCommonPrefixVertices(
        ivertex, slim::element::get<InheritedVertex>(*brother), currentNode,
        childNode, goAheadStack, tInfo, targetDepth, tmp_delete_lst, del_f);
  } else {
    TrieBody *parent = currentNode->parent;

    goBackProcessInnerManyCommonPrefixVertices(ivertex, parent, goAheadStack,
                                               tInfo, targetDepth, tmp_delete_lst, del_f);
  }

  return;
}

// trie is minimal for uniqueness!!
template <typename S>
void goBackProcess(InheritedVertex &ivertex, TrieBody *currentNode,
                   S *goAheadStack, TerminationConditionInfo *tInfo,
                   int targetDepth,
		   vertex_list *tmp_delete_lst,
		   bool del_f) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << "currentNode->depth:" << currentNode->depth << std::endl;
  if (targetDepth < currentNode->depth) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    if (currentNode->inheritedVertices->empty()) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      TrieBody *parent = currentNode->parent;

      (*tInfo->distribution)[currentNode->depth]--;
      if (parent->depth >= 0 && parent->children->size() != 1) {
	printf("%s:%d\n", __FUNCTION__, __LINE__);
        (*tInfo->increase)[parent->depth]--;
      }
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      currentNode->parent->children->erase(currentNode->key);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      delete (currentNode);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      goBackProcessInnerSingleCommonPrefixVertex(ivertex, parent, goAheadStack,
                                                 tInfo, targetDepth, tmp_delete_lst, del_f);
    } else if (currentNode->inheritedVertices->size() == 1) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      auto brother = std::begin(*currentNode->inheritedVertices);
      TrieBody *parent = currentNode->parent;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      (*tInfo->distribution)[omega_array::OMEGA]--;
      (*tInfo->distribution)[omega_array::OMEGA]--;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      goBackProcessInnerDoubleCommonPrefixVertices(
          ivertex, slim::element::get<InheritedVertex>(*brother), parent,
          currentNode, goAheadStack, tInfo, targetDepth, tmp_delete_lst, del_f);
    } else {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      TrieBody *parent = currentNode->parent;

      (*tInfo->distribution)[omega_array::OMEGA]--;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      goBackProcessInnerManyCommonPrefixVertices(ivertex, parent, goAheadStack,
                                                 tInfo, targetDepth, tmp_delete_lst, del_f);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
    }
  } else {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    currentNode->inheritedVertices->splice(
        std::begin(*currentNode->inheritedVertices), *tmp_delete_lst,
        std::begin(*tmp_delete_lst));
    ivertex.ownerList = currentNode->inheritedVertices;
  }
  printf("%s:%d\n", __FUNCTION__, __LINE__);
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
    auto targetCell = iVertex->ownerCell;
    vertex_list *tmp_delete_lst = new vertex_list();
    tmp_delete_lst->splice(std::begin(*tmp_delete_lst), *iVertex->ownerList, targetCell);
    
    goBackProcess(*iVertex, currentNode, goAheadStack, tInfo, targetDepth, tmp_delete_lst, false);
  }

  return;
}

void goAheadProcess(TrieBody *targetNode, std::stack<TrieBody *> *goAheadStack,
                    TerminationConditionInfo *tInfo, hash_generator gen) {
  auto inheritedVerticesList = targetNode->inheritedVertices;
  auto children = targetNode->children;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << "--inheritedVerticesList--" << std::endl;
  std::cout << *inheritedVerticesList << std::endl;
  if (inheritedVerticesList->size() == 1 && children->empty() &&
      targetNode->depth != -1) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    printf("-------goAhead(SINGLETONE)-------\n");
    std::cout << "DEPTH:" << targetNode->depth << std::endl;
    terminationConditionInfoDump(tInfo);
    (*tInfo->distribution)[targetNode->depth]++;
    printf("-------goAhead(SINGLETONE)-------\n");
    terminationConditionInfoDump(tInfo);
    slim::element::get<InheritedVertex>(inheritedVerticesList->front())
        .canonicalLabel.first = targetNode->key;
  } else {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    while (!inheritedVerticesList->empty()) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      printf("-------goAhead-------\n");
      terminationConditionInfoDump(tInfo);
      auto tmpCell = std::begin(*inheritedVerticesList);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << targetNode->depth << std::endl;
      std::cout << *gen.cGraph << std::endl;
      auto key = gen.hash(&slim::element::get<InheritedVertex>(*tmpCell),
                          targetNode->depth);
      std::cout << *gen.cGraph << std::endl;
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
      slim::element::get<InheritedVertex>(*tmpCell).ownerList = nextNode->inheritedVertices;
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
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << *gen.cGraph << std::endl;
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
    std::cout << "targetCVertex:" << *targetCVertex << std::endl;
    std::cout << "targetIVertex: "<< *targetIVertex << std::endl;
    auto targetCell = targetIVertex->ownerCell;
    // targetIVertex->ownerList->erase(targetIVertex->ownerCell);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    for(auto it = targetIVertex->ownerList->begin(); it != targetIVertex->ownerList->end(); ++it) {
      std::cout << *it << std::endl;
    }
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << "targetCell:" << std::endl;
    std::cout << *targetCell << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << *targetIVertex->ownerNode->inheritedVertices << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << *targetIVertex->ownerList << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    if (targetIVertex->ownerNode->inheritedVertices->empty()) {
      std::cout << "EMPTY" << std::endl;
    } else {
      std::cout << "NOT EMPTY" << std::endl;
    }

    printf("%s:%d\n", __FUNCTION__, __LINE__);
    TrieBody *currentNode = targetIVertex->ownerNode;
    // std::cout << "--OWNER-NODE--" << std::endl;
    // std::cout << *currentNode << std::endl;
    vertex_list *tmp_delete_lst = new vertex_list();
    tmp_delete_lst->splice(std::begin(*tmp_delete_lst), *targetIVertex->ownerList, targetCell);
    //targetIVertex->ownerList->erase(targetCell);
    goBackProcess(*targetIVertex, currentNode, goAheadStack, trie->info, -1, tmp_delete_lst, true);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    delete targetIVertex;    
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    trie->dump();
    //targetIVertex->ownerList->erase(targetIVertex->ownerCell);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << *targetCell << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    trie->dump();

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
    //targetIVertex->ownerNode = trie->body;
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
  return omega_array::maxIndex(*tInfo->distribution) ==
             omega_array::index_type(depth) &&
         goAheadStack->empty();
}

Bool isRefinedTrie(TerminationConditionInfo *tInfo, int step) {
  printf("%s:%d:step=%d\n", __FUNCTION__, __LINE__, step);
  int v;
  try {
    v = tInfo->increase->at(step);
  } catch (std::out_of_range &) {
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
      omega_array::maxIndex(*tInfo->increase) ==
          omega_array::index_type(stepOfPropagation - 1)) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    pushInftyDepthTrieNodesIntoGoAheadStack(trie, goAheadStack,
                                            stepOfPropagation);
  }
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  goBackProcessOfCurrentConvertedVertices(BFSStack, goAheadStack, tInfo,
                                          stepOfPropagation);
  std::cout << "----goBack(" << stepOfPropagation << ")----"<< std::endl;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  goAheadProcessOfCurrentTrieNodes(goAheadStack, tInfo, data);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << "----goAhead(" << stepOfPropagation << ")----"<< std::endl;
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
    std::cout << *vert << std::endl;
    std::cout << *vert->hashString << std::endl;
    InheritedVertex cvert = InheritedVertex(*vert);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << list << std::endl;
    // list.insert(list.begin(), *vert);
    list.insert(std::next(beginSentinel, 1), *vert);
    std::cout << list << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::tie(prev_priority, prev_vert) = std::tie(priority, vert);
  }

  return isRefined;
}

template <typename T> bool classify(propagation_list &l, T score) {
  bool changed = false;
  for (auto i = l.begin(); i != l.end(); ++i) {
    i->sort([&](const ConvertedGraphVertex *x, const ConvertedGraphVertex *y) {
      return score(x) > score(y);
    });
    for (auto j = i->begin(); std::next(j, 1) != i->end() and i->size() != 1;
         ++j) {
      if (score(*j) > score(*std::next(j, 1))) {
        std::list<ConvertedGraphVertex *> ll;
        ll.splice(ll.end(), *i, i->begin(), std::next(j, 1));
        l.insert(i, ll);
        j = i->end();
        changed = true;
      }
    }
  }
  return changed;
}

void classifyWithAttribute(propagation_list &l, ConvertedGraph *cAfterGraph,
                           int gapOfGlobalRootMemID) {
  classify(l, [](const ConvertedGraphVertex *x) { return x->type; });
  classify(l, [](const ConvertedGraphVertex *x) { return x->links.size(); });
  classify(l, [](const ConvertedGraphVertex *x) { return strlen(x->name); });
  classify(l,
           [](const ConvertedGraphVertex *x) { return std::string(x->name); });
}

std::map<int, std::vector<int>>
putLabelsToAdjacentVertices(const propagation_list &pList) {
  std::map<int, std::vector<int>> id_to_adjacent_labels;
  int tmpLabel = 0;
  for (auto &list : pList) {   // loop of classes
    for (auto vertex : list) { // loop of vertices
      for (int link_index = 0; link_index < vertex->links.size();
           link_index++) { // loop of links
        auto tmpType = vertex->type;
        auto &tmpLink = vertex->links[link_index];
        switch (tmpLink.attr) {
        case INTEGER_ATTR:
          id_to_adjacent_labels[vertex->ID].push_back(
              tmpLink.data.integer * 256 + INTEGER_ATTR);
          break;
        case HYPER_LINK_ATTR:
          id_to_adjacent_labels[tmpLink.data.ID].push_back(tmpLabel * 256 +
                                                           link_index);
          break;
        case GLOBAL_ROOT_MEM_ATTR:
          break;
        default:
          if (tmpLink.attr < 128) {
            switch (tmpType) {
            case convertedAtom:
              id_to_adjacent_labels[tmpLink.data.ID].push_back(tmpLabel * 256 +
                                                               link_index);
              break;
            case convertedHyperLink:
              id_to_adjacent_labels[tmpLink.data.ID].push_back(tmpLabel * 256 +
                                                               HYPER_LINK_ATTR);
              break;
            default:
              throw("unexpected vertex type");
              break;
            }
          } else {
            throw("unexpected vertex type");
          }
          break;
        }
      }
    }
    tmpLabel++;
  }
  return id_to_adjacent_labels;
}

void refineConventionalPropagationListByPropagation(propagation_list &pList) {
  bool refined = false;
  do {
    auto labels = putLabelsToAdjacentVertices(pList);
    refined = classify(
        pList, [&](const ConvertedGraphVertex *v) { return labels[v->ID]; });
  } while (refined);
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
  std::cout << *cAfterGraph->cv << std::endl;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  deleteInheritedVerticesFromTrie(this, diffInfo->deletedVertices,
                                  &goAheadStack);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  addInheritedVerticesToTrie(this, diffInfo->addedVertices,
                             &initializeConvertedVerticesStack, &goAheadStack,
                             cAfterGraph, gapOfGlobalRootMemID);
  this->dump();
  auto hash_gen = hash_generator(cAfterGraph->cv, gapOfGlobalRootMemID,
                                 &fixCreditIndexStack);
  printf("%s:%d\n", __FUNCTION__, __LINE__);

  for (auto i = cAfterGraph->cv->atoms.begin();
       i != cAfterGraph->cv->atoms.end(); ++i) {
    std::cout << *i->second;
    debug_log << i->second << std::endl;
    // std::cout << *(i->second->correspondingVertexInTrie) << std::endl;
  }
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << *cAfterGraph->cv << std::endl;
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
  std::cout << *cAfterGraph->cv << std::endl;
  std::cout << *hash_gen.cGraph << std::endl;
  this->dump();
  std::cout << *hash_gen.cGraph << std::endl;
  stepOfPropagation = 0;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  goAheadProcessOfCurrentTrieNodes(&goAheadStack, tInfo, hash_gen);
  std::cout << *cAfterGraph->cv << std::endl;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  this->dump();
  std::cout << *cAfterGraph->cv << std::endl;
  while (triePropagationIsContinued(&goAheadStack, tInfo, stepOfPropagation)) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    this->dump();
    stepOfPropagation++;
    triePropagateInner(this, &BFSStack, &initializeConvertedVerticesStack,
                       &goAheadStack, tInfo, stepOfPropagation,
                       hash_generator(cAfterGraph->cv, gapOfGlobalRootMemID,
                                      &fixCreditIndexStack));
  }
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  this->dump();
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
  std::cout << *cAfterGraph->cv << std::endl;
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
  if (body.depth != -1) {
    // if (body.isPushedIntoGoAheadStack)
    //   os << "\x1b[33m";

    for (int i = 0; i < body.depth; i++)
      os << "    ";
    os << "KEY:";
    os << std::uppercase << std::setw(8) << std::setfill('0') << std::hex
       << body.key;
    os << "\n";

    for (int i = 0; i < body.depth; i++)
      os << "    ";
    os << "VERTICES:";
    os << *body.inheritedVertices << "\n";
    // for(auto i = body.inheritedVertices->begin(); i != body.inheritedVertices->end(); ++i) {
    //   if(slim::element::get<InheritedVertex>(*i).ownerList != body.inheritedVertices) {
    // 	std::cout << "owerList and inheritedVertices are NOT EQ!!" << std::endl;
    // 	std::cout << slim::element::get<InheritedVertex>(*i).ownerList << std::endl;
    // 	std::cout << body.inheritedVertices << std::endl;
    //  } else
    // 	std::cout << "owerList and inheritedVertices are EQ!!" << std::endl;
    // }
    // if (body.isPushedIntoGoAheadStack) {
    //   os << "\x1b[39m";
    // }
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
  std::ios::fmtflags flagsSaved = std::cout.flags();
  terminationConditionInfoDump(this->info);
  std::cout << *(this->body);
  std::cout.flags(flagsSaved);
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
