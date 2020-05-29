#include "trie.hpp"
#include "runtime_status.h"
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <stack>
#include <tuple>
#include <vector>
#include <set>
slim::element::conditional_ostream debug_log(std::cout);
//#define DIFFISO_DEB
#define MUTEX_OPT
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
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << "index=" << index << std::endl;
    std::cout << "iVertex:" << *iVertex << std::endl;
    std::cout << "hashString:" << *hashString << std::endl;
    std::cout << "hashString->creditIndex:"<< hashString->creditIndex << std::endl;
#endif
    if (index < 0) {
      return 0;
    } else if (index < hashString->creditIndex) {
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
      return (hashString->body)[index];
    } else if (index == 0) {
#ifdef DIFFISO_GEN
      printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
      Hash tmp = initialHashValue(iVertex->correspondingVertex);
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
      if (hashString->body.size() > 0) {
        // auto old = hashString->body->at(index);
        //  if (old != NULL) {
        //   free(old);
        // }
      }

      hashString->body[0] = uint32_t(tmp);
      hashString->creditIndex = 1;

      fixCreditIndexStack->push(iVertex);
      iVertex->isPushedIntoFixCreditIndex = true;

      return tmp;
    } else {
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << "index=" << index << std::endl;
#endif
      Hash prevMyHash = hash(iVertex, index - 1);
#ifdef DIFFISO_DEB
      std::cout << "prevMyHash=" << prevMyHash << std::endl;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << "index=" << index << std::endl;
#endif
      ConvertedGraphVertex *cv = iVertex->correspondingVertex;
#ifdef DIFFISO_DEB
      std::cout << *cv << std::endl;
#endif
      Hash adjacentHash = hash(cv, index);
#ifdef DIFFISO_DEB
      std::cout << "adjacentHash=" << adjacentHash << std::endl;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << "index=" << index << std::endl;
#endif
      Hash newMyHash = (FNV_PRIME * prevMyHash) ^ adjacentHash;
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << "newMyHash=" << newMyHash << std::endl;
      std::cout << "hashString=" << *hashString << std::endl;
      std::cout << "index=" << index << std::endl;
      // auto old = hashString->body.at(index);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
      (hashString->body)[index] = (uint32_t(newMyHash));
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      // if (old != NULL) {
      //   free(old);
      // }
#endif
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
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << *cVertex << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
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
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
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
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
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
  if (body != NULL) {
    if (!body->isPushedIntoGoAheadStack) {
      stack->push(body);
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
void goBackProcessInnerManyCommonPrefixVertices(
    InheritedVertex &target, TrieBody *currentNode, S *goAheadStack,
    TerminationConditionInfo *tInfo, int targetDepth,
    vertex_list *tmp_delete_lst, bool del_f) {
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
  auto targetCell = target.ownerCell;
  if (targetDepth == currentNode->depth) {
    if (!del_f) {
      currentNode->inheritedVertices->splice(
          std::begin(*currentNode->inheritedVertices), *tmp_delete_lst,
          std::begin(*tmp_delete_lst));
      target.ownerList = currentNode->inheritedVertices;
      target.ownerNode = currentNode;
      target.hashString->creditIndex = currentNode->depth;
      target.ownerCell = std::begin(*currentNode->inheritedVertices);
      // slim::element::get<InheritedVertex>(*targetCell).ownerNode =
      // currentNode;
      // slim::element::get<InheritedVertex>(*targetCell).hashString->creditIndex
      // = currentNode->depth;
      pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, currentNode);
    }
  } else {
    goBackProcessInnerManyCommonPrefixVertices(target, currentNode->parent,
                                               goAheadStack, tInfo, targetDepth,
                                               tmp_delete_lst, del_f);
  }
}

template <typename S>
void goBackProcessInnerDoubleCommonPrefixVertices(
    InheritedVertex &target, InheritedVertex &brother,
    vertex_list *tmp_brother_lst, TrieBody *currentNode, TrieBody *prevNode,
    S *goAheadStack, TerminationConditionInfo *tInfo, int targetDepth,
    vertex_list *tmp_delete_lst, bool del_f) {
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::ios::fmtflags flagsSaved = std::cout.flags();
  std::cout << "CURRENTNODE-KEY: " << std::uppercase << std::setw(8)
            << std::setfill('0') << std::hex << currentNode->key << std::endl;
  std::cout.flags(flagsSaved);
#endif
  auto targetCell = target.ownerCell;
  auto brotherCell = brother.ownerCell;
#ifdef DIFFISO_DEB
  std::cout << "targetCell: " << std::endl;
  std::cout << *targetCell << std::endl;
  std::cout << "brotherCell: " << std::endl;
  std::cout << *brotherCell << std::endl;
#endif
  if (targetDepth == currentNode->depth) {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    if (!del_f) {
      currentNode->inheritedVertices->splice(
          std::begin(*currentNode->inheritedVertices), *tmp_delete_lst,
          std::begin(*tmp_delete_lst));
      target.ownerList = currentNode->inheritedVertices;
      target.ownerNode = currentNode;
      target.ownerCell = std::begin(*currentNode->inheritedVertices);
      target.hashString->creditIndex = currentNode->depth;
      // slim::element::get<InheritedVertex>(*targetCell).ownerNode =
      // currentNode;
      // slim::element::get<InheritedVertex>(*targetCell).hashString->creditIndex
      // = currentNode->depth;
    }
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    // InheritedVertex brother_cp = InheritedVertex(brother);
    // brother_cp.ownerNode =
    prevNode->inheritedVertices->splice(
        std::begin(*prevNode->inheritedVertices), *tmp_brother_lst,
        std::begin(*tmp_brother_lst));

    brother.ownerList = prevNode->inheritedVertices;
    brother.ownerCell = std::begin(*prevNode->inheritedVertices);
    brother.ownerNode = prevNode;
    brother.hashString->creditIndex = prevNode->depth;
    brother.canonicalLabel.first = prevNode->key;

    // slim::element::get<InheritedVertex>(*brotherCell).ownerNode = prevNode;

    // slim::element::get<InheritedVertex>(*brotherCell).hashString->creditIndex
    // = prevNode->depth;

    (*tInfo->distribution)[prevNode->depth]++;

    // slim::element::get<InheritedVertex>(*brotherCell).canonicalLabel.first =
    // prevNode->key;

    pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, currentNode);

  } else if (currentNode->children->size() == 1) {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    flagsSaved = std::cout.flags();
    std::cout << "CURRENTNODE-KEY: " << std::uppercase << std::setw(8)
              << std::setfill('0') << std::hex << prevNode->key << std::endl;
    std::cout.flags(flagsSaved);
#endif
    TrieBody *parent = currentNode->parent;

    prevNode->parent->children->erase(prevNode->key);
    delete (prevNode);
    goBackProcessInnerDoubleCommonPrefixVertices(
        target, brother, tmp_brother_lst, parent, currentNode, goAheadStack,
        tInfo, targetDepth, tmp_delete_lst, del_f);
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
  } else {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    TrieBody *parent = currentNode->parent;
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << "*prevNode->inheritedVertices: " << std::endl;
    std::cout << *prevNode->inheritedVertices << std::endl;
    // std::cout << "*brother.ownerList" << std::endl;
    // std::cout << *brother.ownerList << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    if (brother.correspondingVertex->correspondingVertexInTrie == nullptr)
      std::cout << "!!!!!!!!!!" << std::endl;
#ifdef DIFFISO_DEB
    std::cout << *brother.correspondingVertex->correspondingVertexInTrie
              << std::endl;
#endif
    prevNode->inheritedVertices->splice(
        std::begin(*prevNode->inheritedVertices), *tmp_brother_lst,
        std::begin(*tmp_brother_lst));
    brother.ownerNode = prevNode;
    brother.ownerList = prevNode->inheritedVertices;
    brother.hashString->creditIndex = prevNode->depth;
    brother.canonicalLabel.first = prevNode->key;
    brother.ownerCell = std::begin(*prevNode->inheritedVertices);
    // InheritedVertex brother_cp = InheritedVertex(brother);
    // std::cout << brother_cp << std::endl;
    // brother_cp.ownerNode = prevNode;
    // brother_cp.ownerList = prevNode->inheritedVertices;
    // brother_cp.hashString->creditIndex = prevNode->depth;
    // brother_cp.canonicalLabel.first = prevNode->key;
    // prevNode->inheritedVertices->push_front(brother_cp);

    // std::cout << *brother_cp.correspondingVertex->correspondingVertexInTrie
    //           << std::endl;
    // slim::element::get<InheritedVertex>(
    //     *std::begin(*prevNode->inheritedVertices))
    //     .ownerCell = std::begin(*prevNode->inheritedVertices);
    // printf("%s:%d\n", __FUNCTION__, __LINE__);
#ifdef DIFFISO_DEB
    std::cout << *slim::element::get<InheritedVertex>(
                      *std::begin(*prevNode->inheritedVertices))
                      .correspondingVertex
              << std::endl;
#endif
    if (slim::element::get<InheritedVertex>(
            *std::begin(*prevNode->inheritedVertices))
            .correspondingVertex->correspondingVertexInTrie == nullptr) {
    }
#ifdef DIFFISO_DEB
    std::cout << *slim::element::get<InheritedVertex>(
                      *std::begin(*prevNode->inheritedVertices))
                      .correspondingVertex->correspondingVertexInTrie
              << std::endl;

    // prevNode->inheritedVertices->splice(
    //     std::begin(*prevNode->inheritedVertices), *brother.ownerList,
    //     brotherCell);
    // printf("%s:%d\n", __FUNCTION__, __LINE__);
    // brother.ownerList = prevNode->inheritedVertices;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    // slim::element::get<InheritedVertex>(*brotherCell).ownerNode = prevNode;
    // slim::element::get<InheritedVertex>(*brotherCell).hashString->creditIndex
    // =
    //     prevNode->depth;
    (*tInfo->distribution)[prevNode->depth]++;
    // slim::element::get<InheritedVertex>(*brotherCell).canonicalLabel.first =
    //     prevNode->key;
    goBackProcessInnerManyCommonPrefixVertices(target, parent, goAheadStack,
                                               tInfo, targetDepth,
                                               tmp_delete_lst, del_f);
  }
}

template <typename S>
void goBackProcessInnerSingleCommonPrefixVertex(
    InheritedVertex &ivertex, TrieBody *currentNode, S *goAheadStack,
    TerminationConditionInfo *tInfo, int targetDepth,
    vertex_list *tmp_delete_lst, bool del_f) {
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::ios::fmtflags flagsSaved = std::cout.flags();
  std::cout << "CURRENTNODE-KEY: " << std::uppercase << std::setw(8)
            << std::setfill('0') << std::hex << currentNode->key << std::endl;
  std::cout.flags(flagsSaved);
#endif
  auto targetCell = ivertex.ownerCell;
#ifdef DIFFISO_DEB
  std::cout << *targetCell << std::endl;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
  if (targetDepth == currentNode->depth) {
    if (!del_f) {
      currentNode->inheritedVertices->splice(
          std::begin(*currentNode->inheritedVertices), *tmp_delete_lst,
          std::begin(*tmp_delete_lst));
      ivertex.ownerList = currentNode->inheritedVertices;
      ivertex.ownerNode = currentNode;
      ivertex.ownerCell = std::begin(*currentNode->inheritedVertices);
      ivertex.hashString->creditIndex = currentNode->depth;

      // slim::element::get<InheritedVertex>(*targetCell).ownerNode =
      // currentNode;
      // slim::element::get<InheritedVertex>(*targetCell).hashString->creditIndex
      // = currentNode->depth;
      pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, currentNode);
    }
  } else if (currentNode->children->size() == 1 &&

             currentNode->children->begin()
                     ->second->inheritedVertices->size() == 1) {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    TrieBody *childNode = (TrieBody *)currentNode->children->begin()->second;
#ifdef DIFFISO_DEB
    std::cout << *childNode->inheritedVertices << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << slim::element::get<InheritedVertex>(
                     *std::begin(*childNode->inheritedVertices))
                     .correspondingVertex->correspondingVertexInTrie
              << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    auto brother = std::begin(*childNode->inheritedVertices);
#ifdef DIFFISO_DEB
    std::cout << slim::element::get<InheritedVertex>(*brother) << std::endl;
    printf("%p\n", &slim::element::get<InheritedVertex>(*brother));
    printf("%p\n",
           slim::element::get<InheritedVertex>(*brother).correspondingVertex);
    std::cout
        << *slim::element::get<InheritedVertex>(*brother).correspondingVertex
        << std::endl;
    std::cout << slim::element::get<InheritedVertex>(*brother)
                     .correspondingVertex->correspondingVertexInTrie
              << std::endl;
#endif
    vertex_list *tmp_brother_lst = new vertex_list();
    tmp_brother_lst->splice(std::begin(*tmp_brother_lst),
                            *childNode->inheritedVertices,
                            std::begin(*childNode->inheritedVertices));
    (*tInfo->distribution)[childNode->depth]--;

    goBackProcessInnerDoubleCommonPrefixVertices(
        ivertex, slim::element::get<InheritedVertex>(*brother), tmp_brother_lst,
        currentNode, childNode, goAheadStack, tInfo, targetDepth,
        tmp_delete_lst, del_f);
    delete tmp_brother_lst;
  } else {
    TrieBody *parent = currentNode->parent;

    goBackProcessInnerManyCommonPrefixVertices(ivertex, parent, goAheadStack,
                                               tInfo, targetDepth,
                                               tmp_delete_lst, del_f);
  }

  return;
}

// trie is minimal for uniqueness!!
template <typename S>
void goBackProcess(InheritedVertex &ivertex, TrieBody *currentNode,
                   S *goAheadStack, TerminationConditionInfo *tInfo,
                   int targetDepth, vertex_list *tmp_delete_lst, bool del_f) {
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::ios::fmtflags flagsSaved = std::cout.flags();
  std::cout << "CURRENTNODE-KEY: " << std::uppercase << std::setw(8)
            << std::setfill('0') << std::hex << currentNode->key << std::endl;
  std::cout.flags(flagsSaved);

  std::cout << "currentNode->depth:" << currentNode->depth << std::endl;
#endif
  if (targetDepth < currentNode->depth) {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    if (currentNode->inheritedVertices->empty()) {
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
      TrieBody *parent = currentNode->parent;

      (*tInfo->distribution)[currentNode->depth]--;
      if (parent->depth >= 0 && parent->children->size() != 1) {
        (*tInfo->increase)[parent->depth]--;
      }
      currentNode->parent->children->erase(currentNode->key);
      delete (currentNode);
      goBackProcessInnerSingleCommonPrefixVertex(ivertex, parent, goAheadStack,
                                                 tInfo, targetDepth,
                                                 tmp_delete_lst, del_f);
    } else if (currentNode->inheritedVertices->size() == 1) {
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
      auto brother = std::begin(*currentNode->inheritedVertices);
      TrieBody *parent = currentNode->parent;
      (*tInfo->distribution)[omega_array::OMEGA]--;
      (*tInfo->distribution)[omega_array::OMEGA]--;
      vertex_list *tmp_brother_lst = new vertex_list();
      tmp_brother_lst->splice(std::begin(*tmp_brother_lst),
                              *currentNode->inheritedVertices,
                              std::begin(*currentNode->inheritedVertices));
      goBackProcessInnerDoubleCommonPrefixVertices(
          ivertex, slim::element::get<InheritedVertex>(*brother),
          tmp_brother_lst, parent, currentNode, goAheadStack, tInfo,
          targetDepth, tmp_delete_lst, del_f);
      delete tmp_brother_lst;
    } else {
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
      TrieBody *parent = currentNode->parent;

      (*tInfo->distribution)[omega_array::OMEGA]--;
      goBackProcessInnerManyCommonPrefixVertices(ivertex, parent, goAheadStack,
                                                 tInfo, targetDepth,
                                                 tmp_delete_lst, del_f);
    }
  } else {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    if (!del_f) {
      currentNode->inheritedVertices->splice(
          std::begin(*currentNode->inheritedVertices), *tmp_delete_lst,
          std::begin(*tmp_delete_lst));
      ivertex.ownerList = currentNode->inheritedVertices;
      ivertex.ownerNode = currentNode;
      ivertex.ownerCell = std::begin(*currentNode->inheritedVertices);
      ivertex.hashString->creditIndex = currentNode->depth;
    }
  }
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
}

template <typename S1>
void goBackProcessOfCurrentConvertedVertices(
    S1 *BFSStack, std::stack<TrieBody *> *goAheadStack,
    TerminationConditionInfo *tInfo, int targetDepth) {
  int i;

  for (i = 0; i < BFSStack->size(); i++) {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    ConvertedGraphVertex *cVertex = BFSStack->at(i);
#ifdef DIFFISO_DEB
    std::cout << *cVertex << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    InheritedVertex *iVertex = cVertex->correspondingVertexInTrie;
#ifdef DIFFISO_DEB
    std::cout << *iVertex << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    TrieBody *currentNode = iVertex->ownerNode;
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    auto targetCell = iVertex->ownerCell;
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    vertex_list *tmp_delete_lst = new vertex_list();

    tmp_delete_lst->splice(std::begin(*tmp_delete_lst), *iVertex->ownerList,
                           targetCell);
    goBackProcess(*iVertex, currentNode, goAheadStack, tInfo, targetDepth,
                  tmp_delete_lst, false);
    delete tmp_delete_lst;
  }

  return;
}

void goAheadProcess(TrieBody *targetNode, std::stack<TrieBody *> *goAheadStack,
                    TerminationConditionInfo *tInfo, hash_generator gen) {
  auto inheritedVerticesList = targetNode->inheritedVertices;
  auto children = targetNode->children;
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << "--inheritedVerticesList--" << std::endl;
  std::cout << *inheritedVerticesList << std::endl;
#endif
  if (inheritedVerticesList->size() == 1 && children->empty() &&
      targetNode->depth != -1) {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    printf("-------goAhead(SINGLETONE)-------\n");
    std::cout << "DEPTH:" << targetNode->depth << std::endl;
    terminationConditionInfoDump(tInfo);
#endif
    (*tInfo->distribution)[targetNode->depth]++;
#ifdef DIFFISO_DEB
    printf("-------goAhead(SINGLETONE)-------\n");
    terminationConditionInfoDump(tInfo);
#endif
    slim::element::get<InheritedVertex>(inheritedVerticesList->front())
        .canonicalLabel.first = targetNode->key;
  } else {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    while (!inheritedVerticesList->empty()) {
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      printf("-------goAhead-------\n");
      terminationConditionInfoDump(tInfo);
#endif
      auto tmpCell = std::begin(*inheritedVerticesList);
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << targetNode->depth << std::endl;
      std::cout << *gen.cGraph << std::endl;
#endif
      auto key = gen.hash(&slim::element::get<InheritedVertex>(*tmpCell),
                          targetNode->depth);
#ifdef DIFFISO_DEB
      std::cout << *gen.cGraph << std::endl;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << key << std::endl;
#endif
      auto it = children->find(key);
      TrieBody *nextNode;
      if (it == std::end(*children)) {
        if (!children->empty()) {
          (*tInfo->increase)[targetNode->depth]++;
        }
        nextNode = new TrieBody();
        children->insert(std::make_pair(key, nextNode));
        nextNode->key = key;
        nextNode->parent = targetNode;
        nextNode->depth = targetNode->depth + 1;
      } else {
        nextNode = it->second;
      }
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
      nextNode->inheritedVertices->splice(
          std::begin(*nextNode->inheritedVertices), *inheritedVerticesList,
          tmpCell);
      slim::element::get<InheritedVertex>(*tmpCell).ownerList =
          nextNode->inheritedVertices;
      slim::element::get<InheritedVertex>(*tmpCell).ownerNode = nextNode;
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << "NEXTNODE:" << std::endl;
      std::cout << *nextNode->inheritedVertices << std::endl;
#endif
      pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, nextNode);
    }
  }
}

void goAheadProcessOfCurrentTrieNodes(std::stack<TrieBody *> *goAheadStack,
                                      TerminationConditionInfo *tInfo,
                                      hash_generator gen) {
  auto nextGoAheadStack = std::stack<TrieBody *>();
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << *gen.cGraph << std::endl;
#endif
  while (!goAheadStack->empty()) {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    TrieBody *targetNode =
        popTrieBodyFromGoAheadStackWithoutOverlap(goAheadStack);
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << "targetNode:" << std::endl;
    std::cout << *targetNode->inheritedVertices << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    goAheadProcess(targetNode, &nextGoAheadStack, tInfo, gen);
  }
#ifdef DIFFISO_DEB
  std::cout << "nextGoAheadStack.size() = " << nextGoAheadStack.size()
            << std::endl;
#endif
  nextGoAheadStack.swap(*goAheadStack);
}

template <typename S1, typename S2>
void deleteInheritedVerticesFromTrie(Trie *trie, S1 *deletedVertices,
                                     S2 *goAheadStack) {
  auto rit = deletedVertices->rbegin();
  while (rit != deletedVertices->rend()) {
    auto targetCVertex = *rit;
    targetCVertex->isPushedIntoDiffInfoStack = false;
    rit++;
    // popConvertedVertexFromDiffInfoStackWithoutOverlap(deletedVertices);
    // std::cout << *targetCVertex;

    InheritedVertex *targetIVertex = targetCVertex->correspondingVertexInTrie;
#ifdef DIFFISO_DEB

    std::cout << "targetCVertex:" << *targetCVertex << std::endl;
    printf("targetIVertex pointer: %p\n", targetIVertex);

    if (targetIVertex == nullptr)
      std::cout << "targetIVertex is NULL!!" << std::endl;

    std::cout << "targetIVertex: " << *targetIVertex << std::endl;
#endif
    auto targetCell = targetIVertex->ownerCell;
    // targetIVertex->ownerList->erase(targetIVertex->ownerCell);
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    printf("targetIVertex->ownerList: %p\n", targetIVertex->ownerList);
    for (auto it = targetIVertex->ownerList->begin();
         it != targetIVertex->ownerList->end(); ++it) {
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
#endif
    TrieBody *currentNode = targetIVertex->ownerNode;
    // std::cout << "--OWNER-NODE--" << std::endl;
    // std::cout << *currentNode << std::endl;
    vertex_list *tmp_delete_lst = new vertex_list();
    tmp_delete_lst->splice(std::begin(*tmp_delete_lst),
                           *targetIVertex->ownerList, targetCell);
    // targetIVertex->ownerList->erase(targetCell);

    goBackProcess(*targetIVertex, currentNode, goAheadStack, trie->info, -1,
                  tmp_delete_lst, true);
    // delete targetIVertex;
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    trie->dump();
#endif
    // targetIVertex->ownerList->erase(targetIVertex->ownerCell);

    // std::cout << *targetCell << std::endl;
    delete tmp_delete_lst;
  }
}

void addInheritedVerticesToTrie(
    Trie *trie, std::vector<ConvertedGraphVertex *> *addedVertices,
    std::vector<ConvertedGraphVertex *> *initializeConvertedVerticesStack,
    std::stack<TrieBody *> *goAheadStack, Graphinfo *cAfterGraph,
    int gapOfGlobalRootMemID) {
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  trie->dump();
#endif
  if (!addedVertices->empty()) {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    pushTrieBodyIntoGoAheadStackWithoutOverlap(goAheadStack, trie->body);
  }
  auto rit = addedVertices->rbegin();
  while (rit != addedVertices->rend()) {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    ConvertedGraphVertex *targetCVertex = *rit;
    targetCVertex->isPushedIntoDiffInfoStack = false;
    rit++;
    // popConvertedVertexFromDiffInfoStackWithoutOverlap(addedVertices);
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << *targetCVertex << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    trie->dump();
#endif
    trie->body->inheritedVertices->push_front(
        InheritedVertex(targetCVertex, gapOfGlobalRootMemID));
#ifdef DIFFISO_DEB
    std::cout << *(trie->body->inheritedVertices) << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    trie->dump();
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    InheritedVertex *targetIVertex = &slim::element::get<InheritedVertex>(
        trie->body->inheritedVertices->front());
#ifdef DIFFISO_DEB
    std::cout << *targetIVertex << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << *targetCVertex << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    targetCVertex->correspondingVertexInTrie = targetIVertex;
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    // targetIVertex->ownerNode = trie->body;
#endif
    targetIVertex->ownerList = trie->body->inheritedVertices;
    targetIVertex->ownerCell = std::begin(*trie->body->inheritedVertices);
    targetCVertex->isVisitedInBFS = TRUE;
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    for (auto &v : *initializeConvertedVerticesStack)
      std::cout << v << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    initializeConvertedVerticesStack->push_back(targetCVertex);
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    trie->dump();
#endif
  }
#ifdef DIFFISO_DEB
  std::cout << *(trie->body->inheritedVertices) << std::endl;
#endif
  return;
}

template <typename S1, typename S2, typename S3>
void moveInheritedRelinkedVerticesToBFSStack(
    S1 *relinkedVertices, S2 *initializeConvertedVerticesStack, S3 *BFSStack,
    ConvertedGraph *cv, std::map<int, int> &id_map) {
  auto rit = relinkedVertices->rbegin();
  while (rit != relinkedVertices->rend()) {
    ConvertedGraphVertex *cVertex = *rit;
    rit++;
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    std::cout << *cVertex << std::endl;
    std::cout << cVertex->ID << std::endl;
#endif
    // printf("%s:%d\n", __FUNCTION__, __LINE__);
    // for (auto &v : id_map) {
    //   std::cout << v.first << "-->" << v.second << std::endl;
    // }
    // auto it = cv->atoms.find(id_map[cVertex->ID]);
    // if (it == cv->atoms.end()) {
    //   printf("%s:%d\n", __FUNCTION__, __LINE__);
    //   std::cout << "FIND BUG!!!!!!!!!!!" << std::endl;
    //   exit(0);
    // }
    // auto cAfterVertex = it->second;
    // printf("%s:%d\n", __FUNCTION__, __LINE__);

    // std::cout << *cAfterVertex << std::endl;
    cVertex->isPushedIntoDiffInfoStack = false;
    // popConvertedVertexFromDiffInfoStackWithoutOverlap(relinkedVertices);
    BFSStack->push_back(cVertex);
    cVertex->isVisitedInBFS = TRUE;
    // cAfterVertex->isPushedIntoDiffInfoStack = false;
    // // popConvertedVertexFromDiffInfoStackWithoutOverlap(relinkedVertices);
    // BFSStack->push_back(cAfterVertex);
    // cAfterVertex->isVisitedInBFS = TRUE;
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
  Bool r = isRefinedTrie(tInfo, step);
  Bool d = isDescreteTrie(goAheadStack, tInfo, step);
#ifdef DIFFISO_DEB
  std::cout << "ISREFINEDTRIE: " << r << std::endl;
  std::cout << "ISDESCRETETRIE: " << d << std::endl;
#endif
  return r && !d;
  // if(r)
  //   return r;

  // if(!d)
  //   return !d;
  // else
  //   return d;

  // return isRefinedTrie(tInfo, step) &&
  //        !isDescreteTrie(goAheadStack, tInfo, step);
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
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    pushInftyDepthTrieNodesIntoGoAheadStack(trie, goAheadStack,
                                            stepOfPropagation);
  }
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << "goAheadStack->size() = " << goAheadStack->size() << std::endl;
#endif
  goBackProcessOfCurrentConvertedVertices(BFSStack, goAheadStack, tInfo,
                                          stepOfPropagation);
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << "goAheadStack->size() = " << goAheadStack->size() << std::endl;

  std::cout << "----goBack(" << stepOfPropagation << ")----" << std::endl;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
  goAheadProcessOfCurrentTrieNodes(goAheadStack, tInfo, data);
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << "----goAhead(" << stepOfPropagation << ")----" << std::endl;
#endif
  getNextDistanceConvertedVertices(BFSStack, initializeConvertedVerticesStack,
                                   data.cGraph);
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
      slim::element::get<InheritedVertex>(*targetCell).ownerList =
          this->inheritedVertices;
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
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  trie->dump();
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << "stepOfPropagation = " << stepOfPropagation << std::endl;
#endif
  omega_array::move_to_omega_larger_than(*tInfo->distribution,
                                         stepOfPropagation);
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  trie->dump();
#endif
  omega_array::clear_finite_larger_than(*tInfo->increase, stepOfPropagation);
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  trie->dump();
#endif
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
        ll.splice(ll.end(), *i, std::next(j, 1), std::next(std::next(j,1)));
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

std::map<int, std::map<int, int>>
putLabelsToAdjacentVertices(const propagation_list &pList) {
  std::map<int, std::map<int, int>> id_to_adjacent_labels;
  int tmpLabel = 0;
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
  for (auto &list : pList) {   // loop of classes
    for (auto vertex : list) { // loop of vertices
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << *vertex << std::endl;
#endif
      for (int link_index = 0; link_index < vertex->links.size();
           link_index++) { // loop of links
#ifdef DIFFISO_DEB
        printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
        auto tmpType = vertex->type;
        auto &tmpLink = vertex->links[link_index];
        switch (tmpLink.attr) {
        case INTEGER_ATTR:
          id_to_adjacent_labels[vertex->ID][link_index] = 
              tmpLink.data.integer * 256 + INTEGER_ATTR;
          break;
        case HYPER_LINK_ATTR:
          id_to_adjacent_labels[tmpLink.data.ID][link_index] = tmpLabel * 256 + link_index;
          break;
        case GLOBAL_ROOT_MEM_ATTR:
          break;
        default:
          if (tmpLink.attr < 128) {
#ifdef DIFFISO_DEB
            printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
            switch (tmpType) {
            case convertedAtom: {
#ifdef DIFFISO_DEB
              printf("%s:%d\n", __FUNCTION__, __LINE__);
              std::cout << tmpLink.data.ID << std::endl;
              printf("%s:%d\n", __FUNCTION__, __LINE__);
              std::cout << tmpLabel << std::endl;
              std::cout << link_index << std::endl;
              std::cout << tmpLabel * 256 + link_index << std::endl;
#endif
              id_to_adjacent_labels[tmpLink.data.ID][tmpLink.attr]= tmpLabel * 256 + link_index;
              break;
            }
            case convertedHyperLink:
              id_to_adjacent_labels[tmpLink.data.ID][tmpLink.attr] = tmpLabel * 256 + HYPER_LINK_ATTR;
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
#ifdef MUTEX_OPT
  bool ff = true;
  for (auto cell = pList.begin(); cell != pList.end() and ff; cell++) {
    for(auto v = cell->begin(); v != cell->end(); v++) {
      if((*v)->links.size() == 2) { // degree is one
	auto tmpType = (*v)->type;
	auto &tmpLink = (*v)->links[0];
	if(tmpLink.attr < 128 and tmpType == convertedAtom) {
	  if (cell->size() == 1) { // cell is a singleton
	    auto f = true;
	    for (auto c = pList.begin(); c != pList.end() and f; c++) {
	      for(auto x = c->begin(); x != c->end(); x++) {
		if((*x)->ID == tmpLink.data.ID and c->size() != 1) {
		  std::list<ConvertedGraphVertex *> new_cell;
		  new_cell.splice(new_cell.end(), *c, x);
		  pList.insert(c, new_cell);
		  f = false;
		  ff = false;
		  break;
		}
	      }
	    }
	    break;
	  }
	}
      }
    }
  }
  return;

#endif


  do {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_start_timer(PROFILE_TIME__PUTLABELS);
    }
#endif
    auto labels_map = putLabelsToAdjacentVertices(pList);
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_finish_timer(PROFILE_TIME__PUTLABELS);
    }
#endif
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_start_timer(PROFILE_TIME__MAKELABEL);
    }
#endif
    std::map<int, std::vector<int>> labels;
    for (auto &v : labels_map) {
      for (auto &e : v.second) {
	labels[v.first].push_back(e.second);
      }
    }
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_finish_timer(PROFILE_TIME__MAKELABEL);
    }
#endif
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    for(auto &v : labels) {
      std::cout << v.first << ":" << v.second << std::endl;
    }
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif

#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_start_timer(PROFILE_TIME__CLASSIFY);
    }
#endif
    refined = classify(
        pList, [&](const ConvertedGraphVertex *v) { return labels[v->ID]; });
#ifdef PROFILE
    if (lmn_env.profile_level >= 3) {
      profile_finish_timer(PROFILE_TIME__CLASSIFY);
    }
#endif

#ifdef DIFFISO_DEB
    std::cout << pList << std::endl;
#endif
  } while (refined);
}

void assureReferenceFromConvertedVerticesToInheritedVertices(
    ConvertedGraph *cAfterGraph, ConvertedGraph *cBeforeGraph,
    int gapOfGlobalRootMemID, std::map<int, int> &id_map) {
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  for (auto &v : id_map) {
    std::cout << v.first << "-->" << v.second << std::endl;
  }
#endif
  for (auto &v : cBeforeGraph->atoms) {
    auto cBeforeVertex = v.second;
#ifdef DIFFISO_DEB
    std::cout << v.first << std::endl;
    std::cout << id_map[v.first] << std::endl;
#endif
    // auto &cAfterVertex = cAfterGraph->at(id_map[v.first], convertedAtom);
    auto it = cAfterGraph->atoms.find(id_map[v.first]);
    if (it == cAfterGraph->atoms.end())
      continue;
    auto cAfterVertex = it->second;
#ifdef DIFFISO_DEB
    std::cout << *cAfterVertex << std::endl;
    std::cout << *cBeforeVertex << std::endl;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
    if (cAfterVertex->correspondingVertexInTrie == nullptr) {
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << *cAfterVertex << std::endl;
#endif
      if (cBeforeVertex->correspondingVertexInTrie == nullptr) {
#ifdef DIFFISO_DEB
        std::cout << "!!!!!!!!!!" << std::endl;
        printf("cBeforeVertex pointer:%p\n", cBeforeVertex);
        std::cout << "cBeforeVertex pointer:" << cBeforeVertex << std::endl;
#endif
      }
#ifdef DIFFISO_DEB
      std::cout << *cBeforeVertex->correspondingVertexInTrie << std::endl;
      printf("cBeforeVertex->correspondingVertexInTrie pointer:%p\n",
             cBeforeVertex->correspondingVertexInTrie);
#endif
      cAfterVertex->correspondingVertexInTrie =
          cBeforeVertex->correspondingVertexInTrie;
      cAfterVertex->correspondingVertexInTrie->correspondingVertex =
          cAfterVertex;
      cBeforeVertex->correspondingVertexInTrie = nullptr;
      cAfterVertex->correspondingVertexInTrie->beforeID = cBeforeVertex->ID;
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      printf("cAfterV: %p\n", cAfterVertex);
      printf("cAfterV->IV: %p\n", cAfterVertex->correspondingVertexInTrie);
      printf("cAfterV->IV->CV: %p\n",
             cAfterVertex->correspondingVertexInTrie->correspondingVertex);
#endif
    }
  }
  // for (auto &v : cAfterGraph->atoms) {
  //   auto cAfterVertex = v.second;
  //   printf("%s:%d\n", __FUNCTION__, __LINE__);
  //   if (cAfterVertex != NULL) {
  //     if (cAfterVertex->correspondingVertexInTrie == NULL) {
  //       std::cout << *cAfterVertex << std::endl;
  //       printf("%s:%d\n", __FUNCTION__, __LINE__);
  //       ConvertedGraphVertex *cBeforeVertex = cBeforeGraph->at(
  //           cAfterVertex->ID - gapOfGlobalRootMemID, cAfterVertex->type);
  //       std::cout << *cBeforeVertex << std::endl;
  //       printf("%s:%d\n", __FUNCTION__, __LINE__);
  //       cAfterVertex->correspondingVertexInTrie =
  //           cBeforeVertex->correspondingVertexInTrie;
  //       cAfterVertex->correspondingVertexInTrie->beforeID =
  //       cBeforeVertex->ID; cBeforeVertex->correspondingVertexInTrie =
  //       nullptr;
  //     }
  //   }
  // }

  // for (auto &v : cAfterGraph->hyperlinks) {
  //   auto cAfterVertex = v.second;
  //   if (cAfterVertex != NULL) {
  //     if (cAfterVertex->correspondingVertexInTrie == NULL) {
  //       auto cBeforeVertex = cBeforeGraph->at(
  //           cAfterVertex->ID - gapOfGlobalRootMemID, cAfterVertex->type);
  //       cAfterVertex->correspondingVertexInTrie =
  //           cBeforeVertex->correspondingVertexInTrie;
  //       cAfterVertex->correspondingVertexInTrie->beforeID =
  //       cBeforeVertex->ID;
  //     }
  //   }
  // }

  return;
}

bool Trie::propagate(DiffInfo *diffInfo, Graphinfo *cAfterGraph,
                     Graphinfo *cBeforeGraph, int gapOfGlobalRootMemID,
                     int *stepOfPropagationPtr, std::map<int, int> &id_map) {
  std::stack<TrieBody *> goAheadStack;
  std::vector<ConvertedGraphVertex *> BFSStack;
  std::vector<ConvertedGraphVertex *> initializeConvertedVerticesStack;
  std::stack<InheritedVertex *> fixCreditIndexStack;
  TerminationConditionInfo *tInfo = this->info;
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << *cAfterGraph->cv << std::endl;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  for (auto &v : cBeforeGraph->cv->atoms) {
    std::cout << *v.second << std::endl;
    printf("%p\n", v.second);
    std::cout << "--->" << std::endl;
    std::cout << *v.second->correspondingVertexInTrie << std::endl;
    printf("%p\n", v.second->correspondingVertexInTrie);
    printf("%p\n", v.second->correspondingVertexInTrie->correspondingVertex);
    std::cout << *v.second->correspondingVertexInTrie->correspondingVertex
              << std::endl;
  }
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << "--Before Graph--" << std::endl;
  std::cout << *cBeforeGraph->cv << std::endl;
  std::cout << "----------------" << std::endl;
  std::cout << "--After Graph--" << std::endl;
  std::cout << *cAfterGraph->cv << std::endl;
  std::cout << "----------------" << std::endl;
#endif
  deleteInheritedVerticesFromTrie(this, diffInfo->deletedVertices,
                                  &goAheadStack);
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << "--Before Graph--" << std::endl;
  std::cout << *cBeforeGraph->cv << std::endl;
  std::cout << "----------------" << std::endl;
  std::cout << "--After Graph--" << std::endl;
  std::cout << *cAfterGraph->cv << std::endl;
  std::cout << "----------------" << std::endl;
#endif
  addInheritedVerticesToTrie(this, diffInfo->addedVertices,
                             &initializeConvertedVerticesStack, &goAheadStack,
                             cAfterGraph, gapOfGlobalRootMemID);
#ifdef DIFFISO_DEB
  this->dump();
#endif
  auto hash_gen = hash_generator(cAfterGraph->cv, gapOfGlobalRootMemID,
                                 &fixCreditIndexStack);
#ifdef DIFFISO_DEB
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
#endif
  assureReferenceFromConvertedVerticesToInheritedVertices(
      cAfterGraph->cv, cBeforeGraph->cv, gapOfGlobalRootMemID, id_map);
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  cg_trie_reference_check(cAfterGraph->cv);
  printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
  moveInheritedRelinkedVerticesToBFSStack(diffInfo->relinkedVertices,
                                          &initializeConvertedVerticesStack,
                                          &BFSStack, cAfterGraph->cv, id_map);
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
  int stepOfPropagation = -1;
  goAheadProcessOfCurrentTrieNodes(&goAheadStack, tInfo, hash_gen);
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << *cAfterGraph->cv << std::endl;
  std::cout << *hash_gen.cGraph << std::endl;
  this->dump();
  std::cout << *hash_gen.cGraph << std::endl;
#endif
  stepOfPropagation = 0;
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << "goAheadStack.size() = " << goAheadStack.size() << std::endl;
#endif
  goAheadProcessOfCurrentTrieNodes(&goAheadStack, tInfo, hash_gen);
#ifdef DIFFISO_DEB
  std::cout << *cAfterGraph->cv << std::endl;
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  std::cout << "goAheadStack.size() = " << goAheadStack.size() << std::endl;
  this->dump();
  std::cout << *cAfterGraph->cv << std::endl;
#endif
  while (triePropagationIsContinued(&goAheadStack, tInfo, stepOfPropagation)) {
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    this->dump();
#endif

    stepOfPropagation++;
    triePropagateInner(this, &BFSStack, &initializeConvertedVerticesStack,
                       &goAheadStack, tInfo, stepOfPropagation,
                       hash_generator(cAfterGraph->cv, gapOfGlobalRootMemID,
                                      &fixCreditIndexStack));
  }
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  this->dump();
#endif
  Bool verticesAreCompletelySorted =
      isDescreteTrie(&goAheadStack, tInfo, stepOfPropagation) ||
      isEmptyTrie(this);
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
  if (!verticesAreCompletelySorted) {
    makeTrieMinimum(this, stepOfPropagation);
    while (!goAheadStack.empty()) {
      popTrieBodyFromGoAheadStackWithoutOverlap(&goAheadStack);
    }
  }
  initializeConvertedVertices(&initializeConvertedVerticesStack);
  initializeConvertedVertices(&BFSStack);
  fixCreditIndex(&fixCreditIndexStack);
  //実際のSLIMでは起きない操作
  cBeforeGraph->cv->clearReferencesFromConvertedVerticesToInheritedVertices();
  *stepOfPropagationPtr = stepOfPropagation;
#ifdef DIFFISO_DEB
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  this->dump();
  std::cout << *cAfterGraph->cv << std::endl;
  cg_trie_reference_check(cAfterGraph->cv);
#endif
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
    std::ios::fmtflags flagsSaved = std::cout.flags();
    os << std::uppercase << std::setw(8) << std::setfill('0') << std::hex
       << body.key;
    std::cout.flags(flagsSaved);
    os << "\n";

    for (int i = 0; i < body.depth; i++)
      os << "    ";
    os << "VERTICES:";
    os << *body.inheritedVertices << "\n";
    // for(auto i = body.inheritedVertices->begin(); i !=
    // body.inheritedVertices->end(); ++i) {
    //   if(slim::element::get<InheritedVertex>(*i).ownerList !=
    //   body.inheritedVertices) {
    // 	std::cout << "owerList and inheritedVertices are NOT EQ!!" << std::endl;
    // 	std::cout << slim::element::get<InheritedVertex>(*i).ownerList <<
    // std::endl; 	std::cout << body.inheritedVertices << std::endl;
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
