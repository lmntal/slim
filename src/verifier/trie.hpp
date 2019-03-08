#ifndef _TRIE_H
#define _TRIE_H

#include "collection.hpp"
#include "diff_info.hpp"
#include "element/element.h"
#include "hash.hpp"
#include "omegaArray.hpp"
// #include"convertedgraph.hpp"
#include <list>

struct ConvertedGraph;
struct InheritedVertex;
struct TrieBody;
struct TerminationConditionInfo;

typedef struct _CanonicalLabel {
  Hash first;
  int second;
} CanonicalLabel;

struct HashString {
  int creditIndex;
  std::vector<uint32_t> *body;

  HashString() {
    creditIndex = 0;
    body = new std::vector<uint32_t>();
  }

  HashString(const HashString &h) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    this->creditIndex = h.creditIndex;
    this->body = new std::vector<uint32_t>();
    for (auto v = h.body->begin(); v!=h.body->end(); ++v) {
      this->body->push_back(*v);
    }
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  }

  ~HashString() {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    delete (this->body);
  }
};

using vertex_list = std::list<
  slim::element::variant<slim::element::monostate, InheritedVertex>>;
using trie_body_map = std::map<uint32_t, TrieBody *>;
using vertex_vec = std::vector<
  slim::element::variant<slim::element::monostate, InheritedVertex>>;

struct TrieBody {
  uint32_t key;
  vertex_list *inheritedVertices;
  TrieBody *parent;
  trie_body_map *children;
  int depth;
  Bool isInfinitedDepth;
  Bool isPushedIntoGoAheadStack;

  TrieBody() {
    inheritedVertices = new vertex_list();
    parent = NULL;
    children = new trie_body_map();
    depth = -1;
    isInfinitedDepth = false;
    isPushedIntoGoAheadStack = false;
  }

  ~TrieBody() {
    delete (this->inheritedVertices);
    for (auto &v : *this->children)
      delete v.second;
    delete this->children;
  }

  void clearDescendants() {
    for (auto &v : *this->children)
      delete (v.second);
    this->children->clear();
  }

  void makeTrieMinimumInner(TerminationConditionInfo *tInfo,
                            int stepOfPropagation);

  void collectDescendantConvertedVertices(TrieBody *descendantBody);

  void makeTerminationConditionMemoInner(OmegaArray *distributionMemo,
                                         OmegaArray *increaseMemo);

  void pushInftyDepthTrieNodesIntoGoAheadStackInner(
      std::stack<TrieBody *> *goAheadStack, TerminationConditionInfo *tInfo,
      int targetDepth);
};

struct TerminationConditionInfo {
  OmegaArray *distribution;
  OmegaArray *increase;
  TerminationConditionInfo() {
    distribution = new OmegaArray();
    increase = new OmegaArray();
  };
};

struct InheritedVertex {
  ConvertedGraphVertexType type;
  char name[NAME_LENGTH];
  CanonicalLabel canonicalLabel;
  HashString *hashString;
  Bool isPushedIntoFixCreditIndex;
  int beforeID;
  TrieBody *ownerNode;
  vertex_list *ownerList;
  vertex_list::iterator ownerCell;
  std::vector<int> *conventionalPropagationMemo;
  DisjointSetForest *equivalenceClassOfIsomorphism;

  InheritedVertex(ConvertedGraphVertex *cVertex, int gapOfGlobalRootMemID) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    type = cVertex->type;
    strcpy(name, cVertex->name);
    canonicalLabel.first = 0;
    canonicalLabel.second = 0;
    hashString = new HashString();
    isPushedIntoFixCreditIndex = false;
    beforeID = cVertex->ID - gapOfGlobalRootMemID;
    cVertex->correspondingVertexInTrie = this;
    ownerNode = NULL;
    ownerList = nullptr;
    conventionalPropagationMemo = new std::vector<int>();
    equivalenceClassOfIsomorphism = new DisjointSetForest();
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  };
 
  InheritedVertex(const InheritedVertex &iVertex) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    this->type = iVertex.type;
    strcpy(this->name, iVertex.name);
    this->canonicalLabel = iVertex.canonicalLabel;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    this->hashString = new HashString(*iVertex.hashString);
    this->isPushedIntoFixCreditIndex = iVertex.isPushedIntoFixCreditIndex;
    this->beforeID = iVertex.beforeID;
    this->ownerNode = iVertex.ownerNode;
    this->ownerList = iVertex.ownerList;
    this->ownerCell = iVertex.ownerCell;
    this->conventionalPropagationMemo =
        new std::vector<int>(iVertex.conventionalPropagationMemo->begin(),
                             iVertex.conventionalPropagationMemo->end());
    this->equivalenceClassOfIsomorphism = iVertex.equivalenceClassOfIsomorphism;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  }

  ~InheritedVertex() {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    delete (hashString);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    delete (conventionalPropagationMemo);
    // freeDisjointSetForest(equivalenceClassOfIsomorphism);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  }
};

const slim::element::variant<slim::element::monostate, InheritedVertex> CLASS_SENTINEL = slim::element::monostate();

struct Trie {
  TrieBody *body;
  TerminationConditionInfo *info;
  Trie() {
    body = new TrieBody();
    info = new TerminationConditionInfo();
  };
  // HashTable *trieLeavesTable;

  void makeConventionalPropagationListInner(TrieBody *body, vertex_list &list,
                                            int stepOfPropagation) {
    if (body->children->empty()) {
      if (!list.empty()) {
        list.push_front(CLASS_SENTINEL);
      }

      for (auto &v : *body->inheritedVertices) {
	printf("%s:%d\n", __FUNCTION__, __LINE__);
        list.push_front(v);
      }
    } else {
      for (auto &v : *body->children)
        makeConventionalPropagationListInner(v.second, list, stepOfPropagation);
    }
    return;
  }

  vertex_list conventionalPropagationList(int stepOfPropagation) {
    auto ret = vertex_list();
    makeConventionalPropagationListInner(body, ret, stepOfPropagation);
    return ret;
  }

  bool propagate(DiffInfo *diffInfo, Graphinfo *cAfterGraph,
                 Graphinfo *cBeforeGraph, int gapOfGlobalRootMemID,
                 int *stepOfPropagationPtr);

  Trie *gen_tmp_trie_from_originaltrie_and_gi(Graphinfo *org_gi,
                                              Graphinfo *tmp_gi);

  void dump();
};

inline bool operator==(const InheritedVertex &iVertexA,
                       const InheritedVertex &iVertexB) {
  if (iVertexA.type < iVertexB.type) {
    return false;
  } else if (iVertexA.type > iVertexB.type) {
    return false;
  } else if (strcmp(iVertexA.name, iVertexB.name) < 0) {
    return false;
  } else if (strcmp(iVertexA.name, iVertexB.name) > 0) {
    return false;
  } else if (iVertexA.conventionalPropagationMemo->size() <
             iVertexB.conventionalPropagationMemo->size()) {
    return false;
  } else if (iVertexA.conventionalPropagationMemo->size() >
             iVertexB.conventionalPropagationMemo->size()) {
    return false;
  } else {
    int degree = iVertexA.conventionalPropagationMemo->size();
    int i;
    auto &iStackA = *iVertexA.conventionalPropagationMemo;
    auto &iStackB = *iVertexB.conventionalPropagationMemo;

    for (i = 0; i < degree; i++) {
      if (iStackA[i] < iStackB[i]) {
        return false;
      } else if (iStackA[i] > iStackB[i]) {
        return false;
      }
    }

    return true;
  }
}

inline bool operator<(const InheritedVertex &iVertexA,
                      const InheritedVertex &iVertexB) {
  if (iVertexA.type < iVertexB.type) {
    return true;
  } else if (iVertexA.type > iVertexB.type) {
    return false;
  } else if (strcmp(iVertexA.name, iVertexB.name) < 0) {
    return true;
  } else if (strcmp(iVertexA.name, iVertexB.name) > 0) {
    return false;
  } else if (iVertexA.conventionalPropagationMemo->size() <
             iVertexB.conventionalPropagationMemo->size()) {
    return true;
  } else if (iVertexA.conventionalPropagationMemo->size() >
             iVertexB.conventionalPropagationMemo->size()) {
    return false;
  } else {
    int degree = iVertexA.conventionalPropagationMemo->size();
    int i;
    auto &iStackA = iVertexA.conventionalPropagationMemo;
    auto &iStackB = iVertexB.conventionalPropagationMemo;

    for (i = 0; i < degree; i++) {
      if (iStackA[i] < iStackB[i]) {
        return true;
      } else if (iStackA[i] > iStackB[i]) {
        return false;
      }
    }

    return false;
  }
}

inline bool operator!=(const InheritedVertex &a, const InheritedVertex &b) {
  return !(a == b);
}

inline std::ostream &operator<<(std::ostream &os,
                                const InheritedVertex &iVertex) {
  os << "<";

  switch (iVertex.type) {
  case convertedAtom:
    os << "SYMBOLATOM,";
    break;
  case convertedHyperLink:
    os << " HYPERLINK,";
    break;
  default:
    fprintf(stderr, "This is unexpected vertex type\n");
    exit(EXIT_FAILURE);
    break;
  }

  os << "BEFORE_ID=" << iVertex.beforeID << ",";
  os << "NAME:\"" << iVertex.name << "\"";

  os << ">";
  return os;
}

inline std::ostream &operator<<(std::ostream &os, const HashString &h) {
  os << "<" << h.creditIndex << ": ";
  for (auto &x : *h.body) {
    os << x << ", ";
  }
  os << ">";
  return os;
}

inline std::ostream &operator<<(
    std::ostream &os,
    const slim::element::variant<slim::element::monostate, InheritedVertex>
        &v) {
  if (slim::element::holds_alternative<slim::element::monostate>(v))
    return os << "CLASS_SENTINEL\n";
  else
    return os << slim::element::get<InheritedVertex>(v);
}

template <typename T>
inline std::ostream &operator<<(std::ostream &os, const std::list<T> &list) {
  auto sentinel = std::end(list);
  os << "[";
  for (auto iterator = std::begin(list); iterator != sentinel; ++iterator) {
    os << (*iterator);
    if (std::next(iterator, 1) != sentinel) {
      os << ",";
    }
  }
  os << "]";
  return os;
}

template <typename T>
inline std::ostream &operator<<(std::ostream &os, const std::vector<T> &vec) {
  os << "[";
  for(auto it = vec.begin(); it != vec.end(); ++it) {
    os << (*it);
    if(std::next(it, 1) != vec.end()) {
      os << ",";
    }
  }
  os << "]";
  return os;
}

template <typename S>
void pushTrieBodyIntoGoAheadStackWithoutOverlap(S *stack, TrieBody *body);
void freeInheritedVertex(InheritedVertex *iVertex);
vertex_list::iterator getNextSentinel(vertex_list::iterator beginSentinel);
void putLabelsToAdjacentVertices(vertex_list *pList,
                                 ConvertedGraph *cAfterGraph,
                                 int gapOfGlobalRootMemID);
void classifyConventionalPropagationListWithAttribute(
    vertex_vec &pVec, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID);
Bool getStableRefinementOfConventionalPropagationList(
    vertex_list *pList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID);
void inheritedVertexDump(InheritedVertex *iVertex);
void terminationConditionInfoDump(TerminationConditionInfo *tInfo);
void trieDump(Trie *trie);

template <typename S>
void pushInheritedVertexIntoFixCreditIndexStackWithoutOverlap(
    S *fixCreditIndexStack, InheritedVertex *iVertex);
template <typename S>
InheritedVertex *
popInheritedVertexFromFixCreditIndexStackWithoutOverlap(S *fixCreditIndexStack);
template <typename S>
void fixCreditIndex(S *fixCreditIndexStack, ConvertedGraph *cAfterGraph,
                    int gapOfGlobalRootMemID);
ConvertedGraphVertex *
correspondingVertexInConvertedGraph(InheritedVertex *iVertex,
                                    ConvertedGraph *cAfterGraph,
                                    int gapOfGlobalRootMemID);
#endif
