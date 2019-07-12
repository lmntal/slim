#ifndef _TRIE_H
#define _TRIE_H

#include "collection.hpp"
#include "diff_info.hpp"
#include "element/element.h"
#include "hash.hpp"
#include "omegaArray.hpp"
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
  std::vector<uint32_t> body;

  HashString() {
    creditIndex = 0;
  }

  HashString(const HashString &h) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    this->creditIndex = h.creditIndex;
    for (auto v = h.body.begin(); v != h.body.end(); ++v) {
      this->body.push_back(*v);
    }
  }

  ~HashString() {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  }
};

using vertex_list = std::list<
    slim::element::variant<slim::element::monostate, InheritedVertex>>;
using trie_body_map = std::map<uint32_t, TrieBody *>;
using vertex_vec = std::vector<
    slim::element::variant<slim::element::monostate, InheritedVertex>>;
using propagation_list = std::list<std::list<ConvertedGraphVertex *>>;
std::map<int, std::vector<int>>
putLabelsToAdjacentVertices(const propagation_list &pList);

inline bool
converted_graph_vertex_cmp(const ConvertedGraphVertex *lhs,
                           const ConvertedGraphVertex *rhs,
                           const std::map<int, std::vector<int>> &l_m,
                           const std::map<int, std::vector<int>> &r_m) {
  if (lhs->type < rhs->type) {
    return true;
  } else if (lhs->type > rhs->type) {
    return false;
  } else if (strcmp(lhs->name, rhs->name) < 0) {
    return true;
  } else if (strcmp(lhs->name, rhs->name) > 0) {
    return false;
  } else {
    auto l_key = l_m.find(lhs->ID);
    auto r_key = r_m.find(rhs->ID);
    if (l_key == l_m.end() and r_key == r_m.end()) {
      return false;
    } else if (l_key == l_m.end() and r_key != r_m.end()) {
      return true;
    } else if (l_key != l_m.end() and r_key == r_m.end()) {
      return false;
    } else {
      auto l_it = l_m.at(lhs->ID).begin();
      auto r_it = r_m.at(rhs->ID).begin();
      for (; l_it != l_m.at(lhs->ID).end() and r_it != r_m.at(rhs->ID).end();
           l_it++, r_it++) {
        if (*l_it < *r_it) {
          return true;
        } else if (*l_it > *r_it) {
          return false;
        }
      }
      return false;
    }
  }
}
struct PropagationListCmp {
  bool operator()(const propagation_list &lhs,
                  const propagation_list &rhs) const {
    auto l_m = putLabelsToAdjacentVertices(lhs);
    auto r_m = putLabelsToAdjacentVertices(rhs);
    auto it_l = lhs.begin();
    auto it_r = rhs.begin();
    for (; it_l != lhs.end() and it_r != rhs.end(); it_l++, it_r++) {
      if (it_l->size() != 1 or it_r->size() != 1)
        throw("propagation list is'nt discrete");
      if (converted_graph_vertex_cmp(it_l->front(), it_r->front(), l_m, r_m))
        return true;
      else if (converted_graph_vertex_cmp(it_r->front(), it_l->front(), l_m,
                                          r_m))
        return false;
    }
    if (it_l == lhs.end() and it_r != rhs.end())
      return true;
    else
      return false;
    return true;
  }
};

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
  ConvertedGraphVertex *correspondingVertex;

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
    correspondingVertex = cVertex;
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
    this->correspondingVertex = iVertex.correspondingVertex;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  }

  ~InheritedVertex() {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    delete (hashString);
    delete (conventionalPropagationMemo);
    this->correspondingVertex->correspondingVertexInTrie = nullptr;
    // freeDisjointSetForest(equivalenceClassOfIsomorphism);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  }
};

const slim::element::variant<slim::element::monostate, InheritedVertex>
    CLASS_SENTINEL = slim::element::monostate();

struct Trie {
  TrieBody *body;
  TerminationConditionInfo *info;
  Trie() {
    body = new TrieBody();
    info = new TerminationConditionInfo();
  };
  // HashTable *trieLeavesTable;

  void conventionalPropagationList(TrieBody *body, propagation_list &list) {
    if (body->children->empty()) {
      std::list<ConvertedGraphVertex *> l;
      for (auto &v : *body->inheritedVertices) {
        l.push_back(slim::element::get<InheritedVertex>(v).correspondingVertex);
      }
      list.push_back(l);
    } else {
      for (auto &v : *body->children)
        conventionalPropagationList(v.second, list);
    }
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

inline std::ostream &operator<<(std::ostream &os,
                                const ConvertedGraphVertex *c) {
  os << "<";
  switch (c->type) {
  case convertedAtom:
    os << "SYMBOLATOM,";
    break;
  case convertedHyperLink:
    os << "HYPERLINK,";
    break;
  default:
    throw("This is unexpected vertex type");
    break;
  }
  os << "ID=" << c->ID;
  os << "NAME:\"" << c->name << "\"";
  os << ">";
  return os;
}

inline std::ostream &operator<<(std::ostream &os, const HashString &h) {
  os << "<" << h.creditIndex << ": ";
  for (auto &x : h.body) {
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
  for (auto it = vec.begin(); it != vec.end(); ++it) {
    os << (*it);
    if (std::next(it, 1) != vec.end()) {
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
void classifyWithAttribute(propagation_list &l, ConvertedGraph *cAfterGraph,
                           int gapOfGlobalRootMemID);
void refineConventionalPropagationListByPropagation(propagation_list &pList);
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
correspondingVertexInConvertedGraph(const InheritedVertex *iVertex,
                                    ConvertedGraph *cAfterGraph,
                                    int gapOfGlobalRootMemID);
#endif
