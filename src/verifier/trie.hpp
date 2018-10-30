#ifndef _TRIE_H
#define _TRIE_H

#include "collection.hpp"
#include "diff_info.hpp"
#include "hash.hpp"
#include "omegaArray.hpp"
// #include"convertedgraph.hpp"
#include <list>

struct ConvertedGraph;
struct InheritedVertex;
struct TrieBody;

constexpr auto CLASS_SENTINEL = slim::element::monostate();

using vertex_list = std::list<slim::element::variant<slim::element::monostate, InheritedVertex>>;
using trie_body_map = RedBlackTree__<uint32_t, TrieBody *>;

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
};

struct TerminationConditionInfo {
  OmegaArray *distribution;
  OmegaArray *increase;
  TerminationConditionInfo() {
    distribution = new OmegaArray();
    increase = new OmegaArray();
  };
};

struct Trie {
  TrieBody *body;
  TerminationConditionInfo *info;
  Trie() {
    body = new TrieBody();
    info = new TerminationConditionInfo();
  };
  // HashTable *trieLeavesTable;
};

typedef struct _CanonicalLabel {
  Hash first;
  int second;
} CanonicalLabel;

struct HashString {
  int creditIndex;
  std::vector<uint32_t *> *body;

  HashString() {
    creditIndex = 0;
    body = new std::vector<uint32_t *>();
  }
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
  };

  InheritedVertex(const InheritedVertex &iVertex) {
    this->type = iVertex.type;
    strcpy(this->name, iVertex.name);
    this->canonicalLabel = iVertex.canonicalLabel;
    this->hashString = iVertex.hashString;
    this->isPushedIntoFixCreditIndex = iVertex.isPushedIntoFixCreditIndex;
    this->beforeID = iVertex.beforeID;
    this->ownerNode = iVertex.ownerNode;
    this->ownerList = iVertex.ownerList;
    this->ownerCell = iVertex.ownerCell;
    this->conventionalPropagationMemo = new std::vector<int>();
    int i;
    for (i = 0; i < numStack(iVertex.conventionalPropagationMemo); i++) {
      int tmp = readStack(iVertex.conventionalPropagationMemo, i);
      writeStack(this->conventionalPropagationMemo, i, tmp);
    }
    this->equivalenceClassOfIsomorphism = iVertex.equivalenceClassOfIsomorphism;
  }
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
  } else if (numStack(iVertexA.conventionalPropagationMemo) <
             numStack(iVertexB.conventionalPropagationMemo)) {
    return false;
  } else if (numStack(iVertexA.conventionalPropagationMemo) >
             numStack(iVertexB.conventionalPropagationMemo)) {
    return false;
  } else {
    int degree = numStack(iVertexA.conventionalPropagationMemo);
    int i;
    std::vector<int> *iStackA = iVertexA.conventionalPropagationMemo;
    std::vector<int> *iStackB = iVertexB.conventionalPropagationMemo;

    for (i = 0; i < degree; i++) {
      if (readStack(iStackA, i) < readStack(iStackB, i)) {
        return false;
      } else if (readStack(iStackA, i) > readStack(iStackB, i)) {
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
  } else if (numStack(iVertexA.conventionalPropagationMemo) <
             numStack(iVertexB.conventionalPropagationMemo)) {
    return true;
  } else if (numStack(iVertexA.conventionalPropagationMemo) >
             numStack(iVertexB.conventionalPropagationMemo)) {
    return false;
  } else {
    int degree = numStack(iVertexA.conventionalPropagationMemo);
    int i;
    std::vector<int> *iStackA = iVertexA.conventionalPropagationMemo;
    std::vector<int> *iStackB = iVertexB.conventionalPropagationMemo;

    for (i = 0; i < degree; i++) {
      if (readStack(iStackA, i) < readStack(iStackB, i)) {
        return true;
      } else if (readStack(iStackA, i) > readStack(iStackB, i)) {
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

template <typename S>
void pushTrieBodyIntoGoAheadStackWithoutOverlap(S *stack, TrieBody *body);
void freeInheritedVertex(InheritedVertex *iVertex);
Trie *makeTrie();
void freeTrie(Trie *trie);
Bool triePropagate(Trie *trie, DiffInfo *diffInfo, Graphinfo *cAfterGraph,
                   Graphinfo *cBeforeGraph, int gapOfGlobalRootMemID,
                   int *stepOfPropagationPtr);
vertex_list *makeConventionalPropagationList(Trie *trie, int stepOfPropagation);
vertex_list::iterator getNextSentinel(vertex_list::iterator beginSentinel);
void putLabelsToAdjacentVertices(vertex_list *pList,
                                 ConvertedGraph *cAfterGraph,
                                 int gapOfGlobalRootMemID);
Bool classifyConventionalPropagationListWithAttribute(
    vertex_list *pList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID);
Bool getStableRefinementOfConventionalPropagationList(
    vertex_list *pList, ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID);
void inheritedVertexDump(InheritedVertex *iVertex);
void terminationConditionInfoDump(TerminationConditionInfo *tInfo);
void trieDump(Trie *trie);

HashString *makeHashString();
void freeHashString(HashString *hashString);
template <typename S>
void pushInheritedVertexIntoFixCreditIndexStackWithoutOverlap(
    S *fixCreditIndexStack, InheritedVertex *iVertex);
template <typename S>
InheritedVertex *
popInheritedVertexFromFixCreditIndexStackWithoutOverlap(S *fixCreditIndexStack);
template <typename S>
void fixCreditIndex(S *fixCreditIndexStack, ConvertedGraph *cAfterGraph,
                    int gapOfGlobalRootMemID);
template <typename S>
Hash callHashValue(InheritedVertex *iVertex, int index,
                   ConvertedGraph *cAfterGraph, int gapOfGlobalRootMemID,
                   S *fixCreditIndexStack);

void terminationConditionInfoDumpExperimentFromTrie(Trie *trie);
ConvertedGraphVertex *
correspondingVertexInConvertedGraph(InheritedVertex *iVertex,
                                    ConvertedGraph *cAfterGraph,
                                    int gapOfGlobalRootMemID);
Trie *gen_tmp_trie_from_originaltrie_and_gi(Trie *org_trie, Graphinfo *org_gi,
                                            Graphinfo *tmp_gi);
#endif
