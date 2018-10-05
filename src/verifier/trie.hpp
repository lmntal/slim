#ifndef _TRIE_H
#define _TRIE_H

#include"collection.hpp"
#include"omegaArray.hpp"
#include"hash.hpp"
#include"diff_info.hpp"
// #include"convertedgraph.hpp"

struct ConvertedGraph;

#define CLASS_SENTINEL (NULL)

struct TrieBody{
  KeyContainer key;
  List *inheritedVertices;
  TrieBody *parent;
  RedBlackTree *children;
  int depth;
  Bool isInfinitedDepth;
  Bool isPushedIntoGoAheadStack;

  TrieBody() {
    key.type = key_null;
    inheritedVertices = new List();
    parent = NULL;
    children = new RedBlackTree();
    depth = -1;
    isInfinitedDepth = false;
    isPushedIntoGoAheadStack = false;
  }
};

struct TerminationConditionInfo{
  OmegaArray *distribution;
  OmegaArray *increase;
  TerminationConditionInfo() {
    distribution = new OmegaArray();
    increase = new OmegaArray();
  };
};

struct Trie{
  TrieBody *body;
  TerminationConditionInfo *info;
  Trie() {
    body = new TrieBody();
    info = new TerminationConditionInfo();
  };
  //HashTable *trieLeavesTable;
};

typedef struct _CanonicalLabel{
  Hash first;
  int second;
} CanonicalLabel;

struct HashString{
  int creditIndex;
  DynamicArray *body;

  HashString(){
    creditIndex = 0;
    body = new DynamicArray();
  }

};

struct InheritedVertex{
  ConvertedGraphVertexType type;
  char name[NAME_LENGTH];
  CanonicalLabel canonicalLabel;
  HashString *hashString;
  Bool isPushedIntoFixCreditIndex;
  int beforeID;
  TrieBody *ownerNode;
  ListBody *ownerCell;
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
    ownerCell = NULL;
    conventionalPropagationMemo = new std::vector<int>();
    equivalenceClassOfIsomorphism = new DisjointSetForest();
  };
};

template <typename S>
void pushTrieBodyIntoGoAheadStackWithoutOverlap(S *stack,TrieBody *body);
void freeInheritedVertex(InheritedVertex *iVertex);
Trie *makeTrie();
void freeTrie(Trie *trie);
Bool triePropagate(Trie *trie,DiffInfo *diffInfo,Graphinfo *cAfterGraph,Graphinfo *cBeforeGraph,int gapOfGlobalRootMemID,int *stepOfPropagationPtr);
List *makeConventionalPropagationList(Trie *trie,int stepOfPropagation);
ListBody *getNextSentinel(ListBody *beginSentinel);
void putLabelsToAdjacentVertices(List *pList,ConvertedGraph *cAfterGraph,int gapOfGlobalRootMemID);
Bool classifyConventionalPropagationListWithAttribute(List *pList,ConvertedGraph *cAfterGraph,int gapOfGlobalRootMemID);
Bool getStableRefinementOfConventionalPropagationList(List *pList,ConvertedGraph *cAfterGraph,int gapOfGlobalRootMemID);
InheritedVertex *copyInheritedVertex(InheritedVertex *iVertex);
void *copyInheritedVertexCaster(void *iVertex);
void inheritedVertexDump(InheritedVertex *iVertex);
void inheritedVertexDumpCaster(void *iVertex);
void terminationConditionInfoDump(TerminationConditionInfo *tInfo);
void trieDump(Trie *trie);

HashString *makeHashString();
void freeHashString(HashString *hashString);
template <typename S>
void pushInheritedVertexIntoFixCreditIndexStackWithoutOverlap(S *fixCreditIndexStack,InheritedVertex *iVertex);
template <typename S>
InheritedVertex *popInheritedVertexFromFixCreditIndexStackWithoutOverlap(S *fixCreditIndexStack);
template <typename S>
void fixCreditIndex(S *fixCreditIndexStack,ConvertedGraph *cAfterGraph,int gapOfGlobalRootMemID);
template <typename S>
Hash callHashValue(InheritedVertex *iVertex,int index,ConvertedGraph *cAfterGraph,int gapOfGlobalRootMemID,S *fixCreditIndexStack);

void terminationConditionInfoDumpExperimentFromTrie(Trie *trie);
ConvertedGraphVertex *correspondingVertexInConvertedGraph(InheritedVertex *iVertex,ConvertedGraph *cAfterGraph,int gapOfGlobalRootMemID);
Trie *gen_tmp_trie_from_originaltrie_and_gi(Trie *org_trie, Graphinfo *org_gi, Graphinfo *tmp_gi);
#endif
