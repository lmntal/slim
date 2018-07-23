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
  struct _TrieBody *parent;
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

typedef struct _HashString{
  int creditIndex;
  DynamicArray *body;
}HashString;

struct InheritedVertex{
  ConvertedGraphVertexType type;
  char name[NAME_LENGTH];
  CanonicalLabel canonicalLabel;
  HashString *hashString;
  Bool isPushedIntoFixCreditIndex;
  int beforeID;
  TrieBody *ownerNode;
  ListBody *ownerCell;
  IntStack *conventionalPropagationMemo;
  DisjointSetForest *equivalenceClassOfIsomorphism;
  //set form?
};

void freeInheritedVertex(InheritedVertex *iVertex);
Trie *makeTrie();
void freeTrie(Trie *trie);
Bool triePropagate(Trie *trie,DiffInfo *diffInfo,ConvertedGraph *cAfterGraph,ConvertedGraph *cBeforeGraph,int gapOfGlobalRootMemID,int *stepOfPropagationPtr,Bool measure);
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
void pushInheritedVertexIntoFixCreditIndexStackWithoutOverlap(Stack *fixCreditIndexStack,InheritedVertex *iVertex);
InheritedVertex *popInheritedVertexFromFixCreditIndexStackWithoutOverlap(Stack *fixCreditIndexStack);
void fixCreditIndex(Stack *fixCreditIndexStack,ConvertedGraph *cAfterGraph,int gapOfGlobalRootMemID);
Hash callHashValue(InheritedVertex *iVertex,int index,ConvertedGraph *cAfterGraph,int gapOfGlobalRootMemID,Stack *fixCreditIndexStack);

void terminationConditionInfoDumpExperimentFromTrie(Trie *trie);

#endif
