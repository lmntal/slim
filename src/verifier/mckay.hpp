#ifndef _MCKAY_H
#define _MCKAY_H

#include"hash.hpp"
#include"collection.hpp"
#include"trie.hpp"

#include <vector>

struct CanonicalAdjacencyInformation{
  Hash hash;
  CanonicalLabel myLabel;
  std::vector<void *> adjacentLabels; // ??
};

struct CanonicalAdjacencyList{
  Hash hashSum;
  Hash hashMul;
  std::vector<void *> adjacencyInformations; // ??
};

List* trieMcKay(Trie *trie,DiffInfo *diffInfo,Graphinfo *cAfterGraph,Graphinfo *cBeforeGraph);
Order compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabels(List *listA,List *listB);
void freePreserveDiscreteProapgationList(List *pdpList);
void freePreserveDiscreteProapgationListCaster(void *pdpList);
Bool checkIsomorphismValidity(DynamicArray *slimKeyCollection,RedBlackTree *McKayKeyCollection,List *canonicalDiscreteRefinement,int stateID);

#endif
