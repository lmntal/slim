#ifndef _MCKAY_H
#define _MCKAY_H

#include"hash.hpp"
#include"collection.hpp"
#include"trie.hpp"

struct CanonicalAdjacencyInformation{
  Hash hash;
  CanonicalLabel myLabel;
  Stack *adjacentLabels;
};

struct CanonicalAdjacencyList{
  Hash hashSum;
  Hash hashMul;
  Stack *adjacencyInformations;
};

void trieMcKay(Trie *trie,DiffInfo *diffInfo,Graphinfo *cAfterGraph,Graphinfo *cBeforeGraph);
Order compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabels(List *listA,List *listB);
void freePreserveDiscreteProapgationList(List *pdpList);
void freePreserveDiscreteProapgationListCaster(void *pdpList);
Bool checkIsomorphismValidity(DynamicArray *slimKeyCollection,RedBlackTree *McKayKeyCollection,List *canonicalDiscreteRefinement,int stateID);

#endif
