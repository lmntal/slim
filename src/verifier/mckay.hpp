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

List__<void *>* trieMcKay(Trie *trie,DiffInfo *diffInfo,Graphinfo *cAfterGraph,Graphinfo *cBeforeGraph);
Order compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabelsInnerCaster(void *iVertexA,void *iVertexB);
template <typename Lista, typename Listb>
Order compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabels(Lista *listA,Listb *listB){
  return compareList(listA,listB,compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabelsInnerCaster);
}
template <typename List>
void freePreserveDiscreteProapgationList(List__<void *> *pdpList);
void freePreserveDiscreteProapgationListCaster(void *pdpList);
template <typename List>
Bool checkIsomorphismValidity(unbound_vector<List *> *slimKeyCollection,RedBlackTree *McKayKeyCollection,List *canonicalDiscreteRefinement,int stateID);

#endif
