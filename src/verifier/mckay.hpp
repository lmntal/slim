#ifndef _MCKAY_H
#define _MCKAY_H

#include "collection.hpp"
#include "hash.hpp"
#include "trie.hpp"

#include <vector>

struct CanonicalAdjacencyInformation {
  Hash hash;
  CanonicalLabel myLabel;
  std::vector<void *> adjacentLabels; // ??
};

struct CanonicalAdjacencyList {
  Hash hashSum;
  Hash hashMul;
  std::vector<void *> adjacencyInformations; // ??
};

vertex_list *trieMcKay(Trie *trie, DiffInfo *diffInfo, Graphinfo *cAfterGraph,
                       Graphinfo *cBeforeGraph);
Order compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabelsInner(
    InheritedVertex *iVertexA, InheritedVertex *iVertexB);
void freePreserveDiscreteProapgationList(vertex_list *pdpList);
template <typename List>
Bool checkIsomorphismValidity(unbound_vector<List *> *slimKeyCollection,
                              RedBlackTree *McKayKeyCollection,
                              List *canonicalDiscreteRefinement, int stateID);

#endif
