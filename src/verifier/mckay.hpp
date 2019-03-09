#ifndef _MCKAY_H
#define _MCKAY_H

#include "collection.hpp"
#include "hash.hpp"
#include "trie.hpp"

#include <vector>
#include <map>
#include <list>

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

using discrete_propagation_lists =
    std::map<vertex_list, vertex_list *>;
using key_collection =
    std::map<vertex_list, CollectionInt>;

std::list<std::list<InheritedVertex>> trieMcKay(Trie *trie, DiffInfo *diffInfo, Graphinfo *cAfterGraph, Graphinfo *cBeforeGraph);
Order compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabelsInner(
    InheritedVertex *iVertexA, InheritedVertex *iVertexB);
void freePreserveDiscreteProapgationList(vertex_list *pdpList);
Bool checkIsomorphismValidity(unbound_vector<vertex_list *> *slimKeyCollection,
                              key_collection *McKayKeyCollection,
                              vertex_list *canonicalDiscreteRefinement,
                              long stateID);

#endif
