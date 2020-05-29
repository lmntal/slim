#ifndef _MCKAY_H
#define _MCKAY_H

#include "collection.hpp"
#include "hash.hpp"
#include "trie.hpp"

#include <list>
#include <map>
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



using discrete_propagation_lists =
    std::map<ComparablePropagationList, propagation_list *, PropagationListCmp>;
using key_collection = std::map<vertex_list, CollectionInt>;

std::vector<std::vector<std::string>> trieMcKay(Trie *trie, DiffInfo *diffInfo,
                           Graphinfo *cAfterGraph, Graphinfo *cBeforeGraph,
						std::map<int, int> &id_map, bool forward_f);
Order compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabelsInner(
    InheritedVertex *iVertexA, InheritedVertex *iVertexB);
void freePreserveDiscreteProapgationList(vertex_list *pdpList);
Bool checkIsomorphismValidity(unbound_vector<vertex_list *> *slimKeyCollection,
                              key_collection *McKayKeyCollection,
                              vertex_list *canonicalDiscreteRefinement,
                              long stateID);

#endif
