#include "collection.hpp"
#include "mckay.hpp"
#include "trie.hpp"
#include <iostream>
#define INIT_CAP (4)

// template <typename T1, typename T2>
// Order compareList(List__<T1> *listA, List__<T2> *listB,
//                   Order compareValue(T1, T2)) {
//   auto iteratorCellA = listA->sentinel->next;
//   auto iteratorCellB = listB->sentinel->next;

//   while (iteratorCellA != listA->sentinel && iteratorCellB !=
//   listB->sentinel) {
//     switch (compareValue(iteratorCellA->value, iteratorCellB->value)) {
//     case LT:
//       return LT;
//       break;
//     case GT:
//       return GT;
//       break;
//     case EQ:
//       iteratorCellA = iteratorCellA->next;
//       iteratorCellB = iteratorCellB->next;
//       continue;
//       break;
//     default:
//       CHECKER("unexpected order type\n");
//       exit(EXIT_FAILURE);
//       break;
//     }
//   }

//   if (iteratorCellA == listA->sentinel && iteratorCellB != listB->sentinel) {
//     return LT;
//   } else if (iteratorCellA != listA->sentinel &&
//              iteratorCellB == listB->sentinel) {
//     return GT;
//   } else {
//     return EQ;
//   }
// }

void setRBColor(Color color) {
  if (color == BLACK) {
    printf("\x1b[47m");
    printf("\x1b[30m");
  } else {
    printf("\x1b[41m");
  }
  return;
}

void setDefaultColor() {
  printf("\x1b[49m");
  printf("\x1b[39m");
  return;
}

DisjointSetForest *makeDisjointSetForest() {
  DisjointSetForest *ret =
      (DisjointSetForest *)malloc(sizeof(DisjointSetForest));
  ret->parent = ret;
  ret->rank = 0;

  return ret;
}

void freeDisjointSetForest(DisjointSetForest *x) {
  free(x);

  return;
}

void initializeDisjointSetForest(DisjointSetForest *x) {
  x->parent = x;
  x->rank = 0;

  return;
}

DisjointSetForest *findDisjointSetForest(DisjointSetForest *x) {
  if (x->parent == x) {
    return x;
  } else {
    x->parent = findDisjointSetForest(x->parent);
    return x->parent;
  }
}

void unionDisjointSetForest(DisjointSetForest *x, DisjointSetForest *y) {
  DisjointSetForest *xRoot = findDisjointSetForest(x);
  DisjointSetForest *yRoot = findDisjointSetForest(y);

  if (xRoot->rank > yRoot->rank) {
    yRoot->parent = xRoot;
  } else if (xRoot->rank < yRoot->rank) {
    xRoot->parent = yRoot;
  } else if (xRoot != yRoot) {
    xRoot->parent = xRoot;
    xRoot->rank++;
  }

  return;
}

Bool isInSameDisjointSetForest(DisjointSetForest *x, DisjointSetForest *y) {
  DisjointSetForest *xRoot = findDisjointSetForest(x);
  DisjointSetForest *yRoot = findDisjointSetForest(y);

  return (xRoot == yRoot);
}