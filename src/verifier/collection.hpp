#ifndef _COLLECTION_H
#define _COLLECTION_H

#include "hash.hpp"
#include "util.hpp"
#include <cstring>
#include <queue>
#include <stack>
#include <stdint.h>
#include <string>
#include <vector>

#define INIT_CAP (4)
typedef enum Order {
  LT, // less than
  EQ, // equal
  GT  // greater than
} Order;

using DynamicArray = std::vector<void *>;

inline DynamicArray *makeDynamicArray() { return new DynamicArray; }
template <typename DynamicArray>
inline void freeDynamicArray(DynamicArray *DArray) {
  delete DArray;
}
template <typename DynamicArray>
inline void freeDynamicArrayAndValues(DynamicArray *DArray,
                                      void freeValue(void *)) {
  int i;

  for (i = 0; i < DArray->size(); i++) {
    if ((*DArray)[i] != NULL) {
      freeValue((*DArray)[i]);
      (*DArray)[i] = NULL;
    }
  }
  freeDynamicArray(DArray);
  return;
}
template <typename T>
inline T readDynamicArray(std::vector<T> *DArray, int index) {
  return (*DArray)[index];
}
template <typename T>
inline void writeDynamicArray(std::vector<T> *DArray, int index, T value) {
  DArray->resize(index, nullptr);
  (*DArray)[index] = value;
}
template <typename T>
inline void dynamicArrayDump(std::vector<T> *DArray, void valueDump(T)) {
  int i;
  for (i = 0; i < DArray->size(); i++) {
    if ((*DArray)[i] == NULL) {
      // fprintf(stdout,"%d:NULL\n",i);
    } else {
      fprintf(stdout, "%d:", i);
      valueDump((*DArray)[i]);
      printf("\n");
    }
  }
}

template <typename T> void freeStack(std::stack<T> *stack) { delete stack; }
template <typename T> void freeStack(std::vector<T> *stack) { delete stack; }
template <typename T>
auto popStack(std::vector<T> *stack) -> decltype(stack->back()) {
  auto ret = stack->back();
  stack->pop_back();
  return ret;
}
template <typename T>
auto popStack(std::stack<T> *stack) -> decltype(stack->top()) {
  auto ret = stack->top();
  stack->pop();
  return ret;
}
template <typename T, typename U>
void pushStack(std::vector<T> *stack, U value) {
  stack->push_back(value);
}
template <typename T, typename U>
void pushStack(std::stack<T> *stack, U value) {
  stack->push(value);
}
template <typename T> int numStack(T *stack) { return stack->size(); }
template <typename T> T readStack(std::vector<T> *stack, int index) {
  return stack->at(index);
}
template <typename T>
void writeStack(std::vector<T> *stack, int index, T value) {
  (*stack)[index] = value;
}
template <typename T, typename U> void swapStack(T *source, U *target) {
  source->swap(*target);
}

template <typename T>
void dump(const std::vector<T> &stack, void valueDump(T)) {
  for (int i = 0; i < stack.size(); i++) {
    fprintf(stdout, "%d:", i);
    valueDump(stack[i]);
    fprintf(stdout, "\n");
  }
}

typedef intptr_t CollectionInt;

template <typename T> struct ListBody__ {
  T value;
  ListBody__ *next;
  ListBody__ *prev;
  ListBody__() {
    value = NULL;
    next = this;
    prev = this;
  };
  ListBody__(T value) : value(value), next(nullptr), prev(nullptr) {}
};

template <typename T> class List__ {
public:
  ListBody__<T> *sentinel;

  using iterator = ListBody__<T> *;

  bool empty() { return sentinel->next == sentinel; }
  List__() {
    sentinel = new ListBody__<T>;
    sentinel->value = NULL;
    sentinel->next = sentinel;
    sentinel->prev = sentinel;
  }

  T front() { return sentinel->next->value; }

  iterator begin() { return sentinel->next; }
  iterator end() { return sentinel; }

  void push_front(T value) { splice(begin(), new ListBody__<T>(value)); }

  void splice(iterator iter, iterator cell) {
    cell->next = iter;
    iter->prev = cell;
    iter->prev->next = cell;
    cell->prev = iter->prev;
  }
};

namespace std {
template <typename T> inline ListBody__<T> *next(ListBody__<T> *b, int n) {
  for (int i = 0; i < n; i++)
    b = b->next;
  return b;
}
} // namespace std

List__<void *> *makeList();
template <typename T> void pushCell(List__<T> *list, ListBody__<T> *cell);
template <typename T> ListBody__<T> *popCell(List__<T> *list);
template <typename T> void *cutCell(ListBody__<T> *cell);
template <typename T>
void insertNextCell(ListBody__<T> *cellA, ListBody__<T> *cellB);
template <typename T> void forEachValueOfList(List__<T> *list, void func(T));
template <typename T> void listDump(List__<T> *list, void valueDump(T));
template <typename T> void freeList(List__<T> *list);
void freeListCaster(void *list);
template <typename T>
void freeListWithValues(List__<T> *list, void freeValue(T));
template <typename T> bool isSingletonList(List__<T> *list);
template <typename T1, typename T2>
Order compareList(List__<T1> *listA, List__<T2> *listB,
                  Order compareValue(T1, T2));
template <typename T> List__<T> *copyList(List__<T> *l);
template <typename T>
List__<T> *copyListWithValues(List__<T> *l, void *copyValue(T));

template <typename T> struct KeyContainer__;
template <typename T> T get(const KeyContainer__<T> &key);

template <typename T> struct KeyContainer__ {
  T value;

  KeyContainer__() = default;
  template <typename U>
  KeyContainer__(const KeyContainer__<U> &k) : value(k.value) {
    value = k.value;
  }

  operator T() { return get<KeyContainer__<T>>(*this); }
};

KeyContainer__<List__<void *> *>
makeDiscretePropagationListKey(List__<void *> *dpList);

typedef enum _Color { RED, BLACK } Color;

typedef enum _Direction { LEFT, RIGHT } Direction;

template <typename K, typename V> struct _RedBlackTreeBody {
  KeyContainer__<K> key;
  Color color;
  V value;
  struct _RedBlackTreeBody *children[2];
};
using RedBlackTreeBody = _RedBlackTreeBody<void *, void *>;

template <typename K, typename V> struct RedBlackTree__ {
  _RedBlackTreeBody<K, V> *body;
  RedBlackTree__() { body = NULL; }
};
using RedBlackTree = RedBlackTree__<void *, void *>;

template <typename K, typename V>
void redBlackTreeKeyDump(RedBlackTree__<K, V> *rbt);
RedBlackTree *makeRedBlackTree();
template <typename K, typename V>
void redBlackTreeValueDump(RedBlackTree__<K, V> *rbt, void valueDump(V));
template <typename K, typename V>
void freeRedBlackTree(RedBlackTree__<K, V> *rbt);
template <typename K, typename V>
void freeRedBlackTreeWithValueInner(_RedBlackTreeBody<K, V> *rbtb,
                                    void freeValue(V));
template <typename K, typename V>
void freeRedBlackTreeWithValue(RedBlackTree__<K, V> *rbt, void freeValue(V));
template <typename K, typename V>
void *searchRedBlackTree(RedBlackTree__<K, V> *rbt, K key);
template <typename K, typename V>
void insertRedBlackTree(RedBlackTree__<K, V> *rbt, K key, V value);
template <typename K, typename V>
void deleteRedBlackTree(RedBlackTree__<K, V> *rbt, K key);
template <typename K, typename V>
Bool isEmptyRedBlackTree(RedBlackTree__<K, V> *rbt);
template <typename K, typename V>
Bool isSingletonRedBlackTree(RedBlackTree__<K, V> *rbt);
template <typename K, typename V>
V minimumElementOfRedBlackTree(RedBlackTree__<K, V> *rbt);

struct DisjointSetForest {
  DisjointSetForest *parent;
  int rank;
  DisjointSetForest() {
    parent = this;
    rank = 0;
  }
};

DisjointSetForest *makeDisjointSetForest();
void freeDisjointSetForest(DisjointSetForest *x);
void initializeDisjointSetForest(DisjointSetForest *x);
DisjointSetForest *findDisjointSetForest(DisjointSetForest *x);
void unionDisjointSetForest(DisjointSetForest *x, DisjointSetForest *y);
Bool isInSameDisjointSetForest(DisjointSetForest *x, DisjointSetForest *y);

#endif
