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
#include <list>

#define INIT_CAP (4)
typedef enum Order {
  LT, // less than
  EQ, // equal
  GT  // greater than
} Order;

struct DynamicArray{
  int cap;
  void **body;

  DynamicArray(){
    cap = INIT_CAP;
    if((body = (void **)calloc(sizeof(void *),INIT_CAP)) == NULL){
      CHECKER("CALLOC ERROR");
      exit(EXIT_FAILURE);
    }
  };
};

DynamicArray *makeDynamicArray();
void freeDynamicArray(DynamicArray *DArray);
void freeValuesOfDynamicArray(DynamicArray *DArray,void freeValue(void *));
void freeDynamicArrayAndValues(DynamicArray *DArray,void freeValue(void *));

DynamicArray *assureSizeOfDynamicArray(DynamicArray *DstDArray,int index);
void *readDynamicArray(DynamicArray *DArray,int index);
void *writeDynamicArray(DynamicArray *DArray,int index,void *value);
void dynamicArrayDump(DynamicArray *DArray,void valueDump(void *));

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

  T & operator *() { return value; }
};

template <typename T> class List__ {
public:
  ListBody__<T> *sentinel;

  struct iterator {
    using iterator_category = std::bidirectional_iterator_tag;
    using value_type = T;
    using difference_type = std::ptrdiff_t;
    using pointer = T *;
    using reference = T &;

    ListBody__<T> *body;

    iterator() : body(nullptr) {}
    iterator(ListBody__<T> *body) : body(body) {}
    iterator(const iterator &iter) : body(iter.body) {}

    T &operator *() { return body->value; }
    const T &operator *() const { return body->value; }

    iterator &operator++() {
      body = body->next;
      return *this;
    }
    iterator operator++(int i) {
      auto it = *this;
      ++(*this);
      return it;
    }
    iterator &operator--() {
      body = body->prev;
      return *this;
    }
    iterator operator--(int i) {
      auto it = *this;
      --(*this);
      return it;
    }

    bool operator==(const iterator &iter) const { return iter.body == body; }
    bool operator!=(const iterator &iter) const { return !(*this == iter); }

    bool operator<(const iterator &iter) const { return *(*this) < *iter; }
  };

  bool empty() { return sentinel->next == sentinel; }
  List__() {
    sentinel = new ListBody__<T>;
    sentinel->value = NULL;
    sentinel->next = sentinel;
    sentinel->prev = sentinel;
  }
  ~List__() {
    for (auto it = begin(); it != end(); ++it) delete it.body;
    delete sentinel;
  }

  T front() { return sentinel->next->value; }

  iterator begin() { return sentinel->next; }
  iterator end() { return sentinel; }

  const iterator begin() const { return sentinel->next; }
  const iterator end() const { return sentinel; }

  void push_front(T value) { splice(begin(), iterator(new ListBody__<T>(value))); }

  void insert(iterator iter, T value) {
    splice(iter, iterator(new ListBody__<T>(value)));
  }

  void splice(iterator iter, iterator cell) {
    if (cell.body->prev) cell.body->prev->next = cell.body->next;
    if (cell.body->next) cell.body->next->prev = cell.body->prev;

    cell.body->next = iter.body;
    iter.body->prev = cell.body;
    iter.body->prev->next = cell.body;
    cell.body->prev = iter.body->prev;
  }

  void splice(iterator position, List__& x, iterator i) {
    if (i.body->prev) i.body->prev->next = i.body->next;
    if (i.body->next) i.body->next->prev = i.body->prev;

    i.body->next = position.body;
    position.body->prev = i.body;
    position.body->prev->next = i.body;
    i.body->prev = position.body->prev;
  }

  iterator erase(iterator position) {
    auto ret = std::next(position, 1);
    if (position.body->prev) position.body->prev->next = position.body->next;
    if (position.body->next) position.body->next->prev = position.body->prev;
    delete position.body;
    return ret;
  }

  friend iterator begin(List__ &list);
  friend iterator end(List__ &list);
};

namespace std {
template <typename T> inline ListBody__<T> *next(ListBody__<T> *b, int n) {
  for (int i = 0; i < n; i++)
    b = b->next;
  return b;
}
} // namespace std

List__<void *> *makeList();
template <typename Iter, typename T = typename Iter::value_type> void *cutCell(Iter cell);
template <typename T>
void insertNextCell(typename List__<T>::iterator cellA, typename List__<T>::iterator cellB);
template <typename T> void forEachValueOfList(List__<T> *list, void func(T));
template <typename List, typename T> void listDump(List *list, void valueDump(T));
template <typename T> void freeList(List__<T> *list);
template <typename T>
void freeListWithValues(List__<T> *list, void freeValue(T));
template <typename T> bool isSingletonList(List__<T> *list);
template <typename T>
inline bool isSingletonList(std::list<T> *list) {
  return std::begin(*list) != std::end(*list) && std::next(std::begin(*list), 1) == std::end(*list);
}
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
