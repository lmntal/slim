#ifndef _COLLECTION_H
#define _COLLECTION_H

#include<stdint.h>
#include"util.hpp"
#include"hash.hpp"
#include<string>
#include<cstring>
#include <stack>
#include <vector>

#define INIT_CAP (4)
typedef enum Order{
  LT,//less than
  EQ,//equal
  GT//greater than
}Order;

using DynamicArray = std::vector<void *>;

inline DynamicArray *makeDynamicArray() {
  return new DynamicArray;
}
inline void freeDynamicArray(DynamicArray *DArray) {
  delete DArray;
}
inline void freeDynamicArrayAndValues(DynamicArray *DArray, void freeValue(void *)) {
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

template <typename T>
void freeStack(std::stack<T> *stack) { delete stack; }
template <typename T>
void freeStack(std::vector<T> *stack) { delete stack; }
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
void pushStack(std::vector<T> *stack, U value){
  stack->push_back(value);
}
template <typename T, typename U>
void pushStack(std::stack<T> *stack, U value){
  stack->push(value);
}
template <typename T>
int numStack(T *stack){
  return stack->size();
}
template <typename T>
T readStack(std::vector<T> *stack,int index) {
  return stack->at(index);
}
template <typename T>
void writeStack(std::vector<T> *stack,int index,T value) {
  (*stack)[index] = value;
}
template <typename T, typename U>
void swapStack(T *source,U *target) {
  source->swap(*target);
}

template <typename T>
void dump(const std::vector<T> &stack, void valueDump(T)){
  for (int i = 0; i < stack.size(); i++) {
    fprintf(stdout,"%d:",i);
    valueDump(stack[i]);
    fprintf(stdout,"\n");
  }
}

typedef intptr_t CollectionInt;

typedef enum {
  key_none,
  key_uint32,
  key_discretePropagationList,
  key_int,
  key_double,
  key_string,
  key_null
}KeyType;

struct ListBody{
  void *value;
  ListBody *next;
  ListBody *prev;
  ListBody() {
    value = NULL;
    next = this;
    prev = this;
  };
};

class List{
  ListBody *sentinel;

public:
  using iterator = ListBody *;

  bool empty() {
    return sentinel->next == sentinel;
  }
  List() {
    sentinel = new ListBody;
    sentinel->value = NULL;
    sentinel->next = sentinel;
    sentinel->prev = sentinel;
  }

  void *front() {
    return sentinel->next->value;
  }

  ListBody *begin() {
    return sentinel->next;
  }
  ListBody *end() {
    return sentinel;
  }

  friend List *makeList();
  friend void pushList(List *list,void *value);
  friend void *peekList(List *list);
  friend ListBody *makeCell(void *value);
  friend void pushCell(List *list,ListBody *cell);
  friend ListBody *popCell(List *list);
  friend ListBody *peekCell(List *list);
  friend void *cutCell(ListBody *cell);
  friend void insertNextCell(ListBody *cellA,ListBody *cellB);
  friend void forEachValueOfList(List *list,void func(void *));
  friend void forEachCellOfList(List *list,void func(ListBody *));
  friend void listDump(List *list,void valueDump(void *));
  friend void freeList(List *list);
  friend void freeListCaster(void *list);
  friend void freeListWithValues(List *list,void freeValue(void *));
  friend Bool isEmptyList(List *list);
  friend Bool isSingletonList(List *list);
  friend Order compareList(List *listA,List *listB,Order compareValue(void *,void *));
  friend List *copyList(List *l);
  friend List *copyListWithValues(List *l,void *copyValue(void *));

  void push_front(void *value) {
    pushList(this, value);
  }
};

namespace std {
  inline ListBody *next(ListBody *b, int n) {
    for (int i = 0; i < n; i++)
      b = b->next;
    return b;
  }
}

List *makeList();
ListBody *makeCell(void *value);
void pushCell(List *list,ListBody *cell);
ListBody *popCell(List *list);
void *cutCell(ListBody *cell);
void insertNextCell(ListBody *cellA,ListBody *cellB);
void forEachValueOfList(List *list,void func(void *));
void listDump(List *list,void valueDump(void *));
void freeList(List *list);
void freeListCaster(void *list);
void freeListWithValues(List *list,void freeValue(void *));
Bool isSingletonList(List *list);
Order compareList(List *listA,List *listB,Order compareValue(void *,void *));
List *copyList(List *l);
List *copyListWithValues(List *l,void *copyValue(void *));

typedef struct _KeyContainer{
  KeyType type;
  union {
    uint32_t ui32;
    List *discretePropagationList;
    int integer;
    double dbl;
    char *string;
  } u;
} KeyContainer;

KeyContainer *allocKey(KeyContainer key);
void keyDump(KeyContainer key);
KeyContainer makeUInt32Key(uint32_t ui32);
KeyContainer makeDiscretePropagationListKey(List *dpList);

typedef enum _Color{
  RED,
  BLACK
}Color;

typedef enum _Direction{
  LEFT,
  RIGHT
}Direction;

typedef struct _RedBlackTreeBody{
  KeyContainer key;
  Color color;
  void *value;
  struct _RedBlackTreeBody *children[2];
}RedBlackTreeBody;

struct RedBlackTree{
  RedBlackTreeBody *body;
  RedBlackTree () {
    body = NULL;
  }
};


void redBlackTreeKeyDump(RedBlackTree *rbt);
RedBlackTree *makeRedBlackTree();
void redBlackTreeValueDump(RedBlackTree *rbt,void valueDump(void *));
void freeRedBlackTree(RedBlackTree *rbt);
void freeRedBlackTreeWithValueInner(RedBlackTreeBody *rbtb,void freeValue(void *));
void freeRedBlackTreeWithValue(RedBlackTree *rbt,void freeValue(void *));
void *searchRedBlackTree(RedBlackTree *rbt,KeyContainer key);
void insertRedBlackTree(RedBlackTree *rbt,KeyContainer key,void *value);
void deleteRedBlackTree(RedBlackTree *rbt,KeyContainer key);
Bool isEmptyRedBlackTree(RedBlackTree *rbt);
Bool isSingletonRedBlackTree(RedBlackTree *rbt);
void *minimumElementOfRedBlackTree(RedBlackTree *rbt);

#define HASH_SIZE (1 << 16)
#define HASH_MASK (HASH_SIZE - 1)

typedef struct _HashTable{
  List **body;
}HashTable;

HashTable *makeHashTable();
void freeHashTable(HashTable *hTable);
void *findHashTable(HashTable *hTable,Hash key,void *value,int valueCompare(void *,void *));
void setHashTable(HashTable *hTable,Hash key,void *value,int valueCompare(void *,void *));
void *getHashTable(HashTable *hTable,Hash key,void *value,int valueCompare(void *,void *));

typedef struct _ValueWithPriority{
  void *value;
  int priority;
} ValueWithPriority;

typedef struct _PriorityQueue{
  int num;
  DynamicArray *body;
} PriorityQueue;

PriorityQueue *makePriorityQueue();
void freePriorityQueue(PriorityQueue *pQueue);
Bool isEmptyPriorityQueue(PriorityQueue *pQueue);
ValueWithPriority peekPriorityQueue(PriorityQueue *pQueue);
void pushPriorityQueue(PriorityQueue *pQueue,void *value,int priority);
ValueWithPriority popPriorityQueue(PriorityQueue *pQueue);
int numPriorityQueue(PriorityQueue *pQueue);
void priorityQueueDump(PriorityQueue *pQueue);

struct DisjointSetForest{
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
void unionDisjointSetForest(DisjointSetForest *x,DisjointSetForest *y);
Bool isInSameDisjointSetForest(DisjointSetForest *x,DisjointSetForest *y);


#endif

