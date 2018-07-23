#ifndef _COLLECTION_H
#define _COLLECTION_H

#include<stdint.h>
#include"util.hpp"
#include"hash.hpp"
#include<string>
#include<cstring>
#define INIT_CAP (4)
typedef enum Order{
  LT,//less than
  EQ,//equal
  GT//greater than
}Order;

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

struct Stack{
  int num;
  DynamicArray *body;

  int numStack(){
    return num;
  }

  void stackDump(void valueDump(void *));
  Stack() {
    num=0;
    body=new DynamicArray();
  };
};

Stack *makeStack();
void freeStack(Stack *stack);
void freeStackAndValues(Stack *stack,void freeValue(void *));
Bool isEmptyStack(Stack *stack);
void setStackEmpty(Stack *stack);
void *peekStack(Stack *stack);
void *popStack(Stack *stack);
void *pushStack(Stack *stack,void *value);
int numStack(Stack *stack);
void *readStack(Stack *stack,int index);
void *writeStack(Stack *stack,int index,void *value);
void swapStack(Stack *source,Stack *target);


typedef intptr_t CollectionInt;

typedef struct _IntStack{
  Stack *body;
} IntStack;

IntStack *makeIntStack();
void freeIntStack(IntStack *iStack);
Bool isEmptyIntStack(IntStack *iStack);
void setIntStackEmpty(IntStack *iStack);
int peekIntStack(IntStack *iStack);
int popIntStack(IntStack *iStack);
void pushIntStack(IntStack *iStack,int value);
int numIntStack(IntStack *iStack);
int readIntStack(IntStack *iStack,int index);
int writeIntStack(IntStack *iStack,int index,int value);
void collectionIntDump(CollectionInt cInt);
void collectionIntDumpCaster(void *cInt);
void intStackDump(IntStack *iStack);

typedef enum {
  key_none,
  key_uint32,
  key_discretePropagationList,
  key_int,
  key_double,
  key_string,
  key_null
}KeyType;

typedef struct _ListBody{
  void *value;
  struct _ListBody *next;
  struct _ListBody *prev;
}ListBody;

typedef struct _List{
  ListBody *sentinel;
}List;

List *makeList();
void pushList(List *list,void *value);
void *popList(List *list);
void *peekList(List *list);
ListBody *makeCell(void *value);
void pushCell(List *list,ListBody *cell);
ListBody *popCell(List *list);
ListBody *peekCell(List *list);
void *cutCell(ListBody *cell);
void insertNextCell(ListBody *cellA,ListBody *cellB);
void forEachValueOfList(List *list,void func(void *));
void forEachCellOfList(List *list,void func(ListBody *));
void listDump(List *list,void valueDump(void *));
void freeList(List *list);
void freeListCaster(void *list);
void freeListWithValues(List *list,void freeValue(void *));
Bool isEmptyList(List *list);
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

typedef struct _DisjointSetForest{
  struct _DisjointSetForest *parent;
  int rank;
} DisjointSetForest;

DisjointSetForest *makeDisjointSetForest();
void freeDisjointSetForest(DisjointSetForest *x);
void initializeDisjointSetForest(DisjointSetForest *x);
DisjointSetForest *findDisjointSetForest(DisjointSetForest *x);
void unionDisjointSetForest(DisjointSetForest *x,DisjointSetForest *y);
Bool isInSameDisjointSetForest(DisjointSetForest *x,DisjointSetForest *y);


#endif

