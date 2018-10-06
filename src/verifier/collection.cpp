#include "collection.hpp"
#include "mckay.hpp"
#include "trie.hpp"
#define INIT_CAP (4)





unsigned int round2up(unsigned int n) {
  unsigned int ret = 1;
  while (ret && ret < n) {
    ret <<= 1;
  }
  if (ret == 0) {
    CHECKER("LARGE SIZE ERROR");
    exit(EXIT_FAILURE);
  }
  return ret;
}


List *makeList() { return new List; }

Bool isEmptyList(List *list) {
  return (list->sentinel->next == list->sentinel);
}

Bool isSingletonList(List *list) {
  return (!isEmptyList(list) && (list->sentinel->next->next == list->sentinel));
}

ListBody *makeCell(void *value) {
  ListBody *ret = (ListBody *)malloc(sizeof(ListBody));
  ret->value = value;
  ret->next = NULL;
  ret->prev = NULL;

  return ret;
}

void connectCell(ListBody *cellA, ListBody *cellB) {
  cellA->next = cellB;
  cellB->prev = cellA;

  return;
}

void insertNextCell(ListBody *cellA, ListBody *cellB) {
  connectCell(cellB, cellA->next);
  connectCell(cellA, cellB);

  return;
}

void pushList(List *list, void *value) {
  ListBody *cell = new ListBody();
  cell->value = value;
  insertNextCell(list->sentinel, cell);

  return;
}

void *peekList(List *list) { return list->sentinel->next->value; }

void pushCell(List *list, ListBody *cell) {
  insertNextCell(list->begin(), cell);

  return;
}

ListBody *popCell(List *list) {
  ListBody *cell = list->sentinel->next;
  connectCell(list->sentinel, cell->next);

  return cell;
}

void *cutCell(ListBody *cell) {
  void *ret = cell->value;
  connectCell(cell->prev, cell->next);

  return ret;
}

void forEachValueOfList(List *list, void func(void *)) {
  ListBody *sentinel = list->sentinel;
  ListBody *iterator;

  for (iterator = sentinel->next; iterator != sentinel;) {
    ListBody *iteratorNext = iterator->next;

    func(iterator->value);

    iterator = iteratorNext;
  }

  return;
}

void forEachCellOfList(List *list, void func(ListBody *)) {
  ListBody *sentinel = list->sentinel;
  ListBody *iterator;

  for (iterator = sentinel->next; iterator != sentinel;) {
    ListBody *iteratorNext = iterator->next;

    func(iterator);

    iterator = iteratorNext;
  }

  return;
}

void listDump(List *list, void valueDump(void *)) {
  ListBody *sentinel = list->sentinel;
  ListBody *iterator;

  fprintf(stdout, "[");

  for (iterator = sentinel->next; iterator != sentinel;) {
    ListBody *iteratorNext = iterator->next;

    valueDump(iterator->value);

    if (iterator->next != sentinel) {
      fprintf(stdout, ",");
    }

    iterator = iteratorNext;
  }

  fprintf(stdout, "]");

  return;
}

void freeCell(ListBody *cell) {
  free(cell);

  return;
}

void freeList(List *list) {
  forEachCellOfList(list, freeCell);
  free(list->sentinel);
  free(list);

  return;
}

void freeListCaster(void *list) {
  freeList((List *)list);

  return;
}

void freeListWithValues(List *list, void freeValue(void *)) {
  forEachValueOfList(list, freeValue);
  forEachCellOfList(list, freeCell);
  free(list->sentinel);
  free(list);

  return;
}

Order compareList(List *listA, List *listB,
                  Order compareValue(void *, void *)) {
  ListBody *iteratorCellA = listA->sentinel->next;
  ListBody *iteratorCellB = listB->sentinel->next;

  while (iteratorCellA != listA->sentinel && iteratorCellB != listB->sentinel) {
    switch (compareValue(iteratorCellA->value, iteratorCellB->value)) {
    case LT:
      return LT;
      break;
    case GT:
      return GT;
      break;
    case EQ:
      iteratorCellA = iteratorCellA->next;
      iteratorCellB = iteratorCellB->next;
      continue;
      break;
    default:
      CHECKER("unexpected order type\n");
      exit(EXIT_FAILURE);
      break;
    }
  }

  if (iteratorCellA == listA->sentinel && iteratorCellB != listB->sentinel) {
    return LT;
  } else if (iteratorCellA != listA->sentinel &&
             iteratorCellB == listB->sentinel) {
    return GT;
  } else {
    return EQ;
  }
}

List *copyList(List *l) {
  List *ret = makeList();

  ListBody *iteratorCell;
  for (iteratorCell = l->sentinel->prev; iteratorCell != l->sentinel;
       iteratorCell = iteratorCell->prev) {
    ListBody *tmpCell = (ListBody *)malloc(sizeof(ListBody));
    tmpCell->value = iteratorCell->value;
    pushCell(ret, tmpCell);
  }

  return ret;
}

List *copyListWithValues(List *l, void *copyValue(void *)) {
  List *ret = makeList();

  ListBody *iteratorCell;
  for (iteratorCell = l->sentinel->prev; iteratorCell != l->sentinel;
       iteratorCell = iteratorCell->prev) {
    ListBody *tmpCell = (ListBody *)malloc(sizeof(ListBody));
    tmpCell->value = copyValue(iteratorCell->value);
    pushCell(ret, tmpCell);
  }

  return ret;
}

KeyContainer *allocKey(KeyContainer key) {
  KeyContainer *ret = (KeyContainer *)malloc(sizeof(KeyContainer));
  *ret = key;

  return ret;
}

KeyContainer makeIntKey(int i) {
  KeyContainer ret;

  ret.type = key_int;
  ret.u.integer = i;

  return ret;
}

KeyContainer makeUInt32Key(uint32_t ui32) {
  KeyContainer ret;

  ret.type = key_uint32;
  ret.u.ui32 = ui32;

  return ret;
}

KeyContainer makeDiscretePropagationListKey(List *dpList) {
  KeyContainer ret;

  ret.type = key_discretePropagationList;
  ret.u.discretePropagationList = dpList;

  return ret;
}

Order compareKey(KeyContainer a, KeyContainer b) {
  int strcmpResult;

  assert(a.type == b.type);
  switch (a.type) {

  case key_uint32:
    return (a.u.ui32 < b.u.ui32 ? LT : a.u.ui32 > b.u.ui32 ? GT : EQ);
    break;

  case key_discretePropagationList:
    return compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabels(
        a.u.discretePropagationList, b.u.discretePropagationList);
    break;

  case key_int:
    return (a.u.integer < b.u.integer ? LT
                                      : a.u.integer > b.u.integer ? GT : EQ);
    break;

  case key_double:
    return (a.u.dbl < b.u.dbl ? LT : a.u.dbl > b.u.dbl ? GT : EQ);
    break;

  case key_string:
    strcmpResult = strcmp(a.u.string, b.u.string);
    return (strcmpResult < 0 ? LT : strcmpResult > 0 ? GT : EQ);
    break;

  default:
    CHECKER("This is unexpected key type\n");
    exit(EXIT_FAILURE);
    break;
  }
}

void keyDump(KeyContainer key) {
  switch (key.type) {

  case key_uint32:
    fprintf(stdout, "%08X", key.u.ui32);
    break;

  case key_discretePropagationList:
    listDump(key.u.discretePropagationList, inheritedVertexDumpCaster);
    break;

  case key_int:
    fprintf(stdout, "%d", key.u.integer);
    break;

  case key_double:
    fprintf(stdout, "%f", key.u.dbl);
    break;

  case key_string:
    fprintf(stdout, "%s", key.u.string);
    break;

  default:
    CHECKER("This is enexpected key type\n");
    exit(EXIT_FAILURE);
    break;
  }
}

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

void redBlackTreeKeyDumpInner(RedBlackTreeBody *rbtb, int depth) {
  int i;
  if (rbtb == NULL) {
    return;
  } else {
    redBlackTreeKeyDumpInner(rbtb->children[LEFT], depth + 1);
    for (i = 0; i < depth; i++) {
      fprintf(stdout, "    ");
    }
    setRBColor(rbtb->color);
    keyDump(rbtb->key);
    setDefaultColor();
    fprintf(stdout, "\n");
    redBlackTreeKeyDumpInner(rbtb->children[RIGHT], depth + 1);
  }
}

void redBlackTreeKeyDump(RedBlackTree *rbt) {
  redBlackTreeKeyDumpInner(rbt->body, 0);
  return;
}

RedBlackTree *makeRedBlackTree() {
  RedBlackTree *ret = (RedBlackTree *)malloc(sizeof(RedBlackTree));
  ret->body = NULL;
  return ret;
}

void redBlackTreeValueDumpInner(RedBlackTreeBody *rbtb,
                                void valueDump(void *)) {
  if (rbtb == NULL) {
    return;
  } else {
    redBlackTreeValueDumpInner(rbtb->children[LEFT], valueDump);
    valueDump(rbtb->value);
    // fprintf(stdout,"\n");
    redBlackTreeValueDumpInner(rbtb->children[RIGHT], valueDump);
    return;
  }
}

void redBlackTreeValueDump(RedBlackTree *rbt, void valueDump(void *)) {
  redBlackTreeValueDumpInner(rbt->body, valueDump);
  return;
}

void freeRedBlackTreeInner(RedBlackTreeBody *rbtb) {
  if (rbtb != NULL) {
    freeRedBlackTreeInner(rbtb->children[LEFT]);
    freeRedBlackTreeInner(rbtb->children[RIGHT]);
    free(rbtb);
  }

  return;
}

void freeRedBlackTree(RedBlackTree *rbt) {
  freeRedBlackTreeInner(rbt->body);
  free(rbt);
  return;
}

void freeRedBlackTreeWithValueInner(RedBlackTreeBody *rbtb,
                                    void freeValue(void *)) {
  if (rbtb != NULL) {
    freeRedBlackTreeWithValueInner(rbtb->children[LEFT], freeValue);
    freeRedBlackTreeWithValueInner(rbtb->children[RIGHT], freeValue);
    freeValue(rbtb->value);
    free(rbtb);
  }

  return;
}

void freeRedBlackTreeWithValue(RedBlackTree *rbt, void freeValue(void *)) {
  freeRedBlackTreeWithValueInner(rbt->body, freeValue);
  free(rbt);
  return;
}

void *searchRedBlackTreeInner(RedBlackTreeBody *rbtb, KeyContainer key) {
  if (rbtb == NULL) {
    return NULL;
  } else {
    Order ord = compareKey(key, rbtb->key);
    switch (ord) {

    case LT:
      return searchRedBlackTreeInner(rbtb->children[LEFT], key);
      break;

    case EQ:
      return rbtb->value;
      break;

    case GT:
      return searchRedBlackTreeInner(rbtb->children[RIGHT], key);
      break;

    default:
      CHECKER("This is unexpected order\n");
      exit(EXIT_FAILURE);
      break;
    }
  }
}

void *searchRedBlackTree(RedBlackTree *rbt, KeyContainer key) {
  return searchRedBlackTreeInner(rbt->body, key);
}

RedBlackTreeBody *insertRedBlackTreeInner(RedBlackTreeBody *rbtb,
                                          KeyContainer key, void *value) {
  if (rbtb == NULL) {
    RedBlackTreeBody *newRbtb =
        (RedBlackTreeBody *)malloc(sizeof(RedBlackTreeBody));
    newRbtb->key = key;
    newRbtb->color = RED;
    newRbtb->value = value;
    newRbtb->children[LEFT] = NULL;
    newRbtb->children[RIGHT] = NULL;
    return newRbtb;
  } else {
    Order ord = compareKey(key, rbtb->key);
    RedBlackTreeBody *child, *grandChild;

    switch (ord) {

    case LT:
    case EQ:
      child = insertRedBlackTreeInner(rbtb->children[LEFT], key, value);
      rbtb->children[LEFT] = child;

      if (rbtb->color == BLACK && child != NULL && child->color == RED) {
        if (child->children[LEFT] != NULL &&
            child->children[LEFT]->color == RED) {
          grandChild = child->children[LEFT];

          rbtb->children[LEFT] = child->children[RIGHT];
          child->children[RIGHT] = rbtb;

          grandChild->color = BLACK;

          return child;
        } else if (child->children[RIGHT] != NULL &&
                   child->children[RIGHT]->color == RED) {
          grandChild = child->children[RIGHT];

          rbtb->children[LEFT] = grandChild->children[RIGHT];
          child->children[RIGHT] = grandChild->children[LEFT];
          grandChild->children[LEFT] = child;
          grandChild->children[RIGHT] = rbtb;

          child->color = BLACK;

          return grandChild;
        } else {
          return rbtb;
        }
      } else {
        return rbtb;
      }

      break;

    case GT:
      child = insertRedBlackTreeInner(rbtb->children[RIGHT], key, value);
      rbtb->children[RIGHT] = child;

      if (rbtb->color == BLACK && child != NULL && child->color == RED) {
        if (child->children[LEFT] != NULL &&
            child->children[LEFT]->color == RED) {
          grandChild = child->children[LEFT];

          rbtb->children[RIGHT] = grandChild->children[LEFT];
          child->children[LEFT] = grandChild->children[RIGHT];
          grandChild->children[LEFT] = rbtb;
          grandChild->children[RIGHT] = child;

          child->color = BLACK;

          return grandChild;
        } else if (child->children[RIGHT] != NULL &&
                   child->children[RIGHT]->color == RED) {

          grandChild = child->children[RIGHT];

          rbtb->children[RIGHT] = child->children[LEFT];
          child->children[LEFT] = rbtb;

          grandChild->color = BLACK;

          return child;
        } else {
          return rbtb;
        }
      } else {
        return rbtb;
      }

      break;

    default:
      CHECKER("This is unexpected order\n");
      exit(EXIT_FAILURE);
      break;
    }
  }
}

void insertRedBlackTree(RedBlackTree *rbt, KeyContainer key, void *value) {
  rbt->body = insertRedBlackTreeInner(rbt->body, key, value);
  rbt->body->color = BLACK;
  return;
}

Direction counterDirection(Direction dir) {
  if (dir == LEFT) {
    return RIGHT;
  } else {
    return LEFT;
  }
}

RedBlackTreeBody *correctInDelete(RedBlackTreeBody *rbtb, Bool *changeFlag,
                                  Direction CHILD_DIRECTION) {
  Direction dir = CHILD_DIRECTION;
  Direction cou = counterDirection(dir);

  RedBlackTreeBody *child = rbtb->children[cou];

  if (child->color == BLACK) {
    if (child->children[dir] != NULL && child->children[dir]->color == RED) {
      RedBlackTreeBody *grandChild = child->children[dir];

      rbtb->children[cou] = grandChild->children[dir];
      child->children[dir] = grandChild->children[cou];
      grandChild->children[dir] = rbtb;
      grandChild->children[cou] = child;

      grandChild->color = rbtb->color;
      rbtb->color = BLACK;

      *changeFlag = FALSE;

      return grandChild;
    } else if (child->children[cou] != NULL &&
               child->children[cou]->color == RED) {
      RedBlackTreeBody *grandChild = child->children[cou];

      rbtb->children[cou] = child->children[dir];
      child->children[dir] = rbtb;

      child->color = rbtb->color;
      rbtb->color = BLACK;
      grandChild->color = BLACK;

      *changeFlag = FALSE;

      return child;
    } else {
      *changeFlag = (rbtb->color == BLACK);

      rbtb->color = BLACK;
      child->color = RED;

      return rbtb;
    }
  } else {
    rbtb->children[cou] = child->children[dir];
    child->children[dir] = rbtb;

    rbtb->color = RED;
    child->color = BLACK;

    child->children[dir] = correctInDelete(rbtb, changeFlag, dir);

    return child;
  }
}

RedBlackTreeBody *exchangeMaxValue(RedBlackTreeBody *rbtb,
                                   RedBlackTreeBody *target, Bool *changeFlag) {
  if (rbtb->children[RIGHT] == NULL) {
    target->value = rbtb->value;
    target->key = rbtb->key;

    *changeFlag = (rbtb->color == BLACK);

    RedBlackTreeBody *tmp = rbtb->children[LEFT];

    free(rbtb);

    return tmp;
  } else {
    rbtb->children[RIGHT] =
        exchangeMaxValue(rbtb->children[RIGHT], target, changeFlag);

    if (*changeFlag) {
      return correctInDelete(rbtb, changeFlag, RIGHT);
    } else {
      return rbtb;
    }
  }
}

RedBlackTreeBody *deleteRedBlackTreeInner(RedBlackTreeBody *rbtb,
                                          KeyContainer key, Bool *changeFlag) {
  if (rbtb == NULL) {
    *changeFlag = FALSE;
    return NULL;
  } else {
    Order ord = compareKey(key, rbtb->key);
    switch (ord) {

    case LT:
      rbtb->children[LEFT] =
          deleteRedBlackTreeInner(rbtb->children[LEFT], key, changeFlag);

      if (*changeFlag) {
        return correctInDelete(rbtb, changeFlag, LEFT);
      } else {
        return rbtb;
      }
      break;

    case EQ:
      if (rbtb->children[LEFT] == NULL) {
        *changeFlag = (rbtb->color == BLACK);

        RedBlackTreeBody *tmp = rbtb->children[RIGHT];

        free(rbtb);

        return tmp;
      } else {
        rbtb->children[LEFT] =
            exchangeMaxValue(rbtb->children[LEFT], rbtb, changeFlag);
        if (*changeFlag) {
          return correctInDelete(rbtb, changeFlag, LEFT);
        } else {
          return rbtb;
        }
      }
      break;

    case GT:
      rbtb->children[RIGHT] =
          deleteRedBlackTreeInner(rbtb->children[RIGHT], key, changeFlag);

      if (*changeFlag) {
        return correctInDelete(rbtb, changeFlag, RIGHT);
      } else {
        return rbtb;
      }
      break;

    default:
      CHECKER("This is unexpected order\n");
      exit(EXIT_FAILURE);
      break;
    }
  }
}

void deleteRedBlackTree(RedBlackTree *rbt, KeyContainer key) {
  Bool changeFlagBody;
  changeFlagBody = FALSE;

  rbt->body = deleteRedBlackTreeInner(rbt->body, key, &changeFlagBody);
  return;
}

Bool isEmptyRedBlackTree(RedBlackTree *rbt) { return (rbt->body == NULL); }

Bool isSingletonRedBlackTree(RedBlackTree *rbt) {
  return (!isEmptyRedBlackTree(rbt) && (rbt->body->children[LEFT] == NULL &&
                                        rbt->body->children[RIGHT] == NULL));
}

void *minimumElementOfRedBlackTreeInner(RedBlackTreeBody *rbtb) {
  if (rbtb->children[LEFT] == NULL) {
    return rbtb->value;
  } else {
    return minimumElementOfRedBlackTreeInner(rbtb->children[LEFT]);
  }
}

void *minimumElementOfRedBlackTree(RedBlackTree *rbt) {
  return minimumElementOfRedBlackTreeInner(rbt->body);
}

HashTable *makeHashTable() {
  HashTable *ret = (HashTable *)malloc(sizeof(HashTable));
  ret->body = (List **)calloc(HASH_SIZE, sizeof(List *));

  return ret;
}

void freeHashTable(HashTable *hTable) {
  free(hTable->body);
  free(hTable);

  return;
}

void *findHashTable(HashTable *hTable, Hash key, void *value,
                    int valueCompare(void *, void *)) {
  Hash bucket = key & HASH_MASK;
  List *chain = hTable->body[bucket];

  if (chain == NULL) {
    return NULL;
  } else {
    for (auto iterator = std::begin(*chain); iterator != std::end(*chain);
         iterator = std::next(iterator, 1)) {
      if (valueCompare(value, iterator->value) == 0) {
        return iterator;
      }
    }
  }

  return NULL;
}

void setHashTable(HashTable *hTable, Hash key, void *value,
                  int valueCompare(void *, void *)) {
  Hash bucket = key & HASH_MASK;
  List *chain = hTable->body[bucket];

  if (chain == NULL) {
    chain = makeList();
    hTable->body[bucket] = chain;
  }

  for (auto iterator = std::begin(*chain);; iterator = std::next(iterator, 1)) {
    if (iterator == std::end(*chain) ||
        (valueCompare(value, iterator->value) <= 0)) {
      ListBody *cell = (ListBody *)malloc(sizeof(ListBody));
      cell->value = value;
      insertNextCell(iterator->prev, cell);
      break;
    }
  }

  return;
}

void *getHashTable(HashTable *hTable, Hash key, void *value,
                   int valueCompare(void *, void *)) {
  Hash bucket = key & HASH_MASK;
  List *chain = hTable->body[bucket];
  void *ret = NULL;

  if (chain == NULL) {
    return NULL;
  } else {
    for (auto iterator = std::begin(*chain); iterator != std::end(*chain);
         iterator = iterator->next) {
      int compareResult = valueCompare(value, iterator->value);
      if (compareResult == 0) {
        ret = iterator->value;
        cutCell(iterator);
        freeCell(iterator);
        break;
      } else if (compareResult < 0) {
        break;
      }
    }

    if (isEmptyList(chain)) {
      freeList(chain);
      hTable->body[bucket] = NULL;
    }

    return ret;
  }
}

ValueWithPriority *wrapValueAndPriority(void *value, int priority) {
  ValueWithPriority *ret =
      (ValueWithPriority *)malloc(sizeof(ValueWithPriority));
  ret->value = value;
  ret->priority = priority;

  return ret;
}

void freePriorityWrapper(ValueWithPriority *vwPriority) {
  free(vwPriority);

  return;
}

PriorityQueue *makePriorityQueue() {
  PriorityQueue *ret = (PriorityQueue *)malloc(sizeof(PriorityQueue));
  ret->num = 0;
  ret->body = makeDynamicArray();

  return ret;
}

void freePriorityQueue(PriorityQueue *pQueue) {
  int i;

  for (i = 0; i < pQueue->num; i++) {
    ValueWithPriority *wrapper =
        (ValueWithPriority *)readDynamicArray(pQueue->body, i);
    freePriorityWrapper(wrapper);
  }

  freeDynamicArray(pQueue->body);
  free(pQueue);

  return;
}

Bool isEmptyPriorityQueue(PriorityQueue *pQueue) { return pQueue->num == 0; }

#define PQ_L_CHILD(X) (2 * (X) + 1)
#define PQ_R_CHILD(X) (2 * (X) + 2)
#define PQ_PARENT(X) (((X)-1) / 2)

ValueWithPriority peekPriorityQueue(PriorityQueue *pQueue) {
  return *((ValueWithPriority *)readDynamicArray(pQueue->body, 0));
}

void pushPriorityQueue(PriorityQueue *pQueue, void *value, int priority) {
  ValueWithPriority *tmpWrapper = wrapValueAndPriority(value, priority);

  int tmpIndex = pQueue->num;

  while (tmpIndex != 0) {
    int parentIndex = PQ_PARENT(tmpIndex);
    ValueWithPriority *parentWrapper =
        (ValueWithPriority *)readDynamicArray(pQueue->body, parentIndex);

    if (tmpWrapper->priority < parentWrapper->priority) {
      writeDynamicArray(pQueue->body, tmpIndex, (void *)tmpWrapper);
      break;
    } else {
      writeDynamicArray(pQueue->body, tmpIndex, (void *)parentWrapper);
      tmpIndex = parentIndex;
    }
  }

  if (tmpIndex == 0) {
    writeDynamicArray(pQueue->body, tmpIndex, (void *)tmpWrapper);
  }

  pQueue->num++;

  return;
}

ValueWithPriority popPriorityQueue(PriorityQueue *pQueue) {
  ValueWithPriority *retWrapper =
      (ValueWithPriority *)readDynamicArray(pQueue->body, 0);
  ValueWithPriority ret = *retWrapper;
  freePriorityWrapper(retWrapper);

  int tmpIndex = 0;
  ValueWithPriority *tmpWrapper =
      (ValueWithPriority *)readDynamicArray(pQueue->body, pQueue->num - 1);
  pQueue->num--;

  int greatChildIndex;
  ValueWithPriority *greatWrapper;

  while (PQ_L_CHILD(tmpIndex) < pQueue->num) {
    if (PQ_R_CHILD(tmpIndex) < pQueue->num) {
      int leftChildIndex = PQ_L_CHILD(tmpIndex);
      int rightChildIndex = PQ_R_CHILD(tmpIndex);
      ValueWithPriority *leftWrapper =
          (ValueWithPriority *)readDynamicArray(pQueue->body, leftChildIndex);
      ValueWithPriority *rightWrapper =
          (ValueWithPriority *)readDynamicArray(pQueue->body, rightChildIndex);

      if (leftWrapper->priority > rightWrapper->priority) {
        greatChildIndex = leftChildIndex;
        greatWrapper = leftWrapper;
      } else {
        greatChildIndex = rightChildIndex;
        greatWrapper = rightWrapper;
      }
    } else {
      greatChildIndex = PQ_L_CHILD(tmpIndex);
      greatWrapper =
          (ValueWithPriority *)readDynamicArray(pQueue->body, greatChildIndex);
    }

    if (tmpWrapper->priority > greatWrapper->priority) {
      writeDynamicArray(pQueue->body, tmpIndex, (void *)tmpWrapper);
      break;
    } else {
      writeDynamicArray(pQueue->body, tmpIndex, (void *)greatWrapper);
      tmpIndex = greatChildIndex;
    }
  }

  if (PQ_L_CHILD(tmpIndex) >= pQueue->num) {
    writeDynamicArray(pQueue->body, tmpIndex, (void *)tmpWrapper);
  }

  return ret;
}

int numPriorityQueue(PriorityQueue *pQueue) { return pQueue->num; }

void priorityWrapperDump(ValueWithPriority *wrapper) {
  fprintf(stdout, "<%p,%d>", wrapper->value, wrapper->priority);

  return;
}

void priorityWrapperDumpCaster(void *wrapper) {
  priorityWrapperDump((ValueWithPriority *)wrapper);

  return;
}

void priorityQueueDump(PriorityQueue *pQueue) {
  fprintf(stdout, "PQ_NUM:%d\n", pQueue->num);
  dynamicArrayDump(pQueue->body, priorityWrapperDumpCaster);

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