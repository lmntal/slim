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

List__<void *> *makeList() { return new List__<void *>; }

Bool isEmptyList(List__<void *> *list) {
  return (list->sentinel->next == list->sentinel);
}

template <typename T> bool isSingletonList(List__<T> *list) {
  return (!isEmptyList(list) && (list->sentinel->next->next == list->sentinel));
}

template <typename T> ListBody__<T> *makeCell(T value) {
  return new ListBody__<T>(value);
}

template <typename T>
void connectCell(ListBody__<T> *cellA, ListBody__<T> *cellB) {
  cellA->next = cellB;
  cellB->prev = cellA;

  return;
}

template <typename T>
void insertNextCell(ListBody__<T> *cellA, ListBody__<T> *cellB) {
  connectCell(cellB, cellA->next);
  connectCell(cellA, cellB);

  return;
}

void *peekList(List__<void *> *list) { return list->sentinel->next->value; }

template <typename T> void pushCell(List__<T> *list, ListBody__<T> *cell) {
  insertNextCell(list->begin(), cell);

  return;
}

template <typename T> ListBody__<T> *popCell(List__<T> *list) {
  auto cell = list->sentinel->next;
  connectCell(list->sentinel, cell->next);

  return cell;
}

template <typename T> void *cutCell(ListBody__<T> *cell) {
  void *ret = cell->value;
  connectCell(cell->prev, cell->next);

  return ret;
}

template <typename T> void forEachValueOfList(List__<T> *list, void func(T)) {
  auto sentinel = list->sentinel;

  for (auto iterator = sentinel->next; iterator != sentinel;) {
    auto iteratorNext = iterator->next;

    func(iterator->value);

    iterator = iteratorNext;
  }

  return;
}

template <typename T>
void forEachCellOfList(List__<T> *list, void func(ListBody__<T> *)) {
  auto sentinel = list->sentinel;

  for (auto iterator = sentinel->next; iterator != sentinel;) {
    auto iteratorNext = iterator->next;

    func(iterator);

    iterator = iteratorNext;
  }

  return;
}

template <typename T> void listDump(List__<T> *list, void valueDump(T)) {
  auto sentinel = list->sentinel;

  fprintf(stdout, "[");

  for (auto iterator = sentinel->next; iterator != sentinel;) {
    auto iteratorNext = iterator->next;

    valueDump(iterator->value);

    if (iterator->next != sentinel) {
      fprintf(stdout, ",");
    }

    iterator = iteratorNext;
  }

  fprintf(stdout, "]");

  return;
}

template <typename T> void freeCell(ListBody__<T> *cell) {
  free(cell);

  return;
}

template <typename T> void freeList(List__<T> *list) {
  forEachCellOfList(list, freeCell);
  free(list->sentinel);
  free(list);

  return;
}

void freeListCaster(void *list) {
  freeList((List__<void *> *)list);

  return;
}

template <typename T>
void freeListWithValues(List__<T> *list, void freeValue(T)) {
  forEachValueOfList(list, freeValue);
  forEachCellOfList(list, freeCell);
  free(list->sentinel);
  free(list);

  return;
}

template <typename T1, typename T2>
Order compareList(List__<T1> *listA, List__<T2> *listB,
                  Order compareValue(T1, T2)) {
  auto iteratorCellA = listA->sentinel->next;
  auto iteratorCellB = listB->sentinel->next;

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

template <typename T> List__<T> *copyList(List__<T> *l) {
  List__<T> *ret = new List__<T>();

  for (auto iteratorCell = l->sentinel->prev; iteratorCell != l->sentinel;
       iteratorCell = iteratorCell->prev) {
    pushCell(ret, new ListBody__<T>(iteratorCell->value));
  }

  return ret;
}

template <typename T>
List__<T> *copyListWithValues(List__<T> *l, void *copyValue(T)) {
  List__<void *> *ret = makeList();

  for (auto iteratorCell = l->sentinel->prev; iteratorCell != l->sentinel;
       iteratorCell = iteratorCell->prev) {
    pushCell(ret, new ListBody__<T>(copyValue(iteratorCell->value)));
  }

  return ret;
}

KeyContainer__<List__<void *> *>
makeDiscretePropagationListKey(List__<void *> *dpList) {
  KeyContainer__<List__<void *> *> ret;

  ret.value = dpList;

  return ret;
}

template <typename T>
Order compareKey(KeyContainer__<T> a, KeyContainer__<T> b) {
  return compareDiscretePropagationListOfInheritedVerticesWithAdjacentLabels(
      a.value, b.value);
}

template <typename T>
bool operator<(const KeyContainer__<T> &a, const KeyContainer__<T> &b) {
  return compareKey(a, b) == LT;
}
template <typename T>
bool operator==(const KeyContainer__<T> &a, const KeyContainer__<T> &b) {
  return compareKey(a, b) == EQ;
}
template <typename T>
bool operator>(const KeyContainer__<T> &a, const KeyContainer__<T> &b) {
  return compareKey(a, b) == GT;
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

template <typename K, typename V>
void redBlackTreeKeyDumpInner(_RedBlackTreeBody<K, V> *rbtb, int depth) {
  int i;
  if (rbtb == NULL) {
    return;
  } else {
    redBlackTreeKeyDumpInner(rbtb->children[LEFT], depth + 1);
    for (i = 0; i < depth; i++) {
      fprintf(stdout, "    ");
    }
    setRBColor(rbtb->color);
    listDump(rbtb->key.value, inheritedVertexDumpCaster);
    setDefaultColor();
    fprintf(stdout, "\n");
    redBlackTreeKeyDumpInner(rbtb->children[RIGHT], depth + 1);
  }
}

template <typename K, typename V>
void redBlackTreeKeyDump(RedBlackTree__<K, V> *rbt) {
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

template <typename K, typename V>
void freeRedBlackTreeWithValueInner(_RedBlackTreeBody<K, V> *rbtb,
                                    void freeValue(V)) {
  if (rbtb != NULL) {
    freeRedBlackTreeWithValueInner(rbtb->children[LEFT], freeValue);
    freeRedBlackTreeWithValueInner(rbtb->children[RIGHT], freeValue);
    freeValue(rbtb->value);
    free(rbtb);
  }

  return;
}

template <typename K, typename V>
void freeRedBlackTree(RedBlackTree__<K, V> *rbt) {
  freeRedBlackTreeInner(rbt->body);
  free(rbt);
  return;
}

template <typename K, typename V>
void freeRedBlackTreeWithValue(RedBlackTree__<K, V> *rbt, void freeValue(V)) {
  freeRedBlackTreeWithValueInner(rbt->body, freeValue);
  free(rbt);
  return;
}

template <typename K, typename V>
void *searchRedBlackTreeInner(_RedBlackTreeBody<K, V> *rbtb, K key) {
  if (rbtb == NULL) {
    return NULL;
  } else {
    if (key < rbtb->key)
      return searchRedBlackTreeInner(rbtb->children[LEFT], key);
    else if (key == rbtb->key)
      return rbtb->value;
    else
      return searchRedBlackTreeInner(rbtb->children[RIGHT], key);
  }
}

template <typename K, typename V>
void *searchRedBlackTree(RedBlackTree__<K, V> *rbt, K key) {
  return searchRedBlackTreeInner(rbt->body, key);
}

template <typename K, typename V>
RedBlackTreeBody *insertRedBlackTreeInner(_RedBlackTreeBody<K, V> *rbtb,
                                          const K &key, void *value) {
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
    RedBlackTreeBody *child, *grandChild;

    if (key > rbtb->key) {
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
    } else {
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
    }
  }
}

template <typename K, typename V>
void insertRedBlackTree(RedBlackTree__<K, V> *rbt, K key, void *value) {
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

template <typename K, typename V>
RedBlackTreeBody *deleteRedBlackTreeInner(_RedBlackTreeBody<K, V> *rbtb, K key,
                                          Bool *changeFlag) {
  if (rbtb == NULL) {
    *changeFlag = FALSE;
    return NULL;
  } else {
    if (key < rbtb->key) {
      rbtb->children[LEFT] =
          deleteRedBlackTreeInner(rbtb->children[LEFT], key, changeFlag);

      if (*changeFlag) {
        return correctInDelete(rbtb, changeFlag, LEFT);
      } else {
        return rbtb;
      }
    }
    if (key == rbtb->key) {
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
    }
    if (key > rbtb->key) {
      rbtb->children[RIGHT] =
          deleteRedBlackTreeInner(rbtb->children[RIGHT], key, changeFlag);

      if (*changeFlag) {
        return correctInDelete(rbtb, changeFlag, RIGHT);
      } else {
        return rbtb;
      }
    }
  }
}

template <typename K, typename V>
void deleteRedBlackTree(RedBlackTree__<K, V> *rbt, K key) {
  Bool changeFlagBody;
  changeFlagBody = FALSE;

  rbt->body = deleteRedBlackTreeInner(rbt->body, key, &changeFlagBody);
  return;
}

template <typename K, typename V>
Bool isEmptyRedBlackTree(RedBlackTree__<K, V> *rbt) {
  return (rbt->body == NULL);
}

template <typename K, typename V>
Bool isSingletonRedBlackTree(RedBlackTree__<K, V> *rbt) {
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

template <typename K, typename V>
V minimumElementOfRedBlackTree(RedBlackTree__<K, V> *rbt) {
  return minimumElementOfRedBlackTreeInner(rbt->body);
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