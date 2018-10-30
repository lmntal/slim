#ifndef _COLLECTION_H
#define _COLLECTION_H

#include "element/element.h"

#include "hash.hpp"
#include "list.hpp"
#include "util.hpp"
#include <cstring>
#include <list>
#include <ostream>
#include <queue>
#include <stack>
#include <stdint.h>
#include <string>
#include <type_traits>
#include <vector>

#define INIT_CAP (4)
typedef enum Order {
  LT, // less than
  EQ, // equal
  GT  // greater than
} Order;

template <typename T> struct unbound_vector {
  static_assert(std::is_pointer<T>::value, "");
  std::vector<T> vec;

  using reference = typename std::vector<T>::reference;
  using const_reference = typename std::vector<T>::const_reference;
  using size_type = typename std::vector<T>::size_type;
  using difference_type = typename std::vector<T>::difference_type;
  using iterator = typename std::vector<T>::iterator;
  using const_iterator = typename std::vector<T>::const_iterator;

  unbound_vector() {}
  unbound_vector(size_type s, const T &v) : vec(s, v) {}

  size_type size() const { return vec.size(); }

  void clear() { vec.clear(); }

  reference at(size_type index) { return vec.at(index); }

  reference operator[](size_type index) { return vec[index]; }

  void resize(size_type sz) { vec.resize(sz); }

  void resize(size_type sz, const T &c) { vec.resize(sz, c); }

  iterator begin() noexcept { return vec.begin(); }
  const_iterator begin() const noexcept { return vec.begin(); }
  iterator end() noexcept { return vec.end(); }
  const_iterator end() const noexcept { return vec.end(); }

  void dump(void dumper(T value)) {
    for (int i = 0; i < this->size(); i++) {
      if (!this->at(i))
        continue;

      fprintf(stdout, "%d:", i);
      dumper(this->at(i));
      printf("\n");
    }
  }

  T read(int index) const {
    if (0 <= index && index < vec.size())
      return vec.at(index);
    return nullptr;
  }

  void write(size_type index, const T &value) {
    if (index >= vec.size()) {
      vec.resize(index + 1, nullptr);
    }
    vec.at(index) = value;
  }
};

template <typename T> void freeStack(std::stack<T> *stack) { delete stack; }
template <typename T> void freeStack(std::vector<T> *stack) { delete stack; }
template <typename T> T popStack(std::vector<T> *stack) {
  auto ret = stack->back();
  stack->pop_back();
  return ret;
}
template <typename T> T popStack(std::stack<T> *stack) {
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

template <typename List> inline bool isSingletonList(List *list) {
  return std::begin(*list) != std::end(*list) &&
         std::next(std::begin(*list), 1) == std::end(*list);
}

template <typename T> struct KeyContainer__;
template <typename T> T get(const KeyContainer__<T> &key);

template <typename T> struct KeyContainer__ {
  T value;

  KeyContainer__() = default;
  KeyContainer__(const T &v) : value(v) {}
  template <typename U>
  KeyContainer__(const KeyContainer__<U> &k) : value(k.value) {}

  operator T() const { return this->value; }

  bool operator==(const KeyContainer__<T> &b) const {
    return *value == *b.value;
  }
  bool operator<(const KeyContainer__<T> &b) const { return *value < *b.value; }
  bool operator>(const KeyContainer__<T> &b) const { return *b.value > *value; }
};

KeyContainer__<vertex_list *>
makeDiscretePropagationListKey(vertex_list *dpList);

typedef enum _Color { RED, BLACK } Color;

typedef enum _Direction { LEFT, RIGHT } Direction;

template <typename K, typename V> struct _RedBlackTreeBody {
  Color color;
  std::pair<K, V> elm;
  struct _RedBlackTreeBody *children[2];

  _RedBlackTreeBody(const K &key, const V &value)
      : elm(key, value), color(RED) {
    children[LEFT] = nullptr;
    children[RIGHT] = nullptr;
  }

  ~_RedBlackTreeBody() {
    delete children[LEFT];
    delete children[RIGHT];
  }

  V search(const K &key) const {
    if (key < KeyContainer__<K>(elm.first))
      return children[LEFT] ? children[LEFT]->search(key) : (V) nullptr;
    else if (key == KeyContainer__<K>(elm.first))
      return elm.second;
    else
      return children[RIGHT] ? children[RIGHT]->search(key) : (V) nullptr;
  }

  _RedBlackTreeBody *insert(const K &key, const V &value) {
    _RedBlackTreeBody *child, *grandChild;

    if (key > KeyContainer__<K>(elm.first)) {
      if (!this->children[RIGHT]) {
        child = new _RedBlackTreeBody(key, value);
      } else {
        child = this->children[RIGHT]->insert(key, value);
      }
      this->children[RIGHT] = child;

      if (this->color == BLACK && child != NULL && child->color == RED) {
        if (child->children[LEFT] != NULL &&
            child->children[LEFT]->color == RED) {
          grandChild = child->children[LEFT];

          this->children[RIGHT] = grandChild->children[LEFT];
          child->children[LEFT] = grandChild->children[RIGHT];
          grandChild->children[LEFT] = this;
          grandChild->children[RIGHT] = child;

          child->color = BLACK;

          return grandChild;
        } else if (child->children[RIGHT] != NULL &&
                   child->children[RIGHT]->color == RED) {

          grandChild = child->children[RIGHT];

          this->children[RIGHT] = child->children[LEFT];
          child->children[LEFT] = this;

          grandChild->color = BLACK;

          return child;
        } else {
          return this;
        }
      } else {
        return this;
      }
    } else {
      if (!children[LEFT]) {
        child = new _RedBlackTreeBody(key, value);
      } else {
        child = this->children[LEFT]->insert(key, value);
      }
      this->children[LEFT] = child;

      if (this->color == BLACK && child != NULL && child->color == RED) {
        if (child->children[LEFT] != NULL &&
            child->children[LEFT]->color == RED) {
          grandChild = child->children[LEFT];

          this->children[LEFT] = child->children[RIGHT];
          child->children[RIGHT] = this;

          grandChild->color = BLACK;

          return child;
        } else if (child->children[RIGHT] != NULL &&
                   child->children[RIGHT]->color == RED) {
          grandChild = child->children[RIGHT];

          this->children[LEFT] = grandChild->children[RIGHT];
          child->children[RIGHT] = grandChild->children[LEFT];
          grandChild->children[LEFT] = child;
          grandChild->children[RIGHT] = this;

          child->color = BLACK;

          return grandChild;
        } else {
          return this;
        }
      } else {
        return this;
      }
    }
  }

  _RedBlackTreeBody *exchangeMaxValue(_RedBlackTreeBody *target,
                                      bool *changeFlag) {
    if (this->children[RIGHT] == NULL) {
      target->elm = this->elm;

      *changeFlag = (this->color == BLACK);

      auto tmp = this->children[LEFT];

      free(this);

      return tmp;
    } else {
      this->children[RIGHT] =
          this->children[RIGHT]->exchangeMaxValue(target, changeFlag);

      if (*changeFlag) {
        return this->correctInDelete(changeFlag, RIGHT);
      } else {
        return this;
      }
    }
  }

  _RedBlackTreeBody *erase(const K &key, bool *changeFlag) {
    if (key < KeyContainer__<K>(elm.first)) {
      if (!this->children[LEFT]) {
        *changeFlag = false;
        return nullptr;
      }

      this->children[LEFT] = this->children[LEFT]->erase(key, changeFlag);

      if (*changeFlag) {
        return this->correctInDelete(changeFlag, LEFT);
      } else {
        return this;
      }
    }
    if (key == KeyContainer__<K>(elm.first)) {
      if (this->children[LEFT] == NULL) {
        *changeFlag = (this->color == BLACK);

        auto tmp = this->children[RIGHT];

        free(this);

        return tmp;
      } else {
        this->children[LEFT] =
            this->children[LEFT]->exchangeMaxValue(this, changeFlag);
        if (*changeFlag) {
          return this->correctInDelete(changeFlag, LEFT);
        } else {
          return this;
        }
      }
    }
    if (key > KeyContainer__<K>(elm.first)) {
      if (!this->children[RIGHT]) {
        *changeFlag = false;
        return nullptr;
      }
      this->children[RIGHT] = this->children[RIGHT]->erase(key, changeFlag);

      if (*changeFlag) {
        return this->correctInDelete(changeFlag, RIGHT);
      } else {
        return this;
      }
    }
  }

  _RedBlackTreeBody *correctInDelete(bool *changeFlag,
                                     Direction CHILD_DIRECTION) {
    Direction dir = CHILD_DIRECTION;
    Direction cou = counterDirection(dir);

    auto child = this->children[cou];

    if (child->color == BLACK) {
      if (child->children[dir] != NULL && child->children[dir]->color == RED) {
        auto grandChild = child->children[dir];

        this->children[cou] = grandChild->children[dir];
        child->children[dir] = grandChild->children[cou];
        grandChild->children[dir] = this;
        grandChild->children[cou] = child;

        grandChild->color = this->color;
        this->color = BLACK;

        *changeFlag = FALSE;

        return grandChild;
      } else if (child->children[cou] != NULL &&
                 child->children[cou]->color == RED) {
        auto grandChild = child->children[cou];

        this->children[cou] = child->children[dir];
        child->children[dir] = this;

        child->color = this->color;
        this->color = BLACK;
        grandChild->color = BLACK;

        *changeFlag = FALSE;

        return child;
      } else {
        *changeFlag = (this->color == BLACK);

        this->color = BLACK;
        child->color = RED;

        return this;
      }
    } else {
      this->children[cou] = child->children[dir];
      child->children[dir] = this;

      this->color = RED;
      child->color = BLACK;

      child->children[dir] = this->correctInDelete(changeFlag, dir);

      return child;
    }
  }

  Direction counterDirection(Direction dir) {
    if (dir == LEFT) {
      return RIGHT;
    } else {
      return LEFT;
    }
  }
};
using RedBlackTreeBody = _RedBlackTreeBody<void *, void *>;

template <typename K, typename V> struct RedBlackTree__ {
  _RedBlackTreeBody<K, V> *body;
  RedBlackTree__() { body = NULL; }

  V search(const K &key) const { return body->search(key); }
  void insert(const K &key, const V &value) {
    this->body = this->body->insert(key, value);
    this->body->color = BLACK;
  }

  size_t erase(const K &x) {
    bool changeFlagBody = false;
    this->body = body->erase(x, &changeFlagBody);
    return changeFlagBody;
  }

  bool empty() const { return body == nullptr; }

  struct iterator {
    using iterator_category = std::forward_iterator_tag;
    using value_type = std::pair<K, V>;
    using difference_type = std::ptrdiff_t;
    using pointer = std::pair<K, V> *;
    using reference = std::pair<K, V> &;
    using const_reference = const std::pair<K, V> &;

    iterator() : body(nullptr) {}
    iterator(_RedBlackTreeBody<K, V> &b, std::unique_ptr<iterator> &&parent)
        : body(&b), parent(std::move(parent)) {}
    iterator(const iterator &iter) = delete;
    iterator(iterator &&iter) : body(iter.body), parent(std::move(iter.parent)) {}

    pointer operator->() { return &body->elm; }
    reference operator*() { return body->elm; }
    const_reference operator*() const { return body->elm; }

    iterator &operator++() {
      if (!body->children[RIGHT]) {
        body = parent->body;
        parent = std::move(parent->parent);
      } else {
        auto i = leftmost_descendant(body->children[RIGHT], std::move(parent));
        body = i->body;
        parent = std::move(i->parent);
      }
      return *this;
    }
    iterator operator++(int i) {
      auto it = *this;
      ++(*this);
      return it;
    }

    bool operator==(const iterator &iter) const { return iter.body == body; }
    bool operator!=(const iterator &iter) const { return !(*this == iter); }

    static std::unique_ptr<iterator>
    leftmost_descendant(_RedBlackTreeBody<K, V> *body,
                        std::unique_ptr<iterator> &&p) {
      auto result = std::move(p);
      auto b = body;
      while (b) {
        result = slim::element::make_unique<iterator>(*b, std::move(result));
        b = b->children[LEFT];
      }
      return result;
    }

  private:
    _RedBlackTreeBody<K, V> *body;
    std::unique_ptr<iterator> parent;
    bool visited = false;
  };

  iterator begin() const {
    return std::move(*iterator::leftmost_descendant(body, nullptr));
  }
  iterator end() const { return iterator(); }
};
using RedBlackTree = RedBlackTree__<void *, void *>;

template <typename K, typename V>
void redBlackTreeValueDumpInner(_RedBlackTreeBody<K, V> *rbtb,
                                void valueDump(V)) {
  if (rbtb == NULL) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    return;
  } else {
    redBlackTreeValueDumpInner(rbtb->children[LEFT], valueDump);
    valueDump(rbtb->elm.second);
    // fprintf(stdout,"\n");
    redBlackTreeValueDumpInner(rbtb->children[RIGHT], valueDump);
    return;
  }
}

template <typename K, typename V>
void redBlackTreeValueDump(RedBlackTree__<K, V> *rbt, void valueDump(V)) {
  redBlackTreeValueDumpInner(rbt->body, valueDump);
  return;
}
template <typename K, typename V>
void freeRedBlackTreeWithValueInner(_RedBlackTreeBody<K, V> *rbtb,
                                    void freeValue(V)) {
  if (rbtb != NULL) {
    freeRedBlackTreeWithValueInner(rbtb->children[LEFT], freeValue);
    freeRedBlackTreeWithValueInner(rbtb->children[RIGHT], freeValue);
    free(rbtb);
  }

  return;
}
template <typename K, typename V>
void freeRedBlackTreeWithValue(RedBlackTree__<K, V> *rbt, void freeValue(V)) {
  for (auto &v : *rbt)
    freeValue(v.second);
  delete (rbt->body);
  free(rbt);
  return;
}

template <typename K, typename V>
Bool isSingletonRedBlackTree(RedBlackTree__<K, V> *rbt) {
  return (!rbt->empty() && (rbt->body->children[LEFT] == NULL &&
                            rbt->body->children[RIGHT] == NULL));
}

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
