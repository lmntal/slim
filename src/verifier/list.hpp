
#include <iterator>
#include <ostream>

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

  T &operator*() { return value; }
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

    T &operator*() { return body->value; }
    const T &operator*() const { return body->value; }

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
    for (auto it = begin(); it != end(); ++it)
      delete it.body;
    delete sentinel;
  }

  T front() { return sentinel->next->value; }

  iterator begin() { return sentinel->next; }
  iterator end() { return sentinel; }

  const iterator begin() const { return sentinel->next; }
  const iterator end() const { return sentinel; }

  void push_front(T value) {
    splice(begin(), iterator(new ListBody__<T>(value)));
  }

  void push_back(T value) { splice(end(), iterator(new ListBody__<T>(value))); }

  iterator insert(iterator iter, const T &value) {
    auto it = iterator(new ListBody__<T>(value));
    splice(iter, it);
    return it;
  }

  void splice(iterator iter, iterator cell) {
    if (cell.body->prev)
      cell.body->prev->next = cell.body->next;
    if (cell.body->next)
      cell.body->next->prev = cell.body->prev;

    cell.body->next = iter.body;
    iter.body->prev = cell.body;
    iter.body->prev->next = cell.body;
    cell.body->prev = iter.body->prev;
  }

  void splice(iterator position, List__ &x, iterator i) {
    if (i.body->prev)
      i.body->prev->next = i.body->next;
    if (i.body->next)
      i.body->next->prev = i.body->prev;

    i.body->next = position.body;
    position.body->prev = i.body;
    position.body->prev->next = i.body;
    i.body->prev = position.body->prev;
  }

  iterator erase(iterator position) {
    auto ret = std::next(position, 1);
    if (position.body->prev)
      position.body->prev->next = position.body->next;
    if (position.body->next)
      position.body->next->prev = position.body->prev;
    delete position.body;
    return ret;
  }

  friend iterator begin(List__ &list);
  friend iterator end(List__ &list);
};

template <typename T, typename std::enable_if<std::is_pointer<T>::value,
                                              std::nullptr_t>::type = nullptr>
bool operator==(const List__<T> &listA, const List__<T> &listB) {
  auto iteratorCellA = std::begin(listA);
  auto iteratorCellB = std::end(listB);

  while (iteratorCellA != std::end(listA) && iteratorCellB != std::end(listB)) {
    if (!*iteratorCellA && !*iteratorCellB)
      continue;
    if (**iteratorCellA != **iteratorCellB) {
      return false;
    }

    iteratorCellA = std::next(iteratorCellA, 1);
    iteratorCellB = std::next(iteratorCellB, 1);
  }

  if (iteratorCellA == std::end(listA) && iteratorCellB != std::end(listB)) {
    return false;
  } else if (iteratorCellA != std::end(listA) &&
             iteratorCellB == std::end(listB)) {
    return false;
  } else {
    return true;
  }
}

template <typename T1, typename T2>
bool operator!=(const List__<T1> &listA, const List__<T2> &listB) {
  return !(listA == listB);
}

template <typename T, typename std::enable_if<std::is_pointer<T>::value,
                                              std::nullptr_t>::type = nullptr>
bool operator<(const List__<T> &listA, const List__<T> &listB) {
  auto iteratorCellA = listA.sentinel->next;
  auto iteratorCellB = listB.sentinel->next;

  while (iteratorCellA != listA.sentinel && iteratorCellB != listB.sentinel) {
    if (**iteratorCellA < **iteratorCellB)
      return true;
    if (**iteratorCellB < **iteratorCellA)
      return false;

    iteratorCellA = iteratorCellA->next;
    iteratorCellB = iteratorCellB->next;
  }

  if (iteratorCellA == listA.sentinel && iteratorCellB != listB.sentinel) {
    return true;
  } else if (iteratorCellA != listA.sentinel &&
             iteratorCellB == listB.sentinel) {
    return false;
  } else {
    return false;
  }
}

struct InheritedVertex;
using vertex_list = List__<InheritedVertex *>;

template <typename T, typename std::enable_if<std::is_pointer<T>::value,
                                              std::nullptr_t>::type = nullptr>
inline std::ostream &operator<<(std::ostream &os, const List__<T> &list) {
  auto sentinel = std::end(list);
  os << "[";
  for (auto iterator = std::begin(list); iterator != sentinel; ++iterator) {
    if (*iterator)
      os << **iterator;
    else
      os << "CLASS_SENTINEL\n";
    if (std::next(iterator, 1) != sentinel) {
      os << ",";
    }
  }
  os << "]";
  return os;
}
