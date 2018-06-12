
#ifndef PSD_SSD_DISJOINT_SET_HPP
#define PSD_SSD_DISJOINT_SET_HPP

#include <map>
#include <set>

namespace psr_gsd {
template <typename Key> class disjoint_set {
  std::map<Key, Key> parent;

public:
  Key find(Key k) {
    if (parent.find(k) == parent.end())
      return Key();
    auto p = parent[k];
    if (p == k)
      return p;
    return (parent[k] = find(p));
  }
  void unite(Key a, Key b) {
    auto ra = find(a);
    auto rb = find(b);
    if (ra != rb)
      parent[ra] = rb;
  }
  void insert(Key a) {
    if (parent.find(a) == parent.end())
      parent[a] = a;
  }

  std::map<Key, std::set<Key>> sets() {
    std::map<Key, std::set<Key>> res;
    for (auto &p : parent)
      res[find(p.second)].insert(p.first);
    return res;
  }
};
} // namespace psr_gsd

#endif /* PSD_SSD_DISJOINT_SET_HPP */
