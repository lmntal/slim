#ifndef _UNION_FIND_H
#define _UNION_FIND_H
#include "mckay.hpp"
#include <map>
#include <vector>

struct UnionFind {
  std::vector<int> par;
  std::map<int, int> idmap;
  UnionFind(propagation_list &pList) {
    int cnt = 0;
    for (auto &list : pList) {
      for (auto vertex : list) {
        idmap[vertex->ID] = cnt;
        cnt++;
      }
    }
  }

  int root(int x) {
    if (par[x] < 0)
      return x;
    else
      return par[x] = root(par[x]);
  }

  bool issame(ConvertedGraphVertex *x, ConvertedGraphVertex *y) {
    return root(idmap[x->ID]) == root(idmap[y->ID]);
  }

  bool merge(ConvertedGraphVertex *x, ConvertedGraphVertex *y) {
    int idx = idmap[x->ID];
    int idy = idmap[y->ID];
    idx = root(idx);
    idy = root(idy);
    if (idx == idy)
      return false;
    if (par[idx] > par[idy])
      std::swap(idx, idy);
    par[idx] += par[idy];
    par[idy] = idx;
    return true;
  }

  int size(ConvertedGraphVertex *x) { return -par[root(idmap[x->ID])]; }
};
#endif
