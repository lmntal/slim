/*
 * upe.hpp
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
 *   All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions are
 *   met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *
 *    3. Neither the name of the Ueda Laboratory LMNtal Group nor the
 *       names of its contributors may be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef VERIFIER_UPE_HPP
#define VERIFIER_UPE_HPP

#include "vm/vm.h"

namespace slim {
namespace verifier {
namespace upe {
using node = LmnSymbolAtomRef;
using matching_pair = std::pair<node, node>;

class matching_set {
  /// assert(a.size() == b.size());
  std::vector<node> a;
  std::vector<node> b;

 public:
  void insert(const matching_pair &p) {
    if (std::find(std::begin(a), std::end(a), p.first) != std::end(a) ||
        std::find(std::begin(b), std::end(b), p.first) != std::end(b))
      return;
    a.push_back(p.first);
    b.push_back(p.second);
  }

  template <typename InputIterator>
  void insert(InputIterator first, InputIterator last) {
    for (auto it = first; it != last; ++it) insert(*it);
  }

  size_t size() const { return a.size(); }
  bool empty() const { return a.empty(); }

  class iterator {
    matching_set *set;
    int idx;

   public:
    using iterator_category = std::input_iterator_tag;
    using value_type = std::pair<node, node>;
    using difference_type = std::ptrdiff_t;
    using pointer = std::pair<node, node> *;
    using reference = std::pair<node, node>;

    iterator(matching_set *set, int idx) : set(set), idx(idx) {}
    iterator(const iterator &it) : set(it.set), idx(it.idx) {}
    const iterator &operator=(const iterator &it) {
      set = it.set;
      idx = it.idx;
      return it;
    }
    ~iterator() noexcept = default;

    reference operator*() const { return {set->a[idx], set->b[idx]}; }
    iterator &operator++() {
      idx++;
      if (idx >= set->size()) idx = set->size();
      return *this;
    }
    iterator operator++(int i) {
      auto it = *this;
      ++(*this);
      return it;
    }

    bool operator!=(const iterator &a) { return !(*this == a); }
    bool operator==(const iterator &a) { return set == a.set && idx == a.idx; }
  };

  iterator begin() { return iterator(this, 0); }
  iterator end() { return iterator(this, size()); }
};

matching_set match_connected_process(LmnSymbolAtomRef p, LmnSymbolAtomRef q,
                                     matching_set r = matching_set()) {
  if (LMN_SATOM_GET_FUNCTOR(p) != LMN_SATOM_GET_FUNCTOR(q))
    return matching_set();
  if (std::find(std::begin(r), std::end(r), std::make_pair(p, q)) !=
      std::end(r))
    return r;

  r.insert({p, q});
  for (auto i = 0; i < LMN_SATOM_GET_LINK_NUM(p); i++) {
    if (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(p, i)) ||
        LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(q, i)))
      continue;
    auto pi = reinterpret_cast<LmnSymbolAtomRef>(LMN_SATOM_GET_LINK(p, i));
    auto qi = reinterpret_cast<LmnSymbolAtomRef>(LMN_SATOM_GET_LINK(q, i));
    auto pip = LMN_SATOM_GET_ATTR(p, i);
    auto qip = LMN_SATOM_GET_ATTR(q, i);
    if (LMN_SATOM_GET_FUNCTOR(pi) == LMN_SATOM_GET_FUNCTOR(qi) && pip == qip) {
      auto s = match_connected_process(pi, qi, r);
      r.insert(std::begin(s), std::end(s));
    }
  }
  return r;
}

}  // namespace upe
}  // namespace verifier
}  // namespace slim

#endif /* VERIFIER_UPE_HPP */
