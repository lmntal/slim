#pragma once

#include "ankerl/unordered_dense.hpp"

#include "verifier/state.hpp"

struct StateHash {
  using is_transparent = void; // enable heterogeneous overloads
  using is_avalanching = void; // mark class as high quality avalanching hash

  [[nodiscard]] auto operator()(State *s) const noexcept -> uint64_t { return s->hash * (state_property_state(s) + 1); }
};

struct StateEq {
  [[nodiscard]] auto operator()(State *a, State *b) const noexcept -> bool { return !state_cmp_with_compress(a, b); }
};

using StateMap = ankerl::unordered_dense::map<State *, State *, StateHash, StateEq>;
using StateSet = ankerl::unordered_dense::set<State *, StateHash, StateEq>;

template <typename Key, typename Value, typename Func, typename... Args>
auto for_each(ankerl::unordered_dense::map<Key, Value> &map, Func &&f, Args &&...args) {
  for (auto &kv : map) {
    f(kv.first, kv.second, std::forward<Args>(args)...);
  }
}