#pragma once

#include "ankerl/unordered_dense.hpp"

struct string_hash {
  using is_transparent = void; // enable heterogeneous overloads
  using is_avalanching = void; // mark class as high quality avalanching hash

  [[nodiscard]] auto operator()(std::string_view str) const noexcept -> uint64_t {
    return ankerl::unordered_dense::hash<std::string_view>{}(str);
  }
};

template<typename T>
using mapStrKey = ankerl::unordered_dense::map<std::string, T, string_hash, std::equal_to<>>;