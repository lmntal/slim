#ifndef _OMEGA_ARRAY_H
#define _OMEGA_ARRAY_H

#include "collection.hpp"
#include "element/element.h"
#include "limits.h"

#include <map>

namespace c17 = slim::element;

// #define OMEGA slim::element::monostate()
using OmegaArray = std::map<c17::variant<int, c17::monostate>, int>;

namespace omega_array {
using index_type = c17::variant<int, c17::monostate>;
const index_type OMEGA = c17::monostate();

inline void move_to_omega_larger_than(OmegaArray &body, int index) {
  int sum = 0;
  for (auto it = body.upper_bound(index); it != std::prev(body.end()); ++it)
    sum += it->second;

  body[omega_array::OMEGA] += sum;
  body.erase(body.upper_bound(index), std::prev(body.end()));
}

inline void clear_finite_larger_than(OmegaArray &body, int index) {
  if (body.empty()) return;
  body.erase(body.upper_bound(index), std::prev(body.end()));
}

inline index_type maxIndex(const OmegaArray &body) {
  for (auto it = body.rbegin(); it != body.rend(); ++it)
    if (it->second != 0)
      return it->first;
  return -1;
}
} // namespace omega_array

inline std::ostream &operator<<(std::ostream &os, OmegaArray &body) {
  os << "[";
  if(body.begin()!=body.end()) {
    for (auto it = body.begin(); it != std::prev(body.end()); ++it) {
      os << it->second << ",";
    }
    os << " 0, 0, 0,...," << std::prev(body.end())->second << "]";
  } else
    os << " 0, 0, 0,...,0" << "]";


  return os;
}

#endif
