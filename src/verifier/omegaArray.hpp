#ifndef _OMEGA_ARRAY_H
#define _OMEGA_ARRAY_H

#include "collection.hpp"
#include "element/element.h"
#include "limits.h"
#include <climits>
#include <map>

namespace c17 = slim::element;

// #define OMEGA slim::element::monostate()
using OmegaArray = std::map<int, int>;

namespace omega_array {
  using index_type = int;
  const int OMEGA = std::numeric_limits<int>::max();

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

inline int maxIndex(const OmegaArray &body) {
  for (auto it = body.rbegin(); it != body.rend(); ++it)
    if (it->second != 0)
      return it->first;
  return -1;
}
} // namespace omega_array

inline std::ostream &operator<<(std::ostream &os, const OmegaArray &body) {
  os << "[";
  int end_index = omega_array::maxIndex(body);
  if(end_index >= 0 and end_index != omega_array::OMEGA) {
    for(int i=0; i <= end_index; i++) {
      auto x = body.find(i);
      if(x != body.end()) {
	os << " " <<x->second << ",";
      } else {
	os << " 0,";
      }
    }
  }
  auto o = body.find(omega_array::OMEGA);
  if(o != body.end())
    os << " 0, 0, 0,..., " << o->second << "]";
  else
    os << " 0, 0, 0,..., 0" <<  "]";
  // for (auto it = body.begin(); it != body.end(); ++it) {
  //   os << it->second << ",";
  // }


  return os;
}

#endif
