/*
 * range_remove_if.hpp
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
 *                                          <lmntal@ueda.info.waseda.ac.jp>
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

#include <iterator>

namespace slim {
namespace element {

template <typename Function, typename InputIterator> struct range_remove_if {
  struct remove_if_iterator {
    using difference_type =
        typename std::iterator_traits<InputIterator>::difference_type;
    using value_type = typename std::iterator_traits<InputIterator>::value_type;
    using pointer = typename std::iterator_traits<InputIterator>::pointer;
    using reference = typename std::iterator_traits<InputIterator>::reference;
    using iterator_category = typename std::input_iterator_tag;

    remove_if_iterator(InputIterator iterator, InputIterator end_, Function f)
        : it(iterator), end_(end_), f(f) {}
    remove_if_iterator(const remove_if_iterator &i)
        : it(i.it), end_(i.end_), f(i.f) {}

    reference operator*() { return *it; }
    remove_if_iterator &operator++() {
      do
        ++it;
      while (it != end_ && f(*it));
      return *this;
    }
    pointer operator->() { return it; }
    remove_if_iterator operator++(int i_) {
      auto i = *this;
      ++i;
      return i;
    }

    bool operator==(const remove_if_iterator &i) const { return i.it == it; }
    bool operator!=(const remove_if_iterator &i) const { return !(i.it == it); }

  private:
    InputIterator it;
    InputIterator end_;
    Function f;
  };

  using difference_type =
      typename std::iterator_traits<remove_if_iterator>::difference_type;
  using value_type =
      typename std::iterator_traits<remove_if_iterator>::value_type;
  using pointer = typename std::iterator_traits<remove_if_iterator>::pointer;
  using reference =
      typename std::iterator_traits<remove_if_iterator>::reference;
  using iterator_category =
      typename std::iterator_traits<remove_if_iterator>::iterator_category;

  range_remove_if(InputIterator begin_, InputIterator end_, const Function &f)
      : begin_(begin_), end_(end_), f(f) {}

  remove_if_iterator begin() {
    if (begin_ == end_) return end();
    auto r = remove_if_iterator(begin_, end_, f);
    return !f(*begin_) ? r : ++r;
  }
  remove_if_iterator end() { return remove_if_iterator(end_, end_, f); }

private:
  InputIterator begin_;
  InputIterator end_;
  Function f;
};

template <typename Function, typename InputIterator>
range_remove_if<Function, InputIterator>
make_range_remove_if(InputIterator begin_, InputIterator end_,
                     const Function &f) {
  return range_remove_if<Function, InputIterator>(begin_, end_, f);
}

} // namespace element
} // namespace slim
