/*
 * variant.hpp
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

#ifndef SLIM_ELEMENT_VARINT_HPP
#define SLIM_ELEMENT_VARINT_HPP

#include <memory>
#include <type_traits>
#include <utility>

namespace slim {
namespace element {

template <typename...> struct variant_storage_info;

template <typename T, typename... Types>
struct variant_storage_info<T, Types...> : variant_storage_info<Types...> {
  using base_type = variant_storage_info<Types...>;
  static constexpr auto size =
      (base_type::size > sizeof(T)) ? base_type::size : sizeof(T);
  static constexpr auto align =
      (base_type::align > alignof(T)) ? base_type::align : alignof(T);
};

template <typename Z> struct variant_storage_info<Z> {
  static constexpr auto size = sizeof(Z);
  static constexpr auto align = alignof(Z);
};

template <typename... Types>
using variant_storage =
    typename std::aligned_storage<variant_storage_info<Types...>::size,
                                  variant_storage_info<Types...>::align>::type;

template <typename T, typename U, typename... Types>
static constexpr auto variant_type_index() -> typename std::enable_if<
    std::is_same<typename std::decay<T>::type, U>::value, size_t>::type {
  return 0;
}

template <typename T, typename U, typename... Types>
static constexpr auto variant_type_index() -> typename std::enable_if<
    !std::is_same<typename std::decay<T>::type, U>::value, size_t>::type {
  return 1 + variant_type_index<T, Types...>();
}

template <typename T, typename U, typename... Types>
static auto variant_store(void *p, T &&t) -> typename std::enable_if<
    std::is_same<U, typename std::decay<T>::type>::value>::type {
  ::new (p) typename std::decay<T>::type(std::forward<U>(t));
}

template <typename T, typename U, typename... Types>
static auto variant_store(void *p, T &&t) -> typename std::enable_if<
    !std::is_same<U, typename std::decay<T>::type>::value>::type {
  variant_store<T, Types...>(p, std::forward<T>(t));
}

template <typename T, typename U, typename... Types>
static auto variant_store(void *p, const T &t) -> typename std::enable_if<
    std::is_same<U, typename std::decay<T>::type>::value && std::is_copy_constructible<U>::value>::type {
  ::new (p) typename std::decay<T>::type(t);
}

template <typename T, typename U, typename... Types>
static auto variant_store(void *p, const T &t) -> typename std::enable_if<
    !std::is_same<U, typename std::decay<T>::type>::value>::type {
  variant_store<T, Types...>(p, t);
}

template <typename... Types> struct variant;

template <typename T, typename... Types> T &get(variant<Types...> &v) {
  return *static_cast<T *>(static_cast<void *>(&v.storage_));
}

template <typename T, typename... Types>
const T &get(const variant<Types...> &v) {
  return *static_cast<const T *>(static_cast<const void *>(&v.storage_));
}

template <size_t I, typename Variant>
auto get(Variant &&v)
    -> decltype(get<typename std::tuple_element<
                    I, typename std::decay<Variant>::type::type_tuple>::type>(
        std::forward<Variant>(v))) {
  return get<typename std::tuple_element<
      I, typename std::decay<Variant>::type::type_tuple>::type>(
      std::forward<Variant>(v));
}

template <size_t I, typename Visitor, typename Variant,
          typename TypeTuple = typename std::decay<Variant>::type::type_tuple,
          typename std::enable_if<(I == std::tuple_size<TypeTuple>::value - 1),
                                  std::nullptr_t>::type = nullptr>
constexpr auto visit_impl(Visitor &&vis, Variant &&var)
    -> decltype(vis(get<I>(std::forward<Variant>(var)))) {
  return vis(get<I>(std::forward<Variant>(var)));
}

template <size_t I, typename Visitor, typename Variant,
          typename TypeTuple = typename std::decay<Variant>::type::type_tuple,
          typename std::enable_if<(I < std::tuple_size<TypeTuple>::value - 1),
                                  std::nullptr_t>::type = nullptr>
constexpr auto visit_impl(Visitor &&vis, Variant &&var)
    -> decltype(vis(get<I>(std::forward<Variant>(var)))) {
  return (var.index() == I) ? vis(get<I>(std::forward<Variant>(var)))
                            : visit_impl<I + 1>(std::forward<Visitor>(vis),
                                                std::forward<Variant>(var));
}

template <typename Visitor, typename Variant>
auto visit(Visitor &&vis, Variant &&var)
    -> decltype(visit_impl<0>(std::forward<Visitor>(vis),
                              std::forward<Variant>(var))) {
  return visit_impl<0>(std::forward<Visitor>(vis), std::forward<Variant>(var));
}

inline bool lor() {
  return false;
}

template <class T, class... U>
constexpr bool lor(const T &tf, const U&... l) {
  return tf || lor(l...);
}

template <typename... Types> struct variant {
  using type_tuple = std::tuple<Types...>;
  struct deleter {
    template <typename T> void operator()(T &t) { t.~T(); }
  };

  struct loader {
    void *p;
    loader(void *storage) : p(storage) {}
    template <typename T> void operator()(T &&t) {
      variant_store<T, Types...>(p, std::forward<T>(t));
    }
    template <typename T> void operator()(const T &t) {
      variant_store<T, Types...>(p, t);
    }
  };

  struct equalizer {
    const slim::element::variant<Types...> &v;
    equalizer(const slim::element::variant<Types...> &v) : v(v) {}
    template <typename T> bool operator()(const T &w) {
      return slim::element::get<T>(v) == w;
    }
  };

  struct comparator {
    const slim::element::variant<Types...> &v;
    comparator(const slim::element::variant<Types...> &v) : v(v) {}
    template <typename T> bool operator()(const T &w) {
      return slim::element::get<T>(v) < w;
    }
  };

  variant() {
    using T = typename std::tuple_element<0, type_tuple>::type;
    variant_store<T, Types...>(&storage_, T());
    index_ = variant_type_index<T, Types...>();
  }

  variant(const variant &other) {
    visit(loader(&storage_), other);
    index_ = other.index();
  }

  variant(variant &&other) noexcept {
    visit(loader(&storage_), std::move(other));
    index_ = other.index();
  }
  template <typename T,
            typename std::enable_if<
                !std::is_same<typename std::decay<T>::type, variant>::value && lor(std::is_constructible<Types, T>::value...),
                std::nullptr_t>::type = nullptr>
  variant(T &&t) {
    variant_store<T, Types...>(&storage_, std::forward<T>(t));
    index_ = variant_type_index<T, Types...>();
  }
  variant &operator=(const variant &rhs) {
    visit(loader(&storage_), rhs);
    index_ = rhs.index();
    return *this;
  }
  template <typename T> variant &operator=(T &&t) {
    visit(deleter(), *this);
    variant_store<T, Types...>(&storage_, std::forward<T>(t));
    index_ = variant_type_index<T, Types...>();
    return *this;
  }

  ~variant() { visit(deleter(), *this); }

  size_t index() const { return index_; }

  size_t index_;
  variant_storage<Types...> storage_;
};

template <class T, class... Types>
constexpr bool holds_alternative(const variant<Types...> &v) noexcept {
  return variant_type_index<T, Types...>() == v.index();
}

struct monostate {};

constexpr bool operator<(const monostate &, const monostate &) noexcept { return false; }
constexpr bool operator>(const monostate &, const monostate &) noexcept { return false; }
constexpr bool operator<=(const monostate &, const monostate &) noexcept { return true; }
constexpr bool operator>=(const monostate &, const monostate &) noexcept { return true; }
constexpr bool operator==(const monostate &, const monostate &) noexcept { return true; }
constexpr bool operator!=(const monostate &, const monostate &) noexcept { return false; }

template <class... Types>
constexpr bool operator==(const slim::element::variant<Types...> &v,
                          const slim::element::variant<Types...> &w) {
  return (v.index() != w.index())
             ? false
             : slim::element::visit(
                   typename slim::element::variant<Types...>::equalizer(v), w);
}

template <class T, class... Types>
constexpr bool operator==(const slim::element::variant<Types...> &v, const T &w) {
  return (v.index() != slim::element::variant_type_index<T, Types...>())
             ? false
             : slim::element::get<T>(v) == w;
}

template <class... Types>
constexpr bool operator!=(const slim::element::variant<Types...> &v,
                          const slim::element::variant<Types...> &w) {
  return !(v == w);
}

template <class T, class... Types>
constexpr bool operator!=(const slim::element::variant<Types...> &v, const T &w) {
  return !(v == w);
}

template <class... Types>
constexpr bool operator<(const slim::element::variant<Types...> &v,
                         const slim::element::variant<Types...> &w) {
  return v.index() < w.index() ||
         (v.index() == w.index() &&
          slim::element::visit(
              typename slim::element::variant<Types...>::comparator(v), w));
}

} // namespace element
} // namespace slim

#endif /* SLIM_ELEMENT_VARINT_HPP */
