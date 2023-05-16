/*
 * life_time.hpp
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
 * <lmntal@ueda.info.waseda.ac.jp> All rights reserved.
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

#ifndef SLIM_ELEMENT_LIFE_TIME_HPP
#define SLIM_ELEMENT_LIFE_TIME_HPP

#include "stack_trace.hpp"

#include "config.h" // for DEBUG macro

#include <map>
#ifdef DEBUG
#include <iostream>
#endif

namespace slim::element {

/**
 * A system for profiling object's lifetime.
 * Usage:
 *   Call LifetimeProfiler::check_memory_leak() after all objects are
 *   destructed. If any registered objects are not destructed (staying
 *   in a heap or a stack), the system prints stack traces when they are
 *   contsructed. To register objects to this system, call
 *   LifetimeProfiler::construct and LifetimeProfiler::destruct.
 *   For convenience, there is a class ProfileLifetime below. Inheriting
 *   the class enables us to automatically register objects of a derived
 *   class.
 */
class LifetimeProfiler {
  using backtrace_log = std::vector<std::string>;
  std::map<void *, backtrace_log> construction;
  std::map<void *, backtrace_log> destruction;

  LifetimeProfiler()  = default;
  ~LifetimeProfiler() = default;

  static LifetimeProfiler *getInstance() {
#ifdef DEBUG
    if (instance_) {
      return instance_;
    }
    return instance_ = new LifetimeProfiler();
#else
    return nullptr;
#endif
  }

public:
  static inline void construct(void *object) {
#ifdef DEBUG
    auto ths = getInstance();
    // called on already constructed & destructed object
    if (ths->reusing(object)) {
      ths->construction.erase(object);
      ths->destruction.erase(object);
    }

    // called on already constructed & not destructed object
    if (ths->construction.find(object) != ths->construction.end()) {
      std::cout << "Double-destruction detected on object (" << object << ")\n";
      std::cout << "First detsructed at\n";
      for (auto &s : ths->destruction[object])
        std::cout << "\t" << s << "\n";
      std::cout << "Second detsructed at\n";
      for (auto &s : slim::element::stack_trace::backtrace())
        std::cout << "\t" << s << "\n";
    }

    ths->construction[object] = slim::element::stack_trace::backtrace();
#endif
  }

  static inline void destruct(void *object) {
#ifdef DEBUG
    auto ths = getInstance();
    // called on already destructed object
    if (ths->destruction.find(object) != ths->destruction.end()) {
      std::cout << "Double-destruction detected on object (" << object << ")\n";
      std::cout << "First detsructed at\n";
      for (auto &s : ths->destruction[object])
        std::cout << "\t" << s << "\n";
      std::cout << "Second detsructed at\n";
      for (auto &s : slim::element::stack_trace::backtrace())
        std::cout << "\t" << s << "\n";
    }
    ths->destruction[object] = slim::element::stack_trace::backtrace();
#endif
  }

  static inline void check_memory_leak() {
#ifdef DEBUG
    auto ths = getInstance();
    for (auto &p : ths->construction) {
      auto &obj = p.first;
      if (ths->destruction.find(obj) != ths->destruction.end())
        continue;

      std::cout << "Memory leak detected on object (" << obj << ") constructed at\n";
      for (auto &s : p.second) {
        std::cout << "\t" << s << "\n";
      }
    }
#endif
  }

private:
  bool reusing(void *object) const {
    return construction.find(object) != construction.end() && destruction.find(object) != destruction.end();
  }

#ifdef DEBUG
  static LifetimeProfiler *instance_;
#endif
};

class profile_lifetime {
public:
  profile_lifetime() { LifetimeProfiler::construct(this); }
  ~profile_lifetime() { LifetimeProfiler::destruct(this); }
};

} // namespace slim::element

#endif
