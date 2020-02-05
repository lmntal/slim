/*
 * symbol_atom_range.hpp
 *
 *   Copyright (c) 2020, Ueda Laboratory LMNtal Group
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

#ifndef SLIM_VM_SYMBOL_ATOM_RANGE_HPP
#define SLIM_VM_SYMBOL_ATOM_RANGE_HPP

#include "atom.h"
#include "atomlist.hpp"
#include "membrane.hpp"

/**
 * 膜の中のシンボルアトムを列挙する。
 * ファンクタの降順にアトムを返し、プロキシアトムは含まない。
 * 初期化遅延により余計な中間データを生成せずに列挙できる。
 * 列挙中に膜のシンボルアトムが変更された場合の動作は保証しない。
 */
class symbol_atom_range {
  class symbol_atom_iterator {
    const LmnMembrane *mem;
    int functor;
    AtomListEntry::const_iterator iter, iter_end;
    
    symbol_atom_iterator() : functor(-1) {}
    
  public:
    using difference_type = intptr_t;
    using value_type = LmnSymbolAtomRef;
    using pointer = LmnSymbolAtomRef *;
    using reference = LmnSymbolAtomRef &;
    typedef typename std::input_iterator_tag iterator_category;
    
    static symbol_atom_iterator begin(LmnMembrane *mem) {
      symbol_atom_iterator result;
      for (int i = (int)mem->mem_max_functor() - 1; i >= 0; i--) {
        auto ent = mem->get_atomlist(i);
        if (ent && !LMN_IS_PROXY_FUNCTOR(i) && ent->begin() != ent->end()) {
          result.mem = mem;
          result.functor = i;
          result.iter = ent->begin();
          result.iter_end = ent->end();
          break;
        }
      }
      return result;
    }
    
    static symbol_atom_iterator end(LmnMembrane *mem) {
      symbol_atom_iterator result;
      result.mem = mem;
      result.functor = -1;
      return result;
    }
    
    symbol_atom_iterator &operator++() {
      ++iter;
      while (iter == iter_end && functor >= 0) {
        functor--;
        for (; functor >= 0; functor--) {
          
          auto ent = mem->get_atomlist(functor);
          if (ent && !LMN_IS_PROXY_FUNCTOR(functor)) {
            iter = ent->begin();
            iter_end = ent->end();
            break;
          }
        }
      }
      return *this;
    }
    symbol_atom_iterator operator++(int) {
      auto ret = *this;
      ++ret;
      return ret;
    }
    LmnSymbolAtomRef &operator*() { return *this->iter; };
    const LmnSymbolAtomRef &operator*() const { return *this->iter; };
    
    bool operator!=(const symbol_atom_iterator &itr) const {
      return !(*this == itr);
    };
    bool operator==(const symbol_atom_iterator &itr) const {
      if (functor < 0)
        return functor == itr.functor;
      return iter == itr.iter;
    };
  };
  
  LmnMembrane *mem;
  
public:
  symbol_atom_range(LmnMembrane *mem) : mem(mem) {}
  
  symbol_atom_iterator begin() const { return symbol_atom_iterator::begin(mem); }
  symbol_atom_iterator end() const { return symbol_atom_iterator::end(mem); }
};

#endif /* SLIM_VM_SYMBOL_ATOM_RANGE_HPP */
