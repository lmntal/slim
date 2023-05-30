/*
 * symbol.cpp - mapping symbol names to their id
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
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
 *
 * $Id: symbol.c,v 1.4 2008/09/29 05:23:40 taisuke Exp $
 */

#include "symbol.h"

#include <cstddef>
#include <mutex>
#include <string>
#include <vector>

#include "ankerl/unordered_dense.hpp"
#include "element/element.h"
#include "element/map_utils.hpp"
#include "lmntal.h"

using SymbolRevMap = ankerl::unordered_dense::map<lmn_interned_str, std::string>;
using SymbolMap    = mapStrKey<lmn_interned_str>;
using NextSymbolId = std::vector<lmn_interned_str>;

static SymbolMap    sym_tbl{};
static SymbolRevMap sym_rev_tbl{};
static NextSymbolId next_sym_id{};
static std::mutex   sym_mtx{};

/* prototypes */

void             sym_tbl_init();
void             sym_tbl_destroy();
lmn_interned_str create_new_id();

void sym_tbl_init() {
  auto n = lmn_env.core_num;
  next_sym_id.resize(n);
  for (auto i = 0; i < n; i++) {
    next_sym_id[i] = i + 1; /* 0はIDに使わない */
  }
}

void sym_tbl_destroy() {}

lmn_interned_str create_new_id() {
  auto cid         = env_my_thread_id();
  auto new_id      = next_sym_id[cid];
  next_sym_id[cid] += env_threads_num();
  return new_id;
}

lmn_interned_str lmn_intern(std::string_view name) {
  /* すでにnameに対応する値があるならそれを返す */
  if (auto it = sym_tbl.find(name); it != sym_tbl.end())
    return it->second;

  /* 新しいIDを作る */
  auto        new_id = create_new_id();
  std::string name2{name};

  if (env_threads_num() >= 2) [[likely]] {
    std::lock_guard<std::mutex> lock{sym_mtx};
    sym_tbl[name2]      = new_id;
    sym_rev_tbl[new_id] = name2;
  } else [[unlikely]] {
    sym_tbl[name2]      = new_id;
    sym_rev_tbl[new_id] = name2;
  }

  return new_id;
}

std::string_view lmn_id_to_name(lmn_interned_str id) {
  if (id == ANONYMOUS)
    return "";
  if (auto it = sym_rev_tbl.find(id); it != sym_rev_tbl.end())
    return it->second;
  return {};
}

size_t count_symbols() { return sym_tbl.size() + 1; /* symbol 0 is out of table */ }
