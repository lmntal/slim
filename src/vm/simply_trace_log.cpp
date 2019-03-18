/*
 * simply_trace_log.cpp
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
 *
 */

#include "simply_trace_log.h"

#include "simply_process_table.h"
#include "trace_log.h"

class SimpleTraceLog {
  SimpleProcessTable
      tbl; /* Process IDをkey, 訪問済みか否かの真偽値をvalueとしたテーブル */
  LogTracker tracker;

public:
  SimpleTraceLog() {}
  SimpleTraceLog(unsigned long size) : tbl(size) {}

  void visit(LmnWord key) {
    tracker.trace(key);
    tbl.put(key, true);
  }

  void leave(LmnWord key) { tbl.unput(key); }

  template <typename T> bool is_visited(T key) { return tbl.contains(key); }

  void backtrack() { tracker.revert(this); }

  void set_btpoint() { tracker.push(); }

  void continue_trace() { tracker.pop(); }
};

/*------------
 * SimpleTraceLog
 */

SimplyLog simplylog_make() { return new SimpleTraceLog; }

SimplyLog simplylog_make_with_size(unsigned long size) {
  return new SimpleTraceLog(size);
}

void simplylog_free(SimplyLog s) { delete s; }

void simplylog_put(SimplyLog l, LmnWord key) { l->visit(key); }

void simplylog_put_atom(SimplyLog l, LmnSymbolAtomRef atom) {
  simplylog_put(l, atom->get_id());
}

void simplylog_put_mem(SimplyLog l, LmnMembraneRef mem) {
  simplylog_put(l, lmn_mem_id(mem));
}

BOOL simplylog_contains_atom(SimplyLog l, LmnSymbolAtomRef atom) {
  return l->is_visited(atom);
}

BOOL simplylog_contains_mem(SimplyLog l, LmnMembraneRef mem) {
  return l->is_visited(mem);
}

void simplylog_backtrack(SimplyLog l) { l->backtrack(); }

void simplylog_set_btpoint(SimplyLog l) { l->set_btpoint(); }

void simplylog_continue_trace(SimplyLog l) { l->continue_trace(); }
