/*
 * simply_trace_log.c
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 *
 */

#include "simply_trace_log.h"


/*------------
 * SimplyTraceLog
 */

void simplylog_init(SimplyLog s)
{
  simplylog_init_with_size(s, PROC_TBL_DEFAULT_SIZE);
}

void simplylog_init_with_size(SimplyLog s, unsigned long size)
{
  sproc_tbl_init_with_size(&s->tbl, size);
  tracker_init(&s->tracker);
}

void simplylog_destroy(SimplyLog s)
{
  sproc_tbl_destroy(&s->tbl);
  tracker_destroy(&s->tracker);
}

void simplylog_put(SimplyLog l, LmnWord key)
{
  LogTracker_TRACE(&l->tracker, key);
  sproc_tbl_put(&l->tbl, key, STRACE_TRUE);
}

void simplylog_put_atom(SimplyLog l, LmnSymbolAtomRef atom) {
  simplylog_put(l, LMN_SATOM_ID(atom));
}

void simplylog_put_mem(SimplyLog l, LmnMembraneRef mem) {
  simplylog_put(l, lmn_mem_id(mem));
}

BOOL simplylog_contains_atom(SimplyLog l, LmnSymbolAtomRef atom) {
  return sproc_tbl_contains_atom(&l->tbl, atom);
}

BOOL simplylog_contains_mem(SimplyLog l, LmnMembraneRef mem) {
  return sproc_tbl_contains_mem(&l->tbl, mem);
}

void simplylog_backtrack(SimplyLog l) {
  LogTracker_REVERT(&l->tracker, sproc_tbl_unput, &l->tbl);
}

void simplylog_set_btpoint(SimplyLog l) {
  LogTracker_PUSH(&l->tracker);
}

void simplylog_continue_trace(SimplyLog l) {
  LogTracker_POP(&l->tracker);
}
