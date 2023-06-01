/*
 * normal_thread.cpp
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id$
 */

#include "normal_thread.h"

#include <deque>
#include <functional>
#include <mutex>
#include <vector>

#include "interpret/interpreter.hpp"
#include "verifier/runtime_status.h"

std::vector<std::thread> findthread;
arginfo                **thread_info;
int                      active_thread;
std::deque<int>         *temp;
double                   walltime; // rule walltime
double                   walltime_temp;
BOOL                     normal_parallel_flag;
unsigned long            success_temp_check;
unsigned long            fail_temp_check;

namespace {
auto each_atom_thread_opt(LmnSAtom value, AtomListEntryRef entry, int id, int num, LmnSAtom start,
                          std::function<bool(LmnSAtom)> const &func) {
  auto       flag{1};
  auto const ID{id};
  if (!entry)
    return;
  if (start == nullptr) {
    value = atomlist_head(entry);
    flag--;
  } else {
    value = start;
  }
  for (; value != lmn_atomlist_end(entry) || flag; value = ((LmnSymbolAtomRef)value)->get_next()) {
    if (value == lmn_atomlist_end(entry)) {
      value = atomlist_head(entry);
      id    = ID;
      flag--;
    }
    if (((LmnSymbolAtomRef)(value))->get_functor() != LMN_RESUME_FUNCTOR && id == 0) {
      if (func(value))
        break;
      id = num;
    }
    id--;
  }
}
} // namespace

void *normal_thread(void *arg) {
  LmnSAtom atom{nullptr};
  double   start_time, stop_time;
  auto    *idp         = (int *)arg;
  auto    *thread_data = thread_info[*idp];

  while (true) {
    op_lock(thread_data->id, 1);
    if (!lmn_env.enable_parallel)
      break;
    thread_data->profile->wakeup++;
    if (lmn_env.profile_level >= 1)
      start_time = get_cpu_time();

    each_atom_thread_opt(atom, thread_data->atomlist_ent, thread_data->id, active_thread, thread_data->next_atom,
                         [thread_data, thread_instr = thread_data->instr](LmnSAtom v) {
                           slim::vm::interpreter it(thread_data->rc, thread_data->rule, thread_instr);
                           thread_data->rc->reg(thread_data->atomi) = {(LmnWord)v, LMN_ATTR_MAKE_LINK(0), TT_ATOM};
                           if (rc_hlink_opt(thread_data->atomi, thread_data->rc)) {
                             auto *spc = thread_data->rc->get_hl_sameproccxt()->at(thread_data->atomi);
                             if (spc->is_consistent_with((LmnSymbolAtomRef)thread_data->rc->wt(thread_data->atomi))) {
                               spc->match((LmnSymbolAtomRef)thread_data->rc->wt(thread_data->atomi));
                               if (it.interpret(thread_data->rc, thread_data->rule, thread_instr)) {
                                 thread_data->judge = TRUE;
                                 thread_data->profile->findatom_num++;
                                 return true;
                               }
                             }
                           } else {
                             if (it.interpret(thread_data->rc, thread_data->rule, thread_instr)) {
                               thread_data->judge = TRUE;
                               thread_data->profile->findatom_num++;
                               return true;
                             }
                           }
                           if (lmn_env.find_atom_parallel)
                             return true;
                           thread_data->backtrack++;
                           return false;
                         });
    thread_data->next_atom = atom;
    if (lmn_env.profile_level >= 1) {
      stop_time                                       = get_cpu_time();
      lmn_prof.thread_cpu_time_main[thread_data->id] += stop_time - start_time;
    }
    thread_data->exec.unlock();
  }
  return nullptr;
}

void normal_parallel_init() {
  int i;
  findthread  = std::vector<std::thread>(lmn_env.core_num);
  thread_info = LMN_NALLOC<arginfo *>(lmn_env.core_num);
  for (i = 0; i < lmn_env.core_num; i++) {
    thread_info[i] = new arginfo(i);
  }
  for (i = 0; i < lmn_env.core_num; i++) {
    findthread[i] = std::thread(normal_thread, &(thread_info[i]->id));
  }
  temp               = new std::deque<int>();
  walltime           = 0;
  success_temp_check = 0;
  fail_temp_check    = 0;
}

void normal_parallel_free() {
  int i;
  lmn_env.enable_parallel = FALSE;
  for (i = 0; i < lmn_env.core_num; i++) {
    thread_info[i]->exec.unlock();
    findthread[i].join();
    delete (thread_info[i]->rc);
    lmn_free(thread_info[i]->profile);
    lmn_free(thread_info[i]);
  }
  lmn_env.enable_parallel = TRUE;
  lmn_free(thread_info);
  delete temp;
}

void threadinfo_init(int id, LmnInstrVar atomi, LmnRuleRef rule, LmnReactCxtRef rc, LmnRuleInstr instr,
                     AtomListEntryRef atomlist_ent, int atom_arity) {
  //
  thread_info[id]->judge     = FALSE;
  thread_info[id]->atomi     = atomi;
  thread_info[id]->rule      = rule;
  thread_info[id]->backtrack = 0;
  react_context_copy(thread_info[id]->rc, rc);
  if (thread_info[id]->register_size < rc->capacity()) {
    thread_info[id]->rc->resize(rc->capacity());
    thread_info[id]->register_size = rc->capacity();
  }
  thread_info[id]->rc->work_array = rc->work_array, thread_info[id]->instr = instr;
  thread_info[id]->atomlist_ent = atomlist_ent;
  thread_info[id]->atom_arity   = atom_arity;
}
void op_lock(int id, int flag) {
  while (thread_info[id]->exec_flag != flag)
    ;
  thread_info[id]->exec.lock();
  thread_info[id]->exec_flag = 1 - thread_info[id]->exec_flag;
}

void normal_parallel_prof_dump(FILE *f) {
  // parallel pattern matching profile
  unsigned long findatom_num{0};
  fprintf(f, "\n===Parallel Pattern Matching Profile========================\n");
  fprintf(f, "[id]: [wakeup] [backtrack] [findatom]\n");
  for (auto i = 0; i < lmn_prof.thread_num; i++) {
    fprintf(f, "%3d : %8lu %11lu %10lu \n", i, thread_info[i]->profile->wakeup, thread_info[i]->profile->backtrack_num,
            thread_info[i]->profile->findatom_num);
    findatom_num += thread_info[i]->profile->findatom_num;
  }
  fprintf(f, "\nfindatom num:%16lu \n", findatom_num);
  fprintf(f, "Success Check:%15lu \n", success_temp_check);
  fprintf(f, "Fail Check:%18lu \n", fail_temp_check);
  fprintf(f, "Main Rule Wall Time:%9.2lf\n", walltime);
  fprintf(f, "============================================================\n");
}

BOOL check_exist(LmnSymbolAtomRef atom, LmnFunctor f) {
  if (!atom)
    return FALSE;
  if (atom->get_functor() != f)
    return FALSE;
  return TRUE;
}

void rule_wall_time_start() {
  normal_parallel_flag = FALSE;
  walltime_temp        = get_wall_time();
}

void rule_wall_time_finish() {
  double finish;
  finish    = get_wall_time();
  walltime += finish - walltime_temp;
}
