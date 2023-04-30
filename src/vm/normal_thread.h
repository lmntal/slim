/*
 * normal_thread.h
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

#ifndef NORMAL_THREAD_H
#define NORMAL_THREAD_H

#include "element/element.h"
#include "lmntal.h"
#include "vm/vm.h"

struct normal_prof {
  unsigned long wakeup;
  unsigned long backtrack_num;
  unsigned long findatom_num;
};

struct arginfo {
  int id;     // thread id
  BOOL judge; // whether react atom or not
  LmnInstrVar atomi;
  LmnReactCxtRef rc;
  LmnRuleRef rule;
  LmnRuleInstr instr;
  AtomListEntryRef atomlist_ent;
  unsigned int register_size;
  int atom_arity;
  pthread_mutex_t *exec;
  volatile int exec_flag;
  unsigned long backtrack;
  LmnSAtom next_atom;

  normal_prof *profile;
};
extern pthread_t *findthread;
extern arginfo **thread_info;
extern int active_thread;
extern Deque *temp;
extern double walltime; // rule walltime
extern double walltime_temp;
extern BOOL normal_parallel_flag;
extern unsigned long success_temp_check;
extern unsigned long fail_temp_check;

static LmnRuleInstr instr_parallel;

void *normal_thread(void *arg);

void normal_parallel_init(void);
void normal_parallel_free(void);
void threadinfo_init(int id, LmnInstrVar atomi, LmnRuleRef rule,
                     LmnReactCxtRef rc, LmnRuleInstr instr,
                     AtomListEntryRef atomlist_ent, int atom_arity);

void op_lock(int id, int flag);

void normal_parallel_prof_dump(FILE *f);

BOOL check_exist(LmnSymbolAtomRef atom, LmnFunctor f);

void rule_wall_time_start(void);
void rule_wall_time_finish(void);

#endif