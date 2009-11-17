/*
 * runtime_status.c
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
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
 *
 * $Id$
 */

#include <time.h>
#include "runtime_status.h"
#include "mc.h"
#include "nd.h"
#include "st.h"
#include "rule.h"
#include "mem_encode.h"

struct RuntimeStatus {
  unsigned long atom_num;             /* # of atoms */
  unsigned long membrane_num;         /* # of membranes */
  unsigned long rule_num;             /* # of rules */
  unsigned long peak_atom_num;        /* peak # of atoms */
  unsigned long peak_membrane_num;    /* peak # of membranes */
  unsigned long peak_rule_num;       /* peak # of rules */
  unsigned long atom_space;           /* memory size by atoms (Byte) */
  unsigned long peak_atom_space;      /* peak memory size by atoms (Byte) */
  unsigned long membrane_space;       /* memory size by membranes (Byte) */
  unsigned long peak_membrane_space;  /* peak memory size by membranes (Byte) */
  unsigned long rule_space;       /* memory size by rules (Byte) */
  unsigned long peak_rule_space;  /* peak memory size by rules (Byte) */
  unsigned long hashtbl_space;        /* internal hash table size (Byte) */
  unsigned long peak_hashtbl_space;   /* peak internal hash table size (Byte) */
  unsigned long total_state_space;          /* total state size */
  unsigned long peak_total_state_space;     /* peal total state size */
  st_table_t hash_conflict_tbl;       /* key: # of conflicts, value: kinds */
  unsigned long hash_num;             /* # of hash value */
  clock_t start_time, end_time;        /* elapsed start/end time */
  clock_t tmp_state_hash_start;
  double total_state_hash_time;      /* total time of state hashing */
  unsigned long mhash_call_num;      /* # of mhash call */
  unsigned long mem_equals_num;      /* # of mem_equals call */
  double total_mem_equals_time;      /* total time of mem equals */
  clock_t tmp_mem_equals_start;

  /* 初期化や最後の解放処理を除いた非決定実行の時間 */
  clock_t nd_start_time, nd_end_time;
  time_t nd_time1, nd_time2;               /* clock()のオーバーフロー時に使う */
  BOOL run_nd;

  unsigned long state_free_num;
  double total_state_free_time;
  clock_t tmp_state_free_start;

  unsigned long mem_encode_num;      /* # of mem_encode call */
  double total_mem_encode_time;      /* total time of mem encode */
  clock_t tmp_mem_encode_start;
  unsigned long final_encode_space;           /* memory size by mem encode (Byte) */
  unsigned long encode_space;    
  unsigned long peak_encode_space;    
  unsigned long encode_len_average;     /* average length of mem encode */

  unsigned long mem_enc_eq_num;      /* # of mem_encode call */
  double total_mem_enc_eq_time;      /* total time of mem encode */
  clock_t tmp_mem_enc_eq_start;

  unsigned long mem_dump_num;      /* # of mem_encode call */
  double total_mem_dump_time;      /* total time of mem encode */
  clock_t tmp_mem_dump_start;

  unsigned long mem_decode_num;      /* # of mem decode call */
  double total_mem_decode_time;      /* total time of mem decode */
  clock_t tmp_mem_decode_start;

  time_t time1, time2;               /* clock()のオーバーフロー時に使う */
  double total_expand_time;         /* 状態展開時間 */
  unsigned long total_expand_num;
  double total_commit_time;
  clock_t tmp_expand_start;
  clock_t tmp_commit_start;

  double total_react_rule_time;
  clock_t tmp_react_rule_start;

  unsigned long tmp_rule_trial_num;
  unsigned long tmp_rule_apply_num;
  unsigned long tmp_rule_backtrack_num;
  unsigned long total_rule_trial_num;
  unsigned long total_rule_apply_num;
  unsigned long total_rule_backtrack_num;
  unsigned long counter_example_num;
  unsigned long state_num;
  unsigned long transition_num;
  unsigned long created_state_num;
  unsigned long created_transition_num;
  unsigned long tmp_state_num;

  unsigned long nd_stack_size, nd_max_stack_size;

  unsigned long final_state_space_space;
} runtime_status;

static void output_hash_conflict(FILE *f);
static void calc_hash_conflict(StateSpace states);
static void calc_encode_info(StateSpace states);

void runtime_status_init()
{
  runtime_status.atom_num = 0;
  runtime_status.membrane_num = 0;
  runtime_status.rule_num = 0;
  runtime_status.peak_atom_num = 0;
  runtime_status.peak_membrane_num = 0;
  runtime_status.peak_rule_num = 0;
  runtime_status.atom_space = 0;
  runtime_status.peak_atom_space = 0;
  runtime_status.membrane_space = 0;
  runtime_status.peak_membrane_space = 0;
  runtime_status.rule_space = 0;
  runtime_status.peak_rule_space = 0;
  runtime_status.hashtbl_space = 0;
  runtime_status.peak_hashtbl_space = 0;
  runtime_status.total_state_space = 0;
  runtime_status.peak_total_state_space = 0;
  runtime_status.hash_conflict_tbl = st_init_numtable();
  runtime_status.hash_num = 0;
  runtime_status.total_state_hash_time = 0.0;
  runtime_status.mhash_call_num = 0;
  runtime_status.mem_equals_num = 0;
  runtime_status.total_mem_equals_time = 0.0;
  runtime_status.mem_enc_eq_num = 0;
  runtime_status.total_mem_enc_eq_time = 0.0;
  runtime_status.mem_dump_num = 0;
  runtime_status.total_mem_dump_time = 0.0;
  runtime_status.state_free_num = 0;
  runtime_status.total_state_free_time = 0.0;
  runtime_status.mem_encode_num = 0;
  runtime_status.total_mem_encode_time = 0.0;
  runtime_status.mem_decode_num = 0;
  runtime_status.total_mem_decode_time = 0.0;

  runtime_status.run_nd                   = FALSE;
  runtime_status.total_expand_time        = 0.0;
  runtime_status.total_expand_num         = 0;
  runtime_status.total_commit_time        = 0.0;
  runtime_status.total_react_rule_time    = 0.0;
  runtime_status.tmp_rule_trial_num       = 0;
  runtime_status.tmp_rule_apply_num       = 0;
  runtime_status.tmp_rule_backtrack_num   = 0;
  runtime_status.total_rule_trial_num     = 0;
  runtime_status.total_rule_apply_num     = 0;
  runtime_status.total_rule_backtrack_num = 0;
  runtime_status.counter_example_num      = 0;
  runtime_status.state_num                = 0;
  runtime_status.created_state_num        = 0;
  runtime_status.created_transition_num   = 0;
  runtime_status.nd_stack_size            = 0;
  runtime_status.nd_max_stack_size        = 0;
  runtime_status.final_state_space_space  = 0;
  runtime_status.tmp_state_num            = 0;
  runtime_status.final_encode_space       = 0;
  runtime_status.peak_encode_space        = 0;
  runtime_status.encode_space        = 0;
}

void runtime_status_finalize()
{
  st_free_table(runtime_status.hash_conflict_tbl);
}

void status_start_running()
{
  runtime_status.start_time = clock();
  time(&(runtime_status.time1));
}

void status_finish_running()
{
  runtime_status.end_time = clock();
  time(&(runtime_status.time2));
}

void status_nd_start_running()
{
  runtime_status.nd_start_time = clock();
  time(&(runtime_status.nd_time1));
  runtime_status.run_nd = TRUE;
}

void status_nd_finish_running()
{
  runtime_status.nd_end_time = clock();
  time(&(runtime_status.nd_time2));
  runtime_status.run_nd = FALSE;
}

void status_add_atom_space(unsigned long size)
{
  runtime_status.atom_space += size;
  if (runtime_status.atom_space > runtime_status.peak_atom_space)
    runtime_status.peak_atom_space = runtime_status.atom_space;

  runtime_status.atom_num++;
  if (runtime_status.atom_num > runtime_status.peak_atom_num)
    runtime_status.peak_atom_num = runtime_status.atom_num;
}

void status_remove_atom_space(unsigned long size)
{
  runtime_status.atom_space -= size;
  runtime_status.atom_num--;
}

void status_add_membrane_space(unsigned long size)
{
  runtime_status.membrane_num++;
  runtime_status.peak_membrane_num =
    runtime_status.peak_membrane_num > runtime_status.membrane_num ?
      runtime_status.peak_membrane_num : runtime_status.membrane_num;

  runtime_status.membrane_space += size;
  if (runtime_status.membrane_space > runtime_status.peak_membrane_space)
    runtime_status.peak_membrane_space = runtime_status.membrane_space;
}

void status_remove_membrane_space(unsigned long size)
{
  runtime_status.membrane_num--;
  runtime_status.membrane_space -= size;
}

void status_add_rule_space(unsigned long size)
{
//  rule_spaceの出力はおあずけ
//  runtime_status.rule_space += size;
//  if (runtime_status.rule_space > runtime_status.peak_rule_space)
//    runtime_status.peak_rule_space = runtime_status.rule_space;

  runtime_status.rule_num++;
  if (runtime_status.rule_num > runtime_status.peak_rule_num)
    runtime_status.peak_rule_num = runtime_status.rule_num;
}

void status_remove_rule_space(unsigned long size)
{
//  runtime_status.rule_space -= size;
  runtime_status.rule_num--;
}

void status_add_hashtbl_space(unsigned long size)
{
  runtime_status.hashtbl_space += size;
  if (runtime_status.hashtbl_space > runtime_status.peak_hashtbl_space)
    runtime_status.peak_hashtbl_space = runtime_status.hashtbl_space;
}

void status_remove_hashtbl_space(unsigned long size)
{
  runtime_status.hashtbl_space -= size;
}

void runtime_status_update(StateSpace states)
{
  runtime_status.total_state_space =
    runtime_status.atom_space
    + runtime_status.membrane_space
    + runtime_status.hashtbl_space
    + st_table_space(state_space_tbl(states))
    + runtime_status.tmp_state_num * sizeof(struct State)
    + runtime_status.peak_encode_space;

  if (runtime_status.total_state_space > runtime_status.peak_total_state_space) {
    runtime_status.peak_total_state_space = runtime_status.total_state_space;
  }
}

void status_start_state_free()
{
  if (runtime_status.run_nd) {
    runtime_status.state_free_num++;
    runtime_status.tmp_state_free_start =  clock();
  }
}

void status_finish_state_free()
{
  if (runtime_status.run_nd) {
    runtime_status.total_state_free_time +=
      (clock() - runtime_status.tmp_state_free_start)/(double)CLOCKS_PER_SEC;
  }

  runtime_status.tmp_state_num--;
}

void status_start_state_hash_calc()
{
  runtime_status.mhash_call_num++;
  runtime_status.tmp_state_hash_start =  clock();
}

void status_finish_state_hash_calc()
{
  runtime_status.total_state_hash_time +=
    (clock() - runtime_status.tmp_state_hash_start)/(double)CLOCKS_PER_SEC;
}

void status_start_mem_equals_calc()
{
  runtime_status.mem_equals_num++;
  runtime_status.tmp_mem_equals_start =  clock();
}

void status_finish_mem_equals_calc()
{
  runtime_status.total_mem_equals_time +=
    (clock() - runtime_status.tmp_mem_equals_start)/(double)CLOCKS_PER_SEC;
}

void status_start_mem_enc_eq_calc()
{
  runtime_status.mem_enc_eq_num++;
  runtime_status.tmp_mem_enc_eq_start =  clock();
}

void status_finish_mem_enc_eq_calc()
{
  runtime_status.total_mem_enc_eq_time +=
    (clock() - runtime_status.tmp_mem_enc_eq_start)/(double)CLOCKS_PER_SEC;
}

void status_start_mem_dump_calc()
{
  runtime_status.mem_dump_num++;
  runtime_status.tmp_mem_dump_start =  clock();
}

void status_finish_mem_dump_calc()
{
  runtime_status.total_mem_dump_time +=
    (clock() - runtime_status.tmp_mem_dump_start)/(double)CLOCKS_PER_SEC;
}

void status_start_mem_encode_calc()
{
  runtime_status.mem_encode_num++;
  runtime_status.tmp_mem_encode_start =  clock();
}

void status_finish_mem_encode_calc()
{
  runtime_status.total_mem_encode_time +=
    (clock() - runtime_status.tmp_mem_encode_start)/(double)CLOCKS_PER_SEC;
}

void status_start_mem_decode_calc()
{
  runtime_status.mem_decode_num++;
  runtime_status.tmp_mem_decode_start =  clock();
}

void status_finish_mem_decode_calc()
{
  runtime_status.total_mem_decode_time +=
    (clock() - runtime_status.tmp_mem_decode_start)/(double)CLOCKS_PER_SEC;
}

void status_create_new_state()
{
  runtime_status.created_state_num++;
  runtime_status.tmp_state_num++;
}

void status_nd_push_stack()
{
  runtime_status.nd_stack_size++;
  if (runtime_status.nd_stack_size > runtime_status.nd_max_stack_size) {
    runtime_status.nd_max_stack_size = runtime_status.nd_stack_size;
  }
}
  
void status_nd_pop_stack()
{
  runtime_status.nd_stack_size--;
}
  
void status_binstr_make(LmnBinStr bs)
{
  runtime_status.encode_space += lmn_binstr_space(bs);
  if (runtime_status.encode_space >
      runtime_status.peak_encode_space)
    runtime_status.peak_encode_space = runtime_status.encode_space;
}

void status_binstr_free(LmnBinStr bs)
{
  runtime_status.encode_space -= lmn_binstr_space(bs);
}

void output_runtime_status(FILE *f)
{
  double tmp_total_time, tmp_nd_total_time;

  tmp_total_time =
    (runtime_status.end_time - runtime_status.start_time)/(double)CLOCKS_PER_SEC;
  if(tmp_total_time < 0.0) {
    tmp_total_time = difftime(runtime_status.time2, runtime_status.time1);
  }

  tmp_nd_total_time =
    (runtime_status.nd_end_time - runtime_status.nd_start_time)/(double)CLOCKS_PER_SEC;
  if(tmp_total_time < 0.0) {
    tmp_total_time = difftime(runtime_status.nd_time2, runtime_status.nd_time1);
  }


  if (!lmn_env.sp_verbose) { /* 通常はこちら */

    fprintf(f, "\n== Runtime Status ==========================================\n");
    fprintf(f, "%-30s: %10.2lf\n", "elapsed time (sec)", tmp_total_time);
    if (lmn_env.nd || lmn_env.ltl) {
      fprintf(f, "%-30s: %10lu\n", "# of states", runtime_status.state_num);
      fprintf(f, "%-30s: %10lu\n", "# of created states", runtime_status.created_state_num);
      if (lmn_env.ltl_all){
        fprintf(f, "%-30s: %10lu\n", "# of counter examples", runtime_status.counter_example_num);
      }
    }
    fprintf(f, "============================================================\n");

    if (lmn_env.profile_level >= 2) {
      fprintf(f, "\n== More Status =============================================\n");
      fprintf(f, "%-30s: %10lu\n", "# of rule trial",
              runtime_status.total_rule_trial_num + runtime_status.total_rule_backtrack_num);
      fprintf(f, "%-30s: %10lu\n", "# of rule apply",
              runtime_status.total_rule_apply_num);
      fprintf(f, "%-30s: %10lu\n", "# of backtrack",
              runtime_status.total_rule_backtrack_num);

      fprintf(f, "%-30s: %10lu\n", "peak # of atoms", runtime_status.peak_atom_num);
      fprintf(f, "%-30s: %10lu\n", "peak # of membranes",
              runtime_status.peak_membrane_num);
      fprintf(f, "%-30s: %10lu\n", "peak # of rules", runtime_status.peak_rule_num);
      fprintf(f, "%-30s: %10lu\n", "peak atom space (Bytes)",
              runtime_status.peak_atom_space);
      fprintf(f, "%-30s: %10lu\n", "peak membrane space (Bytes)",
              runtime_status.peak_membrane_space + runtime_status.peak_hashtbl_space);
      fprintf(f, "%-30s: %10lu\n", "peak binstr space (Bytes)",
              runtime_status.peak_encode_space);
      fprintf(f,   "============================================================\n");

      
      /* ND */

      if (lmn_env.nd || lmn_env.ltl) {
        struct {
          char *name;
          unsigned long calls;
          double time;
        } v[] = { {"rule trial", runtime_status.total_rule_trial_num,  runtime_status.total_react_rule_time
                                                                       - runtime_status.total_commit_time},
                  {"mem copy (in commit)", runtime_status.total_rule_apply_num, runtime_status.total_commit_time},
                  {"mem equals",           runtime_status.mem_enc_eq_num, runtime_status.total_mem_enc_eq_time},
                  {"mem id",               runtime_status.mem_encode_num, runtime_status.total_mem_encode_time},
                  {"mem dump",             runtime_status.mem_dump_num, runtime_status.total_mem_dump_time},
                  {"mem decode",           runtime_status.mem_decode_num, runtime_status.total_mem_decode_time},
                  {"mem hash",             runtime_status.mhash_call_num, runtime_status.total_state_hash_time},
                  {"state free",           runtime_status.state_free_num, runtime_status.total_state_free_time},
        };
        int i;
        double total_time = 0.0;

        fprintf(f, "\n=== ND/MC Profile ====================================\n");

        fprintf(f, "\n-- Time (sec) ---------------------------------------\n");
        fprintf(f, "%-24s %10s%10s%8s\n", "", "[calls]", "[total]", "[%]");
        for (i = 0; i < sizeof(v)/sizeof(v[0]); i++) {
          total_time += v[i].time;
          fprintf(f, "%-24s:%10lu%10.2lf%8.1lf\n",
                  v[i].name,
                  v[i].calls,
                  v[i].time,
                  100.0 * v[i].time / tmp_nd_total_time);
        }
        fprintf(f, "%-24s:%10s%10.2lf%8.1lf\n", "other", "", tmp_nd_total_time - total_time,
                (double)(tmp_nd_total_time - total_time)/ total_time * 100.0);
        fprintf(f, "-----------------------------------------------------\n");
        fprintf(f, "%-24s:%10s%10.2lf%8.1lf\n", "total elapsed time (sec)", "", tmp_nd_total_time, 100.0);
        fprintf(f, "-----------------------------------------------------\n");
        fprintf(f, "\n");

        fprintf(f, "-- Space ------------------------------------------\n");
        fprintf(f, "%-24s:%10lu\n", "# of states", runtime_status.state_num);
        fprintf(f, "%-24s:%10.2lf%4s\n", "rough peak state space",
                (runtime_status.peak_total_state_space) / 1024.0 / 1024.0, "MB");
        fprintf(f, "%-24s:%10.2lf%4s\n", "final state space", runtime_status.final_state_space_space / 1024.0 / 1024.0, "MB");
        if (runtime_status.state_num > 0)
          fprintf(f, "%-24s:%10.1lu%4s\n", "avg. space per state", runtime_status.final_state_space_space / runtime_status.state_num, "B");
        fprintf(f, "%-24s:%10lu%4s\n", "avg. encode len",
                runtime_status.encode_len_average, "B");
        fprintf(f, "%-24s:%10lu\n", "max stack size", runtime_status.nd_max_stack_size);
        fprintf(f, "---------------------------------------------------\n");
        fprintf(f, "\n");
        output_hash_conflict(f);

        fprintf(f, "===================================================\n");
      }



      if (lmn_env.profile_level >= 3) {
        fprintf(f, "\n== Detail: Rule Status =====================================\n");
        lmn_rule_show_detail(f);
      }
    }

  } else { /* sp_verboseオプション選択時はこちら。benchmarkの集計用に、csv形式で出力するモードを仮設した */
    fprintf(f, "%lu, ", runtime_status.state_num);
    fprintf(f, "%lu, ", runtime_status.counter_example_num);
    fprintf(f, "%1.2lf, ", tmp_total_time);
    fprintf(f, "%lu, ", runtime_status.peak_atom_num);
    fprintf(f, "%lu, ", runtime_status.peak_membrane_num);
    fprintf(f, "%lu, ", runtime_status.peak_atom_space);
    fprintf(f, "%lu, ", runtime_status.peak_membrane_space);
    fprintf(f, "%lu, ", runtime_status.peak_hashtbl_space);
    fprintf(f, "%lu, ", runtime_status.final_encode_space);
    fprintf(f, "%lu, ", runtime_status.encode_len_average);
    fprintf(f, "%lu, ", runtime_status.peak_total_state_space + runtime_status.final_encode_space);
    fprintf(f, "%lu, ", runtime_status.mhash_call_num);
    fprintf(f, "%1.2lf, ", runtime_status.total_state_hash_time);
    fprintf(f, "%lu, ", runtime_status.mem_equals_num);
    fprintf(f, "%1.2lf, ", runtime_status.total_mem_equals_time);
    fprintf(f, "%lu, ", runtime_status.mem_encode_num);
    fprintf(f, "%1.2lf, ", runtime_status.total_mem_encode_time);
    fprintf(f, "%1.2lf, ", runtime_status.total_expand_time);
    fprintf(f, "%1.2lf, ", runtime_status.total_commit_time);
    fprintf(f, "%1.2lf, ", runtime_status.total_expand_time - runtime_status.total_commit_time);
  }
}

void status_state_space(StateSpace states)
{
  runtime_status.state_num = state_space_num(states);
  calc_hash_conflict(states);
  calc_encode_info(states);
  calc_state_space_space(states);
}

static int dispersal_f(st_data_t key, st_data_t s_, st_data_t tbl_)
{
  st_table_t tbl = (st_table_t)tbl_;
  State *s = (State*)s_;

  st_data_t n;
  if (!st_lookup((st_table_t)tbl, (st_data_t)state_hash(s), &n)) {
    n = 0;
  }
  if (lmn_env.mem_enc) {
    st_insert((st_table_t)tbl, (st_data_t)state_memid_hash(s), (st_data_t)(n+1));
  } else {
    st_insert((st_table_t)tbl, (st_data_t)state_hash(s), (st_data_t)(n+1));
  }
/*   fprintf(stdout, "%d :: ", s->hash); */
/*   lmn_dump_mem_stdout(s->mem); */

  return ST_CONTINUE;
}

static int accum_f(st_data_t hash_value, st_data_t num, st_data_t tbl_)
{
  st_table_t hash_to_values = (st_table_t)tbl_;

  st_data_t n;
  if (!st_lookup((st_table_t)hash_to_values, (st_data_t)num, &n)) {
    n = 0;
  }
  st_insert((st_table_t)hash_to_values, (st_data_t)num, (st_data_t)(n+1));
  runtime_status.hash_num++;

  return ST_CONTINUE;
}

static void calc_hash_conflict(StateSpace states)
{
  st_table_t hash_to_values;

  hash_to_values = st_init_numtable();

  /* hash_valueはハッシュ値とそのハッシュ値を持つ状態の数表を作る */
  st_foreach(state_space_tbl(states), dispersal_f, (st_data_t)hash_to_values);
  /* conflict数とconflictしているハッシュ値の種類を集計 */
  st_foreach(hash_to_values, accum_f, (st_data_t)runtime_status.hash_conflict_tbl);

  st_free_table(hash_to_values);
}

static int dispersal_print_f(st_data_t hash_, st_data_t n_, st_data_t f_)
{
  unsigned int hash = (unsigned int)hash_;
  int n = (int)n_;
  FILE *f = (FILE*)f_;

  fprintf(f, "%9d%9d\n", hash, n);

  return ST_CONTINUE;
}

static void output_hash_conflict(FILE *f)
{
  if (!lmn_env.nd && !lmn_env.ltl) return;

  if (!lmn_env.sp_verbose) {
    fprintf(f, "-- Hash ------------------\n");
    fprintf(f, "%-16s:%9lu\n", "# of hash values", runtime_status.hash_num);
    fprintf(f, "------------------\n");
    fprintf(f, "%9s%9s\n", "conflicts", "kinds");
    st_foreach(runtime_status.hash_conflict_tbl, dispersal_print_f, (st_data_t)f);
    fprintf(f, "------------------\n");
    fprintf(f, "--------------------------\n");
  } else {
    fprintf(f, "%lu, ", runtime_status.hash_num);
    fprintf(f, "%lu\n", runtime_status.state_num - runtime_status.hash_num);
  }
}

static int encode_info_f(st_data_t key, st_data_t s_, st_data_t t)
{
  State *s = (State*)s_;
  LmnBinStr bs = state_mem_binstr(s);
  if (bs) {
    runtime_status.final_encode_space += lmn_binstr_space(bs);
  }
  return ST_CONTINUE;
}

static void calc_encode_info(StateSpace states)
{
  runtime_status.final_encode_space = 0;
  st_foreach(state_space_tbl(states), encode_info_f, (st_data_t)0);
  runtime_status.encode_len_average = runtime_status.final_encode_space / state_space_num(states);
}

void calc_state_space_space(StateSpace states)
{
  runtime_status.final_state_space_space =
    runtime_status.atom_space +
    runtime_status.membrane_space +
    runtime_status.hashtbl_space +
    runtime_status.final_encode_space +
    st_table_space(state_space_tbl(states)) +
    sizeof(struct State) * state_space_num(states);
}

void status_start_rule()
{
  runtime_status.tmp_rule_trial_num     = 1;
  runtime_status.tmp_rule_apply_num     = 0;
  runtime_status.tmp_rule_backtrack_num = 0;
  runtime_status.tmp_react_rule_start= clock();
}

void status_finish_rule(LmnRule rule, BOOL result)
{
  unsigned long tmp_ap, tmp_tr, tmp_ba;
  if(result && !(lmn_env.nd || lmn_env.ltl)) {
  runtime_status.tmp_rule_apply_num = 1; /* for tracer only */
  }
  tmp_ap = runtime_status.tmp_rule_apply_num;
  tmp_tr = runtime_status.tmp_rule_trial_num;
  tmp_ba = runtime_status.tmp_rule_backtrack_num;
  runtime_status.total_rule_apply_num     += tmp_ap;
  runtime_status.total_rule_trial_num     += tmp_tr;
  runtime_status.total_rule_backtrack_num += tmp_ba;
  runtime_status.total_react_rule_time += (clock() - runtime_status.tmp_react_rule_start)/(double)CLOCKS_PER_SEC;
  if(lmn_env.profile_level >= 3) {
    lmn_rule_profile(rule, tmp_ap, tmp_tr, tmp_ba);
  }
}

/* for Tracer only */
void status_rule_output(LmnRule rule) {
  fprintf(stdout,  "   %5s %5lu %5lu %5lu\n", lmn_id_to_name(lmn_rule_get_name(rule)),
      runtime_status.tmp_rule_apply_num,
      runtime_status.tmp_rule_trial_num + runtime_status.tmp_rule_backtrack_num,
      runtime_status.tmp_rule_backtrack_num);
}

void status_backtrack_counter()
{
  runtime_status.tmp_rule_backtrack_num++;
}

void status_start_commit()
{
  runtime_status.tmp_rule_apply_num++;
  runtime_status.tmp_commit_start = clock();
}

void status_finish_commit()
{
  runtime_status.total_commit_time +=
    (clock() - runtime_status.tmp_commit_start) / (double)CLOCKS_PER_SEC;
}

void status_start_expand()
{
  runtime_status.tmp_expand_start = clock();
}

void status_finish_expand()
{
  runtime_status.total_expand_num++;
  runtime_status.total_expand_time +=
    (clock() - runtime_status.tmp_expand_start) / (double)CLOCKS_PER_SEC;
}

void status_count_counterexample()
{
  runtime_status.counter_example_num++;
}
