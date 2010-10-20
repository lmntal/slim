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
#include "runtime_status.h"
#include "mc.h"
#include "state.h"
#include "mem_encode.h"
#include "util.h"
#include "dumper.h"

       static void mc_profiler_init(MCProfiler *p);
       static void mc_profiler_destroy(MCProfiler *p);
inline static void time_profiler_init(TimeProfiler *p);
inline static void time_profiler_destroy(TimeProfiler *p);
inline static void memory_profiler_init(MemoryProfiler *p);
inline static void memory_profiler_destroy(MemoryProfiler *p);
inline static void peak_counter_init(PeakCounter *p);
inline static void peak_counter_destroy(PeakCounter *p);
       static void profile_state_f(State *s);
       static char *profile_space_id_to_name(int type);
       static char *profile_counter_id_to_name(int type);
       static char *profile_time_id_to_name(int type);
       static void mc_profiler_make_up_report(MCProfiler *total);

/** ----------------------------
 *  Rule Profiler
 */
RuleProfiler *rule_profiler_make(LmnRulesetId id, LmnRule src)
{
  RuleProfiler *p = LMN_MALLOC(RuleProfiler);
  time_profiler_init(&p->trial);
  p->src         = src;
  p->backtrack   = 0;
  p->apply       = 0;
  p->ref_rs_id   = id;

  return p;
}

void rule_profiler_free(RuleProfiler *p)
{
  time_profiler_destroy(&p->trial);
  LMN_FREE(p);
}

static int rule_profiler_free_f(st_data_t _key, st_data_t _v, st_data_t _arg)
{
  RuleProfiler *p = (RuleProfiler *)_v;
  rule_profiler_free(p);
  return ST_CONTINUE;
}

static int comp_prule_id_greater_f(const void *a_, const void *b_)
{
  RuleProfiler *p1, *p2;
  p1 = *(RuleProfiler **)a_;
  p2 = *(RuleProfiler **)b_;

  return p1->ref_rs_id - p2 ->ref_rs_id;
}


/** ----------------------------
 *  MC Profiler
 */
static void mc_profiler_init(MCProfiler *p)
{
  unsigned int i;

  /* for spaces */
  for (i = 0; i < ARY_SIZEOF(p->spaces); i++) {
    memory_profiler_init(&p->spaces[i]);
  }
  /* for timers */
  for (i = 0; i < ARY_SIZEOF(p->times); i++) {
    time_profiler_init(&p->times[i]);
  }
  /* for counters */
  for (i = 0; i < ARY_SIZEOF(p->counters); i++) {
    p->counters[i] = 0;
  }
}

static void mc_profiler_destroy(MCProfiler *p)
{
  unsigned int i;

  for (i = 0; i < ARY_SIZEOF(p->spaces); i++) {
    memory_profiler_destroy(&p->spaces[i]);
  }
  for (i = 0; i < ARY_SIZEOF(p->times); i++) {
    time_profiler_destroy(&p->times[i]);
  }
}

inline static void time_profiler_init(TimeProfiler *p)
{
  p->called_num = 0;
  p->total_time = 0.0;
  p->tmp_start  = 0;
}

inline static void time_profiler_destroy(TimeProfiler *p)
{

}

inline static void memory_profiler_init(MemoryProfiler *p)
{
  peak_counter_init(&p->num);
  peak_counter_init(&p->space);
}

inline static void memory_profiler_destroy(MemoryProfiler *c)
{

}

inline static void peak_counter_init(PeakCounter *p)
{
  p->cur  = 0;
  p->peak = 0;
}

inline static void peak_counter_destroy(PeakCounter *p)
{

}


void lmn_profiler_init()
{
  unsigned int i, n;
  n = lmn_env.core_num;

  lmn_prof.valid                 = FALSE;
  lmn_prof.start_wall_time       = 0.0;
  lmn_prof.end_wall_time         = 0.0;
  lmn_prof.start_cpu_time        = 0.0;
  lmn_prof.end_cpu_time          = 0.0;
  lmn_prof.start_wall_time_main  = 0.0;
  lmn_prof.end_wall_time_main    = 0.0;
  lmn_prof.start_cpu_time_main   = LMN_NALLOC(double, n);
  lmn_prof.end_cpu_time_main     = LMN_NALLOC(double, n);
  lmn_prof.state_num_stored      = 0;
  lmn_prof.state_num_end         = 0;
  lmn_prof.error_num             = 0;
  lmn_prof.accept_num            = 0;
  lmn_prof.invalid_end_num       = 0;
  lmn_prof.mhash_num             = 0;
  lmn_prof.rehashed_num          = 0;
  lmn_prof.midhash_num           = 0;
  lmn_prof.transition_num        = 0;
  lmn_prof.statespace_space      = 0;
  lmn_prof.transition_space      = 0;
  lmn_prof.state_space           = 0;
  lmn_prof.binstr_space          = 0;
  lmn_prof.membrane_space        = 0;
  lmn_prof.on_the_fly            = NULL;
  lmn_prof.prules                = NULL;
  lmn_prof.cur                   = NULL;

  if (lmn_env.nd) {
    if (lmn_env.profile_level >= 2) {
      hashset_init(&lmn_prof.hashes, 12);
    }

    if (lmn_env.profile_level >= 3) {
      lmn_prof.on_the_fly = LMN_NALLOC(MCProfiler, n);
      for (i = 0; i < n; i++) {
        mc_profiler_init(&lmn_prof.on_the_fly[i]);
      }
    }
  } else if (lmn_env.profile_level >= 2) {
    lmn_prof.prules = st_init_ptrtable();
  }
}

void lmn_profiler_finalize()
{
  LMN_FREE(lmn_prof.start_cpu_time_main);
  LMN_FREE(lmn_prof.end_cpu_time_main);

  if (lmn_env.nd) {
    if (lmn_env.profile_level >= 2) {
      hashset_destroy(&lmn_prof.hashes);

      if (lmn_env.profile_level >= 3) {
        unsigned int i;
        for (i = 0; i < lmn_env.core_num; i++) {
          mc_profiler_destroy(&lmn_prof.on_the_fly[i]);
        }
        LMN_FREE(lmn_prof.on_the_fly);
      }
    }
  } else if (lmn_env.profile_level >= 2) {
    st_foreach(lmn_prof.prules, rule_profiler_free_f, (st_data_t)0);
    st_free_table(lmn_prof.prules);
  }
}


void profile_start_slim()
{
  lmn_prof.start_wall_time = get_wall_time();
  lmn_prof.start_cpu_time  = get_cpu_time();
}

void profile_finish_slim()
{
  lmn_prof.end_cpu_time  = get_cpu_time();
  lmn_prof.end_wall_time = get_wall_time();
}

void profile_start_exec()
{
  lmn_prof.valid = TRUE;
  lmn_prof.start_wall_time_main = get_wall_time();
}

void profile_start_exec_thread()
{
  lmn_prof.start_cpu_time_main[lmn_thread_id] = get_cpu_time();
}

void profile_finish_exec_thread()
{
  lmn_prof.end_cpu_time_main[lmn_thread_id] = get_cpu_time();
}

void profile_finish_exec()
{
  lmn_prof.valid = FALSE;
  lmn_prof.end_wall_time_main = get_wall_time();
}

void profile_statespace(StateSpace states)
{
  lmn_prof.state_num_stored = state_space_num(states);
  lmn_prof.state_num_end    = state_space_end_num(states);
  if (mc_data.do_search) {
    lmn_prof.error_num        = mc_get_error_num();
  }
  lmn_prof.statespace_space = state_space_space(states);
  state_space_foreach(states, profile_state_f, DEFAULT_ARGS);
}

static void profile_state_f(State *s)
{
  unsigned int succ_num = state_succ_num(s);

  /* メモリ */
  lmn_prof.state_space += sizeof(State);
  if (state_mem(s)) {
    LmnMembrane *src, *dst;
    src = state_mem(s);
    lmn_prof.membrane_space += lmn_mem_space(src);
    ALL_MEMS(src, dst, {
      lmn_prof.membrane_space += lmn_mem_space(dst);
    });
  }
  if (state_mem_binstr(s)) {
    lmn_prof.binstr_space   += lmn_binstr_space(state_mem_binstr(s));
  }

  lmn_prof.transition_space += succ_num * sizeof(succ_data_t); /* # of pointer */
  if (lmn_env.show_transition) {
    unsigned int i;
    for (i = 0; i < succ_num; i++) {
      lmn_prof.transition_space += transition_space(transition(s, i));
    }
  }

  /* 遷移数 */
  lmn_prof.transition_num += succ_num;

  if (!(is_encoded(s) && is_dummy(s))) {
    if (mc_data.has_property) {
      AutomataState atm = automata_get_state(mc_data.property_automata,
                                             state_property_state(s));
      if (atmstate_is_accept(atm)) {
        lmn_prof.accept_num++;
      }
      if (atmstate_is_end(atm)) {
        lmn_prof.invalid_end_num++;
      }
    }
  }

  /* ハッシュ値の種類数 */
  if (!hashset_contains(&lmn_prof.hashes, state_hash(s))) {
    hashset_add(&lmn_prof.hashes, state_hash(s));

    if (is_encoded(s)) {
      lmn_prof.midhash_num++;
      if (is_dummy(s)) {
        lmn_prof.rehashed_num++;
      }
    } else {
      lmn_prof.mhash_num++;
    }
  }

  if (!s->next) {
    /* 同じハッシュ値は同じリストにのみ存在するので, リストが切り替わったらクリア */
    hashset_clear(&lmn_prof.hashes);
  }
}

void calc_statespace_space(MCProfiler *p, StateSpace states)
{
  unsigned int i;
  for (i = 0; i < ARY_SIZEOF(p->spaces); i++) {
    lmn_prof.statespace_space += (&p->spaces[i])->space.cur;
  }
}

void dump_profile_data(FILE *f)
{
  double tmp_total_cpu_time,  tmp_total_cpu_time_main;
  double tmp_total_wall_time, tmp_total_wall_time_main;
  double tmp_total_mem;
  unsigned long total_hash_num;
  unsigned int i;

  tmp_total_cpu_time_main  = 0.0;
  for (i = 0; i < lmn_env.core_num; i++) {
    tmp_total_cpu_time_main  += lmn_prof.end_cpu_time_main[i] - lmn_prof.start_cpu_time_main[i];
  }
  tmp_total_cpu_time_main  = tmp_total_cpu_time_main / lmn_env.core_num;
  tmp_total_cpu_time       = lmn_prof.end_cpu_time  - lmn_prof.start_cpu_time;
  tmp_total_wall_time      = lmn_prof.end_wall_time - lmn_prof.start_wall_time;
  tmp_total_wall_time_main = lmn_prof.end_wall_time_main - lmn_prof.start_wall_time_main;

  tmp_total_mem            = (double)(lmn_prof.state_space
                                    + lmn_prof.transition_space
                                    + lmn_prof.binstr_space
                                    + lmn_prof.membrane_space
                                    + lmn_prof.statespace_space);

  total_hash_num = lmn_prof.mhash_num + lmn_prof.midhash_num - lmn_prof.rehashed_num;

  if (lmn_env.benchmark) { /* データ収集用 */
    fprintf(f, "%lf, %lf, %lf, %lu, %lu, %lu, %lu, %lf, %lf, %lf, %lf, %lf\n",
                            tmp_total_wall_time,
                            tmp_total_wall_time_main,
                            tmp_total_cpu_time_main,
                            lmn_prof.state_num_stored,
                            lmn_prof.transition_num,
                            lmn_prof.state_num_end,
                            total_hash_num,
                            tmp_total_mem / 1024 / 1024,
                            (double)lmn_prof.state_space / 1024 / 1024,
                            lmn_env.enable_compress_mem ? (double)lmn_prof.binstr_space / 1024 / 1024
                                                        : (double)lmn_prof.membrane_space / 1024 / 1024,
                            (double)lmn_prof.transition_space / 1024 / 1024,
                            (double)lmn_prof.statespace_space / 1024 / 1024
                            );
    return;
  }

  if (lmn_env.profile_level >= 1) {
    fprintf(f, "\n== Static Profiler Report ==================================\n");
    fprintf(f, "%-20s%8s  : %15.2lf\n", "Wall Time (sec)",        "Total", tmp_total_wall_time);
    fprintf(f, "%-20s%8s  : %15.2lf\n", " ",                      " Exec", tmp_total_wall_time_main);
    fprintf(f,   "------------------------------------------------------------\n");

    if (lmn_env.core_num == 1) {
      fprintf(f, "%-20s%8s  : %15.2lf\n", "CPU Usage (sec)",      "Total", tmp_total_cpu_time);
      fprintf(f, "%-20s%8s  : %15.2lf\n", " ",                    " Exec", tmp_total_cpu_time_main);
    } else {
      fprintf(f, "%-18s%10s  : %15.2lf\n", "CPU Usage (sec)", "Exec Avg.", tmp_total_cpu_time_main);
      fprintf(f, "%-18s%10s  : %15s\n",    " ",           "---------", "-------------------");
      for (i = 0; i < lmn_env.core_num; i++) {
        fprintf(f, "%-12s%14s%2u  : %15.2lf\n", " ", "Thread", i,
                    lmn_prof.end_cpu_time_main[i] - lmn_prof.start_cpu_time_main[i]);
      }
    }
    if (!lmn_env.nd) {
      fprintf(f,   "============================================================\n");
      if (lmn_env.profile_level >= 2) {
        RuleProfiler *r_total, *r_others;
        struct Vector v;
        unsigned int i;

        r_total  = rule_profiler_make(ANONYMOUS, NULL);
        r_others = rule_profiler_make(ANONYMOUS, NULL);

        vec_init(&v, st_num(lmn_prof.prules));
        st_get_entries_value(lmn_prof.prules, &v);
        vec_sort(&v, comp_prule_id_greater_f);

        fprintf(f, "\n== On-The-Fly Analyzer Report ==============================\n");
        fprintf(f,   "%4s %8s : %9s %9s %9s %12s",
            "[id]", "[name]", "[# Tr.]", "[# Ap.]", "[# BT.]", "[CPU U.(msec)]\n");

        for (i = 0; i < vec_num(&v); i++) {
          RuleProfiler *rp = (RuleProfiler *)vec_get(&v, i);
          if (rp->trial.called_num > 0) {
            if (lmn_rule_get_name(rp->src) == ANONYMOUS) {
              /* 一度もマッチングに成功しなかったルールはまとめる */
              r_others->trial.called_num += rp->trial.called_num;
              r_others->trial.total_time += rp->trial.total_time;
              r_others->backtrack += rp->backtrack;
            } else {
              /* 一応ナノセックまで取得できるが, 精度は環境依存 */
              fprintf(f, "@%-3d %8s : %9lu %9lu %9lu %13.3lf\n",
                         rp->ref_rs_id,
                         lmn_id_to_name(lmn_rule_get_name(rp->src)),
                         rp->trial.called_num,
                         rp->apply,
                         rp->backtrack,
                         rp->trial.total_time / 1e-6);
            }
            r_total->apply            += rp->apply;
            r_total->backtrack        += rp->backtrack;
            r_total->trial.called_num += rp->trial.called_num;
            r_total->trial.total_time += rp->trial.total_time;
          }
        }
        fprintf(f, "%4s %8s : %9lu %9lu %9lu %13.3lf\n",
                   " - ",
                   "OTHERS",
                   r_others->trial.called_num,
                   0UL,
                   r_others->backtrack,
                   r_others->trial.total_time / 1e-6);
        fprintf(f,   "------------------------------------------------------------\n");
        fprintf(f, "%4s %8s : %9lu %9lu %9lu %13.3lf\n",
                   " - ",
                   "Total",
                   r_total->trial.called_num,
                   r_total->apply,
                   r_total->backtrack,
                   r_total->trial.total_time / 1e-6);

        vec_destroy(&v);
        rule_profiler_free(r_others);
        fprintf(f,   "============================================================\n");
      }
    } else if (lmn_env.profile_level < 2) {
      fprintf(f,   "============================================================\n");
    } else {
      fprintf(f,   "------------------------------------------------------------\n");
      fprintf(f, "%-20s%8s  : %15lu\n",   "# of States",             "Stored", lmn_prof.state_num_stored);
      fprintf(f, "%-18s%10s  : %15lu\n",   " ",                  "Successors", lmn_prof.transition_num);
      fprintf(f, "%-18s%10s  : %15lu\n",   " ",                  "Terminates", lmn_prof.state_num_end);
      if (mc_data.has_property) {
        if (mc_data.do_search) {
          fprintf(f, "%-10s%18s  : %15lu\n",   " ",             "Error / Cycle", lmn_prof.error_num);
        }
        fprintf(f, "%-10s%18s  : %15lu\n",   " ",                  "Accepted", lmn_prof.accept_num);
        fprintf(f, "%-10s%18s  : %15lu\n",   " ",              "Invalid Ends", lmn_prof.invalid_end_num);
      }
      fprintf(f,   "------------------------------------------------------------\n");
      fprintf(f, "%-20s%8s  : %15lu\n",   "# of Hash Values",         "Total", total_hash_num);
      fprintf(f, "%-6s%22s  : %15lu\n",   " ",            "Default -  M_Hash", lmn_prof.mhash_num);
      fprintf(f, "%-6s%22s  : %15lu\n",   " ",           "ReHashed -  M_Hash", lmn_prof.rehashed_num);
      fprintf(f, "%-6s%22s  : %15lu\n",   " ",          "Optimized - BS_Hash", lmn_prof.midhash_num);
      fprintf(f,   "------------------------------------------------------------\n");
      fprintf(f, "%-16s%12s    %12s %12s\n", "Memory Usage ", "", "[Amount(MB)]", "[Per State(B)]");
      fprintf(f, "%-10s%18s  : %12.2lf %12.2lf\n", " ",             "Total", tmp_total_mem / 1024 / 1024, tmp_total_mem / lmn_prof.state_num_stored);
      fprintf(f, "%-10s%18s  : %12.2lf %12.2lf\n", " ", "State Descriptors", (double)lmn_prof.state_space / 1024 / 1024, (double)lmn_prof.state_space / lmn_prof.state_num_stored);
      if (lmn_env.enable_compress_mem) {
        fprintf(f, "%-10s%18s  : %12.2lf %12.2lf\n", " ",  "Binary Strings", (double)lmn_prof.binstr_space / 1024 / 1024, (double)lmn_prof.binstr_space / lmn_prof.state_num_stored);
      } else {
        fprintf(f, "%-10s%18s  : %12.2lf %12.2lf\n", " ", "State Membranes", (double)lmn_prof.membrane_space / 1024 / 1024, (double)lmn_prof.membrane_space / lmn_prof.state_num_stored);
      }
      fprintf(f, "%-10s%18s  : %12.2lf %12.2lf\n", " ",       "Transitions", (double)lmn_prof.transition_space / 1024 / 1024, (double)lmn_prof.transition_space / lmn_prof.state_num_stored);
      fprintf(f, "%-10s%18s  : %12.2lf %12.2lf\n", " ",        "StateSpace", (double)lmn_prof.statespace_space / 1024 / 1024, (double)lmn_prof.statespace_space / lmn_prof.state_num_stored);
#ifdef DEVEL
      fprintf(f,   "------------------------------------------------------------\n");
#ifdef DEBUG
      fprintf(f, "%-8s%20s  : %15s\n",   "Option ",              "Debug Mode", "ON");
#else
      fprintf(f, "%-8s%20s  : %15s\n",   "Option ",              "Debug Mode", "OFF");
#endif /* DEBUG */
#ifdef PROFILE
      fprintf(f, "%-8s%20s  : %15s\n",   " ",                  "Profile Mode", "ON");
#else
      fprintf(f, "%-8s%20s  : %15s\n",   " ",                  "Profile Mode", "OFF");
#endif /* PROFILE */
#ifdef TIME_OPT
      fprintf(f, "%-8s%20s  : %15s\n",   " ",                  "Time Opt. Mode", "ON");
#else
      fprintf(f, "%-8s%20s  : %15s\n",   " ",                  "Time Opt. Mode", "OFF");
#endif /* PROFILE */
      fprintf(f, "%-8s%20s  : %15d\n",   " ",                 "# of  Threads", lmn_env.core_num);
      fprintf(f, "%-4s%24s  : %15s\n",   " ",         "Dynamic Load Balancer", lmn_env.optimize_loadbalancing ? "YES" : "NO");
      fprintf(f, "%-8s%20s  : %15s\n",   " ",             "Property Automata", mc_data.has_property  ? "YES" : "NO");
      fprintf(f, "%-8s%20s  : %15s\n",   " ",            "LTL Model Checking", lmn_env.ltl           ? "YES" : "NO");
      fprintf(f, "%-8s%20s  : %15s\n",   " ",               "Search Strategy", lmn_env.bfs           ? "BFS" : "DFS");
      fprintf(f, "%-8s%20s  : %15s\n",   " ",              "Compact OpenNode", lmn_env.compact_stack ? "YES" : "NO");
      fprintf(f, "%-8s%20s  : %15s\n",   " ",                "Delta Membrane", lmn_env.delta_mem     ? "YES" : "NO");
      fprintf(f, "%-4s%24s  : %15s\n",   " ",       "Partial Order Reduction", lmn_env.por           ? "YES" : "NO");
      fprintf(f, "%-8s%20s  : %15s\n",   " ",            "Symmetry Reduction", "NO");
#endif /* DEVEL */
      fprintf(f,   "============================================================\n");

      if (lmn_env.profile_level >= 3) {
        MCProfiler total;
        double total_time;

        for (i = 0; i < lmn_env.core_num; i++) { /* 計測したactive時間をidle時間へ変換 */
          lmn_prof.on_the_fly[i].times[PROFILE_TIME__ACTIVE_FOR_IDLE_PROF].total_time =
            lmn_env.core_num == 1 ? 0 :
               lmn_prof.end_cpu_time_main[i]
             - lmn_prof.start_cpu_time_main[i]
             - lmn_prof.on_the_fly[i].times[PROFILE_TIME__ACTIVE_FOR_IDLE_PROF].total_time;
          /* startからfinishまで回したCPU時間から指定したactiveブロックに費やしたCPU時間を引けばidle時間になる.
           * (一見冗長に見えるが, spin-waitが費やしたCPU時間を除くことができる.) */
        }

        mc_profiler_init(&total);
        mc_profiler_make_up_report(&total);
        total_time = 0.0;

        total.times[PROFILE_TIME__TRANS_RULE].total_time -= total.times[PROFILE_TIME__STATE_COPY_IN_COMMIT].total_time;

        fprintf(f, "\n== On-The-Fly Analyzer Report ==============================\n");
        fprintf(f,   "-- Time Performance ----------------------------------------\n");
        fprintf(f, "%-24s %10s%10s%8s\n", "", "[calls]", "[total]", "[%]");
        for (i = 0; i < ARY_SIZEOF(total.times); i++) {
          total_time += total.times[i].total_time;
          fprintf(f, "%-24s:%10lu%10.2lf%8.1lf\n", profile_time_id_to_name(i),
                                                   total.times[i].called_num,
                                                   total.times[i].total_time,
                                                   100.0 * total.times[i].total_time / tmp_total_cpu_time_main);
        }
        fprintf(f, "%-24s:%10s%10.2lf%8.1lf\n", "other", "", (tmp_total_cpu_time_main - total_time),
                   (double)(tmp_total_cpu_time_main - total_time) / tmp_total_cpu_time_main * 100.0);
        fprintf(f,   "------------------------------------------------------------\n");
        fprintf(f,   "%-24s:%10s%10.2lf%8.1lf\n", lmn_env.core_num > 2 ? "CPU Usage AVG. (sec)"
                                                                       : "CPU Usage (sec)", "", tmp_total_cpu_time_main, 100.0);
        fprintf(f,   "------------------------------------------------------------\n");
        fprintf(f, "\n");
        fprintf(f,   "-- Memory Performance --------------------------------------\n");
        if (lmn_env.core_num >= 2) {
          fprintf(f, "How to show peak performance in parallization? under const.\n");
        } else {
          fprintf(f, "%-24s  %10s %10s %10s\n", " ", "[Fin.(MB)]", "[Peak(MB)]", "[Peak Num]");
          for (i = 0; i < ARY_SIZEOF(total.spaces); i++) {
            fprintf(f, "%-24s: %10.2lf %10.2lf %10lu\n",
                       profile_space_id_to_name(i),
                       (double)total.spaces[i].space.cur / 1024 / 1024,
                       (double)total.spaces[i].space.peak / 1024 / 1024,
                       total.spaces[i].num.peak);
          }
        }
        fprintf(f,   "------------------------------------------------------------\n");
        fprintf(f, "\n");
        fprintf(f,   "-- State Management System (Open Hashing) ------------------\n");
        for (i = 0; i < ARY_SIZEOF(total.counters); i++) {
          fprintf(f, "%-24s:%10lu\n", profile_counter_id_to_name(i), total.counters[i]);
        }
        fprintf(f,   "============================================================\n");
      }
    }
  }
}

static void mc_profiler_make_up_report(MCProfiler *total)
{
  unsigned int data_i, th_id;

  for (data_i = 0; data_i < ARY_SIZEOF(total->times); data_i++) {
    for (th_id = 0; th_id < lmn_env.core_num; th_id++) {
      TimeProfiler *p = &(lmn_prof.on_the_fly[th_id].times[data_i]);
      total->times[data_i].called_num += p->called_num;
      total->times[data_i].total_time += p->total_time;
    }
    if (total->times[data_i].total_time > 0.0) {
      total->times[data_i].total_time /= lmn_env.core_num;
    }
  }

  for (data_i = 0; data_i < ARY_SIZEOF(total->counters); data_i++) {
    for (th_id = 0; th_id < lmn_env.core_num; th_id++) {
      total->counters[data_i] += lmn_prof.on_the_fly[th_id].counters[data_i];
    }
  }

  for (data_i = 0; data_i < ARY_SIZEOF(total->spaces); data_i++) {
    for (th_id = 0; th_id < lmn_env.core_num; th_id++) {
      MCProfiler *p = &lmn_prof.on_the_fly[th_id];
      total->spaces[data_i].space.cur  += p->spaces[data_i].space.cur;
      total->spaces[data_i].space.peak += p->spaces[data_i].space.peak;
      total->spaces[data_i].num.cur    += p->spaces[data_i].space.cur;
      total->spaces[data_i].num.peak   += p->spaces[data_i].num.peak;
    }
    total->spaces[data_i].space.cur  /= lmn_env.core_num;
    total->spaces[data_i].space.peak /= lmn_env.core_num;
    total->spaces[data_i].num.cur    /= lmn_env.core_num;
    total->spaces[data_i].num.peak   /= lmn_env.core_num;
  }
}

static char *profile_time_id_to_name(int type)
{
  char *ret;
  switch (type) {
  case PROFILE_TIME__ACTIVE_FOR_IDLE_PROF:
    ret = "idle";
    break;
  case PROFILE_TIME__STATE_HASH_MEM:
    ret = "state mhash";
    break;
  case PROFILE_TIME__STATE_HASH_MID:
    ret = "state binstr hash";
    break;
  case PROFILE_TIME__STATE_COMPARE_MEQ:
    ret = "state mem compare";
    break;
  case PROFILE_TIME__STATE_COMPARE_MID:
    ret = "state binstr compare";
    break;
  case PROFILE_TIME__STATE_COPY:
    ret = "state copy";
    break;
  case PROFILE_TIME__STATE_COPY_IN_COMMIT:
    ret = "state copy  (in COMMIT)";
    break;
  case PROFILE_TIME__TRANS_RULE:
    ret = "expand rule (ex COMMIT)";
    break;
  case PROFILE_TIME__MENC_DUMP:
    ret = "menc dump";
    break;
  case PROFILE_TIME__MENC_RESTORE:
    ret = "menc restore";
    break;
  case PROFILE_TIME__MENC_CANONICAL:
    ret = "menc canonical";
    break;
  case PROFILE_TIME__DMEM_COMMIT:
    ret = "dmem commit";
    break;
  case PROFILE_TIME__DMEM_REVERT:
    ret = "dmem revert";
    break;
  case PROFILE_TIME__CYCLE_EXPLORE:
    ret = "cycle explore";
    break;
  case PROFILE_TIME__Z_COMPRESS:
    ret = "z compress";
    break;
  case PROFILE_TIME__Z_UNCOMPRESS:
    ret = "z uncompress";
    break;
  default:
    ret = "unknown";
    break;
  }
  return ret;
}

static char *profile_counter_id_to_name(int type)
{
  char *ret;
  switch (type) {
  case PROFILE_COUNT__HASH_CONFLICT_ENTRY:
    ret = "conflict hash entry";
    break;
  case PROFILE_COUNT__HASH_CONFLICT_HASHV:
    ret = "conflict hash value";
    break;
  case PROFILE_COUNT__HASH_RESIZE_TRIAL:
    ret = "trial tbl resize";
    break;
  case PROFILE_COUNT__HASH_RESIZE_APPLY:
    ret = "apply tbl resize";
    break;
  case PROFILE_COUNT__HASH_FAIL_TO_INSERT:
    ret = "fail to insert tbl";
    break;
  default:
    ret = "unknown";
    break;
  }
  return ret;
}

static char *profile_space_id_to_name(int type)
{
  char *ret;
  switch (type) {
  case PROFILE_SPACE__TOTAL:
    ret = "total";
    break;
  case PROFILE_SPACE__STATE_BINSTR:
    ret = "state binstr";
    break;
  case PROFILE_SPACE__STATE_OBJECT:
    ret = "state object";
    break;
  case PROFILE_SPACE__TRANS_OBJECT:
    ret = "trans object";
    break;
  case PROFILE_SPACE__STATE_MEMBRANE:
    ret = "state membrane";
    break;
  case PROFILE_SPACE__OPEN_LIST:
    ret = "open list";
    break;
  case PROFILE_SPACE__REDUCED_MEMSET:
    ret = "reduced State Mem";
    break;
  case PROFILE_SPACE__REDUCED_BINSTR:
    ret = "reduced State BinStr";
    break;
  default:
    ret = "unknown";
    break;
  }
  return ret;
}


