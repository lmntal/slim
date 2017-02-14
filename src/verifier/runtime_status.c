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
#include "vm/vm.h"

struct RuleProfiler {
  LmnRulesetId   ref_rs_id;
  unsigned long  apply;
  unsigned long  backtrack;
  TimeProfiler   trial;
  LmnRuleRef        src;
};

static void mc_profiler2_init(MCProfiler2 *p);
static void mc_profiler2_destroy(MCProfiler2 *p);
static void mc_profiler2_makeup_report(MCProfiler2 *total);
static void mc_profiler3_init(MCProfiler3 *p);
static void mc_profiler3_destroy(MCProfiler3 *p);
static void mc_profiler3_makeup_report(MCProfiler3 *total);
static inline void time_profiler_init(TimeProfiler *p);
static inline void time_profiler_destroy(TimeProfiler *p);
static inline void memory_profiler_init(MemoryProfiler *p);
static inline void memory_profiler_destroy(MemoryProfiler *p);
static inline void peak_counter_init(PeakCounter *p);
static inline void peak_counter_destroy(PeakCounter *p) LMN_UNUSED;
static void profile_state_f(State *s, LmnWord arg);
static const char *profile_space_id_to_name(int type);
static const char *profile_counter_id_to_name(int type);
static const char *profile_time_id_to_name(int type);

/** ----------------------------
 *  Rule Profiler
 */
RuleProfiler *rule_profiler_make(LmnRulesetId id, LmnRuleRef src)
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
static void mc_profiler2_init(MCProfiler2 *p)
{
  p->accept_num            = 0;
  p->invalid_end_num       = 0;
  p->mhash_num             = 0;
  p->rehashed_num          = 0;
  p->midhash_num           = 0;
  p->transition_num        = 0;
  p->statespace_space      = 0;
  p->transition_space      = 0;
  p->state_space           = 0;
  p->binstr_space          = 0;
  p->membrane_space        = 0;
  p->hashes = st_init_numtable();

}

static void mc_profiler2_destroy(MCProfiler2 *p)
{
  if (p->hashes) {
    st_free_table(p->hashes);
  }
}


static void mc_profiler2_makeup_report(MCProfiler2 *total)
{
  unsigned int i;
  memset(total, 0U, sizeof(struct MCProfiler2));
  for (i = 0; i < lmn_prof.thread_num; i++) {
    MCProfiler2 *p = &lmn_prof.lv2[i];
    total->accept_num        += p->accept_num;
    total->invalid_end_num   += p->invalid_end_num;
    total->mhash_num         += p->mhash_num;
    total->rehashed_num      += p->rehashed_num;
    total->midhash_num       += p->midhash_num;
    total->transition_num    += p->transition_num;
    total->statespace_space  += p->statespace_space;
    total->transition_space  += p->transition_space;
    total->state_space       += p->state_space;
    total->binstr_space      += p->binstr_space;
    total->membrane_space    += p->membrane_space;
  }
}

static void mc_profiler3_init(MCProfiler3 *p)
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

static void mc_profiler3_destroy(MCProfiler3 *p)
{
  unsigned int i;

  for (i = 0; i < ARY_SIZEOF(p->spaces); i++) {
    memory_profiler_destroy(&p->spaces[i]);
  }
  for (i = 0; i < ARY_SIZEOF(p->times); i++) {
    time_profiler_destroy(&p->times[i]);
  }
}


static void mc_profiler3_makeup_report(MCProfiler3 *total)
{
  unsigned int data_i, th_id;

  for (data_i = 0; data_i < ARY_SIZEOF(total->times); data_i++) {
    for (th_id = 0; th_id < lmn_prof.thread_num; th_id++) {
      TimeProfiler *p = &(lmn_prof.lv3[th_id].times[data_i]);
      total->times[data_i].called_num += p->called_num;
      total->times[data_i].total_time += p->total_time;
    }
    if (total->times[data_i].total_time > 0.0) {
      total->times[data_i].total_time /= lmn_prof.thread_num;
    }
  }

  for (data_i = 0; data_i < ARY_SIZEOF(total->counters); data_i++) {
    for (th_id = 0; th_id < lmn_prof.thread_num; th_id++) {
      total->counters[data_i] += lmn_prof.lv3[th_id].counters[data_i];
    }
  }

  for (data_i = 0; data_i < ARY_SIZEOF(total->spaces); data_i++) {
    for (th_id = 0; th_id < lmn_prof.thread_num; th_id++) {
      MCProfiler3 *p = &lmn_prof.lv3[th_id];
      total->spaces[data_i].space.cur  += p->spaces[data_i].space.cur;
      total->spaces[data_i].space.peak += p->spaces[data_i].space.peak;
      total->spaces[data_i].num.cur    += p->spaces[data_i].space.cur;
      total->spaces[data_i].num.peak   += p->spaces[data_i].num.peak;
    }
    //total->spaces[data_i].space.cur  /= lmn_prof.thread_num;
    //total->spaces[data_i].space.peak /= lmn_prof.thread_num;
    //total->spaces[data_i].num.cur    /= lmn_prof.thread_num;
    //total->spaces[data_i].num.peak   /= lmn_prof.thread_num;
  }
}

static inline void time_profiler_init(TimeProfiler *p)
{
  p->called_num = 0;
  p->total_time = 0.0;
  p->tmp_start  = 0;
}

static inline void time_profiler_destroy(TimeProfiler *p)
{

}

static inline void memory_profiler_init(MemoryProfiler *p)
{
  peak_counter_init(&p->num);
  peak_counter_init(&p->space);
}

static inline void memory_profiler_destroy(MemoryProfiler *c)
{

}

static inline void peak_counter_init(PeakCounter *p)
{
  p->cur  = 0;
  p->peak = 0;
}

static inline void peak_counter_destroy(PeakCounter *p)
{

}



void lmn_profiler_init(unsigned int nthreads)
{
  unsigned int i;

  lmn_prof.valid                 = FALSE;
  lmn_prof.has_property          = FALSE;
  lmn_prof.found_err             = FALSE;

  lmn_prof.thread_num            = nthreads;
  lmn_prof.start_wall_time       = 0.0;
  lmn_prof.end_wall_time         = 0.0;
  lmn_prof.start_cpu_time        = 0.0;
  lmn_prof.end_cpu_time          = 0.0;
  lmn_prof.start_wall_time_main  = 0.0;
  lmn_prof.end_wall_time_main    = 0.0;
  lmn_prof.start_cpu_time_main   = LMN_NALLOC(double, nthreads);
  lmn_prof.end_cpu_time_main     = LMN_NALLOC(double, nthreads);
  lmn_prof.thread_cpu_time_main  = LMN_NALLOC(double, nthreads);
  for(i = 0; i < nthreads; i++){
    lmn_prof.thread_cpu_time_main[i]=0.0;
  }
  lmn_prof.state_num_stored      = 0;
  lmn_prof.state_num_end         = 0;
  lmn_prof.lv2                   = NULL;
  lmn_prof.lv3                   = NULL;
  lmn_prof.prules                = NULL;
  lmn_prof.cur                   = NULL;

  if (lmn_env.nd) {
    if (lmn_env.profile_level >= 3) {
      lmn_prof.lv3 = LMN_NALLOC(MCProfiler3, nthreads);
      for (i = 0; i < nthreads; i++) {
        mc_profiler3_init(&lmn_prof.lv3[i]);
      }
    }
  }
  else if (lmn_env.profile_level >= 2) {
    lmn_prof.prules = st_init_ptrtable();
  }
}

void lmn_profiler_finalize()
{
  LMN_FREE(lmn_prof.start_cpu_time_main);
  LMN_FREE(lmn_prof.end_cpu_time_main);
  LMN_FREE(lmn_prof.thread_cpu_time_main);

  if (lmn_prof.lv2) {
    mc_profiler2_destroy(lmn_prof.lv2);
  }

  if (lmn_prof.lv3) {
    unsigned int i;
    for (i = 0; i < lmn_prof.thread_num; i++) {
      mc_profiler3_destroy(&lmn_prof.lv3[i]);
    }
    LMN_FREE(lmn_prof.lv3);
  }

  if (lmn_prof.prules) {
    st_foreach(lmn_prof.prules, (st_iter_func)rule_profiler_free_f, (st_data_t)0);
    st_free_table(lmn_prof.prules);
  }
}


void profile_peakcounter_add(PeakCounter *p, unsigned long size)
{
  p->cur += size;
  if (p->cur > p->peak) {
    p->peak = p->cur;
  }
}

void profile_peakcounter_set_v(PeakCounter *p, unsigned long size)
{
  p->cur = size;
  if (p->cur > p->peak) {
    p->peak = p->cur;
  }
}


void profile_total_space_update(StateSpaceRef ss)
{
  unsigned long sum;
  MCProfiler3 *p;
  unsigned int i;

  p = &(lmn_prof.lv3[env_my_thread_id()]);
  sum = 0;
  for (i = 0; i < ARY_SIZEOF(p->spaces); i++) {
    if (i == PROFILE_SPACE__TOTAL ||
        i == PROFILE_SPACE__REDUCED_MEMSET ||
        i == PROFILE_SPACE__REDUCED_BINSTR) continue;
    else sum += p->spaces[i].space.cur;
  }

  sum += statespace_space(ss);
  profile_peakcounter_set_v(&(p->spaces[PROFILE_SPACE__TOTAL].space), sum);
}

void profile_peakcounter_pop(PeakCounter *p, unsigned long size)
{
  p->cur -= size;
}

void profile_add_space(int type, unsigned long size)
{
  MemoryProfiler *p = &(lmn_prof.lv3[env_my_thread_id()].spaces[type]);
  profile_peakcounter_add(&p->num, 1);
  profile_peakcounter_add(&p->space, size);
}

void profile_remove_space(int type, unsigned long size)
{
  if (lmn_prof.valid) { /* finalize処理で現在のメモリ使用量が不明になってしまうので.. */
    MemoryProfiler *p = &(lmn_prof.lv3[env_my_thread_id()].spaces[type]);
    profile_peakcounter_pop(&p->num, 1);
    profile_peakcounter_pop(&p->space, size);
  }
}

void time_profiler_start(TimeProfiler *p)
{
  p->called_num++;
  p->tmp_start = get_cpu_time();
}

void time_profiler_finish(TimeProfiler *p)
{
  p->total_time += get_cpu_time() - p->tmp_start;
}

void profile_start_timer(int type)
{
  TimeProfiler *p = &(lmn_prof.lv3[env_my_thread_id()].times[type]);
  time_profiler_start(p);
}

void profile_finish_timer(int type)
{
  TimeProfiler *p = &(lmn_prof.lv3[env_my_thread_id()].times[type]);
  time_profiler_finish(p);
}

void profile_countup(int type)
{
  MCProfiler3 *p = &lmn_prof.lv3[env_my_thread_id()];
  p->counters[type]++;
}

void profile_count_add(int type, unsigned long num)
{
  lmn_prof.lv3[env_my_thread_id()].counters[type] += num;
}

void profile_rule_obj_set(LmnRuleSetRef src, LmnRuleRef r)
{
  st_data_t t = 0;
  if (st_lookup(lmn_prof.prules, (st_data_t)r, &t)) {
    lmn_prof.cur = (RuleProfiler *)t;
  } else {
    RuleProfiler *p = rule_profiler_make(lmn_ruleset_get_id(src), r);
    lmn_prof.cur = p;
    st_add_direct(lmn_prof.prules, (st_data_t)r, (st_data_t)p);
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
  lmn_prof.start_cpu_time_main[env_my_thread_id()] = get_cpu_time();
}

void profile_finish_exec_thread()
{
  lmn_prof.end_cpu_time_main[env_my_thread_id()] = get_cpu_time();
}

void profile_finish_exec()
{
  lmn_prof.valid = FALSE;
  lmn_prof.end_wall_time_main = get_wall_time();
}

void profile_statespace(LmnWorkerGroup *wp)
{
  LmnWorker *w = workers_get_worker(wp, LMN_PRIMARY_ID);
  lmn_prof.state_num_stored = statespace_num(worker_states(w));
  lmn_prof.state_num_end    = statespace_end_num(worker_states(w));

  if (lmn_env.profile_level >= 3 && lmn_env.tree_compress) {
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, (2 << lmn_env.tree_compress_table_size) * sizeof(TreeNodeRef));
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, sizeof(struct TreeDatabase));
    profile_total_space_update(worker_states(w));
  }
  if (lmn_env.profile_level >= 2) {
    MCProfiler2 *total;
    unsigned int i;

    total = LMN_MALLOC(MCProfiler2);
    total->hashes = NULL;

    lmn_prof.lv2 = LMN_NALLOC(MCProfiler2, lmn_prof.thread_num);
    for (i = 0; i < lmn_prof.thread_num; i++) {
      mc_profiler2_init(&lmn_prof.lv2[i]);
    }
    statespace_foreach(worker_states(w), (void (*)())profile_state_f,
                       (LmnWord)worker_states(w), DEFAULT_ARGS);

    if (lmn_env.tree_compress) {
      MCProfiler2 *p = &lmn_prof.lv2[lmn_OMP_get_my_id()];
      p->binstr_space += lmn_bscomp_tree_space();
    }
    mc_profiler2_makeup_report(total);
    for (i = 0; i < lmn_prof.thread_num; i++) {
      mc_profiler2_destroy(&lmn_prof.lv2[i]);
    }
    LMN_FREE(lmn_prof.lv2);

    total->statespace_space = statespace_space(worker_states(w));
    lmn_prof.lv2 = total;
  }
}

static void profile_state_f(State *s, LmnWord arg)
{
  MCProfiler2 *p;
  StateSpaceRef ss;
  unsigned int succ_num;

  p  = &lmn_prof.lv2[lmn_OMP_get_my_id()];
  ss = (StateSpaceRef)arg;
  succ_num = state_succ_num(s);

  /* メモリ */
  p->state_space += sizeof(State);
  if (!is_binstr_user(s) && state_mem(s)) {
    p->membrane_space += lmn_mem_root_space(state_mem(s));
  }
  else if (is_binstr_user(s) && state_binstr(s)) {
    p->binstr_space   += lmn_binstr_space(state_binstr(s));
  }

  p->transition_space += succ_num * sizeof(succ_data_t);/* # of pointer*/
  if (lmn_env.show_transition) {
    unsigned int i;
    for (i = 0; i < succ_num; i++) {
      p->transition_space += transition_space(transition(s, i));
    }
  }

  /* 遷移数 */
  p->transition_num += succ_num;

  if (!(is_encoded(s) && is_dummy(s))) {
    if (statespace_has_property(ss)) {
      AutomataRef a = statespace_automata(ss);
      if (state_is_accept(a, s)) {
        p->accept_num++;
      }
      if (state_is_end(a, s)) {
        p->invalid_end_num++;
      }
    }
  }

  /* ハッシュ値の種類数 */
  if (!st_contains(p->hashes, (st_data_t)state_hash(s))) {
    st_insert(p->hashes, (st_data_t)state_hash(s), 0);

    if (is_encoded(s)) {
      p->midhash_num++;
      if (is_dummy(s)) {
        p->rehashed_num++;
      }
    } else {
      p->mhash_num++;
    }
  }

  if (!s->next) {
    /* 同じハッシュ値は同じリストにのみ存在するので, リストが切り替わったらクリア */
    st_clear(p->hashes);
  }
}


static void dump_execution_stat(FILE *f)
{
  const char *profile, *timeopt, *tcmalloc, *debug;

#ifdef PROFILE
  profile = "ON";
#else
  profile = "OFF";
#endif
  timeopt = "ON";
#ifdef HAVE_TCMALLOC
  tcmalloc = "ON";
#else
  tcmalloc = "OFF";
#endif
#ifdef DEBUG
  debug    = "ON";
#else
  debug    = "OFF";
#endif

  fprintf(f, "\n== %-20s ====================================\n"
           , lmn_env.nd ? "Verification Mode" : "Simulation Mode");
  fprintf(f, "%-9s: ", "SYSTEM");
  slim_version(f);
  fprintf(f, "---------:--------------------------------------------------\n");
  fprintf(f, "%-9s: %-8s=%6s  %-8s=%6s  %-8s=%6s\n"
           , "CONFIGURE"
           , "profile"  , profile
           , "timeopt"  , timeopt
           , "tcmalloc" , tcmalloc);
  fprintf(f, "%-9s: %-8s=%6s\n"
           , ""
           , "debug"    , debug);
  if (lmn_env.nd) {
    const char *strategy, *expr, *heuristic;

    if (lmn_env.bfs) {
      strategy = lmn_env.bfs_layer_sync ? "LSync" : "BFS";
    } else {
      strategy = "DFS";
    }

    if (!lmn_env.ltl) {
      expr = "NONE";
      heuristic = "NONE";
    }
    else {
      if (lmn_env.enable_map) {
        expr = "MAP";
      } else if (lmn_env.enable_owcty) {
        expr = "OWCTY";
      } else if (lmn_env.enable_bledge) {
        expr = "BLE";
      } else if(lmn_env.enable_mapndfs) {
        expr = "MAPNDFS";
#ifndef MINIMAL_STATE
      } else if(lmn_env.enable_mcndfs) {
        expr = "MCNDFS";
#endif
      } else if (lmn_prof.thread_num == 1 && !lmn_env.enable_parallel) {
        expr = "NDFS";
      } else {
        expr = "Unk.";
      }

      if (lmn_env.enable_map_heuristic) {
        heuristic = "MAP";
      } else {
        heuristic = "NONE";
      }
    }

    fprintf(f,"---------:--------------------------------------------------\n");
    fprintf(f, "%-9s: %-9s=%5u  %-8s=%6s  %-8s=%6s\n"
             , "PALLAREL"
             , "workers"  , lmn_prof.thread_num
             , "strtgy"   , strategy
             , "loadBal." , lmn_env.optimize_loadbalancing ? "OPT" : "ORG");
    fprintf(f, "%-9s: %-9s=%5s  %-8s=%6s  %-8s=%6s\n"
             , "GENERATOR"
             , "mem2bs"   , lmn_env.enable_compress_mem ? "ON" : "OFF"
             , "compact"  , lmn_env.enable_compress_mem ? "AUTO" : "OFF"
             , "rehashr"  , lmn_env.optimize_hash ? "ON" : "OFF");
    fprintf(f, "%-9s: %-9s=%5s  %-8s=%6s  %-8s=%6s\n"
             , ""
             , "mem2id"   , lmn_env.mem_enc       ? "ON" : "OFF"
             , "mdelta"   , lmn_env.delta_mem     ? "ON" : "OFF"
             , "p.o.r."   , lmn_env.enable_por    ? "ON" : "OFF");
    fprintf(f, "%-9s: %-9s=%5s  %-8s=%6s  %-8s=%5s\n"
             , ""
             , "bsZcomp.", lmn_env.z_compress        ? "ON" : "OFF"
             , "bsDcomp.", lmn_env.d_compress        ? "ON" : "OFF"
             , "hashcomp.", lmn_env.hash_compaction ? "ON" : "OFF");
    fprintf(f, "%-9s: %-8s=%5s\n"
             , ""
             , "treecomp.", lmn_env.tree_compress   ? "ON" : "OFF");
    fprintf(f, "%-9s: %-9s=%5s  %-8s=%6s\n"
             , "EXPLORER"
             , "strtgy"  , expr
	     , "heurstc" , heuristic);
  }
  fprintf(f,  "============================================================\n");
  fprintf(f,  "\n");
}


void dump_profile_data(FILE *f)
{
  double tmp_total_cpu_time,  tmp_total_cpu_time_main;
  double tmp_total_wall_time, tmp_total_wall_time_main;
  double tmp_total_mem;
  unsigned long total_hash_num;
  unsigned int i;

  tmp_total_cpu_time_main  = 0.0;
  for (i = 0; i < lmn_prof.thread_num; i++) {
    tmp_total_cpu_time_main  += lmn_prof.end_cpu_time_main[i]
                              - lmn_prof.start_cpu_time_main[i]
                              + lmn_prof.thread_cpu_time_main[i];
  }
  tmp_total_cpu_time_main  = tmp_total_cpu_time_main / lmn_prof.thread_num;
  tmp_total_cpu_time       = lmn_prof.end_cpu_time  - lmn_prof.start_cpu_time;
  tmp_total_wall_time      = lmn_prof.end_wall_time - lmn_prof.start_wall_time;
  tmp_total_wall_time_main = lmn_prof.end_wall_time_main
                           - lmn_prof.start_wall_time_main;

  if (lmn_prof.lv2) {
    tmp_total_mem = (double)(lmn_prof.lv2->state_space
                             + lmn_prof.lv2->transition_space
                             + lmn_prof.lv2->binstr_space
                             + lmn_prof.lv2->membrane_space
                             + lmn_prof.lv2->statespace_space);
    total_hash_num = lmn_prof.lv2->mhash_num + lmn_prof.lv2->midhash_num
                                             - lmn_prof.lv2->rehashed_num;
  } else {
    tmp_total_mem  = 0;
    total_hash_num = 0;
  }

  if (lmn_env.benchmark) { /* データ収集用 */
    if (lmn_env.ltl) {
      fprintf(f, "%lf\t%lf\t%lf\t%lu\t%lu\t%lu\t%lu\t%lf\t%lf\t%lf\t%lf\t%lf\t%s\n"
          , tmp_total_wall_time
          , tmp_total_wall_time_main
          , tmp_total_cpu_time_main
          , lmn_prof.state_num_stored
          , lmn_prof.lv2->transition_num
          , lmn_prof.state_num_end
          , total_hash_num
          , tmp_total_mem / 1024 / 1024
          , (double)lmn_prof.lv2->state_space / 1024 / 1024
          , lmn_env.enable_compress_mem
          ? (double)lmn_prof.lv2->binstr_space   / 1024 / 1024
          : (double)lmn_prof.lv2->membrane_space / 1024 / 1024
          , (double)lmn_prof.lv2->transition_space / 1024 / 1024
          , (double)lmn_prof.lv2->statespace_space / 1024 / 1024
          , lmn_prof.found_err ? "FOUND" : "NOT FOUND"
          );
    } else {
      fprintf(f, "%lf\t%lf\t%lf\t%lu\t%lu\t%lu\t%lu\t%lf\t%lf\t%lf\t%lf\t%lf\n"
          , tmp_total_wall_time
          , tmp_total_wall_time_main
          , tmp_total_cpu_time_main
          , lmn_prof.state_num_stored
          , lmn_prof.lv2->transition_num
          , lmn_prof.state_num_end
          , total_hash_num
          , tmp_total_mem / 1024 / 1024
          , (double)lmn_prof.lv2->state_space / 1024 / 1024
          , lmn_env.enable_compress_mem
          ? (double)lmn_prof.lv2->binstr_space   / 1024 / 1024
          : (double)lmn_prof.lv2->membrane_space / 1024 / 1024
          , (double)lmn_prof.lv2->transition_space / 1024 / 1024
          , (double)lmn_prof.lv2->statespace_space / 1024 / 1024
          );
    }
    return;
  }

  if (lmn_env.profile_level >= 1) {
    if (lmn_env.profile_level >= 2) {
      dump_execution_stat(f);
    }

    fprintf(f, "\n== Static Profiler Report ==================================\n");
    fprintf(f, "%-20s%8s  : %15.2lf\n", "Wall Time (sec)",        "Total", tmp_total_wall_time);
    fprintf(f, "%-20s%8s  : %15.2lf\n", " ",                      " Exec", tmp_total_wall_time_main);
    fprintf(f,   "------------------------------------------------------------\n");

    if(!lmn_env.nd && lmn_env.enable_parallel){
      fprintf(f, "%-20s%8s  : %15.2lf\n", "CPU Usage (sec)",      "Main", tmp_total_cpu_time);
      if (lmn_prof.thread_num == 1) {
	//child_thread == 1
	fprintf(f, "%-20s%8s  : %15.2lf\n", " ",                    "Sub", lmn_prof.thread_cpu_time_main[0]);
      }else{
	//child_thread > 1
#ifdef HAVE_LIBRT
	//fprintf(f, "%-18s%10s  : %15.2lf\n", " ", "Exec Avg.", tmp_total_cpu_time_main);
      fprintf(f, "%-18s%10s  : %15s\n",    " ",           "---------", "--------------------------");
      for (i = 0; i < lmn_prof.thread_num; i++) {
        fprintf(f, "%-12s%13s%3u  : %15.2lf\n", " ", "Thread", i
                 , lmn_prof.thread_cpu_time_main[i]);
      }
#else
      //fprintf(f, "%-18s%10s  : %15.2lf\n", "CPU Usage (sec)", "Exec Avg."
      //         , tmp_total_cpu_time_main / lmn_prof.thread_num);
#endif
      }
    }else if (lmn_prof.thread_num == 1) {
      fprintf(f, "%-20s%8s  : %15.2lf\n", "CPU Usage (sec)",      "Total", tmp_total_cpu_time);
      fprintf(f, "%-20s%8s  : %15.2lf\n", " ",                    " Exec", tmp_total_cpu_time_main);
    } else {
#ifdef HAVE_LIBRT
      fprintf(f, "%-18s%10s  : %15.2lf\n", "CPU Usage (sec)", "Exec Avg.", tmp_total_cpu_time_main);
      fprintf(f, "%-18s%10s  : %15s\n",    " ",           "---------", "-------------------");
      for (i = 0; i < lmn_prof.thread_num; i++) {
        fprintf(f, "%-12s%13s%3u  : %15.2lf\n", " ", "Thread", i
		, lmn_prof.end_cpu_time_main[i] - lmn_prof.start_cpu_time_main[i]);
      }
#else
      fprintf(f, "%-18s%10s  : %15.2lf\n", "CPU Usage (sec)", "Exec Avg."
               , tmp_total_cpu_time_main / lmn_prof.thread_num);
#endif
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
        fprintf(f,   "%4s %8s : %9s %9s %9s %12s"
                 , "[id]", "[name]", "[# Tr.]", "[# Ap.]", "[# Ba.]", "[CPU U.(usec)]\n");

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
              fprintf(f, "@%-3d %8.8s : %9lu %9lu %9lu %13.1lf\n"
                       , rp->ref_rs_id
                       , lmn_id_to_name(lmn_rule_get_name(rp->src))
                       , rp->trial.called_num
                       , rp->apply
                       , rp->backtrack
                       , rp->trial.total_time / 1e-6);
            }
            r_total->apply            += rp->apply;
            r_total->backtrack        += rp->backtrack;
            r_total->trial.called_num += rp->trial.called_num;
            r_total->trial.total_time += rp->trial.total_time;
          }
        }
        fprintf(f, "%4s %8s : %9lu %9lu %9lu %13.1lf\n"
                 , " - "
                 , "OTHERS"
                 , r_others->trial.called_num
                 , 0UL
                 , r_others->backtrack
                 , r_others->trial.total_time / 1e-6);
        fprintf(f,   "------------------------------------------------------------\n");
        fprintf(f, "%4s %8s : %9lu %9lu %9lu %13.1lf\n"
                 , " - "
                 , "Total"
                 , r_total->trial.called_num
                 , r_total->apply
                 , r_total->backtrack
                 , r_total->trial.total_time / 1e-6);

        vec_destroy(&v);
        rule_profiler_free(r_others);
        fprintf(f,   "============================================================\n");
      }
    } else if (lmn_env.profile_level < 2) {
      fprintf(f,   "------------------------------------------------------------\n");
      fprintf(f, "%-20s%8s  : %15lu\n",   "# of States",             "Stored", lmn_prof.state_num_stored);
      fprintf(f, "%-18s%10s  : %15lu\n",   " ",                  "Terminates", lmn_prof.state_num_end);
      if (lmn_prof.has_property) {
        fprintf(f, "%-1s%27s  : %15s\n",    " ",   "Accepting Cycle / Error", lmn_prof.found_err ? "FOUND" : "NOT FOUND");
      }
      fprintf(f,   "============================================================\n");
    } else {
      fprintf(f,   "------------------------------------------------------------\n");
      fprintf(f, "%-20s%8s  : %15lu\n",   "# of States",             "Stored", lmn_prof.state_num_stored);
      fprintf(f, "%-18s%10s  : %15lu\n",   " ",                  "Successors", lmn_prof.lv2->transition_num);
      fprintf(f, "%-18s%10s  : %15lu\n",   " ",                  "Terminates", lmn_prof.state_num_end);
      if (lmn_prof.has_property) {
        fprintf(f, "%-10s%18s  : %15lu\n",   " ",                  "Accepted", lmn_prof.lv2->accept_num);
        fprintf(f, "%-10s%18s  : %15lu\n",   " ",              "Invalid Ends", lmn_prof.lv2->invalid_end_num);
        fprintf(f, "%-10s%18s  : %15s\n",    " ",           "Accepting Cycle", lmn_prof.found_err ? "FOUND" : "NOT FOUND");
      }
      fprintf(f,   "------------------------------------------------------------\n");
      fprintf(f, "%-20s%8s  : %15lu\n",   "# of Hash Values",         "Total", total_hash_num);
      fprintf(f, "%-6s%22s  : %15lu\n",   " ",            "Default -  M_Hash", lmn_prof.lv2->mhash_num);
      fprintf(f, "%-6s%22s  : %15lu\n",   " ",           "ReHashed -  M_Hash", lmn_prof.lv2->rehashed_num);
      fprintf(f, "%-6s%22s  : %15lu\n",   " ",          "Optimized - BS_Hash", lmn_prof.lv2->midhash_num);
      fprintf(f,   "------------------------------------------------------------\n");
      fprintf(f, "%-16s%12s    %12s %12s\n", "Memory Usage ", "", "[Amount(MB)]", "[Per State(B)]");
      fprintf(f, "%-10s%18s  : %12.2lf %12.2lf\n", " ",             "Total"
               , tmp_total_mem / 1024 / 1024, tmp_total_mem / lmn_prof.state_num_stored);
      fprintf(f, "%-10s%18s  : %12.2lf %12.2lf\n", " ", "State Descriptors"
               , (double)lmn_prof.lv2->state_space / 1024 / 1024
               , (double)lmn_prof.lv2->state_space / lmn_prof.state_num_stored);
      if (lmn_env.enable_compress_mem) {
        fprintf(f, "%-10s%18s  : %12.2lf %12.2lf\n", " ",  "Binary Strings"
                 , (double)lmn_prof.lv2->binstr_space / 1024 / 1024
                 , (double)lmn_prof.lv2->binstr_space / lmn_prof.state_num_stored);
      } else {
        fprintf(f, "%-10s%18s  : %12.2lf %12.2lf\n", " ", "State Membranes"
                 , (double)lmn_prof.lv2->membrane_space / 1024 / 1024
                 , (double)lmn_prof.lv2->membrane_space / lmn_prof.state_num_stored);
      }
      fprintf(f, "%-10s%18s  : %12.2lf %12.2lf\n", " ",       "Transitions"
               , (double)lmn_prof.lv2->transition_space / 1024 / 1024
               , (double)lmn_prof.lv2->transition_space / lmn_prof.state_num_stored);
      fprintf(f, "%-10s%18s  : %12.2lf %12.2lf\n", " ",        "StateSpace"
               , (double)lmn_prof.lv2->statespace_space / 1024 / 1024
               , (double)lmn_prof.lv2->statespace_space / lmn_prof.state_num_stored);
      fprintf(f,   "============================================================\n");

      if (lmn_env.profile_level >= 3) {
        MCProfiler3 total;
        double total_time;

        for (i = 0; i < lmn_prof.thread_num; i++) { /* 計測したactive時間をidle時間へ変換 */
          lmn_prof.lv3[i].times[PROFILE_TIME__ACTIVE_FOR_IDLE_PROF].total_time =
            lmn_prof.thread_num == 1 ? 0 :
               lmn_prof.end_cpu_time_main[i]
             - lmn_prof.start_cpu_time_main[i]
             - lmn_prof.lv3[i].times[PROFILE_TIME__ACTIVE_FOR_IDLE_PROF].total_time;
          /* startからfinishまで回したCPU時間から指定したactiveブロックに費やしたCPU時間を引けばidle時間になる.
           * (一見冗長に見えるが, spin-waitが費やしたCPU時間を除くことができる.) */
        }

        mc_profiler3_init(&total);
        mc_profiler3_makeup_report(&total);
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
        fprintf(f,   "%-24s:%10s%10.2lf%8.1lf\n"
                 , lmn_prof.thread_num > 2 ? "CPU Usage AVG. (sec)"
                                           : "CPU Usage (sec)"
                 , ""
                 , tmp_total_cpu_time_main
                 , 100.0);
        fprintf(f,   "------------------------------------------------------------\n");
        fprintf(f, "\n");
        fprintf(f,   "-- Memory Performance --------------------------------------\n");
        fprintf(f, "%-24s  %10s %10s %10s\n", " ", "[Fin.(MB)]", "[Peak(MB)]", "[Peak Num]");
        for (i = 0; i < ARY_SIZEOF(total.spaces); i++) {
          if (lmn_prof.thread_num >= 2) {
            fprintf(f, "%-24s: %10.2lf\n"
                , profile_space_id_to_name(i)
                , (double)total.spaces[i].space.cur / 1024 /1024);
          } else {
            fprintf(f, "%-24s: %10.2lf %10.2lf %10lu\n"
                , profile_space_id_to_name(i)
                , (double)total.spaces[i].space.cur  / 1024 / 1024
                , (double)total.spaces[i].space.peak / 1024 / 1024
                , total.spaces[i].num.peak);
          }
        }
        fprintf(f,   "------------------------------------------------------------\n");
        fprintf(f, "\n");
        if (lmn_env.tree_compress) {
          fprintf(f,  "-- Tree Compressin Info ------------------------------------\n");
          lmn_bscomp_tree_profile(f);
          fprintf(f,   "------------------------------------------------------------\n");
          fprintf(f, "\n");
        }
        fprintf(f,   "-- State Management System (Open Hashing) ------------------\n");
        for (i = 0; i < ARY_SIZEOF(total.counters); i++) {
          fprintf(f, "%-24s:%10lu\n", profile_counter_id_to_name(i), total.counters[i]);
        }
        fprintf(f,   "============================================================\n");
      }
    }
  }
}


static const char *profile_time_id_to_name(int type)
{
  const char *ret;
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
  case PROFILE_TIME__D_COMPRESS:
    ret = "d compress";
    break;
  case PROFILE_TIME__D_UNCOMPRESS:
    ret = "d uncompress";
    break;
  case PROFILE_TIME__TREE_COMPRESS:
    ret = "tree compress";
    break;
  case PROFILE_TIME__TREE_UNCOMPRESS:
    ret = "tree uncompress";
    break;
  case PROFILE_TIME__COST_UPDATE:
    ret = "cost update";
    break;
  case PROFILE_TIME__LOCK:
    ret = "lock";
    break;
  case PROFILE_TIME__REPAIR:
    ret = "repair phase";
    break;
  default:
    ret = "unknown";
    break;
  }
  return ret;
}

static const char *profile_counter_id_to_name(int type)
{
  const char *ret;
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

static const char *profile_space_id_to_name(int type)
{
  const char *ret;
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

