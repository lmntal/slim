/*
 * lmntal.h - global header file
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
 * $Id: lmntal.h,v 1.13 2008/10/17 07:36:32 riki Exp $
 */

#ifndef LMNTAL_H
#define LMNTAL_H

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <vector>

#include "config.h"

#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif

#ifdef HAVE_INTTYPES_H
#include <cinttypes>
#endif

#define LMN_EXTERN extern

#if defined(__GNUG__) || defined(__clang__)
#define LMN_UNUSED __attribute__((unused))
#else
#define LMN_UNUSED
#endif

/*------------------------------------------------------------------------
 *  Some useful macros
 */

using BOOL           = unsigned char;
constexpr BOOL TRUE  = 1;
constexpr BOOL FALSE = 0;

/* This defines several auxiliary routines that are useful for debugging */
#ifndef LMN_DEBUG_HELPER
#define LMN_DEBUG_HELPER TRUE
#endif

#if SIZEOF_DOUBLE <= SIZEOF_UINTPTR_T
#define LMN_DOUBLE_IS_IMMEDIATE 1
#endif

/*----------------------------------------------------------------------
 * data types
 */

#if SIZEOF_UINTPTR_T < SIZEOF_VOIDP
#error sizeof(intptr_t) < sizeof(void*)
#endif

using LmnWord = uintptr_t;
using BYTE    = unsigned char;
using LmnByte = unsigned char;

// Word size
constexpr auto                       LMN_WORD_BYTES = SIZEOF_LONG;
constexpr auto                       LMN_WORD_BITS  = LMN_WORD_BYTES * 8;
template <typename T> constexpr auto LMN_WORD(T X) { return (LmnWord)((X)); }

using LmnAtom  = LmnWord;
using LmnSAtom = void *;
/**
 * @struct LmnLinkAttr
 */
using LmnLinkAttr = uint8_t;

// using ProcessID = uint16_t;
using ProcessID = LmnWord;

using LmnCost = LmnWord;

/* uint16_t is not defined if there is no 2Byte data type */
using LmnFunctor                                       = uint16_t;
constexpr auto                       LMN_FUNCTOR_BYTES = sizeof(LmnFunctor);
constexpr auto                       LMN_FUNCTOR_BITS  = LMN_FUNCTOR_BYTES * 8;
template <typename T> constexpr auto LMN_FUNCTOR(T X) { return (LmnFunctor)((X)); }

/* this type must be enough to represent arity */
using LmnArity = uint8_t;

using lmn_interned_str = unsigned int;

using LmnRuleInstr    = BYTE *;
using LmnInstrOp      = uint16_t;
using LmnInstrVar     = uint16_t;
using LmnJumpOffset   = uint16_t;
using LmnLineNum      = uint32_t;
using LmnRulesetId    = int16_t;
using LmnSubInstrSize = uint32_t;

// using LmnMembrane = struct LmnMembrane;
// using DeltaMembrane = struct DeltaMembrane;

static_assert(LMN_WORD_BYTES == 4 || LMN_WORD_BYTES == 8, "Word size is not 2^N");
constexpr auto LMN_WORD_SHIFT = LMN_WORD_BYTES == 4 ? 2 : 3;

#ifndef HAVE___INT64
#ifdef HAVE_LONG_LONG_INT
using __int64 = long long;
#define HAVE___INT64
#endif
#endif

/*----------------------------------------------------------------------
 * Special Atom
 */

struct LmnSPAtomHeader {
  LmnByte type;

  LmnSPAtomHeader() = default;
  LmnSPAtomHeader(LmnByte type) : type(type) {}
};

/* スペシャルアトムは構造体の最初の要素としてに必ずこのヘッダを含めなけ
   ればならない */
#define LMN_SP_ATOM_HEADER struct LmnSPAtomHeader hdr

/*----------------------------------------------------------------------
 * Hyperlink
 */

using LmnHlinkRank = uint32_t;

/*----------------------------------------------------------------------
 * Mem Stack
 */

struct Vector;

/* ---------------------------------------------------------------------
 * for Model Checking
 */

using StateSpaceRef = struct StateSpace *;
using StateTable    = struct StateTable;
using State         = struct State;
using TransitionRef = struct Transition *;
using McDporData    = struct McDporData;
using MemDeltaRoot  = struct MemDeltaRoot;

/*----------------------------------------------------------------------
 * Utility
 */

/*  Memory */

LMN_EXTERN void *lmn_calloc(size_t num, size_t size);
LMN_EXTERN void *lmn_malloc(size_t num);
LMN_EXTERN void *lmn_realloc(void *p, size_t num);
LMN_EXTERN void  lmn_free(void *p);

template <typename T> auto LMN_NALLOC(size_t num) -> T * { return (T *)lmn_malloc(sizeof(T) * (num)); }

template <typename T> auto LMN_CALLOC(size_t num) -> T * { return (T *)lmn_calloc((num), sizeof(T)); }

template <typename T> auto LMN_MALLOC() -> T * { return (T *)lmn_malloc(sizeof(T)); }

template <typename T> auto LMN_REALLOC(T *p, size_t num) -> T * { return (T *)lmn_realloc((p), (num) * sizeof(T)); }

template <typename T> void LMN_FREE(T p) { lmn_free((void *)(p)); }

/* Assertion */
#ifdef DEBUG
#include <cassert>
#define LMN_ASSERT(expr) assert(expr)
#else
#define LMN_ASSERT(expr) ((void)0) /* nothing */
#endif

/*----------------------------------------------------------------------
 * Global data
 */

/* 階層グラフ構造の出力形式 */
enum OutputFormat { DEFAULT = 1, DEV, DOT, JSON };
enum MCdumpFormat { CUI, LaViT, Dir_DOT, LMN_FSM_GRAPH, LMN_FSM_GRAPH_MEM_NODE, LMN_FSM_GRAPH_HL_NODE };
enum SPdumpFormat { SP_NONE, INCREMENTAL, LMN_SYNTAX };

/* 最適化実行 */
enum OptimizeMode { OPT_NONE, OPT_MINIMIZE, OPT_MAXIMIZE };

struct LmnEnv {
  BOOL trace;
  BOOL show_proxy;
  BOOL show_ruleset;
  BOOL show_chr;

  BOOL nd;
  BOOL ltl;
  BOOL ltl_all;
  BOOL show_transition;

  BOOL enable_por; /* to enable partial order reduction for nondeterministic
                      execution or LTL model checking */
  BOOL enable_por_old;
  BYTE optimization_level;
  BYTE profile_level;

  BOOL translate;
  BOOL bfs;
  BOOL mem_enc;
  BOOL enable_compress_mem;

  unsigned int depth_limits;
  unsigned int core_num;
  unsigned int cutoff_depth;

  BOOL delta_mem;
  BOOL z_compress;
  BOOL d_compress;
  BOOL r_compress;

  BOOL prop_scc_driven;
  BOOL property_dump;
  BOOL enable_parallel;
  BOOL optimize_loadbalancing;

  BOOL optimize_lock;
  BOOL optimize_hash;
  BOOL dump;
  BOOL end_dump;

  BOOL enable_owcty;
  BOOL enable_map;
  BOOL enable_map_heuristic;
  BOOL enable_bledge;
  BOOL enable_mapndfs;
#ifndef MINIMAL_STATE
  BOOL enable_mcndfs;
#endif

  BOOL enable_visualize;

  BOOL show_reduced_graph;
  BOOL bfs_layer_sync;
  BOOL interactive;
  BOOL normal_remain;

  BOOL normal_remaining;
  BOOL normal_cleaning;
  BOOL nd_remain;
  BOOL nd_remaining;

  BOOL nd_cleaning;
  BOOL nd_search_end;
  BOOL hyperlink;
  BOOL show_hyperlink;

  BOOL benchmark;

  BOOL hash_compaction;
  int  hash_depth;

  BOOL         tree_compress;
  unsigned int tree_compress_table_size;

  // #ifdef PROFILE
  BOOL optimize_hash_old;
  BOOL prof_no_memeq;
  // #endif

  // findatom最適化オプション（変数名は仮置き）
  BOOL history_management;

#ifdef DEBUG
  BOOL debug_isomor;
  BOOL debug_delta;
  BOOL debug_id;
  BOOL debug_hash;
  BOOL debug_mc;
  BOOL debug_por;
  BOOL debug_por_dep;
#endif

  BOOL findatom_parallel_mode;
  BOOL find_atom_parallel;
  BOOL findatom_parallel_inde;

  BOOL run_test;

  BOOL shuffle_rule;
  BOOL shuffle_atom;

  BOOL interactive_debug;

  BOOL show_laststep_only;

  enum OutputFormat output_format;
  enum MCdumpFormat mc_dump_format;
  enum SPdumpFormat sp_dump_format;
  enum OptimizeMode opt_mode;

  int         load_path_num;
  char const *load_path[256];
  char       *automata_file;        /* never claim file */
  char       *propositional_symbol; /* file for propositional symbol definitions */
  char       *ltl_exp;

  // member methods
  LmnEnv();
};

/*----------------------------------------------------------------------
 * Others
 */
void slim_version(FILE *f);

/* check for thread library */

#if defined(HAVE_OMP_H) && !defined(__APPLE__)
#include <omp.h>
#define ENABLE_OMP
static inline auto lmn_OMP_set_thread_num(int N) { return omp_set_num_threads(N); }
static inline auto lmn_OMP_get_my_id() { return omp_get_thread_num(); }
#else
#define lmn_OMP_set_thread_num(N)
#define lmn_OMP_get_my_id() (0U)
#endif

#if defined(HAVE_LIBPTHREAD) || defined(HAVE_WINAPI)
#define HAVE_MT_LIBRARY (1)
#
#ifdef HAVE_LIBPTHREAD
#include <pthread.h>
#else /* HAVE_LIBPTHREAD */
#include <windows.h>
#endif /* HAVE_LIBPTHREAD */
#
#else /* HAVE_LIBPTHREAD or HAVE_WINAPI */
#undef HAVE_MT_LIBRARY
#endif /* HAVE_LIBPTHREAD or HAVE_WINAPI */

/* setting thread library for slim */
#ifndef HAVE_LIBPTHREAD
#error "sorry, need pthread.h or implementation for other thread library "
#
#else /* defined (HAVE_LIBPTHREAD) */
#
using lmn_thread_t  = pthread_t;
using lmn_mutex_t   = pthread_mutex_t;
using lmn_key_t     = pthread_key_t;
#
#ifdef HAVE_PTHREAD_BARRIER
using lmn_barrier_t = pthread_barrier_t;
#else /* !defined (HAVE_PTHREAD_BARRIER) */
#
using lmn_barrier_t = struct LmnBarrier;
struct LmnBarrier {
  unsigned int    thread_num;
  unsigned int    reach_num;
  pthread_mutex_t mutex;
  pthread_cond_t  cond;
};
#
#endif /* HAVE_PTHREAD_BARRIER */
#define lmn_thread_create(Pth, Pfunc, Parg) pthread_create(Pth, NULL, (void *(*)(void *))(Pfunc), (void *)(Parg))
#define lmn_thread_join(Th) pthread_join(Th, NULL)
#define lmn_mutex_init(Pm) pthread_mutex_init(Pm, NULL)
#define lmn_mutex_init_onthefly(Pm) (Pm) = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER
#define lmn_mutex_destroy(Pm) pthread_mutex_destroy(Pm)
#define lmn_mutex_lock(Pm) pthread_mutex_lock(Pm)
#define lmn_mutex_unlock(Pm) pthread_mutex_unlock(Pm)
#define lmn_TLS_key_init(Pk) pthread_key_create(Pk, NULL)
#define lmn_TLS_key_destroy(K) pthread_key_delete(K)
#define lmn_TLS_set_value(K, Pval) pthread_setspecific(K, Pval)
#define lmn_TLS_get_value(K) pthread_getspecific(K)
#
#ifdef HAVE_PTHREAD_BARRIER
#define lmn_barrier_init(Pm, Num) pthread_barrier_init(Pm, NULL, Num)
#define lmn_barrier_destroy(Pm) pthread_barrier_destroy(Pm)
#define lmn_barrier_wait(Pm) pthread_barrier_wait(Pm)
#else
static inline void lmn_barrier_init(lmn_barrier_t *b, unsigned int num) {
  b->thread_num = num;
  b->reach_num  = 0;
  b->mutex      = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
  b->cond       = (pthread_cond_t)PTHREAD_COND_INITIALIZER;
}
#
static inline void lmn_barrier_destroy(lmn_barrier_t *b) {
  pthread_mutex_destroy(&b->mutex);
  pthread_cond_destroy(&b->cond);
}
#
static inline void lmn_barrier_wait(lmn_barrier_t *b) {
  pthread_mutex_lock(&b->mutex);
  b->reach_num++;
  if (b->reach_num != b->thread_num) {
    pthread_cond_wait(&b->cond, &b->mutex);
  } else { /* ok! */
    b->reach_num = 0;
    pthread_cond_broadcast(&b->cond);
  }
  pthread_mutex_unlock(&b->mutex);
}
#
#
#endif /* HAVE_PTHREAD_BARRIER */
#endif /* HAVE_LIBPTHREAD */

/* mac osXのxcode gccではthread local
 * storageを容易に実現する__threadは実装予定がない.
 * (osXの都合で実装が難しいらしい?)
 * 代わりにpthread keyによるthread local
 * storageの実装が優れているらしいの使ってみた */

#if defined(HAVE_MT_LIBRARY) && defined(HAVE___THREAD)
#define USE_TLS_KEYWORD
#define LMN_TLS_TYPE(T) thread_local T
#define ENABLE_PARALLEL
#
#elif defined(HAVE_LIBPTHREAD) && defined(__APPLE__)
#define USE_TLS_PTHREAD_KEY
#define LMN_TLS_TYPE(T) lmn_key_t
#define ENABLE_PARALLEL
#
#else /* disable prallel */
#define LMN_TLS_TYPE(T) T
#endif

using LmnTLS = struct LmnTLS;
struct LmnTLS {
  unsigned int  thread_num;
  unsigned int  thread_id;
  unsigned long state_id;
  ProcessID     proc_next_id;
};

extern std::vector<LmnWord> *lmn_id_pool;
extern struct LmnEnv         lmn_env;
extern LMN_TLS_TYPE(LmnTLS) lmn_tls;

void env_my_TLS_init(unsigned int th_id);
void env_my_TLS_finalize();
void lmn_stream_init();
void lmn_stream_destroy();

#define LMN_PRIMARY_ID (0U)

static inline auto env_proc_id_pool() { return lmn_id_pool; }
static inline auto env_set_proc_id_pool(std::vector<LmnWord> *v) { lmn_id_pool = v; }
static inline auto env_return_id(LmnWord n) {
  if (lmn_id_pool)
    lmn_id_pool->emplace_back(n);
}
#if /**/ !defined(ENABLE_PARALLEL) || defined(USE_TLS_KEYWORD)
static inline auto env_gen_state_id() {
  lmn_tls.state_id += lmn_tls.thread_num;
  return lmn_tls.state_id;
}
static inline auto env_my_thread_id() { return lmn_tls.thread_id; }
static inline auto env_set_my_thread_id(unsigned int N) { lmn_tls.thread_id = N; }
static inline auto env_threads_num() { return lmn_tls.thread_num; }
static inline auto env_set_threads_num(unsigned int N) { lmn_tls.thread_num = N; }
static inline auto env_reset_proc_ids() { lmn_tls.proc_next_id = 1U; }
static inline auto env_set_next_id(ProcessID N) { lmn_tls.proc_next_id = N; }
static inline auto env_gen_next_id() {
  if (lmn_id_pool && !lmn_id_pool->empty()) {
    auto ret = lmn_id_pool->back();
    lmn_id_pool->pop_back();
    return ret;
  }
  return lmn_tls.proc_next_id++;
}
static inline auto env_next_id() { return lmn_tls.proc_next_id; }
#
#elif /**/ defined(USE_TLS_PTHREAD_KEY)
static inline unsigned long env_gen_state_id() {
  LmnTLS *p   = (LmnTLS *)lmn_TLS_get_value(lmn_tls);
  p->state_id += p->thread_num;
  return p->state_id;
}
#
static inline unsigned int env_my_thread_id() {
  LmnTLS *p = (LmnTLS *)lmn_TLS_get_value(lmn_tls);
  return p->thread_id;
}
#
static inline void env_set_my_thread_id(unsigned int n) {
  LmnTLS *p    = (LmnTLS *)lmn_TLS_get_value(lmn_tls);
  p->thread_id = n;
}
#
static inline unsigned int env_threads_num() {
  LmnTLS *p = (LmnTLS *)lmn_TLS_get_value(lmn_tls);
  return p->thread_num;
}
#
static inline void env_set_threads_num(unsigned int n) {
  LmnTLS *p     = (LmnTLS *)lmn_TLS_get_value(lmn_tls);
  p->thread_num = n;
}
#
static inline void env_reset_proc_ids() {
  LmnTLS *p       = (LmnTLS *)lmn_TLS_get_value(lmn_tls);
  p->proc_next_id = 1UL;
}
#
static inline void env_set_next_id(unsigned long n) {
  LmnTLS *p       = (LmnTLS *)lmn_TLS_get_value(lmn_tls);
  p->proc_next_id = n;
}
#
#define env_gen_next_id()                                                                                              \
  ((lmn_id_pool && lmn_id_pool->get_num() > 0) ? lmn_id_pool->pop()                                                    \
                                               : ((LmnTLS *)lmn_TLS_get_value(lmn_tls))->proc_next_id++)

static inline unsigned long env_next_id() {
  LmnTLS *p = (LmnTLS *)lmn_TLS_get_value(lmn_tls);
  return p->proc_next_id;
}

#endif

namespace slim::config {
#ifdef PROFILE
static constexpr bool profile = true;
#else
static constexpr bool profile = false;
#endif
} // namespace slim::config

#endif /* LMNTAL_H */
