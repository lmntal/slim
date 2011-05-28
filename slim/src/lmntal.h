/*
 * lmntal.h - global header file
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id: lmntal.h,v 1.13 2008/10/17 07:36:32 riki Exp $
 */

#ifndef LMNTAL_H
#define LMNTAL_H

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WITH_DMALLOC
#  include <dmalloc.h>
#endif

#ifdef HAVE_INTTYPES_H
#  include <inttypes.h>
#endif

#define LMN_EXTERN extern

/*------------------------------------------------------------------------
 *  Some useful macros
 */

#ifndef BOOL
#  define BOOL unsigned char
#endif

#ifndef FALSE
#  define FALSE (0)
#endif
#ifndef TRUE
#  define TRUE (!FALSE)
#endif

/* This defines several auxiliary routines that are useful for debugging */
#ifndef LMN_DEBUG_HELPER
#  define LMN_DEBUG_HELPER      TRUE
#endif

#ifndef LMN_DECL_BEGIN
#  ifdef __cplusplus
#    define LMN_DECL_BEGIN  extern "C" {
#    define LMN_DECL_END    }
#  else
#    define LMN_DECL_BEGIN
#    define LMN_DECL_END
#  endif
#endif /*!defined(LMN_DECL_BEGIN)*/

LMN_DECL_BEGIN



/*----------------------------------------------------------------------
 * data types
 */

#if SIZEOF_LONG < SIZEOF_VOIDP
#  error sizeof(long) < sizeof(void*)
#endif


typedef unsigned long LmnWord;
typedef unsigned char BYTE, LmnByte;

#define LMN_WORD_BYTES  SIZEOF_LONG
#define LMN_WORD_BITS   (SIZEOF_LONG * 8)
#define LMN_WORD(X)     ((LmnWord)(X))


typedef LmnWord  LmnAtom;
typedef void    *LmnSAtom;
typedef uint8_t  LmnLinkAttr;

//typedef uint16_t ProcessID;
typedef LmnWord  ProcessID;

/* uint16_t is not defined if there is no 2Byte data type */
typedef uint16_t LmnFunctor;
#define LMN_FUNCTOR_BYTES (sizeof(LmnFunctor))
#define LMN_FUNCTOR_BITS (LMN_FUNCTOR_BYTES*8)
#define LMN_FUNCTOR(X) ((LmnFunctor)((X)))

/* this type must be enough to represent arity */
typedef uint8_t LmnArity;

typedef unsigned int lmn_interned_str;

typedef BYTE* LmnRuleInstr;
typedef uint16_t LmnInstrOp;
typedef uint16_t LmnInstrVar;
typedef uint16_t LmnJumpOffset;
typedef uint32_t LmnLineNum;
typedef int16_t LmnRulesetId;
typedef uint32_t LmnSubInstrSize;

typedef struct LmnMembrane LmnMembrane;
typedef struct DeltaMembrane DeltaMembrane;

#if LMN_WORD_BYTES == 4
#  define LMN_WORD_SHIFT 2
#elif LMN_WORD_BYTES == 8
#  define LMN_WORD_SHIFT 3
#else
#  error Word size is not 2^N
#endif

#ifndef HAVE___INT64
#  ifdef HAVE_LONG_LONG_INT
     typedef long long __int64;
#    define HAVE___INT64
#  endif
#endif

typedef struct ProcessTbl *ProcessTbl;

/*----------------------------------------------------------------------
 * Special Atom
 */

struct LmnSPAtomHeader {
  LmnByte type;
};

typedef struct LmnSPAtomHeader LmnSpAtom;

/* スペシャルアトムは構造体の最初の要素としてに必ずこのヘッダを含めなけ
   ればならない */
#define LMN_SP_ATOM_HEADER struct LmnSPAtomHeader hdr


/*----------------------------------------------------------------------
 * React Context
 */

typedef struct ReactCxt *ReactCxt;

/*----------------------------------------------------------------------
 * Mem Stack
 */

struct Vector;
typedef struct Vector Vector;
typedef struct Vector *LmnMemStack;



/*----------------------------------------------------------------------
 * Utility
 */

/*  Memory */

LMN_EXTERN void *lmn_calloc(size_t num, size_t size);
LMN_EXTERN void *lmn_malloc(size_t num);
LMN_EXTERN void *lmn_realloc(void *p, size_t num);
LMN_EXTERN void lmn_free (void *p);

#define LMN_NALLOC(TYPE, NUM)          ((TYPE *)lmn_malloc(sizeof(TYPE) * (NUM)))
#define LMN_CALLOC(TYPE, NUM)          ((TYPE *)lmn_calloc((NUM), sizeof(TYPE)))
#define LMN_MALLOC(TYPE)               ((TYPE *)lmn_malloc(sizeof(TYPE)))
#define LMN_REALLOC(TYPE, P, NUM)      ((TYPE *)lmn_realloc((P), (NUM) * sizeof(TYPE)))
#define LMN_FREE(P)                    (lmn_free((void *)(P)))

/* Assertion */
#ifdef DEBUG
#  include <assert.h>
#  define LMN_ASSERT(expr)   assert(expr)
#else
#  define LMN_ASSERT(expr)   ((void)0)/* nothing */
#endif


/*----------------------------------------------------------------------
 * Global data
 */

/* 階層グラフ構造の出力形式 */
enum OutputFormat { DEFAULT, DEV, DOT };
enum MCdumpFormat { CUI, LaViT, Dir_DOT, FSM };
enum SPdumpFormat { SP_NONE, INCREMENTAL, LMN_SYNTAX};

#ifdef DEBUG
#  define ISM_DEBUG(Pr) if (lmn_env.debug_isomor2) { Pr; }
#else
#  define ISM_DEBUG(Pr)
#endif


struct LmnEnv {
  BOOL trace;
  BOOL show_proxy;
  BOOL show_ruleset;
  BOOL show_chr;

  BOOL nd;
  BOOL ltl;
  BOOL ltl_all;
  BOOL show_transition;

  BOOL enable_por;          /* to enable partial order reduction for nondeterministic execution or LTL model checking */
  BOOL enable_por_old;
  BYTE optimization_level;
  BYTE profile_level;

  BOOL translate;
  BOOL bfs;
  BOOL mem_enc;
  BOOL compact_stack;

  unsigned int depth_limits;
  unsigned int core_num;

  BOOL enable_compress_mem;
  BOOL delta_mem;
  BOOL z_compress;
  BOOL prop_scc_driven;

  BOOL property_dump;
  BOOL enable_parallel;
  BOOL optimize_loadbalancing;
  BOOL optimize_lock;

  BOOL optimize_hash;
  BOOL dump;
  BOOL end_dump;
  BOOL benchmark;

  /* LTL model checking algorithms */
  BOOL enable_owcty;
  BOOL enable_map;
  BOOL enable_bledge;
  BOOL bfs_layer_sync;

  BOOL enable_map_heuristic;

  /* only jni-interactive mode*/
  BOOL interactive;
  BOOL normal_remain;
  BOOL normal_remaining;
  BOOL normal_cleaning;

  BOOL nd_remain;
  BOOL nd_remaining;
  BOOL nd_cleaning;
  BOOL nd_search_end;

  /* allow hyperlink system */
  BOOL hyperlink;
  BOOL show_hyperlink;

#ifdef PROFILE
  BOOL optimize_hash_old;
  BOOL prof_no_memeq;
#endif

  BOOL show_reduced_graph;
#ifdef DEBUG
  BOOL debug_isomor;
  BOOL debug_isomor2;
  BOOL debug_memenc;
  BOOL debug_delta;
  BOOL debug_id;
  BOOL debug_hash;
  BOOL debug_mc;
  BOOL debug_por;
  BOOL debug_por_dep;
#endif

  enum OutputFormat output_format;
  enum MCdumpFormat mc_dump_format;
  enum SPdumpFormat sp_dump_format;
  int load_path_num;
  char *load_path[256];
  char *automata_file;         /* never claim file */
  char *propositional_symbol;  /* file for propositional symbol definitions */
  char *ltl_exp;
};

extern struct LmnEnv  lmn_env;
extern struct Vector *lmn_id_pool;
extern unsigned int lmn_thread_num;

/*----------------------------------------------------------------------
 * Others
 */
void slim_version(FILE *f);


/* check for thread library */
#if defined (HAVE_LIBPTHREAD) || defined (HAVE_WINAPI)
#  define HAVE_MT_LIBRARY  (1)
#  ifdef HAVE_LIBPTHREAD
#    include <pthread.h>
#  else /* winapi */
#    include <windows.h>
#  endif
#else
#  undef  HAVE_MT_LIBRARY
#endif /* HAVE_LIBPTHREAD or HAVE_WINAPI */

/* check for thread local storage  */
#if defined (HAVE___THREAD)
#  define HAVE_TLS_KEYWORD  (1)
#  define LMN_TLS           __thread
#else
#  define LMN_TLS
#endif /* HAVE___THREAD */

#if defined (HAVE_MT_LIBRARY) && defined (HAVE_TLS_KEYWORD)
#  define ENABLE_PARALLEL
#endif


#ifdef ENABLE_PARALLEL
extern LMN_TLS unsigned long lmn_thread_id;
extern LMN_TLS unsigned long lmn_state_id;
extern LMN_TLS ProcessID lmn_next_id;
#else
const static unsigned long lmn_thread_id = 0;
extern unsigned long lmn_state_id;
extern ProcessID lmn_next_id;
#endif /* ENABLE_PARALLEL */

#define env_gen_state_id()     (lmn_state_id += lmn_thread_num)

#ifdef TIME_OPT
#  define env_reset_proc_ids() (lmn_next_id = 1U)
#  define env_set_next_id(N)   (lmn_next_id = (N))
#  define env_gen_next_id()    ((lmn_id_pool && vec_num(lmn_id_pool) > 0) \
                                 ? vec_pop(lmn_id_pool)                   \
                                 : lmn_next_id++)
#  define env_return_id(N)     if (lmn_id_pool) vec_push(lmn_id_pool, (vec_data_t)(N))
#  define env_next_id()        (lmn_next_id)
#else
#  define env_reset_proc_ids()
#  define env_set_next_id(N)
#  define env_gen_next_id()    0
#  define env_return_id(N)
#  define env_next_id()        0
#endif

LMN_DECL_END

#endif /* LMNTAL_H */
