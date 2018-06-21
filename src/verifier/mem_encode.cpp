/*
 * mem_encode.cpp
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
 */

/* 膜を一意のバイト列に変換する */

/* encode specification
 *
 * アトムや膜などの各要素をバイト列に書き込む際には、まず要素の種類を表す4ビットのタグを書き込み、
 * その次の位置から、それぞれの要素毎の値を書き込んでいく。
 *
 * 実装では、4ビットを一つの単位としてバイト列に値を書き込んでいく。
 *
 * atom
 *   tag         :  0000
 *   functor     : 2Byte
 *   arguments
 *
 * membrane
 *   tag:        :  0001
 *   elements    : (any length)
 *   end tag     :  0010
 *
 * named membrane
 *   tag         :  0011
 *   name        : 4Byte
 *   elements    : (any length)
 *   end tag     :  0010
 *
 * atom/hlink ref
 *   tag         :  0100
 *   ref id      : 4byte
 *   arg num     : 1byte
 *
 * mem ref
 *   tag         :  0101
 *   ref id      : 4byte
 *
 * escape from membrane
 *   tag         :  0110
 *
 * escape from membrane with data atom
 *   tag         :  0111
 *   data tag
 *   data value
 *
 * from of traversal
 *   tag         :  1000
 *
 * rule sets(only one ruleset)
 *   tag         :  1001
 *   ruleset id  : 2byte
 *
 * rule sets
 *   tag         :  1010
 *   ruleset num : 4byte
 *   foreach
 *     ruleset id  : 2byte
 *
 * uniq applied histories
 *   tag         :  1011
 *   ruleset num : 4byte
 *   foreach
 *     ruleset id  : 2byte
 *     history num : 4byte
 *     foreach
 *       histroy ids  : 4byte
 *
 * int atom
 *   tag         : 1100
 *   value       : 1 word
 *
 * double atom
 *   tag         : 1101
 *   value       : sizeof(double)
 *
 * string atom
 *   tag         : 1110
 *   value       : sizeof(lmn_interned_str)
 *
 * hlink object
 *   tag         : 1111
 *   rank        : 4byte
 *
 */
#include "mem_encode.h"
#include "binstr_compress.h"
#include "delta_membrane.h"
#include "element/element.h"
#include "vm/vm.h"

#ifdef PROFILE
#include "runtime_status.h"
#endif

/* Tags */
#define TAG_ATOM_START 0x0
#define TAG_MEM_START 0x1
#define TAG_MEM_END 0x2
#define TAG_NAMED_MEM_START 0x3
#define TAG_VISITED_ATOMHLINK 0x4
#define TAG_VISITED_MEM 0x5
#define TAG_ESCAPE_MEM 0x6
#define TAG_ESCAPE_MEM_DATA 0x7
#define TAG_FROM 0x8
#define TAG_RULESET1 0x9
#define TAG_RULESET 0xa
#define TAG_RULESET_UNIQ 0xb
#define TAG_INT_DATA 0xc
#define TAG_DBL_DATA 0xd
#define TAG_STR_DATA 0xe
#define TAG_HLINK 0xf

/* Binary Stringのpositionを進めるためのカウンタ群. */
#define BS_PROC_REF_SIZE                                                       \
  (TAG_IN_BYTE *                                                               \
   sizeof(uint32_t)) /* 訪問番号(4byteへ拡張したが,                  \
                        sizeof(ProcessID)とするのが好ましいはず) */
#define BS_ATOM_REF_ARG_SIZE                                                   \
  (TAG_IN_BYTE *                                                               \
   sizeof(                                                                     \
       LmnArity)) /* アトムあたりのリンク本数は127本までなので1Byteで良い */
#define BS_FUNCTOR_SIZE (TAG_IN_BYTE * sizeof(LmnFunctor)) /* Functor ID */
#define BS_INT_SIZE                                                            \
  (TAG_IN_BYTE *                                                               \
   SIZEOF_LONG) /* lmntalにおける整数データは1wordであるためlongで良い */
#define BS_MEM_NAME_SIZE (TAG_IN_BYTE * sizeof(lmn_interned_str)) /* 膜名 */
#define BS_RULESET_SIZE                                                        \
  (TAG_IN_BYTE * sizeof(LmnRulesetId)) /* ルールセットID */
#define BS_RULESET_NUM_SIZE                                                    \
  (TAG_IN_BYTE * sizeof(uint32_t)) /* ルールセット数 */
#define BS_RULE_NUM_SIZE (TAG_IN_BYTE * sizeof(uint32_t)) /* ルール数 */
#define BS_DBL_SIZE (TAG_IN_BYTE * sizeof(double)) /* 浮動小数点数 */
#define BS_STR_ID_SIZE                                                         \
  (TAG_IN_BYTE * sizeof(lmn_interned_str)) /* 文字列に対応させたID */
#define BS_HISTORY_NUM_SIZE (BS_RULE_NUM_SIZE)
#define BS_HISTORY_SIZE                                                        \
  (BS_STR_ID_SIZE) /* UNIQ制約が管理する履歴型のサイズ.          \
                      lmn_histroy_tみたいな型にしたい */
#define BS_HLINK_NUM_SIZE                                                      \
  (TAG_IN_BYTE *                                                               \
   sizeof(LmnHlinkRank)) /* ハイパーリンクの接続個数. 同型性判定で使う */

struct BsDecodeLog {
  LmnWord v;
  BYTE type;
};

typedef struct BsDecodeLog BsDecodeLog;
#define BS_LOG_TYPE_NONE (0x0U)
#define BS_LOG_TYPE_ATOM (0x1U)
#define BS_LOG_TYPE_MEM (0x2U)
#define BS_LOG_TYPE_HLINK (0x3U)

typedef struct BinStrRef *BinStr;
typedef struct BinStrPtr *BinStrPtrRef;

/* ファンクタ優先度付けの実装 cf) mem_idの高速化 @hori */
uint16_t functor_priority[FUNCTOR_MAX + 1];

/*----------------------------------------------------------------------
 * Prototypes
 */

/* Dump Membrane to Binary String */

static void dump_mem_atoms(LmnMembraneRef mem, BinStrPtrRef bsp,
                           VisitLogRef visited);
static void dump_mols(Vector *atoms, BinStrPtrRef bsp, VisitLogRef visited);
static void dump_mems(LmnMembraneRef mem, BinStrPtrRef bsp,
                      VisitLogRef visited);

static int comp_functor_greater_f(const void *a_, const void *b_);

/*----------------------------------------------------------------------
 * Initialization
 */

void mem_isom_init() {
  memset(functor_priority, 0xff, sizeof(uint16_t) * FUNCTOR_MAX + 1);
}

void mem_isom_finalize() {}

void set_functor_priority(LmnFunctor f, int priority) {
  if (priority <= 0)
    lmn_fatal("implementation error");
  functor_priority[f] = priority;
}

/*----------------------------------------------------------------------
 * Binary String
 */
LmnBinStrRef lmn_binstr_make(unsigned int real_len) {
  LmnBinStrRef bs = LMN_MALLOC(struct LmnBinStr);
  bs->len = real_len * TAG_IN_BYTE;
  bs->type = 0x00U;
  bs->v = LMN_NALLOC(BYTE, real_len);
  memset(bs->v, 0x0U, sizeof(BYTE) * real_len);
  return bs;
}

LmnBinStrRef lmn_binstr_copy(struct LmnBinStr *src_bs) {
  unsigned long v_len_real;
  struct LmnBinStr *dst_bs;

  v_len_real = ((src_bs->len + 1) / TAG_IN_BYTE);
  dst_bs = lmn_binstr_make(v_len_real);
  dst_bs->len = src_bs->len;

  memcpy(dst_bs->v, src_bs->v, v_len_real);

  return dst_bs;
}

void lmn_binstr_free(struct LmnBinStr *bs) {
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_remove_space(PROFILE_SPACE__STATE_BINSTR, lmn_binstr_space(bs));
  }
#endif
  LMN_FREE(bs->v);
  LMN_FREE(bs);
}

unsigned long lmn_binstr_space(struct LmnBinStr *bs) {
  /* TODO: アラインメントで切り上げる必要があるはず */
  return sizeof(struct LmnBinStr) +
         sizeof(BYTE) * ((bs->len + 1) / TAG_IN_BYTE);
}

/* エンコード処理(計算中)に用いるバイナリストリング(作業領域) */
struct BinStrRef {
  BYTE *v;  /* バイト列(128個で初期化) */
  int size; /* バッファのサイズ（4ビット単位）: 現在のバイト列の大きさ(128 *
               TAG_IN_BYTEで初期化) */
  int cur; /* 書き込み位置（4ビット単位）    : 次に書き込む位置(0で初期化) */
  Vector *ptrs; /* バッファの位置を指し示す、BinStrPtrのベクタ :
                   BinStrPtrへのポインタ列 */
  Vector *ptrs2; /* 作業用の領域 */
};

static inline void bsptr_invalidate(BinStrPtrRef p);
static void binstr_invalidate_ptrs(struct BinStrRef *p, int start);
static inline void bsptr_destroy(struct BinStrPtr *p);
static inline int bsptr_pos(struct BinStrPtr *p);

#define BS_TBL_SIZE (128)
static struct BinStrRef *binstr_make() {
  struct BinStrRef *p = LMN_MALLOC(struct BinStrRef);
  p->size = BS_TBL_SIZE * TAG_IN_BYTE;
  p->v = LMN_NALLOC(BYTE, p->size / TAG_IN_BYTE);
  memset(p->v, 0x0U, sizeof(BYTE) * BS_TBL_SIZE);
  p->cur = 0;
  p->ptrs = vec_make(64);
  p->ptrs2 = vec_make(64);
  return p;
}

static inline void binstr_free(BinStr p) {
  vec_free(p->ptrs);
  vec_free(p->ptrs2);
  LMN_FREE(p->v);
  LMN_FREE(p);
}

int binstr_byte_size(LmnBinStrRef p) {
  return (p->len / TAG_IN_BYTE) + sizeof(struct LmnBinStr);
}

/* バイナリストリングのハッシュ値を返す */
unsigned long binstr_hash(const LmnBinStrRef a) {
  unsigned long hval;
#ifdef PROFILE
  if (lmn_env.profile_level >= 3)
    profile_start_timer(PROFILE_TIME__STATE_HASH_MID);
#endif

  hval = lmn_byte_hash(a->v, (a->len + 1) / TAG_IN_BYTE);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3)
    profile_finish_timer(PROFILE_TIME__STATE_HASH_MID);
#endif

  return hval;
}

#define BS_SET(a, pos, v)                                                      \
  (((pos)&1)                                                                   \
       ? ((a)[(pos) >> 1] = ((a)[(pos) >> 1] & 0x0f) | ((v) << TAG_BIT_SIZE))  \
       : ((a)[(pos) >> 1] = (v & 0x0f) | ((a)[(pos) >> 1] & 0xf0)))
#define BS_GET(a, pos)                                                         \
  (((pos)&1) ? ((a)[(pos) >> 1] & 0xf0) >> TAG_BIT_SIZE                        \
             : (a)[(pos) >> 1] & 0x0f)

/* bsの位置posにbの下位4ビットを書き込む。書き込みに成功した場合は真を
   返し、失敗した場合は偽を返す。*/
static inline int binstr_set(struct BinStrRef *bs, BYTE b, int pos) {
  /* 書き込む位置よりサイズが小さければストリングの長さを倍にする */
  while (bs->size <= pos) {
    int org_size = bs->size / TAG_IN_BYTE;
    bs->size *= 2;
    bs->v = LMN_REALLOC(BYTE, bs->v, bs->size / TAG_IN_BYTE);
    memset(bs->v + org_size, 0x0U,
           sizeof(BYTE) * ((bs->size / TAG_IN_BYTE) - org_size));
  }

  /* cur==posならば無条件で書き込み成功 */
  if (bs->cur == pos) {
    BS_SET(bs->v, pos, b);
    bs->cur++;
    return 1;
  } else if (bs->cur > pos) {
    /* 下位4ビットの比較を行う */
    if (BS_GET(bs->v, pos) < b)
      return 0;
    else if (BS_GET(bs->v, pos) > b) {
      /* 現在、curが指す位置よりも前に書き込みを行ったため，
         posよりも後の位置を指すポインタをすべて無効にする */
      binstr_invalidate_ptrs(bs, pos + 1); /* curの位置をposまで戻す */
      bs->cur = pos + 1;
      BS_SET(bs->v, pos, b);
      return 1;
    } else { /* bs->v[pos] == b */
      return 1;
    }
  } else { /*(bs->cur < pos)*/
    lmn_fatal("unexpected");
    return 0;
  }
}

static inline int binstr_set_direct(struct BinStrRef *bs, BYTE b, int pos) {
  while (bs->size <= pos) {
    int org_size = bs->size / TAG_IN_BYTE;
    bs->size *= 2;
    bs->v = LMN_REALLOC(BYTE, bs->v, bs->size / TAG_IN_BYTE);
    memset(bs->v + org_size, 0x0U,
           sizeof(BYTE) * ((bs->size / TAG_IN_BYTE) - org_size));
  }

  BS_SET(bs->v, pos, b);
  bs->cur = pos + 1;
  return 1;
}

/* bsの位置posから1バイト読み込み，返す */
static inline BYTE binstr_get_byte(BYTE *bs, int pos) {
  return (BS_GET(bs, pos + 1)) | (BS_GET(bs, pos) << 4);
}

static inline uint16_t binstr_get_uint16(BYTE *bs, int pos) {
  return (uint16_t)((binstr_get_byte(bs, pos + 2)) |
                    ((binstr_get_byte(bs, pos)) << 8));
}

static inline uint32_t binstr_get_uint32(BYTE *bs, int pos) {
  return (uint32_t)(binstr_get_uint16(bs, pos + 4) |
                    (binstr_get_uint16(bs, pos) << 16));
}

static inline uint64_t binstr_get_uint64(BYTE *bs, int pos) {
  return (uint64_t)(binstr_get_uint32(bs, pos + 8) |
                    (uint64_t)binstr_get_uint32(bs, pos) << 32);
}

/* LMNtal言語はinteger valueの範囲を1wordとしている(型がない)ため, long型で良い
 */
static inline long binstr_get_int(BYTE *bs, int pos) {
#if SIZEOF_LONG == 4
  return (long)binstr_get_uint32(bs, pos);
#elif SIZEOF_LONG == 8
  return (long)binstr_get_uint64(bs, pos);
#else
#error "not supported"
#endif
}

static inline double binstr_get_dbl(BYTE *bs, int pos) {
  int i;
  union {
    double d;
    BYTE t[8];
  } v;

  for (i = 7; i >= 0; i--) {
    v.t[i] = binstr_get_byte(bs, pos + (7 - i) * TAG_IN_BYTE);
  }

  return v.d;
}

static inline LmnWord binstr_get_word(BYTE *bs, int pos) LMN_UNUSED;
static inline LmnWord binstr_get_word(BYTE *bs, int pos) {
#if SIZEOF_LONG == 4
  return (LmnWord)binstr_get_uint32(bs, pos);
#elif SIZEOF_LONG == 8
  return (LmnWord)binstr_get_uint64(bs, pos);
#else
#error "not supported"
#endif
}

static inline LmnFunctor binstr_get_functor(BYTE *bs, int pos) {
  LMN_ASSERT(sizeof(LmnFunctor) == 2);
  long f = binstr_get_uint16(bs, pos);
  return (LmnFunctor)(FUNCTOR_MAX - f);
}

static inline unsigned int binstr_get_ref_num(BYTE *bs, int pos) {
  LMN_ASSERT(BS_PROC_REF_SIZE == (sizeof(uint32_t) * TAG_IN_BYTE));
  return (unsigned int)binstr_get_uint32(bs, pos);
}

static inline unsigned int binstr_get_arg_ref(BYTE *bs, int pos) {
  LMN_ASSERT(BS_ATOM_REF_ARG_SIZE == 2);
  return binstr_get_byte(bs, pos);
}

static inline lmn_interned_str binstr_get_mem_name(BYTE *bs, int pos) {
  return binstr_get_uint32(bs, pos);
}

static inline long binstr_get_ruleset_num(BYTE *bs, int pos) {
  return binstr_get_uint32(bs, pos);
}

/* ruleset id(2byte)の取得 */
static inline long binstr_get_ruleset(BYTE *bs, int pos) {
  return binstr_get_uint16(bs, pos);
}

static inline lmn_interned_str binstr_get_strid(BYTE *bs, int pos) {
  LMN_ASSERT(BS_HISTORY_SIZE == 8);
  return binstr_get_uint32(bs, pos);
}

static inline long binstr_get_history_num(BYTE *bs, int pos) {
  LMN_ASSERT(BS_HISTORY_NUM_SIZE == 8);
  return binstr_get_uint32(bs, pos);
}

static inline lmn_interned_str binstr_get_history(BYTE *bs, int pos) {
  return binstr_get_strid(bs, pos);
}

/* start以降を指すポインタをすべて無効にする */
static void binstr_invalidate_ptrs(struct BinStrRef *p, int start) {
  int i;
  Vector *tmp;
  int len = vec_num(p->ptrs);
  /* EFFICIENCY */
  for (i = 0; i < len; i++) {
    BinStrPtrRef bsp = (BinStrPtrRef)vec_get(p->ptrs, i);
    if (bsptr_pos(bsp) >= start)
      bsptr_invalidate(bsp);
    else
      vec_push(p->ptrs2, (vec_data_t)bsp);
  }

  /* swap */
  tmp = p->ptrs;
  p->ptrs = p->ptrs2;
  p->ptrs2 = tmp;
  vec_clear(p->ptrs2);
}

/* バイナリストリングaとbの比較を行いaがbより、小さい、同じ、大きい場合に、
 * それぞれ負の値、0、正の値を返す。*/
int binstr_compare(const LmnBinStrRef a, const LmnBinStrRef b) {
  int ret;
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__STATE_COMPARE_MID);
  }
#endif

  ret = lmn_byte_cmp(a->v, (a->len + 1) / TAG_IN_BYTE, b->v,
                     (b->len + 1) / TAG_IN_BYTE);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__STATE_COMPARE_MID);
  }
#endif
  return ret;
}

/* bsにポインタptrを追加する */
static inline void binstr_add_ptr(const struct BinStrRef *bs,
                                  struct BinStrPtr *ptr) {
  vec_push(bs->ptrs, (vec_data_t)ptr);
}

/* BinStrをLmnBinStrに変換する */
static inline struct LmnBinStr *binstr_to_lmn_binstr(BinStr bs) {
  struct LmnBinStr *ret_bs;
  int size = (bs->cur + 1) / 2;
  ret_bs = LMN_MALLOC(struct LmnBinStr);
  ret_bs->v = LMN_NALLOC(BYTE, size);
  ret_bs->type = 0x00U;
  memcpy(ret_bs->v, bs->v, size);
  ret_bs->len = bs->cur;
  if (ret_bs->len & 1) {
    ret_bs->v[ret_bs->len >> 1] = ret_bs->v[ret_bs->len >> 1] & 0x0f;
  }

  return ret_bs;
}

static inline void binstr_dump_data_atom(BYTE *bs, int *pos, unsigned int tag) {
  switch (tag) {
  case TAG_INT_DATA: {
    long n = binstr_get_int(bs, *pos);
    (*pos) += BS_INT_SIZE;
    printf("_INT%ld_ ", n);
  } break;
  case TAG_DBL_DATA: {
    double n = binstr_get_dbl(bs, *pos);
    (*pos) += BS_DBL_SIZE;
    printf("_DBL%lf_", n);
  } break;
  case TAG_STR_DATA: {
    lmn_interned_str n = binstr_get_strid(bs, *pos);
    (*pos) += BS_STR_ID_SIZE;
    printf("\"%s\"", lmn_id_to_name(n));
  } break;
  default:
    lmn_fatal("unexpected.");
    break;
  }
}

static void binstr_dump(BYTE *bs, int len) {
  BsDecodeLog *log;
  int pos, v_i;

  log = LMN_NALLOC(BsDecodeLog, len * TAG_IN_BYTE);
  pos = 0;
  v_i = 1;
  while (pos < len) {
    unsigned int tag = BS_GET(bs, pos);
    pos++;

    switch (tag) {
    case TAG_ATOM_START: {
      LmnFunctor f;
      f = binstr_get_functor(bs, pos);
      pos += BS_FUNCTOR_SIZE;
      log[v_i].v = (LmnWord)f;
      log[v_i].type = BS_LOG_TYPE_ATOM;
      printf("%s/%d_%d ", lmn_id_to_name(LMN_FUNCTOR_NAME_ID(f)),
             LMN_FUNCTOR_ARITY(f), v_i++);
    } break;
    case TAG_NAMED_MEM_START: /* FALL THROUGH */
    case TAG_MEM_START: {
      log[v_i].v = 0; /* とりえあずゼロクリア */
      log[v_i].type = BS_LOG_TYPE_MEM;

      if (tag == TAG_MEM_START) {
        printf("_%d{ ", v_i);
      } else {
        lmn_interned_str name;
        name = binstr_get_mem_name(bs, pos);
        pos += BS_MEM_NAME_SIZE;
        printf("%s_%d{ ", lmn_id_to_name(name), v_i);
      }

      v_i++;
    } break;
    case TAG_MEM_END: {
      printf("}. ");
    } break;
    case TAG_HLINK: {
      LmnHlinkRank hl_num;

      hl_num = binstr_get_ref_num(bs, pos);
      pos += BS_HLINK_NUM_SIZE;

      log[v_i].v = 0; /* とりあえずゼロクリア */
      log[v_i].type = BS_LOG_TYPE_HLINK;
      printf("HL/%d_%d ", hl_num, v_i++);

      tag = BS_GET(bs, pos);
      pos++;
      switch (tag) {
      case TAG_FROM: {
        printf("_F_ ");
      } break;
      case TAG_ATOM_START: {
        LmnFunctor f;
        f = binstr_get_functor(bs, pos);
        pos += BS_FUNCTOR_SIZE;
        printf("%s/%d ", lmn_id_to_name(LMN_FUNCTOR_NAME_ID(f)),
               LMN_FUNCTOR_ARITY(f));
      } break;
      case TAG_INT_DATA:
      case TAG_DBL_DATA:
      case TAG_STR_DATA: {
        binstr_dump_data_atom(bs, &pos, tag);
      } break;
      default:
        lmn_fatal("unexpected");
        break;
      }
    } break;
    case TAG_VISITED_ATOMHLINK: /* FALL THROUGH */
    case TAG_VISITED_MEM: {
      unsigned int ref;

      ref = binstr_get_ref_num(bs, pos);
      pos += BS_PROC_REF_SIZE;

      switch (log[ref].type) {
      case BS_LOG_TYPE_ATOM:
        printf("$%d's%d ", ref, binstr_get_arg_ref(bs, pos));
        pos += BS_ATOM_REF_ARG_SIZE;
        break;
      case BS_LOG_TYPE_MEM:
        printf("#%d ", ref);
        break;
      case BS_LOG_TYPE_HLINK:
        printf("$HL%d ", ref);
        break;
      default:
        lmn_fatal("unexpected reference");
        break;
      }
    } break;
    case TAG_ESCAPE_MEM_DATA: {
      unsigned int sub_tag = BS_GET(bs, pos);
      pos++;
      printf("!");
      binstr_dump_data_atom(bs, &pos, sub_tag);
      printf("! ");
    } break;
    case TAG_ESCAPE_MEM: {
      printf("! ");
    } break;
    case TAG_FROM: {
      printf("_F_ ");
    } break;
    case TAG_INT_DATA: /* FALL TROUGH */
    case TAG_DBL_DATA: /* FALL TROUGH */
    case TAG_STR_DATA:
      binstr_dump_data_atom(bs, &pos, tag);
      break;
    case TAG_RULESET1: {
      int rs_id;

      rs_id = binstr_get_ruleset(bs, pos);
      pos += BS_RULESET_SIZE;
      printf("@%d", rs_id);
    } break;
    case TAG_RULESET: {
      int j, n, rs_id;

      n = binstr_get_ruleset_num(bs, pos);
      pos += BS_RULESET_NUM_SIZE;
      for (j = 0; j < n; j++) {
        rs_id = binstr_get_ruleset(bs, pos);
        pos += BS_RULESET_SIZE;
        printf("@%d", rs_id);
      }
    } break;
    case TAG_RULESET_UNIQ: {
      LmnRuleSetRef rs;
      lmn_interned_str id;
      unsigned int j, k, l, n, rs_id, rule_num, his_num;

      n = binstr_get_ruleset_num(bs, pos);
      pos += BS_RULESET_NUM_SIZE;
      for (j = 0; j < n; j++) {
        rs_id = binstr_get_ruleset(bs, pos);
        pos += BS_RULESET_SIZE;
        printf("@%d/", rs_id);

        /* dump applied histories of uniq constraint rules */

        rs = ruleset_table->get(rs_id);
        rule_num = rs->num;

        for (k = 0; k < rule_num; k++) {
          printf("[%s", lmn_id_to_name(
                            rs->get_rule(k)->name));

          his_num = binstr_get_history_num(bs, pos);
          pos += BS_HISTORY_NUM_SIZE;
          for (l = 0; l < his_num; l++) {
            id = binstr_get_history(bs, pos);
            pos += BS_HISTORY_SIZE;
            printf("\"%s\"", lmn_id_to_name(id));
          }
          printf("]");
        }
      }
    } break;
    default:
      printf("pos = %d, len = %d\n", pos, len);
      lmn_fatal("unexpected");
      break;
    }
  }
  printf("\n");
  LMN_FREE(log);
}

/* 開発用：　標準出力にバイナリストリングの内容を出力する */
void lmn_binstr_dump(const LmnBinStrRef bs) { binstr_dump(bs->v, bs->len); }

/*----------------------------------------------------------------------
 * Binary String Pointer
 */

/* BinStrの特定の位置を指し示すポインタ。BinStrへの書き込みはBinStrPtr
   を介して行う。他のBinStrPtrの書き込みにより、現在のBinStrPtrが無効に
   なる場合があり、binstr_validが真を返すようになる。 */

struct BinStrPtr {
  struct BinStrRef *binstr; /* 所属するBinStrを指す */
  int pos;                  /* bit (0で初期化) */
  BOOL valid;               /* TRUEで初期化 */
  BOOL direct; /* FALSEで初期化, directメソッドを用いた場合はTRUEで初期化 */
};

static inline void bsptr_init(struct BinStrPtr *p, struct BinStrRef *bs) {
  p->binstr = bs;
  p->pos = 0;
  p->valid = TRUE;
  binstr_add_ptr(bs, p);
  p->direct = FALSE;
}

/* 1度のみ呼ばれる.
 * BinStrPtrとBinStrをセットで初期化する(双方向なポインタにする) */
static inline void bsptr_init_direct(struct BinStrPtr *p,
                                     struct BinStrRef *bs) {
  bsptr_init(p, bs);
  p->direct = TRUE;
}

static inline int bsptr_pos(struct BinStrPtr *p) { return p->pos; }

static inline void bsptr_copy_to(const BinStrPtrRef from, BinStrPtrRef to) {
  to->binstr = from->binstr;
  to->pos = from->pos;
  to->valid = from->valid;
  to->direct = from->direct;
}

static inline void bsptr_destroy(struct BinStrPtr *p) {}

static inline BOOL bsptr_valid(BinStrPtrRef p) { return p->valid; }

/* ポインタpが指すバイナリストリングに、vからサイズsize分だけ書き込む.
 * 書き込みに成功した場合は1を, 失敗した場合は0を返す.
 * BYTE型(8bit)へキャストした任意のサイズの書き込みデータvを,
 * サイズsizeまでポインタアクセスで読み出し,
 * 4bitずつバイナリストリングpへ書き込む. sizeが奇数の場合(size & 1),
 * 8bit単位からあぶれる最後の4bitをv[size>>1] & 0x0fで求めて書き込む.
 * bit単位で書き込むため, ループを回し, 1度のループで, 4bitずつ2回書き込む.
 * 故に, 渡されたsizeの半分half_lenを予め求めておき処理に活用している.  */
static inline int bsptr_push(struct BinStrPtr *p, const BYTE *v, int size) {
  int i;
  int half_len = size >> 1;
  if (!bsptr_valid(p))
    return 0;

  if (p->direct) {
    if (size & 1) {
      binstr_set_direct(p->binstr, v[size >> 1] & 0x0f, p->pos++);
    }
    for (i = half_len - 1; i >= 0; i--) {
      binstr_set_direct(p->binstr, (v[i] >> TAG_BIT_SIZE & 0x0f), p->pos++);
      binstr_set_direct(p->binstr, v[i] & 0x0f, p->pos++);
    }
    return 1;
  } else {
    if (size & 1) {
      if (binstr_set(p->binstr, v[size >> 1] & 0x0f, p->pos))
        p->pos++;
      else {
        bsptr_invalidate(p);
        return 0;
      }
    }

    for (i = half_len - 1; i >= 0; i--) {
      if (binstr_set(p->binstr, (v[i] >> TAG_BIT_SIZE & 0x0f), p->pos)) {
        p->pos++;
      } else {
        bsptr_invalidate(p);
        return 0;
      }
      if (binstr_set(p->binstr, v[i] & 0x0f, p->pos))
        p->pos++;
      else {
        bsptr_invalidate(p);
        return 0;
      }
    }
    return 1;
  }
}

/* pのBinStrのバイト列へ4bitのTAG vを書き込む. */
static inline int bsptr_push1(struct BinStrPtr *p, const BYTE v) {
  if (!bsptr_valid(p))
    return 0;

  if (binstr_set(p->binstr, v & 0x0f, p->pos)) {
    p->pos++;
    return 1;
  } else {
    bsptr_invalidate(p);
    return 0;
  }
}

/* ポインタを無効にする */
static inline void bsptr_invalidate(BinStrPtrRef p) { p->valid = FALSE; }

static inline int bsptr_push_start_mem(BinStrPtrRef p, lmn_interned_str name) {
  if (name == ANONYMOUS) {
    return bsptr_push1(p, TAG_MEM_START);
  } else {
    return bsptr_push1(p, TAG_NAMED_MEM_START) &&
           bsptr_push(p, (BYTE *)&name, sizeof(lmn_interned_str) * TAG_IN_BYTE);
  }
}

static inline int bsptr_push_end_mem(BinStrPtrRef p) {
  return bsptr_push1(p, TAG_MEM_END);
}

static inline int bsptr_push_atom(BinStrPtrRef p, LmnSymbolAtomRef a) {
  /* ファンクタの最大値からファンクタの値を引いて、大小を反転させる */
  LmnFunctor f = (LmnFunctor)FUNCTOR_MAX - LMN_SATOM_GET_FUNCTOR(a);

  return bsptr_push1(p, TAG_ATOM_START) &&
         bsptr_push(p, (const BYTE *)&f, BS_FUNCTOR_SIZE);
}

static inline int bsptr_push_data_atom(BinStrPtrRef p, LmnAtomRef atom,
                                       LmnLinkAttr attr, VisitLogRef log);
static inline int bsptr_push_from(BinStrPtrRef p);
static inline int bsptr_push_hlink(BinStrPtrRef p, LmnAtomRef atom,
                                   VisitLogRef log) {
  HyperLink *hl_root;
  LmnWord ref;
  LmnHlinkRank hl_num;

  /* hyperlink構造を圧縮する際は, rootオブジェクトをバイト列に記録する. */
  hl_root =
      lmn_hyperlink_get_root(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)atom));
  hl_num = lmn_hyperlink_element_num(hl_root);
  if (visitlog_get_hlink(log, hl_root, &ref)) {
    return bsptr_push1(p, TAG_VISITED_ATOMHLINK) &&
           bsptr_push(p, (BYTE *)&ref, BS_PROC_REF_SIZE);
  } else {
    visitlog_put_hlink(log, hl_root); /* 訪問済みにした */
    bsptr_push1(p, TAG_HLINK);
    bsptr_push(p, (const BYTE *)&hl_num, BS_HLINK_NUM_SIZE);

    if (LMN_HL_HAS_ATTR(hl_root)) {
      LmnLinkAttr attr = LMN_HL_ATTRATOM_ATTR(hl_root);
      LmnAtomRef attrAtom = LMN_HL_ATTRATOM(hl_root);

      /* データアトムの場合 */
      if (LMN_ATTR_IS_DATA(attr)) {
        return bsptr_push_data_atom(p, attrAtom, attr, log);
      } else {
        return bsptr_push_atom(p, (LmnSymbolAtomRef)attrAtom);
      }
    } else {
      bsptr_push_from(p);
    }

    if (!bsptr_valid(p)) {
      return 0;
    } else {
      return 1;
    }
  }
}

/* データアトムをバイト列へ書き込む. まずTAG_INT_DATA or
 * TAG_DBL_DATAを書き込んでから, 4bitずつ値を書き込んでいく.
 * ハイパーリンクの処理を追加 @rev.461 */
static inline int bsptr_push_data_atom(BinStrPtrRef p, LmnAtomRef atom,
                                       LmnLinkAttr attr, VisitLogRef log) {
  switch (attr) {
  case LMN_INT_ATTR:
    return bsptr_push1(p, TAG_INT_DATA) &&
           bsptr_push(p, (const BYTE *)&atom, BS_INT_SIZE);
  case LMN_DBL_ATTR:
    return bsptr_push1(p, TAG_DBL_DATA) &&
           bsptr_push(p, (const BYTE *)LMN_GETREF_DOUBLE(atom), BS_DBL_SIZE);
  case LMN_HL_ATTR:
    return bsptr_push_hlink(p, atom, log);
  case LMN_SP_ATOM_ATTR:
    if (lmn_is_string(atom, attr)) {
      lmn_interned_str id = lmn_intern(lmn_string_c_str(LMN_STRING(atom)));
      return bsptr_push1(p, TAG_STR_DATA) &&
             bsptr_push(p, (const BYTE *)&id, BS_STR_ID_SIZE);
    } /*
    else
      FALLTHROUGH  */
  default:
    lmn_fatal("no implementations");
    break;
  }
  return 0;
}

/* 書き込んだ"順番"を参照IDとして書き込む */
static inline int bsptr_push_visited_atom(BinStrPtrRef p, int n, int arg) {
  return bsptr_push1(p, TAG_VISITED_ATOMHLINK) &&
         bsptr_push(p, (BYTE *)&n, BS_PROC_REF_SIZE) &&
         bsptr_push(p, (BYTE *)&arg, BS_ATOM_REF_ARG_SIZE);
}

static inline int bsptr_push_visited_mem(BinStrPtrRef p, int n) {
  return bsptr_push1(p, TAG_VISITED_MEM) &&
         bsptr_push(p, (BYTE *)&n, BS_PROC_REF_SIZE);
}

static inline int bsptr_push_escape_mem(BinStrPtrRef p) {
  return bsptr_push1(p, TAG_ESCAPE_MEM);
}

static inline int bsptr_push_escape_mem_data(BinStrPtrRef p, LmnAtomRef atom,
                                             LmnLinkAttr attr,
                                             VisitLogRef log) {
  return bsptr_push1(p, TAG_ESCAPE_MEM_DATA) &&
         bsptr_push_data_atom(p, atom, attr, log);
}

static inline int bsptr_push_from(BinStrPtrRef p) {
  return bsptr_push1(p, TAG_FROM);
}

static inline int bsptr_push_start_rulesets(BinStrPtrRef p, int n) {
  if (n == 1)
    return bsptr_push1(p, TAG_RULESET1);
  else {
    return bsptr_push1(p, TAG_RULESET) &&
           bsptr_push(p, (BYTE *)&n, BS_RULESET_NUM_SIZE);
  }
}

static inline int bsptr_push_ruleset(BinStrPtrRef p, LmnRuleSetRef rs) {
  int id;

  id = rs->id;
  return bsptr_push(p, (BYTE *)&id, BS_RULESET_SIZE);
}

/* 履歴表は, interned_idをkeyに, valueを0にしている */
static inline int bsptr_push_history_f(st_data_t _key, st_data_t _value,
                                       st_data_t _arg) {
  BinStrPtrRef bsp;
  lmn_interned_str id;

  bsp = (BinStrPtrRef)_arg;
  id = (lmn_interned_str)_key;
  bsptr_push(bsp, (BYTE *)&id, BS_HISTORY_SIZE);

  return ST_CONTINUE;
}

static inline void bsptr_push_rule_histories(BinStrPtrRef bsp, LmnRuleRef r) {
  st_table_t his_tbl;
  unsigned int his_num;

  his_tbl = r->history_tbl;
  his_num = his_tbl ? st_num(his_tbl) : 0;
  bsptr_push(bsp, (BYTE *)&his_num,
             BS_HISTORY_NUM_SIZE); /* write history num */

  if (his_num > 0) { /* write each id of histories */
    st_foreach(his_tbl, (st_iter_func)bsptr_push_history_f, (st_data_t)bsp);
  }
}

static inline void bsptr_push_ruleset_uniq(BinStrPtrRef bsp, LmnMembraneRef mem,
                                           int n) {
  unsigned int i, j;

  /* write UNIQ_TAG and Number of All Rulesets */
  bsptr_push1(bsp, TAG_RULESET_UNIQ);
  bsptr_push(bsp, (BYTE *)&n, BS_RULESET_NUM_SIZE);

  for (i = 0; i < n; i++) { /* foreach ruleset */
    LmnRuleSetRef rs = lmn_mem_get_ruleset(mem, i);
    bsptr_push_ruleset(bsp, rs); /* write ruleset id */

    for (j = 0; j < rs->num; j++) { /* foreach rule history */
      bsptr_push_rule_histories(bsp, rs->get_rule(j));
    }
  }
}

/*----------------------------------------------------------------------
 * Membrane Encode
 * 膜を一意なバイナリストリングにエンコードする
 */

/* prototypes */

static void encode_root_mem(LmnMembraneRef mem, BinStrPtrRef bsp,
                            VisitLogRef visited);
static Vector *mem_atoms(LmnMembraneRef mem);
static Vector *mem_functors(LmnMembraneRef mem);
static void write_mem_atoms(LmnMembraneRef mem, BinStrPtrRef bsp,
                            VisitLogRef visited);
static void write_mols(Vector *atoms, BinStrPtrRef bsp, VisitLogRef visited);
static void write_mem(LmnMembraneRef mem, LmnAtomRef from_atom,
                      LmnLinkAttr attr, int from, BinStrPtrRef bsp,
                      VisitLogRef visited, BOOL is_id);
static void write_mems(LmnMembraneRef mem, BinStrPtrRef bsp,
                       VisitLogRef visited);
static void write_mol(LmnAtomRef atom, LmnLinkAttr attr, int from,
                      BinStrPtrRef bsp, VisitLogRef visited, BOOL is_id);
static void write_rulesets(LmnMembraneRef mem, BinStrPtrRef bsp);
static LmnBinStrRef lmn_mem_encode_sub(LmnMembraneRef mem,
                                       unsigned long tbl_size);

/* memを一意なバイナリストリングに変換する */
LmnBinStrRef lmn_mem_encode(LmnMembraneRef mem) {
  LmnBinStrRef ret;
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__MENC_CANONICAL);
  }
#endif

  // ret = lmn_mem_encode_sub(mem, 1024);
  ret = lmn_mem_encode_sub(mem, round2up(env_next_id() + 1));

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__MENC_CANONICAL);
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, lmn_binstr_space(ret));
  }
#endif

  return ret;
}

LmnBinStrRef lmn_mem_encode_delta(struct MemDeltaRoot *d) {
  LmnBinStrRef ret_bs;

  dmem_root_commit(d);
  ret_bs =
      lmn_mem_encode_sub(dmem_root_get_root_mem(d), dmem_root_get_next_id(d));
  dmem_root_revert(d);

  return ret_bs;
}

/* memを一意なバイナリストリングに変換する */
static LmnBinStrRef lmn_mem_encode_sub(LmnMembraneRef mem,
                                       unsigned long tbl_size) {
  struct BinStrPtr bsp;
  VisitLogRef visited = visitlog_create();
  LmnBinStrRef ret_bs;
  BinStr bs;

  bs = binstr_make();
  bsptr_init(&bsp, bs);
  visitlog_init_with_size(visited, tbl_size);

  encode_root_mem(mem, &bsp, visited);

  /* 最後に、ポインタの位置を修正する */
  bs->cur = bsp.pos;
  ret_bs = binstr_to_lmn_binstr(bs);

  binstr_free(bs);
  bsptr_destroy(&bsp);
  visitlog_destroy(visited);

  return ret_bs;
}

static void encode_root_mem(LmnMembraneRef mem, BinStrPtrRef bsp,
                            VisitLogRef visited) {
  write_mem_atoms(mem, bsp, visited);
  write_mems(mem, bsp, visited);
  write_rulesets(mem, bsp);
}

/* 膜memの全てのアトムのバイナリストリングを書き込む.
 * 辿ってきたアトムfrom_atomとそのアトムの属性attrとこちらからのリンク番号fromを受け取る.
 */
static void write_mem(LmnMembraneRef mem, LmnAtomRef from_atom,
                      LmnLinkAttr attr, int from, BinStrPtrRef bsp,
                      VisitLogRef visited, BOOL is_id) {
  LmnWord n_visited;

  if (!mem)
    return;

  if (!bsptr_valid(bsp))
    return;

  /* 訪問済み */
  if (visitlog_get_mem(visited, mem, &n_visited)) {
    bsptr_push_visited_mem(bsp, n_visited);

    if (from_atom) { /* 引き続きアトムをたどる */
      write_mol(from_atom, attr, from, bsp, visited, is_id);
    }
    return;
  }

  visitlog_put_mem(visited, mem);
  bsptr_push_start_mem(bsp, LMN_MEM_NAME_ID(mem));

  if (!bsptr_valid(bsp))
    return;

  if (from_atom) {
    write_mol(from_atom, attr, from, bsp, visited, is_id);
  }

  /* アトム・膜・ルールセットの順に書込み */
  if (is_id) { /* 膜に対して一意なIDとなるバイナリストリングへエンコードする場合
                */
    write_mem_atoms(mem, bsp, visited);
    write_mems(mem, bsp, visited);
  } else { /* 単なるバイナリストリングへエンコードする場合 */
    dump_mem_atoms(mem, bsp, visited);
    dump_mems(mem, bsp, visited);

    /* 膜memに存在するデータアトムを起点にしたinside
     * proxyアトムをちゃんと書き込んでおく */
    {
      AtomListEntryRef ent = lmn_mem_get_atomlist(mem, LMN_IN_PROXY_FUNCTOR);
      if (ent) {
        LmnSymbolAtomRef in, out;
        EACH_ATOM(
            in, ent, ({
              if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(in, 1)) ||
                  visitlog_get_atom(visited, in, NULL)) {
                continue;
              }
              /* -------------------------+
               * [DATA ATOM]-0--1-[in]-0--|--0-[out]-1--..
               * -------------------------+
               */
              bsptr_push_escape_mem_data(bsp, LMN_SATOM_GET_LINK(in, 1),
                                         LMN_SATOM_GET_ATTR(in, 1), visited);
              out = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(in, 0);
              write_mol(LMN_SATOM_GET_LINK(out, 1), LMN_SATOM_GET_ATTR(out, 1),
                        LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(out, 1)), bsp,
                        visited, is_id);
            }));
      }
    }
  }

  write_rulesets(mem, bsp);
  bsptr_push_end_mem(bsp);
}

static void write_mem_atoms(LmnMembraneRef mem, BinStrPtrRef bsp,
                            VisitLogRef visited) {
  Vector *atoms;

  if (!bsptr_valid(bsp))
    return;

  atoms = mem_atoms(mem);

  write_mols(atoms, bsp, visited);
  vec_free(atoms);
}

/* アトムatomをバイナリストリングへ書き込む
 * 入力:
 *   アトムatomと, atomのリンク属性attr,
 *   アトムatomへ辿ってきた際に辿ったリンクと接続するリンク番号from,
 *   エンコード領域bsp, 訪問管理visited,
 * is_idは計算するバイナリストリングがmem_idならば真 */
static void write_mol(LmnAtomRef atom, LmnLinkAttr attr, int from,
                      BinStrPtrRef bsp, VisitLogRef visited, BOOL is_id) {
  int i_arg;
  int arity;
  LmnWord n_visited;
  LmnFunctor f;

  if (!bsptr_valid(bsp))
    return;

  /* データアトムの場合 */
  if (LMN_ATTR_IS_DATA(attr)) {
    bsptr_push_data_atom(bsp, atom, attr, visited);
    return;
  }

  f = LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)atom);
  if (f == LMN_OUT_PROXY_FUNCTOR) {
    /* outside proxyの場合, inside proxy側の膜をwrite_memで書き込む */
    LmnSymbolAtomRef in;
    LmnMembraneRef in_mem;

    in = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK((LmnSymbolAtomRef)atom, 0);
    in_mem = LMN_PROXY_GET_MEM(in);
    if (visitlog_get_atom(visited, in, NULL)) {
      visitlog_put_atom(visited, in);
    }
    write_mem(in_mem, LMN_SATOM_GET_LINK(in, 1), LMN_SATOM_GET_ATTR(in, 1),
              LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(in, 1)), bsp, visited,
              is_id);
  } else if (f == LMN_IN_PROXY_FUNCTOR) {
    /* inside proxyの場合, 親膜へ抜ける旨を示すタグTAG_ESCAPE_MEMを書き込む.
     * その後, outside proxyから分子のトレース(write_mol)を引き続き実行する */
    LmnSymbolAtomRef out =
        (LmnSymbolAtomRef)LMN_SATOM_GET_LINK((LmnSymbolAtomRef)atom, 0);
    bsptr_push_escape_mem(bsp);

    if (visitlog_get_atom(visited, (LmnSymbolAtomRef)atom, NULL)) {
      visitlog_put_atom(visited, (LmnSymbolAtomRef)atom);
    }

    write_mol(LMN_SATOM_GET_LINK(out, 1), LMN_SATOM_GET_ATTR(out, 1),
              LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(out, 1)), bsp, visited,
              is_id);
  } else if (!visitlog_get_atom(visited, (LmnSymbolAtomRef)atom, &n_visited)) {
    /* 未訪問のシンボルアトムの場合 */
    visitlog_put_atom(visited, (LmnSymbolAtomRef)atom);
    bsptr_push_atom(bsp, (LmnSymbolAtomRef)atom);
    if (!bsptr_valid(bsp))
      return;

    arity = LMN_FUNCTOR_GET_LINK_NUM(f);
    for (i_arg = 0; i_arg < arity; i_arg++) {
      if (i_arg == from) {    /* 辿ってきたリンクに接続 */
        bsptr_push_from(bsp); /* TAG_FROM */
        continue;
      }
      write_mol(
          LMN_SATOM_GET_LINK((LmnSymbolAtomRef)atom, i_arg),
          LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)atom, i_arg),
          LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)atom, i_arg)),
          bsp, visited, is_id);
    }
  } else {
    /* 訪問済のシンボルアトムの場合 */
    bsptr_push_visited_atom(bsp, n_visited, from);
  }
}

/* atomsに含まれるアトムを起点とする未訪問分子を、バイナリストリングが
   最小となるように書き込む */
static void write_mols(Vector *atoms, BinStrPtrRef bsp, VisitLogRef visited) {
  int i, natom;
  struct BinStrPtr last_valid_bsp;
  int last_valid_i, first_func = 0;
  CheckpointRef last_valid_checkpoint = NULL;

  if (!bsptr_valid(bsp))
    return;

  /* atoms中の未訪問のアトムを起点とする分子を、それぞれ試みる */
  natom = vec_num(atoms);
  last_valid_i = -1;
  for (i = 0; i < natom; i++) {
    LmnSymbolAtomRef atom = (LmnSymbolAtomRef)vec_get(atoms, i);

    if (!atom || LMN_IS_HL(atom))
      continue;
    /* 最適化: 最小のファンクタ以外は試す必要なし */
    else if (last_valid_i >= 0 && LMN_SATOM_GET_FUNCTOR(atom) != first_func)
      break;
    else if (visitlog_get_atom(visited, atom, NULL)) {
      continue;
    } else {
      struct BinStrPtr new_bsptr;

      bsptr_copy_to(bsp, &new_bsptr);
      visitlog_set_checkpoint(visited);

      write_mol((LmnAtomRef)atom, LMN_ATTR_MAKE_LINK(0), -1, &new_bsptr,
                visited, TRUE);
      if (bsptr_valid(&new_bsptr)) {
        /* atomからたどった分子が書き込みに成功したので、last_validに記憶する */
        if (last_valid_i < 0) {
          first_func = LMN_SATOM_GET_FUNCTOR(atom);
        }
        if (last_valid_i >= 0) {
          bsptr_destroy(&last_valid_bsp);
          checkpoint_free(last_valid_checkpoint);
        }
        last_valid_bsp = new_bsptr;
        last_valid_checkpoint = visitlog_pop_checkpoint(visited);
        last_valid_i = i;
      } else {
        bsptr_destroy(&new_bsptr);
        visitlog_revert_checkpoint(visited);
      }
    }
  }

  if (last_valid_i >= 0) {
    /* 書き込みに成功した分子をログに記録して、次の分子に進む */
    vec_data_t t = vec_get(atoms, last_valid_i);
    vec_set(atoms, last_valid_i, 0);
    visitlog_push_checkpoint(visited, last_valid_checkpoint);
    write_mols(atoms, &last_valid_bsp, visited);
    vec_set(atoms, last_valid_i, t);

    if (bsptr_valid(&last_valid_bsp)) {
      bsptr_copy_to(&last_valid_bsp, bsp);
      visitlog_commit_checkpoint(visited);
      bsptr_destroy(&last_valid_bsp);
    } else {
      visitlog_revert_checkpoint(visited);
    }
  }
}

/* write_atomsの膜バージョン.
 * ここで書き込む計算する分子には膜のみが含まれている */
static void write_mems(LmnMembraneRef mem, BinStrPtrRef bsp,
                       VisitLogRef visited) {
  LmnMembraneRef m;
  struct BinStrPtr last_valid_bsp;
  BOOL last_valid;
  CheckpointRef last_valid_checkpoint = NULL;

  if (!bsptr_valid(bsp))
    return;

  last_valid = FALSE;
  for (m = lmn_mem_child_head(mem); m; m = lmn_mem_next(m)) {
    if (!visitlog_get_mem(visited, m, NULL)) {
      struct BinStrPtr new_bsptr;

      bsptr_copy_to(bsp, &new_bsptr);
      visitlog_set_checkpoint(visited);

      write_mem(m, 0, -1, -1, &new_bsptr, visited, TRUE);

      if (bsptr_valid(&new_bsptr)) {
        /* mからたどった分子が書き込みに成功したので、last_validに記憶する */
        if (last_valid) {
          bsptr_destroy(&last_valid_bsp);
          checkpoint_free(last_valid_checkpoint);
        }
        last_valid_bsp = new_bsptr;
        last_valid_checkpoint = visitlog_pop_checkpoint(visited);
        last_valid = TRUE;
      } else {
        bsptr_destroy(&new_bsptr);
        visitlog_revert_checkpoint(visited);
      }
    }
  }

  if (last_valid) {
    /* 書き込みに成功した分子をログに記録して、次の分子に進む */
    visitlog_push_checkpoint(visited, last_valid_checkpoint);
    write_mems(mem, &last_valid_bsp, visited);

    if (bsptr_valid(&last_valid_bsp)) {
      bsptr_copy_to(&last_valid_bsp, bsp);
      visitlog_commit_checkpoint(visited);

      bsptr_destroy(&last_valid_bsp);
    } else {
      visitlog_revert_checkpoint(visited);
    }
  }
}

static void write_rulesets(LmnMembraneRef mem, BinStrPtrRef bsp) {
  unsigned int i, n;
  /* ルールセットがルールセットIDでソートされていることに基づいたコード */

  n = lmn_mem_ruleset_num(mem);
  if (n > 0) {
    BOOL has_uniq = FALSE;
    /* TODO: uniqルールセットが存在するか否かを検査するためだけに
     *       O(ルールセット数)かかってしまうため.
     *       ルールの移動や複製を行うプログラムで非効率 */
    for (i = 0; i < n; i++) {
      if (lmn_mem_get_ruleset(mem, i)->has_unique()) {
        has_uniq = TRUE;
        break;
      }
    }
    if (!has_uniq) {
      bsptr_push_start_rulesets(bsp, n);

      for (i = 0; i < n; i++) {
        bsptr_push_ruleset(bsp, lmn_mem_get_ruleset(mem, i));
      }
    } else {
      bsptr_push_ruleset_uniq(bsp, mem, n);
    }
  }
}

/* 膜にあるアトムのファンクタを降順で返す */
static Vector *mem_functors(LmnMembraneRef mem) {
  Vector *v = vec_make(16);
  int i_atomlist;
  for (i_atomlist = lmn_mem_max_functor(mem) - 1; i_atomlist >= 0;
       i_atomlist--) {
    if (lmn_mem_get_atomlist(mem, i_atomlist) &&
        !LMN_IS_PROXY_FUNCTOR(i_atomlist)) {
      vec_push(v, i_atomlist);
    }
  }
  vec_sort(v, comp_functor_greater_f);
  return v;
}

static int comp_functor_greater_f(const void *a_, const void *b_) {
  vec_data_t f0 = *(vec_data_t *)a_;
  vec_data_t f1 = *(vec_data_t *)b_;

  if (functor_priority[f0] == functor_priority[f1]) {
    return f0 > f1 ? -1 : (f0 == f1 ? 0 : 1);
  } else {
    return (int)functor_priority[f0] - (int)functor_priority[f1];
  }
}

/* 膜memに存在する全てのアトムへのポインタをVectorに積めて返す.
 * アトムはfunctor_idの降順 */
static Vector *mem_atoms(LmnMembraneRef mem) {
  Vector *functors;
  Vector *atoms;
  int i;
  AtomListEntryRef ent;
  LmnSymbolAtomRef a;

  functors = mem_functors(mem);
  atoms = vec_make(128);
  for (i = 0; i < vec_num(functors); i++) {
    ent = lmn_mem_get_atomlist(mem, (LmnFunctor)vec_get(functors, i));
    EACH_ATOM(a, ent, ({ vec_push(atoms, (vec_data_t)a); }));
  }

  vec_free(functors);
  return atoms;
}

/*----------------------------------------------------------------------
 * Decode Binary String
 */

static int binstr_decode_cell(LmnBinStrRef bs, int pos, BsDecodeLog *log,
                              int *nvisit, LmnMembraneRef mem,
                              LmnSymbolAtomRef from_atom, int from_arg);
static int binstr_decode_mol(LmnBinStrRef bs, int pos, BsDecodeLog *log,
                             int *nvisit, LmnMembraneRef mem,
                             LmnSymbolAtomRef from_atom, int from_arg);
static int binstr_decode_atom(LmnBinStrRef bs, int pos, BsDecodeLog *log,
                              int *nvisit, LmnMembraneRef mem,
                              LmnSymbolAtomRef from_atom, int from_arg);
static void binstr_decode_rulesets(LmnBinStrRef bs, int *i_bs, Vector *rulesets,
                                   int rs_num);
static void dump_root_mem(LmnMembraneRef mem, BinStrPtrRef bsp,
                          VisitLogRef visitlog);
static LmnBinStrRef lmn_mem_to_binstr_sub(LmnMembraneRef mem,
                                          unsigned long tbl_size);

/* エンコードされた膜をデコードし、構造を再構築する */
static inline LmnMembraneRef lmn_binstr_decode_sub(const LmnBinStrRef bs) {
  LmnMembraneRef groot;
  BsDecodeLog *log;
  int nvisit;

  /* MEMO:
   *   8bit列を, binary stringの長さ * TAG_IN_BYTE(== 2)だけ確保(少し多めになる)
   *   logは, 復元したプロセスへのポインタを持ち,
   * 出現(nvisited)順に先頭から積んでいく */
  log = LMN_NALLOC(BsDecodeLog, bs->len * TAG_IN_BYTE);

  groot = lmn_mem_make();
  lmn_mem_set_active(groot, TRUE); /* globalだから恒真 */
  nvisit =
      VISITLOG_INIT_N; /* カウンタ(== 1): 順序付けを記録しながらデコードする.
                        * (0はグローバルルート膜なので1から) */
  binstr_decode_cell(bs, 0, log, &nvisit, groot, NULL, 0);
  LMN_FREE(log);

  return groot;
}

LmnMembraneRef lmn_binstr_decode(const LmnBinStrRef bs) {
  LmnMembraneRef ret;
  LmnBinStrRef target;

  if (is_comp_z(bs)) {
    target = lmn_bscomp_z_decode(bs);
  } else {
    target = bs;
  }

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__MENC_RESTORE);
  }
#endif
  ret = lmn_binstr_decode_sub(target);
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__MENC_RESTORE);
  }
#endif

  if (is_comp_z(bs)) {
    lmn_binstr_free(target);
  }
  return ret;
}

static int binstr_decode_cell(LmnBinStrRef bs, int pos, BsDecodeLog *log,
                              int *nvisit, LmnMembraneRef mem,
                              LmnSymbolAtomRef from_atom, int from_arg) {
  int i;

  for (i = 0; pos < bs->len; i++) {
    unsigned int tag = BS_GET(bs->v, pos);

    if (tag == TAG_MEM_END) {
      /* 膜の終わり */
      pos++;
      break;
    } else if (tag == TAG_RULESET1) {
      /* ルールセット(only 1) */
      int rs_id;
      pos++;
      rs_id = binstr_get_ruleset(bs->v, pos);
      pos += BS_RULESET_SIZE;
      lmn_mem_add_ruleset(mem, ruleset_table->get(rs_id));
    } else if (tag == TAG_RULESET) {
      /* 複数のルールセット */
      int j, n, rs_id;
      pos++;
      n = binstr_get_ruleset_num(bs->v, pos);
      pos += BS_RULESET_NUM_SIZE;
      for (j = 0; j < n; j++) {
        rs_id = binstr_get_ruleset(bs->v, pos);
        pos += BS_RULESET_SIZE;
        lmn_mem_add_ruleset(mem, ruleset_table->get(rs_id));
      }
    } else if (tag == TAG_RULESET_UNIQ) {
      int rs_num;

      pos++;
      rs_num = binstr_get_ruleset_num(bs->v, pos);
      pos += BS_RULESET_NUM_SIZE;

      binstr_decode_rulesets(bs, &pos, lmn_mem_get_rulesets(mem), rs_num);
    } else {
      if (i == 0) {
        /* 最初の要素は膜の外からアトムをたどって来た可能性がある */
        pos = binstr_decode_mol(bs, pos, log, nvisit, mem, from_atom, from_arg);
      } else { /* それ以外は、アトムからたどられて到達されていない */
        pos = binstr_decode_mol(bs, pos, log, nvisit, mem, NULL, from_arg);
      }
    }
  }
  return pos;
}

/* UNIQ制約を含むルールセットrulesetsを再構築する */
static void binstr_decode_rulesets(LmnBinStrRef bs, int *i_bs, Vector *rulesets,
                                   int rs_num) {
  int i, j, k;
  for (i = 0; i < rs_num; i++) {
    LmnRuleSetRef rs;
    lmn_interned_str id;

    rs =
        ruleset_table->get(binstr_get_ruleset(bs->v, *i_bs))->duplicate();
    (*i_bs) += BS_RULESET_SIZE;

    for (j = 0; j < rs->num; j++) {
      LmnRuleRef r;
      int his_num;

      /* ruleset idから復元したrulesetには既に履歴が存在しており,
       * 履歴ごと複製した可能性がある. そのため,
       * バイナリスストリングから履歴をデコードする前に,
       * ruleset上の履歴を一旦解放する必要がある. MEMO: 現実装では,
       * コピー元となるルールセットオブジェクトに直接履歴を持たせていないため,
       *       上記コメントは考慮しなくてよい. */

      r = rs->get_rule(j);
      his_num = binstr_get_history_num(bs->v, *i_bs);
      (*i_bs) += BS_HISTORY_NUM_SIZE;

      if (his_num > 0) {
        for (k = 0; k < his_num; k++) {
          id = binstr_get_history(bs->v, *i_bs);
          (*i_bs) += BS_HISTORY_SIZE;
          st_add_direct(r->history_tbl, (st_data_t)id, 0);
        }
      }
    }
    lmn_mem_add_ruleset_sort(rulesets, rs);
  }
}

static int binstr_decode_mol(LmnBinStrRef bs, int pos, BsDecodeLog *log,
                             int *nvisit, LmnMembraneRef mem,
                             LmnSymbolAtomRef from_atom, int from_arg) {
  unsigned int tag;
  lmn_interned_str mem_name;

  if (pos >= bs->len)
    return pos;

  tag = BS_GET(bs->v, pos);
  pos++;

  mem_name = ANONYMOUS;

  switch (tag) {
  case TAG_ATOM_START:
    return binstr_decode_atom(bs, pos, log, nvisit, mem, from_atom, from_arg);
  case TAG_NAMED_MEM_START:
    mem_name = binstr_get_mem_name(bs->v, pos);
    pos += BS_MEM_NAME_SIZE;
    /* FALL THROUGH */
  case TAG_MEM_START: {
    LmnMembraneRef new_mem;
    new_mem = lmn_mem_make();
    lmn_mem_set_name(new_mem, mem_name);
    lmn_mem_set_active(new_mem, TRUE);
    lmn_mem_add_child_mem(mem, new_mem);

    log[(*nvisit)].v = (LmnWord)new_mem;
    log[(*nvisit)].type = BS_LOG_TYPE_MEM;
    (*nvisit)++;

    if (from_atom) {
      LmnSymbolAtomRef in, out;
      in = lmn_mem_newatom(new_mem, LMN_IN_PROXY_FUNCTOR);
      out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
      lmn_newlink_in_symbols(in, 0, out, 0);
      lmn_newlink_in_symbols(out, 1, from_atom, from_arg);
      pos = binstr_decode_cell(bs, pos, log, nvisit, new_mem, in, 1);
    } else {
      pos = binstr_decode_cell(bs, pos, log, nvisit, new_mem, from_atom,
                               from_arg);
    }
  } break;
  case TAG_MEM_END:
    break;
  case TAG_ESCAPE_MEM_DATA: {
    LmnSymbolAtomRef in, out;
    LmnWord n;
    unsigned int sub_tag;
    LmnLinkAttr n_attr;

    in = lmn_mem_newatom(mem, LMN_IN_PROXY_FUNCTOR);
    out = lmn_mem_newatom(lmn_mem_parent(mem), LMN_OUT_PROXY_FUNCTOR);
    sub_tag = BS_GET(bs->v, pos);
    pos++;

    if (sub_tag == TAG_INT_DATA) {
      n = (LmnWord)binstr_get_int(bs->v, pos);
      n_attr = LMN_INT_ATTR;
      pos += BS_INT_SIZE;
    } else if (sub_tag == TAG_DBL_DATA) {
      n = (LmnWord)binstr_get_dbl(bs->v, pos);
      n_attr = LMN_DBL_ATTR;
      pos += BS_DBL_SIZE;
    } else if (sub_tag == TAG_STR_DATA) {
      lmn_interned_str n_id = binstr_get_strid(bs->v, pos);
      n = (LmnWord)lmn_string_make(lmn_id_to_name(n_id));
      n_attr = LMN_STRING_ATTR;
      pos += BS_STR_ID_SIZE;
    } else {
      n = 0;
      n_attr = 0; /* false positive対策 */
      lmn_fatal("unexpected");
    }

    /* -----------------+
     * [n]-0--1-[in]-0--|--0-[out]-1--?-..
     * -----------------+
     */
    lmn_newlink_in_symbols(in, 0, out, 0);
    LMN_SATOM_SET_LINK(in, 1, (LmnAtomRef)n);
    LMN_SATOM_SET_ATTR(in, 1, n_attr);
    lmn_mem_push_atom(mem, (LmnAtomRef)n, n_attr);
    pos = binstr_decode_mol(bs, pos, log, nvisit, lmn_mem_parent(mem), out, 1);
  } break;
  case TAG_ESCAPE_MEM: {
    LmnMembraneRef parent = lmn_mem_parent(mem);
    if (from_atom) {
      LmnSymbolAtomRef in, out;

      in = lmn_mem_newatom(mem, LMN_IN_PROXY_FUNCTOR);
      out = lmn_mem_newatom(parent, LMN_OUT_PROXY_FUNCTOR);
      lmn_newlink_in_symbols(in, 0, out, 0);
      lmn_newlink_in_symbols(in, 1, from_atom, from_arg);

      pos = binstr_decode_mol(bs, pos, log, nvisit, parent, out, 1);
    } else {
      pos = binstr_decode_mol(bs, pos, log, nvisit, parent, NULL, 1);
    }
  } break;
  case TAG_HLINK: {
    LmnSymbolAtomRef hl_atom = lmn_hyperlink_new();
    log[(*nvisit)].v = (LmnWord)hl_atom;
    log[(*nvisit)].type = BS_LOG_TYPE_HLINK;
    (*nvisit)++;

    lmn_mem_push_atom(mem, hl_atom, LMN_HL_ATTR);
    lmn_mem_newlink(mem, from_atom, LMN_ATTR_GET_VALUE((LmnWord)from_atom),
                    from_arg, hl_atom, LMN_HL_ATTR, 0);
    pos += BS_HLINK_NUM_SIZE;

    tag = BS_GET(bs->v, pos);
    pos++;
    switch (tag) {
    case TAG_FROM:
      break;
    case TAG_ATOM_START: {
      LmnFunctor f;
      LmnSymbolAtomRef atom;

      f = binstr_get_functor(bs->v, pos); /* functorを持ってくる */
      pos += BS_FUNCTOR_SIZE;
      atom = lmn_new_atom(f); /* アトムを生成する */
      lmn_hyperlink_put_attr(lmn_hyperlink_at_to_hl(hl_atom), atom, 0);
    } break;
    case TAG_INT_DATA: {
      long n;
      n = binstr_get_int(bs->v, pos);
      pos += BS_INT_SIZE;
      lmn_hyperlink_put_attr(lmn_hyperlink_at_to_hl(hl_atom), (LmnAtomRef)n,
                             LMN_INT_ATTR);
    } break;
    case TAG_DBL_DATA: {
      LmnAtomRef n;

      n = (LmnAtomRef)lmn_create_double_atom(binstr_get_dbl(bs->v, pos));
      pos += BS_DBL_SIZE;
      lmn_hyperlink_put_attr(lmn_hyperlink_at_to_hl(hl_atom), n, LMN_DBL_ATTR);
    } break;
    case TAG_STR_DATA: {
      LmnStringRef str;
      lmn_interned_str n;

      n = binstr_get_strid(bs->v, pos);
      pos += BS_STR_ID_SIZE;
      str = lmn_string_make(lmn_id_to_name(n));
      lmn_hyperlink_put_attr(lmn_hyperlink_at_to_hl(hl_atom), (LmnAtomRef)str,
                             LMN_SP_ATOM_ATTR);
    } break;
    default:
      printf("tag = %d\n", tag);
      lmn_fatal("binstr decode, unexpected");
      break;
    }
  } break;
  case TAG_VISITED_ATOMHLINK:
  case TAG_VISITED_MEM: {
    unsigned int ref;
    ref = binstr_get_ref_num(bs->v, pos);
    pos += BS_PROC_REF_SIZE;

    switch (log[ref].type) {
    case BS_LOG_TYPE_ATOM: {
      LmnSymbolAtomRef atom;
      unsigned int arg;
      arg = binstr_get_arg_ref(bs->v, pos);
      pos += BS_ATOM_REF_ARG_SIZE;
      atom = (LmnSymbolAtomRef)log[ref].v;
      if (from_atom) {
        lmn_newlink_in_symbols(atom, arg, from_atom, from_arg);
      }
    } break;
    case BS_LOG_TYPE_MEM: {
      LmnMembraneRef ref_mem = (LmnMembraneRef)log[ref].v;
      if (!from_atom) {
        pos = binstr_decode_mol(bs, pos, log, nvisit, ref_mem, NULL, from_arg);
      } else {
        LmnSymbolAtomRef in, out;

        in = lmn_mem_newatom(ref_mem, LMN_IN_PROXY_FUNCTOR);
        out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);

        lmn_newlink_in_symbols(in, 0, out, 0);
        lmn_newlink_in_symbols(out, 1, from_atom, from_arg);
        pos = binstr_decode_mol(bs, pos, log, nvisit, ref_mem, in, 1);
      }
    } break;
    case BS_LOG_TYPE_HLINK: {
      LmnSymbolAtomRef hl_atom;
      LmnAtomRef ref_hl_atom;

      ref_hl_atom = (LmnAtomRef)log[ref].v;
      hl_atom = (LmnSymbolAtomRef)lmn_copy_atom(ref_hl_atom, LMN_HL_ATTR);

      lmn_newlink_in_symbols(hl_atom, 0, from_atom, from_arg);
      lmn_mem_push_atom(mem, hl_atom, LMN_HL_ATTR);
      lmn_mem_newlink(mem, from_atom, LMN_ATTR_GET_VALUE((LmnWord)from_atom),
                      from_arg, hl_atom, LMN_HL_ATTR, 0);
    } break;
    default:
      lmn_fatal("unexpected reference");
      break;
    }
  } break;
  case TAG_INT_DATA: {
    long n;
    n = binstr_get_int(bs->v, pos);
    pos += BS_INT_SIZE;
    LMN_SATOM_SET_LINK(from_atom, from_arg, (LmnAtomRef)n);
    LMN_SATOM_SET_ATTR(from_atom, from_arg, LMN_INT_ATTR);
    lmn_mem_push_atom(mem, (LmnAtomRef)n, LMN_INT_ATTR);
  } break;
  case TAG_DBL_DATA: {
    LmnAtomRef n;

    n = (LmnAtomRef)lmn_create_double_atom(binstr_get_dbl(bs->v, pos));
    pos += BS_DBL_SIZE;
    LMN_SATOM_SET_LINK(from_atom, from_arg, n);
    LMN_SATOM_SET_ATTR(from_atom, from_arg, LMN_DBL_ATTR);
    lmn_mem_push_atom(mem, n, LMN_DBL_ATTR);
  } break;
  case TAG_STR_DATA: {
    LmnStringRef str;
    lmn_interned_str n;

    n = binstr_get_strid(bs->v, pos);
    pos += BS_STR_ID_SIZE;
    str = lmn_string_make(lmn_id_to_name(n));
    LMN_SATOM_SET_LINK(from_atom, from_arg, str);
    LMN_SATOM_SET_ATTR(from_atom, from_arg, LMN_SP_ATOM_ATTR);
    lmn_mem_push_atom(mem, str, LMN_STRING_ATTR);
  } break;
  default:
    printf("tag = %d\n", tag);
    lmn_fatal("binstr decode, unexpected");
    break;
  }

  return pos;
}

/* bsの位置posから膜memにデコードしたアトムを書き込む
 * *nvisitは出現番号
 * 辿って来た場合, from_atomとそのリンク番号が渡される */
static int binstr_decode_atom(LmnBinStrRef bs, int pos, BsDecodeLog *log,
                              int *nvisit, LmnMembraneRef mem,
                              LmnSymbolAtomRef from_atom, int from_arg) {
  LmnFunctor f;
  int arity, i;
  LmnSymbolAtomRef atom;

  f = binstr_get_functor(bs->v, pos); /* functorを持ってくる */
  pos += BS_FUNCTOR_SIZE;
  arity = LMN_FUNCTOR_ARITY(f); /* functorから, リンク数が分かる */

  atom = lmn_mem_newatom(mem, f); /* アトムを生成する */
  log[(*nvisit)].v = (LmnWord)atom; /* アドレスを記録(*nvisitは初期値1) */
  log[(*nvisit)].type = BS_LOG_TYPE_ATOM;
  (*nvisit)++;

  for (i = 0; i < arity; i++) {
    /* zero clear */
    LMN_SATOM_SET_LINK(atom, i, 0);
  }

  for (i = 0; i < arity; i++) {
    unsigned int tag = BS_GET(bs->v, pos);

    switch (tag) {
    case TAG_FROM:
      /* 辿って来た場合は, リンクを張る */
      lmn_newlink_in_symbols(from_atom, from_arg, atom, i);
      pos++;
      break;
    default:
      if (LMN_SATOM_GET_LINK(atom, i)) {
        /* すでにリンクが設定されているので、相手側から訪問済み */
        pos = binstr_decode_mol(bs, pos, log, nvisit, mem, NULL, i);
      } else {
        pos = binstr_decode_mol(bs, pos, log, nvisit, mem, atom, i);
      }
      break;
    }
  }

  return pos;
}

/*----------------------------------------------------------------------
 * Dump Membrane to Binary String
 */

LmnBinStrRef lmn_mem_to_binstr(LmnMembraneRef mem) {
  LmnBinStrRef ret;
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__MENC_DUMP);
  }
#endif
  // ret = lmn_mem_to_binstr_sub(mem, 128);
  ret = lmn_mem_to_binstr_sub(mem, round2up(env_next_id() + 1));

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, lmn_binstr_space(ret));
    profile_finish_timer(PROFILE_TIME__MENC_DUMP);
  }
#endif
  return ret;
}

/* 膜のdumpを計算する. dump_root_memとかから名称変更したみたい */
static LmnBinStrRef lmn_mem_to_binstr_sub(LmnMembraneRef mem,
                                          unsigned long tbl_size) {
  LmnBinStrRef ret_bs;
  BinStr bs;
  struct BinStrPtr bsp;
  VisitLogRef visitlog = visitlog_create();

  bs = binstr_make();
  bsptr_init_direct(&bsp, bs);
  visitlog_init_with_size(visitlog, tbl_size);

  dump_root_mem(mem, &bsp, visitlog);

  /* 最後に、ポインタの位置を修正する */
  bs->cur = bsp.pos;
  ret_bs = binstr_to_lmn_binstr(bs);

  binstr_free(bs);
  bsptr_destroy(&bsp);
  visitlog_destroy(visitlog);

  return ret_bs;
}

static void dump_root_mem(LmnMembraneRef mem, BinStrPtrRef bsp,
                          VisitLogRef visitlog) {
  dump_mem_atoms(mem, bsp, visitlog); /* 1. アトムから */
  dump_mems(mem, bsp, visitlog);      /* 2. 子膜から */
  write_rulesets(mem, bsp);           /* 3. 最後にルール */
}

/* 膜memに存在する全てのアトムをファンクタIDの降順の列で求め,
 * 求めた列をdump_molsする */
static void dump_mem_atoms(LmnMembraneRef mem, BinStrPtrRef bsp,
                           VisitLogRef visited) {
  Vector *atoms;

  atoms = mem_atoms(mem);

  dump_mols(atoms, bsp, visited);
  vec_free(atoms);
}

/* アトム列atomsから, visitedに未登録のアトムに対し, write_molを行う
 * つまり, mhash同様に, 各アトムを起点とした分子単位でエンコードを行っている */
static void dump_mols(Vector *atoms, BinStrPtrRef bsp, VisitLogRef visited) {
  int i, natom;

  /* atoms中の未訪問のアトムを起点とする分子を、それぞれ試みる */
  natom = vec_num(atoms);
  for (i = 0; i < natom; i++) {
    LmnSymbolAtomRef atom = (LmnSymbolAtomRef)vec_get(atoms, i);

    if (visitlog_get_atom(visited, atom, NULL) || LMN_IS_HL(atom)) {
      continue;
    } else {
      write_mol((LmnAtomRef)atom, LMN_ATTR_MAKE_LINK(0), -1, bsp, visited,
                FALSE);
    }
  }
}

/* 膜中心の計算単位. 未訪問の子膜Xに対して, 子膜の分子を書き込み,
 * dump_memsへ再起する 兄弟膜は子膜内のプロセスを全て書き込んでから訪問される */
static void dump_mems(LmnMembraneRef mem, BinStrPtrRef bsp,
                      VisitLogRef visited) {
  LmnMembraneRef m;

  for (m = lmn_mem_child_head(mem); m; m = lmn_mem_next(m)) {
    if (!visitlog_get_mem(visited, m, NULL)) {
      write_mem(m, 0, -1, -1, bsp, visited, FALSE);
    }
  }
}

/*----------------------------------------------------------------------
 * Membrane Isomorphism
 */

// BS_MEMEQ_OLD is never defined
#ifdef BS_MEMEQ_OLD
#define LmnMeqLog struct VisitLog
#else
#define LmnMeqLog struct TraceLog
#endif

static int mem_eq_enc_mols(LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem,
                           BsDecodeLog *ref_log, int *i_ref, LmnMeqLog *log);
static inline BOOL mem_eq_enc_mol(LmnBinStrRef bs, int *i_bs,
                                  LmnMembraneRef mem, LmnAtomRef atom,
                                  LmnLinkAttr attr, BsDecodeLog *ref_log,
                                  int *i_ref, LmnMeqLog *log);
static inline BOOL mem_eq_enc_end(LmnMembraneRef mem, BOOL rule_flag,
                                  LmnMeqLog *log);
static BOOL mem_eq_enc_atom(LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem,
                            LmnAtomRef atom, LmnLinkAttr attr,
                            BsDecodeLog *ref_log, int *i_ref, LmnMeqLog *log);
static inline BOOL mem_eq_enc_data_atom(unsigned int tag, LmnBinStrRef bs,
                                        int *i_bs, LmnAtomRef atom,
                                        LmnLinkAttr attr, LmnMeqLog *log);
static inline BOOL mem_eq_enc_rulesets(LmnBinStrRef bs, int *i_bs,
                                       LmnMembraneRef mem);
static inline BOOL mem_eq_enc_ruleset(LmnBinStrRef bs, int *i_bs,
                                      LmnRuleSetRef rs);
static inline BOOL mem_eq_enc_rulesets_uniq(LmnBinStrRef bs, int *i_bs,
                                            LmnMembraneRef mem);
static inline BOOL mem_eq_enc_mem(LmnBinStrRef bs, int *i_bs,
                                  LmnMembraneRef mem, BsDecodeLog *ref_log,
                                  int *i_ref, LmnMeqLog *log);
static inline BOOL mem_eq_enc_traced_mem(BOOL is_named, LmnBinStrRef bs,
                                         int *i_bs, LmnMembraneRef mem,
                                         LmnAtomRef atom, LmnLinkAttr attr,
                                         BsDecodeLog *ref_log, int *i_ref,
                                         LmnMeqLog *log);
static inline BOOL mem_eq_enc_escape_mem(LmnBinStrRef bs, int *i_bs,
                                         LmnMembraneRef mem, LmnAtomRef atom,
                                         LmnLinkAttr attr, BsDecodeLog *ref_log,
                                         int *i_ref, LmnMeqLog *log);
static inline BOOL mem_eq_enc_visited(unsigned int tag, LmnBinStrRef bs,
                                      int *i_bs, LmnAtomRef atom,
                                      LmnLinkAttr attr, BsDecodeLog *ref_log,
                                      int *i_ref, LmnMeqLog *log);
static inline BOOL mem_eq_enc_hlink(LmnBinStrRef bs, int *i_bs,
                                    LmnMembraneRef mem, LmnAtomRef atom,
                                    LmnLinkAttr attr, BsDecodeLog *ref_log,
                                    int *i_ref, LmnMeqLog *visitlog);

static BOOL mem_equals_enc_sub(LmnBinStrRef bs, LmnMembraneRef mem,
                               unsigned long tbl_size);
#ifdef BS_MEMEQ_OLD
static long process_num(LmnMembraneRef mem);
#endif

/* 膜のダンプ or エンコードと、膜の同型性判定を行う */
BOOL lmn_mem_equals_enc(LmnBinStrRef bs, LmnMembraneRef mem) {
  BOOL ret;

  if (is_comp_z(bs)) {
    LmnBinStrRef target = lmn_bscomp_z_decode(bs);
    ret = mem_equals_enc_sub(target, mem, round2up(env_next_id() + 1));
    lmn_binstr_free(target);
  } else {
    ret = mem_equals_enc_sub(bs, mem, round2up(env_next_id() + 1));
    // ret = mem_equals_enc_sub(bs, mem, 128);
  }

  return ret;
}

/* 膜のダンプ or エンコードと、膜の同型性判定を行う */
BOOL lmn_mem_equals_enc_delta(LmnBinStrRef bs, struct MemDeltaRoot *d) {
  BOOL t;

  dmem_root_commit(d);
  t = lmn_mem_equals_enc(bs, dmem_root_get_root_mem(d));
  dmem_root_revert(d);

  return t;
}

static BOOL mem_equals_enc_sub(LmnBinStrRef bs, LmnMembraneRef mem,
                               unsigned long tbl_size) {
  LmnMeqLog *log;
  BsDecodeLog *ref_log;
  int i_bs, i_ref;
  BOOL t;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__STATE_COMPARE_MEQ);
  }
#endif

  i_bs = 0;
  i_ref = VISITLOG_INIT_N;

#ifndef BS_MEMEQ_OLD
  ref_log = NULL;
  log = tracelog_make_with_size(tbl_size);
  tracelog_put_mem(log, mem, TLOG_MATCHED_ID_NONE);
  t = mem_eq_enc_mols(bs, &i_bs, mem, ref_log, &i_ref, log);
  tracelog_free(log);

#else

  /* **とりあえず**これなら参照の数以上のサイズになる */
  ref_log =
      LMN_NALLOC(BsDecodeLog, round2up(binstr_byte_size(bs) * TAG_IN_BYTE));
  log = visitlog_make_with_size(log, tbl_size);
  t = mem_eq_enc_mols(bs, &i_bs, mem, ref_log, &i_ref, log)
      /* memに未訪問のプロセスが存在する場合, FALSE */
      && visitlog_element_num(log) == process_num(mem);
  visitlog_free(log);
  LMN_FREE(ref_log);
#endif

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__STATE_COMPARE_MEQ);
  }
#endif

  return t;
}

/* 膜memに対するトレースを初めて行う際に呼び出す. */
static int mem_eq_enc_mols(LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem,
                           BsDecodeLog *ref_log, int *i_ref, LmnMeqLog *log) {
  unsigned int tag;
  int tmp_i_bs, tmp_i_ref;
  BOOL ok, rule_flag;

  rule_flag = FALSE;
  while (*i_bs < bs->len) {
    tag = BS_GET(bs->v, *i_bs);
    (*i_bs)++;

    switch (tag) {
    case TAG_ATOM_START: {
      LmnFunctor f;
      LmnSymbolAtomRef atom;
      AtomListEntryRef ent;

      f = binstr_get_functor(bs->v, *i_bs);
      ok = FALSE;

      /* 1. 読み出したファンクタのエントリリストを取得する */
      ent = lmn_mem_get_atomlist(mem, f);
      if (!ent)
        return FALSE;

      EACH_ATOM(
          atom, ent, ({
      /* 2. 未チェックのアトムを選択する */
#ifdef BS_MEMEQ_OLD
            if (visitlog_get_atom(log, atom, NULL))
              continue;
            visitlog_set_checkpoint(log);
#else
            if (tracelog_contains_atom(log, atom))
              continue;
            tracelog_set_btpoint(log);
#endif
            tmp_i_bs = *i_bs;
            tmp_i_ref = *i_ref;

            /* 3. 未チェックのアトムを起点にトレースを行う.
             * 　 失敗した場合は読み出し位置を現時点にバックトラックさせるため,
             * tmp変数を使用
             * 4. この先の探索で失敗したらバックトラック */
            if (mem_eq_enc_atom(bs, &tmp_i_bs, mem, atom, LMN_ATTR_MAKE_LINK(0),
                                ref_log, &tmp_i_ref, log) &&
                mem_eq_enc_mols(bs, &tmp_i_bs, mem, ref_log, &tmp_i_ref, log)) {
#ifdef BS_MEMEQ_OLD
              visitlog_commit_checkpoint(log);
#else
              tracelog_continue_trace(log);
#endif
              *i_bs = tmp_i_bs;
              *i_ref = tmp_i_ref;
              ok = TRUE;
              break;
            } else {
#ifdef BS_MEMEQ_OLD
              visitlog_revert_checkpoint(log);
#else
              tracelog_backtrack(log);
#endif
            }
          }));

      return ok;
    } break;
    case TAG_NAMED_MEM_START:
    case TAG_MEM_START: {
      LmnMembraneRef m;
      lmn_interned_str mem_name = ANONYMOUS;

      if (tag == TAG_NAMED_MEM_START) {
        mem_name = binstr_get_mem_name(bs->v, *i_bs);
        (*i_bs) += BS_MEM_NAME_SIZE;
      }

      ok = FALSE;
      for (m = lmn_mem_child_head(mem); m; m = lmn_mem_next(m)) {
        /* 1. 未チェックの子膜を選択する. 同時に膜名チェックを行う */
#ifdef BS_MEMEQ_OLD
        if (LMN_MEM_NAME_ID(m) != mem_name || visitlog_get_mem(log, m, NULL))
          continue;
        visitlog_set_checkpoint(log);
#else
        if (LMN_MEM_NAME_ID(m) != mem_name || tracelog_contains_mem(log, m))
          continue;
        tracelog_set_btpoint(log);
#endif

        tmp_i_bs = *i_bs;
        tmp_i_ref = *i_ref;

        /* 膜を起点にした判定を行う.
         * 1. 子以下の階層を検査
         * 2. この先の探索に失敗したらバックトラック　@rev.450 */
        if (mem_eq_enc_mem(bs, &tmp_i_bs, m, ref_log, &tmp_i_ref, log) &&
            mem_eq_enc_mols(bs, &tmp_i_bs, mem, ref_log, &tmp_i_ref, log)) {
#ifdef BS_MEMEQ_OLD
          visitlog_commit_checkpoint(log);
#else
          tracelog_continue_trace(log);
#endif
          *i_bs = tmp_i_bs;
          *i_ref = tmp_i_ref;
          ok = TRUE;
          break;
        } else {
#ifdef BS_MEMEQ_OLD
          visitlog_revert_checkpoint(log);
#else
          tracelog_backtrack(log);
#endif
        }
      }

      return ok;
    } break;
    case TAG_ESCAPE_MEM_DATA: {

      AtomListEntryRef ent;
      LmnSymbolAtomRef in;
      unsigned int sub_tag;

      sub_tag = BS_GET(bs->v, *i_bs);
      (*i_bs)++;
      ok = FALSE;
      ent = lmn_mem_get_atomlist(mem, LMN_IN_PROXY_FUNCTOR);
      if (!ent)
        return FALSE;

      EACH_ATOM(
          in, ent, ({
            LmnSymbolAtomRef data;
            LmnLinkAttr data_attr;

            /* -----------------+
             * [n]-0--1-[in]-0--|--0-[out]-1--?-..
             * -----------------+
             */

            data = (LmnSymbolAtomRef)LMN_SATOM_GET_LINK(in, 1);
            data_attr = LMN_SATOM_GET_ATTR(in, 1);

            tmp_i_bs = *i_bs;
            tmp_i_ref = *i_ref;

            if (!LMN_ATTR_IS_DATA(data_attr) ||
                !mem_eq_enc_data_atom(sub_tag, bs, &tmp_i_bs, data, data_attr,
                                      log)) {
              /* nがデータアトムでない, もしくは等しいデータアトムでない */
              continue;
            }

        /* シンボルアトムを起点としていないため, バックトラックポインtがない.
         * proxyをバックトラックポイントにする必要がある */
#ifdef BS_MEMEQ_OLD
            visitlog_set_checkpoint(log);
#else
            tracelog_set_btpoint(log);
#endif

            if (mem_eq_enc_escape_mem(bs, &tmp_i_bs, mem, in,
                                      LMN_ATTR_MAKE_LINK(0), ref_log,
                                      &tmp_i_ref, log) &&
                mem_eq_enc_mols(bs, &tmp_i_bs, mem, ref_log, &tmp_i_ref, log)) {
#ifdef BS_MEMEQ_OLD
              visitlog_commit_checkpoint(log);
#else
              tracelog_continue_trace(log);
#endif
              *i_bs = tmp_i_bs;
              *i_ref = tmp_i_ref;
              ok = TRUE;
              break;
            } else {
#ifdef BS_MEMEQ_OLD
              visitlog_revert_checkpoint(log);
#else
              tracelog_backtrack(log);
#endif
            }
          }));

      return ok;
    } break;
    case TAG_MEM_END:
      return mem_eq_enc_end(mem, rule_flag, log);
    case TAG_RULESET1:
      if ((lmn_mem_ruleset_num(mem) != 1) ||
          !mem_eq_enc_ruleset(bs, i_bs, lmn_mem_get_ruleset(mem, 0))) {
        return FALSE;
      }
      rule_flag = TRUE;
      break;
    case TAG_RULESET:
      if (!mem_eq_enc_rulesets(bs, i_bs, mem))
        return FALSE;
      rule_flag = TRUE;
      break;
    case TAG_RULESET_UNIQ:
      if (!mem_eq_enc_rulesets_uniq(bs, i_bs, mem))
        return FALSE;
      rule_flag = TRUE;
      break;
    default:
      lmn_fatal("unexpected");
      break;
    }
  }

  return mem_eq_enc_end(mem, rule_flag, log);
}

/* TAG_MEM_ENDが出たときに,
 * 対象の膜に対して訪問したプロセス数が等しい場合に真を返す. 訪問プロセスは,
 * シンボルアトム(except proxies), 子膜, inside proxies */
static inline BOOL mem_eq_enc_end(LmnMembraneRef mem, BOOL rule_flag,
                                  LmnMeqLog *log) {
  if (!rule_flag && lmn_mem_ruleset_num(mem) != 0) {
    return FALSE;
  } else {
#ifdef BS_MEMEQ_OLD
    return TRUE;
#else
    return tracelog_eq_traversed_proc_num(
        log, mem, lmn_mem_get_atomlist(mem, LMN_IN_PROXY_FUNCTOR),
        lmn_mem_get_atomlist(mem, LMN_EXCLAMATION_FUNCTOR));
#endif
  }
}

/* バイト列bsの*i_bs番目からシンボルアトムデータを読み出し,
 * 膜memに存在するアトムatomと比較する.
 * 既にトレース済みである場合や異なるアトムと比較した場合は直ちに偽を返す.
 * 引き続きバイト列を読み出し,
 * アトムatomを起点にしたプロセスと等価な構造をトレースできた場合に 真を返し,
 * 等価な構造をトレースできない場合は偽を返す  */
static BOOL mem_eq_enc_atom(LmnBinStrRef bs, int *i_bs, LmnMembraneRef mem,
                            LmnAtomRef atom, LmnLinkAttr attr,
                            BsDecodeLog *ref_log, int *i_ref, LmnMeqLog *log) {
  LmnFunctor f;
  int arity;
  int i;

  if (LMN_ATTR_IS_DATA(attr))
    return FALSE;

  /* 1. ファンクタを取得 */
  f = binstr_get_functor(bs->v, *i_bs);
  (*i_bs) += BS_FUNCTOR_SIZE;

  /* アトムatomがファンクタfのアトムでない場合,
   * 既にチェック済みのアトムの場合, FALSEを返す */

  if (f != LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)atom))
    return FALSE;
#ifdef BS_MEMEQ_OLD
  if (!visitlog_put_atom(log, atom))
    return FALSE;
  ref_log[*i_ref].v = (LmnWord)atom;
  ref_log[*i_ref].type = BS_LOG_TYPE_ATOM;
#else
  if (!tracelog_put_atom(log, (LmnSymbolAtomRef)atom, *i_ref, mem))
    return FALSE;
#endif
  (*i_ref)++;
  arity = LMN_FUNCTOR_ARITY(f);

  /* アトムatomの接続先を検査する */
  for (i = 0; i < arity; i++) {
    if (!mem_eq_enc_mol(bs, i_bs, mem,
                        LMN_SATOM_GET_LINK((LmnSymbolAtomRef)atom, i),
                        LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)atom, i), ref_log,
                        i_ref, log)) {
      return FALSE;
    }
  }

  return TRUE;
}

/* outsideproxyアトムatomへ到達した際のトレース(子膜へリンクが突き抜けた場合)
 * -----------------+
 * ...-0--1-[in]-0--|--0-[atom]-1--..
 * -----------------+
 */
static inline BOOL mem_eq_enc_traced_mem(BOOL is_named, LmnBinStrRef bs,
                                         int *i_bs, LmnMembraneRef mem,
                                         LmnAtomRef atom, LmnLinkAttr attr,
                                         BsDecodeLog *ref_log, int *i_ref,
                                         LmnMeqLog *log) {
  LmnMembraneRef in_mem;
  LmnAtomRef in;

  if (LMN_ATTR_IS_DATA(attr) ||
      LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)atom) != LMN_OUT_PROXY_FUNCTOR) {
    return FALSE;
  }

  /* 子膜側のinside proxyアトムを取得 */
  in = LMN_SATOM_GET_LINK((LmnSymbolAtomRef)atom, 0);

  if (is_named) {
    const lmn_interned_str mem_name = binstr_get_mem_name(bs->v, *i_bs);
    (*i_bs) += BS_MEM_NAME_SIZE;
    if (mem_name != LMN_MEM_NAME_ID(LMN_PROXY_GET_MEM((LmnSymbolAtomRef)in))) {
      return FALSE;
    }
  }

  in_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)in);

#ifdef BS_MEMEQ_OLD
  visitlog_put_mem(log, in_mem);
  ref_log[*i_ref].v = (LmnWord)in_mem;
  ref_log[*i_ref].type = BS_LOG_TYPE_MEM;
#else
  tracelog_put_mem(log, in_mem, *i_ref);
  tracelog_put_atom(log, (LmnSymbolAtomRef)in, TLOG_MATCHED_ID_NONE, in_mem);
#endif
  (*i_ref)++;

  /* 1. mem_eq_enc_mol : 引き続き子膜側へ踏み込んで連結分子をトレース
   * 2. mem_eq_enc_mols: 1のトレースに成功したならば,
   * 残りの子膜のコンテンツをトレース. */
  if (mem_eq_enc_mol(
          bs, i_bs, in_mem, LMN_SATOM_GET_LINK((LmnSymbolAtomRef)in, 1),
          LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)in, 1), ref_log, i_ref, log) &&
      mem_eq_enc_mols(bs, i_bs, in_mem, ref_log, i_ref, log)) {
    return TRUE;
  } else {
    return FALSE;
  }
}

static inline BOOL mem_eq_enc_mem(LmnBinStrRef bs, int *i_bs,
                                  LmnMembraneRef mem, BsDecodeLog *ref_log,
                                  int *i_ref, LmnMeqLog *log) {
#ifdef BS_MEMEQ_OLD
  if (!visitlog_put_mem(log, mem))
    return FALSE;
  ref_log[*i_ref].v = (LmnWord)mem;
  ref_log[*i_ref].type = BS_LOG_TYPE_MEM;
#else
  if (!tracelog_put_mem(log, mem, *i_ref))
    return FALSE;
#endif
  (*i_ref)++;

  if (mem_eq_enc_mols(bs, i_bs, mem, ref_log, i_ref, log)) {
    return TRUE;
  } else {
    return FALSE;
  }
}

/* tagに応じたデータアトムをbsから読み出し, アトムatomと比較する.
 * 等しければTRUE, 等しくなければFALSEを返す. */
static inline BOOL mem_eq_enc_data_atom(unsigned int tag, LmnBinStrRef bs,
                                        int *i_bs, LmnAtomRef atom,
                                        LmnLinkAttr attr, LmnMeqLog *log) {

  /* データアトムはシンボルアトムに埋め込まれている.
   * よって, シンボルアトム単位でトレースをバックトラックする現状では
   * データアトムの訪問数を記録しておかなくてもよい.
   * つまり, 最終的にトレースokなシンボルアトムの数さえ一致してさえいれば,
   * そこに埋め込まれたデータアトムの比較結果も全て真である.
   *
   * @see struct LmnMembraneRef: アトムの記録方式を変更 */
  if (tag == TAG_INT_DATA) {
    long n = binstr_get_int(bs->v, *i_bs);
    (*i_bs) += BS_INT_SIZE;

    if ((attr == LMN_INT_ATTR) && (n == (LmnWord)atom)) {
#ifdef BS_MEMEQ_OLD
      visitlog_put_data(log);
#endif
      return TRUE;
    }
  } else if (tag == TAG_DBL_DATA) {
    double n = binstr_get_dbl(bs->v, *i_bs);
    (*i_bs) += BS_DBL_SIZE;

    if ((attr == LMN_DBL_ATTR) && (n == lmn_get_double((LmnDataAtomRef)atom))) {
#ifdef BS_MEMEQ_OLD
      visitlog_put_data(log);
#endif
      return TRUE;
    }
  } else if (tag == TAG_STR_DATA) {
    lmn_interned_str n = binstr_get_strid(bs->v, *i_bs);
    (*i_bs) += BS_STR_ID_SIZE;
    if (lmn_is_string(atom, attr) &&
        (n == lmn_intern(lmn_string_c_str(LMN_STRING(atom))))) {
#ifdef BS_MEMEQ_OLD
      visitlog_put_data(log);
#endif
      return TRUE;
    }
  } else {
    lmn_fatal("unexpected.");
  }

  return FALSE;
}

/* アトムのリンク先(連結分子)を辿る際の入り口.
 * 基本的にはタグを読み出し, タグに応じて比較関数を呼び出す.
 * バイト列bsの*i_bs番目からデータを読み出すことで,
 * 膜memのアトムatomを起点に等価な構造をトレースできた場合に真を返す. */
static inline BOOL mem_eq_enc_mol(LmnBinStrRef bs, int *i_bs,
                                  LmnMembraneRef mem, LmnAtomRef atom,
                                  LmnLinkAttr attr, BsDecodeLog *ref_log,
                                  int *i_ref, LmnMeqLog *log) {
  unsigned int tag;

  tag = BS_GET(bs->v, *i_bs);
  (*i_bs)++;
  switch (tag) {
  case TAG_ATOM_START:
    return mem_eq_enc_atom(bs, i_bs, mem, atom, attr, ref_log, i_ref, log);
  case TAG_NAMED_MEM_START:
    return mem_eq_enc_traced_mem(TRUE, bs, i_bs, mem, atom, attr, ref_log,
                                 i_ref, log);
  case TAG_MEM_START:
    return mem_eq_enc_traced_mem(FALSE, bs, i_bs, mem, atom, attr, ref_log,
                                 i_ref, log);
  case TAG_VISITED_ATOMHLINK:
  case TAG_VISITED_MEM:
    return mem_eq_enc_visited(tag, bs, i_bs, atom, attr, ref_log, i_ref, log);
  case TAG_ESCAPE_MEM:
    return mem_eq_enc_escape_mem(bs, i_bs, mem, atom, attr, ref_log, i_ref,
                                 log);
  case TAG_HLINK:
    return mem_eq_enc_hlink(bs, i_bs, mem, atom, attr, ref_log, i_ref, log);
  case TAG_FROM:
    return TRUE;
  case TAG_INT_DATA: /* FALLTHROUGH */
  case TAG_DBL_DATA: /* FALLTHROUGH */
  case TAG_STR_DATA:
    return mem_eq_enc_data_atom(tag, bs, i_bs, atom, attr, log);
  default:
    lmn_fatal("not implemented");
    break;
  }
}

/* TAG_RULESET1 */
static inline BOOL mem_eq_enc_ruleset(LmnBinStrRef bs, int *i_bs,
                                      LmnRuleSetRef rs) {
  long id;

  id = binstr_get_ruleset(bs->v, *i_bs);
  if (id != rs->id) {
    return FALSE;
  }
  (*i_bs) += BS_RULESET_SIZE;
  return TRUE;
}

/* TAG_RULESETS */
static inline BOOL mem_eq_enc_rulesets(LmnBinStrRef bs, int *i_bs,
                                       LmnMembraneRef mem) {
  long n;
  int i;

  n = binstr_get_ruleset_num(bs->v, *i_bs);
  if (n != lmn_mem_ruleset_num(mem))
    return FALSE;
  (*i_bs) += BS_RULESET_NUM_SIZE;

  for (i = 0; i < n; i++) {
    if (!mem_eq_enc_ruleset(bs, i_bs, lmn_mem_get_ruleset(mem, i)))
      return FALSE;
  }
  return TRUE;
}

static inline BOOL mem_eq_enc_rulesets_uniq(LmnBinStrRef bs, int *i_bs,
                                            LmnMembraneRef mem) {
  int rs_num;
  Vector *rulesets;
  BOOL result;

  rs_num = binstr_get_ruleset_num(bs->v, *i_bs);
  if (rs_num != lmn_mem_ruleset_num(mem))
    return FALSE;
  (*i_bs) += BS_RULESET_NUM_SIZE;

  /* TODO: on-the-flyにできるはず */
  rulesets = vec_make(rs_num + 1);
  binstr_decode_rulesets(bs, i_bs, rulesets, rs_num);

  result = lmn_rulesets_equals(rulesets, lmn_mem_get_rulesets(mem));

  lmn_mem_rulesets_destroy(rulesets);
  LMN_FREE(rulesets);

  return result;
}

static inline BOOL mem_eq_enc_atom_ref(LmnBinStrRef bs, int *i_bs,
                                       LmnAtomRef atom, LmnLinkAttr attr,
                                       BsDecodeLog *ref_log, unsigned int ref,
                                       LmnMeqLog *log);
static inline BOOL mem_eq_enc_mem_ref(LmnBinStrRef bs, int *i_bs,
                                      LmnAtomRef atom, LmnLinkAttr attr,
                                      BsDecodeLog *ref_log, int *i_ref,
                                      unsigned int ref, LmnMeqLog *log);
static inline BOOL mem_eq_enc_hlink_ref(LmnBinStrRef bs, int *i_bs,
                                        LmnAtomRef atom, LmnLinkAttr attr,
                                        BsDecodeLog *ref_log, int *i_ref,
                                        unsigned int ref, LmnMeqLog *visitlog);

/* 訪問済みのプロセスとの比較処理. 等価ならば真を返す. */
static inline BOOL mem_eq_enc_visited(unsigned int tag, LmnBinStrRef bs,
                                      int *i_bs, LmnAtomRef atom,
                                      LmnLinkAttr attr, BsDecodeLog *ref_log,
                                      int *i_ref, LmnMeqLog *log) {
  unsigned int ref;
  BOOL ret;

  ref = binstr_get_ref_num(bs->v, *i_bs);
  (*i_bs) += BS_PROC_REF_SIZE;

#ifndef BS_MEMEQ_OLD
  if (tag == TAG_VISITED_MEM) {
    return mem_eq_enc_mem_ref(bs, i_bs, atom, attr, ref_log, i_ref, ref, log);
  } else if (attr == LMN_HL_ATTR) {
    return mem_eq_enc_hlink_ref(bs, i_bs, atom, attr, ref_log, i_ref, ref, log);
  } else {
    return mem_eq_enc_atom_ref(bs, i_bs, atom, attr, ref_log, ref, log);
  }

#else
  switch (ref_log[ref].type) {
  case BS_LOG_TYPE_ATOM:
    ret = mem_eq_enc_atom_ref(bs, i_bs, atom, attr, ref_log, ref, log);
    break;
  case BS_LOG_TYPE_MEM:
    ret = mem_eq_enc_mem_ref(bs, i_bs, atom, attr, ref_log, i_ref, ref, log);
    break;
  case BS_LOG_TYPE_HLINK:
    ret = mem_eq_enc_hlink_ref(bs, i_bs, atom, attr, ref_log, i_ref, ref, log);
    break;
  default:
    lmn_fatal("unexpected reference");
    ret = FALSE;
    break;
  }
#endif

  return ret;
}

/* 既に一度訪れたアトムにリンク接続する場合 */
static inline BOOL mem_eq_enc_atom_ref(LmnBinStrRef bs, int *i_bs,
                                       LmnAtomRef atom, LmnLinkAttr attr,
                                       BsDecodeLog *ref_log, unsigned int ref,
                                       LmnMeqLog *log) {
  if (LMN_ATTR_IS_DATA(attr)) {
    return FALSE;
  } else {
    unsigned int arg;

    arg = binstr_get_arg_ref(bs->v, *i_bs);
    (*i_bs) += BS_ATOM_REF_ARG_SIZE;

#ifdef BS_MEMEQ_OLD
    return (ref_log[ref].v == (LmnWord)atom) &&
           (LMN_ATTR_GET_VALUE(attr) == arg);
#else
    return LMN_ATTR_GET_VALUE(attr) == arg &&
           ref == tracelog_get_atomMatched(log, (LmnSymbolAtomRef)atom);
#endif
  }
}

/* outside proxyアトムatomを経て, 既に1度訪れた膜へ再訪問する場合.
 * -----------------+
 * ...-0--1-[in]-0--|--0-[atom:out]-1--..
 * -----------------+ */
static inline BOOL mem_eq_enc_mem_ref(LmnBinStrRef bs, int *i_bs,
                                      LmnAtomRef atom, LmnLinkAttr attr,
                                      BsDecodeLog *ref_log, int *i_ref,
                                      unsigned int ref, LmnMeqLog *log) {
  if (LMN_ATTR_IS_DATA(attr) ||
      LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)atom) != LMN_OUT_PROXY_FUNCTOR) {
    return FALSE;
  } else {
    LmnMembraneRef in_mem;
    LmnAtomRef in;

    in = LMN_SATOM_GET_LINK((LmnSymbolAtomRef)atom, 0);
    in_mem = LMN_PROXY_GET_MEM((LmnSymbolAtomRef)in);
#ifndef BS_MEMEQ_OLD
    tracelog_put_atom(log, (LmnSymbolAtomRef)in, TLOG_MATCHED_ID_NONE, in_mem);
    if (ref != tracelog_get_memMatched(log, in_mem))
      return FALSE;
#else
    if (ref_log[ref].v != (LmnWord)in_mem)
      return FALSE;
#endif

    /*  out, inをskipし, nextアトムからmem_eq_enc_molを行う.
     * ------------------------+
     * ..--[next]-0--1-[in]-0--|--0-[out]-1--..
     * ------------------------+
     */
    return mem_eq_enc_mol(
        bs, i_bs, in_mem, LMN_SATOM_GET_LINK((LmnSymbolAtomRef)in, 1),
        LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)in, 1), ref_log, i_ref, log);
  }
}

/* -- */
static inline BOOL mem_eq_enc_hlink_ref(LmnBinStrRef bs, int *i_bs,
                                        LmnAtomRef atom, LmnLinkAttr attr,
                                        BsDecodeLog *ref_log, int *i_ref,
                                        unsigned int ref, LmnMeqLog *log) {
  if (attr != LMN_HL_ATTR) {
    return FALSE; /* 比較先属性がハイパーリンクアトムでなければ偽 */
  } else {
    HyperLink *hl_root;

    hl_root =
        lmn_hyperlink_get_root(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)atom));
#ifndef BS_MEMEQ_OLD
    return ref == tracelog_get_hlinkMatched(log, hl_root);
#else
    visitlog_put_data(log);
    return lmn_hyperlink_eq_hl((HyperLink *)ref_log[ref].v, hl_root);
#endif
  }
}

/* inside proxyアトムatomからのトレース (トレース中に親膜へ抜ける場合)
 * なお, 親膜側からトレースを行うので, 子膜から親膜へ辿る際,
 * 既に親膜は訪問済みテーブルに記録済み.
 * -----------------+
 * ...--1-[atom]-0--|--0-[out]-1--..
 * -----------------+
 */
static inline BOOL mem_eq_enc_escape_mem(LmnBinStrRef bs, int *i_bs,
                                         LmnMembraneRef mem, LmnAtomRef atom,
                                         LmnLinkAttr attr, BsDecodeLog *ref_log,
                                         int *i_ref, LmnMeqLog *log) {
  if (LMN_ATTR_IS_DATA(attr) ||
      LMN_SATOM_GET_FUNCTOR((LmnSymbolAtomRef)atom) != LMN_IN_PROXY_FUNCTOR) {
    return FALSE;
  } else {
    LmnAtomRef out = LMN_SATOM_GET_LINK((LmnSymbolAtomRef)atom, 0);
#ifndef BS_MEMEQ_OLD
    tracelog_put_atom(log, (LmnSymbolAtomRef)atom, TLOG_MATCHED_ID_NONE,
                      LMN_PROXY_GET_MEM((LmnSymbolAtomRef)atom));
#endif
    return mem_eq_enc_mol(bs, i_bs, lmn_mem_parent(mem),
                          LMN_SATOM_GET_LINK((LmnSymbolAtomRef)out, 1),
                          LMN_SATOM_GET_ATTR((LmnSymbolAtomRef)out, 1), ref_log,
                          i_ref, log);
  }
}

static inline BOOL mem_eq_enc_hlink(LmnBinStrRef bs, int *i_bs,
                                    LmnMembraneRef mem, LmnAtomRef atom,
                                    LmnLinkAttr attr, BsDecodeLog *ref_log,
                                    int *i_ref, LmnMeqLog *log) {
  if (attr == LMN_HL_ATTR) { /* 比較先属性がハイパーリンクアトムかチェック */
    HyperLink *hl_root;
    LmnHlinkRank bs_hl_num;

    hl_root =
        lmn_hyperlink_get_root(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)atom));
    bs_hl_num = binstr_get_ref_num(bs->v, *i_bs);
    (*i_bs) += BS_HLINK_NUM_SIZE;

    if (lmn_hyperlink_element_num(hl_root) == bs_hl_num) {

#ifndef BS_MEMEQ_OLD
      if (!tracelog_contains_hlink(log, hl_root)) {
        unsigned int tag;
        tag = BS_GET(bs->v, *i_bs);
        (*i_bs)++;

        switch (tag) {
        case TAG_FROM: {
          if (LMN_HL_HAS_ATTR(hl_root)) {
            return FALSE;
          }
        } break;
        case TAG_ATOM_START: {
          LmnFunctor f;

          f = binstr_get_functor(bs->v, *i_bs); /* functorを持ってくる */
          *i_bs += BS_FUNCTOR_SIZE;
          if (LMN_ATTR_IS_DATA(LMN_HL_ATTRATOM_ATTR(hl_root)) ||
              f != LMN_SATOM_GET_FUNCTOR(
                       (LmnSymbolAtomRef)LMN_HL_ATTRATOM(hl_root))) {
            return FALSE;
          }
        } break;
        case TAG_INT_DATA: {
          long n;
          n = binstr_get_int(bs->v, *i_bs);
          *i_bs += BS_INT_SIZE;
          if (LMN_HL_ATTRATOM_ATTR(hl_root) != LMN_INT_ATTR ||
              n != (LmnWord)LMN_HL_ATTRATOM(hl_root)) {
            return FALSE;
          }
        } break;
        case TAG_DBL_DATA: {
          double *n = LMN_MALLOC(double);

          *n = binstr_get_dbl(bs->v, *i_bs);
          *i_bs += BS_DBL_SIZE;
          if (LMN_HL_ATTRATOM_ATTR(hl_root) != LMN_DBL_ATTR ||
              *n != lmn_get_double((LmnDataAtomRef)LMN_HL_ATTRATOM(hl_root))) {
            return FALSE;
          }
        } break;
        case TAG_STR_DATA: {
          LmnStringRef str;
          lmn_interned_str n;

          n = binstr_get_strid(bs->v, *i_bs);
          str = lmn_string_make(lmn_id_to_name(n));
          *i_bs += BS_STR_ID_SIZE;
          if (LMN_HL_ATTRATOM_ATTR(hl_root) != LMN_STRING_ATTR ||
              !lmn_string_eq(str, (LmnStringRef)LMN_HL_ATTRATOM(hl_root))) {
            return FALSE;
          }
        } break;
        default:
          printf("tag = %d\n", tag);
          lmn_fatal("binstr decode, unexpected");
          break;
        }

        tracelog_put_hlink(log, hl_root, *i_ref);
        (*i_ref)++;
        return TRUE;
      }
#else
      if (visitlog_put_hlink(log, hl_root)) {
        ref_log[*i_ref].v = (LmnWord)hl_root;
        ref_log[*i_ref].type = BS_LOG_TYPE_HLINK;
        (*i_ref)++;
        return TRUE;
      }
#endif
    }
  }

  return FALSE;
}

#ifdef BS_MEMEQ_OLD
/* mem以下にあるアトムと膜の数を返す */
static long process_num(LmnMembrane *mem) {
  LmnMembrane *m;
  long n = 0;

  if (mem == NULL)
    return 0;

  n += lmn_mem_atom_num(mem) + lmn_mem_child_mem_num(mem);

  for (m = mem->child_head; m; m = m->next) {
    n += process_num(m);
  }
  return n;
}
#endif
