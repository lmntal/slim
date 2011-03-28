/*
 * mem_encode.c
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
 * atom ref
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
 * from of traversal
 *   tag         :  0111
 *
 * rule sets(only one ruleset)
 *   tag         :  0100
 *   ruleset id  : 2byte
 *
 * rule sets
 *   tag         :  0101
 *   ruleset num : 4byte
 *   foreach
 *     ruleset id  : 2byte
 *
 * uniq rule sets
 *   tag         :  0110
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
*/

#include "mem_encode.h"
#include "delta_membrane.h"
#include "lmntal_thread.h"
#include "binstr_compress.h"
#include "visitlog.h"
#include "dumper.h"
#include "error.h"
#include "functor.h"
#include "atom.h"
#include "st.h"
#include "slim_header/port.h"
#include "dumper.h"
#include "util.h"
#include "st.h"
#ifdef PROFILE
#  include "../runtime_status.h"
#endif

/* Tags */
#define TAG_ATOM_START       0x0
#define TAG_MEM_START        0x1
#define TAG_MEM_END          0x2
#define TAG_NAMED_MEM_START  0x3
#define TAG_MEM_NORULE_END   0x4
#define TAG_ATOM_REF         0x5
#define TAG_MEM_REF          0x6
#define TAG_ESCAPE_MEM       0x7
#define TAG_FROM             0x8
#define TAG_RULESET1         0x9
#define TAG_RULESET          0xa
#define TAG_RULESET_UNIQ     0xb
#define TAG_INT_DATA         0xc
#define TAG_DBL_DATA         0xd

/* Binary Stringのpositionを進めるためのカウンタ群. */
#define ATOM_REF_SIZE          (TAG_IN_BYTE * sizeof(uint32_t))         /* 訪問番号(4byteへ拡張したが, sizeof(ProcessID)とするのが好ましいはず) */
#define ATOM_REF_ARG_SIZE      (TAG_IN_BYTE * sizeof(LmnArity))         /* アトムあたりのリンク本数は127本までなので1Byteで良い */
#define MEM_REF_SIZE           (TAG_IN_BYTE * sizeof(uint32_t))         /* 訪問番号 */
#define FUNCTOR_SIZE           (TAG_IN_BYTE * sizeof(LmnFunctor))       /* Functor ID */
#define BS_INT_SIZE            (TAG_IN_BYTE * SIZEOF_LONG)              /* lmntalにおける整数データは1wordであるためlongで良い */
#define BS_MEM_NAME_SIZE       (TAG_IN_BYTE * sizeof(lmn_interned_str)) /* 膜名 */
#define BS_RULESET_SIZE        (TAG_IN_BYTE * sizeof(LmnRulesetId))     /* ルールセットID */
#define BS_RULESET_NUM_SIZE    (TAG_IN_BYTE * sizeof(uint32_t))         /* ルールセット数 */
#define BS_RULE_NUM_SIZE       (TAG_IN_BYTE * sizeof(uint32_t))         /* ルール数 */
#define BS_HISTORY_NUM_SIZE    (BS_RULE_NUM_SIZE)
#define BS_HISTORY_SIZE        (TAG_IN_BYTE * sizeof(lmn_interned_str)) /* UNIQ制約が管理する履歴型のサイズ. lmn_histroy_tみたいな型にしたい */
#define BS_DBL_SIZE            (TAG_IN_BYTE * sizeof(double))           /* 浮動小数点数 */

typedef struct BinStr *BinStr;
typedef struct BinStrPtr *BinStrPtr;

/* ファンクタ優先度付けの実装 cf) mem_idの高速化 @hori */
uint16_t functor_priority[FUNCTOR_MAX+1];

/*----------------------------------------------------------------------
 * Prototypes
 */

/* Dump Membrane to Binary String */

static void dump_mem_atoms(LmnMembrane *mem,
                           BinStrPtr bsp,
                           VisitLog visited);
static void dump_mols(Vector *atoms,
                      BinStrPtr bsp,
                      VisitLog visited);
static void dump_mems(LmnMembrane *mem,
                      BinStrPtr bsp,
                      VisitLog visited);

static int comp_functor_greater_f(const void *a_, const void *b_);

/*----------------------------------------------------------------------
 * Initialization
 */

void mem_isom_init()
{
  memset(functor_priority, 0xff, sizeof(uint16_t) * FUNCTOR_MAX + 1);
}

void mem_isom_finalize()
{
}

void set_functor_priority(LmnFunctor f, int priority)
{
  if (priority <= 0) lmn_fatal("implementation error");
  functor_priority[f] = priority;
}

/*----------------------------------------------------------------------
 * Binary String
 */
inline LmnBinStr lmn_binstr_make(unsigned int size)
{
  LmnBinStr bs = LMN_MALLOC(struct LmnBinStr);
  bs->len = size;
  bs->comp_type = 0x00U;
  bs->v   = LMN_NALLOC(BYTE, size);
  return bs;
}

LmnBinStr lmn_binstr_copy(struct LmnBinStr *src_bs)
{
  unsigned long len;
  struct LmnBinStr *dst_bs;

  len = ((src_bs->len +1)/ TAG_IN_BYTE);
  dst_bs = lmn_binstr_make(len);
  dst_bs->len = src_bs->len;

  memcpy(dst_bs->v, src_bs->v, len);

  return dst_bs;
}

inline void lmn_binstr_free(struct LmnBinStr *bs)
{
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_remove_space(PROFILE_SPACE__STATE_BINSTR, lmn_binstr_space(bs));
  }
#endif
  LMN_FREE(bs->v);
  LMN_FREE(bs);
}

unsigned long lmn_binstr_space(struct LmnBinStr *bs)
{
  /* TODO: アラインメントで切り上げる必要があるはず */
  return sizeof(struct LmnBinStr) + sizeof(BYTE) * ((bs->len + 1)/ TAG_IN_BYTE);
}

/* エンコード処理(計算中)に用いるバイナリストリング(作業領域) */
struct BinStr {
  BYTE *v;        /* バイト列(128個で初期化) */
  int size;       /* バッファのサイズ（4ビット単位）: 現在のバイト列の大きさ(128 * TAG_IN_BYTEで初期化) */
  int cur;        /* 書き込み位置（4ビット単位）    : 次に書き込む位置(0で初期化) */
  Vector *ptrs;   /* バッファの位置を指し示す、BinStrPtrのベクタ : BinStrPtrへのポインタ列 */
  Vector *ptrs2;  /* 作業用の領域 */
};

static inline void bsptr_invalidate(BinStrPtr p);
static        void binstr_invalidate_ptrs(struct BinStr *p, int start);
static inline void bsptr_destroy(struct BinStrPtr *p);
static inline int  bsptr_pos(struct BinStrPtr *p);

static struct BinStr *binstr_make()
{
  struct BinStr *p = LMN_MALLOC(struct BinStr);
  p->size = 128 * TAG_IN_BYTE;
  p->v = LMN_NALLOC(BYTE, p->size / TAG_IN_BYTE);
  p->cur = 0;
  p->ptrs = vec_make(64);
  p->ptrs2 = vec_make(64);
  return p;
}

static inline void binstr_free(BinStr p)
{
  vec_free(p->ptrs);
  vec_free(p->ptrs2);
  LMN_FREE(p->v);
  LMN_FREE(p);
}

int binstr_byte_size(LmnBinStr p)
{
  return (p->len / TAG_IN_BYTE) + sizeof(struct LmnBinStr);
}

/* See http://isthe.com/chongo/tech/comp/fnv/ */
#if SIZEOF_LONG == 4
#  define FNV_PRIME 16777619UL
#  define FNV_BASIS 2166136261UL
#elif SIZEOF_LONG == 8
#  define FNV_PRIME  1099511628211UL
#  define FNV_BASIS 14695981039346656037UL
#else
#  error "not supported"
#endif

/* バイナリストリングのハッシュ値を返す */
unsigned long binstr_hash(const LmnBinStr a)
{
  unsigned long hval;
  int i;
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__STATE_HASH_MID);
  }
#endif
  hval = FNV_BASIS;
  i = (a->len + 1) / TAG_IN_BYTE;
  /*
   * FNV-1a hash each octet in the buffer
   */
  while (--i >= 0) {
    /* xor the bottom with the current octet */
    hval ^= (unsigned int) a->v[i];

    /* multiply by the FNV magic prime mod 2^32 or 2^64 */
    hval *= FNV_PRIME;
  }

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__STATE_HASH_MID);
  }
#endif

  return hval;
}

#define BS_SET(a, pos, v)                                               \
  (((pos) & 1) ?                                                        \
   ((a)[(pos)>>1] = ((a)[(pos)>>1] & 0x0f) | ((v) << TAG_BIT_SIZE)) :   \
   ((a)[(pos)>>1] = (v&0x0f) | ((a)[(pos)>>1] & 0xf0)))
#define BS_GET(a, pos)                                                  \
  (((pos) & 1) ? ((a)[(pos)>>1] & 0xf0)>>TAG_BIT_SIZE :  (a)[(pos)>>1] & 0x0f)

/* bsの位置posにbの下位4ビットを書き込む。書き込みに成功した場合は真を
   返し、失敗した場合は偽を返す。*/
static inline int binstr_set(struct BinStr *bs, BYTE b, int pos)
{
  if (bs->size <= pos) {
    bs->size *= 2;
    bs->v = LMN_REALLOC(BYTE, bs->v, bs->size / TAG_IN_BYTE);
  }

  if (bs->cur == pos) {
    BS_SET(bs->v, pos, b);
    bs->cur++;
    return 1;
  }
  else if (bs->cur > pos) {
    if (BS_GET(bs->v, pos) < b) return 0;
    else if (BS_GET(bs->v, pos) > b) {
      /* 現在、curが指す位置よりも前に書き込みを行ったため，
         curよりも後の位置を指すポインタをすべて無効にする */
      binstr_invalidate_ptrs(bs, pos + 1);
      bs->cur = pos + 1;
      BS_SET(bs->v, pos, b);
      return 1;
    }
    else { /* bs->v[pos] == b */
      return 1;
    }
  }
  else { /*(bs->cur < pos)*/
    lmn_fatal("unexpected");
    return 0;
  }
}

static inline int binstr_set_direct(struct BinStr *bs, BYTE b, int pos)
{
  if (bs->size <= pos) {
    bs->size *= 2;
    bs->v = LMN_REALLOC(BYTE, bs->v, bs->size / TAG_IN_BYTE);
  }

  BS_SET(bs->v, pos, b);
  bs->cur = pos+1;
  return 1;
}

/* bsの位置posから1バイト読み込み，返す */
static inline BYTE binstr_get_byte(BYTE *bs, int pos)
{
  return (BS_GET(bs, pos + 1)) | (BS_GET(bs, pos) << 4);
}

static inline uint16_t binstr_get_uint16(BYTE *bs, int pos)
{
  return
    (uint16_t)((binstr_get_byte(bs, pos + 2)) | ((binstr_get_byte(bs, pos)) << 8));
}

static inline uint32_t binstr_get_uint32(BYTE *bs, int pos)
{
  return (uint32_t)(binstr_get_uint16(bs, pos + 4) | (binstr_get_uint16(bs, pos) << 16));
}

static inline uint64_t binstr_get_uint64(BYTE *bs, int pos)
{
  return (uint64_t)(binstr_get_uint32(bs, pos + 8) | (uint64_t)binstr_get_uint32(bs, pos) << 32);
}

/* LMNtal言語はinteger valueの範囲を1wordとしている(型がない)ため, long型で良い */
static inline long binstr_get_int(BYTE *bs, int pos)
{
#if SIZEOF_LONG == 4
  return (long)binstr_get_uint32(bs, pos);
#elif SIZEOF_LONG == 8
  return (long)binstr_get_uint64(bs, pos);
#else
#  error "not supported"
#endif
}

static inline double binstr_get_dbl(BYTE *bs, int pos)
{
  int i;
  union {
    double d;
    BYTE t[8];
  } v;

  for (i = 7; i >= 0; i--) {
    v.t[i] = binstr_get_byte(bs, pos + (7-i) * TAG_IN_BYTE);
  }

  return v.d;
}

static inline LmnWord binstr_get_word(BYTE *bs, int pos)
{
#if SIZEOF_LONG == 4
  return (LmnWord)binstr_get_uint32(bs, pos);
#elif SIZEOF_LONG == 8
  return (LmnWord)binstr_get_uint64(bs, pos);
#else
#  error "not supported"
#endif
}

static inline LmnFunctor binstr_get_functor(BYTE *bs, int pos)
{
  LMN_ASSERT(sizeof(LmnFunctor) == 2);
  long f = binstr_get_uint16(bs, pos);
  return (LmnFunctor)(FUNCTOR_MAX - f);
}

static inline unsigned int binstr_get_ref_num(BYTE *bs, int pos)
{
  LMN_ASSERT(ATOM_REF_SIZE == (sizeof(uint32_t) * TAG_IN_BYTE));
  return (unsigned int)binstr_get_uint32(bs, pos);
}

static inline unsigned int binstr_get_arg_ref(BYTE *bs, int pos)
{
  LMN_ASSERT(ATOM_REF_ARG_SIZE == 2);
  return binstr_get_byte(bs, pos);
}

static inline lmn_interned_str binstr_get_mem_name(BYTE *bs, int pos)
{
  return binstr_get_uint32(bs, pos);
}

static inline long binstr_get_ruleset_num(BYTE *bs, int pos)
{
  return binstr_get_uint32(bs, pos);
}

/* ruleset id(2byte)の取得 */
static inline long binstr_get_ruleset(BYTE *bs, int pos)
{
  return binstr_get_uint16(bs, pos);
}

static inline long binstr_get_history_num(BYTE *bs, int pos)
{
  LMN_ASSERT(BS_HISTORY_NUM_SIZE == 8);
  return binstr_get_uint32(bs, pos);
}

static inline lmn_interned_str binstr_get_history(BYTE *bs, int pos)
{
  LMN_ASSERT(BS_HISTORY_SIZE == 8);
  return binstr_get_uint32(bs, pos);
}

/* start以降を指すポインタをすべて無効にする */
static void binstr_invalidate_ptrs(struct BinStr *p, int start)
{
  int i;
  Vector *tmp;
  int len = vec_num(p->ptrs);
  /* EFFICIENCY */
  for (i = 0; i < len; i++) {
    BinStrPtr bsp = (BinStrPtr)vec_get(p->ptrs, i);
    if (bsptr_pos(bsp) >= start) bsptr_invalidate(bsp);
    else vec_push(p->ptrs2, (vec_data_t)bsp);
  }

  /* swap */
  tmp = p->ptrs;
  p->ptrs = p->ptrs2;
  p->ptrs2 = tmp;
  vec_clear(p->ptrs2);
}

/* バイナリストリングaとbの比較を行いaがbより、小さい、同じ、大きい場合
   に、それぞれ負の値、0、正の値を返す。*/
int binstr_compare(const LmnBinStr a, const LmnBinStr b)
{
  int ret;
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__STATE_COMPARE_MID);
  }
#endif

  if (a->len != b->len) {
    ret = a->len - b->len;
  } else {
    ret = memcmp(a->v, b->v, (a->len+1) / TAG_IN_BYTE);
  }

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__STATE_COMPARE_MID);
  }
#endif
  return ret;
}

/* bsにポインタptrを追加する */
static inline void binstr_add_ptr(const struct BinStr *bs, struct BinStrPtr* ptr)
{
  vec_push(bs->ptrs, (vec_data_t)ptr);
}

/* BinStrをLmnBinStrに変換する */
static inline struct LmnBinStr *binstr_to_lmn_binstr(BinStr bs)
{
  struct LmnBinStr *ret_bs;
  int size = (bs->cur + 1) / 2;
  ret_bs = LMN_MALLOC(struct LmnBinStr);
  ret_bs->v = LMN_NALLOC(BYTE, size);
  ret_bs->comp_type = 0x00U;
  memcpy(ret_bs->v, bs->v, size);
  ret_bs->len = bs->cur;
  if (ret_bs->len & 1) {
    ret_bs->v[ret_bs->len >> 1] = ret_bs->v[ret_bs->len >> 1] & 0x0f;
  }

  return ret_bs;

}

static void binstr_dump(BYTE *bs, int len)
{
  int pos, v_i;

  pos = 0;
  v_i = 1;
  while (pos < len) {
    unsigned int tag = BS_GET(bs, pos);
    pos++;

    switch (tag) {
    case TAG_ATOM_START:
      {
        LmnFunctor f = binstr_get_functor(bs, pos);
        pos += FUNCTOR_SIZE;
        printf("%s/%d_%d ", lmn_id_to_name(LMN_FUNCTOR_NAME_ID(f)),
                                          LMN_FUNCTOR_ARITY(f), v_i++);
      }
      break;
    case TAG_MEM_START:
      {
        printf("_%d{ ", v_i++);
      }
      break;
    case TAG_MEM_END:
      {
        printf("}. ");
      }
      break;
    case TAG_NAMED_MEM_START:
      {
        lmn_interned_str name;
        name = binstr_get_mem_name(bs, pos);
        pos += BS_MEM_NAME_SIZE;
        printf("%s_%d{ ", lmn_id_to_name(name), v_i++);
      }
      break;
    case TAG_ATOM_REF:
      {
        unsigned int ref, arg;
        ref = binstr_get_ref_num(bs, pos);
        pos += ATOM_REF_SIZE;
        arg =  binstr_get_arg_ref(bs, pos);
        pos += ATOM_REF_ARG_SIZE;
        printf("$%d's%d ", ref, arg);
      }
      break;
    case TAG_MEM_REF:
      {
        unsigned int ref;
        ref = binstr_get_ref_num(bs, pos);
        pos += MEM_REF_SIZE;
        printf("#%d ", ref);
      }
      break;
    case TAG_ESCAPE_MEM:
      {
        printf("! ");
      }
      break;
    case TAG_FROM:
      {
        printf("_F_ ");
      }
      break;
    case TAG_INT_DATA:
      {
        long n;

        n = binstr_get_int(bs, pos);
        pos += BS_INT_SIZE;
        printf("_INT%ld_ ", n);
      }
      break;
    case TAG_DBL_DATA:
      {
        double n;

        n = binstr_get_dbl(bs, pos);
        pos += BS_DBL_SIZE;
        printf("_DBL%lf_", n);
      }
      break;
    case TAG_RULESET1:
      {
        int rs_id;

        rs_id = binstr_get_ruleset(bs, pos);
        pos += BS_RULESET_SIZE;
        printf("@%d", rs_id);
      }
      break;
    case TAG_RULESET:
      {
        int j, n, rs_id;

        n = binstr_get_ruleset_num(bs, pos);
        pos += BS_RULESET_NUM_SIZE;
        for (j = 0; j < n; j++) {
          rs_id = binstr_get_ruleset(bs, pos);
          pos += BS_RULESET_SIZE;
          printf("@%d", rs_id);
        }
      }
      break;
    case TAG_RULESET_UNIQ:
      {
        LmnRuleSet rs;
        lmn_interned_str id;
        unsigned int j, k, l, n, rs_id, rule_num, his_num;

        n = binstr_get_ruleset_num(bs, pos);
        pos += BS_RULESET_NUM_SIZE;
        for (j = 0; j < n; j++) {
          rs_id = binstr_get_ruleset(bs, pos);
          pos += BS_RULESET_SIZE;
          printf("@%d/", rs_id);

          /* dump applied histories of uniq constraint rules */

          rs = lmn_ruleset_from_id(rs_id);
          rule_num = lmn_ruleset_rule_num(rs);

          for (k = 0; k < rule_num; k++) {
            printf("[%s", lmn_id_to_name(lmn_rule_get_name(lmn_ruleset_get_rule(rs, k))));

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
      }
      break;
    default:
      printf("pos = %d, len = %d\n", pos, len);
      lmn_fatal("unexpected");
      break;
    }
  }
  printf("\n");
}


/* 開発用：　標準出力にバイナリストリングの内容を出力する */
void lmn_binstr_dump(const LmnBinStr bs)
{
  binstr_dump(bs->v, bs->len);
}

/*----------------------------------------------------------------------
 * Binary String Pointer
 */


/* BinStrの特定の位置を指し示すポインタ。BinStrへの書き込みはBinStrPtr
   を介して行う。他のBinStrPtrの書き込みにより、現在のBinStrPtrが無効に
   なる場合があり、binstr_validが真を返すようになる。 */

struct BinStrPtr {
  struct BinStr *binstr; /* 所属するBinStrを指す */
  int pos;               /* bit (0で初期化) */
  BOOL valid;            /* TRUEで初期化 */
  BOOL direct;           /* FALSEで初期化, directメソッドを用いた場合はTRUEで初期化 */
};

static inline void bsptr_init(struct BinStrPtr *p, struct BinStr *bs)
{
  p->binstr = bs;
  p->pos = 0;
  p->valid = TRUE;
  binstr_add_ptr(bs, p);
  p->direct = FALSE;
}

/* 1度のみ呼ばれる. BinStrPtrとBinStrをセットで初期化する(双方向なポインタにする) */
static inline void bsptr_init_direct(struct BinStrPtr *p, struct BinStr *bs)
{
  bsptr_init(p, bs);
  p->direct = TRUE;
}

static inline int bsptr_pos(struct BinStrPtr *p)
{
  return p->pos;
}

static inline void bsptr_copy_to(const BinStrPtr from, BinStrPtr to)
{
  to->binstr = from->binstr;
  to->pos = from->pos;
  to->valid = from->valid;
  to->direct = from->direct;
}

static inline void bsptr_destroy(struct BinStrPtr *p)
{
}

static inline BOOL bsptr_valid(BinStrPtr p)
{
  return p->valid;
}

/* ポインタpが指すバイナリストリングに、vからサイズsize分だけ書き込む。
   書き込みに成功した場合は1を、失敗した場合は0を返す。*/
/* BYTE型(8bit)にキャストして渡すことで, sizeから8bitずつポインタアクセスで読み出し,
 * binstr_setで4bitずつバイナリストリングとして書き込む
   sizeが奇数の場合(size & 1), 8bit単位からあぶれる最後の4bitをv[size>>1] & 0x0fで求めて書き込む.
   bit単位で書き込むため, ループを回し, 1度のループで, 4bitずつ2回書き込む.
   故に, 渡されたsizeの半分half_lenを予め求めておき処理に活用している.  */
static inline int bsptr_push(struct BinStrPtr *p, const BYTE *v, int size)
{
  int i;
  int half_len = size>>1;
  if (!bsptr_valid(p)) return 0;

  if (p->direct) {
    if (size & 1) {
      binstr_set_direct(p->binstr, v[size>>1] & 0x0f, p->pos++);
    }
    for (i = half_len-1; i >= 0; i--) {
      binstr_set_direct(p->binstr, (v[i]>>TAG_BIT_SIZE & 0x0f), p->pos++);
      binstr_set_direct(p->binstr, v[i] & 0x0f, p->pos++);
    }
    return 1;
  } else {
    if (size & 1) {
      if (binstr_set(p->binstr, v[size>>1] & 0x0f, p->pos)) p->pos++;
      else { bsptr_invalidate(p); return 0;}
    }

    for (i = half_len-1; i >= 0; i--) {
      if (binstr_set(p->binstr, (v[i]>>TAG_BIT_SIZE & 0x0f), p->pos)) {p->pos++;}
      else { bsptr_invalidate(p); return 0;}
      if (binstr_set(p->binstr, v[i] & 0x0f, p->pos)) p->pos++;
      else { bsptr_invalidate(p); return 0;}
    }
    return 1;
  }
}

/* pのBinStrのバイト列へ4bitのTAG vを書き込む. */
static inline int bsptr_push1(struct BinStrPtr *p, const BYTE v)
{
  if (!bsptr_valid(p)) return 0;

  if (binstr_set(p->binstr, v & 0x0f, p->pos)) {
    p->pos++;
    return 1;
  }
  else {
    bsptr_invalidate(p);
    return 0;
  }
}

/* ポインタを無効にする */
static inline void bsptr_invalidate(BinStrPtr p)
{
  p->valid = FALSE;
}

static inline int bsptr_push_start_mem(BinStrPtr p, lmn_interned_str name)
{
  if (name == ANONYMOUS) {
    return bsptr_push1(p, TAG_MEM_START);
  } else {
    return
      bsptr_push1(p, TAG_NAMED_MEM_START) &&
      bsptr_push(p, (BYTE*)&name, sizeof(lmn_interned_str)*TAG_IN_BYTE);
  }
}

static inline int bsptr_push_end_mem(BinStrPtr p)
{
  return bsptr_push1(p, TAG_MEM_END);
}

static inline int bsptr_push_atom(BinStrPtr p, LmnSAtom a)
{
  /* ファンクタの最大値からファンクタの値を引いて、大小を反転させる */
  LmnFunctor f = (LmnFunctor)FUNCTOR_MAX - LMN_SATOM_GET_FUNCTOR(a);

  return
    bsptr_push1(p, TAG_ATOM_START) &&
    bsptr_push(p, (const BYTE*)&f, FUNCTOR_SIZE);
}

/* データアトムをバイト列へ書き込む. まずTAG_INT_DATA or TAG_DBL_DATAを書き込んでから,
 * 4bitずつ値を書き込んでいく */
static inline int bsptr_push_data_atom(BinStrPtr p, LmnAtom atom, LmnLinkAttr attr)
{
  switch (attr) {
  case LMN_INT_ATTR:
    return
      bsptr_push1(p, TAG_INT_DATA) &&
      bsptr_push(p, (const BYTE*)&atom, BS_INT_SIZE);
  case LMN_DBL_ATTR:
    return
      bsptr_push1(p, TAG_DBL_DATA) &&
      bsptr_push(p, (const BYTE*)(double*)atom, BS_DBL_SIZE);
  case LMN_HL_ATTR:
    lmn_fatal("under constructions: verification for hyper graph model");
    break;
  case LMN_STRING_ATTR:
  default: /* Special Atomは未実装 */
    lmn_fatal("not implemented");
    return 0;
  }
}

/* 書き込んだ"順番"を参照IDとして書き込む */
static inline int bsptr_push_visited_atom(BinStrPtr p, int n, int arg)
{
  return
    bsptr_push1(p, TAG_ATOM_REF)           &&
    bsptr_push(p, (BYTE*)&n, ATOM_REF_SIZE) &&
    bsptr_push(p, (BYTE*)&arg, ATOM_REF_ARG_SIZE);
}

static inline int bsptr_push_visited_mem(BinStrPtr p, int n)
{
  return
    bsptr_push1(p, TAG_MEM_REF)           &&
    bsptr_push(p, (BYTE*)&n, MEM_REF_SIZE);
}

static inline int bsptr_push_escape_mem(BinStrPtr p)
{
  return bsptr_push1(p, TAG_ESCAPE_MEM);
}

static inline int bsptr_push_from(BinStrPtr p)
{
  return bsptr_push1(p, TAG_FROM);
}

static inline int bsptr_push_start_rulesets(BinStrPtr p, int n)
{
  if (n == 1) return bsptr_push1(p, TAG_RULESET1);
  else {
    return
      bsptr_push1(p, TAG_RULESET) &&
      bsptr_push(p, (BYTE*)&n, BS_RULESET_NUM_SIZE);
  }
}

static inline int bsptr_push_ruleset(BinStrPtr p, LmnRuleSet rs)
{
  int id ;

  id = lmn_ruleset_get_id(rs);
  return bsptr_push(p, (BYTE*)&id, BS_RULESET_SIZE);
}

/* 履歴表は, interned_idをkeyに, valueを0にしている */
static inline int bsptr_push_history_f(st_data_t _key, st_data_t _value, st_data_t _arg)
{
  BinStrPtr bsp;
  lmn_interned_str id;

  bsp = (BinStrPtr)_arg;
  id  = (lmn_interned_str)_key;
  bsptr_push(bsp, (BYTE*)&id, BS_HISTORY_SIZE);

  return ST_CONTINUE;
}

static inline void bsptr_push_rule_histories(BinStrPtr bsp, LmnRule r)
{
  st_table_t his_tbl;
  unsigned int his_num;

  his_tbl = lmn_rule_get_history_tbl(r);
  his_num = his_tbl ? st_num(his_tbl) : 0;
  bsptr_push(bsp, (BYTE*)&his_num, BS_HISTORY_NUM_SIZE); /* write history num */

  if (his_num > 0) { /* write each id of histories */
    st_foreach(his_tbl, bsptr_push_history_f, (st_data_t)bsp);
  }
}

static inline void bsptr_push_ruleset_uniq(BinStrPtr bsp, LmnMembrane *mem, int n)
{
  unsigned int i, j;

  /* write UNIQ_TAG and Number of All Rulesets */
  bsptr_push1(bsp, TAG_RULESET_UNIQ);
  bsptr_push(bsp, (BYTE*)&n, BS_RULESET_NUM_SIZE);

  for (i = 0; i < n; i++) { /* foreach ruleset */
    LmnRuleSet rs = lmn_mem_get_ruleset(mem, i);
    bsptr_push_ruleset(bsp, rs); /* write ruleset id */

    for (j = 0; j < lmn_ruleset_rule_num(rs); j++) { /* foreach rule history */
      bsptr_push_rule_histories(bsp, lmn_ruleset_get_rule(rs, j));
    }
  }
}

/*----------------------------------------------------------------------
 * Membrane Encode
 */

/*
 * 膜を一意なバイナリストリングにエンコードする */

/* prototypes */

static void encode_root_mem(LmnMembrane *mem, BinStrPtr bsp, VisitLog visited);
static Vector *mem_atoms(LmnMembrane *mem);
static Vector *mem_functors(LmnMembrane *mem);
static void write_mem_atoms(LmnMembrane *mem,
                            BinStrPtr bsp,
                            VisitLog visited);
static void write_mols(Vector *atoms,
                       BinStrPtr bsp,
                       VisitLog visited);
static void write_mem(LmnMembrane *mem,
                      LmnAtom from_atom,
                      LmnLinkAttr attr,
                      int from,
                      BinStrPtr bsp,
                      VisitLog visited,
                      BOOL is_id);
static void write_mems(LmnMembrane *mem,
                       BinStrPtr bsp,
                       VisitLog visited);
static void write_mol(LmnAtom atom,
                      LmnLinkAttr attr,
                      int from,
                      BinStrPtr bsp,
                      VisitLog visited,
                      BOOL is_id);
static void write_rulesets(LmnMembrane *mem, BinStrPtr bsp);
static LmnBinStr lmn_mem_encode_sub(LmnMembrane *mem, unsigned long tbl_size);

/* memを一意なバイナリストリングに変換する */
LmnBinStr lmn_mem_encode(LmnMembrane *mem)
{
  LmnBinStr ret;
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__MENC_CANONICAL);
  }
#endif

  ret = lmn_mem_encode_sub(mem, 1024);


#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__MENC_CANONICAL);
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, lmn_binstr_space(ret));
  }
#endif

  return ret;
}

LmnBinStr lmn_mem_encode_delta(struct MemDeltaRoot *d)
{
  LmnBinStr ret_bs;

  dmem_root_commit(d);
  ret_bs = lmn_mem_encode_sub(dmem_root_get_root_mem(d), dmem_root_get_next_id(d));
  dmem_root_revert(d);

  return ret_bs;
}

/* memを一意なバイナリストリングに変換する */
static LmnBinStr lmn_mem_encode_sub(LmnMembrane *mem, unsigned long tbl_size)
{
  struct BinStrPtr bsp;
  struct VisitLog visited;
  LmnBinStr ret_bs;
  BinStr bs;

  bs = binstr_make();
  bsptr_init(&bsp, bs);
  visitlog_init_with_size(&visited, tbl_size);

  encode_root_mem(mem, &bsp, &visited);

  /* 最後に、ポインタの位置を修正する */
  bs->cur = bsp.pos;
  ret_bs = binstr_to_lmn_binstr(bs);

  binstr_free(bs);
  bsptr_destroy(&bsp);
  visitlog_destroy(&visited);

  return ret_bs;
}

static void encode_root_mem(LmnMembrane *mem, BinStrPtr bsp, VisitLog visited)
{
  write_mem_atoms(mem, bsp, visited);
  write_mems(mem, bsp, visited);
  write_rulesets(mem, bsp);
}

/* 膜memの全てのアトムのバイナリストリングを書き込む.
 * 辿ってきたアトムfrom_atomとそのアトムの属性attrとこちらからのリンク番号fromを受け取る  */
static void write_mem(LmnMembrane *mem,
                      LmnAtom from_atom,
                      LmnLinkAttr attr,
                      int from,
                      BinStrPtr bsp,
                      VisitLog visited,
                      BOOL is_id)
{
  LmnWord n_visited;

  if (!mem) return;

  if (!bsptr_valid(bsp)) return;

  /* 訪問済み */
  if (visitlog_get_mem(visited, mem, &n_visited)) {
    bsptr_push_visited_mem(bsp, n_visited);

    if (from_atom) {
      /* 引き続きアトムをたどる */
      write_mol(from_atom, attr, from, bsp, visited, is_id);
    }
    return;
  }

  visitlog_put_mem(visited, mem);                  /* 訪問済みにした */
  bsptr_push_start_mem(bsp, LMN_MEM_NAME_ID(mem)); /* TAGを書き込む. Namedか無印かで内部分岐 */

  if (!bsptr_valid(bsp)) return;

  /* 辿って来た場合は, 先に, そっちの分子を書き込む */
  if (from_atom != 0) {
    /* 引き続きアトムをたどる */
    write_mol(from_atom, attr, from, bsp, visited, is_id);
  }

  /* アトム・膜・ルールセットの順に書込み */
  if (is_id) { /* 膜のIDを計算する場合 */
    write_mem_atoms(mem, bsp, visited);
    write_mems(mem, bsp, visited);
  } else {
    dump_mem_atoms(mem, bsp, visited);
    dump_mems(mem, bsp, visited);
  }
  write_rulesets(mem, bsp);
  bsptr_push_end_mem(bsp);
}

static void write_mem_atoms(LmnMembrane *mem,
                            BinStrPtr bsp,
                            VisitLog visited)
{
  Vector *atoms;

  if (!bsptr_valid(bsp)) return;

  atoms = mem_atoms(mem);

  write_mols(atoms, bsp, visited);
  vec_free(atoms);
}

/* アトムatomをバイナリストリングへ書き込む
 * 入力:
 *   アトムatomと, atomのリンク属性attr,
 *   アトムatomへ辿ってきた際に辿ったリンクと接続するリンク番号from,
 *   エンコード領域bsp, 訪問管理visited, is_idは計算するバイナリストリングがmem_idならば真 */
static void write_mol(LmnAtom atom,
                      LmnLinkAttr attr,
                      int from,
                      BinStrPtr bsp,
                      VisitLog visited,
                      BOOL is_id)
{
  int i_arg;
  int arity;
  LmnWord n_visited;
  LmnFunctor f;

  if (!bsptr_valid(bsp)) return;

  /* データアトムの場合 */
  if (LMN_ATTR_IS_DATA(attr)) {
    bsptr_push_data_atom(bsp, atom, attr);
    return;
  }

  f = LMN_SATOM_GET_FUNCTOR(atom);

  /* outside proxyの場合 */
  if (f == LMN_OUT_PROXY_FUNCTOR) {
    LmnSAtom in = LMN_SATOM(LMN_SATOM_GET_LINK(atom, 0));
    LmnMembrane *in_mem = LMN_PROXY_GET_MEM(in);    /* inside proxyから子膜in_memを取得 */

    /* 子膜を書き込む.
     * >>> 分子として子膜が現れた場合 <<<
     * 1st: write_mem側で, TAG_MEM_STARTが書き込まれる
     * 2nd: write_mem側で, こちらの残りの分子への訪問を引き継ぐ
     *  */
    write_mem(in_mem,
              LMN_SATOM_GET_LINK(in, 1),
              LMN_SATOM_GET_ATTR(in, 1),
              LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(in, 1)),
              bsp,
              visited,
              is_id);
    return;
  }

  /* inside proxyの場合 */
  if (f == LMN_IN_PROXY_FUNCTOR) {
    /* 親膜のoutside proxyアトムを取得 */
    LmnSAtom out = LMN_SATOM(LMN_SATOM_GET_LINK(atom, 0));

    /* outside proxyアトムは, TAG_ESCAPE_MEMを書き込むのみ. 親膜へ抜けるという意味 */
    bsptr_push_escape_mem(bsp);

    /* outの接続先から再びwrite_molする */
    write_mol(LMN_SATOM_GET_LINK(out, 1),
              LMN_SATOM_GET_ATTR(out, 1),
              LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(out, 1)),
              bsp,
              visited,
              is_id);
    return;
  }

  /* >>>>> 以下, proxyではない場合 <<<<< */

  /* 訪問済みのアトムの場合 */
  if (visitlog_get_atom(visited, LMN_SATOM(atom), &n_visited)) {
    /* atomに訪問済みならば, そのatomへのアドレス値(timeoptでは, プロセスID)をkeyにして, n_visitedへatomの被訪問数をセット
     * *(uint16_t*)(((BYTE*)(((LmnWord*)(ATOM))+2))+ LMN_FUNCTOR_BYTES);*/
    bsptr_push_visited_atom(bsp, n_visited, from);
    return;
  }

  /* PPPP FUNCTOR_IDの大小反転など、ごにょごにょやって小さなIDを作ってpush */
  bsptr_push_atom(bsp, LMN_SATOM(atom));
  if (!bsptr_valid(bsp)) return;

  /* PPPP LMN_SATOM_ID番目の値を1に! */
  visitlog_put_atom(visited, LMN_SATOM(atom));

  arity = LMN_FUNCTOR_GET_LINK_NUM(f);
  for (i_arg = 0; i_arg < arity; i_arg++) {
    /* 深さ優先探索で訪問していく */
    if (i_arg == from) { /* 辿ってきたリンクに接続 */
      bsptr_push_from(bsp);
      continue;
    }
    write_mol(LMN_SATOM_GET_LINK(atom, i_arg),
              LMN_SATOM_GET_ATTR(atom, i_arg),
              LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(atom, i_arg)),
              bsp,
              visited,
              is_id);
  }
}

/* atomsに含まれるアトムを起点とする未訪問分子を、バイナリストリングが
   最小となるように書き込む */
static void write_mols(Vector *atoms,
                       BinStrPtr bsp,
                       VisitLog visited)
{
  int i, natom;
  struct BinStrPtr last_valid_bsp;
  int last_valid_i, first_func=0;
  Checkpoint last_valid_checkpoint = NULL;

  if (!bsptr_valid(bsp)) return;

  /* atoms中の未訪問のアトムを起点とする分子を、それぞれ試みる */
  natom = vec_num(atoms);
  last_valid_i = -1;
  for (i = 0; i < natom; i++) {
    LmnSAtom atom = LMN_SATOM(vec_get(atoms, i));

    if (!atom) continue;
    /* 最適化、最小のファンク以外は試す必要なし */
    else if (last_valid_i>=0 && LMN_SATOM_GET_FUNCTOR(atom) != first_func)
      break;
    else if (visitlog_get_atom(visited, atom, NULL)) {
      continue;
    } else {
      struct BinStrPtr new_bsptr;

      bsptr_copy_to(bsp, &new_bsptr);
      visitlog_set_checkpoint(visited);

      write_mol((LmnAtom)atom, LMN_ATTR_MAKE_LINK(0), -1, &new_bsptr, visited, TRUE);
      if (bsptr_valid(&new_bsptr)) {
        /* atomからたどった分子が書き込みに成功したので、last_validに記憶する */
        if (last_valid_i < 0) { first_func = LMN_SATOM_GET_FUNCTOR(atom); }
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

/* write_atomsの膜バージョン。ここで書き込む計算する分子には、膜のみが
   含まれている */
static void write_mems(LmnMembrane *mem,
                       BinStrPtr bsp,
                       VisitLog visited)
{
  LmnMembrane *m;
  struct BinStrPtr last_valid_bsp;
  BOOL last_valid;
  Checkpoint last_valid_checkpoint = NULL;

  if (!bsptr_valid(bsp)) return;

  last_valid = FALSE;
  for (m = mem->child_head; m; m = m->next) {
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

static void write_rulesets(LmnMembrane *mem, BinStrPtr bsp)
{
  unsigned int i, n;
  /* ルールセットがルールセットIDでソートされていることに基づいたコード */

  n = lmn_mem_ruleset_num(mem);
  if (n > 0) {
    BOOL has_uniq = FALSE;
    /* TODO: uniqルールセットが存在するか否かを検査するためだけに
     *       O(ルールセット数)かかってしまうため.
     *       ルールの移動や複製を行うプログラムで非効率 */
    for (i = 0; i < n; i++) {
      if (lmn_ruleset_has_uniqrule(lmn_mem_get_ruleset(mem, i))) {
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
static Vector *mem_functors(LmnMembrane *mem)
{
  Vector *v = vec_make(16);
#ifdef TIME_OPT
  int i_atomlist;
  for (i_atomlist = mem->max_functor-1; i_atomlist >=0; i_atomlist--) {
    if (mem->atomset[i_atomlist] && !LMN_IS_PROXY_FUNCTOR(i_atomlist)) {
      vec_push(v, i_atomlist);
    }
  }
#else
  AtomListEntry *ent;
  LmnFunctor f;
  EACH_ATOMLIST_WITH_FUNC(mem, ent, f, ({
    if (!LMN_IS_PROXY_FUNCTOR(f)) {
      vec_push(v, f);
    }
  }));
#endif
  vec_sort(v, comp_functor_greater_f);
  return v;
}

static int comp_functor_greater_f(const void *a_, const void *b_)
{
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
static Vector *mem_atoms(LmnMembrane *mem)
{
  Vector *functors;
  Vector *atoms;
  int i;
  AtomListEntry *ent;
  LmnSAtom a;

  functors = mem_functors(mem);
  atoms = vec_make(128);
  for (i = 0; i < vec_num(functors); i++) {
    ent = lmn_mem_get_atomlist(mem, (LmnFunctor)vec_get(functors, i));
    EACH_ATOM(a, ent, ({
      vec_push(atoms, (vec_data_t)a);
    }));
  }

  vec_free(functors);
  return atoms;
}

/*----------------------------------------------------------------------
 * Decode Binary String
 */

static int binstr_decode_cell(LmnBinStr bs,
                              int pos,
                              void **log,
                              int *nvisit,
                              LmnMembrane *mem,
                              LmnSAtom from_atom,
                              int from_arg);
static int binstr_decode_mol(LmnBinStr bs,
                             int pos,
                             void **log,
                             int *nvisit,
                             LmnMembrane *mem,
                             LmnSAtom from_atom,
                             int from_arg);
static int binstr_decode_atom(LmnBinStr bs,
                              int pos,
                              void **log,
                              int *nvisit,
                              LmnMembrane *mem,
                              LmnSAtom from_atom,
                              int from_arg);
static void binstr_decode_rulesets(LmnBinStr bs,
                              int *i_bs,
                              Vector *rulesets,
                              int rs_num);
static void dump_root_mem(LmnMembrane *mem, BinStrPtr bsp, VisitLog visitlog);
static LmnBinStr lmn_mem_to_binstr_sub(LmnMembrane *mem, unsigned long tbl_size);

/* エンコードされた膜をデコードし、構造を再構築する */
inline static LmnMembrane *lmn_binstr_decode_sub(const LmnBinStr bs)
{
  LmnMembrane *groot;
  void **log;
  int nvisit;

  env_reset_proc_ids();

#ifdef DEBUG
  if (lmn_env.debug_memenc) {
    lmn_binstr_dump(bs);
  }
#endif
  /* MEMO:
   *   8bit列を, binary stringの長さ * TAG_IN_BYTE(== 2)だけ確保(少し多めになる)
   *   logは, 復元したプロセスへのポインタを持ち, 出現(nvisited)順に先頭から積んでいく */
  log = LMN_NALLOC(void *, bs->len * TAG_IN_BYTE);

  groot = lmn_mem_make();
  lmn_mem_set_active(groot, TRUE); /* globalだから恒真 */
  nvisit = VISITLOG_INIT_N;        /* カウンタ(== 1): 順序付けを記録しながらデコードするため */
  binstr_decode_cell(bs, 0, log, &nvisit, groot, NULL, 0);
  LMN_FREE(log);

  return groot;
}

LmnMembrane *lmn_binstr_decode(const LmnBinStr bs)
{
  LmnMembrane *ret;
  LmnBinStr target;
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__MENC_RESTORE);
  }
#endif

  target = is_comp_z(bs) ? lmn_bscomp_z_decode(bs)
                         : bs;
  ret    = lmn_binstr_decode_sub(target);
  if (is_comp_z(bs)) {
    lmn_binstr_free(target);
  }

#ifdef PROFILE
  if (lmn_env.profile_level >= 3)
    profile_finish_timer(PROFILE_TIME__MENC_RESTORE);
#endif
  return ret;
}

static int binstr_decode_cell(LmnBinStr bs,
                              int pos,
                              void **log,
                              int *nvisit,
                              LmnMembrane *mem,
                              LmnSAtom from_atom,
                              int from_arg)
{
  int i;

  for (i = 0; pos < bs->len; i++) {
    unsigned int tag = BS_GET(bs->v, pos);

    if (tag == TAG_MEM_END) {
      /* 膜の終わり */
      pos++;
      break;
    }
    else if (tag == TAG_RULESET1) {
      /* ルールセット(only 1) */
      int rs_id;
      pos++;
      rs_id = binstr_get_ruleset(bs->v, pos);
      pos += BS_RULESET_SIZE;
      lmn_mem_add_ruleset(mem, lmn_ruleset_from_id(rs_id));
    }
    else if (tag == TAG_RULESET) {
      /* 複数のルールセット */
      int j, n, rs_id;
      pos++;
      n = binstr_get_ruleset_num(bs->v, pos);
      pos += BS_RULESET_NUM_SIZE;
      for (j = 0; j < n; j++) {
        rs_id = binstr_get_ruleset(bs->v, pos);
        pos += BS_RULESET_SIZE;
        lmn_mem_add_ruleset(mem, lmn_ruleset_from_id(rs_id));
      }
    }
    else if (tag == TAG_RULESET_UNIQ) {
      int rs_num;

      pos++;
      rs_num = binstr_get_ruleset_num(bs->v, pos);
      pos += BS_RULESET_NUM_SIZE;

      binstr_decode_rulesets(bs, &pos, lmn_mem_get_rulesets(mem), rs_num);
    }
    else {
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
static void binstr_decode_rulesets(LmnBinStr bs,
                                   int *i_bs,
                                   Vector *rulesets,
                                   int rs_num)
{
  int i, j, k, rs_id, rule_num, his_num;
  LmnRuleSet rs;
  LmnRule r;
  st_table_t st;
  lmn_interned_str id;

  for (i = 0; i < rs_num; i++) {
    rs_id = binstr_get_ruleset(bs->v, *i_bs);
    (*i_bs) += BS_RULESET_SIZE;
    rs = lmn_ruleset_copy(lmn_ruleset_from_id(rs_id));

    rule_num = lmn_ruleset_rule_num(rs);

    for (j = 0; j < rule_num; j++) {
      r = lmn_ruleset_get_rule(rs, j);

      /* rs_idから復元したrulesetがすでに履歴を持っている可能性があるため、一旦解放する */
      st = lmn_rule_get_history_tbl(r);

      his_num = binstr_get_history_num(bs->v, *i_bs);
      (*i_bs) += BS_HISTORY_NUM_SIZE;

      if (his_num > 0) {

        for (k = 0; k < his_num; k++) {
          id = binstr_get_history(bs->v, *i_bs);
          (*i_bs) += BS_HISTORY_SIZE;
          st_add_direct(st, (st_data_t)id, 0);
        }
      }
    }
    lmn_mem_add_ruleset_sort(rulesets, rs);

  }
}

static int binstr_decode_mol(LmnBinStr bs,
                             int pos,
                             void **log,
                             int *nvisit,
                             LmnMembrane *mem,
                             LmnSAtom from_atom,
                             int from_arg)
{
  unsigned int tag;
  lmn_interned_str mem_name;

  if (pos >= bs->len) return pos;

  tag = BS_GET(bs->v, pos);
  pos++;

  mem_name = ANONYMOUS;

  switch (tag) {
  case TAG_ATOM_START:
    return binstr_decode_atom(bs, pos, log, nvisit, mem, from_atom, from_arg);

  case TAG_NAMED_MEM_START:
    mem_name = binstr_get_mem_name(bs->v, pos);
    pos += BS_MEM_NAME_SIZE;
    /* fall through */
  case TAG_MEM_START:
    {
      LmnMembrane *new_mem;
      new_mem = lmn_mem_make();
      lmn_mem_set_name(new_mem, mem_name);
      lmn_mem_set_active(new_mem, TRUE);
      lmn_mem_add_child_mem(mem, new_mem);

      log[(*nvisit)++] = new_mem;
      if (from_atom) {
        LmnSAtom in, out;
        in = lmn_mem_newatom(new_mem, LMN_IN_PROXY_FUNCTOR);
        out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
        lmn_newlink_in_symbols(in, 0, out, 0);
        lmn_newlink_in_symbols(out, 1, from_atom, from_arg);
        pos = binstr_decode_cell(bs, pos, log, nvisit, new_mem, in, 1);
      } else {
        pos = binstr_decode_cell(bs, pos, log, nvisit, new_mem, from_atom, from_arg);
      }
    }
    break;
  case TAG_MEM_END:
    break;
  case TAG_ESCAPE_MEM:
    {
      LmnMembrane *parent;
      LmnSAtom in, out;

      parent = mem->parent;
      if (from_atom) {
        in = lmn_mem_newatom(mem, LMN_IN_PROXY_FUNCTOR);
        out = lmn_mem_newatom(parent, LMN_OUT_PROXY_FUNCTOR);
        lmn_newlink_in_symbols(in, 0, out, 0);
        lmn_newlink_in_symbols(in, 1, from_atom, from_arg);

        pos = binstr_decode_mol(bs, pos, log, nvisit, parent, out, 1);
      } else {
        pos = binstr_decode_mol(bs, pos, log, nvisit, parent, NULL, 1);
      }
    }
    break;
  case TAG_ATOM_REF:
    {
      unsigned int ref, arg;
      LmnSAtom atom;

      ref = binstr_get_ref_num(bs->v, pos);
      pos += ATOM_REF_SIZE;
      arg =  binstr_get_arg_ref(bs->v, pos);
      pos += ATOM_REF_ARG_SIZE;

      atom = LMN_SATOM(log[ref]);
      if (from_atom) {
        lmn_newlink_in_symbols(atom, arg, from_atom, from_arg);
      }
    }
    break;
  case TAG_MEM_REF:
    {
      unsigned int ref;
      LmnSAtom in, out;
      LmnMembrane *ref_mem;

      ref = binstr_get_ref_num(bs->v, pos);
      pos += MEM_REF_SIZE;
      ref_mem = (LmnMembrane *)log[ref];

      if (from_atom == NULL) {
        pos = binstr_decode_mol(bs, pos, log, nvisit, ref_mem, NULL, from_arg);
      } else {
        in = lmn_mem_newatom(ref_mem, LMN_IN_PROXY_FUNCTOR);
        out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
        lmn_newlink_in_symbols(in, 0, out, 0);
        lmn_newlink_in_symbols(out, 1, from_atom, from_arg);
        pos = binstr_decode_mol(bs, pos, log, nvisit, ref_mem, in, 1);
      }
    }
    break;
  case TAG_INT_DATA:
    {
      long n;
      n = binstr_get_int(bs->v, pos);
      pos += BS_INT_SIZE;
      LMN_SATOM_SET_LINK(from_atom, from_arg, n);
      LMN_SATOM_SET_ATTR(from_atom, from_arg, LMN_INT_ATTR);
      lmn_mem_push_atom(mem, n, LMN_INT_ATTR);
    }
    break;
  case TAG_DBL_DATA:
    {
      double *n = LMN_MALLOC(double);

      *n = binstr_get_dbl(bs->v, pos);
      pos += BS_DBL_SIZE;
      LMN_SATOM_SET_LINK(from_atom, from_arg, n);
      LMN_SATOM_SET_ATTR(from_atom, from_arg, LMN_DBL_ATTR);
      lmn_mem_push_atom(mem, (LmnWord)n, LMN_DBL_ATTR);
    }
    break;
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
static int binstr_decode_atom(LmnBinStr bs,
                              int pos,
                              void **log,
                              int *nvisit,
                              LmnMembrane *mem,
                              LmnSAtom from_atom,
                              int from_arg)
{
  LmnFunctor f;
  int arity, i;
  LmnSAtom atom;

  f = binstr_get_functor(bs->v, pos); /* functorを持ってくる */
  pos += FUNCTOR_SIZE;
  arity = LMN_FUNCTOR_ARITY(f);       /* functorから, リンク数が分かる */


  atom = lmn_mem_newatom(mem, f);     /* アトムを生成する */
  log[(*nvisit)++] = atom;            /* ポインタを記録(*nvisitは初期値1) */

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

LmnBinStr lmn_mem_to_binstr(LmnMembrane *mem)
{
  LmnBinStr ret;
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__MENC_DUMP);
  }
#endif
  ret = lmn_mem_to_binstr_sub(mem, 1024);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, lmn_binstr_space(ret));
    profile_finish_timer(PROFILE_TIME__MENC_DUMP);
  }
#endif
  return ret;
}

/* 膜のdumpを計算する. dump_root_memとかから名称変更したみたい */
static LmnBinStr lmn_mem_to_binstr_sub(LmnMembrane *mem, unsigned long tbl_size)
{
  LmnBinStr ret_bs;
  BinStr bs;
  struct BinStrPtr bsp;
  struct VisitLog visitlog;

  bs = binstr_make();
  bsptr_init_direct(&bsp, bs);
  visitlog_init_with_size(&visitlog, tbl_size);

  dump_root_mem(mem, &bsp, &visitlog);

  /* 最後に、ポインタの位置を修正する */
  bs->cur = bsp.pos;
  ret_bs = binstr_to_lmn_binstr(bs);

  binstr_free(bs);
  bsptr_destroy(&bsp);
  visitlog_destroy(&visitlog);

  return ret_bs;
}

static void dump_root_mem(LmnMembrane *mem, BinStrPtr bsp, VisitLog visitlog)
{
  dump_mem_atoms(mem, bsp, visitlog); /* 1. アトムから */
  dump_mems(mem, bsp, visitlog);      /* 2. 子膜から */
  write_rulesets(mem, bsp);           /* 3. 最後にルール */
}

/* 膜memに存在する全てのアトムをファンクタIDの降順の列で求め, 求めた列をdump_molsする */
static void dump_mem_atoms(LmnMembrane *mem,
                           BinStrPtr bsp,
                           VisitLog visited)
{
  Vector *atoms;

  atoms = mem_atoms(mem);

  dump_mols(atoms, bsp, visited);
  vec_free(atoms);
}

/* アトム列atomsから, visitedに未登録のアトムに対し, write_molを行う
 * つまり, mhash同様に, 各アトムを起点とした分子単位でエンコードを行っている */
static void dump_mols(Vector *atoms,
                      BinStrPtr bsp,
                      VisitLog visited)
{
  int i, natom;

  /* atoms中の未訪問のアトムを起点とする分子を、それぞれ試みる */
  natom = vec_num(atoms);
  for (i = 0; i < natom; i++) {
    LmnSAtom atom = LMN_SATOM(vec_get(atoms, i));

    if (visitlog_get_atom(visited, atom, NULL)) {
      continue;
    } else {
      write_mol((LmnAtom)atom, LMN_ATTR_MAKE_LINK(0), -1, bsp, visited, FALSE);
    }
  }
}

/* 膜中心の計算単位. 未訪問の子膜Xに対して, 子膜の分子を書き込み, dump_memsへ再起する
 * 兄弟膜は子膜内のプロセスを全て書き込んでから訪問される */
static void dump_mems(LmnMembrane *mem,
                      BinStrPtr bsp,
                      VisitLog visited)
{
  LmnMembrane *m;

  for (m = mem->child_head; m; m = m->next) {
    if (!visitlog_get_mem(visited, m, NULL)) {
      write_mem(m, 0, -1, -1, bsp, visited, FALSE);
    }
  }
}

/*----------------------------------------------------------------------
 * Membrane Isomorphism
 */

static int mem_eq_enc_mols(LmnBinStr bs, int *i_bs, LmnMembrane *mem, void **ref_log, int *i_ref, VisitLog visitlog);
static inline BOOL mem_eq_enc_mol(LmnBinStr bs,
                                  int *i_bs,
                                  LmnMembrane *mem,
                                  LmnAtom atom,
                                  LmnLinkAttr attr,
                                  void **ref_log,
                                  int *i_ref,
                                  VisitLog visitlog);
static BOOL mem_eq_enc_atom(LmnBinStr bs,
                            int *i_bs,
                            LmnMembrane *mem,
                            LmnAtom atom,
                            LmnLinkAttr attr,
                            void **ref_log,
                            int *i_ref,
                            VisitLog visitlog);
static inline BOOL mem_eq_enc_rulesets(LmnBinStr bs, int *i_bs, LmnMembrane *mem);
static inline BOOL mem_eq_enc_ruleset(LmnBinStr bs, int *i_bs, LmnRuleSet rs);
static inline BOOL mem_eq_enc_rulesets_uniq(LmnBinStr bs, int *i_bs, LmnMembrane *mem);
static inline BOOL mem_eq_enc_atom_ref(LmnBinStr bs,
                                       int *i_bs,
                                       LmnAtom atom,
                                       LmnLinkAttr attr,
                                       void **ref_log,
                                       int *i_ref,
                                       VisitLog visitlog);
static inline BOOL mem_eq_enc_mem_ref(LmnBinStr bs,
                                      int *i_bs,
                                      LmnAtom atom,
                                      LmnLinkAttr attr,
                                      void **ref_log,
                                      int *i_ref,
                                      VisitLog visitlog);
static inline BOOL mem_eq_enc_mem(LmnBinStr bs,
                                  int *i_bs,
                                  LmnMembrane *mem,
                                  void **ref_log,
                                  int *i_ref,
                                  VisitLog visitlog);
static inline BOOL mem_eq_enc_traced_mem(BOOL is_named,
                                         LmnBinStr bs,
                                         int *i_bs,
                                         LmnMembrane *mem,
                                         LmnAtom atom,
                                         LmnLinkAttr attr,
                                         void **ref_log,
                                         int *i_ref,
                                         VisitLog visitlog);
static inline BOOL mem_eq_enc_escape_mem(LmnBinStr bs,
                                         int *i_bs,
                                         LmnMembrane *mem,
                                         LmnAtom atom,
                                         LmnLinkAttr attr,
                                         void **ref_log,
                                         int *i_ref,
                                         VisitLog visitlog);
static long process_num(LmnMembrane *mem);
static BOOL lmn_mem_equals_enc_sub(LmnBinStr bs, LmnMembrane *mem, unsigned long tbl_size);

/* 膜のダンプ or エンコードと、膜の同型性判定を行う */
BOOL lmn_mem_equals_enc(LmnBinStr bs, LmnMembrane *mem)
{
  LmnBinStr target;
  BOOL ret;

  target = is_comp_z(bs) ? lmn_bscomp_z_decode(bs)
                         : bs;
  ret    = lmn_mem_equals_enc_sub(target, mem, 1024);
  if (is_comp_z(bs)) {
    lmn_binstr_free(target);
  }
  return ret;
}

/* 膜のダンプ or エンコードと、膜の同型性判定を行う */
BOOL lmn_mem_equals_enc_delta(LmnBinStr bs, struct MemDeltaRoot *d)
{
  BOOL t;

  dmem_root_commit(d);
  t = lmn_mem_equals_enc_sub(bs, dmem_root_get_root_mem(d), dmem_root_get_next_id(d));
  dmem_root_revert(d);

  return t;
}

static BOOL lmn_mem_equals_enc_sub(LmnBinStr bs, LmnMembrane *mem, unsigned long tbl_size)
{
  struct VisitLog visitlog;
  void **ref_log;
  int i_bs, i_ref;
  BOOL t;

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__STATE_COMPARE_MEQ);
  }
#endif

  /* **とりあえず**これなら参照の数以上のサイズになる */
  ref_log = LMN_NALLOC(void*, round2up(binstr_byte_size(bs)*TAG_IN_BYTE));
  visitlog_init_with_size(&visitlog, tbl_size);
  i_bs = 0;
  i_ref = VISITLOG_INIT_N;

/*   lmn_binstr_dump(bs); */

  t = mem_eq_enc_mols(bs, &i_bs, mem, ref_log, &i_ref, &visitlog)
    /* memに未訪問したプロセスあるなら FALSE */
    && visitlog_element_num(&visitlog) == process_num(mem);
  visitlog_destroy(&visitlog);
  LMN_FREE(ref_log);

#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_finish_timer(PROFILE_TIME__STATE_COMPARE_MEQ);
  }
#endif

  return t;
}

static int mem_eq_enc_mols(LmnBinStr  bs,
                          int         *i_bs,
                          LmnMembrane *mem,
                          void        **ref_log,
                          int         *i_ref,
                          VisitLog    visitlog)
{
  unsigned int tag;
  BOOL ok;
  BOOL rule_flag = FALSE;   /* ruleset有無の確認用フラグ */
  int tmp_i_bs, tmp_i_ref;

  while (*i_bs < bs->len) {
    tag =  BS_GET(bs->v, *i_bs);
    (*i_bs)++;

    switch (tag) {
    case TAG_ATOM_START:
      {
        LmnFunctor f;
        LmnSAtom atom;
        AtomListEntry *ent;

        f = binstr_get_functor(bs->v, *i_bs);

        ok = FALSE;

        ent = lmn_mem_get_atomlist(mem, f);
        if (!ent) return FALSE;
        EACH_ATOM(atom, ent, ({
          if (!visitlog_get_atom(visitlog, atom, NULL)) {
            tmp_i_bs = *i_bs;
            tmp_i_ref = *i_ref;

            visitlog_set_checkpoint(visitlog);
            if (mem_eq_enc_atom(bs, &tmp_i_bs, mem,
                                LMN_ATOM(atom), LMN_ATTR_MAKE_LINK(0),
                                ref_log, &tmp_i_ref, visitlog)) {
              *i_bs = tmp_i_bs;
              *i_ref = tmp_i_ref;
              visitlog_commit_checkpoint(visitlog);
              ok = TRUE;
              break;
            } else {
              visitlog_revert_checkpoint(visitlog);
            }
          }
        }));

        if (!ok) {
          return FALSE;
        }
        break;
      }
    case TAG_NAMED_MEM_START:
    case TAG_MEM_START:
      {
        LmnMembrane *m;
        lmn_interned_str mem_name = ANONYMOUS;

        if (tag == TAG_NAMED_MEM_START) {
          mem_name = binstr_get_mem_name(bs->v, *i_bs);
          (*i_bs) += BS_MEM_NAME_SIZE;
        }

        ok = FALSE;
        for (m = mem->child_head; m; m = m->next) {
          if (LMN_MEM_NAME_ID(m) == mem_name &&
              !visitlog_get_mem(visitlog, m, NULL)) {
            tmp_i_bs = *i_bs;
            tmp_i_ref = *i_ref;

            visitlog_set_checkpoint(visitlog);
            if (mem_eq_enc_mem(bs, &tmp_i_bs, m, ref_log, &tmp_i_ref, visitlog)) {
              *i_bs = tmp_i_bs;
              *i_ref = tmp_i_ref;
              visitlog_commit_checkpoint(visitlog);
              ok = TRUE;
              break;
            } else {
              visitlog_revert_checkpoint(visitlog);
            }
          }
        }

        if (!ok) return FALSE;
        break;
      }
    case TAG_MEM_END:
      {
        if (!rule_flag && lmn_mem_ruleset_num(mem) != 0) return FALSE;
        return TRUE;
      }
    case TAG_RULESET1:
      {
        if (lmn_mem_ruleset_num(mem) != 1) return FALSE;
        if (!mem_eq_enc_ruleset(bs, i_bs, lmn_mem_get_ruleset(mem, 0))) return FALSE;
        rule_flag = TRUE;
        break;
      }
    case TAG_RULESET:
      {
        if (!mem_eq_enc_rulesets(bs, i_bs, mem)) return FALSE;
        rule_flag = TRUE;
        break;
      }
    case TAG_RULESET_UNIQ:
      {
        if (!mem_eq_enc_rulesets_uniq(bs, i_bs, mem)) return FALSE;
        rule_flag = TRUE;
        break;
      }
    default:
      lmn_fatal("unexpected");
      break;
    }
  }
  return TRUE;
}

static BOOL mem_eq_enc_atom(LmnBinStr bs,
                            int *i_bs,
                            LmnMembrane *mem,
                            LmnAtom atom,
                            LmnLinkAttr attr,
                            void **ref_log,
                            int *i_ref,
                            VisitLog visitlog)
{
  LmnFunctor f;
  int arity;
  int i;
  if (LMN_ATTR_IS_DATA(attr)) return FALSE;

  f = binstr_get_functor(bs->v, *i_bs);
  (*i_bs) += FUNCTOR_SIZE;

  if (f != LMN_SATOM_GET_FUNCTOR(atom)) return FALSE;

  if (!visitlog_put_atom(visitlog, LMN_SATOM(atom))) return FALSE;
  ref_log[*i_ref] = LMN_SATOM(atom);
  (*i_ref)++;
  arity = LMN_FUNCTOR_ARITY(f);

  for (i = 0; i < arity; i++) {
    if (!mem_eq_enc_mol(bs,
                        i_bs,
                        mem,
                        LMN_SATOM_GET_LINK(atom, i),
                        LMN_SATOM_GET_ATTR(atom, i),
                        ref_log,
                        i_ref,
                        visitlog)) return FALSE;
  }

  return TRUE;
}

static inline BOOL mem_eq_enc_traced_mem(BOOL is_named,
                                         LmnBinStr bs,
                                         int *i_bs,
                                         LmnMembrane *mem,
                                         LmnAtom atom,
                                         LmnLinkAttr attr,
                                         void **ref_log,
                                         int *i_ref,
                                         VisitLog visitlog)
{
  LmnAtom in;

  if (LMN_ATTR_IS_DATA(attr) ||
      LMN_SATOM_GET_FUNCTOR(atom) != LMN_OUT_PROXY_FUNCTOR) {
    return FALSE;
  }

  in = LMN_SATOM_GET_LINK(atom, 0);

  if (is_named) {
    const lmn_interned_str mem_name = binstr_get_mem_name(bs->v, *i_bs);
    (*i_bs) += BS_MEM_NAME_SIZE;
    if (mem_name != LMN_MEM_NAME_ID(LMN_PROXY_GET_MEM(in))) return FALSE;
  }

  visitlog_put_mem(visitlog, LMN_PROXY_GET_MEM(in));
  ref_log[*i_ref] = LMN_PROXY_GET_MEM(in);
  (*i_ref)++;

  if (mem_eq_enc_mol(bs,
                     i_bs,
                     LMN_PROXY_GET_MEM(in),
                     LMN_SATOM_GET_LINK(in, 1),
                     LMN_SATOM_GET_ATTR(in, 1),
                     ref_log,
                     i_ref,
                     visitlog) &&
      mem_eq_enc_mols(bs, i_bs, LMN_PROXY_GET_MEM(in), ref_log, i_ref, visitlog)) {
    return TRUE;
  } else {
    return FALSE;
  }
}

static inline BOOL mem_eq_enc_mem(LmnBinStr bs,
                                  int *i_bs,
                                  LmnMembrane *mem,
                                  void **ref_log,
                                  int *i_ref,
                                  VisitLog visitlog)
{
  if (!visitlog_put_mem(visitlog, mem)) return FALSE;
  ref_log[*i_ref] = mem;
  (*i_ref)++;

  if (mem_eq_enc_mols(bs, i_bs, mem, ref_log, i_ref, visitlog)) {
    return TRUE;
  } else {
    return FALSE;
  }
}

static inline BOOL mem_eq_enc_mol(LmnBinStr bs,
                                  int *i_bs,
                                  LmnMembrane *mem,
                                  LmnAtom atom,
                                  LmnLinkAttr attr,
                                  void **ref_log,
                                  int *i_ref,
                                  VisitLog visitlog)
{
  int tag;

  tag =  BS_GET(bs->v, *i_bs);
  (*i_bs)++;
  switch (tag) {
  case TAG_ATOM_START:
    return mem_eq_enc_atom(bs, i_bs, mem, atom, attr, ref_log, i_ref, visitlog);
  case TAG_NAMED_MEM_START:
    return mem_eq_enc_traced_mem(TRUE,
                                 bs,
                                 i_bs,
                                 mem,
                                 atom,
                                 attr,
                                 ref_log,
                                 i_ref,
                                 visitlog);
  case TAG_MEM_START:
    return mem_eq_enc_traced_mem(FALSE,
                                 bs,
                                 i_bs,
                                 mem,
                                 atom,
                                 attr,
                                 ref_log,
                                 i_ref,
                                 visitlog);
  case TAG_ATOM_REF:
    return mem_eq_enc_atom_ref(bs, i_bs, atom, attr, ref_log, i_ref, visitlog);
  case TAG_MEM_REF:
    return mem_eq_enc_mem_ref(bs, i_bs, atom, attr, ref_log, i_ref, visitlog);
  case TAG_ESCAPE_MEM:
    return mem_eq_enc_escape_mem(bs, i_bs, mem, atom, attr, ref_log, i_ref, visitlog);
  case TAG_FROM:
    /* 何もしなくて大丈夫。だよね？ */
    return TRUE;
  case TAG_INT_DATA:
    {
      long n = binstr_get_int(bs->v, *i_bs);
      (*i_bs) += BS_INT_SIZE;

      if (attr == LMN_INT_ATTR && n == atom) {
        visitlog_put_data(visitlog);
        return TRUE;
      } else {
        return FALSE;
      }
    }
  case TAG_DBL_DATA:
    {
      double n = binstr_get_dbl(bs->v, *i_bs);
      (*i_bs) += BS_DBL_SIZE;

      if (attr == LMN_DBL_ATTR && n == *(double*)atom) {
        visitlog_put_data(visitlog);
        return TRUE;
      } else {
        return FALSE;
      }
    }
  default:
    lmn_fatal("not implemented");
    break;
  }
}

static inline BOOL mem_eq_enc_rulesets(LmnBinStr bs, int *i_bs, LmnMembrane *mem)
{
  long n;
  int i;

  n = binstr_get_ruleset_num(bs->v, *i_bs);
  if (n != lmn_mem_ruleset_num(mem)) return FALSE;
  (*i_bs) += BS_RULESET_NUM_SIZE;

  for (i = 0; i < n; i++) {
    if (!mem_eq_enc_ruleset(bs, i_bs, lmn_mem_get_ruleset(mem, i))) return FALSE;
  }
  return TRUE;
}

static inline BOOL mem_eq_enc_ruleset(LmnBinStr bs, int *i_bs, LmnRuleSet rs)
{
  long id;

  id = binstr_get_ruleset(bs->v, *i_bs);
  if (id != lmn_ruleset_get_id(rs)) return FALSE;
  (*i_bs) += BS_RULESET_SIZE;
  return TRUE;
}

static inline BOOL mem_eq_enc_rulesets_uniq(LmnBinStr bs, int *i_bs, LmnMembrane *mem)
{
  int rs_num;
  Vector *rulesets;
  BOOL result;

  rs_num = binstr_get_ruleset_num(bs->v, *i_bs);
  if (rs_num != lmn_mem_ruleset_num(mem)) return FALSE;
  (*i_bs) += BS_RULESET_NUM_SIZE;

  /* TODO: on-the-flyにできるはず */
  rulesets = vec_make(rs_num + 1);
  binstr_decode_rulesets(bs, i_bs, rulesets, rs_num);

  result = lmn_rulesets_equals(rulesets, lmn_mem_get_rulesets(mem));

  lmn_mem_rulesets_destroy(rulesets);
  LMN_FREE(rulesets);

  return result;
}

static inline BOOL mem_eq_enc_atom_ref(LmnBinStr bs,
                                       int *i_bs,
                                       LmnAtom atom,
                                       LmnLinkAttr attr,
                                       void **ref_log,
                                       int *i_ref,
                                       VisitLog visitlog)
{
  unsigned int ref, arg;

  if (LMN_ATTR_IS_DATA(attr)) return FALSE;

  ref = binstr_get_ref_num(bs->v, *i_bs);
  (*i_bs) += ATOM_REF_SIZE;
  arg =  binstr_get_arg_ref(bs->v, *i_bs);
  (*i_bs) += ATOM_REF_ARG_SIZE;

  return ref_log[ref] == (void *)atom && LMN_ATTR_GET_VALUE(attr) == arg;
}

static inline BOOL mem_eq_enc_mem_ref(LmnBinStr bs,
                                      int *i_bs,
                                      LmnAtom atom,
                                      LmnLinkAttr attr,
                                      void **ref_log,
                                      int *i_ref,
                                      VisitLog visitlog)
{
  unsigned int ref;
  LmnAtom in;

  if (LMN_ATTR_IS_DATA(attr) ||
      LMN_SATOM_GET_FUNCTOR(atom) != LMN_OUT_PROXY_FUNCTOR) return FALSE;

  ref = binstr_get_ref_num(bs->v, *i_bs);
  (*i_bs) += MEM_REF_SIZE;
  in = LMN_SATOM_GET_LINK(atom, 0);

  return
    ref_log[ref] == LMN_PROXY_GET_MEM(in) &&
    mem_eq_enc_mol(bs,
                   i_bs,
                   LMN_PROXY_GET_MEM(in),
                   LMN_SATOM_GET_LINK(in, 1),
                   LMN_SATOM_GET_ATTR(in, 1),
                   ref_log,
                   i_ref,
                   visitlog);
}

static inline BOOL mem_eq_enc_escape_mem(LmnBinStr bs,
                                         int *i_bs,
                                         LmnMembrane *mem,
                                         LmnAtom atom,
                                         LmnLinkAttr attr,
                                         void **ref_log,
                                         int *i_ref,
                                         VisitLog visitlog)
{
  LmnAtom out;

  if (LMN_ATTR_IS_DATA(attr) ||
      LMN_SATOM_GET_FUNCTOR(atom) != LMN_IN_PROXY_FUNCTOR) return FALSE;

  out = LMN_SATOM_GET_LINK(atom, 0);

  return mem_eq_enc_mol(bs,
                        i_bs,
                        lmn_mem_parent(mem),
                        LMN_SATOM_GET_LINK(out, 1),
                        LMN_SATOM_GET_ATTR(out, 1),
                        ref_log,
                        i_ref,
                        visitlog);
}


/* mem以下にあるアトムと膜の数を返す */
static long process_num(LmnMembrane *mem)
{
  LmnMembrane *m;
  long n = 0;

  if (mem == NULL) return 0;

  n += lmn_mem_atom_num(mem) + lmn_mem_child_mem_num(mem);

  for (m = mem->child_head; m; m = m->next) {
    n += process_num(m);
  }
  return n;
}

