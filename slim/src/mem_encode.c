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
 *   tag: 0000
 *   functor:  (2 Byte)
 *   arguments
 *
 * membrane
 *   tag: 0001
 *   elements: (any length)
 *   end tag:   0010
 * named membrane
 *   tag: 0011
 *   name: 4 Byte
 *   elements
 *   end tag:   0010
 * atom ref
 *   tag: 0100
 *   ref id : 2 byte
 *   arg num: 1 byte
 * mem ref
 *   tag: 0101 
 * escape from membrane
 *   tag: 0110
 * from of traversal
 *   tag: 0111
 * rule sets(only one ruleset):
 *   tag: 0100
 *   ruleset id
 * rule sets:
 *   tag: 0101
 *   ruleset num: 2 byte
 *   ruleset ids
 * int atom:
 *   tag: 1100
 * double atom:
 *   tag: 1101
*/

#include "mem_encode.h"
#include "visitlog.h"
#include "error.h"
#include "functor.h"
#include "atom.h"
#include "st.h"
#include "util.h"
#include "st.h"
#ifdef PROFILE
#include "runtime_status.h"
#endif

#define TAG_BIT_SIZE 4
#define TAG_DATA_TYPE_BIT 2
#define TAG_IN_BYTE 2

/* Tags */
#define TAG_ATOM_START       0x0
#define TAG_MEM_START        0x1
#define TAG_MEM_END          0x2
#define TAG_NAMED_MEM_START  0x3
#define TAG_ATOM_REF         0x4
#define TAG_MEM_REF          0x5
#define TAG_ESCAPE_MEM       0x6
#define TAG_FROM             0x7
#define TAG_RULESET1         0x8
#define TAG_RULESET          0x9
#define TAG_INT_DATA         0xc
#define TAG_DBL_DATA         0xd


#define ATOM_REF_SIZE 4
#define ATOM_REF_ARG_SIZE 2
#define MEM_REF_SIZE 4
#define FUNCTOR_SIZE (sizeof(LmnFunctor) * 2)
#define BS_INT_SIZE (SIZEOF_LONG * 2)
#define BS_MEM_NAME_SIZE (sizeof(lmn_interned_str) * 2)
#define BS_RULESET_SIZE 4
#define BS_RULESET_NUM_SIZE 4
#define BS_DBL_SIZE (sizeof(double) * 2)

typedef struct BinStr *BinStr;
typedef struct BinStrPtr *BinStrPtr;


uint16_t functor_priority[FUNCTOR_MAX+1];

/*----------------------------------------------------------------------
 * Prototypes
 */

/* Dump Membrane to Binary String */

static BinStr dump_root_mem(LmnMembrane *mem);
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

/* 最終的なエンコード結果を表すバイナリストリング */
struct LmnBinStr {
  BYTE *v;
  unsigned int len;
};

inline void lmn_binstr_free(struct LmnBinStr *bs)
{
#ifdef PROFILE
  status_binstr_free(bs);
#endif

  LMN_FREE(bs->v);
  LMN_FREE(bs);

}

unsigned long lmn_binstr_space(struct LmnBinStr *bs)
{
  return sizeof(struct LmnBinStr) + sizeof(BYTE) * bs->len / TAG_IN_BYTE;
}

/* エンコード処理に用いるバイナリストリング */
struct BinStr {
  BYTE *v;
  /* バッファのサイズ（4ビット単位） */
  int size;
  /* 書き込み位置（4ビット単位） */
  int cur;
  /* バッファの位置を指し示す、BinStrPtrのベクタ */
  Vector *ptrs;
  /* 作業用 */
  Vector *ptrs2;
};

static inline void bsptr_invalidate(BinStrPtr p);
static void binstr_invalidate_ptrs(struct BinStr *p, int start);
static inline void bsptr_destroy(struct BinStrPtr *p);
static inline int bsptr_pos(struct BinStrPtr *p);

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
#define FNV_PRIME 16777619UL
#define FNV_BASIS 2166136261UL
#elif SIZEOF_LONG == 8
#define FNV_PRIME  1099511628211UL
#define FNV_BASIS 14695981039346656037UL
#endif

/* バイナリストリングのハッシュ値を返す */
unsigned long binstr_hash(const LmnBinStr a)
{
   unsigned long hval = FNV_BASIS;
  int i = (a->len+1) / TAG_IN_BYTE;

  /*
   * FNV-1a hash each octet in the buffer
   */
  while (--i >= 0) {
    /* xor the bottom with the current octet */
    hval ^= (unsigned int) a->v[i];

    /* multiply by the FNV magic prime mod 2^32 or 2^64 */
    hval *= FNV_PRIME;
  }
  return hval;
}

#define BS_SET(a, pos, v)                                                \
  (((pos) & 1) ?                                                        \
   ((a)[(pos)>>1] = ((a)[(pos)>>1] & 0x0f) | ((v) << TAG_BIT_SIZE)) :   \
   ((a)[(pos)>>1] = (v&0x0f) | ((a)[(pos)>>1] & 0xf0)))
#define BS_GET(a, pos)                                                \
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
  return (BS_GET(bs, pos+1)) | (BS_GET(bs, pos)<<4);
}

static inline uint16_t binstr_get_uint16(BYTE *bs, int pos)
{
  return
    (uint16_t)((binstr_get_byte(bs, pos+2)) | ((binstr_get_byte(bs, pos))<<8));
}

static inline uint32_t binstr_get_uint32(BYTE *bs, int pos)
{
  return (uint32_t)(binstr_get_uint16(bs, pos+4) | (binstr_get_uint16(bs, pos)<<16));
}

static inline LmnFunctor binstr_get_functor(BYTE *bs, int pos)
{
  if (sizeof(LmnFunctor) == 2) {
    long f = binstr_get_uint16(bs, pos);
    return (LmnFunctor)(FUNCTOR_MAX - f);
  } else {
    lmn_fatal("unexpected");
  }
}

static inline unsigned int binstr_get_ref_num(BYTE *bs, int pos)
{
  if (ATOM_REF_SIZE == 4) {
    return binstr_get_uint16(bs, pos);
  } else {
    lmn_fatal("unexpected");
  }
}

static inline unsigned int binstr_get_arg_ref(BYTE *bs, int pos)
{
  if (ATOM_REF_ARG_SIZE == 2) {
    return binstr_get_byte(bs, pos);
  } else {
    lmn_fatal("unexpected");
  }
}

static inline uint32_t binstr_get_uin32_t(BYTE *bs, int pos)
{
  return (uint32_t)binstr_get_uint16(bs, pos+4) | (uint32_t)(binstr_get_uint16(bs, pos)<<16);
}

static inline uint64_t binstr_get_uint64_t(BYTE *bs, int pos)
{
  return (uint64_t)binstr_get_uint32(bs, pos+8) | (uint64_t)binstr_get_uint32(bs, pos)<<32;
}

static inline long binstr_get_int(BYTE *bs, int pos)
{
#if SIZEOF_LONG == 4
  return (long)binstr_get_uint32(bs, pos);
#elif SIZEOF_LONG == 8
  return (long)binstr_get_uint64_t(bs, pos);
#else
    #error "not supported"
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

static inline lmn_interned_str binstr_get_mem_name(BYTE *bs, int pos)
{
  if (BS_MEM_NAME_SIZE == 8) {
    return binstr_get_uint32(bs, pos);
  } else {
    lmn_fatal("implementation error");
  }
}

static inline long binstr_get_ruleset_num(BYTE *bs, int pos)
{
#if BS_RULESET_NUM_SIZE == 4
  return binstr_get_uint16(bs, pos);
#else
  #error unexpected
#endif
}

static inline long binstr_get_ruleset(BYTE *bs, int pos)
{
#if BS_RULESET_SIZE == 4
  return binstr_get_uint16(bs, pos);
#else
  #error unexpected
#endif
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
int binstr_comp(const LmnBinStr a, const LmnBinStr b)
{
  if (a->len != b->len) return a->len - b->len;
  else {
    return memcmp(a->v, b->v, (a->len+1) / TAG_IN_BYTE);
  }
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
  int size = (bs->cur+1)/2;
  ret_bs = LMN_MALLOC(struct LmnBinStr);
  ret_bs->v = LMN_NALLOC(BYTE, size);
  memcpy(ret_bs->v, bs->v, size);
  ret_bs->len = bs->cur;
  if (ret_bs->len & 1) {
    ret_bs->v[ret_bs->len >> 1] = ret_bs->v[ret_bs->len >> 1] & 0x0f;
  }

#ifdef PROFILE
  status_binstr_make(ret_bs);
#endif
  return ret_bs;
}

static void binstr_dump(BYTE *bs, int len)
{
  int pos;

  pos = 0;
  while (pos < len) {
    unsigned int tag = BS_GET(bs, pos);
    pos++;

    switch (tag) {
    case TAG_ATOM_START:
      {
        LmnFunctor f = binstr_get_functor(bs, pos);
        pos += FUNCTOR_SIZE;
        printf("%s(", lmn_id_to_name(LMN_FUNCTOR_NAME_ID(f)));
      }
      break;
    case TAG_MEM_START:
      {
        printf("{");
      }
      break;
    case TAG_MEM_END:
      {
        printf("}");
      }
      break;
    case TAG_NAMED_MEM_START:
      {
        lmn_interned_str name;
        name = binstr_get_mem_name(bs, pos);
        pos += BS_MEM_NAME_SIZE;
        printf("%s{", lmn_id_to_name(name));
      }
      break;
    case TAG_ATOM_REF:
      {
        unsigned int ref, arg;
        ref = binstr_get_ref_num(bs, pos);
        pos += ATOM_REF_SIZE;
        arg =  binstr_get_arg_ref(bs, pos);
        pos += ATOM_REF_ARG_SIZE;
        printf("$%d_%d", ref, arg);
      }
      break;
    case TAG_MEM_REF:
      {
        unsigned int ref;
        ref = binstr_get_ref_num(bs, pos);
        pos += MEM_REF_SIZE;
        printf("#%d", ref);
      }
      break;
    case TAG_ESCAPE_MEM:
      {
        printf("!");
      }
      break;
    case TAG_FROM:
      {
        printf("_");
      }
      break;
    case TAG_INT_DATA:
      {
        long n;

        n = binstr_get_int(bs, pos);
        pos += BS_INT_SIZE;
        printf("%ld", n);
      }
      break;
    case TAG_DBL_DATA:
      {
        double n;

        n = binstr_get_dbl(bs, pos);
        pos += BS_DBL_SIZE;
        printf("%lf", n);
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
  struct BinStr *binstr;
  int pos; /* bit */
  BOOL valid;
  BOOL direct;
};

static inline void bsptr_init(struct BinStrPtr *p, struct BinStr *bs)
{
  p->binstr = bs;
  p->pos = 0;
  p->valid = TRUE;
  binstr_add_ptr(bs, p);
  p->direct = FALSE;
}
                                    
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
  default:
    lmn_fatal("not implemented");
    return 0;
  }
}

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

/*----------------------------------------------------------------------
 * Membrane Encode
 */

/*
 * 膜を一意なバイナリストリングにエンコードする */

/* prototypes */

static BinStr encode_root_mem(LmnMembrane *mem);
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

/* memを一意なバイナリストリングに変換する */
LmnBinStr lmn_mem_encode(LmnMembrane *mem)
{
  LmnBinStr ret_bs;
  BinStr bs;
  
#ifdef PROFILE
  status_start_mem_encode_calc();
#endif
  
  bs = encode_root_mem(mem);

#ifdef PROFILE
  status_finish_mem_encode_calc();
#endif

  ret_bs = binstr_to_lmn_binstr(bs);
  /* lmn_binstr_dump(ret_bs); */
  binstr_free(bs);
  return ret_bs;
}

static BinStr encode_root_mem(LmnMembrane *mem)
{
  BinStr bs = binstr_make();
  struct BinStrPtr bsp;
  struct VisitLog visited;

  bsptr_init(&bsp, bs);
  visitlog_init(&visited);
  

  write_mem_atoms(mem, &bsp, &visited);
  write_mems(mem, &bsp, &visited);
  write_rulesets(mem, &bsp);

  /* 最後に、ポインタの位置を修正する */
  bs->cur = bsp.pos;
  bsptr_destroy(&bsp);
  visitlog_destroy(&visited);
  return bs;
}

static void write_mem(LmnMembrane *mem,
                      LmnAtom from_atom,
                      LmnLinkAttr attr,
                      int from,
                      BinStrPtr bsp,
                      VisitLog visited,
                      BOOL is_id)
{
  LmnWord n_visited;
  
  if (!bsptr_valid(bsp)) return;

  if (visitlog_get_mem(visited, mem, &n_visited)) {
    bsptr_push_visited_mem(bsp, n_visited);

    if (from_atom) {
      /* 引き続きアトムをたどる */
      write_mol(from_atom, attr, from, bsp, visited, is_id);
    }
    return;
  }

  visitlog_put_mem(visited, mem);
  
  bsptr_push_start_mem(bsp, LMN_MEM_NAME_ID(mem));

  if (!bsptr_valid(bsp)) return;

  if (from_atom != 0) {
    /* 引き続きアトムをたどる */
    write_mol(from_atom, attr, from, bsp, visited, is_id);
  }

  if (is_id) {
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

  if (LMN_ATTR_IS_DATA(attr)) {
    bsptr_push_data_atom(bsp, atom, attr);
    return;
  }

  f = LMN_SATOM_GET_FUNCTOR(atom);
  
  if (f == LMN_OUT_PROXY_FUNCTOR) {
    LmnSAtom in = LMN_SATOM(LMN_SATOM_GET_LINK(atom, 0));
    LmnMembrane *in_mem = LMN_PROXY_GET_MEM(in);

    write_mem(in_mem,
              LMN_SATOM_GET_LINK(in, 1),
              LMN_SATOM_GET_ATTR(in, 1),
              LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(in, 1)),
              bsp,
              visited,
              is_id);
    return;
  }

  if (f == LMN_IN_PROXY_FUNCTOR) {
    LmnSAtom out = LMN_SATOM(LMN_SATOM_GET_LINK(atom, 0));

    bsptr_push_escape_mem(bsp);
    write_mol(LMN_SATOM_GET_LINK(out, 1),
              LMN_SATOM_GET_ATTR(out, 1),
              LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(out, 1)),
              bsp,
              visited,
              is_id);
    return;
  }

  if (visitlog_get_atom(visited, LMN_SATOM(atom), &n_visited)) {
    bsptr_push_visited_atom(bsp, n_visited, from);
    return;
  }

  bsptr_push_atom(bsp, LMN_SATOM(atom));
  if (!bsptr_valid(bsp)) return;

  visitlog_put_atom(visited, LMN_SATOM(atom));

  arity = LMN_FUNCTOR_GET_LINK_NUM(f);
  for (i_arg = 0; i_arg < arity; i_arg++) {
    if (i_arg == from) {
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
  int i, n;

  /* ルールセットがルールセットIDでソートされていることに基づいたコード */
  /* uniqルールがndで実装されると、ここは書き換えなければならない */
  n = lmn_mem_ruleset_num(mem);
  if (n == 0) return;
  bsptr_push_start_rulesets(bsp, n);

  for (i = 0; i < n; i++) {
    bsptr_push_ruleset(bsp, lmn_mem_get_ruleset(mem, i));
  }
}

/* 膜にあるアトムのファンクタを降順で返す */
static Vector *mem_functors(LmnMembrane *mem)
{
  Vector *v = vec_make(16);
  HashIterator iter;
  LmnFunctor f;
  
  for (iter = hashtbl_iterator(&mem->atomset);
       !hashtbliter_isend(&iter);
       hashtbliter_next(&iter)) {
    f = hashtbliter_entry(&iter)->key;
    if (!LMN_IS_PROXY_FUNCTOR(f)) {
      vec_push(v, f);
    }
  }

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


static Vector *mem_atoms(LmnMembrane *mem)
{
  Vector *functors;
  Vector *atoms;
  unsigned i;
  AtomListEntry *ent;
  
  functors = mem_functors(mem);
  atoms = vec_make(128);
  for (i = 0; i < vec_num(functors); i++) {
    ent = lmn_mem_get_atomlist(mem, (LmnFunctor)vec_get(functors, i));
    LmnSAtom a;
    EACH_ATOM(a, ent, {
        vec_push(atoms, (vec_data_t)a);
    });
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

/* エンコードされた膜をデコードし、構造を再構築する */
LmnMembrane *lmn_binstr_decode(const LmnBinStr bs)
{
  LmnMembrane *groot;
  void **log;
  int nvisit;
  
  log = LMN_NALLOC(void *, bs->len * TAG_IN_BYTE);

  groot = lmn_mem_make();
  lmn_mem_set_active(groot, TRUE);
  nvisit = VISITLOG_INIT_N;
  binstr_decode_cell(bs, 0, log, &nvisit, groot, NULL, 0);
  LMN_FREE(log);

  return groot;
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
      pos++;
      break;
    }
    else if (tag == TAG_RULESET1) {
      int rs_id;

      pos++;
      rs_id = binstr_get_ruleset(bs->v, pos);
      pos += BS_RULESET_SIZE;

      lmn_mem_add_ruleset(mem, lmn_ruleset_from_id(rs_id));
    }
    else if (tag == TAG_RULESET) {
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
    lmn_fatal("unexpected");
    break;
  }
  return pos;
}

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

  f = binstr_get_functor(bs->v, pos);
  pos += FUNCTOR_SIZE;
  arity = LMN_FUNCTOR_ARITY(f);

  atom = lmn_mem_newatom(mem, f);
  log[(*nvisit)++] = atom;

  for (i = 0; i < arity; i++) {
    /* zero clear */
    LMN_SATOM_SET_LINK(atom, i, 0);
  }
  
  for (i = 0; i < arity; i++) {
    unsigned int tag = BS_GET(bs->v, pos);

    switch (tag) {
    case TAG_FROM:
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
  LmnBinStr ret_bs;
  BinStr bs;
  
#ifdef PROFILE
  status_start_mem_dump_calc();
#endif

  bs = dump_root_mem(mem);

#ifdef PROFILE
  status_finish_mem_dump_calc();
#endif

  ret_bs = binstr_to_lmn_binstr(bs);

  binstr_free(bs);
  return ret_bs;
}

static BinStr dump_root_mem(LmnMembrane *mem)
{
  BinStr bs = binstr_make();
  struct BinStrPtr bsp;
  struct VisitLog visitlog;

  bsptr_init_direct(&bsp, bs);
  visitlog_init(&visitlog);
  
  dump_mem_atoms(mem, &bsp, &visitlog);
  dump_mems(mem, &bsp, &visitlog);
  write_rulesets(mem, &bsp);

  /* 最後に、ポインタの位置を修正する */
  bs->cur = bsp.pos;
  bsptr_destroy(&bsp);
  visitlog_destroy(&visitlog);
  return bs;
}

static void dump_mem_atoms(LmnMembrane *mem,
                           BinStrPtr bsp,
                           VisitLog visited)
{
  Vector *atoms;

  atoms = mem_atoms(mem);

  dump_mols(atoms, bsp, visited);
  vec_free(atoms);
}

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

/* 膜のダンプ or エンコードと、膜の同型性判定を行う */
BOOL lmn_mem_equals_enc(LmnBinStr bs, LmnMembrane *mem)
{
  struct VisitLog visitlog;
  void **ref_log;
  int i_bs, i_ref;
  BOOL t;

#ifdef PROFILE
  status_start_mem_enc_eq_calc();
#endif

  /* **とりあえず**これなら参照の数以上のサイズになる */
  ref_log = LMN_NALLOC(void*, round2up(binstr_byte_size(bs)*TAG_IN_BYTE)); 
  visitlog_init(&visitlog);
  i_bs = 0;
  i_ref = VISITLOG_INIT_N;

/*   lmn_binstr_dump(bs); */
  
  t = mem_eq_enc_mols(bs, &i_bs, mem, ref_log, &i_ref, &visitlog)
    /* memに未訪問したプロセスあるなら FALSE */
    && visitlog_element_num(&visitlog) == process_num(mem);
  visitlog_destroy(&visitlog);
  LMN_FREE(ref_log);

#ifdef PROFILE
  status_finish_mem_enc_eq_calc();
#endif
    
  return t;
}

static int mem_eq_enc_mols(LmnBinStr bs, int *i_bs, LmnMembrane *mem, void **ref_log, int *i_ref, VisitLog visitlog)
{
  unsigned int tag;
  BOOL ok;
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
        EACH_ATOM(atom, ent, {
            if (!visitlog_get_atom(visitlog, atom, NULL)) {
              tmp_i_bs = *i_bs;
              tmp_i_ref = *i_ref;
              
              visitlog_set_checkpoint(visitlog);
              if (mem_eq_enc_atom(bs, &tmp_i_bs, mem, LMN_ATOM(atom), LMN_ATTR_MAKE_LINK(0), ref_log, &tmp_i_ref, visitlog)) {
                *i_bs = tmp_i_bs; 
                *i_ref = tmp_i_ref;
                visitlog_commit_checkpoint(visitlog);
                ok = TRUE;
                break;
              } else {
                visitlog_revert_checkpoint(visitlog);
              }
            }
          });

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
      return TRUE;
    case TAG_RULESET1:
      {
        if (lmn_mem_ruleset_num(mem) != 1) return FALSE;
        if (!mem_eq_enc_ruleset(bs, i_bs, lmn_mem_get_ruleset(mem, 0))) return FALSE;
        break;
      }
    case TAG_RULESET:
      {
        if (!mem_eq_enc_rulesets(bs, i_bs, mem)) return FALSE;
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
