/**
 * mhash.c
 *
 * cf. devel/sample/khiroto/mhash.lmn
 *
 * 初期値によってはオーバーフローが生じてしまい、
 * 計算結果が計算順序に因ってしまうことがあるので
 * 注意すること
 *
 * TODO: データアトム（浮動小数点数、文字列）のハッシュ値
 */

#include "mhash.h"
#include "internal_hash.h"
#include "atom.h"
#include "membrane.h"
#include "functor.h"
#include "st.h"
/* #include "symbol.h" /\* TODO: for debug *\/ */
#include <limits.h>

#ifdef PROFILE
#include "runtime_status.h"
#endif

/* 膜が自分に至るリンクを持つ場合に，膜のハッシュ値の計算が無限ループに
   なるのを防ぐために現在計算中の膜のハッシュ値が必要になる場合は定数を
   返す
*/


/* #define C 31 /\* 深さを深くした場合、31は小さすぎるかも *\/ */
#define C 101 /* 深さを深くした場合、31は小さすぎるかも */
#define B 13
#define E 3
#define ADD_0 0
#define MUL_0 41
#define MEM_ADD_0 3412
#define MEM_MUL_0 3412
#define CALUCULATING_MEM 1
#define DEPTH 3

typedef unsigned long hash_t;

typedef struct Context {
  HashSet *done_mol; /* 計算済みの分子 */
  st_table_t done_mem; /* 計算済みの膜とハッシュ値 */
} *Context;

static Context init_context(void);
static void add_mem_hash(Context ctx, LmnMembrane *mem, hash_t hash);
static void add_done_mol(Context ctx, void *p);
static int is_done_mol(Context ctx, void *p);
static void free_context(Context ctx);
static int calculated_mem_hash(Context ctx, LmnMembrane *mem, hash_t *hash);

static hash_t membrane(LmnMembrane *mem, LmnMembrane *calc_mem, Context ctx);
static hash_t molecule(LmnAtomPtr atom,
                       LmnMembrane *calc_mem,
                       Context ctx);
static void do_molecule(LmnWord atom,
                        LmnLinkAttr attr,
                        LmnMembrane *calc_mem,
                        Context ctx,
                        int i_parent,
                        hash_t *sum,
                        hash_t *mul);
static hash_t unit(LmnWord atom,
                   LmnLinkAttr attr,
                   LmnMembrane *calc_mem,
                   Context ctx,
                   int depth);
static hash_t atomunit(LmnWord atom,
                       LmnLinkAttr attr,
                       LmnMembrane *calc_mem,
                       int depth,
                       Context ctx);
static hash_t memunit(LmnMembrane *mem,
                      LmnAtomPtr from_in_proxy,
                      LmnMembrane *calc_mem,
                      Context ctx,
                      int depth);
static hash_t mem_fromlink(LmnMembrane *mem,
                           LmnAtomPtr in_proxy,
                           LmnMembrane *calc_mem,
                           Context ctx);
static hash_t link(LmnWord atom,
                   LmnLinkAttr attr,
                   LmnMembrane *calc_mem,
                   Context ctx);
static hash_t atomlink(LmnWord atom, LmnLinkAttr attr);
static hash_t memlink(LmnAtomPtr in_proxy, LmnMembrane *calc_mem, Context ctx);
static hash_t atom_type(LmnWord atom, LmnLinkAttr attr);
static hash_t symbol_atom_type(LmnAtomPtr atom);
static hash_t data_atom_type(LmnWord atom, LmnLinkAttr attr);

hash_t mhash(LmnMembrane *mem)
{
  Context c = init_context();
  hash_t t;

#ifdef PROFILE
  status_start_state_hash_calc();
#endif

/*   printf("------- mhash(%s,%p) ------------\n", LMN_MEM_NAME(mem), mem); */
  t = membrane(mem, NULL, c);

#ifdef PROFILE
  status_finish_state_hash_calc();
#endif

  free_context(c);
  return t;
}

static hash_t membrane(LmnMembrane *mem, LmnMembrane *calc_mem, Context ctx)
{
  hash_t hash_sum = MEM_ADD_0;
  hash_t hash_mul = MEM_MUL_0;
  HashIterator iter;
  LmnAtomPtr atom;
  hash_t t;
  hash_t u;
  hash_t hash;

  if (mem == calc_mem) return CALUCULATING_MEM;
  if (calculated_mem_hash(ctx, mem, &t)) return (hash_t)t;

/*   printf("mem atom num = %d\n", mem->atom_num); */
/*   lmn_dump_mem(mem); */
  /* atoms */
  for (iter = hashtbl_iterator(&mem->atomset);
       !hashtbliter_isend(&iter);
       hashtbliter_next(&iter)) {
    AtomListEntry *ent = (AtomListEntry *)hashtbliter_entry(&iter)->data;
    LmnAtomPtr head = atomlist_head(ent);

    /* プロキシは除く */
    if (head != lmn_atomlist_end(ent) &&
        LMN_IS_PROXY_FUNCTOR(LMN_ATOM_GET_FUNCTOR(head))) continue;

    for (atom = head;
         atom != lmn_atomlist_end(ent);
         atom = LMN_ATOM_GET_NEXT(atom)) {
/*       printf("membrane: atom0 %s\n", */
/*              lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_ATOM_GET_FUNCTOR(atom)))); */
      if (!is_done_mol(ctx, atom)) {
/*         printf("membrane: atom %s\n", */
/*                lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_ATOM_GET_FUNCTOR(atom)))); */
        u = molecule(atom, mem, ctx);
        hash_sum += u;
        hash_mul *= u;
      }
    }
  }

  /* membranes */
  {
    LmnMembrane *child_mem;
    
    for(child_mem = mem->child_head; child_mem; child_mem = child_mem->next) {
      hash_t u = memunit(child_mem, NULL, mem, ctx, 0);
 /*      hash_t u = membrane(child_mem, NULL, done); */
      hash_sum += u;
      hash_mul *= u;
    }
  }


  hash = hash_sum ^ hash_mul;
  add_mem_hash(ctx, mem, hash);
/*   printf("mem(%s,%p) %lu, %lu, hash = %lu\n", */
/*          LMN_MEM_NAME(mem), mem, hash_sum, hash_mul, hash); */
  return hash;
}

static hash_t molecule(LmnAtomPtr atom, LmnMembrane *calc_mem, Context ctx)
{
  hash_t sum = ADD_0, mul = MUL_0;
  
  do_molecule((LmnWord)atom,
              LMN_ATTR_MAKE_LINK(0),
              calc_mem,
              ctx,
              -1,
              &sum,
              &mul);
  return sum ^ mul;
}

static void do_molecule(LmnWord atom,
                        LmnLinkAttr attr,
                        LmnMembrane *calc_mem,
                        Context ctx,
                        int i_parent,
                        hash_t *sum,
                        hash_t *mul)
{
  hash_t t;
  const int is_data = LMN_ATTR_IS_DATA(attr);

  if (!is_data && LMN_ATOM_GET_FUNCTOR(atom) == LMN_IN_PROXY_FUNCTOR) {
    /* 分子の計算では膜の外部に出て行かない */
    return;
  }
  else {
    if (!is_data) {
      if (is_done_mol(ctx, LMN_ATOM(atom))) return;
      add_done_mol(ctx, LMN_ATOM(atom));
    }

    t = unit(atom, attr, calc_mem, ctx, 0);
    (*sum) += t;
    (*mul) *= t;

    if (!is_data &&
        LMN_IS_SYMBOL_FUNCTOR(LMN_ATOM_GET_FUNCTOR(atom))) {
      const int arity = LMN_ATOM_GET_ARITY(atom);
      int i_arg;

      for (i_arg = 0; i_arg < arity; i_arg++) {
        if (i_arg != i_parent) {
          LmnLinkAttr to_attr = LMN_ATOM_GET_ATTR(atom, i_arg);
          do_molecule(LMN_ATOM_GET_LINK(atom, i_arg),
                      to_attr,
                      calc_mem,
                      ctx,
                      /* ↓ リンク先がシンボルアトムの場合にのみ意味のある値 */
                      LMN_ATTR_GET_VALUE(to_attr), 
                      sum,
                      mul);
        }
      }
    }
  }
}

static hash_t unit(LmnWord atom,
                   LmnLinkAttr attr,
                   LmnMembrane *calc_mem,
                   Context ctx,
                   int depth)
{
  if (LMN_ATTR_IS_DATA(attr)) {
    return data_atom_type(atom, attr);
  }
  else if (LMN_ATOM_GET_FUNCTOR(atom) == LMN_OUT_PROXY_FUNCTOR) {
    const LmnAtomPtr in_proxy = LMN_ATOM(LMN_ATOM_GET_LINK(atom, 0));
    return memunit(LMN_PROXY_GET_MEM(in_proxy),
                   LMN_ATOM(in_proxy),
                   calc_mem,
                   ctx,
                   depth);
  }
  else if (LMN_ATOM_GET_FUNCTOR(atom) == LMN_IN_PROXY_FUNCTOR) {
    return symbol_atom_type(LMN_ATOM(atom));
  }
  else {
    return atomunit(atom, attr, calc_mem, depth, ctx);
  }
}

/* アトム中心の計算単位 */
static hash_t atomunit(LmnWord atom,
                       LmnLinkAttr attr,
                       LmnMembrane *calc_mem,
                       int depth,
                       Context ctx)
{
  hash_t hash = 0;

  if (depth == DEPTH) {
    return link(atom, attr, calc_mem, ctx);
  }
  else if (LMN_ATTR_IS_DATA(attr)) {
    return atomlink(atom, attr);
  }
  else {
    const int arity = LMN_ATOM_GET_ARITY(atom);
    const int i_from = (depth == 0) ? -1 : (int)LMN_ATTR_GET_VALUE(attr);
    int i_arg;

    hash = atom_type(atom, attr);
    for (i_arg = 0; i_arg < arity; i_arg++) {
      if (i_arg != i_from) {
        hash_t t = unit(LMN_ATOM_GET_LINK(atom, i_arg),
                                      LMN_ATOM_GET_ATTR(atom, i_arg),
                                      calc_mem,
                                      ctx,
                                      depth + 1);
        /* TODO: ここでtに定数を掛けたほうがいいかも再帰的にCを掛けてい
           るので、係数が重なる危険性がある */
        hash = C*hash+t*E;
                       
      }
    }
/*     printf("atomunit(%s,%p,r=%d): %lu\n", */
/*            lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_ATOM_GET_FUNCTOR(atom))), */
/*            (void*)atom, depth, hash); */
    return hash;
  }
}

static hash_t memunit(LmnMembrane *mem,
                      LmnAtomPtr from_in_proxy,
                      LmnMembrane *calc_mem,
                      Context ctx,
                      int depth)
{
  hash_t hash = 0;
  AtomListEntry *insides;

  if (depth == DEPTH) return memlink(from_in_proxy, calc_mem, ctx);
  else {
    insides = lmn_mem_get_atomlist(mem, LMN_IN_PROXY_FUNCTOR);

    if (insides) {
      LmnAtomPtr in, out;
      for (in = atomlist_head(insides);
           in != lmn_atomlist_end(insides);
           in = LMN_ATOM_GET_NEXT(in)) {
        if (in != from_in_proxy) {
          out = LMN_ATOM(LMN_ATOM_GET_LINK(in, 0));
          hash += unit(LMN_ATOM_GET_LINK(out, 1),
                       LMN_ATOM_GET_ATTR(out, 1),
                       calc_mem,
                       ctx,
                       depth + 1)
            * mem_fromlink(mem, in, calc_mem, ctx);
        }
      }
    }

    hash += membrane(mem, calc_mem, ctx); /* hiroto論文にないh(Mem)の加算処理 */
/*  printf("memunit(%s,%p,r=%d): %lu\n", LMN_MEM_NAME(mem), mem, depth, hash); */
    return hash;
  }
}

static hash_t mem_fromlink(LmnMembrane *mem,
                           LmnAtomPtr in_proxy,
                           LmnMembrane *calc_mem,
                           Context ctx)
{
  LmnWord atom;
  hash_t hash = 0;
  LmnLinkAttr attr;

  for (;;) {
    atom = LMN_ATOM_GET_LINK(in_proxy, 1);
    attr = LMN_ATOM_GET_ATTR(in_proxy, 1);
    if (LMN_ATTR_IS_DATA(attr) ||
        LMN_ATOM_GET_FUNCTOR(atom) != LMN_OUT_PROXY_FUNCTOR) break;
    in_proxy = LMN_ATOM(LMN_ATOM_GET_LINK(atom, 0));
    hash = B * hash + membrane(LMN_PROXY_GET_MEM(in_proxy), calc_mem, ctx);
  }
          
/*   printf("memlink(a_to:%s) = %lu\n", */
/*          lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_ATOM_GET_FUNCTOR(atom))), */
/*          hash * (LMN_ATTR_GET_VALUE(attr) + 1) * atom_type(atom, attr)); */
  return membrane(mem, calc_mem, ctx) ^ (hash * atomlink(atom, attr));
}

static hash_t link(LmnWord atom,
                   LmnLinkAttr attr,
                   LmnMembrane *calc_mem,
                   Context ctx)
{
  if (LMN_ATTR_IS_DATA(attr)) {
    return atomlink(atom, attr);
  }
  else {
/*     printf("atom : %s\n", */
/*            lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_ATOM_GET_FUNCTOR(atom)))); */


     if (LMN_OUT_PROXY_FUNCTOR == LMN_ATOM_GET_FUNCTOR(atom)) {
       return memlink(LMN_ATOM(LMN_ATOM_GET_LINK(LMN_ATOM(atom), 0)),
                      calc_mem,
                      ctx);
    }
    else {
      return atomlink(atom, attr);
    }
  }
}

static hash_t memlink(LmnAtomPtr in_proxy, LmnMembrane *calc_mem, Context ctx)
{
  LmnWord atom;
  hash_t hash = 0;
  LmnLinkAttr attr;

  for (;;) {
    hash = B * hash + membrane(LMN_PROXY_GET_MEM(in_proxy), calc_mem, ctx);

    atom = LMN_ATOM_GET_LINK(in_proxy, 1);
    attr = LMN_ATOM_GET_ATTR(in_proxy, 1);
    if (LMN_ATTR_IS_DATA(attr) ||
        LMN_ATOM_GET_FUNCTOR(atom) != LMN_OUT_PROXY_FUNCTOR) break;
    in_proxy = LMN_ATOM(LMN_ATOM_GET_LINK(atom, 0));
  }
          
/*   printf("memlink(a_to:%s) = %lu\n", */
/*          lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_ATOM_GET_FUNCTOR(atom))), */
/*          hash * (LMN_ATTR_GET_VALUE(attr) + 1) * atom_type(atom, attr)); */
  return hash * atomlink(atom, attr);
}

static hash_t atomlink(LmnWord atom, LmnLinkAttr attr)
{
  const int i_from = LMN_ATTR_IS_DATA(attr) ? 0 : LMN_ATTR_GET_VALUE(attr);
  return (i_from+1) * atom_type(atom, attr);
}

static hash_t atom_type(LmnWord atom, LmnLinkAttr attr)
{
  if (LMN_ATTR_IS_DATA(attr)) {
    return data_atom_type(atom, attr);
  }
  else {
    return symbol_atom_type(LMN_ATOM(atom));
  }
}

static hash_t symbol_atom_type(LmnAtomPtr atom)
{
  return LMN_ATOM_GET_FUNCTOR(atom);
}

static hash_t data_atom_type(LmnWord atom, LmnLinkAttr attr) {
  switch(attr) {
    case LMN_INT_ATTR:
      return atom;
      break;
    case LMN_DBL_ATTR:
      /* TODO: 未実装 */
      LMN_ASSERT(FALSE);
      break;
    default:
      LMN_ASSERT(FALSE);
      break;
  }
  return -1;
}

static Context init_context(void)
{
  Context c = LMN_MALLOC(struct Context);
  c->done_mol = hashset_make(64);
  c->done_mem = st_init_ptrtable();
  return c;
}

static void free_context(Context ctx)
{
  hashset_free(ctx->done_mol);
  st_free_table(ctx->done_mem);
  LMN_FREE(ctx);
}

static int calculated_mem_hash(Context ctx, LmnMembrane *mem, hash_t *hash)
{
  if (st_lookup(ctx->done_mem, (st_data_t)mem, (st_data_t*)hash)) return 1;
  else return 0;
}

static int is_done_mol(Context ctx, void *p)
{
  return hashset_contains(ctx->done_mol, (HashKeyType)p);
}

static void add_mem_hash(Context ctx, LmnMembrane *mem, hash_t hash)
{
  st_insert(ctx->done_mem, (st_data_t)mem, (st_data_t)hash);
}

static void add_done_mol(Context ctx, void *p)
{
  hashset_add(ctx->done_mol, (HashKeyType)p);
}

