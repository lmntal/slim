/**
 * mhash.c
 *
 * cf. devel/sample/khiroto/mhash.lmn
 * 膜階層を上向きにたどらない点が広戸版と異なる
 * （理由はコメントやローカルページ参照）
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
#include <limits.h>
#include "functor.h"

/* tag */
#define ATOM_OBJ  0
#define MEM_OBJ   1

/* 加乗および乗算を行う際の剰余演算に用いる定数 */
#define ADD_MOD_FACTOR  (INT_MAX/100)
#define MUL_MOD_FACTOR  (INT_MAX/100000)

/* シンボルアトムのハッシュ値 */
static inline unsigned int atom_hash(LmnAtomPtr a) {
  return LMN_FUNCTOR_NAME_ID(LMN_ATOM_GET_FUNCTOR(a));
}
/* データアトムのハッシュ値 */
static inline int data_hash(LmnAtomPtr a, LmnArity n) {
  int ret;
  switch(LMN_ATOM_GET_ATTR(a, n)) {
    case LMN_INT_ATTR:
      ret = LMN_ATOM_GET_LINK(a,n);
      break;
    case LMN_DBL_ATTR:
      /* TODO: 未実装 */
      LMN_ASSERT(FALSE);
      break;
    default:
      LMN_ASSERT(FALSE);
      break;
  }
  return ret;
}

/* 膜 -> ハッシュ値 */
static SimpleHashtbl mem2h;

static int mhash_internal(LmnMembrane *mem)
{
  /*
   * 適当な初期値
   *   add: 加算の基本値
   *   mul: 乗算の基本値
   */
  long long int add = 3412;
  long long int mul = 3412;
  /* 一時変数 */
  LmnAtomPtr a = NULL;
  LmnMembrane *m = NULL;
  HashEntry *obj = NULL;
  /* 履歴管理 */
  SimpleHashtbl objs0;  /* 全ての子膜および子アトム */
  SimpleHashtbl objs1;  /* 未処理 */
  HashSet objs2;        /* 既処理 */
  hashtbl_init(&objs0, 128);
  hashtbl_init(&objs1, 128);
  hashset_init(&objs2, 128);
  HashIterator i0;
  HashIterator i1;
  HashSetIterator i2;

  HashIterator atomit;

  /*
   * objs0 <- atom
   */
  for (atomit = hashtbl_iterator(&mem->atomset);
      !hashtbliter_isend(&atomit);
      hashtbliter_next(&atomit)) {
    AtomListEntry *ent = (AtomListEntry *)hashtbliter_entry(&atomit)->data;
    if (atomlist_head(ent) != lmn_atomlist_end(ent) &&
        LMN_IS_PROXY_FUNCTOR(LMN_ATOM_GET_FUNCTOR(atomlist_head(ent)))) {
      continue; /* skip proxies */
    }
    for (a = atomlist_head(ent);
        a != lmn_atomlist_end(ent);
        a = LMN_ATOM_GET_NEXT(a)) {
      hashtbl_put(&objs0, (HashKeyType)a, (HashValueType)ATOM_OBJ);
    }
  }
  /* objs0 <- mem */
  for(m = mem->child_head; m; m = m->next) {
    hashtbl_put(&objs0, (HashKeyType)m, (HashValueType)MEM_OBJ);
    /* 子膜はあらかじめ計算しておく */
    hashtbl_put(&mem2h, (HashKeyType)m, (HashValueType)mhash_internal(m));
  }

  while (hashtbl_num(&objs0) > 0) {
    /* 初期値 */
    long long int mol;           /* 分子のハッシュ値 */
    long long int tmp = 0;       /* 基本計算単位のハッシュ値 */
    long long int mol_add = 0;   /* 基本計算単位の加算基本値 */
    long long int mol_mul = 41;  /* 基本計算単位の乗算基本値 */

    hashtbl_clear(&objs1);
    hashset_clear(&objs2);
    i0 = hashtbl_iterator(&objs0);
    obj = hashtbliter_entry(&i0);

    hashtbl_put(&objs1, (HashKeyType)obj->key, (HashValueType)obj->data);
    hashtbl_delete(&objs0, (HashKeyType)obj->key);

    /* 分子のハッシュ値の計算 */
    while (hashtbl_num(&objs1) > 0) {
      HashKeyType objptr;
      HashValueType objtag;

      i1 = hashtbl_iterator(&objs1);
      obj = hashtbliter_entry(&i1);
      objptr = obj->key;
      objtag = obj->data;

      hashset_add(&objs2, (HashKeyType)obj->key);
      hashtbl_delete(&objs1, (HashKeyType)obj->key);

      /* DEBUG */
      assert(!hashtbl_contains(&objs1,objptr));

      if (ATOM_OBJ == objtag) { /* アトム */
        int i;

        a = (LmnAtomPtr)objptr;
        tmp = atom_hash(a);

        for (i = 0; i < LMN_ATOM_GET_ARITY(a); i++) { /* リンク先 */
          if (LMN_IS_PROXY_FUNCTOR(LMN_ATOM_GET_FUNCTOR(a)) && i == 2) continue; /* 膜 */
          LmnWord link = LMN_ATOM_GET_LINK(a, i);
          LmnLinkAttr attr = LMN_ATOM_GET_ATTR(a, i);
          tmp *= 31; /* 適当な値 */
          assert(tmp>=0);

          if (LMN_ATTR_IS_DATA(attr)) { /* データアトム */
            tmp += data_hash(a, i);
            assert(tmp>=0);
          }
          else if (LMN_IN_PROXY_FUNCTOR == LMN_ATOM_GET_FUNCTOR((LmnAtomPtr)link)) {
            tmp += (atom_hash((LmnAtomPtr)link) * ((unsigned int)attr + 1));
            assert(tmp>=0);
          }
          else if (LMN_OUT_PROXY_FUNCTOR == LMN_ATOM_GET_FUNCTOR((LmnAtomPtr)link)) {
            unsigned int t = 0;
            LmnAtomPtr in; /* inside proxy */

            m = LMN_PROXY_GET_MEM(LMN_ATOM_GET_LINK((LmnAtomPtr)link, 0)); /* 子膜 */
            if(!hashset_contains(&objs2, (HashKeyType)m)) {
              hashtbl_put(&objs1, (HashKeyType)m, MEM_OBJ);
            }
            /* 膜を貫くリンクの処理 */
            while (!LMN_ATTR_IS_DATA(attr) &&
            LMN_OUT_PROXY_FUNCTOR == LMN_ATOM_GET_FUNCTOR((LmnAtomPtr)link)) {
              in = (LmnAtomPtr)LMN_ATOM_GET_LINK((LmnAtomPtr)link, 0);
              link = LMN_ATOM_GET_LINK(in, 1);
              attr = LMN_ATOM_GET_ATTR(in, 1);
              m = LMN_PROXY_GET_MEM((LmnAtomPtr)in);
              assert(hashtbl_get_default(&mem2h, (HashKeyType)m, 0));
              t += hashtbl_get_default(&mem2h, (HashKeyType)m, 0);
              t *= 13; /* 適当な値 */
              assert(t>0);
            }
            if (LMN_ATTR_IS_DATA(attr)) { /* 接続先がデータアトム */
              t += data_hash((LmnAtomPtr)in, 1);
            }
            else { /* 接続先がシンボルアトム */
              t += atom_hash((LmnAtomPtr)link);
              t *= (unsigned int)(attr + 1);
              assert(t>0);
            }
          }
          else { /* シンボルアトム */
            if(!hashset_contains(&objs2, (HashKeyType)link)) {
              hashtbl_put(&objs1, (HashKeyType)link, ATOM_OBJ);
            }
            tmp += (atom_hash((LmnAtomPtr)link) * ((unsigned int)attr + 1));
            assert(tmp>=0);
          }
        }
      }
      else if (MEM_OBJ == objtag) { /* 膜 */
        LmnWord link;
        LmnLinkAttr attr;
        LmnAtomPtr in;
        AtomListEntry *insides;
        unsigned int myhash; /* この膜のハッシュ値 */

        m = (LmnMembrane *)objptr;
        assert(hashtbl_contains(&mem2h, (HashKeyType)m));
        myhash = (unsigned int)hashtbl_get_default(&mem2h, (HashKeyType)m, 0);
        tmp = myhash;
        /*
         * TODO:
         * a(X1). {{'+'(X1)}}
         * のような構造で広戸版と同じバグ
         * （mem2hにない膜をgetしようとする）
         * が出るので，上向きにたどらないようにした
         */
//        insides = lmn_mem_get_atomlist(m, LMN_IN_PROXY_FUNCTOR);
//        if (insides) {
//          for (a = atomlist_head(insides);
//              a != lmn_atomlist_end(insides);
//              a = LMN_ATOM_GET_NEXT(a)) {
//            unsigned int s = 0, t = 0;
//
//            /* 膜の外側をたどる */
//            LmnAtomPtr out = (LmnAtomPtr)LMN_ATOM_GET_LINK(a, 0);
//            if(LMN_OUT_PROXY_FUNCTOR == LMN_ATOM_GET_ATTR(out, 1)) { /* リンク先が膜の場合 */
//              LmnMembrane *mm = LMN_PROXY_GET_MEM(out);
//              if (!hashset_contains(&objs2, (HashKeyType)mm)) {
//                hashtbl_put(&objs1, (HashKeyType)mm, (HashValueType)MEM_OBJ);
//              }
//            }
//            else { /* リンク先がアトムの場合 */
//              if (LMN_ATTR_IS_DATA(LMN_ATOM_GET_ATTR(out, 1))) { /* データアトム */
//                tmp += data_hash(out, 1);
//              }
//              else { /* シンボルアトム */
//                LmnAtomPtr aa = (LmnAtomPtr)LMN_ATOM_GET_LINK(out, 1);
//                if (!hashset_contains(&objs2, (HashKeyType)aa)) {
//                  hashtbl_put(&objs1, (HashKeyType)aa, (HashValueType)ATOM_OBJ);
//                }
//              }
//            }
//            in = out;
//            link = LMN_ATOM_GET_LINK(out, 1);
//            attr = LMN_ATOM_GET_ATTR(out, 1);
//            /* 膜を貫くリンクの処理 */
//            while (!LMN_ATTR_IS_DATA(attr) &&
//                LMN_OUT_PROXY_FUNCTOR == LMN_ATOM_GET_FUNCTOR((LmnAtomPtr)link)) {
//              in = (LmnAtomPtr)LMN_ATOM_GET_LINK((LmnAtomPtr)link, 0);
//              link = LMN_ATOM_GET_LINK(in, 1);
//              attr = LMN_ATOM_GET_ATTR(in, 1);
//lmn_dump_cell(LMN_PROXY_GET_MEM(in));
//              assert(hashtbl_get_default(&mem2h, (HashKeyType)LMN_PROXY_GET_MEM(in), 0));
//              s += (unsigned int)hashtbl_get_default(&mem2h, (HashKeyType)LMN_PROXY_GET_MEM(in), 0);
//              s *= 13;
//              assert(s>0);
//            }
//            if (LMN_ATTR_IS_DATA(attr)) { /* データアトム */
//              s += data_hash(in, 1);
//            }
//            else { /* シンボルアトム */
//              s += atom_hash((LmnAtomPtr)link);
//              s *= (unsigned int)(attr + 1);
//              assert(s>0);
//            }
//
//            /* 膜の内側をたどる */
//            in = a;
//            link = LMN_ATOM_GET_LINK(a, 1);
//            attr = LMN_ATOM_GET_ATTR(a, 1);
//            /* 膜を貫くリンクの処理 */
//            while (!LMN_ATTR_IS_DATA(attr) &&
//                LMN_OUT_PROXY_FUNCTOR == LMN_ATOM_GET_FUNCTOR((LmnAtomPtr)link)) {
//              in = (LmnAtomPtr)LMN_ATOM_GET_LINK((LmnAtomPtr)link, 0);
//              link = LMN_ATOM_GET_LINK(in, 1);
//              attr = LMN_ATOM_GET_ATTR(in, 1);
//lmn_dump_cell(LMN_PROXY_GET_MEM(in));
//              assert(hashtbl_get_default(&mem2h, (HashKeyType)LMN_PROXY_GET_MEM(in), 0));
//              t += (int)hashtbl_get_default(&mem2h, (HashKeyType)LMN_PROXY_GET_MEM(in), 0);
//              t *= 13;
//              assert(t>0);
//            }
//            if (LMN_ATTR_IS_DATA(attr)) { /* データアトム */
//              assert(LMN_IN_PROXY_FUNCTOR == LMN_ATOM_GET_FUNCTOR((LmnAtomPtr)a));
//              t *= data_hash(in, 1);
//              assert(t>=0);
//            }
//            else { /* シンボルアトム */
//              t *= atom_hash((LmnAtomPtr)link);
//              assert(t>=0);
//              t *= (unsigned int)(attr + 1);
//              assert(t>=0);
//            }
//            tmp += myhash ^ t * s;
//            assert(tmp>0);
//          }
//        }
      }
      else {
        assert(0);
      }
      mol_add += tmp;
      mol_add %= ADD_MOD_FACTOR;
      mol_mul *= (tmp % MUL_MOD_FACTOR);
      assert(mol_mul>=0);
      mol_mul %= (MUL_MOD_FACTOR / 10);
    }

    for (i2 = hashset_iterator(&objs2);
        !hashsetiter_isend(&i2);
        hashsetiter_next(&i2)) {
      /*
       * a(X1). {{'+'(X1)}}
       * のようなとき，膜階層最上位において
       * 「アトム->膜」という走査順と「膜->アトム」という走査順で
       * 結果が違ってしまっていたため以下のif文を追加
       * （前者では子膜が１回，後者では２回処理されていた）
       * メインループでアトム->膜の順で処理するなど，他に対処方法あり
       */
      if(hashtbl_get(&objs0, hashsetiter_entry(&i2)) != MEM_OBJ) {
        hashtbl_delete(&objs0, hashsetiter_entry(&i2));
      }
    }

    mol = mol_add ^ mol_mul;
    add += mol;
    assert(add>0);
    add %= (unsigned int)ADD_MOD_FACTOR;
    mul *= mol;
    assert(mul>=0);
    mul %= (unsigned int)MUL_MOD_FACTOR;
  }

  hashtbl_destroy(&objs0);
  hashtbl_destroy(&objs1);
  hashset_destroy(&objs2);
  assert(INT_MIN <= mul^add && mul^add <= INT_MAX);
  return (int)(mul ^ add) + mem->name; /* 膜名を反映させる */
}

int mhash(LmnWord mem)
{
  int ret = 1;
/*   hashtbl_init(&mem2h, 32); */
/*   ret =  mhash_internal((LmnMembrane *)mem); */
/*   hashtbl_destroy(&mem2h); */
  return ret;
}

