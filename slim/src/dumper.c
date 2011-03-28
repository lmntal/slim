/*
 * dumper.c - dump membrane
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
 * $Id: dumper.c,v 1.15 2008/09/19 18:03:22 taisuke Exp $
 */

#include <ctype.h>
#include "dumper.h"
#include "internal_hash.h"
#include "vector.h"
#include "rule.h"
#include "membrane.h"
#include "atom.h"
#include "symbol.h"
#include "functor.h"
#include "special_atom.h"
#include "util.h"
#include "error.h"
#include "ccallback.h"
#include "slim_header/memstack.h"
#include "slim_header/port.h"

#define MAX_DEPTH 1000
#define LINK_PREFIX "L"

struct AtomRec {
  BOOL done;
  SimpleHashtbl args;
  int link_num; /* 一連のプロキシに割り当てられた番号, proxy only */
};

struct DumpState {
  int link_num;
};

/* 文字からエスケープキャラクタへの対応表 */
char char_to_escape_char[] =
  {0,   0,   0,   0,   0,   0,   0,   0,   0, 't', 'n',   0,   0,  'r',   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0, '"',   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,'\\',   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0};

static BOOL dump_atom(LmnPort port,
                      LmnAtom atom,
                      SimpleHashtbl *ht,
                      LmnLinkAttr attr,
                      struct DumpState *s,
                      int call_depth);
static void lmn_dump_cell_internal(LmnPort port,
                                   LmnMembrane *mem,
                                   SimpleHashtbl *ht,
                                   struct DumpState *s);

static void dump_link(LmnPort port, LmnSAtom atom, int i, SimpleHashtbl *ht, struct DumpState *s);
static BOOL lmn_dump_mem_internal(LmnPort port,
                                  LmnMembrane *mem,
                                  SimpleHashtbl *ht,
                                  struct DumpState *s);

static BOOL dump_atom_args(LmnPort port,
                           LmnSAtom atom,
                           SimpleHashtbl *ht,
                           struct DumpState *s,
                           int call_depth);
static void dump_link_name(LmnPort port, int link_num);

static struct AtomRec *atomrec_make()
{
  struct AtomRec *a = LMN_MALLOC(struct AtomRec);
  a->done = FALSE;
  hashtbl_init(&a->args, 16);
  a->link_num = -1;
  return a;
}

static void atomrec_free(struct AtomRec *a)
{
  hashtbl_destroy(&a->args);
  LMN_FREE(a);
}

/* atomrec用 hashtblの解放 */
static void atomrec_tbl_destroy(SimpleHashtbl *ht)
{
  HashIterator iter;

  /* 開放処理. 今のところdataに0以外が入っていた場合
     struct AtomRecのポインタが格納されている */
  for (iter = hashtbl_iterator(ht); !hashtbliter_isend(&iter); hashtbliter_next(&iter)) {
    if (hashtbliter_entry(&iter)->data) {
      atomrec_free((struct AtomRec *)hashtbliter_entry(&iter)->data);
    }
  }
  hashtbl_destroy(ht);
}

static void dump_state_init(struct DumpState *s)
{
  s->link_num = 0;
}

static BOOL is_direct_printable(LmnFunctor f)
{
  const char *s;

  if (LMN_IS_PROXY_FUNCTOR(f) ||
      f == LMN_NIL_FUNCTOR) return TRUE;

  s = LMN_FUNCTOR_STR(f);
  if (!(isalpha(*s) && islower(*s))) return FALSE;
  while (*(++s)) {
    if (!(isalpha(*s) || isdigit(*s) || *s=='_')) return FALSE;
  }
  return TRUE;
}

/* htからatomに対応するAtomRecを取得。なければ追加してから返す */
static struct AtomRec *get_atomrec(SimpleHashtbl *ht, LmnSAtom atom)
{
  if (hashtbl_contains(ht, (HashKeyType)atom)) {
    return (struct AtomRec *)hashtbl_get(ht, (HashKeyType)atom);
  } else {
    struct AtomRec *t;
    t = atomrec_make();
    hashtbl_put(ht, (HashKeyType)atom, (HashValueType)t);
    return t;
  }
}

static void dump_atomname(LmnPort port, LmnFunctor f)
{
  /* dump module name */
  if (LMN_FUNCTOR_MODULE_ID(f) != ANONYMOUS) {
    port_put_raw_s(port, lmn_id_to_name(LMN_FUNCTOR_MODULE_ID(f)));
    port_put_raw_s(port, ".");
  }

  /* dump atom name */
  {
    const char *atom_name = lmn_id_to_name(LMN_FUNCTOR_NAME_ID(f));

    if (is_direct_printable(f)) {
      port_put_raw_s(port, atom_name);
    } else {
      port_put_raw_s(port, "'");
      dump_escaped(port, atom_name);
      port_put_raw_s(port, "'");
    }
  }
}

static void dump_arg(LmnPort port,
                     LmnSAtom atom,
                     int i,
                     SimpleHashtbl *ht,
                     struct DumpState *s,
                     int call_depth)
{
  struct AtomRec *rec;

  rec = get_atomrec(ht, atom);

  if (hashtbl_contains(&rec->args, i)) {
    dump_link(port, atom, i, ht, s);
  } else {
    dump_atom(port,
              LMN_SATOM_GET_LINK(atom, i),
              ht,
              LMN_SATOM_GET_ATTR(atom, i),
              s,
              call_depth + 1);
  }
}

static void dump_link(LmnPort port, LmnSAtom atom, int i, SimpleHashtbl *ht, struct DumpState *s)
{
  int link;
  struct AtomRec *t;

  t = get_atomrec(ht, atom);
  if (hashtbl_contains(&t->args, i)) {
    /* リンク名が決まっている */
    link = hashtbl_get(&t->args, i);
  } else {
    /* リンク名が決まっていないので新たに作る */
    link = s->link_num++;
    hashtbl_put(&t->args, i, link);
  }
  dump_link_name(port, link);
}

static void dump_link_name(LmnPort port, int link_num)
{
  port_put_raw_s(port, LINK_PREFIX);
  {
    char *s = int_to_str(link_num);
    port_put_raw_s(port, s);
    LMN_FREE(s);
  }
}

static BOOL dump_data_atom(LmnPort port,
                           LmnAtom data,
                           LmnLinkAttr attr)
{
  /* print only data (no link) */
  switch (attr) {
  case  LMN_INT_ATTR:
    {
      char *s = int_to_str((long)data);
      port_put_raw_s(port, s);
      LMN_FREE(s);
    }
    break;
  case  LMN_DBL_ATTR:
    {
      char buf[64];
      sprintf(buf, "%f", *(double*)data);
      port_put_raw_s(port, buf);
    }
    break;
  case LMN_SP_ATOM_ATTR:
    SP_ATOM_DUMP(data, port);
    break;
  case LMN_HL_ATTR:
    {
      char buf[16];
      port_put_raw_s(port, EXCLAMATION_NAME);
      if (lmn_env.show_hyperlink) {
        sprintf(buf, "%lx", LMN_HL_ID(lmn_hyperlink_at_to_hl(LMN_SATOM(data))));
      } else {
        sprintf(buf, "%lx", LMN_HL_ID(LMN_HL_ATOM_ROOT_HL(LMN_SATOM(data))));
      }
      port_put_raw_s(port, buf);
    }
    break;
  default:
    lmn_fatal("unexpected attr");
  }
  return TRUE;
}

static BOOL dump_list(LmnPort port,
                      LmnSAtom atom,
                      SimpleHashtbl *ht,
                      struct DumpState *s ,
                      int call_depth)
{
  BOOL first = TRUE;
  LmnLinkAttr attr;
  LmnAtom a = LMN_ATOM(atom);

  if (get_atomrec(ht, atom)->done) {
    dump_link(port, atom, 2, ht, s);
    return TRUE;
  }

  attr = LMN_ATTR_MAKE_LINK(2); /* 2 is output link position */

  port_put_raw_s(port, "[");

  /* リストの要素を出力していく*/
  while (TRUE) {
    if (LMN_HAS_FUNCTOR(a, attr, LMN_LIST_FUNCTOR) &&
        LMN_ATTR_GET_VALUE(attr) == 2) {
      struct AtomRec *rec;

      rec = get_atomrec(ht, LMN_SATOM(a));

      if (rec->done) { /* cyclic */
        int link = s->link_num++;
        port_put_raw_s(port, "|");
        hashtbl_put(&rec->args, LMN_ATTR_GET_VALUE(attr), link);
        dump_link_name(port, link);
        break;
      }
      rec->done = TRUE;

      if (!first)   port_put_raw_s(port, ",");
      first = FALSE;

      dump_arg(port, LMN_SATOM(a), 0, ht, s, call_depth + 1);

      attr = LMN_SATOM_GET_ATTR(a, 1);
      a = LMN_SATOM_GET_LINK(a, 1);
    }
    else if (LMN_HAS_FUNCTOR(a, attr, LMN_NIL_FUNCTOR)) {
      struct AtomRec *rec;
      rec = atomrec_make();
      rec->done = TRUE;
      hashtbl_put(ht, (HashKeyType)a, (HashValueType)rec);
      break;
    } else { /* list ends with non nil data */
      port_put_raw_s(port, "|");
      dump_atom(port, a, ht, attr, s, call_depth + 1);
      break;
    }
  }
  port_put_raw_s(port, "]");
  return TRUE;
}

/* propagate a link number to connected proxies */
static void propagate_proxy_link(LmnSAtom atom,
                                 LmnLinkAttr attr,
                                 SimpleHashtbl *ht,
                                 int link_num)
{
  struct AtomRec *t;
  int i;

  if (LMN_ATTR_IS_DATA(attr)) return;
  if (LMN_SATOM_GET_FUNCTOR(atom) != LMN_IN_PROXY_FUNCTOR &&
      LMN_SATOM_GET_FUNCTOR(atom) != LMN_OUT_PROXY_FUNCTOR) return;
  t = get_atomrec(ht, atom);
  if (t->link_num >= 0) return;

  t->link_num = link_num;
  for (i = 0; i < 2; i++) {
    propagate_proxy_link(LMN_SATOM(LMN_SATOM_GET_LINK(atom, i)),
                         LMN_SATOM_GET_ATTR(atom, i),
                         ht,
                         link_num);
  }
}

/* assign a link number to all connected proxies */
static void assign_link_to_proxy(LmnSAtom atom, SimpleHashtbl *ht, struct DumpState *s)
{
  struct AtomRec *t;

  t = get_atomrec(ht, atom);
  if (t->link_num < 0) {
    int link_num = s->link_num++;
    propagate_proxy_link(atom, LMN_ATTR_MAKE_LINK(0), ht, link_num);
  }
}

static BOOL dump_proxy(LmnPort port,
                       LmnSAtom atom,
                       SimpleHashtbl *ht,
                       int link_pos,
                       struct DumpState *s,
                       int call_depth)
{
  struct AtomRec *t;
  t = get_atomrec(ht, atom);
  t->done = TRUE;

  if (call_depth == 0) {
    LmnLinkAttr attr = LMN_SATOM_GET_ATTR(atom, 1);
    if (LMN_ATTR_IS_DATA(attr)) {
      dump_data_atom(port, LMN_SATOM_GET_LINK(atom, 1), attr);
      port_put_raw_s(port, "(");
      dump_link_name(port, t->link_num);
      port_put_raw_s(port, ")");
    } else {
      /* symbol atom has dumped */
      return FALSE;
    }
  }
  else {
    BOOL dumped = FALSE;
    /* outプロキシの接続先である膜の自由リンクが一つで、一引数の'+'アトムに
       接続している場合、膜をその場に出力する */
    if (LMN_SATOM_GET_FUNCTOR(atom) == LMN_OUT_PROXY_FUNCTOR) {
      const LmnSAtom in = LMN_SATOM(LMN_SATOM_GET_LINK(atom, 0));
      if (!LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(in, 1)) &&
          LMN_SATOM_GET_FUNCTOR(LMN_SATOM_GET_LINK(in, 1)) == LMN_UNARY_PLUS_FUNCTOR) {
        LmnMembrane *mem = LMN_PROXY_GET_MEM(in);
        if (lmn_mem_nfreelinks(mem, 1)) {
          get_atomrec(ht, LMN_SATOM(LMN_SATOM_GET_LINK(in, 1)))->done = TRUE;
          lmn_dump_mem_internal(port, mem, ht, s);
          dumped = TRUE;
        }
      }
    }
    if (!dumped) {
      dump_link_name(port, t->link_num);
    }
  }
  return TRUE;
}

static BOOL dump_symbol_atom(LmnPort port,
                             LmnSAtom atom,
                             SimpleHashtbl *ht,
                             int link_pos,
                             struct DumpState *s,
                             int call_depth)
{
  LmnFunctor f;
  LmnArity arity;
  struct AtomRec *t;

  f = LMN_SATOM_GET_FUNCTOR(atom);
  arity = LMN_FUNCTOR_ARITY(f);
  if (LMN_IS_PROXY_FUNCTOR(f)) arity--;

  t = get_atomrec(ht, atom);

  if ((call_depth > 0 && link_pos != arity - 1) || /* not last link */
      (call_depth > 0 && t->done)               || /* already printed */
      call_depth > MAX_DEPTH) {                    /* limit overflow */
    dump_link(port, atom, link_pos, ht, s);
    return TRUE;
  }

  if (t->done) return FALSE;
  t->done = TRUE;


  if (call_depth == 0 &&
      (f == LMN_UNARY_PLUS_FUNCTOR ||
       f == LMN_UNARY_MINUS_FUNCTOR)) {
    port_put_raw_s(port, lmn_id_to_name(LMN_FUNCTOR_NAME_ID(f)));
    return dump_atom(port,
                     LMN_SATOM_GET_LINK(atom, 0),
                     ht,
                     LMN_SATOM_GET_ATTR(atom, 0),
                     s,
                     1);
  }
  dump_atomname(port, f);
  dump_atom_args(port, atom, ht, s, call_depth);
  return TRUE;
}


static BOOL dump_atom_args(LmnPort port,
                           LmnSAtom atom,
                           SimpleHashtbl *ht,
                           struct DumpState *s,
                           int call_depth)
{

  int i;
  int limit = LMN_SATOM_GET_LINK_NUM(atom);

  if (call_depth > 0) limit--;

  if (limit > 0) {
    port_put_raw_s(port, "(");
    for (i = 0; i < limit; i++) {
      if (i > 0)     port_put_raw_s(port, ",");

      dump_arg(port, atom, i, ht, s, call_depth + 1);
    }
    port_put_raw_s(port, ")");
  }

  return TRUE;
}

static BOOL dump_atom(LmnPort port,
                      LmnAtom atom,
                      SimpleHashtbl *ht,
                      LmnLinkAttr attr,
                      struct DumpState *s,
                      int call_depth)
{
  if (LMN_ATTR_IS_DATA(attr)) {
    return dump_data_atom(port, atom, attr);
  }
  else {
    LmnFunctor f = LMN_SATOM_GET_FUNCTOR(atom);
    LmnLinkAttr link_pos = LMN_ATTR_GET_VALUE(attr);
    if (!lmn_env.show_proxy &&
        (f == LMN_IN_PROXY_FUNCTOR ||
         f == LMN_OUT_PROXY_FUNCTOR)) {
      return dump_proxy(port, LMN_SATOM(atom), ht, attr, s, call_depth);
    }
    else if (f == LMN_LIST_FUNCTOR &&
             link_pos == 2) {
      return dump_list(port, LMN_SATOM(atom), ht, s, call_depth);
    }
    else {
      return dump_symbol_atom(port, LMN_SATOM(atom), ht, link_pos, s, call_depth);
    }
  }
}

/* atom must be a symbol atom */
static BOOL dump_toplevel_atom(LmnPort port,
                               LmnSAtom atom,
                               SimpleHashtbl *ht,
                               struct DumpState *s)
{
  const LmnFunctor f = LMN_SATOM_GET_FUNCTOR(atom);
  if (!lmn_env.show_proxy &&
      (f == LMN_IN_PROXY_FUNCTOR ||
       f == LMN_OUT_PROXY_FUNCTOR)) {
    return dump_proxy(port, atom, ht, LMN_ATTR_MAKE_LINK(0), s, 0);
  }
  else {
    return dump_symbol_atom(port, atom, ht, LMN_ATTR_MAKE_LINK(0), s, 0);
  }
}

static int dump_history_f(st_data_t _key, st_data_t _value, st_data_t _arg)
{
  LmnPort port = (LmnPort)_arg;

  port_put_raw_s(port, " ");
  port_put_raw_s(port, lmn_id_to_name((lmn_interned_str)_key));

  return ST_CONTINUE;
}

static void dump_rule(LmnPort port, LmnRuleSet rs)
{
  unsigned int i, n;

  if (!lmn_ruleset_has_uniqrule(rs)) return;

  port_put_raw_s(port, "_CHR");

  n = lmn_ruleset_rule_num(rs);
  for (i = 0; i < n; i++) {
    LmnRule r;
    st_table_t   his_tbl;
    unsigned int his_num;

    r = lmn_ruleset_get_rule(rs, i);
    /* TODO: uniqはコピー先のルールオブジェクトに名前を設定するため,
     *        コピー元のルールオブジェクトの名前が空になってしまう */
    // if (lmn_rule_get_name(r) == ANONYMOUS) continue;
    his_tbl = lmn_rule_get_history_tbl(r);
    his_num = his_tbl ? st_num(his_tbl) : 0;

    /* 少なくともCOMMIT命令を1度以上処理したuniqルールを対象に, ルール名と履歴を出力する */
    if (his_num > 0) {
      port_put_raw_s(port, "[id:");
      port_put_raw_s(port, lmn_id_to_name(lmn_rule_get_name(r))); /* ルール名 */
      port_put_raw_s(port, "\"");
      st_foreach(his_tbl, dump_history_f, (st_data_t)port);
      port_put_raw_s(port, "\"]");
    }
  }
}

/* for debug @seiji */
void lmn_dump_rule(LmnPort port, LmnRuleSet rs)
{
  dump_rule(port, rs);
}

static void dump_ruleset(LmnPort port, struct Vector *v)
{
  unsigned int i;

  for (i = 0; i < vec_num(v); i++) {
    LmnRuleSet rs;
    char *s;

    rs = (LmnRuleSet)vec_get(v, i);
    s  = int_to_str(lmn_ruleset_get_id(rs));
    if (lmn_env.sp_dump_format == LMN_SYNTAX) {
      if (i > 0) {
        port_put_raw_s(port, ",");
      }
      port_put_raw_s(port, "'");
    }

    port_put_raw_s(port, "@");
    port_put_raw_s(port, s);
    if (lmn_env.show_chr) {
      dump_rule(port, rs);
    }

    if (lmn_env.sp_dump_format == LMN_SYNTAX) {
      port_put_raw_s(port, "'");
    }
    port_put_raw_s(port, ". ");
    LMN_FREE(s);
  }
}

/* for debug @seiji */
void lmn_dump_ruleset(LmnPort port, struct Vector *v)
{
  dump_ruleset(port, v);
}

static BOOL lmn_dump_mem_internal(LmnPort port,
                                  LmnMembrane *mem,
                                  SimpleHashtbl *ht,
                                  struct DumpState *s)
{
  if (hashtbl_contains(ht, (HashKeyType)mem)) return FALSE;

  hashtbl_put(ht, (HashKeyType)mem, (HashValueType)0);

  if (mem->name != ANONYMOUS) {
    port_put_raw_s(port, lmn_id_to_name(mem->name));
  }
  port_put_raw_s(port, "{");
  lmn_dump_cell_internal(port, mem, ht, s);
  port_put_raw_s(port, "}");

  return TRUE;
}

static void lmn_dump_cell_internal(LmnPort port,
                                   LmnMembrane *mem,
                                   SimpleHashtbl *ht,
                                   struct DumpState *s)
{
  enum {P0, P1, P2, P3, PROXY, PRI_NUM};
  Vector pred_atoms[PRI_NUM];
  AtomListEntry *ent;
  unsigned int i, j;
  LmnFunctor f;
  BOOL printed;

  if (!mem) return;

/*   if (hashtbl_contains(ht, (HashKeyType)mem)) return; */

  for (i = 0; i < PRI_NUM; i++) {
    vec_init(&pred_atoms[i], 16);
  }

  /* 優先順位に応じて起点となるアトムを振り分ける */
  EACH_ATOMLIST_WITH_FUNC(mem, ent, f, ({
    LmnSAtom atom;
    if (LMN_IS_EX_FUNCTOR(f)) continue;
    EACH_ATOM(atom, ent, ({
      int arity = LMN_SATOM_GET_ARITY(atom);
      if(LMN_SATOM_GET_FUNCTOR(atom)==LMN_RESUME_FUNCTOR)
        continue;
      if (f == LMN_IN_PROXY_FUNCTOR ||
          f == LMN_OUT_PROXY_FUNCTOR) {
        vec_push(&pred_atoms[PROXY], (LmnWord)atom);
      }
      /* 0 argument atom */
      else if (arity == 0) {
        vec_push(&pred_atoms[P0], (LmnWord)atom);
      }
      /* 1 argument, link to the last argument */
      else if (arity == 1 &&
               f != LMN_NIL_FUNCTOR &&
               (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, 0)) ||
                (int)LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(atom, 0)) ==
                LMN_SATOM_GET_ARITY(LMN_SATOM_GET_LINK(atom, 0)) - 1)) {
        vec_push(&pred_atoms[P1], (LmnWord)atom);
      }
      /* link to the last argument */
      else if (arity > 1 &&
               (LMN_ATTR_IS_DATA(LMN_SATOM_GET_ATTR(atom, arity-1)) ||
                (int)LMN_ATTR_GET_VALUE(LMN_SATOM_GET_ATTR(atom, arity-1)) ==
                LMN_SATOM_GET_ARITY(LMN_SATOM_GET_LINK(atom, arity-1)) - 1)) {
        vec_push(&pred_atoms[P2], (LmnWord)atom);
      }
      else {
        vec_push(&pred_atoms[P3], (LmnWord)atom);
      }
    }));
  }));

  if (!lmn_env.show_proxy) {
    /* assign link to proxies */
    for (i = 0; i < pred_atoms[PROXY].num; i++) {
      assign_link_to_proxy(LMN_SATOM(vec_get(&pred_atoms[PROXY], i)), ht, s);
    }
  }

  printed = FALSE;
  { /* dump atoms */
    for (i = 0; i < PRI_NUM; i++) {
      for (j = 0; j < pred_atoms[i].num; j++) {
        LmnSAtom atom = LMN_SATOM(vec_get(&pred_atoms[i], j));
        if (dump_toplevel_atom(port, atom, ht, s)) {
          /* TODO アトムの出力の後には常に ". "が入ってしまう.
             アトムの間に ", "を挟んだ方が見栄えが良い */
          port_put_raw_s(port, ". ");

          printed = TRUE;
        }
      }
    }
  }
  for (i = 0; i < PRI_NUM; i++) {
    vec_destroy(&pred_atoms[i]);
  }

  { /* dump chidren */
    LmnMembrane *m;
    BOOL dumped = FALSE;
    for (m = mem->child_head; m; m = m->next) {
      if (lmn_dump_mem_internal(port, m, ht, s)) {
        dumped = TRUE;
        if (m->next)
          port_put_raw_s(port, ", ");
      }
    }
    if (dumped) {
      /* 最後の膜の後に ". "を出力 */
      port_put_raw_s(port, ". ");
    }
  }

  if (lmn_env.show_ruleset) {
    dump_ruleset(port, &mem->rulesets);
  }
}

static void lmn_dump_cell_nonewline(LmnPort port, LmnMembrane *mem)
{
  SimpleHashtbl ht;
  struct DumpState s;

  dump_state_init(&s);

  hashtbl_init(&ht, 128);
  lmn_dump_cell_internal(port, mem, &ht, &s);

  atomrec_tbl_destroy(&ht);
}

void lmn_dump_cell_stdout(LmnMembrane *mem)
{
  LmnPort port = lmn_stdout_port();
  lmn_dump_cell(mem, port);
  port_put_raw_s(port, "\n");
  lmn_port_free(port);
}

void lmn_dump_cell(LmnMembrane *mem, LmnPort port)
{
  switch (lmn_env.output_format) {
  case DEFAULT:
    lmn_dump_cell_nonewline(port, mem);
    break;
  case DOT:
    lmn_dump_dot(mem);
    break;
  case DEV:
    lmn_dump_mem_dev(mem);
    break;
  default:
    lmn_fatal("unexpected.");
    exit(EXIT_FAILURE);
  }
}

void lmn_dump_mem_stdout(LmnMembrane *mem)
{
  LmnPort port = lmn_stdout_port();
  lmn_dump_mem(mem, port);
  port_put_raw_s(port, "\n");
  lmn_port_free(port);
}

/* print membrane structure */
void lmn_dump_mem(LmnMembrane *mem, LmnPort port)
{
  switch (lmn_env.output_format) {
  case DEFAULT:
    port_put_raw_s(port, "{");
    lmn_dump_cell_nonewline(port, mem);
    port_put_raw_s(port, "}");
    break;
  case DOT:
    lmn_dump_dot(mem);
    break;
  case DEV:
    lmn_dump_mem_dev(mem);
    break;
  default:
    lmn_fatal("unexpected.");
    exit(EXIT_FAILURE);
  }
}

static void dump_atom_dev(LmnSAtom atom)
{
  LmnFunctor f;
  LmnArity arity;
  unsigned int i;

  f = LMN_SATOM_GET_FUNCTOR(atom);
  arity = LMN_FUNCTOR_ARITY(f);
  fprintf(stdout, "Func[%3u], Name[%5s], A[%2u], Addr[%lu], ID[%2lu], "
                , f
                , lmn_id_to_name(LMN_FUNCTOR_NAME_ID(f))
                , arity
                , (LmnWord)atom
                , LMN_SATOM_ID(atom));

  for (i = 0; i < arity; i++) {
    LmnLinkAttr attr;

    fprintf(stdout, "%u: ", i);
    attr = LMN_SATOM_GET_ATTR(atom,i);
    if (i == 2 && LMN_IS_PROXY_FUNCTOR(f)) { /* membrane */
      fprintf(stdout, "mem[%p], ", (void*)LMN_PROXY_GET_MEM(atom));
    }
    else if (!LMN_ATTR_IS_DATA(attr)) { /* symbol atom */
      fprintf(stdout, "link[%d, %lu], ", LMN_ATTR_GET_VALUE(attr), (LmnWord)LMN_SATOM_GET_LINK(atom, i));
    } else {
      switch (attr) {
        case  LMN_INT_ATTR:
          fprintf(stdout, "int[%lu], ", LMN_SATOM_GET_LINK(atom,i));
          break;
        case  LMN_DBL_ATTR:
          fprintf(stdout, "double[%f], ", *(double*)LMN_SATOM_GET_LINK(atom,i));
          break;
        default:
          fprintf(stdout, "unknown data type[%d], ", attr);
          break;
      }
    }
  }

  fprintf(stdout, "\n");
}

static void dump_ruleset_dev(struct Vector *v)
{
  unsigned int i;
  fprintf(stdout, "ruleset[");
  for (i = 0;i < v->num; i++) {
     fprintf(stdout, "%d ", lmn_ruleset_get_id((LmnRuleSet)vec_get(v, i)));
  }
  fprintf(stdout, "]\n");
}


void lmn_dump_mem_dev(LmnMembrane *mem)
{
  AtomListEntry *ent;
  if (!mem) return;

  fprintf(stdout, "{\n");
  fprintf(stdout, "Mem[%u], Addr[%lu], ID[%lu]\n"
                , LMN_MEM_NAME_ID(mem)
                , (LmnWord)mem
                , lmn_mem_id(mem));
  EACH_ATOMLIST(mem, ent, ({
    LmnSAtom atom;
    EACH_ATOM(atom, ent, {
      dump_atom_dev(atom);
    });
  }));

  dump_ruleset_dev(&mem->rulesets);
  lmn_dump_mem_dev(mem->child_head);
  lmn_dump_mem_dev(mem->next);

  fprintf(stdout, "}\n");
}


/*----------------------------------------------------------------------
 * dump dot
 */

static void dump_dot_cell(LmnMembrane *mem,
                          SimpleHashtbl *ht,
                          int *data_id,
                          int *cluster_id)
{
  AtomListEntry *ent;
  LmnMembrane *m;
  LmnPort out;
  unsigned int i;

  if (!mem) return;

  out = lmn_stdout_port();

  /* dump node labels */
  EACH_ATOMLIST(mem, ent, ({
    LmnSAtom atom;
    EACH_ATOM(atom, ent, ({
      fprintf(stdout, "%lu [label = \"", (LmnWord)atom);
      dump_atomname(out, LMN_SATOM_GET_FUNCTOR(atom));
      fprintf(stdout, "\", shape = circle];\n");
      for (i = 0; i < LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(atom)); i++) {
        LmnLinkAttr attr = LMN_SATOM_GET_ATTR(atom, i);
        if (LMN_ATTR_IS_DATA(attr)) {
          fprintf(stdout, "%lu [label = \"", (LmnWord)LMN_SATOM_PLINK(atom, i));
          dump_data_atom(out, LMN_SATOM_GET_LINK(atom, i), attr);
          fprintf(stdout, "\", shape = box];\n");
        }
      }
    }));
  }));

  /* dump connections */
  EACH_ATOMLIST(mem, ent, ({
    LmnSAtom atom;
    EACH_ATOM(atom, ent, ({
      struct AtomRec *ar = (struct AtomRec *)hashtbl_get_default(ht, (HashKeyType)atom, 0);
      unsigned int arity = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(atom));

      for (i = 0; i < arity; i++) {
        LmnLinkAttr attr = LMN_SATOM_GET_ATTR(LMN_SATOM(atom), i);

        if (ar && hashtbl_contains(&ar->args, i)) continue;
        fprintf(stdout, "%lu -- ", (LmnWord)atom);
        if (LMN_ATTR_IS_DATA(attr)) {
          fprintf(stdout, " %lu", (LmnWord)LMN_SATOM_PLINK(atom, i));
          (*data_id)++;
        }
        else { /* symbol atom */
          struct AtomRec *ar;
          LmnAtom atom2 = LMN_SATOM_GET_LINK(atom, i);
          if (hashtbl_contains(ht, atom2)) {
            ar = (struct AtomRec *)hashtbl_get(ht, atom2);
          } else {
            ar = atomrec_make();
            hashtbl_put(ht, (HashKeyType)atom2, (HashValueType)ar);
          }
          hashtbl_put(&ar->args, LMN_ATTR_GET_VALUE(attr), 1);
          fprintf(stdout, "%lu", atom2);
        }
        fprintf(stdout, "\n");
      }
    }));
  }));

  /* dump chidren */
  for (m = mem->child_head; m; m = m->next) {
    fprintf(stdout, "subgraph cluster%d {\n", *cluster_id);
    (*cluster_id)++;
    dump_dot_cell(m, ht, data_id, cluster_id);
    fprintf(stdout, "}\n");
  }
}

void lmn_dump_dot(LmnMembrane *mem)
{
  int cluster_id = 0, data_id = 0;
  struct DumpState s;
  SimpleHashtbl ht;

  dump_state_init(&s);
  hashtbl_init(&ht, 128);

  fprintf(stdout, "// This is an auto generated file by SLIM\n\n"
                  "graph {\n"
                  "node [bgcolor=\"trasnparent\",truecolor=true,color=\"#000000\",style=filled,fillcolor=\"#ffd49b50\"];\n"
                  "edge [color=\"#000080\"];\n"

          );

  dump_dot_cell(mem, &ht, &data_id, &cluster_id);

  fprintf(stdout, "}\n");

  atomrec_tbl_destroy(&ht);
}


void cb_dump_mem(ReactCxt rc,
                 LmnMembrane *mem,
                 LmnAtom a0, LmnLinkAttr t0,
                 LmnAtom a1, LmnLinkAttr t1,
                 LmnAtom a2, LmnLinkAttr t2)
{
  LmnAtom in = LMN_SATOM_GET_LINK(a1, 0);
  LmnMembrane *m = LMN_PROXY_GET_MEM(in);

  lmn_mem_delete_atom(m, LMN_SATOM_GET_LINK(in, 1), LMN_SATOM_GET_ATTR(in, 1));
  lmn_mem_delete_atom(m, in, LMN_SATOM_GET_ATTR(a1, 0));
  lmn_mem_delete_atom(mem, a1, t1);

  lmn_dump_mem(m, LMN_PORT(a0));

  lmn_mem_newlink(mem,
                  a0, t0, 0,
                  a2, t2, LMN_ATTR_GET_VALUE(t2));

  if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
    lmn_memstack_delete(RC_MEMSTACK(rc), m);
  }
  lmn_mem_delete_mem(m->parent, m);
}

void dumper_init()
{
  lmn_register_c_fun("cb_dump_mem", cb_dump_mem, 3);
}

void dumper_finalize()
{
}

void dump_escaped(LmnPort port, const char *s)
{
  while (*s) {
    if (char_to_escape_char[(int)*s]) {
      port_put_raw_c(port, '\\');
      port_put_raw_c(port, char_to_escape_char[(int)*s]);
    } else {
      port_put_raw_c(port, *s);
    }
    s++;
  }
}

void lmn_dump_atom(LmnPort port, LmnWord atom, LmnLinkAttr attr)
{
  struct DumpState s;
  SimpleHashtbl ht;

  dump_state_init(&s);
  hashtbl_init(&ht, 0);
  dump_atom(port, atom, &ht, attr, &s, 0);
  atomrec_tbl_destroy(&ht);
}
