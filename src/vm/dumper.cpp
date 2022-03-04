/*
 * dumper.cpp - dump membrane
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
 * $Id: dumper.c,v 1.15 2008/09/19 18:03:22 taisuke Exp $
 */

#include "dumper.h"

#include <ctype.h>

#include <ios>
#include <ostream>

#include "atomlist.hpp"
#include "ccallback.h"
#include "rule.hpp"
#include "symbol.h"

#define MAX_DEPTH   1000
#define LINK_PREFIX "L"

struct AtomRec {
  BOOL done;
  SimpleHashtbl args;
  int link_num; /* 一連のプロキシに割り当てられた番号, proxy only */
};

struct DumpState {
  int link_num;
  DumpState();
};

/* 文字からエスケープキャラクタへの対応表 */
char char_to_escape_char[] = {
  0, 0, 0, 0, 0, 0,    0, 0, 0, 't', 'n', 0, 0, 'r', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, '"',  0, 0, 0, 0,   0,   0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0,    0, 0, 0, 0,   0,   0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, '\\', 0, 0, 0, 0,   0,   0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0,    0, 0, 0, 0,   0,   0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0,    0, 0, 0, 0,   0,   0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0,    0, 0, 0, 0,   0,   0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0,    0, 0, 0, 0,   0,   0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0,    0, 0, 0, 0,   0,   0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

static BOOL dump_atom(
    LmnPortRef port,
    LmnAtomRef atom,
    SimpleHashtbl *ht,
    LmnLinkAttr attr,
    struct DumpState *s,
    int call_depth);
static void
lmn_dump_cell_internal(LmnPortRef port, LmnMembraneRef mem, SimpleHashtbl *ht, struct DumpState *s);

static void
dump_link(LmnPortRef port, LmnSymbolAtomRef atom, int i, SimpleHashtbl *ht, struct DumpState *s);
static BOOL
lmn_dump_mem_internal(LmnPortRef port, LmnMembraneRef mem, SimpleHashtbl *ht, struct DumpState *s);

static BOOL dump_atom_args(
    LmnPortRef port,
    LmnSymbolAtomRef atom,
    SimpleHashtbl *ht,
    struct DumpState *s,
    int call_depth);
static void dump_link_name(LmnPortRef port, int link_num);
static BOOL dump_hl_attratom(LmnPortRef port, LmnAtomRef atom, LmnLinkAttr attr);

static void lmn_dump_atom_json(LmnSymbolAtomRef atom);
static void lmn_dump_link_json(LmnSymbolAtomRef atom, int index);
static void lmn_dump_mem_json(LmnMembraneRef mem);

static struct AtomRec *atomrec_make() {
  struct AtomRec *a = LMN_MALLOC(struct AtomRec);
  a->done = FALSE;
  hashtbl_init(&a->args, 16);
  a->link_num = -1;
  return a;
}

static void atomrec_free(struct AtomRec *a) {
  hashtbl_destroy(&a->args);
  LMN_FREE(a);
}

/* atomrec用 hashtblの解放 */
static void atomrec_tbl_destroy(SimpleHashtbl *ht) {
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

// static void dump_state_init(struct DumpState *s) { ; }
DumpState::DumpState() {
  link_num = 0;
}

static BOOL is_direct_printable(LmnFunctor f) {
  const char *s;

  if (LMN_IS_PROXY_FUNCTOR(f) || f == LMN_NIL_FUNCTOR)
    return TRUE;

  s = LMN_FUNCTOR_STR(f);
  if (!(isalpha((unsigned char)*s) && islower((unsigned char)*s)))
    return FALSE;
  while (*(++s)) {
    if (!(isalpha((unsigned char)*s) || isdigit((unsigned char)*s) || *s == '_'))
      return FALSE;
  }
  return TRUE;
}

/* htからatomに対応するAtomRecを取得。なければ追加してから返す */
static struct AtomRec *get_atomrec(SimpleHashtbl *ht, LmnSymbolAtomRef atom) {
  if (hashtbl_contains(ht, (HashKeyType)atom)) {
    return (struct AtomRec *)hashtbl_get(ht, (HashKeyType)atom);
  } else {
    struct AtomRec *t;
    t = atomrec_make();
    hashtbl_put(ht, (HashKeyType)atom, (HashValueType)t);
    return t;
  }
}

static void dump_atomname(LmnPortRef port, LmnFunctor f) {
  /* dump module name */
  if (LMN_FUNCTOR_MODULE_ID(lmn_functor_table, f) != ANONYMOUS) {
    port_put_raw_s(port, lmn_id_to_name(LMN_FUNCTOR_MODULE_ID(lmn_functor_table, f)));
    port_put_raw_s(port, ".");
  }

  /* dump atom name */
  {
    const char *atom_name = lmn_id_to_name(LMN_FUNCTOR_NAME_ID(lmn_functor_table, f));

    if (is_direct_printable(f)) {
      port_put_raw_s(port, atom_name);
    } else {
      port_put_raw_s(port, "'");
      dump_escaped(port, atom_name);
      port_put_raw_s(port, "'");
    }
  }
}

static void dump_arg(
    LmnPortRef port,
    LmnSymbolAtomRef atom,
    int i,
    SimpleHashtbl *ht,
    struct DumpState *s,
    int call_depth) {
  struct AtomRec *rec;

  rec = get_atomrec(ht, atom);

  if (hashtbl_contains(&rec->args, i)) {
    dump_link(port, atom, i, ht, s);
  } else {
    dump_atom(port, atom->get_link(i), ht, atom->get_attr(i), s, call_depth + 1);
  }
}

static void
dump_link(LmnPortRef port, LmnSymbolAtomRef atom, int i, SimpleHashtbl *ht, struct DumpState *s) {
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

static void dump_link_name(LmnPortRef port, int link_num) {
  port_put_raw_s(port, LINK_PREFIX);
  {
    char *s = int_to_str(link_num);
    port_put_raw_s(port, s);
    LMN_FREE(s);
  }
}

static BOOL dump_data_atom(LmnPortRef port, LmnAtomRef data, LmnLinkAttr attr) {
  /* print only data (no link) */
  switch (attr) {
    case LMN_INT_ATTR: {
      char *s = int_to_str((long)data);
      port_put_raw_s(port, s);
      LMN_FREE(s);
    } break;
    case LMN_DBL_ATTR: {
      char buf[64];
      sprintf(buf, "%#g", lmn_get_double((LmnDataAtomRef)data));
      port_put_raw_s(port, buf);
    } break;
    case LMN_SP_ATOM_ATTR: SP_ATOM_DUMP(data, port); break;
    case LMN_HL_ATTR: {
      char buf[18];
      port_put_raw_s(port, EXCLAMATION_NAME);
      if (lmn_env.show_hyperlink) {
        // sprintf(buf, "H%lx",
        sprintf(buf, "H%lu", LMN_HL_ID(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)data)));
      } else {
        // sprintf(buf, "H%lx",
        sprintf(buf, "H%lu", LMN_HL_ID(LMN_HL_ATOM_ROOT_HL((LmnSymbolAtomRef)data)));
      }
      if (LMN_HL_HAS_ATTR(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)data))) {
        port_put_raw_s(port, buf);
        sprintf(buf, ":");
        port_put_raw_s(port, buf);
        dump_hl_attratom(
            port,
            LMN_HL_ATTRATOM(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)data)),
            LMN_HL_ATTRATOM_ATTR(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)data)));
      } else {
        port_put_raw_s(port, buf);
      }
    } break;
    default: lmn_fatal("unexpected attr");
  }
  return TRUE;
}

static BOOL dump_hl_attratom(LmnPortRef port, LmnAtomRef atom, LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA(attr)) {
    dump_data_atom(port, atom, attr);
  } else {  // unary型atomに対する処理
    LmnFunctor f;
    f = ((LmnSymbolAtomRef)atom)->get_functor();

    dump_atomname(port, f);
  }
  return TRUE;
}

static BOOL dump_list(
    LmnPortRef port,
    LmnSymbolAtomRef atom,
    SimpleHashtbl *ht,
    struct DumpState *s,
    int call_depth) {
  BOOL first = TRUE;
  LmnLinkAttr attr;
  LmnAtomRef a = (LmnAtomRef)atom;
  LmnAtomRef prev_a;

  if (get_atomrec(ht, atom)->done) {
    dump_link(port, atom, 2, ht, s);
    return TRUE;
  }

  attr = LMN_ATTR_MAKE_LINK(2); /* 2 is output link position */

  port_put_raw_s(port, "[");

  /* リストの要素を出力していく*/
  while (TRUE) {
    if (LMN_HAS_FUNCTOR((LmnSymbolAtomRef)a, attr, LMN_LIST_FUNCTOR) &&
        LMN_ATTR_GET_VALUE(attr) == 2) {
      struct AtomRec *rec;

      rec = get_atomrec(ht, (LmnSymbolAtomRef)a);

      if (rec->done) { /* cyclic */
        int link = s->link_num++;
        port_put_raw_s(port, "|");
        hashtbl_put(&rec->args, LMN_ATTR_GET_VALUE(attr), link);
        dump_link_name(port, link);
        break;
      }
      rec->done = TRUE;

      if (!first)
        port_put_raw_s(port, ",");
      first = FALSE;

      dump_arg(port, (LmnSymbolAtomRef)a, 0, ht, s, call_depth + 1);

      attr = ((LmnSymbolAtomRef)a)->get_attr(1);

      prev_a = a;
      a = ((LmnSymbolAtomRef)a)->get_link(1);
    } else if (LMN_HAS_FUNCTOR((LmnSymbolAtomRef)a, attr, LMN_NIL_FUNCTOR)) {
      struct AtomRec *rec;
      rec = atomrec_make();
      rec->done = TRUE;
      hashtbl_put(ht, (HashKeyType)a, (HashValueType)rec);
      break;
    } else { /* list ends with non nil data */
      port_put_raw_s(port, "|");

      //      /* 直前の.アトムを取得 */
      //            LmnSymbolAtomRef atom = LMN_SATOM(a->get_link(LMN_ATTR_GET_VALUE(attr)));
      dump_arg(port, (LmnSymbolAtomRef)prev_a, 1, ht, s, call_depth + 1);
      break;
    }
  }
  port_put_raw_s(port, "]");
  return TRUE;
}

/* propagate a link number to connected proxies */
static void
propagate_proxy_link(LmnSymbolAtomRef atom, LmnLinkAttr attr, SimpleHashtbl *ht, int link_num) {
  struct AtomRec *t;
  int i;

  if (LMN_ATTR_IS_DATA(attr))
    return;
  if (atom->get_functor() != LMN_IN_PROXY_FUNCTOR && atom->get_functor() != LMN_OUT_PROXY_FUNCTOR)
    return;
  t = get_atomrec(ht, atom);
  if (t->link_num >= 0)
    return;

  t->link_num = link_num;
  for (i = 0; i < 2; i++) {
    propagate_proxy_link((LmnSymbolAtomRef)atom->get_link(i), atom->get_attr(i), ht, link_num);
  }
}

/* assign a link number to all connected proxies */
static void assign_link_to_proxy(LmnSymbolAtomRef atom, SimpleHashtbl *ht, struct DumpState *s) {
  struct AtomRec *t;

  t = get_atomrec(ht, atom);
  if (t->link_num < 0) {
    int link_num = s->link_num++;
    propagate_proxy_link(atom, LMN_ATTR_MAKE_LINK(0), ht, link_num);
  }
}

static BOOL dump_proxy(
    LmnPortRef port,
    LmnSymbolAtomRef atom,
    SimpleHashtbl *ht,
    int link_pos,
    struct DumpState *s,
    int call_depth) {
  struct AtomRec *t;
  t = get_atomrec(ht, atom);
  t->done = TRUE;

  if (call_depth == 0) {
    LmnLinkAttr attr = atom->get_attr(1);
    if (LMN_ATTR_IS_DATA(attr)) {
      dump_data_atom(port, atom->get_link(1), attr);
      port_put_raw_s(port, "(");
      dump_link_name(port, t->link_num);
      port_put_raw_s(port, ")");
    } else {
      /* symbol atom has dumped */
      return FALSE;
    }
  } else {
    BOOL dumped = FALSE;
    /* outプロキシの接続先である膜の自由リンクが一つで、一引数の'+'アトムに
       接続している場合、膜をその場に出力する */
    if (atom->get_functor() == LMN_OUT_PROXY_FUNCTOR) {
      const LmnSymbolAtomRef in = (LmnSymbolAtomRef)atom->get_link(0);
      if (!LMN_ATTR_IS_DATA(in->get_attr(1)) &&
          ((LmnSymbolAtomRef)in->get_link(1))->get_functor() == LMN_UNARY_PLUS_FUNCTOR) {
        LmnMembraneRef mem = LMN_PROXY_GET_MEM(in);
        if (mem->nfreelinks(1)) {
          get_atomrec(ht, (LmnSymbolAtomRef)in->get_link(1))->done = TRUE;
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

static BOOL dump_symbol_atom(
    LmnPortRef port,
    LmnSymbolAtomRef atom,
    SimpleHashtbl *ht,
    int link_pos,
    struct DumpState *s,
    int call_depth) {
  LmnFunctor f;
  LmnArity arity;
  struct AtomRec *t;

  f = atom->get_functor();
  arity = LMN_FUNCTOR_ARITY(lmn_functor_table, f);
  if (LMN_IS_PROXY_FUNCTOR(f))
    arity--;

  t = get_atomrec(ht, atom);

  if ((call_depth > 0 && link_pos != arity - 1) || /* not last link */
      (call_depth > 0 && t->done) ||               /* already printed */
      call_depth > MAX_DEPTH) {                    /* limit overflow */
    dump_link(port, atom, link_pos, ht, s);
    return TRUE;
  }

  if (t->done)
    return FALSE;

  // トップレベルに cons ('.') が出現した場合は，
  // `[1,2,3|X]=X` のようにイコールアトムで繋いで [] （角括弧） を用いて出力する．
  if (call_depth == 0 && f == LMN_LIST_FUNCTOR) {
    dump_list(port, atom, ht, s, 0);
    port_put_raw_s(port, "=");
    dump_arg(port, atom, 2, ht, s, 1);
    return TRUE;
  }

  t->done = TRUE;

  if (call_depth == 0 && (f == LMN_UNARY_PLUS_FUNCTOR || f == LMN_UNARY_MINUS_FUNCTOR)) {
    port_put_raw_s(port, lmn_id_to_name(LMN_FUNCTOR_NAME_ID(lmn_functor_table, f)));
    return dump_atom(port, atom->get_link(0), ht, atom->get_attr(0), s, 1);
  }
  dump_atomname(port, f);
  dump_atom_args(port, atom, ht, s, call_depth);
  return TRUE;
}

static BOOL dump_atom_args(
    LmnPortRef port,
    LmnSymbolAtomRef atom,
    SimpleHashtbl *ht,
    struct DumpState *s,
    int call_depth) {
  int i;
  int limit = atom->get_link_num();

  if (call_depth > 0)
    limit--;

  if (limit > 0) {
    port_put_raw_s(port, "(");
    for (i = 0; i < limit; i++) {
      if (i > 0)
        port_put_raw_s(port, ",");

      dump_arg(port, atom, i, ht, s, call_depth + 1);
    }
    port_put_raw_s(port, ")");
  }

  return TRUE;
}

static BOOL dump_atom(
    LmnPortRef port,
    LmnAtomRef atom,
    SimpleHashtbl *ht,
    LmnLinkAttr attr,
    struct DumpState *s,
    int call_depth) {
  if (LMN_ATTR_IS_DATA(attr)) {
    return dump_data_atom(port, atom, attr);
  } else {
    LmnSymbolAtomRef a = (LmnSymbolAtomRef)atom;
    LmnFunctor f = a->get_functor();
    int link_pos = LMN_ATTR_GET_VALUE(attr);
    if (!lmn_env.show_proxy && (f == LMN_IN_PROXY_FUNCTOR || f == LMN_OUT_PROXY_FUNCTOR)) {
      return dump_proxy(port, a, ht, link_pos, s, call_depth);
    } else if (f == LMN_LIST_FUNCTOR && link_pos == 2) {
      return dump_list(port, a, ht, s, call_depth);
    } else {
      return dump_symbol_atom(port, a, ht, link_pos, s, call_depth);
    }
  }
}

/* atom must be a symbol atom */
static BOOL
dump_toplevel_atom(LmnPortRef port, LmnSymbolAtomRef atom, SimpleHashtbl *ht, struct DumpState *s) {
  const LmnFunctor f = atom->get_functor();
  if (!lmn_env.show_proxy && (f == LMN_IN_PROXY_FUNCTOR || f == LMN_OUT_PROXY_FUNCTOR)) {
    return dump_proxy(port, atom, ht, 0, s, 0);
  } else {
    return dump_symbol_atom(port, atom, ht, 0, s, 0);
  }
}

static void dump_rule(LmnPortRef port, LmnRuleSetRef rs) {
  unsigned int i, n;

  if (!rs->has_unique())
    return;

  port_put_raw_s(port, "_CHR");

  for (auto r : *rs) {
    /* TODO: uniqはコピー先のルールオブジェクトに名前を設定するため,
     *        コピー元のルールオブジェクトの名前が空になってしまう */
    // if (r->name == ANONYMOUS) continue;

    /* 少なくともCOMMIT命令を1度以上処理したuniqルールを対象に,
     * ルール名と履歴を出力する */
    if (r->history().size() > 0) {
      port_put_raw_s(port, "[id:");
      port_put_raw_s(port, lmn_id_to_name(r->name)); /* ルール名 */
      port_put_raw_s(port, "\"");
      for (auto entry : r->history()) {
        port_put_raw_s(port, " ");
        port_put_raw_s(port, lmn_id_to_name(entry));
      }
      port_put_raw_s(port, "\"]");
    }
  }
}

/* for debug @seiji */
void lmn_dump_rule(LmnPortRef port, LmnRuleSetRef rs) {
  dump_rule(port, rs);
}

static void dump_ruleset(LmnPortRef port, const std::vector<LmnRuleSet *> &v) {
  unsigned int i;

  for (i = 0; i < v.size(); i++) {
    LmnRuleSetRef rs;
    char *s;

    rs = v[i];
    s = int_to_str(rs->id);
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
void lmn_dump_ruleset(LmnPortRef port, const std::vector<LmnRuleSet *> &v) {
  dump_ruleset(port, v);
}

static BOOL
lmn_dump_mem_internal(LmnPortRef port, LmnMembraneRef mem, SimpleHashtbl *ht, struct DumpState *s) {
  if (hashtbl_contains(ht, (HashKeyType)mem))
    return FALSE;

  hashtbl_put(ht, (HashKeyType)mem, (HashValueType)0);

  if (mem->NAME_ID() != ANONYMOUS) {
    port_put_raw_s(port, lmn_id_to_name(mem->NAME_ID()));
  }
  port_put_raw_s(port, "{");
  lmn_dump_cell_internal(port, mem, ht, s);
  port_put_raw_s(port, "}");

  return TRUE;
}

static void lmn_dump_cell_internal(
    LmnPortRef port,
    LmnMembraneRef mem,
    SimpleHashtbl *ht,
    struct DumpState *s) {
  enum { P0, P1, P2, P3, PROXY, PRI_NUM };
  Vector pred_atoms[PRI_NUM];
  AtomListEntryRef ent;
  unsigned int i, j;
  LmnFunctor f;

  if (!mem)
    return;

  /*   if (hashtbl_contains(ht, (HashKeyType)mem)) return; */

  for (i = 0; i < PRI_NUM; i++) {
    pred_atoms[i].init(16);
  }

  /* 優先順位に応じて起点となるアトムを振り分ける */
  EACH_ATOMLIST_WITH_FUNC(
      mem, ent, f, ({
        LmnSymbolAtomRef atom;
        if (LMN_IS_EX_FUNCTOR(f))
          continue;
        EACH_ATOM(atom, ent, ({
                    // fprintf(stderr,"dump_cell_internal: %s/%d Addr[%p] ID[%lu]\n ",
                    // 	atom->str(), atom->get_arity(), atom, atom->get_id());
                    int arity = atom->get_arity();
                    if (atom->get_functor() == LMN_RESUME_FUNCTOR) {
                      continue;
                    }
                    // fprintf(stderr,"dump_cell_internal2:%s/%d\n", atom->str(),
                    // atom->get_arity());
                    if (f == LMN_IN_PROXY_FUNCTOR || f == LMN_OUT_PROXY_FUNCTOR) {
                      pred_atoms[PROXY].push((LmnWord)atom);
                    }
                    /* 0 argument atom */
                    else if (arity == 0) {
                      if (!atom->record_flag) {
                        pred_atoms[P0].push((LmnWord)atom);
                        // fprintf(stderr, "pushed to P0\n");
                      }
                    }
                    /* 1 argument, link to the last argument */
                    else if (
                        arity == 1 && f != LMN_NIL_FUNCTOR &&
                        (LMN_ATTR_IS_DATA(atom->get_attr(0)) ||
                         (int)LMN_ATTR_GET_VALUE(atom->get_attr(0)) ==
                             ((LmnSymbolAtomRef)atom->get_link(0))->get_arity() - 1)) {
                      pred_atoms[P1].push((LmnWord)atom);
                      // fprintf(stderr, "pushed to P1\n");
                    }
                    /* link to the last argument */
                    else {
                      // if文の条件が複雑なのでデバッグ用に残しておく
                      // BOOL c1 = arity > 1;  // should be true
                      // BOOL c2 = c1 && LMN_ATTR_IS_DATA(atom->get_attr(arity - 1));
                      // fprintf(stderr,"case 3: last_attr_is_data=%d, ", c2); fflush(stderr);
                      // BOOL c3 = false;
                      // if (c1 && !c2) {
                      //   int k = (int)LMN_ATTR_GET_VALUE(atom->get_attr(arity - 1));
                      //   fprintf(stderr,"last_attr=%d, ", k); fflush(stderr);
                      //   LmnSymbolAtomRef a = (LmnSymbolAtomRef)atom->get_link(arity - 1);
                      //   fprintf(stderr,"last_atom=%s/%d (%p), ", a->str(), a->get_arity(), a);
                      //   fflush(stderr); fprintf(stderr,"last_atom=%p, ", a); fflush(stderr); int
                      //   l = a->get_arity() - 1; c3 = (k == l);
                      // }
                      if (arity > 1 &&
                          (LMN_ATTR_IS_DATA(atom->get_attr(arity - 1)) ||
                           (int)LMN_ATTR_GET_VALUE(atom->get_attr(arity - 1)) ==
                               ((LmnSymbolAtomRef)atom->get_link(arity - 1))->get_arity() - 1)) {
                        // if (c1 && (c2 || c3)) {
                        pred_atoms[P2].push((LmnWord)atom);
                        // fprintf(stderr, "pushed to P2\n");
                      } else {
                        pred_atoms[P3].push((LmnWord)atom);
                        // fprintf(stderr, "pushed to P3\n");
                      }
                    }
                  }));
      }));

  if (!lmn_env.show_proxy) {
    /* assign link to proxies */
    for (i = 0; i < pred_atoms[PROXY].get_num(); i++) {
      assign_link_to_proxy((LmnSymbolAtomRef)pred_atoms[PROXY].get(i), ht, s);
    }
  }

  { /* dump atoms */
    for (i = 0; i < PRI_NUM; i++) {
      for (j = 0; j < pred_atoms[i].get_num(); j++) {
        LmnSymbolAtomRef atom = (LmnSymbolAtomRef)pred_atoms[i].get(j);
        if (dump_toplevel_atom(port, atom, ht, s)) {
          /* TODO アトムの出力の後には常に ". "が入ってしまう.
             アトムの間に ", "を挟んだ方が見栄えが良い */
          port_put_raw_s(port, ". ");
        }
      }
    }
  }
  for (i = 0; i < PRI_NUM; i++) {
    pred_atoms[i].destroy();
  }

  { /* dump chidren */
    LmnMembraneRef m;
    BOOL dumped = FALSE;
    for (m = mem->mem_child_head(); m; m = m->mem_next()) {
      if (lmn_dump_mem_internal(port, m, ht, s)) {
        dumped = TRUE;
      }
      /* 一回でも出力したことがあって、かつ次回が出力可能ならカンマを打つ */
      if (dumped && m->mem_next() && !hashtbl_contains(ht, (HashKeyType)(m->mem_next()))) {
        port_put_raw_s(port, ", ");
      }
    }
    if (dumped) {
      /* 最後の膜の後に ". "を出力 */
      port_put_raw_s(port, ". ");
    }
  }

  if (lmn_env.show_ruleset) {
    dump_ruleset(port, mem->get_rulesets());
  }
}

static void lmn_dump_cell_nonewline(LmnPortRef port, LmnMembraneRef mem) {
  SimpleHashtbl ht;
  DumpState s;

  hashtbl_init(&ht, 128);
  lmn_dump_cell_internal(port, mem, &ht, &s);

  atomrec_tbl_destroy(&ht);
}

void lmn_dump_cell_stdout(LmnMembraneRef mem) {
  LmnPortRef port = lmn_stdout_port();
  lmn_dump_cell(mem, port);
  port_put_raw_s(port, "\n");
  lmn_port_free(port);
}

void lmn_dump_cell(LmnMembraneRef mem, LmnPortRef port, OutputFormat format) {
  switch (format) {
    case DEFAULT: lmn_dump_cell_nonewline(port, mem); break;
    case DOT: lmn_dump_dot(mem); break;
    case DEV:
      lmn_dump_mem_dev(mem);
      lmn_dump_cell_nonewline(port, mem);  // ueda
      port_put_raw_s(port, "\n");
      break;
    case JSON: lmn_dump_mem_json(mem); break;
    default: lmn_fatal("unexpected."); exit(EXIT_FAILURE);
  }
}

void lmn_dump_cell(LmnMembraneRef mem, LmnPortRef port) {
  lmn_dump_cell(mem, port, lmn_env.output_format);
}

void lmn_dump_mem_stdout(LmnMembraneRef mem) {
  LmnPortRef port = lmn_stdout_port();
  lmn_dump_mem(mem, port);
  port_put_raw_s(port, "\n");
  lmn_port_free(port);
}

/* print membrane structure */
void lmn_dump_mem(LmnMembraneRef mem, LmnPortRef port) {
  switch (lmn_env.output_format) {
    case DEFAULT:
      port_put_raw_s(port, "{");
      lmn_dump_cell_nonewline(port, mem);
      port_put_raw_s(port, "}");
      break;
    case DOT: lmn_dump_dot(mem); break;
    case DEV: lmn_dump_mem_dev(mem); break;
    case JSON: lmn_dump_mem_json(mem); break;
    default: lmn_fatal("unexpected."); exit(EXIT_FAILURE);
  }
}

void dump_atom_dev(LmnSymbolAtomRef atom) {
  LmnFunctor f;
  LmnArity arity;
  unsigned int i;

  f = atom->get_functor();
  arity = LMN_FUNCTOR_ARITY(lmn_functor_table, f);

  esc_code_add(CODE__FORECOLOR_LIGHTBLUE);
  fprintf(
      stdout,
      "Func[%3u], Name[%5s], A[%2u], Addr[%p], ID[%2lu], ",
      f,
      lmn_id_to_name(LMN_FUNCTOR_NAME_ID(lmn_functor_table, f)),
      arity,
      atom,
      atom->get_id());

  if (LMN_FUNC_IS_HL(f)) {
    fprintf(stdout, "HL_OBJ_ID[%2lu], ", LMN_HL_ID(LMN_HL_ATOM_ROOT_HL(atom)));
  }
  esc_code_clear();

  fprintf(stdout, "\n");
  fflush(stdout);

  for (i = 0; i < arity; i++) {
    LmnLinkAttr attr;
    LmnWord id2;

    fprintf(stdout, "   %2u: ", i);
    attr = atom->get_attr(i);
    if (i == 2 && LMN_IS_PROXY_FUNCTOR(f)) { /* membrane */
      fprintf(stdout, "mem[%p], ", (void *)LMN_PROXY_GET_MEM(atom));
    } else if (i == 1 && LMN_FUNC_IS_HL(f)) {
      HyperLink *h = (HyperLink *)atom->get_link(i);
      fprintf(
          stdout,
          " link[HLobj, Addr:%p, HL_ID:%2lu, ROOT_HL_ID:%2lu, "
          "Owner!Addr:%p, Owner'!'ID:%2lu], ",
          h,
          LMN_HL_ID(h),
          LMN_HL_ID(h->get_root()),
          h->hl_to_at(),
          (h->hl_to_at())->get_id());
    } else if (!LMN_ATTR_IS_DATA(attr)) { /* symbol atom */
      fprintf(
          stdout,
          " link[%5d, Addr:%p,    ID:%2lu], ",
          LMN_ATTR_GET_VALUE(attr),
          atom->get_link(i),
          ((LmnSymbolAtomRef)atom->get_link(i))->get_id());
      // checking buddy
      id2 = ((LmnSymbolAtomRef)((LmnSymbolAtomRef)atom->get_link(i))->get_link(atom->get_attr(i)))
                ->get_id();
      fprintf(stdout, "buddy check:%2lu", id2);
      if (id2 != atom->get_id()) {
        fprintf(stdout, " ****ILL-FORMED!!****");
      }
    } else {
      switch (attr) {
        case LMN_INT_ATTR: fprintf(stdout, "int[%lu], ", (LmnWord)atom->get_link(i)); break;
        case LMN_DBL_ATTR:
          fprintf(stdout, "double[%f], ", lmn_get_double((LmnDataAtomRef)atom->get_link(i)));
          break;
        case LMN_HL_ATTR:
          fprintf(
              stdout,
              "hlink[ !, Addr:%lu, ID:%lu], ",
              (LmnWord)atom->get_link(i),
              ((LmnSymbolAtomRef)atom->get_link(i))->get_id());
          // checking buddy
          id2 = ((LmnSymbolAtomRef)((LmnSymbolAtomRef)atom->get_link(i))->get_link(0))->get_id();
          fprintf(stdout, "buddy check:%2lu", id2);
          if (id2 != atom->get_id()) {
            fprintf(stdout, " ****ILL-FORMED!!****");
          }
          break;
        default: fprintf(stdout, "unknown data type[%d], ", attr); break;
      }
    }
    fprintf(stdout, "\n");
    fflush(stdout);
  }

  if (arity == 0)
    fprintf(stdout, "\n");
  fflush(stdout);
}

static void dump_ruleset_dev(const std::vector<LmnRuleSet *> &v) {
  unsigned int i;
  fprintf(stdout, "ruleset[");
  for (i = 0; i < v.size(); i++) {
    fprintf(stdout, "%d ", (v[i])->id);
  }
  fprintf(stdout, "]\n");
}

void lmn_dump_mem_dev(LmnMembraneRef mem) {
  AtomListEntryRef ent;
  if (!mem)
    return;

  fprintf(stdout, "{\n");
  fprintf(stdout, "Mem[%u], Addr[%p], ID[%lu]\n", mem->NAME_ID(), mem, mem->mem_id());
  EACH_ATOMLIST(mem, ent, ({
                  LmnSymbolAtomRef atom;
                  EACH_ATOM(atom, ent, ({ dump_atom_dev(atom); }));
                }));

  dump_ruleset_dev(mem->get_rulesets());
  lmn_dump_mem_dev(mem->mem_child_head());
  fprintf(stdout, "}\n");
  lmn_dump_mem_dev(mem->mem_next());
}

/*----------------------------------------------------------------------
 * dump dot
 */

static void dump_dot_cell(LmnMembraneRef mem, SimpleHashtbl *ht, int *data_id, int *cluster_id) {
  AtomListEntryRef ent;
  LmnMembraneRef m;
  LmnPortRef out;
  unsigned int i;

  if (!mem)
    return;

  out = lmn_stdout_port();

  /* dump node labels */
  EACH_ATOMLIST(mem, ent, ({
                  LmnSymbolAtomRef atom;
                  EACH_ATOM(atom, ent, ({
                              fprintf(stdout, "%lu [label = \"", (LmnWord)atom);
                              dump_atomname(out, atom->get_functor());
                              fprintf(stdout, "\", shape = circle];\n");
                              for (i = 0; i < LMN_FUNCTOR_GET_LINK_NUM(atom->get_functor()); i++) {
                                LmnLinkAttr attr = atom->get_attr(i);
                                if (LMN_ATTR_IS_DATA(attr)) {
                                  fprintf(stdout, "%lu [label = \"", (LmnWord)atom->get_plink(i));
                                  dump_data_atom(out, atom->get_link(i), attr);
                                  fprintf(stdout, "\", shape = box];\n");
                                }
                              }
                            }));
                }));

  /* dump connections */
  EACH_ATOMLIST(mem, ent, ({
                  LmnSymbolAtomRef atom;
                  EACH_ATOM(atom, ent, ({
                              struct AtomRec *ar =
                                  (struct AtomRec *)hashtbl_get_default(ht, (HashKeyType)atom, 0);
                              unsigned int arity = LMN_FUNCTOR_GET_LINK_NUM(atom->get_functor());

                              for (i = 0; i < arity; i++) {
                                LmnLinkAttr attr = atom->get_attr(i);

                                if (ar && hashtbl_contains(&ar->args, i))
                                  continue;
                                fprintf(stdout, "%lu -- ", (LmnWord)atom);
                                if (LMN_ATTR_IS_DATA(attr)) {
                                  fprintf(stdout, " %lu", (LmnWord)atom->get_plink(i));
                                  (*data_id)++;
                                } else { /* symbol atom */
                                  struct AtomRec *ar;
                                  LmnAtomRef atom2 = atom->get_link(i);
                                  if (hashtbl_contains(ht, (HashKeyType)atom2)) {
                                    ar = (struct AtomRec *)hashtbl_get(ht, (HashKeyType)atom2);
                                  } else {
                                    ar = atomrec_make();
                                    hashtbl_put(ht, (HashKeyType)atom2, (HashValueType)ar);
                                  }
                                  hashtbl_put(&ar->args, LMN_ATTR_GET_VALUE(attr), 1);
                                  fprintf(stdout, "%lu", (LmnWord)atom2);
                                }
                                fprintf(stdout, "\n");
                              }
                            }));
                }));

  /* dump chidren */
  for (m = mem->mem_child_head(); m; m = m->mem_next()) {
    fprintf(stdout, "subgraph cluster%d {\n", *cluster_id);
    (*cluster_id)++;
    dump_dot_cell(m, ht, data_id, cluster_id);
    fprintf(stdout, "}\n");
  }
}

void lmn_dump_dot(LmnMembraneRef mem) {
  int cluster_id = 0, data_id = 0;
  DumpState s;
  SimpleHashtbl ht;

  hashtbl_init(&ht, 128);

  fprintf(
      stdout,
      "// This is an auto generated file by SLIM\n\n"
      "graph {\n"
      "node "
      "[bgcolor=\"trasnparent\",truecolor=true,color=\"#000000\","
      "style=filled,fillcolor=\"#ffd49b50\"];\n"
      "edge [color=\"#000080\"];\n"

  );

  dump_dot_cell(mem, &ht, &data_id, &cluster_id);

  fprintf(stdout, "}\n");

  atomrec_tbl_destroy(&ht);
}

static void lmn_dump_link_json(LmnSymbolAtomRef atom, int index) {
  LmnLinkAttr attr;
  void *data;

  attr = atom->get_attr(index);
  data = (void *)atom->get_link(index);

  fprintf(stdout, "{");
  fprintf(stdout, "\"attr\":%d,", (int)attr);

  if (LMN_ATTR_IS_DATA(attr)) {
    switch (attr) {
      case LMN_INT_ATTR: fprintf(stdout, "\"data\":%d", (int)((LmnWord)data)); break;
      case LMN_DBL_ATTR:
      case LMN_CONST_DBL_ATTR:
        fprintf(stdout, "\"data\":%f", lmn_get_double((LmnDataAtomRef)data));
        break;
      case LMN_SP_ATOM_ATTR:
      case LMN_CONST_STR_ATTR:
        fprintf(stdout, "\"data\":\"\\\"%s\\\"\"", ((LmnStringRef)data)->c_str());
        break;
      case LMN_HL_ATTR: {
        LmnSymbolAtomRef a = (LmnSymbolAtomRef)data;
        HyperLink *root = LMN_HL_ATOM_ROOT_HL(a);
        fprintf(stdout, "\"data\":%d", (int)root->id);
      } break;
      default: break;
    }
  } else {
    LmnSymbolAtomRef a = (LmnSymbolAtomRef)data;
    if (a != NULL) {
      fprintf(stdout, "\"data\":%d", (int)a->get_id());
    }
  }
  fprintf(stdout, "}");
}

static void lmn_dump_atom_json(LmnSymbolAtomRef atom) {
  int i;
  int arity;
  fprintf(stdout, "{");
  fprintf(stdout, "\"id\":%d,", (int)atom->get_id());
  fprintf(stdout, "\"name\":\"%s\",", atom->str());
  fprintf(stdout, "\"links\":[");
  {
    BOOL needs_comma = FALSE;
    for (i = 0, arity = atom->get_link_num(); i < arity; i++) {
      if (needs_comma)
        fprintf(stdout, ",");
      needs_comma = TRUE;
      lmn_dump_link_json(atom, i);
    }
  }
  fprintf(stdout, "]");
  fprintf(stdout, "}");
}

static void lmn_dump_mem_json(LmnMembraneRef mem) {
  if (!mem)
    return;

  fprintf(stdout, "{");
  fprintf(stdout, "\"id\":%d,", (int)mem->mem_id());
  fprintf(stdout, "\"name\":\"%s\",", mem->MEM_NAME());
  fprintf(stdout, "\"atoms\":[");
  {
    AtomListEntryRef ent;
    LmnFunctor f;
    BOOL needs_comma = FALSE;
    EACH_ATOMLIST_WITH_FUNC(mem, ent, f, ({
                              LmnSymbolAtomRef atom;
                              if (LMN_IS_EX_FUNCTOR(f)) {
                                continue;
                              }
                              EACH_ATOM(atom, ent, ({
                                          if (needs_comma)
                                            fprintf(stdout, ",");
                                          needs_comma = TRUE;
                                          lmn_dump_atom_json(atom);
                                        }));
                            }));
  }
  fprintf(stdout, "],");
  fprintf(stdout, "\"membranes\":[");
  {
    LmnMembraneRef m;
    BOOL needs_comma = FALSE;
    for (m = mem->mem_child_head(); m; m = m->mem_next()) {
      if (needs_comma)
        fprintf(stdout, ",");
      needs_comma = TRUE;
      lmn_dump_mem_json(m);
    }
  }
  fprintf(stdout, "]");

  fprintf(stdout, "}");
}

void cb_dump_mem(
    LmnReactCxtRef rc,
    LmnMembraneRef mem,
    LmnAtomRef a0,
    LmnLinkAttr t0,
    LmnAtomRef a1,
    LmnLinkAttr t1,
    LmnAtomRef a2,
    LmnLinkAttr t2) {
  LmnSymbolAtomRef in = (LmnSymbolAtomRef)((LmnSymbolAtomRef)a1)->get_link(0);
  LmnMembraneRef m = LMN_PROXY_GET_MEM(in);

  lmn_mem_delete_atom(m, in->get_link(1), in->get_attr(1));
  lmn_mem_delete_atom(m, in, ((LmnSymbolAtomRef)a1)->get_attr(0));
  lmn_mem_delete_atom(mem, a1, t1);

  lmn_dump_mem(m, LMN_PORT(a0));

  lmn_mem_newlink(mem, a0, t0, 0, a2, t2, LMN_ATTR_GET_VALUE(t2));

  if (rc->has_mode(REACT_MEM_ORIENTED)) {
    ((MemReactContext *)rc)->memstack_remove(m);
  }
  (m->mem_parent())->delete_mem(m);
}

void dumper_init() {
  CCallback::lmn_register_c_fun("cb_dump_mem", (void *)cb_dump_mem, 3);
}

void dumper_finalize() {
}

void dump_escaped(LmnPortRef port, const char *s) {
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

void lmn_dump_atom(LmnPortRef port, LmnAtomRef atom, LmnLinkAttr attr) {
  struct DumpState s;
  SimpleHashtbl ht;

  hashtbl_init(&ht, 0);
  dump_atom(port, atom, &ht, attr, &s, 0);
  atomrec_tbl_destroy(&ht);
}

namespace slim {
  static int output_format_index = std::ios_base::xalloc();

  namespace format {
    std::ostream &env(std::ostream &os) {
      os.iword(output_format_index) = 0;
      return os;
    }

    std::ostream &lmntal(std::ostream &os) {
      os.iword(output_format_index) = OutputFormat::DEFAULT;
      return os;
    }

    std::ostream &verbal(std::ostream &os) {
      os.iword(output_format_index) = OutputFormat::DEV;
      return os;
    }

    std::ostream &dot(std::ostream &os) {
      os.iword(output_format_index) = OutputFormat::DOT;
      return os;
    }

    std::ostream &json(std::ostream &os) {
      os.iword(output_format_index) = OutputFormat::JSON;
      return os;
    }
  }  // namespace format

  // TODO: portを使わないようにする
  std::string to_string(const LmnMembrane *mem, OutputFormat format) {
    auto port = lmn_make_output_string_port();
    lmn_dump_cell(const_cast<LmnMembrane *>(mem), port, format);
    auto str = std::unique_ptr<LmnString>(lmn_port_output_string(port));
    lmn_port_free(port);
    return str->str;
  }

  std::string to_string(const LmnMembrane *mem) {
    return to_string(mem, lmn_env.output_format);
  }

  // TODO: incrementalに作る
  void dump_mem(std::ostream &os, const LmnMembrane *mem) {
    if (os.iword(output_format_index) == 0)
      os << to_string(mem);
    else
      os << to_string(mem, (OutputFormat)os.iword(output_format_index));
  }
}  // namespace slim
