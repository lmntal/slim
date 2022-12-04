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

#include "stringifier.hpp"
#include "atomlist.hpp"
#include "rule.hpp"
#include "symbol.h"
#include <ctype.h>

#include <ios>
#include <ostream>

#include <iostream>
#include <sstream>

constexpr int MAX_DEPTH = 1000;
constexpr char LINK_PREFIX[] = "L";

namespace slim {
namespace stringifier {

struct AtomRec {
  BOOL done;
  SimpleHashtbl args;
  int link_num; /* 一連のプロキシに割り当てられた番号, proxy only */
};

struct DumpState {
  int link_num;
  DumpState();
};

static std::string stringify_atom(LmnAtomRef atom, SimpleHashtbl *ht,
                      LmnLinkAttr attr, struct DumpState *s, int call_depth);
static std::string lmn_stringify_cell_internal(LmnMembraneRef mem,
                                   SimpleHashtbl *ht, struct DumpState *s);

static std::string stringify_link(LmnSymbolAtomRef atom, int i,
                      SimpleHashtbl *ht, struct DumpState *s);
static std::string lmn_stringify_mem_internal(LmnMembraneRef mem,
                                  SimpleHashtbl *ht, struct DumpState *s);

static std::string stringify_atom_args(LmnSymbolAtomRef atom,
                           SimpleHashtbl *ht, struct DumpState *s,
                           int call_depth);
static std::string stringify_link_name(int link_num);
static std::string stringify_hl_attratom(LmnAtomRef atom,
                             LmnLinkAttr attr);

static std::string lmn_stringify_atom_json(LmnSymbolAtomRef atom);
static std::string lmn_stringify_link_json(LmnSymbolAtomRef atom, int index);
static std::string lmn_stringify_mem_json(LmnMembraneRef mem);

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
  for (iter = hashtbl_iterator(ht); !hashtbliter_isend(&iter);
       hashtbliter_next(&iter)) {
    if (hashtbliter_entry(&iter)->data) {
      atomrec_free((struct AtomRec *)hashtbliter_entry(&iter)->data);
    }
  }
  hashtbl_destroy(ht);
}

// static void dump_state_init(struct DumpState *s) { ; }
DumpState::DumpState() { link_num = 0; }

static BOOL is_direct_printable(LmnFunctor f) {
  const char *s;

  if (LMN_IS_PROXY_FUNCTOR(f) || f == LMN_NIL_FUNCTOR)
    return TRUE;

  s = LMN_FUNCTOR_STR(f);
  if (!(isalpha((unsigned char)*s) && islower((unsigned char)*s)))
    return FALSE;
  while (*(++s)) {
    if (!(isalpha((unsigned char)*s) || isdigit((unsigned char)*s) ||
          *s == '_'))
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

static std::string stringify_atomname(LmnFunctor f) {
  std::ostringstream retVal;
  /* dump module name */
  if (LMN_FUNCTOR_MODULE_ID(lmn_functor_table, f) != ANONYMOUS) {
    retVal << lmn_id_to_name(LMN_FUNCTOR_MODULE_ID(lmn_functor_table, f));
    retVal << ".";
  }

  /* dump atom name */
  {
    const char *atom_name = lmn_id_to_name(LMN_FUNCTOR_NAME_ID(lmn_functor_table, f));

    if (is_direct_printable(f)) {
      retVal << atom_name;
    } else {
      retVal << "'" << stringify_escaped(atom_name) << "'";
    }
  }

  return retVal.str();
}

static std::string stringify_arg(LmnSymbolAtomRef atom, int i,
                     SimpleHashtbl *ht, struct DumpState *s, int call_depth) {
  std::ostringstream retVal;
  struct AtomRec *rec;
  rec = get_atomrec(ht, atom);

  if (hashtbl_contains(&rec->args, i)) {
    retVal << stringify_link(atom, i, ht, s);
  } else {
    retVal << stringify_atom(atom->get_link(i), ht,
              atom->get_attr(i), s, call_depth + 1);
  }

  return retVal.str();
}

static std::string stringify_link(LmnSymbolAtomRef atom, int i,
                      SimpleHashtbl *ht, struct DumpState *s) {
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
  return stringify_link_name(link);
}

static std::string stringify_link_name(int link_num) {
  std::ostringstream retVal;
  retVal << LINK_PREFIX << (int) link_num;
  return retVal.str();
}

static std::string stringify_data_atom(LmnAtomRef data, LmnLinkAttr attr) {
  std::ostringstream retVal;
  /* print only data (no link) */
  switch (attr) {
  case LMN_INT_ATTR: {
    retVal << (long)data;
  } break;
  case LMN_DBL_ATTR: {
    char buf[64];
    retVal << lmn_get_double((LmnDataAtomRef)data);
  } break;
  case LMN_SP_ATOM_ATTR: {
    // TODO: do not use port
    // taken from slim::to_string(const LmnMembrane*, OutputFormat) at src/vm/dumper.cpp
    LmnPortRef tmp_port = lmn_make_output_string_port();
    SP_ATOM_DUMP(data, tmp_port);
    LmnStringRef str_ref = lmn_port_output_string(tmp_port);
    lmn_port_free(tmp_port);
    retVal << str_ref->str;
    }
    break;
  case LMN_HL_ATTR: {
    char buf[18];
    retVal << EXCLAMATION_NAME << "H";
    if (lmn_env.show_hyperlink) {
      retVal << (unsigned long) LMN_HL_ID(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)data));
    } else {
      retVal << LMN_HL_ID(LMN_HL_ATOM_ROOT_HL((LmnSymbolAtomRef)data));
    }
    if (LMN_HL_HAS_ATTR(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)data))) {
      retVal << ":";
      retVal << stringify_hl_attratom(
            LMN_HL_ATTRATOM(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)data)),
            LMN_HL_ATTRATOM_ATTR(lmn_hyperlink_at_to_hl((LmnSymbolAtomRef)data))
                );
    }
  } break;
  default:
    lmn_fatal("unexpected attr");
  }
  return retVal.str();
}

static std::string stringify_hl_attratom(LmnAtomRef atom,
                             LmnLinkAttr attr) {
  if (LMN_ATTR_IS_DATA(attr)) {
    return stringify_data_atom(atom, attr);
  } else { // unary型atomに対する処理
    LmnFunctor f;
    f = ((LmnSymbolAtomRef)atom)->get_functor();
    return stringify_atomname(f);
  }
}

static std::string stringify_list(LmnSymbolAtomRef atom, SimpleHashtbl *ht,
                      struct DumpState *s, int call_depth) {
  std::ostringstream retVal;
  BOOL first = TRUE;
  LmnLinkAttr attr;
  LmnAtomRef a = (LmnAtomRef)atom;
  LmnAtomRef prev_a;

  if (get_atomrec(ht, atom)->done) {
    retVal << stringify_link(atom, 2, ht, s);
    return retVal.str();
  }

  attr = LMN_ATTR_MAKE_LINK(2); /* 2 is output link position */

  retVal << "[";

  /* リストの要素を出力していく*/
  while (TRUE) {
    if (LMN_HAS_FUNCTOR((LmnSymbolAtomRef)a, attr, LMN_LIST_FUNCTOR) &&
        LMN_ATTR_GET_VALUE(attr) == 2) {
      struct AtomRec *rec;

      rec = get_atomrec(ht, (LmnSymbolAtomRef)a);

      if (rec->done) { /* cyclic */
        int link = s->link_num++;
        retVal << "|";
        hashtbl_put(&rec->args, LMN_ATTR_GET_VALUE(attr), link);
        retVal << stringify_link_name(link);
        break;
      }
      rec->done = TRUE;

      if (!first)
        retVal << ",";
      first = FALSE;

      retVal << stringify_arg((LmnSymbolAtomRef)a, 0, ht, s, call_depth + 1);

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
      retVal << "|";

      //      /* 直前の.アトムを取得 */
      //            LmnSymbolAtomRef atom = LMN_SATOM(a->get_link(LMN_ATTR_GET_VALUE(attr)));
      retVal << stringify_arg((LmnSymbolAtomRef)prev_a, 1, ht, s, call_depth + 1);
      break;
    }
  }
  retVal << "]";
  return retVal.str();
}

/* propagate a link number to connected proxies */
static void propagate_proxy_link(LmnSymbolAtomRef atom, LmnLinkAttr attr,
                                 SimpleHashtbl *ht, int link_num) {
  struct AtomRec *t;
  int i;

  if (LMN_ATTR_IS_DATA(attr))
    return;
  if (atom->get_functor() != LMN_IN_PROXY_FUNCTOR &&
      atom->get_functor() != LMN_OUT_PROXY_FUNCTOR)
    return;
  t = get_atomrec(ht, atom);
  if (t->link_num >= 0)
    return;

  t->link_num = link_num;
  for (i = 0; i < 2; i++) {
    propagate_proxy_link((LmnSymbolAtomRef)atom->get_link(i),
                         atom->get_attr(i), ht, link_num);
  }
}

/* assign a link number to all connected proxies */
static void assign_link_to_proxy(LmnSymbolAtomRef atom, SimpleHashtbl *ht,
                                 struct DumpState *s) {
  struct AtomRec *t;

  t = get_atomrec(ht, atom);
  if (t->link_num < 0) {
    int link_num = s->link_num++;
    propagate_proxy_link(atom, LMN_ATTR_MAKE_LINK(0), ht, link_num);
  }
}

static std::string stringify_proxy(LmnSymbolAtomRef atom,
                       SimpleHashtbl *ht, int link_pos, struct DumpState *s,
                       int call_depth) {
  std::ostringstream retVal;
  struct AtomRec *t;
  t = get_atomrec(ht, atom);
  t->done = TRUE;

  if (call_depth == 0) {
    LmnLinkAttr attr = atom->get_attr(1);
    if (LMN_ATTR_IS_DATA(attr)) {
      retVal << stringify_data_atom(atom->get_link(1), attr);
      retVal << "(";
      retVal << stringify_link_name(t->link_num);
      retVal << ")";
      return retVal.str();
    } else {
      /* symbol atom has dumped */
      return "";
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
          get_atomrec(ht, (LmnSymbolAtomRef)in->get_link(1))->done =
              TRUE;
          retVal << lmn_stringify_mem_internal(mem, ht, s);
          dumped = TRUE;
        }
      }
    }
    if (!dumped) {
      retVal << stringify_link_name(t->link_num);
    }
  }
  return retVal.str();
}

static std::string stringify_symbol_atom(LmnSymbolAtomRef atom,
                             SimpleHashtbl *ht, int link_pos,
                             struct DumpState *s, int call_depth) {
  std::ostringstream retVal;
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
    retVal << stringify_link(atom, link_pos, ht, s);
    return retVal.str();
  }

  if (t->done)
    return retVal.str();

  // トップレベルに cons ('.') が出現した場合は，
  // `[1,2,3|X]=X` のようにイコールアトムで繋いで [] （角括弧） を用いて出力する．
  if (call_depth == 0 &&
      f == LMN_LIST_FUNCTOR) {
    retVal << stringify_list(atom, ht, s, 0);
    retVal << "=";
    retVal << stringify_arg(atom, 2, ht, s, 1);
    return retVal.str();
  }

  t->done = TRUE;

  if (call_depth == 0 &&
      (f == LMN_UNARY_PLUS_FUNCTOR || f == LMN_UNARY_MINUS_FUNCTOR)) {
    retVal << lmn_id_to_name(LMN_FUNCTOR_NAME_ID(lmn_functor_table, f));
    retVal << stringify_atom(atom->get_link(0), ht,
                     atom->get_attr(0), s, 1);
    return retVal.str();
  }
  retVal << stringify_atomname(f);
  retVal << stringify_atom_args(atom, ht, s, call_depth);
  return retVal.str();
}

static std::string stringify_atom_args(LmnSymbolAtomRef atom,
                           SimpleHashtbl *ht, struct DumpState *s,
                           int call_depth) {
  std::ostringstream retVal;
  int i;
  int limit = atom->get_link_num();

  if (call_depth > 0)
    limit--;

  if (limit > 0) {
    retVal << "(";
    for (i = 0; i < limit; i++) {
      if (i > 0)
        retVal << ",";

      retVal << stringify_arg(atom, i, ht, s, call_depth + 1);
    }
    retVal << ")";
  }

  return retVal.str();
}

static std::string stringify_atom(LmnAtomRef atom, SimpleHashtbl *ht,
                      LmnLinkAttr attr, struct DumpState *s, int call_depth) {
  if (LMN_ATTR_IS_DATA(attr)) {
    return stringify_data_atom(atom, attr);
  } else {
    LmnSymbolAtomRef a = (LmnSymbolAtomRef)atom;
    LmnFunctor f = a->get_functor();
    int link_pos = LMN_ATTR_GET_VALUE(attr);
    if (!lmn_env.show_proxy &&
        (f == LMN_IN_PROXY_FUNCTOR || f == LMN_OUT_PROXY_FUNCTOR)) {
      return stringify_proxy(a, ht, link_pos, s, call_depth);
    } else if (f == LMN_LIST_FUNCTOR && link_pos == 2) {
      return stringify_list(a, ht, s, call_depth);
    } else {
      return stringify_symbol_atom(a, ht, link_pos, s, call_depth);
    }
  }
}

/* atom must be a symbol atom */
static std::string stringify_toplevel_atom(LmnSymbolAtomRef atom,
                               SimpleHashtbl *ht, struct DumpState *s) {
  const LmnFunctor f = atom->get_functor();
  if (!lmn_env.show_proxy &&
      (f == LMN_IN_PROXY_FUNCTOR || f == LMN_OUT_PROXY_FUNCTOR)) {
    return stringify_proxy(atom, ht, 0, s, 0);
  } else {
    return stringify_symbol_atom(atom, ht, 0, s, 0);
  }
}

static std::string stringify_rule(LmnRuleSetRef rs) {
  std::ostringstream retVal;
  unsigned int i, n;

  if (!rs->has_unique())
    return "";

  retVal << "_CHR";

  for (auto r : *rs) {
    /* TODO: uniqはコピー先のルールオブジェクトに名前を設定するため,
     *        コピー元のルールオブジェクトの名前が空になってしまう */
    // if (r->name == ANONYMOUS) continue;

    /* 少なくともCOMMIT命令を1度以上処理したuniqルールを対象に,
     * ルール名と履歴を出力する */
    if (r->history().size() > 0) {
      retVal << "[id:";
      retVal << lmn_id_to_name(r->name); /* ルール名 */
      retVal << "\"";
      for (auto entry : r->history()) {
        retVal << " ";
        retVal << lmn_id_to_name(entry);
      }
      retVal << "\"]";
    }
  }
  return retVal.str();
}

/* for debug @seiji */
std::string lmn_stringify_rule(LmnRuleSetRef rs) { return stringify_rule(rs); }

static std::string stringify_ruleset(const std::vector<LmnRuleSet *> &v) {
  std::ostringstream retVal;
  unsigned int i;

  for (i = 0; i < v.size(); i++) {
    LmnRuleSetRef rs;
    char *s;

    rs = v[i];
    if (lmn_env.sp_dump_format == LMN_SYNTAX) {
      if (i > 0) {
        retVal << ",";
      }
      retVal << "'";
    }

    retVal << "@";
    retVal << (long) (rs->id);
    if (lmn_env.show_chr) {
      retVal << stringify_rule(rs);
    }

    if (lmn_env.sp_dump_format == LMN_SYNTAX) {
      retVal << "'";
    }
    retVal << ". ";
  }

  return retVal.str();
}

/* for debug @seiji */
std::string lmn_stringify_ruleset(const std::vector<LmnRuleSet *> &v) {
  return stringify_ruleset(v);
}

static std::string lmn_stringify_mem_internal(LmnMembraneRef mem,
                                  SimpleHashtbl *ht, struct DumpState *s) {
  std::ostringstream retVal;
  if (hashtbl_contains(ht, (HashKeyType)mem))
    return "";

  hashtbl_put(ht, (HashKeyType)mem, (HashValueType)0);

  if (mem->NAME_ID() != ANONYMOUS) {
    retVal << lmn_id_to_name(mem->NAME_ID());
  }
  retVal << "{";
  retVal << lmn_stringify_cell_internal(mem, ht, s);
  retVal << "}";

  return retVal.str();
}

static std::string lmn_stringify_cell_internal(LmnMembraneRef mem,
                                   SimpleHashtbl *ht, struct DumpState *s) {
  std::ostringstream retVal;
  enum { P0, P1, P2, P3, PROXY, PRI_NUM };
  Vector pred_atoms[PRI_NUM];
  AtomListEntryRef ent;
  unsigned int i, j;
  LmnFunctor f;

  if (!mem)
    return "";

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
        EACH_ATOM(
            atom, ent, ({
		// fprintf(stderr,"dump_cell_internal: %s/%d Addr[%p] ID[%lu]\n ",
		// 	atom->str(), atom->get_arity(), atom, atom->get_id());
              int arity = atom->get_arity();
              if (atom->get_functor() == LMN_RESUME_FUNCTOR) {
                continue;
	      }
	      // fprintf(stderr,"dump_cell_internal2:%s/%d\n", atom->str(), atom->get_arity());
              if (f == LMN_IN_PROXY_FUNCTOR || f == LMN_OUT_PROXY_FUNCTOR) {
                pred_atoms[PROXY].push((LmnWord)atom);
              }
              /* 0 argument atom */
              else if (arity == 0) {
		if(!atom->record_flag) {
		  pred_atoms[P0].push((LmnWord)atom);
		  // fprintf(stderr, "pushed to P0\n");
		}
              }
              /* 1 argument, link to the last argument */
              else if (arity == 1 && f != LMN_NIL_FUNCTOR &&
                       (LMN_ATTR_IS_DATA(atom->get_attr(0)) ||
                        (int)LMN_ATTR_GET_VALUE(atom->get_attr(0)) ==
                            ((LmnSymbolAtomRef)atom->get_link(0))->get_arity() -
                                1)) {
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
		//   fprintf(stderr,"last_atom=%s/%d (%p), ", a->str(), a->get_arity(), a); fflush(stderr);
		//   fprintf(stderr,"last_atom=%p, ", a); fflush(stderr);
		//   int l = a->get_arity() - 1;
		//   c3 = (k == l);
		// }
		if (arity > 1 &&
                       (LMN_ATTR_IS_DATA(atom->get_attr(arity - 1)) ||
                        (int)LMN_ATTR_GET_VALUE(
                            atom->get_attr(arity - 1)) ==
                            ((LmnSymbolAtomRef)atom->get_link(arity - 1))->get_arity() -
                                1)) {
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
      assign_link_to_proxy((LmnSymbolAtomRef)pred_atoms[PROXY].get(i), ht,
                           s);
    }
  }

  { /* dump atoms */
    for (i = 0; i < PRI_NUM; i++) {
      for (j = 0; j < pred_atoms[i].get_num(); j++) {
        LmnSymbolAtomRef atom = (LmnSymbolAtomRef)pred_atoms[i].get(j);
        retVal << stringify_toplevel_atom(atom, ht, s);
        // TODO: inspect
        // if (dump_toplevel_atom(port, atom, ht, s)) {
          retVal << ". ";
        // }
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
      std::string mem_string_rep = lmn_stringify_mem_internal(m, ht, s);
      retVal << mem_string_rep;
      if (!mem_string_rep.empty()) {
        dumped = TRUE;
      }
      /* 一回でも出力したことがあって、かつ次回が出力可能ならカンマを打つ */
      if (dumped && m->mem_next() &&
          !hashtbl_contains(ht, (HashKeyType)(m->mem_next()))) {
        retVal << ", ";
      }
    }
    if (dumped) {
      /* 最後の膜の後に ". "を出力 */
      retVal << ". ";
    }
  }

  if (lmn_env.show_ruleset) {
    retVal << stringify_ruleset(mem->get_rulesets());
  }

  return retVal.str();
}

static std::string lmn_stringify_cell_nonewline(LmnMembraneRef mem) {
  SimpleHashtbl ht;
  DumpState s;

  hashtbl_init(&ht, 128);
  std::string retVal = lmn_stringify_cell_internal(mem, &ht, &s);

  atomrec_tbl_destroy(&ht);
  return retVal;
}

std::string lmn_stringify_cell(LmnMembraneRef mem, OutputFormat format) {
  std::ostringstream retVal;
  switch (format) {
  case DEFAULT:
    retVal << lmn_stringify_cell_nonewline(mem);
    break;
  case DOT:
    retVal << lmn_stringify_dot(mem);
    break;
  case DEV:
    retVal << lmn_stringify_mem_dev(mem);
    retVal << lmn_stringify_cell_nonewline(mem); // ueda
    retVal << "\n";
    break;
  case JSON:
    retVal << lmn_stringify_mem_json(mem);
    break;
  default:
    lmn_fatal("unexpected.");
    exit(EXIT_FAILURE);
  }
  return retVal.str();
}

std::string lmn_stringify_cell(LmnMembraneRef mem) {
  return lmn_stringify_cell(mem, lmn_env.output_format);
}

/* print membrane structure */
std::string lmn_stringify_mem(LmnMembraneRef mem) {
  std::ostringstream retVal;
  switch (lmn_env.output_format) {
  case DEFAULT:
    retVal << "{ ";
    retVal << lmn_stringify_cell_nonewline(mem);
    retVal << "}";
    break;
  case DOT:
    retVal << lmn_stringify_dot(mem);
    break;
  case DEV:
    retVal << lmn_stringify_mem_dev(mem);
    break;
  case JSON:
    retVal << lmn_stringify_mem_json(mem);
    break;
  default:
    lmn_fatal("unexpected.");
    exit(EXIT_FAILURE);
  }
  return retVal.str();
}

std::string stringify_atom_dev(LmnSymbolAtomRef atom) {
  std::ostringstream retVal;
  LmnFunctor f;
  LmnArity arity;
  unsigned int i;

  f = atom->get_functor();
  arity = LMN_FUNCTOR_ARITY(lmn_functor_table, f);

  retVal << __ESC_START__ << CODE__FORECOLOR_LIGHTBLUE << __ESC_END__;
  retVal << "Func[" << (unsigned int) f << "], ";
  retVal << "Name[" << lmn_id_to_name(LMN_FUNCTOR_NAME_ID(lmn_functor_table, f)) << "], ";
  retVal << "A[" << (unsigned int) arity << "], ";
  retVal << "Addr[" << std::hex << atom << std::dec << "], ";
  retVal << "ID[" << (unsigned long) atom->get_id() << "], ";

  if (LMN_FUNC_IS_HL(f)) {
    retVal << "HL_OBJ_ID[" << (unsigned long) LMN_HL_ID(LMN_HL_ATOM_ROOT_HL(atom)) << "], ";
  }
  retVal << __ESC_START__ << __ESC_END__ << "\n";

  for (i = 0; i < arity; i++) {
    LmnLinkAttr attr;
    LmnWord id2;

    retVal << "   " << i << ": ";
    attr = atom->get_attr(i);
    if (i == 2 && LMN_IS_PROXY_FUNCTOR(f)) { /* membrane */
      retVal << "mem[" << std::hex << LMN_PROXY_GET_MEM(atom) << std::dec << "], ";
    } else if (i == 1 && LMN_FUNC_IS_HL(f)) {
      HyperLink *h = (HyperLink *)atom->get_link(i);
      retVal << " link[HLobj, Addr:" << std::hex << h << std::dec << ", ";
      retVal << "HL_ID:" << (unsigned long) LMN_HL_ID(h) << ", ";
      retVal << "ROOT_HL_ID:" << (unsigned long) LMN_HL_ID(h->get_root()) << ", ";
      retVal << "Owner!Addr:" << std::hex << h->hl_to_at() << std::dec << ", ";
      retVal << "Owner'!'ID:" << (unsigned long) (h->hl_to_at())->get_id() << "], ";
    } else if (!LMN_ATTR_IS_DATA(attr)) { /* symbol atom */
      retVal << " link[" << LMN_ATTR_GET_VALUE(attr) << ", ";
      retVal << "Addr:" << std::hex << atom->get_link(i) << std::dec << ",    ";
      retVal << "ID:" << (unsigned long) ((LmnSymbolAtomRef)atom->get_link(i))->get_id() << "], ";
      // checking buddy
      id2 = ((LmnSymbolAtomRef)
	     ((LmnSymbolAtomRef)atom->get_link(i))->get_link(atom->get_attr(i)))->get_id();
      retVal << "buddy check:" << (unsigned long) id2;
      if (id2 != atom->get_id()) {
        retVal << " ****ILL-FORMED!!****";
      }
    } else {
      switch (attr) {
      case LMN_INT_ATTR:
        retVal << "int[" << (unsigned long)atom->get_link(i) << "], ";
        break;
      case LMN_DBL_ATTR:
        retVal << "double[" << lmn_get_double((LmnDataAtomRef)atom->get_link(i)) << "], ";
        break;
      case LMN_HL_ATTR:
        retVal << "hlink[ !, Addr:" << (unsigned long)atom->get_link(i) << ", ";
        retVal << "ID:" << (unsigned long) ((LmnSymbolAtomRef)atom->get_link(i))->get_id() << ", ";
        // checking buddy
	id2 = ((LmnSymbolAtomRef)
	       ((LmnSymbolAtomRef)atom->get_link(i))->get_link(0))->get_id();
  retVal << "buddy check:" << (unsigned long) id2;
	if (id2 != atom->get_id()) {
	  retVal << " ****ILL-FORMED!!****";
	}
        break;
      default:
        retVal << "unknown data type[" << (int) attr << "], ";
        break;
      }
    }
    retVal << "\n";
  }

  if (arity == 0)
    retVal << "\n";

  return retVal.str();
}

static std::string stringify_ruleset_dev(const std::vector<LmnRuleSet *> &v) {
  std::ostringstream retVal;
  unsigned int i;
  retVal << "ruleset[";
  for (i = 0; i < v.size(); i++) {
    retVal << (int) (v[i])->id << " ";
  }
  retVal << "]\n";
  return retVal.str();
}

std::string lmn_stringify_mem_dev(LmnMembraneRef mem) {
  std::ostringstream retVal;
  AtomListEntryRef ent;
  if (!mem)
    return "";

  retVal << "{\n";
  retVal << "Mem[" << (unsigned int) mem->NAME_ID() << "], ";
  retVal << "Addr[" << std::hex << mem << std::dec << "], ";
  retVal << "ID[" << (unsigned long) mem->mem_id() << "]\n";

  for (auto it : (mem)->atom_lists()) {
    ent = it.second;
    LmnSymbolAtomRef atom;
    if (ent) {
      for (auto iter_ : *(ent)) {
        atom = iter_;
        if (((LmnSymbolAtomRef)atom)->get_functor() != 6) {
            retVal << stringify_atom_dev(atom);
          }
        }
    }
  }

  retVal << stringify_ruleset_dev(mem->get_rulesets());
  retVal << lmn_stringify_mem_dev(mem->mem_child_head());
  retVal << "}\n";
  retVal << lmn_stringify_mem_dev(mem->mem_next());
  return retVal.str();
}

/*----------------------------------------------------------------------
 * dump dot
 */

static std::string stringify_dot_cell(LmnMembraneRef mem, SimpleHashtbl *ht, int *data_id,
                          int *cluster_id) {
  std::ostringstream retVal;
  AtomListEntryRef ent;
  LmnMembraneRef m;
  unsigned int i;

  if (!mem)
    return "";

  /* dump node labels */
  EACH_ATOMLIST(mem, ent, ({
    LmnSymbolAtomRef atom;
    EACH_ATOM(atom, ent, ({
      retVal << (unsigned long) atom << " [label = \"";
      retVal << stringify_atomname(atom->get_functor());
      retVal << "\", shape = circle];\n";
      for (i = 0; i < LMN_FUNCTOR_GET_LINK_NUM(atom->get_functor());i++) {
        LmnLinkAttr attr = atom->get_attr(i);
        if (LMN_ATTR_IS_DATA(attr)) {
          retVal << (unsigned long) atom->get_plink(i) << " [label = \"";
          retVal << stringify_data_atom(atom->get_link(i), attr);
          retVal << "\", shape = box];\n";
        }
      }
    }));
  }));

  /* dump connections */
  EACH_ATOMLIST(
      mem, ent, ({
        LmnSymbolAtomRef atom;
        EACH_ATOM(
            atom, ent, ({
              struct AtomRec *ar = (struct AtomRec *)hashtbl_get_default(
                  ht, (HashKeyType)atom, 0);
              unsigned int arity =
                  LMN_FUNCTOR_GET_LINK_NUM(atom->get_functor());

              for (i = 0; i < arity; i++) {
                LmnLinkAttr attr = atom->get_attr(i);

                if (ar && hashtbl_contains(&ar->args, i))
                  continue;
                retVal << (unsigned long) atom << " -- ";
                if (LMN_ATTR_IS_DATA(attr)) {
                  retVal << " " << (unsigned long) atom->get_plink(i);
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
                  retVal << (unsigned long) atom2;
                }
                retVal << "\n";
              }
            }));
      }));

  /* dump chidren */
  for (m = mem->mem_child_head(); m; m = m->mem_next()) {
    retVal << "subgraph cluster" << *cluster_id << " {\n";
    (*cluster_id)++;
    retVal << stringify_dot_cell(m, ht, data_id, cluster_id);
    retVal << "}\n";
  }
  return retVal.str();
}

std::string lmn_stringify_dot(LmnMembraneRef mem) {
  std::ostringstream retVal;
  int cluster_id = 0, data_id = 0;
  DumpState s;
  SimpleHashtbl ht;

  hashtbl_init(&ht, 128);

  retVal << "// This is an auto generated file by SLIM\n\n"
                  "graph {\n"
                  "node "
                  "[bgcolor=\"trasnparent\",truecolor=true,color=\"#000000\","
                  "style=filled,fillcolor=\"#ffd49b50\"];\n"
                  "edge [color=\"#000080\"];\n";

  retVal << stringify_dot_cell(mem, &ht, &data_id, &cluster_id) << "}\n";

  atomrec_tbl_destroy(&ht);

  return retVal.str();
}

static std::string lmn_stringify_link_json(LmnSymbolAtomRef atom, int index) {
  std::ostringstream retVal;
  LmnLinkAttr attr;
  void *data;

  attr = atom->get_attr(index);
  data = (void *)atom->get_link(index);

  retVal << "{";
  retVal << "\"attr\":";
  retVal << (int) attr;
  retVal << ",";

  if (LMN_ATTR_IS_DATA(attr)) {
    switch (attr) {
    case LMN_INT_ATTR:
      retVal << "\"data\":" << (int)((LmnWord)data);
      break;
    case LMN_DBL_ATTR:
    case LMN_CONST_DBL_ATTR:
      retVal << "\"data\":" << lmn_get_double((LmnDataAtomRef)data);
      break;
    case LMN_SP_ATOM_ATTR:
    case LMN_CONST_STR_ATTR:
      retVal << "\"data\":\"\\\"";
      retVal << ((LmnStringRef)data)->c_str();
      retVal << "\\\"\"";
      break;
    case LMN_HL_ATTR: {
      LmnSymbolAtomRef a = (LmnSymbolAtomRef)data;
      HyperLink *root = LMN_HL_ATOM_ROOT_HL(a);
      retVal << "\"data\":" << (int)root->id;
    } break;
    default:
      break;
    }
  } else {
    LmnSymbolAtomRef a = (LmnSymbolAtomRef)data;
    if (a != NULL) {
      retVal << "\"data\":" << (int) a->get_id();
    }
  }
  retVal << "}";
  return retVal.str();
}

static std::string lmn_stringify_atom_json(LmnSymbolAtomRef atom) {
  std::ostringstream retVal;
  int i;
  int arity;
  retVal << "{";
  retVal << "\"id\":" << (int)atom->get_id() << ",";
  retVal << "\"name\":\"" << atom->str() << "\",";
  retVal << "\"links\":[";
  {
    BOOL needs_comma = FALSE;
    for (i = 0, arity = atom->get_link_num(); i < arity; i++) {
      if (needs_comma)
        retVal << ",";
      needs_comma = TRUE;
      retVal << lmn_stringify_link_json(atom, i);
    }
  }
  retVal << "]}";
  return retVal.str();
}

static std::string lmn_stringify_mem_json(LmnMembraneRef mem) {
  std::ostringstream retVal;
  if (!mem)
    return "";

  retVal << "{";
  retVal << "\"id\":" << (int)mem->mem_id() << ",";
  retVal << "\"name\":\"" << mem->MEM_NAME() << "\",";
  retVal << "\"atoms\":[";
  {
    AtomListEntryRef ent;
    LmnFunctor f;
    BOOL needs_comma = FALSE;

    for (auto it : (mem)->atom_lists()) {
      ent = it.second;
      f = it.first;

      LmnSymbolAtomRef atom;
      if (LMN_IS_EX_FUNCTOR(f)) {
        continue;
      }
      if (ent) {
        for (auto iter_ : *ent) {
          atom = iter_;
          if (((LmnSymbolAtomRef)atom)->get_functor() != 6) {
            if (needs_comma)
              retVal << ",";
            needs_comma = TRUE;
            retVal << lmn_stringify_atom_json(atom);
          }
        }
      }
    }
  }

  retVal << "],";
  retVal << "\"membranes\":[";
  {
    LmnMembraneRef m;
    BOOL needs_comma = FALSE;
    for (m = mem->mem_child_head(); m; m = m->mem_next()) {
      if (needs_comma)
        retVal << ",";
      needs_comma = TRUE;
      retVal << lmn_stringify_mem_json(m);
    }
  }
  retVal << "]}";
  return retVal.str();
}

std::string stringify_escaped(const char *s) {
  std::ostringstream retVal;
  while (*s) {
    // refer ascii code table
    if (*s == '\t') {
      retVal << "\\t";
    } else if (*s == '\n') {
      retVal << "\\n";
    } else if (*s == '\r') {
      retVal << "\\r";
    } else if (*s == '"') {
      retVal << "\\\"";
    } else if (*s == '\\') {
      retVal << "\\\\";
    } else {
      retVal << *s;
    }
    s++;
  }
  return retVal.str();
}

std::string lmn_stringify_atom(LmnAtomRef atom, LmnLinkAttr attr) {
  struct DumpState s;
  SimpleHashtbl ht;

  hashtbl_init(&ht, 0);
  std::string retVal = stringify_atom(atom, &ht, attr, &s, 0);
  atomrec_tbl_destroy(&ht);
  return retVal;
}

} // namespace stringifier
} // namespace slim
