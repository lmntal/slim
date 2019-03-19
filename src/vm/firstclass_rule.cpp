/*
 * firstclass_rule.cpp
 *
 *   Copyright (c) 2017, Ueda Laboratory LMNtal Group
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
 */

#include "firstclass_rule.h"

#include "element/st.h"
#include "ffi/lmntal_system_adapter.h"
#include "loader/loader.h"

#include <cstdio>

#define LINK_PREFIX "L"
#define LINKCONNECTION_MAX 100000
#define MAX_RULE_STR 10000

struct LinkConnection {
  LmnSymbolAtomRef atom;
  HyperLink *hl;
  int link_pos, link_name;
};

int linkconnection_push(Vector *link_connections, LmnSymbolAtomRef satom,
                        int link_p, HyperLink *hl) {
  int link_name = vec_num(link_connections);
  struct LinkConnection *c = LMN_MALLOC(struct LinkConnection);
  c->atom = satom;
  c->hl = hl;
  c->link_pos = link_p;
  c->link_name = link_name;
  vec_push(link_connections, (vec_data_t)c);
  return link_name;
}

int linkconnection_make_linkno(Vector *link_connections, LmnSymbolAtomRef satom,
                               int link_p) {
  if (LMN_IS_HL((LmnSymbolAtomRef)satom->get_link(link_p))) {
    HyperLink *hll = lmn_hyperlink_at_to_hl(
        (LmnSymbolAtomRef)satom->get_link(link_p));
    HyperLink *p_hl = hll->parent;

    for (int i = 0; i < vec_num(link_connections); i++) {
      struct LinkConnection *c =
          (struct LinkConnection *)vec_get(link_connections, i);
      if (c->hl && lmn_hyperlink_eq_hl(p_hl, c->hl)) {
        return c->link_name;
      }
    }
    return linkconnection_push(link_connections, NULL, -1, p_hl);
  }

  for (int i = 0; i < vec_num(link_connections); i++) {
    struct LinkConnection *c =
        (struct LinkConnection *)vec_get(link_connections, i);
    if (c->atom == satom && c->link_pos == link_p) {
      return c->link_name;
    }
  }

  LmnSymbolAtomRef dst_atom =
      (LmnSymbolAtomRef)satom->get_link(link_p);

  if (dst_atom->get_functor() == LMN_IN_PROXY_FUNCTOR) {
    LmnSymbolAtomRef out_proxy =
        (LmnSymbolAtomRef)dst_atom->get_link(0);
    LmnSymbolAtomRef atom = (LmnSymbolAtomRef)out_proxy->get_link(1);
    int arity = LMN_FUNCTOR_GET_LINK_NUM(atom->get_functor());
    for (int i = 0; i < arity; i++) {
      LmnSymbolAtomRef linked_atom =
          (LmnSymbolAtomRef)atom->get_link(i);
      if (linked_atom->get_functor() == LMN_OUT_PROXY_FUNCTOR) {
        LmnSymbolAtomRef in_proxy =
            (LmnSymbolAtomRef)linked_atom->get_link(0);
        if (satom == in_proxy->get_link(1)) {
          return linkconnection_push(link_connections, atom, i, NULL);
        }
      }
    }
  } else if (dst_atom->get_functor() == LMN_OUT_PROXY_FUNCTOR) {
    LmnSymbolAtomRef in_proxy =
        (LmnSymbolAtomRef)dst_atom->get_link(0);
    LmnSymbolAtomRef atom = (LmnSymbolAtomRef)in_proxy->get_link(1);
    return linkconnection_push(link_connections, atom, 0, NULL);
  }

  int arity = LMN_FUNCTOR_GET_LINK_NUM(dst_atom->get_functor());
  for (int i = 0; i < arity; i++) {
    if (satom == dst_atom->get_link(i)) {
      return linkconnection_push(link_connections, dst_atom, i, NULL);
    }
  }

  LMN_ASSERT(false);
  return -1;
}

std::string string_of_data_atom(LmnDataAtomRef data, LmnLinkAttr attr) {
  if (attr == LMN_INT_ATTR) {
    return std::to_string((long)data);
  } else if (attr == LMN_DBL_ATTR) {
    return std::to_string(lmn_get_double(data));
  }

  return "";
}

std::string string_of_template_membrane(Vector *link_connections,
                                        LmnMembraneRef mem,
                                        LmnSymbolAtomRef cm_atom) {
  std::string result;
  AtomListEntryRef ent;
  LmnFunctor f;
  char istr[(int)(8 * sizeof(int) * 0.3010) + 2]; /* int型の桁数 + 1より長い */

  for (auto it : mem->atom_lists()) {
    auto &ent = it.second;
    auto &f = it.first;
    LmnSymbolAtomRef satom;
    if (LMN_IS_EX_FUNCTOR(f))
      continue;
    if (LMN_IS_PROXY_FUNCTOR(f))
      continue;

    EACH_ATOM(
        satom, ent, ({
          int arity = LMN_FUNCTOR_GET_LINK_NUM(satom->get_functor());
          const char *atom_name =
              lmn_id_to_name(LMN_FUNCTOR_NAME_ID(satom->get_functor()));

          if (f == LMN_UNARY_PLUS_FUNCTOR) {
            LmnSymbolAtomRef in_proxy =
                (LmnSymbolAtomRef)satom->get_link(0);
            LmnSymbolAtomRef out_proxy =
                (LmnSymbolAtomRef)in_proxy->get_link(0);
            if (cm_atom == out_proxy->get_link(1))
              continue;

            sprintf(istr, "%d",
                    linkconnection_make_linkno(link_connections, satom, 0));
            result += atom_name;
            result += LINK_PREFIX;
            result += istr;
          } else if (strcmp(atom_name, "==") == 0) {
            result += LINK_PREFIX;
            sprintf(istr, "%d",
                    linkconnection_make_linkno(link_connections, satom, 0));
            result += istr;
            result += "=";
            result += LINK_PREFIX;
            sprintf(istr, "%d",
                    linkconnection_make_linkno(link_connections, satom, 1));
            result += istr;
          } else if (atom_name[0] == '@') {
            result += atom_name;
          } else if (atom_name[0] == '$') {
            result += atom_name;
            result += '[';

            for (int i = 0; i < arity; i++) {
              if (i > 0)
                result += ",";
              result += LINK_PREFIX;
              sprintf(istr, "%d",
                      linkconnection_make_linkno(link_connections, satom, i));
              result += istr;
            }

            result += ']';
          } else {
            if (strcmp(atom_name, ":-") == 0) {
              result += "':-'";
            } else if (strcmp(atom_name, ".") == 0) {
              result += "'.'";
            } else if (strcmp(atom_name, "[]") == 0) {
              result += "'[]'";
            } else {
              result += atom_name;
            }

            if (arity > 0) {
              result += "(";
              for (int i = 0; i < arity; i++) {
                LmnLinkAttr attr = satom->get_attr(i);
                if (i > 0)
                  result += ",";

                if (LMN_ATTR_IS_DATA(attr) && LMN_HL_ATTR == attr) {
                  result += LINK_PREFIX;
                  sprintf(
                      istr, "%d",
                      linkconnection_make_linkno(link_connections, satom, i));
                  result += istr;
                } else if (LMN_ATTR_IS_DATA(attr) && LMN_INT_ATTR == attr) {
                  LmnAtomRef data = satom->get_link(i);
                  char *s = int_to_str((long)data);
                  result += s;
                } else if (LMN_ATTR_IS_DATA(attr) && LMN_DBL_ATTR == attr) {
                  LmnAtomRef data = satom->get_link(i);
                  char buf[64];
                  sprintf(buf, "%#g", lmn_get_double((LmnDataAtomRef)data));
                  result += buf;
                } else {
                  result += LINK_PREFIX;
                  sprintf(
                      istr, "%d",
                      linkconnection_make_linkno(link_connections, satom, i));
                  result += istr;
                }
              }
              result += ")";
            }
          }
          result += ",";
        }));
  }

  for (LmnMembraneRef m = lmn_mem_child_head(mem); m; m = lmn_mem_next(m)) {
    auto s = string_of_template_membrane(link_connections, m, cm_atom);
    if (!s.empty() && s.back() == ',')
      s.pop_back();

    result += "{";
    result += s;
    result += "},";
  }

  return result;
}

std::string string_of_guard_op(LmnSymbolAtomRef satom) {
  std::string result;
  const char *atom_name =
      lmn_id_to_name(LMN_FUNCTOR_NAME_ID(satom->get_functor()));
  int arity = LMN_FUNCTOR_GET_LINK_NUM(satom->get_functor());
  LmnLinkAttr attr;
  if (arity == 1)
    result += atom_name;
  else {
    attr = satom->get_attr(0);
    if (LMN_ATTR_IS_DATA(attr))
      result += string_of_data_atom(
          (LmnDataAtomRef)satom->get_link(0), attr);
    else
      result +=
          string_of_guard_op((LmnSymbolAtomRef)satom->get_link(0));

    if (strcmp(":=", atom_name) == 0)
      result += "=";
    else
      result += atom_name;

    attr = satom->get_attr(1);
    if (LMN_ATTR_IS_DATA(attr))
      result += string_of_data_atom(
          (LmnDataAtomRef)satom->get_link(1), attr);
    else
      result +=
          string_of_guard_op((LmnSymbolAtomRef)satom->get_link(1));
  }

  return result;
}

std::string string_of_guard_mem(LmnMembraneRef mem, LmnSymbolAtomRef cm_atom) {
  AtomListEntryRef ent;
  LmnFunctor f;
  const char *constraint_name[] = {"int",   "float", "ground",
                                   "unary", "hlink", "new"};
  const char *op_name[] = {"=:=", "=\\=", ">",  "<",   "=<",
                           ">=",  ":=",   "==", "\\=", "><"};
  std::string result;
  for (auto it : mem->atom_lists()) {
    auto &ent = it.second;
    auto &f = it.first;
    if (LMN_IS_EX_FUNCTOR(f) || LMN_IS_PROXY_FUNCTOR(f))
      continue;
    LmnSymbolAtomRef satom;
    EACH_ATOM(
        satom, ent, ({
          const char *atom_name =
              lmn_id_to_name(LMN_FUNCTOR_NAME_ID(satom->get_functor()));

          if (f == LMN_UNARY_PLUS_FUNCTOR) {
            LmnSymbolAtomRef in_proxy =
                (LmnSymbolAtomRef)satom->get_link(0);
            LmnSymbolAtomRef out_proxy =
                (LmnSymbolAtomRef)in_proxy->get_link(0);
            if (cm_atom == out_proxy->get_link(1))
              continue;
          } else {
            for (int i = 0; i < ARY_SIZEOF(constraint_name); i++) {
              if (strcmp(constraint_name[i], atom_name) != 0)
                continue;
              LmnSymbolAtomRef typed_pc_atom =
                  (LmnSymbolAtomRef)satom->get_link(0);
              const char *typed_pc_atom_name = lmn_id_to_name(
                  LMN_FUNCTOR_NAME_ID(typed_pc_atom->get_functor()));
              result += constraint_name[i];
              result += "(";
              result += typed_pc_atom_name;
              result += "),";
            }
            for (int i = 0; i < ARY_SIZEOF(op_name); i++) {
              if (strcmp(op_name[i], atom_name) != 0)
                continue;
              result += string_of_guard_op(satom);
              result += ",";
            }
          }
        }));
  }

  return result;
}

std::string
string_of_firstclass_rule(LmnMembraneRef h_mem, LmnMembraneRef g_mem,
                          LmnMembraneRef b_mem, LmnSymbolAtomRef imply)
/* 3引数の':-' のアトムで接続先が全て膜．
   引数は第一引数から順につながってる膜 */
{
  Vector *link_connections = new Vector(10);

  auto head = string_of_template_membrane(link_connections, h_mem, imply);
  auto guard = string_of_guard_mem(g_mem, imply);
  auto body = string_of_template_membrane(link_connections, b_mem, imply);

  std::string result;
  result += head;
  result += ":-";
  result += guard;
  result += "|";
  result += body;
  result += ".";

  for (int i = 0; i < vec_num(link_connections); i++)
    LMN_FREE(vec_get(link_connections, i));
  vec_free(link_connections);

  return result;
}

LmnMembraneRef get_mem_linked_atom(LmnSymbolAtomRef target_atom, int link_n) {
  LmnSymbolAtomRef atom =
      (LmnSymbolAtomRef)target_atom->get_link(link_n);
  return LMN_PROXY_GET_MEM((LmnSymbolAtomRef)atom->get_link(0));
}

void delete_ruleset(LmnMembraneRef mem, LmnRulesetId del_id) {
  Vector *mem_rulesets = lmn_mem_get_rulesets(mem);

  for (int i = 0; i < vec_num(mem_rulesets); i++) {
    LmnRuleSetRef rs = (LmnRuleSetRef)vec_get(mem_rulesets, i);
    if (rs->id != del_id)
      continue;

    /* move successors forward */
    for (int j = i; j < vec_num(mem_rulesets) - 1; j++) {
      LmnRuleSetRef next = (LmnRuleSetRef)vec_get(mem_rulesets, j + 1);
      vec_set(mem_rulesets, j, (vec_data_t)next);
    }

    mem_rulesets->num--;
    break;
  }
}

st_table_t first_class_rule_tbl;

static int colon_minus_cmp(LmnSymbolAtomRef x, LmnSymbolAtomRef y) {
  return x != y;
}

static long colon_minus_hash(LmnSymbolAtomRef x) { return (long)x; }

static struct st_hash_type type_colon_minushash = {
    (st_cmp_func)colon_minus_cmp, (st_hash_func)colon_minus_hash};

void first_class_rule_tbl_init() {
  first_class_rule_tbl = st_init_table(&type_colon_minushash);
}

LmnRulesetId imply_to_rulesetid(LmnSymbolAtomRef imply) {
  st_data_t entry;
  if (st_lookup(first_class_rule_tbl, (st_data_t)imply, &entry)) {
    return (LmnRulesetId)entry;
  }
  return -1;
}

LmnRuleSetRef firstclass_ruleset_create(LmnSymbolAtomRef imply) {
  /* ':-'_3アトムがプロキシにつながっていなければ中止 */
  for (int j = 0; j < 3; j++) {
    LmnSymbolAtomRef pa = (LmnSymbolAtomRef)imply->get_link(j);
    if (!pa->is_proxy())
      return NULL;
  }

  /* ':-'_3(head, guard, body)からルール文字列を生成してコンパイル */
  LmnMembraneRef head = get_mem_linked_atom(imply, 0);
  LmnMembraneRef guard = get_mem_linked_atom(imply, 1);
  LmnMembraneRef body = get_mem_linked_atom(imply, 2);
  auto rule_str = string_of_firstclass_rule(head, guard, body, imply);
  auto compiled_rulesets = lmntal_compile_rule_str(rule_str.c_str());

  /* コンパイルされたルールからルールセットを生成 */
  auto ruleAST = il_parse_rule(std::move(compiled_rulesets));
  LmnRulesetId id = LmnRuleSetTable::gen_id();
  LmnRuleSetRef ruleset = new LmnRuleSet(id, 1);
  auto rule = load_rule(*ruleAST);
  ruleset->put(rule.get());
  LmnRuleSetTable::add(ruleset, id);
  rule.release();

  /* :-アトムとコンパイルされたルールセットIDを対応付けるハッシュテーブルへ追加
   */
  st_insert(first_class_rule_tbl, (st_data_t)imply, (st_data_t)id);

  return ruleset;
}

void firstclass_ruleset_release(LmnSymbolAtomRef imply) {
  LMN_ASSERT(st_contains(first_class_rule_tbl, (st_data_t)imply));
  st_delete(first_class_rule_tbl, (st_data_t)imply, NULL);
}

LmnRuleSetRef firstclass_ruleset_lookup(LmnSymbolAtomRef imply) {
  LmnRulesetId id = imply_to_rulesetid(imply);
  return (id > 0) ? LmnRuleSetTable::at(id) : NULL;
}
