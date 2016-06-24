/*
 * firstclass_rule.c
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
 */

#include "firstclass_rule.h"

#include <st.h>

#include "lmntal_system_adapter.h"
#include "load.h"
#include "membrane.h"
#include "syntax.h"

#define LINK_PREFIX "L"
#define LINKCONNECTION_MAX 100000
#define MAX_RULE_STR 10000


struct LinkConnection
{
  LmnSAtom atom;
  HyperLink *hl;
  int link_pos, link_name;
};

static int link_connection_max = 0;
static int link_name_max = 0;

static struct LinkConnection link_connection_array[LINKCONNECTION_MAX];

int store_link_connection(LmnSAtom satom, int link_p, HyperLink *hl)
{
  link_connection_array[link_connection_max].atom = satom;
  link_connection_array[link_connection_max].hl = hl;
  link_connection_array[link_connection_max].link_pos = link_p;
  link_connection_array[link_connection_max].link_name = link_name_max;
  link_connection_max++;
  return link_name_max++;
}

int generate_linkname(LmnSAtom satom, int link_p)
{
  int i;

  if(LMN_IS_HL(LMN_SATOM(LMN_SATOM_GET_LINK(satom, link_p))))
    {
      HyperLink *hll = lmn_hyperlink_at_to_hl(LMN_SATOM(LMN_SATOM_GET_LINK(satom, link_p)));
      HyperLink *p_hl = hll->parent;

      for(i = 0; i < link_connection_max; i++)
        {
          if(link_connection_array[i].hl != NULL)
            {
              if(lmn_hyperlink_eq_hl(p_hl, link_connection_array[i].hl) == TRUE)
                {
                  return link_connection_array[i].link_name;
                }
            }
        }
      return store_link_connection(NULL, -1, p_hl);
    }

  for(i = 0; i < link_connection_max; i++)
    {
      if(link_connection_array[i].atom != NULL)
        {
          if((unsigned long)satom == (unsigned long)link_connection_array[i].atom && link_connection_array[i].link_pos == link_p)
            {
              return link_connection_array[i].link_name;
            }
        }
    }

  LmnSAtom dst_atom = LMN_SATOM(LMN_SATOM_GET_LINK(satom, link_p));

  if(LMN_SATOM_GET_FUNCTOR(dst_atom) == LMN_IN_PROXY_FUNCTOR)
    {
      LmnSAtom out_proxy = LMN_SATOM(LMN_SATOM_GET_LINK(dst_atom, 0));
      dst_atom = LMN_SATOM(LMN_SATOM_GET_LINK(out_proxy, 1));
      int arity = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(dst_atom));
      for(i = 0; i < arity; i++)
        {
          LmnSAtom linked_atom = LMN_SATOM(LMN_SATOM_GET_LINK(dst_atom, i));
          if(LMN_SATOM_GET_FUNCTOR(linked_atom) == LMN_OUT_PROXY_FUNCTOR)
            {
              LmnSAtom in_proxy = LMN_SATOM(LMN_SATOM_GET_LINK(linked_atom, 0));
              if(satom == LMN_SATOM(LMN_SATOM_GET_LINK(in_proxy, 1)))
                {
                  return store_link_connection(dst_atom, i, NULL);
                }
            }
        }
    }
  else if(LMN_SATOM_GET_FUNCTOR(dst_atom) == LMN_OUT_PROXY_FUNCTOR)
    {
      /* satom = ... */
      LmnSAtom in_proxy = LMN_SATOM(LMN_SATOM_GET_LINK(dst_atom, 0));
      dst_atom = LMN_SATOM(LMN_SATOM_GET_LINK(in_proxy, 1));
      return store_link_connection(dst_atom, 0, NULL);
    }
  int arity = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(dst_atom));


  for(i = 0; i < arity; i++)
    {
      if((unsigned long)satom == (unsigned long)LMN_SATOM(LMN_SATOM_GET_LINK(dst_atom, i)))
        {
          return store_link_connection(dst_atom, i, NULL);
        }
    }

    LMN_ASSERT(false);
    return -1;
}

LmnStringRef string_of_template_membrane(LmnMembrane *mem, LmnSAtom cm_atom)
{
  LmnStringRef result = lmn_string_make_empty();
  AtomListEntry *ent;
  LmnFunctor f;
  char istr[(int)(8 * sizeof(int) * 0.3010) + 2]; /* int型の桁数 + 1より長い */

  EACH_ATOMLIST_WITH_FUNC(mem, ent, f, ({
    LmnSAtom satom;
    if(LMN_IS_EX_FUNCTOR(f)) continue;
    if(LMN_IS_PROXY_FUNCTOR(f)) continue;

    EACH_ATOM(satom, ent, ({
      int arity = LMN_FUNCTOR_GET_LINK_NUM(LMN_SATOM_GET_FUNCTOR(satom));
      const char *atom_name = lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(satom)));
      
      if (f == LMN_UNARY_PLUS_FUNCTOR) {
        LmnSAtom in_proxy = LMN_SATOM(LMN_SATOM_GET_LINK(satom, 0));
        LmnSAtom out_proxy = LMN_SATOM(LMN_SATOM_GET_LINK(in_proxy, 0));
        if(cm_atom == LMN_SATOM(LMN_SATOM_GET_LINK(out_proxy, 1))) continue ;
        
        sprintf(istr, "%d", generate_linkname(satom, 0));
        lmn_string_push_raw_s(result, atom_name);
        lmn_string_push_raw_s(result, LINK_PREFIX);
        lmn_string_push_raw_s(result, istr);
      }
      else if(strcmp(atom_name, "==") == 0) {
        lmn_string_push_raw_s(result, LINK_PREFIX);
        sprintf(istr, "%d", generate_linkname(satom, 0));
        lmn_string_push_raw_s(result, istr);
        lmn_string_push_raw_s(result, "==");
        lmn_string_push_raw_s(result, LINK_PREFIX);
        sprintf(istr, "%d", generate_linkname(satom, 1));
        lmn_string_push_raw_s(result, istr);
      }
      else if(atom_name[0] == '@') {
        lmn_string_push_raw_s(result, atom_name);
      }
      else if(atom_name[0] == '$') {
        lmn_string_push_raw_s(result, atom_name);
        lmn_string_push_raw_c(result, '[');
        
        for(int i = 0; i < arity; i++) {
          if(i > 0) lmn_string_push_raw_s(result, ",");
          lmn_string_push_raw_s(result, LINK_PREFIX);
          sprintf(istr, "%d", generate_linkname(satom, i));
          lmn_string_push_raw_s(result, istr);
        }

        lmn_string_push_raw_c(result, ']');
      }
      else {
        if(strcmp(atom_name, ":-") == 0) {
          lmn_string_push_raw_s(result, "':-'");
        } else if(strcmp(atom_name, ".") == 0) {
          lmn_string_push_raw_s(result, "'.'");
        } else if(strcmp(atom_name, "[]") == 0) {
          lmn_string_push_raw_s(result, "'[]'");
        } else {
          lmn_string_push_raw_s(result, atom_name);
        }

        if(arity > 0) {
          lmn_string_push_raw_s(result, "(");
          for(int i = 0; i < arity; i++) {
            if(i > 0) lmn_string_push_raw_s(result, ",");
            lmn_string_push_raw_s(result, LINK_PREFIX);
            sprintf(istr, "%d", generate_linkname(satom, i));
            lmn_string_push_raw_s(result, istr);
          }
          lmn_string_push_raw_s(result, ")");
        }
      }
      lmn_string_push_raw_s(result, ",");               
    }));
  }));
  
  for(LmnMembrane *m = mem->child_head; m; m = m->next) {
    LmnStringRef s = string_of_template_membrane(m, cm_atom);
    if(lmn_string_last(s) == ',') lmn_string_pop(s);

    lmn_string_push_raw_s(result, "{");
    lmn_string_push(result, s);
    lmn_string_push_raw_s(result, "},");

    lmn_string_free(s);
  }

  return result;
}


LmnStringRef string_of_guard_mem(LmnMembrane *mem, LmnSAtom cm_atom)
{
  LmnStringRef result;
  AtomListEntry *ent;
  LmnFunctor f;
  const char* constraint_name[3] = {"int", "float", "ground"};

  result = lmn_string_make_empty();
  EACH_ATOMLIST_WITH_FUNC(mem, ent, f, ({
    if(LMN_IS_EX_FUNCTOR(f) || LMN_IS_PROXY_FUNCTOR(f)) continue;

    LmnSAtom satom;
    EACH_ATOM(satom, ent, ({
      const char *atom_name = lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(satom)));
      LmnSAtom in_proxy = LMN_SATOM(LMN_SATOM_GET_LINK(satom, 0));
      LmnSAtom out_proxy = LMN_SATOM(LMN_SATOM_GET_LINK(in_proxy, 0));

      if(f == LMN_UNARY_PLUS_FUNCTOR && cm_atom == LMN_SATOM(LMN_SATOM_GET_LINK(out_proxy, 1))) continue;

      for(int i = 0; i < 3; i++) {
        if(strcmp(constraint_name[i], atom_name) != 0) continue;

        const char *in_proxy_name = lmn_id_to_name(LMN_FUNCTOR_NAME_ID(LMN_SATOM_GET_FUNCTOR(in_proxy)));
        lmn_string_push_raw_s(result, constraint_name[i]);
        lmn_string_push_raw_s(result, "(");
        lmn_string_push_raw_s(result, in_proxy_name);
        lmn_string_push_raw_s(result, "),");
      }
    }));
  }));

  return result;
}

LmnStringRef generate_string_of_first_class_rule(LmnMembrane *h_mem, LmnMembrane *g_mem, LmnMembrane *b_mem, LmnSAtom imply)
/* 3引数の':-' のアトムで接続先が全て膜．
   引数は第一引数から順につながってる膜 */
{
  link_name_max = 0;
  link_connection_max = 0;

  LmnStringRef head = string_of_template_membrane(h_mem, imply);
  LmnStringRef guard = string_of_guard_mem(g_mem, imply);
  LmnStringRef body = string_of_template_membrane(b_mem, imply);

  LmnStringRef result = lmn_string_make_empty();
  lmn_string_push(result, head);
  lmn_string_push_raw_s(result, ":-");
  lmn_string_push(result, guard);
  lmn_string_push_raw_s(result, "|");
  lmn_string_push(result, body);
  lmn_string_push_raw_s(result, ".");

  lmn_string_free(head);
  lmn_string_free(guard);
  lmn_string_free(body);

  return result;
}

LmnMembrane* get_mem_linked_atom(LmnSAtom target_atom, int link_n)
{
  LmnAtom atom = LMN_SATOM_GET_LINK(LMN_ATOM(target_atom), link_n);
  return LMN_PROXY_GET_MEM(LMN_SATOM(LMN_SATOM_GET_LINK(LMN_SATOM(atom), 0)));
}


void delete_ruleset(LmnMembrane *mem, LmnRulesetId del_id)
{
  Vector *src_v = &(mem->rulesets);

  for(int i = 0; i < vec_num(src_v); i++) {
    LmnRuleSetRef rs_i = (LmnRuleSetRef)vec_get(src_v, i);
    if (lmn_ruleset_get_id(rs_i) != del_id) continue;
    
    /* move successors forward */
    for(int j = i; j < vec_num(src_v) - 1; j++) {
      LmnRuleSetRef next = (LmnRuleSetRef)vec_get(src_v, j + 1);
      vec_set(src_v, j, (vec_data_t)next);
    }

    src_v->num--;
    break;
  }
}

st_table_t first_class_rule_tbl;

static int colon_minus_cmp(LmnSAtom x, LmnSAtom y)
{
  return !((unsigned long)x == (unsigned long)y);
}

static long colon_minus_hash(LmnSAtom x)
{
  return (long)x;
}

static struct st_hash_type type_colon_minushash =
  {
    (st_cmp_func)colon_minus_cmp,
    (st_hash_func)colon_minus_hash
  };
  


void first_class_rule_tbl_init()
{
  first_class_rule_tbl = st_init_table(&type_colon_minushash);
}

void register_first_class_rule(LmnSAtom colon_minus, LmnRulesetId rs_id)
{
  st_insert(first_class_rule_tbl, (st_data_t)colon_minus, (st_data_t)rs_id);
}

void delete_first_class_rule(LmnSAtom colon_minus)
{
  st_delete(first_class_rule_tbl, (st_data_t)colon_minus, 0);
}


LmnRulesetId imply_to_rulesetid(LmnSAtom imply)
{
  st_data_t entry;
  if(st_lookup(first_class_rule_tbl, (st_data_t)imply, (st_data_t *)&entry)){
    return (LmnRulesetId)entry;
  }
  return -1;
}

void firstclass_ruleset_register(LmnSAtom imply, LmnMembrane *membrane) {
  /* insert ruleset to membrane */
  for(int j = 0; j < 3; j++){
    LmnAtom pa = LMN_SATOM_GET_LINK(LMN_ATOM(imply), j);
    if(!LMN_SATOM_IS_PROXY(pa))
      break;
  }
  
  LmnStringRef rule_str = generate_string_of_first_class_rule(get_mem_linked_atom(imply, 0), get_mem_linked_atom(imply, 1), get_mem_linked_atom(imply, 2), imply);

  FILE *output_fp = fopen("tmp.lmn", "w");
  fputs(lmn_string_c_str(rule_str), output_fp);
  lmn_string_free(rule_str);
  fclose(output_fp);
  FILE *compiled_rulesets = lmntal_compile_file("tmp.lmn");
  ILRef il;
  /* ILをパース */
  il_parse(compiled_rulesets, &il);
  /* パースしたILをロード */
  Vector *rulesets = load_rulesets_with_il(il);
  LmnRuleSetRef dynamic_ruleset = (LmnRuleSetRef)vec_get(rulesets, 1);
  LmnRulesetId r_i = lmn_ruleset_get_id(dynamic_ruleset);
  /* :-アトムとコンパイルされたルールセットIDを対応付けるハッシュテーブルへ追加 */
  register_first_class_rule(imply, r_i);
  /* 膜のルールセットにコンパイルされたルールセットを追加 */
  lmn_mem_add_ruleset(membrane, dynamic_ruleset);

  vec_free(rulesets);
}


void firstclass_ruleset_delete(LmnSAtom imply, LmnMembrane *membrane) {
  LmnRulesetId id = imply_to_rulesetid(imply);

  if(id > 0) {
    delete_ruleset(membrane, id);
    delete_first_class_rule(imply);
  }
}

