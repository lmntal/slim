/*
 * state_map.c
 *
 *   Copyright (c) 2017, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
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
 * $Id$
 */

int id_find=0;
#include "../cb.h"
#include <time.h>
#include "state_map.h"
#include "set.h"
#include "vm/vm.h"
#include "verifier/verifier.h"

#ifdef CB
extern struct timespec cb_time[6];
extern int cb_call[6];
#endif

/* #ifdef CB */
/* extern int cb_times[6][2]; */
/* #endif */

static int state_map_atom_type;
static BYTE mc_flag = 0x10U;

static LmnStateMapRef lmn_make_state_map(LmnMembraneRef mem)
{
  LmnStateMapRef s = LMN_MALLOC(struct LmnStateMap);
  LMN_SP_ATOM_SET_TYPE(s, state_map_atom_type);
  s->states = statespace_make(NULL, NULL);
  s->id_tbl = st_init_table(&type_id_hash);
  return s;
}

void lmn_state_map_free(LmnStateMapRef state_map, LmnMembraneRef mem)
{
  statespace_free(LMN_STATE_MAP(state_map)->states);
  st_free_table(LMN_STATE_MAP(state_map)->id_tbl);
  LMN_FREE(state_map);
}

/*----------------------------------------------------------------------
 * Callbacks
 */

/*
 * 生成
 * -a0 Map
 */
void cb_state_map_init(LmnReactCxtRef rc,
                       LmnMembraneRef mem,
                       LmnAtomRef a0, LmnLinkAttr t0)
{
#ifdef CB
  struct timespec tp0, tp1;
  clock_gettime(CLOCK_REALTIME, &tp0);
#endif
  LmnStateMapRef atom = lmn_make_state_map(mem);
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;
  LMN_SP_ATOM_SET_TYPE(atom, state_map_atom_type);
  lmn_mem_push_atom(mem, atom, attr);
  lmn_mem_newlink(mem,
                  a0, t0, LMN_ATTR_GET_VALUE(t0),
                  atom, attr, 0);
#ifdef CB
  clock_gettime(CLOCK_REALTIME, &tp1);
  long sec, nsec;
  sec = tp1.tv_sec-tp0.tv_sec;
  nsec = tp1.tv_nsec-tp0.tv_nsec;
  if(nsec < 0)
    {
      sec--;
      nsec += 1000000000L;
    }
  if(cb_time[2].tv_nsec+nsec>=1000000000L)
    {
      sec++;
      nsec-=1000000000L;
    }

  cb_time[2].tv_sec+= sec;
  cb_time[2].tv_nsec+= nsec;
  cb_call[2]++;
#endif
/* #ifdef CB */
/*   cb_times[2][0]+=clock()-s; */
/*   cb_times[2][1]++; */
/* #endif */
}

/*
 * 解放
 * +a0 Map
 */
void cb_state_map_free(LmnReactCxtRef rc,
                       LmnMembraneRef mem,
                       LmnAtomRef a0, LmnLinkAttr t0)
{
  lmn_state_map_free(LMN_STATE_MAP(a0), mem);
  lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)a0, t0);
}

/*
 * 状態->ID
 * +a0 Map
 * +a1 状態
 * -a2 ID
 * -a3 Map
 */
void cb_state_map_id_find(LmnReactCxtRef rc,
                          LmnMembraneRef mem,
                          LmnAtomRef a0, LmnLinkAttr t0,
                          LmnAtomRef a1, LmnLinkAttr t1,
                          LmnAtomRef a2, LmnLinkAttr t2,
                          LmnAtomRef a3, LmnLinkAttr t3)
{
#ifdef CB
  struct timespec tp0, tp1;
  clock_gettime(CLOCK_REALTIME, &tp0);
  id_find++;
  /* clock_t s = clock(); */
#endif
  LmnMembraneRef m = LMN_PROXY_GET_MEM(LMN_SATOM_GET_LINK(a1, 0));
  LmnSAtom in = LMN_SATOM_GET_LINK(a1, 0);
  LmnSAtom out = a1;
  LmnSAtom plus = LMN_SATOM_GET_LINK(in, 1);
  LmnLinkAttr in_attr = LMN_SATOM_GET_ATTR(a1, 0);
  StateSpaceRef ss = LMN_STATE_MAP(a0)->states;
  st_table_t i_tbl = LMN_STATE_MAP(a0)->id_tbl;

  lmn_mem_delete_atom(m, in, in_attr);

  LmnSAtom at = lmn_mem_newatom(m, lmn_functor_intern(ANONYMOUS, lmn_intern("@"), 1));
  lmn_newlink_in_symbols(plus, 0, at, 0);

  State *new_s = state_make(m, 0, mc_use_canonical(mc_flag));

  State *succ = statespace_insert(ss, new_s);

  if(succ == new_s){
    /* new state */
    state_id_issue(succ);
    mem_remove_symbol_atom(m, at);
    lmn_delete_atom(at);
    in = lmn_mem_newatom(m, LMN_IN_PROXY_FUNCTOR);
    lmn_newlink_in_symbols(in, 0, out, 0);
    lmn_newlink_in_symbols(in, 1, plus, 0);
    st_insert(i_tbl, (st_data_t)new_s, (st_data_t)m);
  }
  lmn_mem_push_atom(mem, succ, LMN_INT_ATTR);
  lmn_mem_newlink(mem,
                  a2, t2, LMN_ATTR_GET_VALUE(t2),
                  succ, LMN_INT_ATTR, 0);

  lmn_mem_newlink(mem,
                  a0, t0, LMN_ATTR_GET_VALUE(t0),
                  a3, t3, LMN_ATTR_GET_VALUE(t3));

  lmn_mem_delete_atom(mem, a1, t1);
  lmn_mem_remove_mem(mem, m);
#ifdef CB
  clock_gettime(CLOCK_REALTIME, &tp1);
  long sec, nsec;
  sec = tp1.tv_sec-tp0.tv_sec;
  nsec = tp1.tv_nsec-tp0.tv_nsec;
  if(nsec < 0)
    {
      sec--;
      nsec += 1000000000L;
    }
  if(cb_time[3].tv_nsec+nsec>=1000000000L)
    {
      sec++;
      nsec-=1000000000L;
    }

  cb_time[3].tv_sec+=sec;
  cb_time[3].tv_nsec+=nsec;
  cb_call[3]++;
#endif
/* #ifdef CB */
/*   cb_times[3][0]+=clock()-s; */
/*   cb_times[3][1]++; */
/* #endif */
}

/*
 * ID->状態
 * +a0 Map
 * +a1 ID
 * -a2 状態
 * -a3 Map
 */
void cb_state_map_state_find(LmnReactCxtRef rc,
			     LmnMembraneRef mem,
			     LmnAtomRef a0, LmnLinkAttr t0,
			     LmnAtomRef a1, LmnLinkAttr t1,
			     LmnAtomRef a2, LmnLinkAttr t2,
			     LmnAtomRef a3, LmnLinkAttr t3)
{
#ifdef CB
  struct timespec tp0, tp1;
  clock_gettime(CLOCK_REALTIME, &tp0);
  /* clock_t st=clock(); */
#endif
  st_table_t i_tbl=LMN_STATE_MAP(a0)->id_tbl;
  State *s=(State *)a1;
  st_data_t entry;
  int res=st_lookup(i_tbl, (st_data_t)s, &entry);
  LmnSAtom result;
  if(res){
    LmnMembraneRef val=lmn_mem_copy((LmnMembraneRef)entry);
    AtomListEntryRef ent;
    LmnFunctor f;
    LmnSAtom in;
    LmnSAtom out = lmn_mem_newatom(mem, LMN_OUT_PROXY_FUNCTOR);
    EACH_ATOMLIST_WITH_FUNC(val, ent, f, ({
	  LmnSAtom satom;
	  EACH_ATOM(satom, ent, ({
		if(f==LMN_IN_PROXY_FUNCTOR){
		  in=satom;
		}
	      }))
	    }));
    lmn_newlink_in_symbols(out, 0, in, 0);
    lmn_mem_newlink(mem,
		    a2, t2, LMN_ATTR_GET_VALUE(t2),
		    out, LMN_ATTR_MAKE_LINK(1),1);
  }else{
    result=lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("none"), 1));
    lmn_mem_newlink(mem,
		    result, LMN_ATTR_MAKE_LINK(0), 0,
		    a2, t2, LMN_ATTR_GET_VALUE(t2));
  }
  lmn_mem_newlink(mem,
		  a0, t0, LMN_ATTR_GET_VALUE(t1),
		  a3, t3, LMN_ATTR_GET_VALUE(t3));
/* #ifdef CB */
/*   cb_times[4][0]+=clock()-st; */
/*   cb_times[4][1]++; */
/* #endif */
#ifdef CB
  clock_gettime(CLOCK_REALTIME, &tp1);
  long sec, nsec;
  sec = tp1.tv_sec-tp0.tv_sec;
  nsec = tp1.tv_nsec-tp0.tv_nsec;
  if(nsec < 0)
    {
      sec--;
      nsec += 1000000000L;
    }
  if(cb_time[4].tv_nsec+nsec>=1000000000L)
    {
      sec++;
      nsec-=1000000000L;
    }

  cb_time[4].tv_sec+=sec;
  cb_time[4].tv_nsec+=nsec;
  cb_call[4]++;
#endif
}

/*----------------------------------------------------------------------
 * Initialization
 */

void *sp_cb_state_map_copy(void *data)
{
  return data;
}

void sp_cb_state_map_free(void *data)
{
}

unsigned char sp_cb_state_map_eq(void *_p1, void *_p2)
{
  return 0;
}

void sp_cb_state_map_dump(void *state_map, LmnPortRef port)
{
  port_put_raw_s(port, "<state_map>");
}

unsigned char sp_cb_state_map_is_ground(void *data)
{
  return 1;
}

void init_state_map(void)
{
  state_map_atom_type = lmn_sp_atom_register("state_map",
                                         sp_cb_state_map_copy,
                                         sp_cb_state_map_free,
                                         sp_cb_state_map_eq,
                                         sp_cb_state_map_dump,
                                         sp_cb_state_map_is_ground);
  lmn_register_c_fun("cb_state_map_init", (void *)cb_state_map_init, 1);
  lmn_register_c_fun("cb_state_map_free", (void *)cb_state_map_free, 1);
  lmn_register_c_fun("cb_state_map_id_find", (void *)cb_state_map_id_find, 4);
  lmn_register_c_fun("cb_state_map_state_find", (void *)cb_state_map_state_find, 4);
}
