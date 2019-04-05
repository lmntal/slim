/*
 * dpor_naive.h
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
 * $Id$
 */

#ifndef LMN_MC_POR_H
#define LMN_MC_POR_H

/**
 * @ingroup  Verifier
 * @defgroup DPOR
 * @{
 */

#include "../lmntal.h"
#include "element/element.h"
#include "statespace.h"

struct McPorData {
	State *root;
	st_table_t
	    strans_independency; 	/* 独立性情報テーブル:
                            	*   構造体StateTransitionのidをキーとし
                            	*   bins[id]は高々1個のエントリー(Vector)を持つ．
                            	*   Vectorには,
                            	* キーであるidの遷移と独立関係にある遷移idが積まれる.
                            	*/
	st_table_t
	    states; /* ample(s)計算中のみ使用．展開されたすべてのStateを管理． */
	Queue *queue; /* C1のチェックにあたってstate graphを展開する際に使用 */
	Vector *
	    ample_candidate; /* ample(s)の候補を管理するVector．本Vector内のすべての遷移が，C0〜C3のチェック対象となる
                        */
	std::unique_ptr<MCReactContext> rc;
	unsigned long next_strans_id;
	BOOL flags;
	McPorData();
	void init_por_vars();//public
	void free_por_vars();//public
	void por_calc_ampleset(StateSpaceRef ss, State *s, LmnReactCxtRef rc, Vector *new_s, BOOL flag);//public
	static int independency_vec_free(st_data_t _k, st_data_t vec, st_data_t _a);//public
	static int destroy_tmp_state_graph(State *s, LmnWord _a);//public
	void por_gen_successors(State *s, LmnReactCxtRef rc, AutomataRef a, Vector *psyms);
	void por_store_successors(State *s, LmnReactCxtRef rc, BOOL is_store);
private:
	void finalize_ample(BOOL arg_f);
	State *por_state_insert(State *succ, struct MemDeltaRoot *d);
	void por_store_successors_inner(State *s, LmnReactCxtRef rc);

};
static McPorData mc_por;

//void por_calc_ampleset(StateSpaceRef ss, State *s, LmnReactCxtRef rc,
//                       Vector *new_s, BOOL flag);
//void McPorData::init_por_vars();

/* @} */

#endif
