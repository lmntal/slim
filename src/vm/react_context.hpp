/*
 * react_context.hpp
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
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
/* ルールの適用時に使用するデータ */

#ifndef LMN_REACT_CONTEXT_HPP
#define LMN_REACT_CONTEXT_HPP
	struct LmnRegister {
		LmnWord wt;
		LmnByte at;
		LmnByte tt;
		LmnWord register_wt() { return this->wt; }
		LmnByte register_at() { return this->at; }
		LmnByte register_tt() { return this->tt; }
		void register_set_wt(LmnWord wt) { this->wt = wt; }
		void register_set_at(LmnByte at) { this->at = at; }
		void register_set_tt(LmnByte tt) { this->tt = tt; }



	};
	struct LmnReactCxt {
	  LmnMembraneRef
	      global_root; /* ルール適用対象となるグローバルルート膜. != wt[0] */
	  LmnRegisterArray work_array; /* ルール適用レジスタ */
	  unsigned int warray_cur;     /* work_arrayの現在の使用サイズ */
	  unsigned int warray_num; /* work_arrayの最大使用サイズ(SPEC命令指定) */
	  unsigned int warray_cap; /* work_arrayのキャパシティ */
	  unsigned int trace_num; /* ルール適用回数 (通常実行用トレース実行で使用)  */
	  LmnRulesetId
	      atomic_id; /* atomic step中: atomic set id(signed int), default:-1 */
	  ProcessID proc_org_id; /* atomic step終了時に Process ID をこの値に復帰 */
	  ProcessID proc_next_id; /* atomic step継続時に Process ID をこの値に設定 */
	  LmnMembraneRef cur_mem; /* atomic step継続時に現在膜をこの値に設定 */
	  BYTE mode;
	  BOOL flag;                     /* mode以外に指定するフラグ */
	  void *v;                       /* 各mode毎に固有の持ち物 */
	  SimpleHashtbl *hl_sameproccxt; /* findatom
	                                    時のアトム番号と、同名型付きプロセス文脈を持つアトム引数との対応関係を保持
	                                  */
	#ifdef USE_FIRSTCLASS_RULE
	  Vector *insertion_events;
	#endif
	};

#endif
