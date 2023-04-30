/*
 * thread.cpp
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

#include "lmntal.h"
#include "vm/vm.h"

/**
 * @brief threadルールセットを登録するためのコールバック
 *
 * @details
 *   膜の中に入っているルールセットを親膜に移動し、元々の膜を削除する。
 *   ただし、callback命令が終わった際、'$callback'アトムがdeleteされるため、
 *   膜のメモリを解放してしまうとメモリアクセス違反が起きる。
 *   現状ではメモリを解放せず親膜からの削除だけ行う。（これはメモリリークになる）
 * @todo
 *   メモリアクセス違反が起きないように膜のメモリを解放できるような仕様にする。
 */
void cb_create_thread(LmnReactCxtRef rc, LmnMembraneRef mem) {
  LmnMembraneRef parent = mem->mem_parent();

  for (int i = 0; i < mem->ruleset_num(); i++) {
    LmnRuleSetRef rs = lmn_mem_get_ruleset(mem, i);
    rs->validate_para_ruleset();
    lmn_mem_add_ruleset(parent, new LmnRuleSet(*rs));
  }

  if (rc->has_mode(REACT_MEM_ORIENTED)) {
    ((MemReactContext *)rc)->memstack_remove(mem);
  }
  // lmn_mem_delete_mem(parent, mem); //< may cause memory error
  parent->remove_mem(mem);
}

void init_thread(void) {
  CCallback::lmn_register_c_fun("thread", (void *)cb_create_thread, 0);
}
