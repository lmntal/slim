/*
 * task.h
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
 * $Id: task.h,v 1.5 2008/10/16 18:14:32 sasaki Exp $
 */

#ifndef LMN_TASK_H
#define LMN_TASK_H

/**
 * @ingroup VM
 * @defgroup Task
 * @{
 */

#include "element/element.h"
#include "lmntal.h"
#include "membrane.h"
#include "react_context.hpp"

#include <functional>
#include <vector>

/* 中間命令で出現するデータ構造
 * LINK_LIST    リンクオブジェクトのリスト
 * LIST_AND_MAP 第１要素がリンクオブジェクトのリストで第２要素がマップ
 * MAP          マップ
 */
#define LINK_LIST 1
#define LIST_AND_MAP 2
#define MAP 3

#define SWAP(T, X, Y)                                                          \
  do {                                                                         \
    T t = (X);                                                                 \
    (X) = (Y);                                                                 \
    (Y) = t;                                                                   \
  } while (0)
#define READ_VAL(T, I, X) ((X) = *(T *)(I), I += sizeof(T))
#define REWRITE_VAL(T, I, X) (I -= sizeof(T), *(T *)(I) = (X))
#define SKIP_VAL(T, I) I += sizeof(T)

/* 属性配列ttに使用するタグ */
enum {
  TT_OTHER = 0,
  TT_ATOM = 1, /* symbol atom  */
  TT_MEM = 2   /* membrane */
};

void task_init(void);
void task_finalize(void);
void lmn_dmem_interpret(LmnReactCxtRef rc, LmnRuleRef rule, LmnRuleInstr instr);
void lmn_run(Vector *rulesets);
BOOL react_rule(LmnReactCxtRef rc, LmnMembraneRef mem, LmnRuleRef rule, int ti=0);
void react_start_rulesets(LmnMembraneRef mem, Vector *rulesets);
BOOL react_all_rulesets(LmnReactCxtRef rc, LmnMembraneRef cur_mem, int ti=0);
void memstack_push(LmnMembraneRef mem);
extern struct Vector user_system_rulesets; /* system ruleset defined by user */
HashSet *insertconnectors(slim::vm::RuleContext *rc, LmnMembraneRef mem,
                          const Vector *links);

Vector *links_from_idxs(const Vector *link_idxs, LmnReactCxtRef v);
void free_links(Vector *links);

/* @} */

#endif
