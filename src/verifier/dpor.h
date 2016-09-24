/*
 * dpor.h
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
 * $Id:
 */

#ifndef MC_DPOR_H
#define MC_DPOR_H

/* cldoc:begin-category(Verifier::Dpor) */

#include "../lmntal.h"
#include "delta_membrane.h"
#include "statespace.h"
#include "visitlog.h"
#ifdef DEBUG
# include "dumper.h"
# include "error.h"
# define POR_DEBUG(V) if (lmn_env.debug_por) {(V);}
#else
# define POR_DEBUG(V)
#endif

typedef struct ContextC2 *ContextC2Ref;
typedef struct ContextC1 *ContextC1Ref;

struct McDporData {
  unsigned int cur_depth;

  ContextC1Ref tmp;  /* ちょっと退避する場所 */
  ContextC2Ref c2;

  Vector     *wt_gatoms;  /* マッチング中, ground命令によるProcessTblを集める作業場 */
  ProcessTableRef wt_flags;    /* マッチング中, プロセスIDに対するフラグを設定していく作業場 */

  Vector *ample_cand;     /* ample setに含める予定のContextC1へのポインタを積む */
  st_table_t delta_tbl;   /* MemDeltaRootをkey, ContextC1をvalue */
  Vector *free_deltas;    /* ゴミ置き場 */
  unsigned int nxt_tr_id; /* 遷移に割り当てる遷移番号 */
};

extern McDporData **dpor_data;

#define DPOR_DATA()        (dpor_data[env_my_thread_id()])


#define LHS_DEFAULT                (0x00U)
#define LHS_MEM_GROOT              (0x01U)
#define LHS_MEM_NATOMS             (0x01U << 1)
#define LHS_MEM_NMEMS              (0x01U << 2)
#define LHS_MEM_NFLINKS            (0x01U << 3)
#define LHS_MEM_NORULES            (0x01U << 4)
#define LHS_MEM_STABLE             (0x01U << 5)

#define LHS_FL_SET(F, N)           ((F) |= (N))
#define LHS_FL_UNSET(F, N)         ((F) &= ~(N))
#define LHS_FL(F, S)               ((F) & (S))


#define OP_NONE                  (0x00U)
#define OP_DEP_EXISTS            (0x01U)
#define OP_DEP_EXISTS_EX_GROOT   (0x01U << 1)
#define OP_DEP_NATOMS            (0x01U << 2)
#define OP_DEP_NMEMS             (0x01U << 3)
#define OP_DEP_NFLINKS           (0x01U << 4)
#define OP_DEP_NORULES           (0x01U << 5)
#define OP_DEP_STABLE            (0x01U << 6)

#define RHS_OP_SET(F, S)   ((F) |= (S))
#define RHS_OP_UNSET(F, S) ((F) &= ~(S))
#define RHS_OP(F, S)       ((F) & (S))


void dpor_explore_redundunt_graph(StateSpaceRef ss);


void dpor_start(StateSpaceRef  ss,
                State       *s,
                LmnReactCxt *rc,
                Vector      *new_s,
                BOOL flag);

void dpor_env_init(void);
void dpor_env_destroy(void);

void dpor_transition_gen_LHS(McDporData   *mc,
                             MemDeltaRoot *d,
                             LmnReactCxt  *rc,
                             LmnRegister  *v);
BOOL dpor_transition_gen_RHS(McDporData   *mc,
                             MemDeltaRoot *d,
                             LmnReactCxt  *rc,
                             LmnRegister  *v);


void dpor_LHS_flag_add(McDporData *d, LmnWord proc_id, BYTE set_f);

void dpor_LHS_flag_remove(McDporData *d, LmnWord proc_id, BYTE unset_f);

void dpor_LHS_add_ground_atoms(McDporData *d, ProcessTableRef atoms);

void dpor_LHS_remove_ground_atoms(McDporData *d, ProcessTableRef atoms);



/* for debug only */
void dpor_contextC1_dump_eachL(ContextC1Ref c);
void dpor_contextC1_dump_eachR(ContextC1Ref c);
void dpor_contextC1_dump(McDporData *d);
int  dpor_dependency_tbl_dump(McDporData *d);

/* cldoc:end-category() */

#endif
