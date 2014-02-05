/*
 * mc_explorer.h
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

#ifndef LMN_MC_EXP_H
#define LMN_MC_EXP_H

#include "state.h"
#include "mc_worker.h"
#include "stack_macro.h"


/* Nested-DFSが起動するための条件 */
#define NDFS_COND(W, SYST_S, PROP_S)    (!worker_on_parallel(W)           \
                                          && worker_use_ndfs(W)           \
                                          && atmstate_is_accept(PROP_S)   \
                                          && !is_snd(SYST_S)              \
                                          && !is_on_cycle(SYST_S))

#define MAPNDFS_COND(W, SYST_S, PROP_S)    (worker_on_parallel(W)          \
                                          && worker_use_mapndfs(W)         \
                                          &&atmstate_is_accept(PROP_S)    \
                                          && !is_snd(SYST_S)              \
                                          && !is_on_cycle(SYST_S)         \
                                           && s_is_visited_by_explorer(SYST_S)           \
                                           && worker_is_explorer(W))

#define MCNDFS_COND(W, SYST_S, PROP_S)    (atmstate_is_accept(PROP_S))

#define OWCTY_COND(W)                   (worker_use_owcty(W)              \
                                          && !w->group->mc_exit)

#define MAP_COND(W)                     (worker_use_map(W)                \
                                          && !w->group->mc_exit)

#define BLEDGE_COND(W)                  (worker_use_ble(W)                \
                                          && !w->group->mc_exit)

#define MC_MAP_MASK                     (0x01U)
#define MC_MAP2_MASK                    (0x01U << 1)
#define MC_BLE_MASK                     (0x01U << 2)


#define smap_set_deleted(S)       (OR_AND_FETCH((S)->flags2,  MC_MAP_MASK))
#define smap_unset_deleted(S)     (AND_AND_FETCH((S)->flags2, MC_MAP_MASK))
#define smap_is_deleted(S)        ((S)->flags2 & MC_MAP_MASK)
#define smap_set_not_delete(S)    (OR_AND_FETCH((S)->flags2,  MC_MAP2_MASK))
#define smap_unset_not_delete(S)  (AND_AND_FETCH((S)->flags2, MC_MAP2_MASK))
#define smap_is_not_delete(S)     ((S)->flags2 & MC_MAP2_MASK)

#define sble_set_on_stack(S)      (OR_AND_FETCH((S)->flags2,  MC_BLE_MASK))
#define sble_unset_on_stack(S)    (AND_AND_FETCH((S)->flags2, MC_BLE_MASK))
#define sble_is_on_stack(S)       ((S)->flags2 & MC_BLE_MASK)


/** prototypes
 */

void backward_elimination(LmnWorker *w, State *s);

void ndfs_env_set(LmnWorker *w);
void ndfs_start(LmnWorker *w, State *seed);
void ndfs_worker_init(LmnWorker *w);
void ndfs_worker_finalize(LmnWorker *w);
void ndfs_worker_start(LmnWorker *w);

void owcty_env_set(LmnWorker *w);
void owcty_start(LmnWorker *w);
void owcty_worker_finalize(LmnWorker *w);
void owcty_worker_init(LmnWorker *w);

void map_env_set(LmnWorker *w);
void map_start(LmnWorker *w, State *u);
void map_iteration_start(LmnWorker *w);
void map_worker_finalize(LmnWorker *w);
void map_worker_init(LmnWorker *w);

void bledge_env_set(LmnWorker *w);
void bledge_start(LmnWorker *w);
void bledge_store_layer(LmnWorker *w, State *s);
void bledge_worker_finalize(LmnWorker *w);
void bledge_worker_init(LmnWorker *w);

void mapndfs_env_set(LmnWorker *w);
void mapndfs_start(LmnWorker *w, State *seed);
void mapndfs_worker_init(LmnWorker *w);
void mapndfs_worker_finalize(LmnWorker *w);
void mapndfs_worker_start(LmnWorker *w);

void mcndfs_env_set(LmnWorker *w);
void mcndfs_start(LmnWorker *w, State *seed, Vector *red_states);
void mcndfs_worker_init(LmnWorker *w);
void mcndfs_worker_finalize(LmnWorker *w);
void mcndfs_worker_start(LmnWorker *w);

#endif
