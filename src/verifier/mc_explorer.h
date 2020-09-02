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

/**
 * @ingroup  Verifier
 * @defgroup Explorer
 * @{
 */

#include "mc_worker.h"
#include "stack_macro.h"

/* Nested-DFSが起動するための条件 */
#define NDFS_COND(W, SYST_S, PROP_S)                                           \
  (!worker_on_parallel(W) && worker_use_ndfs(W) && PROP_S->get_is_accept() &&  \
   !SYST_S->is_snd() && !SYST_S->is_on_cycle())

#define MAPNDFS_COND(W, SYST_S, PROP_S)                                        \
  (worker_on_parallel(W) && worker_use_mapndfs(W) &&                           \
   PROP_S->get_is_accept() && !SYST_S->is_snd() && !SYST_S->is_on_cycle() &&   \
   SYST_S->s_is_visited_by_explorer() && worker_is_explorer(W))

#define MCNDFS_COND(W, SYST_S, PROP_S) (PROP_S->get_is_accept())

#define OWCTY_COND(W) (worker_use_owcty(W) && !w->group->workers_are_exit())

#define MAP_COND(W) (worker_use_map(W) && !w->group->workers_are_exit())

#define BLEDGE_COND(W) (worker_use_ble(W) && !w->group->workers_are_exit())

#define MC_MAP_MASK (0x01U)
#define MC_MAP2_MASK (0x01U << 1)
#define MC_BLE_MASK (0x01U << 2)

#define smap_set_deleted(S) (OR_AND_FETCH((S)->flags2, MC_MAP_MASK))
#define smap_unset_deleted(S) (AND_AND_FETCH((S)->flags2, MC_MAP_MASK))
#define smap_is_deleted(S) ((S)->flags2 & MC_MAP_MASK)
#define smap_set_not_delete(S) (OR_AND_FETCH((S)->flags2, MC_MAP2_MASK))
#define smap_unset_not_delete(S) (AND_AND_FETCH((S)->flags2, MC_MAP2_MASK))
#define smap_is_not_delete(S) ((S)->flags2 & MC_MAP2_MASK)

#define sble_set_on_stack(S) (OR_AND_FETCH((S)->flags2, MC_BLE_MASK))
#define sble_unset_on_stack(S) (AND_AND_FETCH((S)->flags2, MC_BLE_MASK))
#define sble_is_on_stack(S) ((S)->flags2 & MC_BLE_MASK)

/** prototypes
 */

void backward_elimination(LmnWorker *w, State *s);

void ndfs_start(LmnWorker *w, State *seed);

void owcty_start(LmnWorker *w);

void map_start(LmnWorker *w, State *u);
void map_iteration_start(LmnWorker *w);

void bledge_start(LmnWorker *w);
void bledge_store_layer(LmnWorker *w, State *s);

void mapndfs_start(LmnWorker *w, State *seed);

void mcndfs_start(LmnWorker *w, State *seed, Vector *red_states);

namespace slim {
namespace verifier {
namespace tactics {

struct NDFS : public slim::verifier::StateExplorer {
  Vector *open;
  Vector *path;

  NDFS(LmnWorker *owner) : StateExplorer(owner) {
    type |= WORKER_F2_MC_NDFS_MASK;
  }

  void initialize(LmnWorker *w);
  void finalize(LmnWorker *w);
  void start(State *seed);

private:
  void ndfs_found_accepting_cycle(LmnWorker *w, State *seed,
                                  Vector *cycle_path);
  BOOL ndfs_loop(State *seed, Vector *search, Vector *path);
};

struct OWCTY : public slim::verifier::StateExplorer {
  Queue *accepts1;
  Queue *accepts2;
  unsigned long old;
  unsigned long iteration;
  st_table_t traversed; /* 反例生成用 */

  OWCTY(LmnWorker *owner) : StateExplorer(owner) {
    type |= WORKER_F2_MC_OWCTY_MASK;
  }

  void initialize(LmnWorker *w);
  void finalize(LmnWorker *w);
  void start(LmnWorker *u);

private:
  static void owcty_report_midterm(LmnWorker *w);
  static void owcty_termination_detection(LmnWorker *w);
  static void owcty_env_init(LmnWorker *w);
  void owcty_reachability(LmnWorker *w, Queue *primary, Queue *secondary,
                          BOOL set_flag, BOOL is_up);
  BOOL owcty_traversed_owner_is_me(State *succ, BOOL set_flag, BOOL is_up);
  void owcty_found_accepting_cycle(LmnWorker *w, AutomataRef a);
};

struct MAP : public slim::verifier::StateExplorer {
  Queue *propagate;
  Queue *waitingSeed;
  st_table_t traversed;

  MAP(LmnWorker *owner) : StateExplorer(owner) {
    type |= WORKER_F2_MC_MAP_MASK;
  }

  void initialize(LmnWorker *w);
  void finalize(LmnWorker *w);
  void start(State *u);

private:
  State *map_ordering_propagate_state(LmnWorker *w, State *u, AutomataRef a);
  void map_propagate(LmnWorker *w, State *s, State *t, State *propag,
                     AutomataRef a);
  BOOL map_entry_state(State *t, State *propag, AutomataRef a);
  State *map_ordering(State *s1, State *s2, AutomataRef a);
  State *map_ordering_states(AutomataRef a, unsigned int num, ...);
  void map_found_accepting_cycle(LmnWorker *w, State *s);
};

#define MAPNDFS_USE_MAP

struct MAP_NDFS : public slim::verifier::StateExplorer {
#ifdef MAPNDFS_USE_MAP
  Queue *propagate;
  Queue *waitingSeed;
  st_table_t traversed;
#endif
  Vector *open;
  Vector *path;

  MAP_NDFS(LmnWorker *owner) : StateExplorer(owner) {
    type |= WORKER_F2_MC_MAPNDFS_MASK;
  }

  void initialize(LmnWorker *w);
  void finalize(LmnWorker *w);
  void start(State *u);

  void mapndfs_found_accepting_cycle(LmnWorker *w, State *seed,
                                     Vector *cycle_path);

private:
  BOOL mapndfs_loop(State *seed, Vector *search, Vector *path);
};

#ifndef MINIMAL_STATE
struct MultiNDFS : public slim::verifier::StateExplorer {
#ifdef MAPNDFS_USE_MAP
  Queue *propagate;
  Queue *waitingSeed;
  st_table_t traversed;
#endif
  Vector *open;
  Vector *path;

  MultiNDFS(LmnWorker *owner) : StateExplorer(owner) {
    type |= WORKER_F2_MC_MCNDFS_MASK;
  }

  void initialize(LmnWorker *w);
  void finalize(LmnWorker *w);
  void start(LmnWorker *w, State *seed, Vector *red_states);

private:
  void mcndfs_found_accepting_cycle(LmnWorker *w, State *seed,
                                    Vector *cycle_path);
  BOOL mcndfs_loop(LmnWorker *w, State *seed, Vector *search, Vector *path,
                   Vector *red_states);
};
#endif

struct BLE : public slim::verifier::StateExplorer {
  Queue *layer;
  Vector *path;
  Vector *search;
  st_table_t traversed;

  BLE(LmnWorker *owner) : StateExplorer(owner) {
    type |= WORKER_F2_MC_BLE_MASK;
  }

  void initialize(LmnWorker *w);
  void finalize(LmnWorker *w);
  void start();
  void store_layer(LmnWorker *w, State *s);

private:
  void bledge_found_accepting_cycle(LmnWorker *w, Vector *cycle_path);
  BOOL bledge_explorer_accepting_cycle(LmnWorker *w, State *u, State *v);
  BOOL bledge_path_accepting(Vector *v, AutomataRef a);
};
} // namespace tactics
} // namespace verifier
} // namespace slim

/* @} */

#endif
