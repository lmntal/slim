/*
 * mc_generator.h
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

#ifndef LMN_MC_GEN_H
#define LMN_MC_GEN_H

/**
 * @ingroup  Verifier
 * @defgroup Generator
 * @{
 */

#include "../lmntal.h"
#include "mc_worker.h"
#include "stack_macro.h"

/** prototypes
 */

namespace slim {
namespace verifier {
namespace tactics {

struct DFS : public slim::verifier::StateGenerator {
  struct Vector stack;
  Deque deq;
  unsigned int cutoff_depth;

  DFS(LmnWorker *owner) : StateGenerator(owner), deq(8192) {
    type |= WORKER_F1_MC_DFS_MASK;
  }

  void initialize(LmnWorker *w);
  void finalize(LmnWorker *w);
  void start();
  bool check();

private:
  std::unique_ptr<Queue> q;

  /* 他のワーカーothersを未展開状態を奪いに巡回する.
   * 未展開状態を発見した場合, そのワーカーのキューからdequeueして返す.
   * 発見できなかった場合, NULLを返す */
  template <class Container> State *steal_unexpand_state(const Container &others) {
    for (auto &dst : others) {
      auto gen = (DFS *)dst.strategy.generator.get();
      if (worker_is_active(&dst) && !gen->q->is_empty()) {
        worker_set_active(owner);
        worker_set_stealer(owner);
        return (State *)gen->q->dequeue();
      }
    }
    return nullptr;
  }

  void handoff_all_tasks(Vector *expands, LmnWorker &rn);
  void handoff_task(State *task, LmnWorker &rn);

#ifndef MINIMAL_STATE
  void mcdfs_start(LmnWorker *w);
#endif
  void dfs_loop(LmnWorker *w, Vector *stack, Vector *new_ss, AutomataRef a,
                Vector *psyms);
  void mapdfs_loop(LmnWorker *w, Vector *stack, Vector *new_ss, AutomataRef a,
                   Vector *psyms);
  void mcdfs_loop(LmnWorker *w, Vector *stack, Vector *new_ss, AutomataRef a,
                  Vector *psyms);
  void costed_dfs_loop(LmnWorker *w, Deque *deq, Vector *new_ss, AutomataRef a,
                       Vector *psyms);
};

struct BFS : public slim::verifier::StateGenerator {
  Queue *cur; /* 現在のFront Layer */
  Queue *nxt; /* 次のLayer */

  BFS(LmnWorker *owner) : StateGenerator(owner) {
    type |= WORKER_F1_MC_BFS_MASK;
  }

  void initialize(LmnWorker *w);
  void finalize(LmnWorker *w);
  void start();
  bool check();

private:
  void bfs_loop(LmnWorker *w, Vector *new_ss, AutomataRef a, Vector *psyms);
};

} // namespace tactics
} // namespace verifier
} // namespace slim

/* @} */

#endif
