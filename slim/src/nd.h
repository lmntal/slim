/*
 * nd.h
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

#ifndef LMN_ND_H
#define LMN_ND_H

#include "lmntal.h"
#include "state.h"
#include "statespace.h"
#include "rule.h"
#include "vector.h"
#include "st.h"
#include "automata.h"

/* prefixにndがつく関数に渡すフラグ */
#define ND_FLAGS_DUMP           (0x01U)
#define ND_FLAGS_POR            (0x01U << 1)
#define ND_FLAGS_HAS_PROPERTY   (0x01U << 2)
#define ND_FLAGS_USE_TRANSITION (0x01U << 3)
#define ND_FLAGS_COMPRESS       (0x01U << 4)
#define ND_FLAGS_COMPACT_STACK  (0x01U << 5)
#define ND_FLAGS_DELTAMEM       (0x01U << 6)

#define is_dump(f)           ((f) & ND_FLAGS_DUMP)
#define set_dump(f)          ((f) |= ND_FLAGS_DUMP)
#define unset_dump(f)        ((f) &= (~ND_FLAGS_DUMP))
#define enable_por(f)        ((f) & ND_FLAGS_POR)
#define set_por(f)           ((f) |= ND_FLAGS_POR)
#define unset_por(f)         ((f) &= (~ND_FLAGS_POR))
#define has_property(f)      ((f) & ND_FLAGS_HAS_PROPERTY)
#define set_property(f)      ((f) |= ND_FLAGS_HAS_PROPERTY)
#define unset_property(f)    ((f) &= (~ND_FLAGS_HAS_PROPERTY))
#define has_trans(f)         ((f) & ND_FLAGS_USE_TRANSITION)
#define set_trans(f)         ((f) |= ND_FLAGS_USE_TRANSITION)
#define unset_trans(f)       ((f) &= (~ND_FLAGS_USE_TRANSITION))
#define use_delta(f)         ((f) & ND_FLAGS_DELTAMEM)
#define set_delta(f)         ((f) |= ND_FLAGS_DELTAMEM)
#define unset_delta(f)       ((f) &= (~ND_FLAGS_DELTAMEM))
#define use_compress(f)      ((f) & ND_FLAGS_COMPRESS)
#define set_compress(f)      ((f) |= ND_FLAGS_COMPRESS)
#define unset_compress(f)    ((f) &= (~ND_FLAGS_COMPRESS))
#define use_compact(f)       ((f) & ND_FLAGS_COMPACT_STACK)
#define set_compact(f)       ((f) |= ND_FLAGS_COMPACT_STACK)
#define unset_compact(f)     ((f) &= (~ND_FLAGS_COMPACT_STACK))

void nd_loop_dfs(StateSpace      states,
                 State           *init_state,
                 struct ReactCxt *rc,
                 BOOL            flags);
void nd_loop_bfs(StateSpace      states,
                 State           *init_state,
                 struct ReactCxt *rc,
                 BOOL            flags);
void expand(const StateSpace states,
            State            *state,
            AutomataState    property_automata_state,
            struct ReactCxt  *rc,
            Vector           *new_s,
            BOOL             flag);
void nd_gen_successors(State           *src,
                       LmnMembrane     *mem,
                       BYTE            prop_labels,
                       struct ReactCxt *rc,
                       BOOL            flags);
void run_nd(Vector *start_rulesets);
StateSpace do_nd(LmnMembrane *world_mem, BOOL flags);

#endif
