/*
 * membrane.hpp
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
 */


#ifndef LMN_MEMBRANE_HPP
#define LMN_MEMBRANE_HPP
/*----------------------------------------------------------------------
 * Membrane
 */


#include "lmntal.h"
#include "../element/vector.h"

#include <map>

struct LmnMembrane;
typedef struct LmnMembrane *LmnMembraneRef;
typedef struct AtomListEntry **AtomSet;

struct LmnMembrane {
  AtomSet atomset;
  ProcessID id;
  unsigned int max_functor;
  unsigned int atomset_size;
  unsigned int atom_symb_num; /* # of symbol atom except proxy */
  unsigned int atom_data_num;
  lmn_interned_str name;
  BOOL is_activated;
  LmnMembraneRef parent;
  LmnMembraneRef child_head;
  LmnMembraneRef prev, next;
  struct Vector rulesets;
#ifdef USE_FIRSTCLASS_RULE
  Vector *firstclass_rulesets;
#endif

  std::map<LmnFunctor, AtomListEntry *> atom_lists() const {
    std::map<LmnFunctor, AtomListEntry *> res;
    for (int i = 0; i < max_functor; i++)
      if (atomset[i])
        res[i] = atomset[i];
    return res;
  }
};

#endif