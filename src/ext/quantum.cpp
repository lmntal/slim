/*
 * ext/quantum.cpp
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
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
 */

#include "vm/vm.h"
#include "quantum.hpp"

namespace slim {
namespace ext {
int Quantum::atom_type;

namespace quantum {

namespace callback {
void create(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0,
            LmnLinkAttr t0) {
  auto q = new Quantum;
  LMN_SP_ATOM_SET_TYPE(q, Quantum::atom_type);
  lmn_mem_push_atom(mem, q, LMN_SP_ATOM_ATTR);
  lmn_mem_newlink(mem,
                  a0, t0, LMN_ATTR_GET_VALUE(t0),
                  q, LMN_SP_ATOM_ATTR, 0);
}
}  // namespace callback

void *copy(void *atom) {
  auto q = new Quantum(*(Quantum *)atom);
  LMN_SP_ATOM_SET_TYPE(q, Quantum::atom_type);
  return q;
}

void free(void *atom) {
  delete (Quantum *)atom;
}

std::vector<uint8_t> encode(void *atom) {
  return ((Quantum *)atom)->encode();
}

void *decode(const std::vector<uint8_t> &bytes) {
  auto q = new Quantum(bytes);
  LMN_SP_ATOM_SET_TYPE(q, Quantum::atom_type);
  return q;
}

void dump(void *atom, LmnPortRef port) {
  port_put_raw_s(port, reinterpret_cast<Quantum *>(atom)->to_string().c_str());
}

BOOL is_ground(void *atom) { return true; }

BOOL equals(void *atom1, void *atom2) {
  auto q1 = reinterpret_cast<Quantum *>(atom1);
  auto q2 = reinterpret_cast<Quantum *>(atom2);
  if (q1->constraints->is_int() != q2->constraints->is_int()) return false;
  if (q1->constraints->is_int() == tru)
    return q1->constraints->assumed_range == q2->constraints->assumed_range;
  else
    return true;
}
}  // namespace quantum
}  // namespace ext
}  // namespace slim

using namespace slim::ext::quantum;

void init_quantum() {
  slim::ext::Quantum::atom_type = lmn_sp_atom_register(
      "quantum", copy, slim::ext::quantum::free, equals, dump, is_ground, encode, decode);
  lmn_register_c_fun("quantum_create", (void *)callback::create, 1);
}
