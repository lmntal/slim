/*
 * mem_encode.h
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

#ifndef LMN_MEM_ENCODE_H
#define LMN_MEM_ENCODE_H

/**
 * @ingroup  Verifier
 * @defgroup MembraneEncoder
 * @{
 */

#include "../lmntal.h"
#include "delta_membrane.h"
#include "mem_encode/lmn_binstr.hpp"

#define BS_COMP_Z (0x01U)
#define BS_COMP_D (0x01U << 1)

#define is_comp_z(BS) (((BS)->type) & BS_COMP_Z)
#define set_comp_z(BS) (((BS)->type) |= BS_COMP_Z)
#define unset_comp_z(BS) (((BS)->type) &= ~(BS_COMP_Z))
#define is_comp_d(BS) (((BS)->type) & BS_COMP_D)
#define set_comp_d(BS) (((BS)->type) |= BS_COMP_D)
#define unset_comp_d(BS) (((BS)->type) &= ~(BS_COMP_D))

#define TAG_BIT_SIZE 4
#define TAG_DATA_TYPE_BIT 2
#define TAG_IN_BYTE 2

#define lmn_binstr_byte_size(bs) ((bs->len + 1) / TAG_IN_BYTE)

void mem_isom_init(void);
void mem_isom_finalize(void);
void set_functor_priority(LmnFunctor f, int priority);

LmnBinStrRef   lmn_mem_encode(LmnMembraneRef mem);
LmnBinStrRef   lmn_mem_encode_delta(struct MemDeltaRoot *d);
int            binstr_compare(const LmnBinStrRef a, const LmnBinStrRef b);
unsigned long  binstr_hash(const LmnBinStrRef a);
int            binstr_byte_size(LmnBinStrRef p);
LmnBinStrRef   lmn_binstr_make(unsigned int size);
LmnBinStrRef   lmn_binstr_copy(LmnBinStrRef src_bs);
LmnMembraneRef lmn_binstr_decode(const LmnBinStrRef bs);

BOOL lmn_mem_equals_enc(LmnBinStrRef bs, LmnMembraneRef mem);

void lmn_binstr_free(LmnBinStrRef p);
// void lmn_binstr_dump(const LmnBinStrRef bs);
unsigned long lmn_binstr_space(struct LmnBinStr *bs);
LmnBinStrRef  lmn_mem_to_binstr(LmnMembraneRef mem);
LmnBinStrRef  lmn_mem_to_binstr_delta(struct MemDeltaRoot *d);

/* @} */

#endif /* LMN_MEM_ENCODE_H */
