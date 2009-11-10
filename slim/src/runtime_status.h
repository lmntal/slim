/*
 * runtime_status.h
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 *                                          <lmntal@ueda.info.waseda.ac.jp>
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

#ifndef RUNTIME_STATUS_H
#define RUNTIME_STATUS_H

#include "stdio.h"
#include "st.h"
#include "nd.h"
#include "rule.h"

void runtime_status_init(void);
void runtime_status_finalize(void);
void status_add_atom_space(unsigned long size);
void status_remove_atom_space(unsigned long size);
void status_add_membrane_space(unsigned long size);
void status_remove_membrane_space(unsigned long size);
void status_add_rule_space(unsigned long size);
void status_remove_rule_space(unsigned long size);
void status_add_hashtbl_space(unsigned long size);
void status_remove_hashtbl_space(unsigned long size);
void status_start_state_hash_calc(void);
void status_finish_state_hash_calc(void);
void status_start_state_free(void);
void status_finish_state_free(void);
void status_start_mem_equals_calc(void);
void status_finish_mem_equals_calc(void);
void status_start_mem_enc_eq_calc(void);
void status_finish_mem_enc_eq_calc(void);
void status_start_mem_dump_calc(void);
void status_finish_mem_dump_calc(void);
void status_start_mem_encode_calc(void);
void status_finish_mem_encode_calc(void);
void status_start_mem_decode_calc(void);
void status_finish_mem_decode_calc(void);
void output_runtime_status(FILE *f);
void status_start_running(void);
void status_finish_running(void);
void status_nd_start_running(void);
void status_nd_finish_running(void);
void status_start_rule(void);
void status_finish_rule(LmnRule rule, BOOL result);
void status_rule_output(LmnRule rule);
void status_backtrack_counter(void);
void status_start_expand(void);
void status_finish_expand(void);
void status_start_commit(void);
void status_finish_commit(void);
void status_count_counterexample(void);
void status_set_state_num(unsigned long n);
void status_create_new_state(void);
void calc_state_space_space(StateSpace states);
void status_nd_push_stack(void);
void status_nd_pop_stack(void);
void status_state_space(StateSpace states);
void status_binstr_make(LmnBinStr bs);
void status_binstr_free(LmnBinStr bs);

#endif
