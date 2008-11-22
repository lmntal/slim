/*
 * runtime_status.c
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

#include "runtime_status.h"
#include "mc.h"


struct RuntimeStatus {
  unsigned long atom_num;             /* # of atoms */
  unsigned long membrane_num;         /* # of membranes */
  unsigned long peak_atom_num;        /* peak # of atoms */
  unsigned long peak_membrane_num;    /* peak # of membranes */
  unsigned long atom_space;           /* memory size by atoms (Byte) */
  unsigned long peak_atom_space;      /* peak memory size by atoms (Byte) */
  unsigned long membrane_space;       /* memory size by membranes (Byte) */
  unsigned long peak_membrane_space;  /* peak memory size by membranes (Byte) */
  unsigned long hashtbl_space;        /* internal hash table size (Byte) */
  unsigned long peak_hashtbl_space;   /* peak internal hash table size (Byte) */
  unsigned long total_state_space;          /* total state size */
  unsigned long peak_total_state_space;     /* peal total state size */
  st_table_t hash_conflict_tbl;       /* key: # of conflicts, value: kinds */
  unsigned long hash_num;            /* # of hash value */
} runtime_status;

static void runtime_status_update(void);

void runtime_status_init()
{
  runtime_status.atom_num = 0;
  runtime_status.membrane_num = 0;
  runtime_status.peak_atom_num = 0;
  runtime_status.peak_membrane_num = 0;
  runtime_status.atom_space = 0;
  runtime_status.peak_atom_space = 0;
  runtime_status.membrane_space = 0;
  runtime_status.peak_membrane_space = 0;
  runtime_status.hashtbl_space = 0;
  runtime_status.peak_hashtbl_space = 0;
  runtime_status.total_state_space = 0;
  runtime_status.peak_total_state_space = 0;
  runtime_status.hash_conflict_tbl = st_init_numtable();
  runtime_status.hash_num = 0;
  
}

void runtime_status_finalize()
{
  st_free_table(runtime_status.hash_conflict_tbl);
}

void status_add_atom_space(unsigned long size)
{
  runtime_status.atom_space += size;
  if (runtime_status.atom_space > runtime_status.peak_atom_space)
    runtime_status.peak_atom_space = runtime_status.atom_space;

  runtime_status.atom_num++;
  if (runtime_status.atom_num > runtime_status.peak_atom_num)
    runtime_status.peak_atom_num = runtime_status.atom_num;

  runtime_status_update();
}

void status_remove_atom_space(unsigned long size)
{
  runtime_status.atom_space -= size;
  runtime_status.atom_num--;
}

void status_add_membrane_space(unsigned long size)
{
  runtime_status.membrane_num++;
  runtime_status.peak_membrane_num =
    runtime_status.peak_membrane_num > runtime_status.membrane_num ?
      runtime_status.peak_membrane_num : runtime_status.membrane_num;

  runtime_status.membrane_space += size;
  if (runtime_status.membrane_space > runtime_status.peak_membrane_space)
    runtime_status.peak_membrane_space = runtime_status.membrane_space;

  runtime_status_update();
}

void status_remove_membrane_space(unsigned long size)
{
  runtime_status.membrane_num--;
  runtime_status.membrane_space -= size;
}

void status_add_hashtbl_space(unsigned long size)
{
  runtime_status.hashtbl_space += size;
  if (runtime_status.hashtbl_space > runtime_status.peak_hashtbl_space) 
    runtime_status.peak_hashtbl_space = runtime_status.hashtbl_space;

  runtime_status_update();
}

void status_remove_hashtbl_space(unsigned long size)
{
  runtime_status.hashtbl_space -= size;
}

static void runtime_status_update()
{
  runtime_status.total_state_space =
    runtime_status.atom_space
    + runtime_status.membrane_space
    + runtime_status.hashtbl_space;
  
  if (runtime_status.total_state_space > runtime_status.peak_total_state_space)
    runtime_status.peak_total_state_space = runtime_status.total_state_space;
}

void output_runtime_status(FILE *f)
{
  fprintf(f, "\n== Runtime Status ==========================================\n");

  fprintf(f, "%-30s: %10lu\n", "peak # of atoms", runtime_status.peak_atom_num);
  fprintf(f, "%-30s: %10lu\n", "peak # of membranes",
         runtime_status.peak_membrane_num);
  fprintf(f, "%-30s: %10lu\n", "peak atom space (Bytes)",
         runtime_status.peak_atom_space);
  fprintf(f, "%-30s: %10lu\n", "peak membrane space (Bytes)",
         runtime_status.peak_membrane_space);
  fprintf(f, "%-30s: %10lu\n", "peak hash table space (Bytes)",
         runtime_status.peak_hashtbl_space);
  fprintf(f, "%-30s: %10lu\n", "rough peak state space (Bytes)",
          runtime_status.peak_total_state_space);
  
  fprintf(f, "============================================================\n");
}

static int dispersal_f(st_data_t key, st_data_t s_, st_data_t tbl_)
{
  st_table_t tbl = (st_table_t)tbl_;
  State *s = (State*)s_;
  
  int n;
  if (!st_lookup((st_table_t)tbl, (st_data_t)s->hash, (st_data_t)&n)) {
    n = 0;
  }
  st_insert((st_table_t)tbl, (st_data_t)s->hash, (st_data_t)(n+1));
/*   fprintf(stdout, "%d :: ", s->hash); */
/*   lmn_dump_mem(s->mem); */

  return ST_CONTINUE;
}

static int accum_f(st_data_t hash_value, st_data_t num, st_data_t tbl_)
{
  st_table_t hash_to_values = (st_table_t)tbl_;
  
  int n;
  if (!st_lookup((st_table_t)hash_to_values, (st_data_t)num, (st_data_t)&n)) {
    n = 0;
  }
  st_insert((st_table_t)hash_to_values, (st_data_t)num, (st_data_t)(n+1));
  runtime_status.hash_num++;

  return ST_CONTINUE;
}

void calc_hash_conflict(st_table_t states)
{
  st_table_t hash_to_values;

  hash_to_values = st_init_numtable();

  /* hash_valueはハッシュ値とそのハッシュ値を持つ状態の数表を作る */
  st_foreach(states, dispersal_f, (st_data_t)hash_to_values);
  /* conflict数とconflictしているハッシュ値の種類を集計 */
  st_foreach(hash_to_values, accum_f, (st_data_t)runtime_status.hash_conflict_tbl);

  st_free_table(hash_to_values);
}

static int dispersal_print_f(st_data_t hash_, st_data_t n_, st_data_t f_)
{
  unsigned int hash = (unsigned int)hash_;
  int n = (int)n_;
  FILE *f = (FILE*)f_;

  fprintf(f, "%9d %6d\n", hash, n);

  return ST_CONTINUE;
}

void output_hash_conflict(FILE *f)
{
  fprintf(f, "\n== hash conflict ===========================================\n");
  fprintf(f, "# of hash values : %lu\n", runtime_status.hash_num);
  fprintf(f, "----------------\n");
  fprintf(f, "%9s %6s\n", "conflicts", "kinds");
  st_foreach(runtime_status.hash_conflict_tbl, dispersal_print_f, (st_data_t)f);
  fprintf(f, "----------------\n");
  fprintf(f, "============================================================\n");
}
