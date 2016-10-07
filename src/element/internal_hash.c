/*
 * intrnal_hash.c - hash table for internal use
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
 * $Id: internal_hash.c,v 1.7 2008/10/16 18:06:26 sasaki Exp $
 */



/** CAUTION:
 *
 *    このhashset, hashttblは, ProcessTblやst_tableに比べて性能が非常に悪い.
 *    使用する際には性能面で十分な注意が必要
 *
 */

#include "internal_hash.h"
#include "../config.h"
#include "util.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

/* Hashtable
 *
 *  This hashtable uses 'open addressing with double hashing'.
 *  Once key&value is inserted to hashtable, it can not be deleted.
 *  The size of hashtable is always power of 2. If # of elements / table size >
 *  LOAD_FACTOR then execute rehash.
 *
 *  Key should be lower than DELETED_KEY, because hashtable uses EMPTY_KEY and
 *  DELETED_KEY aa special flags.
 */

#define LOAD_FACTOR 0.75
#define K 2654435761UL
/* maximum capacity */
#define MAX_CAP 0x80000000UL
/* 別に最大容量を制限する必要はないが、制限をしない場合は,
   Kが定数なのでインデックスの計算の時に、hash_valを32bitに
   畳み込む必要がある */

#if SIZEOF_LONG == 4
#  define EMPTY_KEY   0xffffffffUL
#  define DELETED_KEY 0xfffffffeUL
#elif SIZEOF_LONG == 8
#  define EMPTY_KEY   0xffffffffffffffffUL
#  define DELETED_KEY 0xfffffffffffffffeUL
#endif

#define INT_HASH(val)  ((val) * K)

static void hashtbl_extend(SimpleHashtbl *ht);
static struct HashEntry *hashtbl_get_p(SimpleHashtbl *ht, HashKeyType key);
static inline HashKeyType* hashset_get_p(HashSet* set, HashKeyType key, unsigned long dummy_key);

/* HashMap <HashKeyType, HashValueType> */
void hashtbl_init(SimpleHashtbl *ht, unsigned int init_size)
{
  ht->num = 0;
  ht->cap = round2up(init_size);
  ht->tbl = (HashEntry *)malloc(sizeof(struct HashEntry) * ht->cap);
  memset(ht->tbl, 0xffU, sizeof(struct HashEntry) * ht->cap);
}

SimpleHashtbl *hashtbl_make(unsigned int init_size)
{
  SimpleHashtbl *ht = (SimpleHashtbl *)malloc(sizeof(SimpleHashtbl));
  hashtbl_init(ht, init_size);
  return ht;
}

void hashtbl_destroy(SimpleHashtbl *ht)
{
  LMN_FREE(ht->tbl);
}

void hashtbl_free(SimpleHashtbl *ht)
{
  hashtbl_destroy(ht);
  LMN_FREE(ht);
}

HashValueType hashtbl_get(SimpleHashtbl *ht, HashKeyType key)
{
  return hashtbl_get_p(ht, key)->data;
}

HashValueType hashtbl_get_default(SimpleHashtbl *ht,
                                  HashKeyType key,
                                  HashValueType default_value)
{
  HashEntry *e =  hashtbl_get_p(ht, key);
  if (e->key == EMPTY_KEY) return default_value;
  else return e->data;
}

int hashtbl_contains(SimpleHashtbl *ht, HashKeyType key)
{
  return hashtbl_get_p(ht, key)->key != EMPTY_KEY;
}

void hashtbl_put(SimpleHashtbl *ht, HashKeyType key, HashValueType data)
{
  struct HashEntry *e;

  e = hashtbl_get_p(ht, key);
  if (e->key == EMPTY_KEY) {
    ht->num++;
    e->key = key;
  }
  e->data = data;

  if (ht->num > ht->cap * LOAD_FACTOR) {
    hashtbl_extend(ht);
  }
}

void hashtbl_clear(SimpleHashtbl *ht) {
  memset(ht->tbl, 0xffU, sizeof(struct HashEntry) * ht->cap);
}

static struct HashEntry *hashtbl_get_p(SimpleHashtbl *ht, HashKeyType key)
{
  HashKeyType probe;
  HashKeyType increment = (key | 1) & (ht->cap-1);

  for (probe = INT_HASH(key) & (ht->cap-1);
       ht->tbl[probe].key != EMPTY_KEY && ht->tbl[probe].key != key;
       probe = (probe + increment) & (ht->cap-1)) {
  }

  return &ht->tbl[probe];
}

static void hashtbl_extend(SimpleHashtbl *ht)
{
  struct HashEntry *tbl, *e;
  unsigned int i, cap;

  if (ht->cap == MAX_CAP) {
    fprintf(stderr, "hashtable capacity overflow\n");
    exit(1);
  }

  cap = ht->cap;
  tbl = ht->tbl;
  ht->cap <<= 1;
  ht->tbl = (HashEntry *)malloc(sizeof(struct HashEntry) *  ht->cap);
  memset(ht->tbl, 0xffU, sizeof(struct HashEntry) * ht->cap);

  for (i = 0; i < cap; i++) {
    if (tbl[i].key != EMPTY_KEY) {
      e = hashtbl_get_p(ht, tbl[i].key);
      e->key = tbl[i].key;
      e->data = tbl[i].data;
    }
  }
  LMN_FREE(tbl);
}

HashIterator hashtbl_iterator(SimpleHashtbl *ht)
{
  HashIterator iter;
  iter.i = 0;
  iter.ht = ht;
  if (ht->cap > 0 && ht->tbl[iter.i].key >= DELETED_KEY) {
    hashtbliter_next(&iter);
  }
  return iter;
}

void hashtbliter_next(HashIterator *iter)
{
  while (++iter->i < iter->ht->cap &&
         iter->ht->tbl[iter->i].key >= DELETED_KEY) ;
}

/* HashSet <HashKeyType> */
void hashset_init(HashSet *set, unsigned int init_size)
{
  set->num = 0;
  set->cap = round2up(init_size);
  set->tbl = (HashKeyType *)malloc(sizeof(HashKeyType) * set->cap);
  memset(set->tbl, 0xffU, sizeof(HashKeyType) * set->cap);
}

HashSet *hashset_make(unsigned int init_size)
{
  HashSet *hs = (HashSet *)malloc(sizeof(HashSet));
  hashset_init(hs, init_size);
  return hs;
}

void hashset_destroy(HashSet *set)
{
  LMN_FREE(set->tbl);
}

void hashset_free(HashSet *set)
{
  hashset_destroy(set);
  LMN_FREE(set);
}

int hashset_contains(HashSet *set, HashKeyType key)
{
  return *hashset_get_p(set, key, EMPTY_KEY) != EMPTY_KEY;
}

static void hashset_extend(HashSet *set)
{
  HashKeyType *tbl, *entry;
  unsigned int i, cap;

  if (set->cap == MAX_CAP) {
    fprintf(stderr, "hashset capacity overflow\n");
    exit(1);
  }

  cap = set->cap;
  tbl = set->tbl;
  set->cap <<= 1;
  set->tbl = (HashKeyType *)malloc(sizeof(HashKeyType) *  set->cap);
  memset(set->tbl, 0xffU, sizeof(HashKeyType) * set->cap);

  for(i = 0; i < cap; i++) {
    if(tbl[i] != EMPTY_KEY) {
      entry = hashset_get_p(set, tbl[i], DELETED_KEY); /* 新しいindex */
      *entry = tbl[i];
    }
  }
  LMN_FREE(tbl);
}

void hashset_add(HashSet *set, HashKeyType key) {
  HashKeyType* entry;
  LMN_ASSERT(key < DELETED_KEY);

  entry = hashset_get_p(set, key, DELETED_KEY);
  if(*entry == EMPTY_KEY || *entry == DELETED_KEY) {
    set->num++;
    *entry = key;
  }
  if(set->num > set->cap * LOAD_FACTOR) {
    hashset_extend(set);
  }
}

void hashset_clear(HashSet *set) {
  memset(set->tbl, 0xffU, sizeof(HashKeyType) * set->cap);
}

void hashset_delete(HashSet *set, HashKeyType key) {
  HashKeyType* entry;
  LMN_ASSERT(key < DELETED_KEY);

  entry = hashset_get_p(set, key, EMPTY_KEY);
  if(*entry != EMPTY_KEY) {
    set->num--;
    *entry = DELETED_KEY;
  }
  /* EFFICIENCY: hashset_reduce() が必要 */
}

HashSetIterator hashset_iterator(HashSet *set) {
  HashSetIterator it;
  it.i = 0;
  it.set = set;
  if(set->cap > 0 && set->tbl[it.i] >= DELETED_KEY) {
    hashsetiter_next(&it);
  }
  return it;
}

void hashsetiter_next(HashSetIterator *it) {
  while (++it->i < it->set->cap && it->set->tbl[it->i] >=  DELETED_KEY);
}

static inline HashKeyType* hashset_get_p(HashSet* set, HashKeyType key, unsigned long dummykey)
{
  HashKeyType probe;
  HashKeyType increment = (key | 1) & (set->cap-1);

  for (probe = INT_HASH(key) & (set->cap-1);
       set->tbl[probe] < dummykey && set->tbl[probe] != key;
       probe = (probe + increment) & (set->cap-1)) {
  }
  return &set->tbl[probe];
}
