/*
 * intrnal_hash.h
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
 * $Id: internal_hash.h,v 1.5 2008/10/16 18:06:34 sasaki Exp $
 */

#ifndef INTERNAL_HASH_H
#define INTERNAL_HASH_H

/**
 * @ingroup Element
 * @defgroup Hash
 * @{
 */

/* HashMap */
typedef unsigned long HashKeyType;
typedef unsigned long HashValueType;

typedef struct HashEntry {
  HashKeyType key;
  HashValueType data;
} HashEntry;

typedef struct SimpleHashtbl {
  struct HashEntry *tbl;
  unsigned int cap, num;
} SimpleHashtbl;

typedef struct HashIterator {
  SimpleHashtbl *ht;
  unsigned int i;
} HashIterator;

void hashtbl_init(SimpleHashtbl *ht, unsigned int init_size);
SimpleHashtbl *hashtbl_make(unsigned int init_size);
HashValueType hashtbl_get(SimpleHashtbl *ht, HashKeyType key);
HashValueType hashtbl_get_default(SimpleHashtbl *ht, HashKeyType key,
                                  HashValueType default_value);
int hashtbl_contains(SimpleHashtbl *ht, HashKeyType key);
void hashtbl_put(SimpleHashtbl *ht, HashKeyType key, HashValueType val);
void hashtbl_clear(SimpleHashtbl *ht);
void hashtbl_destroy(SimpleHashtbl *ht);
void hashtbl_free(SimpleHashtbl *ht);
#define hashtbl_num(HT) (HT)->num

HashIterator hashtbl_iterator(SimpleHashtbl *ht);
void hashtbliter_next(HashIterator *iter);
#define hashtbliter_entry(I) (&((I)->ht->tbl[(I)->i]))
#define hashtbliter_isend(I) ((I)->i >= (I)->ht->cap)

/* HashSet */
struct HashSet {
  HashKeyType *tbl;
  unsigned int cap, num;

  HashSet(unsigned int init_size);
};

typedef struct HashSetItrator {
  HashSet *set;
  unsigned int i;
} HashSetIterator;

int hashset_contains(HashSet *set, HashKeyType key);
void hashset_add(HashSet *set, HashKeyType key);
void hashset_clear(HashSet *set);
void hashset_delete(HashSet *set, HashKeyType key);
void hashset_free(HashSet *set);
void hashset_destroy(HashSet *set);
HashSetIterator hashset_iterator(HashSet *set);
void hashsetiter_next(HashSetIterator *it);
#define hashset_num(HT) (HT)->num

#define hashsetiter_entry(I) ((I)->set->tbl[(I)->i])
#define hashsetiter_isend(I) ((I)->i >= (I)->set->cap)

inline static unsigned long internal_hashtbl_space_inner(SimpleHashtbl *ht) {
  return (sizeof(struct HashEntry) * ht->cap);
}

inline static unsigned long internal_hashtbl_space(SimpleHashtbl *ht) {
  return sizeof(struct SimpleHashtbl) + internal_hashtbl_space_inner(ht);
}

/* cldoc:end-category() */

#endif /*INTERNAL_HASH_H*/
