/*
 * tree_compress_test.c
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

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "tree_compress_test.h"
#include <tree_compress.h>

TreeDatabase treedb = NULL;

int test_tree_init()
{
  if (treedb == NULL) {
    treedb = tree_make(TREE_DB_DEFAULT_SIZE);
  }
  return 0;
}

int test_tree_clean()
{

  if (treedb != NULL) {
    tree_free(treedb);
    treedb = NULL;
  }
  return 0;
}


void print_bs(LmnBinStr bs)
{
  int k;
  for (k = 0; k < lmn_binstr_byte_size(bs); k++)
    printf("%2x ", bs->v[k]);
  printf("\n");
}

#define TEST_1_STR_SIZE (8 * 4 + 6)
#define TEST_1_STR_COUNT 2

void test_tree_001()
{
  LmnBinStr strings[TEST_1_STR_COUNT];
  BOOL found;
  int i, j;

  for (i = 0; i < TEST_1_STR_COUNT; i++) {
    strings[i] = lmn_binstr_make(TEST_1_STR_SIZE);
    for (j = 0; j < TEST_1_STR_SIZE; j++) {
        strings[i]->v[j] = j + 1;
    }
  }

  for (i = 0; i < TEST_1_STR_COUNT; i++) {
    tree_find_or_put(treedb, strings[i], &found);
  }

  for (i = 0; i < TEST_1_STR_COUNT; i++) {
    lmn_binstr_free(strings[i]);
  }
  tree_clear(treedb);
}

#define TEST_2_STR_SIZE (8 * 4 + 4)

void test_tree_002()
{
  LmnBinStr string;
  LmnBinStr ret;
  BOOL found;
  TreeNodeRef ref;
  int i, j;

  string = lmn_binstr_make(TEST_2_STR_SIZE);
  for (i = 0; i < TEST_2_STR_SIZE; i++)
    string->v[i] = i + 1;

  ref = tree_find_or_put(treedb, string, &found);
  ret = tree_get(treedb, ref, string->len);

  CU_ASSERT(binstr_compare(string, ret) == 0);

  lmn_binstr_free(string);
  lmn_binstr_free(ret);
  tree_clear(treedb);
}

void test_tree_003()
{
  LmnBinStr bs;
  LmnBinStr bs_ret;
  TreeNodeRef ref;
  BOOL found;
  int i, j, k;

  srand((unsigned)time(NULL));
  for(i = 0; i < 100; i++) {
    int size = 24 + rand() % 50;
    bs = lmn_binstr_make(size);
    for (j = 0; j < size; j++)
      bs->v[j] = (rand() % 0xFF);

    ref = tree_find_or_put(treedb, bs, &found);
    bs_ret = tree_get(treedb, ref, bs->len);
    CU_ASSERT(binstr_compare(bs, bs_ret) == 0);
    lmn_binstr_free(bs);
    lmn_binstr_free(bs_ret);
  }
  tree_clear(treedb);
}
