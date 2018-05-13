/*
 * tree_compress.cpp
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

/** @author Taketo Yoshida
 *  Parallel Recursive State Compression for Free
 */
#include "tree_compress.h"
#include <math.h>

#define atomic_fetch_and_inc(t) __sync_fetch_and_add(t, 1)
#define atomic_fetch_and_dec(t) __sync_fetch_and_sub(t, 1)
#define atomic_compare_and_swap(t, old, new)                                   \
  __sync_bool_compare_and_swap(t, old, new)

#define TREE_UNIT_SIZE 8
#define TREE_THRESHOLD 10
#define TREE_CACHE_LINE 8

typedef struct TreeNodeStr *TreeNodeStrRef;

struct TreeNodeStr {
  TreeNodeElement *nodes;
  int len;
  int extra;
};

uint64_t murmurhash64(const void *key, int len, unsigned int seed) {
  const uint64_t m = 0xc6a4a7935bd1e995;
  const int r = 47;

  uint64_t h = seed ^ (len * m);

  const uint64_t *data = (const uint64_t *)key;
  const uint64_t *end = data + (len / 8);

  while (data != end) {
    uint64_t k = *data++;

    k *= m;
    k ^= k >> r;
    k *= m;

    h ^= k;
    h *= m;
  }

  const unsigned char *data2 = (const unsigned char *)data;

  switch (len & 7) {
  case 7:
    h ^= ((uint64_t)data2[6]) << 48;
  case 6:
    h ^= ((uint64_t)data2[5]) << 40;
  case 5:
    h ^= ((uint64_t)data2[4]) << 32;
  case 4:
    h ^= ((uint64_t)data2[3]) << 24;
  case 3:
    h ^= ((uint64_t)data2[2]) << 16;
  case 2:
    h ^= ((uint64_t)data2[1]) << 8;
  case 1:
    h ^= ((uint64_t)data2[0]);
    h *= m;
  };

  h ^= h >> r;
  h *= m;
  h ^= h >> r;

  return h;
}

uint64_t mix64(uint64_t key) {
  key = (~key) + (key << 21); // key = (key << 21) - key - 1;
  key = key ^ (((int64_t)key) >> 24);
  key = (key + (key << 3)) + (key << 8); // key * 265
  key = key ^ (((int64_t)key) >> 14);
  key = (key + (key << 2)) + (key << 4); // key * 21
  key = key ^ (((int64_t)key) >> 28);
  key = key + (key << 31);
  return key;
}

int tree_get_split_position(int start, int end) {
  int size = end - start;
  return (int)(size / 2.0);
}

TreeNodeUnit vector_unit(TreeNodeStrRef str, int start, int end) {
  unsigned long long ret;
  int copy_len = TREE_UNIT_SIZE;
  if (str->extra && end == str->len - 1) {
    copy_len = str->extra;
  }
  // printf("start :%d\n", start * TREE_UNIT_SIZE);
  // printf("copy_len :%d\n", copy_len);
  memcpy(&ret, ((BYTE *)str->nodes + (start * TREE_UNIT_SIZE)),
         sizeof(BYTE) * copy_len);
  // printf("start :0x%14llx\n", ret);
  return ret;
}

uint64_t hash_node(TreeNodeElement left, TreeNodeElement right) {
  struct TreeNode node = {.left = left, .right = right};
  return murmurhash64(&node, 16, 0x5bd1e995);
}

BOOL is_compress_node(TreeNodeElement left, TreeNodeElement right) {
  return ((left & 0x00000000FFFFFFFF) == left &&
          (right & 0x00000000FFFFFFFF) == right);
}

BOOL tree_node_equal(TreeNodeRef node1, TreeNodeElement left,
                     TreeNodeElement right) {
  return node1->left == left && node1->right == right;
}

LmnBinStrRef binstr_make(unsigned int len) {
  int real_len = ((len + 1) / TAG_IN_BYTE);
  LmnBinStrRef bs = LMN_MALLOC(struct LmnBinStr);
  bs->len = len;
  bs->type = 0x00U;
  bs->v = LMN_NALLOC(BYTE, real_len);
  memset(bs->v, 0x0U, sizeof(BYTE) * real_len);
  return bs;
}

TreeNodeRef tree_node_make(TreeNodeElement left, TreeNodeElement right) {
  TreeNodeRef node = LMN_MALLOC(struct TreeNode);
  node->left = left;
  node->right = right;
  return node;
}

BOOL table_find_or_put(TreeDatabaseRef treedb, TreeNodeElement left,
                       TreeNodeElement right, TreeNodeID *ref) {
  int count, i;
  uint64_t mask = treedb->mask;
  TreeNodeRef *table = treedb->nodes;
  uint64_t offset;
redo:
  offset = (hash_node(left, right) & mask);
  count = 0;

  while (count < TREE_THRESHOLD) {
    // Walk Cache line
    for (i = 0; i < TREE_CACHE_LINE; i++) {
      if (table[(offset + i) & mask] == 0) {
        TreeNodeRef node = tree_node_make(left, right);
        if (atomic_compare_and_swap(&table[(offset + i) & mask], 0, node)) {
          atomic_fetch_and_inc(&treedb->node_count);
          *ref = (offset + i) & mask;
          return FALSE;
        } else {
          free(node);
          goto redo;
        }
      } else if (tree_node_equal(table[(offset + i) & mask], left, right)) {
        *ref = (offset + i) & mask;
        return TRUE;
      }
    }
    offset = (mix64(offset) & mask);
    count++;
  }
  fprintf(stderr, "error full table\n");
  fprintf(stderr, "node count  : %10llu\n", treedb->node_count);
  fprintf(stderr, "table size  : %10lu\n", (treedb->mask + 1));
  fprintf(stderr, "load factor : %10.3lf\n",
          (double)tree_db_node_count(treedb) / (treedb->mask + 1));
  fprintf(stderr, "memory      : %7llu MB\n",
          (uint64_t)tree_space(treedb) / 1024 / 1024);
  exit(EXIT_FAILURE);
}

TreeDatabaseRef tree_make(size_t size) {
  TreeDatabaseRef treedb = LMN_MALLOC(struct TreeDatabase);
  treedb->nodes = LMN_CALLOC(TreeNodeRef, size);
  treedb->mask = size - 1;
  treedb->node_count = 0;
  return treedb;
}

void tree_clear(TreeDatabaseRef treedb) {
  int i;
  treedb->node_count = 0;
  for (i = 0; i < treedb->mask + 1; i++) {
    if (treedb->nodes[i]) {
      LMN_FREE(treedb->nodes[i]);
      treedb->nodes[i] = NULL;
    }
  }
}

void tree_free(TreeDatabaseRef treedb) {
  tree_clear(treedb);
  LMN_FREE(treedb->nodes);
  LMN_FREE(treedb);
  return;
}

TreeNodeElement tree_find_or_put_rec(TreeDatabaseRef treedb, TreeNodeStrRef str,
                                     int start, int end, BOOL *found) {
  int split;
  TreeNodeID ref;

  if ((end - start + 1) <= 1) {
    return vector_unit(str, start, end);
  }
  split = tree_get_split_position(start, end);
  TreeNodeElement left =
      tree_find_or_put_rec(treedb, str, start, start + split, found);
  TreeNodeElement right =
      tree_find_or_put_rec(treedb, str, start + split + 1, end, found);
  if ((end - start + 1) == str->len) {
    BOOL _found = table_find_or_put(treedb, left, right, &ref);
    if (found)
      (*found) = _found;
  } else {
    table_find_or_put(treedb, left, right, &ref);
  }
  return ref;
}

TreeNodeID tree_find_or_put(TreeDatabaseRef treedb, LmnBinStrRef bs,
                            BOOL *found) {
  struct TreeNodeStr str;
  TreeNodeID ref;
  int v_len_real = ((bs->len + 1) / TAG_IN_BYTE);
  str.len = v_len_real / TREE_UNIT_SIZE;
  str.extra = v_len_real % TREE_UNIT_SIZE;
  str.nodes = (TreeNodeElement *)bs->v;

  if (str.extra > 0)
    str.len += 1;
  // printf("node_count: %d, extra:%d\n", str.len, str.extra);
  ref = tree_find_or_put_rec(treedb, &str, 0, str.len - 1, found);
  return ref;
}

void tree_get_rec(TreeDatabaseRef treedb, TreeNodeElement elem, int start,
                  int end, TreeNodeStrRef dst) {
  int k = end - start + 1;
  if (k <= TREE_UNIT_SIZE) {
    int copy_len = TREE_UNIT_SIZE;
    if (end + 1 == (dst->len * TREE_UNIT_SIZE) && dst->extra != 0) {
      copy_len = dst->extra;
    }
    // printf("%d-%d len:%d\n", start, end, dst->len * TREE_UNIT_SIZE);
    // printf("elem:%llu, copy_len: %d\n", elem, copy_len);
    memcpy((BYTE *)dst->nodes + start, &elem, sizeof(BYTE) * copy_len);
  } else if (treedb->nodes[elem & treedb->mask] != NULL) {
    TreeNodeRef node = treedb->nodes[elem & treedb->mask];
    int split = ((end - start) / TREE_UNIT_SIZE) / 2;
    // printf("Split: %d\n", split);
    tree_get_rec(treedb, node->left, start, start + (split * TREE_UNIT_SIZE),
                 dst);
    tree_get_rec(treedb, node->right, start + ((split + 1) * TREE_UNIT_SIZE),
                 end, dst);
  }
}

LmnBinStrRef tree_get(TreeDatabaseRef treedb, TreeNodeID ref, int len) {
  LmnBinStrRef bs = binstr_make(len);
  struct TreeNodeStr str;
  int real_len = ((len + 1) / TAG_IN_BYTE);
  str.len = real_len / TREE_UNIT_SIZE;
  str.extra = real_len % TREE_UNIT_SIZE;
  str.nodes = (TreeNodeElement *)bs->v;

  if (str.extra > 0)
    str.len += 1;
  // printf("node_count: %d, extra:%d\n", str.len, str.extra);
  tree_get_rec(treedb, ref, 0, str.len * TREE_UNIT_SIZE - 1, &str);
  return bs;
}

uint64_t tree_space(TreeDatabaseRef treedb) {
  uint64_t memory = 0;
  memory += sizeof(struct TreeDatabase);
  memory += treedb->node_count * sizeof(struct TreeNode);
  memory += (treedb->mask + 1) * sizeof(TreeNodeRef);
  return memory;
}
