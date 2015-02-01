/*
 * tree_compress.h
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

#ifndef TREE_COMPRESS_H
#  define TREE_COMPRESS_H

#include "mem_encode.h"

#define TREE_DB_DEFAULT_SIZE (1024 * 1024 * 128)

typedef uint64_t TreeNodeElement;
typedef TreeNodeElement TreeNodeRef;
typedef TreeNodeElement TreeNodeUnit;

typedef struct TreeDatabase *TreeDatabase;
typedef struct TreeNode *TreeNode;

struct TreeDatabase {
  TreeNode    *nodes;
  uint64_t    node_count;
  size_t      mask;
};

struct TreeNode {
  TreeNodeElement left;
  TreeNodeElement right;
};

TreeDatabase   tree_make(size_t size);
void           tree_free(TreeDatabase treedb);
void           tree_clear(TreeDatabase treedb);
LmnBinStr      tree_get(TreeDatabase treedb, TreeNodeRef ref, int len);
TreeNodeRef    tree_find_or_put(TreeDatabase treedb, LmnBinStr bs, BOOL *found);
uint64_t       tree_space(TreeDatabase treedb);

#define tree_db_node_count(db) (db->node_count)
#define tree_db_string_count(db) (db->string_count)

#endif /* ifndef TREE_COMPRESS_H */
