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
#include "../memory_count.h"
#include <math.h>
#include<fstream>
#include<mutex>
using namespace std;
std::mutex mtx;
#define atomic_fetch_and_inc(t) __sync_fetch_and_add(t, 1)
#define atomic_fetch_and_dec(t) __sync_fetch_and_sub(t, 1)
#define atomic_compare_and_swap(t, old, new)                                   \
  __sync_bool_compare_and_swap(t, old, new)

#define TREE_UNIT_SIZE 4
#define TREE_THRESHOLD 10
#define TREE_CACHE_LINE 8
/*bool check_l=false;
bool check_r=false;
int vecunitlen=0;
int vecunitlen_l=0;
int vecunitlen_r=0;
int vecunitlenal_l=TREE_UNIT_SIZE;
int vecunitlenal_r=TREE_UNIT_SIZE;*/
int depth=0;
typedef struct TreeNodeStr *TreeNodeStrRef;

struct TreeNodeStr {
  TreeNodeElement *nodes;
  int len;
  int extra;
};

uint32_t murmurhash64(const void *key, int len, unsigned int seed) {
  //const uint64_t m = 0xc6a4a7935bd1e995;
  const unsigned int m = 0x5bd1e995;
  const int r = 24;

  unsigned int h = seed ^ len;

  const unsigned char *data = (const unsigned char *)key;
  while (len>=4) {
    uint32_t k = *(unsigned int *)data;

    k *= m;
    k ^= k >> r;
    k *= m;
    
    h *= m;
    h ^= k;
    data+=4;
    len-=4;
  }


  switch (len) {
  case 3:
    h ^= data[2] << 16;
  case 2:
    h ^= data[1] << 8;
  case 1:
    h ^= data[0];
    h *= m;
  };

  h ^= h >> 13;
  h *= m;
  h ^= h >> 15;

  return h;
}

uint32_t mix64(uint32_t key) {
  key = (~key) + (key << 21); // key = (key << 21) - key - 1;
  key = key ^ (((int32_t)key) >> 24);
  key = (key + (key << 3)) + (key << 8); // key * 265
  key = key ^ (((int32_t)key) >> 14);
  key = (key + (key << 2)) + (key << 4); // key * 21
  key = key ^ (((int32_t)key) >> 28);
  key = key + (key << 31);
  return key;
}

int tree_get_split_position(int start, int end) {
  int size = end - start;
  return (int)(size / 2.0);
}

TreeNodeUnit vector_unit(TreeNodeStrRef str, int start, int end) {
  unsigned long ret;
  int copy_len = TREE_UNIT_SIZE;
  if (str->extra && end == str->len - 1) {
    copy_len = str->extra;
  }
  // printf("start :%d\n", start * TREE_UNIT_SIZE);
  // printf("copy_len :%d\n", copy_len);
  memcpy(&ret, ((BYTE *)str->nodes + (start * TREE_UNIT_SIZE)),
         sizeof(BYTE) * copy_len);
  //memory_count_no_comp+=copy_len;
  //vecunitlen=copy_len;
  // printf("start :0x%14llx\n", ret);
  /*if(ret>maxtreenodeid){
    maxtreenodeid=ret;
  }
  if(ret<minvectorunitid){
    minvectorunitid=ret;
    }*/
  return ret;
}

uint32_t hash_node(TreeNodeElement left, TreeNodeElement right) {
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
  //memory_count_binarystring+=sizeof(struct TreeNode);
  node->left = left;
  node->right = right;
  return node;
}

BOOL TreeDatabase::table_find_or_put(TreeNodeElement left,
				     TreeNodeElement right, TreeNodeID *ref,int treedepth) {
  int count, i;
  uint32_t mask = this->mask;
  TreeNodeRef *table = this->nodes;
  uint32_t offset;
redo:
  offset = (hash_node(left, right) & mask);
  count = 0;

  while (count < TREE_THRESHOLD) {
    // Walk Cache line
    for (i = 0; i < TREE_CACHE_LINE; i++) {
      if (table[(offset + i) & mask] == 0) {
        TreeNodeRef node = tree_node_make(left, right);
        if (atomic_compare_and_swap(&table[(offset + i) & mask], 0, node)) {
          atomic_fetch_and_inc(&this->node_count);
          *ref = (offset + i) & mask;
	  /*if(check_l==true){
	    if(check_r==true){
	      memory_count_vectorunit+=vecunitlen_l+vecunitlen_r;
	      treevalue+=pow(2,depth-1)*2*TREE_UNIT_SIZE;
	    }else{
	      memory_count_vectorunit+=vecunitlen_l;
	      memory_count_ref+=4;
	      treevalue+=pow(2,depth-1)*TREE_UNIT_SIZE;
	    }
	  }else if(check_r==true){
	    memory_count_vectorunit+=vecunitlen_r;
	    memory_count_ref+=4;
	    treevalue+=pow(2,depth-1)*TREE_UNIT_SIZE;
	  }else{
	    memory_count_ref+=8;
	  }
	  check_l=false;
	  check_r=false;
	  vecunitlen=0;
	  depth--;*/
	  //nodeintree++;
	  {
	    std::lock_guard<std::mutex> lock(mtx);
	    nodecount_level[treedepth]++;
	    nodeintree1++;
	  }
          return FALSE;
        } else {
          std::free(node);
          goto redo;
        }
      } else if (tree_node_equal(table[(offset + i) & mask], left, right)) {
        *ref = (offset + i) & mask;
	//printf("a unit is shared in hashtable no %x in depth %d\n",(offset+i)&mask,depth);
	/*sharenode++;
	check_l=false;
	check_r=false;
	vecunitlen=0;
	depth--;*/
        return TRUE;
      }
      //conflict_count++;
    }
    offset = (mix64(offset) & mask);
    count++;
  }
  fprintf(stderr, "error full table\n");
  fprintf(stderr, "node count  : %10lu\n", this->node_count);
  fprintf(stderr, "table size  : %10lu\n", (this->mask + 1));
  fprintf(stderr, "load factor : %10.3lf\n",
          (double)tree_db_node_count(this) / (this->mask + 1));
  fprintf(stderr, "memory      : %7lu MB\n",
          (uint32_t)this->space() / 1024 / 1024);
  exit(EXIT_FAILURE);
}

TreeDatabase::TreeDatabase(size_t size){
  //memory_count_binarystring+=sizeof(struct TreeDatabase);
  this->nodes = LMN_CALLOC(TreeNodeRef, size);
  //memory_count_binarystring+=sizeof(TreeNodeRef)*size;
  this->mask = size - 1;
  this->node_count = 0;
}

void TreeDatabase::clear(){
  int i;
  this->node_count = 0;
  for (i = 0; i < this->mask + 1; i++) {
    if (this->nodes[i]) {
      LMN_FREE(this->nodes[i]);
      this->nodes[i] = NULL;
    }
  }
}

TreeDatabase::~TreeDatabase() {
  this->clear();
  LMN_FREE(this->nodes);
  return;
}

TreeNodeElement TreeDatabase::tree_find_or_put_rec(TreeNodeStrRef str,
						   int start, int end, BOOL *found,int treedepth) {
  int split;
  TreeNodeID ref;
  if ((end - start + 1) <= 1) {
    return vector_unit(str, start, end);
  }
  /*depth++;
  if(depth>tree_database_max_depth){
    tree_database_max_depth=depth;
    }*/
  {
    std::lock_guard<std::mutex> lock(mtx);
    if(treedepth>tree_database_max_depth){
      tree_database_max_depth=treedepth;
    }
  }
  split = tree_get_split_position(start, end);
  TreeNodeElement left =
    this->tree_find_or_put_rec(str, start, start + split, found,treedepth+1);
  //vecunitlen_l=vecunitlen;
  TreeNodeElement right =
    this->tree_find_or_put_rec(str, start + split + 1, end, found,treedepth+1);
  /*vecunitlen_r=vecunitlen;
  nodecount++;
  if((split+1)<=1){
    check_l=true;
  }
  if((end-(start+split+1)+1)<=1){
    check_r=true;
    }*/
  if ((end - start + 1) == str->len) {
    BOOL _found = this->table_find_or_put(left, right, &ref,treedepth);
    if (found)
      (*found) = _found;
  } else {
    this->table_find_or_put(left, right, &ref,treedepth);
  }
  //ofstream outputfile("treedatabase.dot",std::ios::app);
  //outputfile<<"\""<<hex<<ref<<"\" -> \""<<hex<<left<<"\";"<<"\n";
  //outputfile<<"\""<<hex<<ref<<"\" -> \""<<hex<<right<<"\";"<<"\n";
  //outputfile.close();
  {
    std::lock_guard<std::mutex> lock(mtx);
    depth--;
    nodecount++;
  }
  return ref;
}

TreeNodeID TreeDatabase::tree_find_or_put(LmnBinStrRef bs,
                            BOOL *found) {
  struct TreeNodeStr str;
  TreeNodeID ref;
  int v_len_real = ((bs->len + 1) / TAG_IN_BYTE);
  str.len = v_len_real / TREE_UNIT_SIZE;
  str.extra = v_len_real % TREE_UNIT_SIZE;
  str.nodes = (TreeNodeElement *)bs->v;
  //depth=0;
  if (str.extra > 0)
    str.len += 1;
  //printf("node_count: %d, extra:%d\n", str.len, str.extra);
  ref = this->tree_find_or_put_rec(&str, 0, str.len - 1, found,0);
  return ref;
}

void TreeDatabase::get_rec(TreeNodeElement elem, int start,
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
  } else if (this->nodes[elem & this->mask] != NULL) {
    TreeNodeRef node = this->nodes[elem & this->mask];
    int split = ((end - start) / TREE_UNIT_SIZE) / 2;
    // printf("Split: %d\n", split);
    this->get_rec(node->left, start, start + (split * TREE_UNIT_SIZE),
                 dst);
    this->get_rec(node->right, start + ((split + 1) * TREE_UNIT_SIZE),
                 end, dst);
  }
}

LmnBinStrRef TreeDatabase::get(TreeNodeID ref, int len) {
  LmnBinStrRef bs = binstr_make(len);
  struct TreeNodeStr str;
  int real_len = ((len + 1) / TAG_IN_BYTE);
  str.len = real_len / TREE_UNIT_SIZE;
  str.extra = real_len % TREE_UNIT_SIZE;
  str.nodes = (TreeNodeElement *)bs->v;

  if (str.extra > 0)
    str.len += 1;
  // printf("node_count: %d, extra:%d\n", str.len, str.extra);
  this->get_rec(ref, 0, str.len * TREE_UNIT_SIZE - 1, &str);
  return bs;
}

uint32_t TreeDatabase::space(void) {
  uint32_t memory = 0;
  memory += sizeof(struct TreeDatabase);
  memory += this->node_count * sizeof(struct TreeNode);
  memory += (this->mask + 1) * sizeof(TreeNodeRef);
  return memory;
}
