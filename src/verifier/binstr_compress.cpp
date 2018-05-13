/*
 * binstr_compress.cpp
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
#include "binstr_compress.h"
#include "../third_party/zdelta-2.1/zdlib.h"
#include "element/element.h"
#ifdef HAVE_LIBZ
#include <zlib.h>
#endif
#ifdef PROFILE
#include "runtime_status.h"
#endif

/** ============================
 *  Binary String Compressor
 *  ============================
 *  AUTHOR: Masato Gocho
 */

/* ハーフバイトは1バイトの半分のサイズであるため, バイトサイズの2倍の長さとなる.
 * ハーフバイトサイズhとバイトサイズbの変換では,
 * 除算による1未満切り捨てを考慮する必要がある.
 *
 * ENCODE:
 *   LmnBinStr org, cmp;
 *   1. cmp = compress(org)
 *   2. cmp.h <==  x2 <== cmp.b
 *   3. free(src)
 * DECODE:
 *   LmnBinStr org, cmp;
 *   1. cmp.h ==> 1/2 ===> cmp.b
 *   2. org = uncompress(cmp);
 *   3. org.h <==  x2 <== org.b
 *
 * bからhを求めると, 2倍しているため必ず偶数となり,
 * DECODEした際に正確なhの値とならない. そこで, ENCODEの際に,
 * orgのハーフバイトサイズが奇数ならば, cmpのハーフバイトサイズに1を加算する.
 * 加算した1はDECODEの際に1/2するため切り捨てられる.
 * 最終的に, DECODEしたハーフバイトサイズを計算したとき,
 * cmpのハーフバイトサイズが奇数なら1を減算する.
 *
 * 例:
 * ENCODE(org.h = 13)
 *        org.b = (org.h + 1) / 2
 *              = 7
 *  ===>  cmp.b = 5
 *  ===>  cmp.h = cmp.b * 2 + 1
 *              = 11
 * DECODE(cmp.h = 11)
 *        cmp.b = org.h / 2
 *              = 5
 *  ===>  org.b = 7
 *  ===>  org.h = org.b * 2 - 1
 *              = 13
 */

/** --------------------------
 *  Method1: zlib
 *    "A Massively Spiffy Yet Delicately Unobtrusive Compression Library"
 *    @see http://www.zlib.net/
 */
LmnBinStrRef lmn_bscomp_z_encode(const LmnBinStrRef org) {
#ifndef HAVE_LIBZ
  return org;
#else
  LmnBinStrRef cmp;
  unsigned long org_8len, cmp_8len;
  int ret;

#ifdef PROFILE
  unsigned long old_space, cmp_space;
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__Z_COMPRESS);
  }
#endif

  LMN_ASSERT(!is_comp_z(org)); /* z圧縮の多重掛けは想定していない */

  org_8len = (org->len + 1) / TAG_IN_BYTE;
  cmp_8len = org_8len * 2;
  cmp = lmn_binstr_make(cmp_8len);
  cmp->type = org->type;
  ret = compress(cmp->v, &cmp_8len, org->v, org_8len);
  if (ret != Z_OK) { /* zlib */
    fprintf(stderr, "%s\n", ret == Z_MEM_ERROR ? "Z_MEM_ERROR" : "Z_BUF_ERROR");
    lmn_fatal("fail to compress: zlib");
  }

  cmp->len = cmp_8len * TAG_IN_BYTE + ((org->len & 0x1U) ? 1 : 0);
  set_comp_z(cmp);

#ifdef PROFILE
  old_space = lmn_binstr_space(org);
  cmp_space = lmn_binstr_space(cmp);
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, cmp_space);
    profile_add_space(PROFILE_SPACE__REDUCED_BINSTR, old_space - cmp_space);
    profile_finish_timer(PROFILE_TIME__Z_COMPRESS);
  }
#endif
  return cmp;
#endif
}

LmnBinStrRef lmn_bscomp_z_decode(const LmnBinStrRef cmp) {
#ifndef HAVE_LIBZ
  return cmp;
#else
  LmnBinStrRef org;
  unsigned long cmp_8len, org_8len;
  int ret;

#ifdef PROFILE
  unsigned long org_space;
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__Z_UNCOMPRESS);
  }
#endif
  LMN_ASSERT(is_comp_z(cmp));

  cmp_8len = cmp->len / TAG_IN_BYTE;
  org_8len = cmp_8len * 5;
  org = lmn_binstr_make(org_8len);
  ret = uncompress(org->v, &org_8len, cmp->v, cmp_8len);
  if (ret != Z_OK) { /* zlib */
    fprintf(stderr, "%s\n", ret == Z_MEM_ERROR ? "Z_MEM_ERROR" : "Z_BUF_ERROR");
    lmn_fatal("fail to uncompress: zlib");
  }

  org->type = cmp->type;
  org->len = org_8len * TAG_IN_BYTE - ((cmp->len & 0x1U) ? 1 : 0);
  unset_comp_z(org);

#ifdef PROFILE
  org_space = lmn_binstr_space(org);
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, org_space);
    profile_finish_timer(PROFILE_TIME__Z_UNCOMPRESS);
  }
#endif

  return org;
#endif
}

/** --------------------------
 *  Method2: zdelta
 *    "A General Purpose Lossless Delta Compression Library")
 *    @see http://cis.poly.edu/zdelta/
 */

static inline const char *zd_ret_str(int n) {
  switch (n) {
  case ZD_BUF_ERROR:
    return "Buffer Error";
  case ZD_MEM_ERROR:
    return "Memory Error";
  case ZD_STREAM_ERROR:
    return "Stream Error";
  case ZD_DATA_ERROR:
    return "Data Error";
  case ZD_OK:
    return "OK";
  default:
    return "Unknown Error";
  }
}

static int zd_buf_n = 3;

/* バイト列refからバイト列orgへの差分を求めて返す.
 * org, ref共にRead Only */
LmnBinStrRef lmn_bscomp_d_encode(const LmnBinStrRef org,
                                 const LmnBinStrRef ref) {
  LmnBinStrRef dif;
  unsigned long org_8len, ref_8len, dif_8len;
  int ret, mul;

#ifdef PROFILE
  unsigned long old_space, dif_space;
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__D_COMPRESS);
  }
#endif

  LMN_ASSERT(!is_comp_d(org)); /* diffからのdiff計算は禁止 */

  org_8len = (org->len + 1) / TAG_IN_BYTE;
  ref_8len = (ref->len + 1) / TAG_IN_BYTE;

  mul = zd_buf_n;
  dif = NULL;
  dif_8len = org_8len;

  while (1) {
    if (dif)
      lmn_binstr_free(dif);
    dif_8len *= mul;
    dif = lmn_binstr_make(dif_8len);
    ret = zd_compress(ref->v, ref_8len, org->v, org_8len, dif->v, &dif_8len);

    if (ret == ZD_BUF_ERROR) {
      zd_buf_n *= mul;
    } else {
      break;
    }
  }

  if (ret != ZD_OK) {
    fprintf(stderr, "%s\n", zd_ret_str(ret));
    lmn_fatal("fail to compress");
  }

  dif->len = dif_8len * TAG_IN_BYTE + ((org->len & 0x1U) ? 1 : 0);
  dif->type = org->type;
  set_comp_d(dif);

#ifdef PROFILE
  old_space = lmn_binstr_space(org);
  dif_space = lmn_binstr_space(dif);
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, dif_space);
    profile_add_space(PROFILE_SPACE__REDUCED_BINSTR, old_space - dif_space);
    profile_finish_timer(PROFILE_TIME__D_COMPRESS);
  }
#endif
  return dif;
}

/* バイト列refに対して差分difを適用してorgを復元して返す.
 * メモリは読み出し専用 */
LmnBinStrRef lmn_bscomp_d_decode(const LmnBinStrRef ref,
                                 const LmnBinStrRef dif) {
  LmnBinStrRef org;
  unsigned long ref_8len, dif_8len, org_8len;
  int ret, mul;

#ifdef PROFILE
  unsigned long org_space;
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__D_UNCOMPRESS);
  }
#endif
  LMN_ASSERT(is_comp_d(dif));

  ref_8len = (ref->len + 1) / TAG_IN_BYTE;
  dif_8len = dif->len / TAG_IN_BYTE;

  mul = zd_buf_n;
  org = NULL;
  org_8len = ref_8len + dif_8len;

  while (1) {
    if (org)
      lmn_binstr_free(org);
    org_8len *= mul;
    org = lmn_binstr_make(org_8len);

    ret = zd_uncompress(ref->v, ref_8len, org->v, &org_8len, dif->v, dif_8len);

    if (ret == ZD_BUF_ERROR) {
      zd_buf_n *= mul;
    } else {
      break;
    }
  }

  if (ret != ZD_OK) {
    fprintf(stderr, "%s\n", zd_ret_str(ret));
    lmn_fatal("fail to uncompress: zdlib");
  }

  org->len = org_8len * TAG_IN_BYTE - ((dif->len & 0x1U) ? 1 : 0);
  org->type = dif->type;
  unset_comp_d(org);

#ifdef PROFILE
  org_space = lmn_binstr_space(org);
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, org_space);
    profile_finish_timer(PROFILE_TIME__D_UNCOMPRESS);
  }
#endif

  return org;
}

TreeDatabaseRef treedb;
#ifdef PROFILE
uint64_t node_count;
uint64_t table_size;
double load_factor;
uint64_t memory;
#endif

void lmn_bscomp_tree_profile(FILE *f) {
#ifdef PROFILE
  fprintf(f, "node count              : %10llu\n", node_count);
  fprintf(f, "table size              : %10llu\n", table_size);
  fprintf(f, "load factor             : %10.3lf\n", load_factor);
  fprintf(f, "memory                  : %7llu MB\n", memory);
#else
  fprintf(f, "have to enable profile option\n");
#endif
}

BOOL lmn_bscomp_tree_init() {
  if (treedb == NULL) {
    treedb = tree_make(2ULL << lmn_env.tree_compress_table_size);
    return TRUE;
  }
  return FALSE;
}

BOOL lmn_bscomp_tree_clean() {
  if (treedb != NULL) {
#ifdef PROFILE
    node_count = tree_db_node_count(treedb);
    table_size = treedb->mask + 1;
    load_factor = (double)node_count / (treedb->mask + 1);
    memory = (uint64_t)tree_space(treedb) / 1024 / 1024;
#endif
    tree_free(treedb);
    treedb = NULL;
    return TRUE;
  }
  return FALSE;
}

unsigned long lmn_bscomp_tree_space() { return tree_space(treedb); }

TreeNodeID lmn_bscomp_tree_encode(LmnBinStrRef str) {
  BOOL found;
  TreeNodeID ref;
#ifdef PROFILE
  int pre_node_count = tree_db_node_count(treedb);
  int post_node_count = 0;
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__TREE_COMPRESS);
  }
#endif
  LMN_ASSERT(treedb);
  LMN_ASSERT(str);
  ref = tree_find_or_put(treedb, str, &found);
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    unsigned long old_space, dif_space;
    post_node_count = tree_db_node_count(treedb);
    dif_space = (post_node_count - pre_node_count) * sizeof(struct TreeNode);
    old_space = lmn_binstr_space(str);

    profile_add_space(PROFILE_SPACE__STATE_BINSTR, dif_space);
    profile_add_space(PROFILE_SPACE__REDUCED_BINSTR, old_space - dif_space);
    profile_finish_timer(PROFILE_TIME__TREE_COMPRESS);
  }
#endif
  return ref;
}

LmnBinStrRef lmn_bscomp_tree_decode(TreeNodeID ref, int len) {
  LmnBinStrRef bs;
  LMN_ASSERT(treedb);
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__TREE_UNCOMPRESS);
  }
#endif
  bs = tree_get(treedb, ref, len);
#ifdef PROFILE
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, lmn_binstr_space(bs));
    profile_finish_timer(PROFILE_TIME__TREE_UNCOMPRESS);
  }
#endif
  return bs;
}
