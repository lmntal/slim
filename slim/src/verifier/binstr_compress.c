/*
 * binstr_compress.c
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
#include "error.h"
#ifdef PROFILE
#  include "runtime_status.h"
#endif


/** ----------------------------------------------------------------------
 *  Binary String Compression
 *    using Delta Encoding
 */
LmnBinStr lmn_bscomp_delta_encode(LmnBinStr bs1, LmnBinStr bs2)
{
  return bs2;
}
LmnBinStr lmn_bscomp_delta_decode(LmnBinStr bs1, LmnBinStr bs2)
{
  return bs2;
}


/** -----------------------------------------------------------------
 *  Binary String Compression
 *    using zlib("A Massively Spiffy Yet Delicately Unobtrusive Compression Library")
 *  @see http://www.zlib.net/
 */
LmnBinStr lmn_bscomp_z_encode(const LmnBinStr org)
{
#ifndef HAVE_LIBZ
  return org;
#else
  LmnBinStr new;
  unsigned long org_8len, buf_len;
  int ret;
#ifdef PROFILE
  unsigned long old_space, new_space;
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__Z_COMPRESS);
  }
#endif

  buf_len = org->len << 2; /* 適当に確保 */
  new = lmn_binstr_make(buf_len);
  org_8len = (org->len + 1) / TAG_IN_BYTE; /* org->lenは4bit単位の要素数なので, 8bit単位の数値へ戻す */

  ret = compress(new->v, &buf_len, org->v, org_8len);
  if (ret != Z_OK) { /* zlib */
    printf("%s\n", ret == Z_MEM_ERROR ? "Z_MEM_ERROR" : "Z_BUF_ERROR");
    lmn_fatal("fail to compress");
    /* 成功するまで繰り返せるけど, とりあえずfatalでいいや */
  }

  new->len = buf_len * TAG_IN_BYTE + ((org->len & 0x1U) ? 1 : 0);
  set_comp_z(new);

#ifdef PROFILE
  old_space = lmn_binstr_space(org);
  new_space = lmn_binstr_space(new);
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, new_space);
    profile_add_space(PROFILE_SPACE__REDUCED_BINSTR, old_space - new_space);
    profile_finish_timer(PROFILE_TIME__Z_COMPRESS);
  }
#endif
  return new;
#endif
}

LmnBinStr lmn_bscomp_z_decode(const LmnBinStr org)
{
#ifndef HAVE_LIBZ
  return org;
#else
  LmnBinStr new;
  unsigned long org_8len, buf_len;
  int ret;

#ifdef PROFILE
  unsigned long org_space, new_space;
  if (lmn_env.profile_level >= 3) {
    profile_start_timer(PROFILE_TIME__Z_UNCOMPRESS);
  }
#endif


  LMN_ASSERT(is_comp_z(org));

  buf_len = org->len << 3; /* とりあえず適当 */
  new = lmn_binstr_make(buf_len);
  org_8len = (org->len + 1) / TAG_IN_BYTE; /* org->lenは4bit単位の要素数なので, 8bit単位の数値へ戻す */

  ret = uncompress(new->v, &buf_len, org->v, org_8len);
  if (ret != Z_OK) { /* zlib */
    printf("%s\n", ret == Z_MEM_ERROR ? "Z_MEM_ERROR" : "Z_BUF_ERROR");
    lmn_fatal("fail to uncompress");
    /* デコードの方はBUF_ERRORならバッファを大きくして成功するまで再試行すべき
     * だけど, とりあえずfatalでいいや */
  }

  new->len = buf_len * TAG_IN_BYTE - ((org->len & 0x1U) ? 1 : 0);

#ifdef PROFILE
  org_space = lmn_binstr_space(org);
  new_space = lmn_binstr_space(new);
  if (lmn_env.profile_level >= 3) {
    profile_add_space(PROFILE_SPACE__STATE_BINSTR, new_space);
    profile_finish_timer(PROFILE_TIME__Z_UNCOMPRESS);
  }
#endif

  return new;
#endif
}

