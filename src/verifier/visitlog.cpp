/*
 * visitlog.cpp
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
 */
#include "visitlog.h"

/* ログに記録されたアトムatomに対応する値をvalueに設定し, 正の値を返す.
 * ログにatomが存在しない場合は, 0を返す. */
int visitlog_get_atom(VisitLogRef visitlog, LmnSymbolAtomRef atom,
                      LmnWord *value) {
  return proc_tbl_get_by_atom(visitlog->tbl, atom, value);
}

/* ログに記録された膜memに対応する値をvalueに設定, 正の値を返す.
 * ログにmemが存在しない場合は, 0を返す. */
int visitlog_get_mem(VisitLogRef visitlog, LmnMembraneRef mem, LmnWord *value) {
  return proc_tbl_get_by_mem(visitlog->tbl, mem, value);
}

/* ログに記録されたhlに対応する値をvalueに設定し, 正の値を返す.
 * ログにhlが存在しない場合は, 0を返す. */
int visitlog_get_hlink(VisitLogRef visitlog, HyperLink *hl, LmnWord *value) {
  return proc_tbl_get_by_hlink(visitlog->tbl, hl, value);
}

/* visitlogに記録した要素（膜、アトム）の数を返す */
int visitlog_element_num(VisitLogRef visitlog) { return visitlog->element_num; }
