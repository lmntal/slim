/*
 * membrane.h
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
 * $Id: membrane.h,v 1.8 2008/09/19 05:18:17 taisuke Exp $
 */

#ifndef LMN_MEMBRANE_HPP
#define LMN_MEMBRANE_HPP

#include "atom.h"

struct SimpleHashtbl;

typedef struct AtomListEntry {
  LmnSymbolAtomRef tail, head;
#ifdef NEW_ATOMLIST
  int n;
#endif
  struct SimpleHashtbl *record;

  bool is_empty(){
    return this -> head == reinterpret_cast<LmnSymbolAtomRef>(this);
  }

  void set_empty(){
    LMN_SATOM_SET_PREV(reinterpret_cast<LmnSymbolAtomRef>(this), reinterpret_cast<LmnSymbolAtomRef>(this));
    LMN_SATOM_SET_NEXT(reinterpret_cast<LmnSymbolAtomRef>(this), reinterpret_cast<LmnSymbolAtomRef>(this));
    atomlist_set_num(this, 0);
  }

  /* アトムリストALからアトムAを削除する.
   * ただし, リストのつなぎ変えだけを行い,
   * 膜からのアトムAのdeleteやatomのfreeはしない */
  void remove(LmnSymbolAtomRef a){
    LMN_SATOM_SET_PREV(LMN_SATOM_GET_NEXT_RAW(a), LMN_SATOM_GET_PREV(a));
    LMN_SATOM_SET_NEXT(LMN_SATOM_GET_PREV(a), LMN_SATOM_GET_NEXT_RAW(a));
    atomlist_modify_num(this, -1);
  }

} AtomListEntry;


#endif
