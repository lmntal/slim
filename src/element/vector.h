/*
 * vector.h
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
 * $Id: vector.h,v 1.9 2008/09/19 05:18:17 taisuke Exp $
 */

#ifndef LMN_VECTOR_H
#define LMN_VECTOR_H

/**
 * @ingroup  Element
 * @defgroup Vector
 * @{
 */

#include "../lmntal.h"
#include <vector>
#include "util.h"


typedef struct Vector *PVector;
typedef LmnWord vec_data_t;

struct Vector {
  LmnWord *tbl;
  unsigned int num, cap;
  Vector(){
    tbl = (LmnWord *)NULL;
  }
  Vector(unsigned int init_size){
    LMN_ASSERT(init_size > 0);
    init(init_size);
  }
  template <class T> Vector(const std::vector<T> &v){
    static_assert(std::is_scalar<T>::value && sizeof(T) <= sizeof(LmnWord),
                "vector elements must be scalars.");
    LMN_ASSERT(v.size() > 0);
    init(v.size());
    memcpy(tbl, v.data(), sizeof(T) * v.size());
    num = v.size();
  }
  Vector(const Vector &vec){
    int i;
    init(vec.get_num() > 0 ? vec.get_num() : 1);

    for (i = 0; i < vec.get_num(); i++) {
      this->tbl[i] = vec.tbl[i];
    }
    this->num = vec.get_num();
  }
  ~Vector(){
    if(tbl != NULL){
      LMN_FREE(tbl);
    }
  }
  void init(unsigned int init_size){
    tbl = LMN_NALLOC(LmnWord, init_size);
    num = 0;
    cap = init_size;
  }
  void extend(){
    cap *= 2;
    tbl = LMN_REALLOC(LmnWord, tbl, cap);
  }
  unsigned int get_num() const{
    return num;
  }
  unsigned int get_cap() const{
    return cap;
  }
  bool is_empty() const{
    return num==0;
  }
  void push(LmnWord keyp){
    if (num == cap) {
      extend();
    }
    (tbl)[num] = keyp;
    num++;
  }
  void reduce(){
    cap /= 2;
    tbl = LMN_REALLOC(LmnWord, tbl, cap);
  }
  LmnWord pop(){
    LmnWord ret;
    LMN_ASSERT(num > 0);
    /* Stackとして利用する場合, reallocが頻繁に発生してしまう.
     * Stackなのでサイズの増減は頻繁に発生するものだが,
     * 頻繁なreallocはパフォーマンスに影響する.
     * >>とりあえず<< サイズの下限値を設ける. LmnStackなる構造を別に作るべきかも.
     * (gocho) */
    if (num <= cap / 2 && cap > 1024) {
      reduce();
    }
    ret = get(num - 1);
    num--;
    return ret;
  }
  //pop Nth element
  LmnWord pop_n(unsigned int n){
    unsigned int i;
    LmnWord ret;

    LMN_ASSERT(num > 0 && n >= 0 && n < num);

    if (num <= cap / 2) {
      reduce();
    }
    ret = get(n);
    for (i = n; i < num - 1; ++i) {
      set(i, get(i + 1));
    }
    num--;
    return ret;
  }
  LmnWord peek() const{
    return get(num - 1);
  }
  void set(unsigned int index, LmnWord keyp){
    LMN_ASSERT(index < cap);
    tbl[index] = keyp;    
  }
  LmnWord get(unsigned int index) const {
    LMN_ASSERT(index < num);
    return (tbl[index]);
  }
  LmnWord last() const{
    return tbl[num-1];
  }
/* pop all elements from vec */
  void clear(){
    num = 0;
  }
  void destroy(){
    LMN_FREE(tbl);
    tbl = (LmnWord *)NULL;
  }
  unsigned long space_inner() const{
    return get_cap() * sizeof(vec_data_t);
  }
  BOOL contains(LmnWord keyp) const{
    unsigned int i = 0;
    while (i < get_num()) {
      if (get(i++) == keyp) {
        return TRUE;
      }
    }
    return FALSE;
  }
  void reverse(){ /* Vectorに詰んだ要素を逆順に並べ直す */
    unsigned int r, l;

    r = 0;
    l = get_num() - 1;

    while (r < l) {
      vec_data_t tmp = tbl[r];
      set(r, tbl[l]);
      set(l, tmp);
      r++;
      l--;
    }
  }
};

void vec_resize(Vector *vec, unsigned int size, vec_data_t val);
void vec_sort(const Vector *vec, int (*compare)(const void *, const void *));


//namespace slim {
//namespace element {
//template <class T> std::vector<T> make_vector(Vector *v) {
//  if (!v)
//    return std::vector<T>();
//  return std::vector<T>(
//      raw_pointer_iterator<T>(reinterpret_cast<T *>(v->tbl)),
//      raw_pointer_iterator<T>(reinterpret_cast<T *>(v->tbl + v->num)));
//}
//}
//}

/* @} */

#endif /* LMN_VECTOR_H */
