/*
 * vector.c
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
 * $Id: vector.c,v 1.8 2008/09/19 05:18:17 taisuke Exp $
 */

#include "vector.h"


Vector::Vector(){
  this->tbl = (LmnWord *)NULL;
}
Vector::Vector(unsigned int init_size){
  LMN_ASSERT(init_size > 0);
  this->init(init_size);
}
Vector::Vector(const Vector &vec){
  int i;
  init(vec.get_num() > 0 ? vec.get_num() : 1);

  for (i = 0; i < vec.get_num(); i++) {
    this->tbl[i] = vec.tbl[i];
  }
  this->num = vec.get_num();
}
Vector::~Vector(){
  if(this->tbl != NULL){
    LMN_FREE(this->tbl);
  }
}
void Vector::init(unsigned int init_size){
  this->tbl = LMN_NALLOC(LmnWord, init_size);
  this->num = 0;
  this->cap = init_size;
}
void Vector::extend(){
  this->cap *= 2;
  this->tbl = LMN_REALLOC(LmnWord, this->tbl, this->cap);
}
void Vector::set_num(unsigned int n){
  this->num = n;
}
unsigned int Vector::get_cap() const{
  return this->cap;
}
void Vector::set_cap(unsigned int c){
  this->cap = c;
}
void Vector::memset_tbl(int ch, std::size_t count){
  memset(this->tbl,ch,count);
}
bool Vector::is_empty() const{
  return this->num==0;
}
void Vector::push(LmnWord keyp){
  if (this->num == this->cap) {
    this->extend();
  }
  (this->tbl)[this->num] = keyp;
  this->num++;
}
void Vector::reduce(){
  this->cap /= 2;
  this->tbl = LMN_REALLOC(LmnWord, this->tbl, this->cap);
}
LmnWord Vector::pop(){
  LmnWord ret;
  LMN_ASSERT(this->num > 0);
  /* Stackとして利用する場合, reallocが頻繁に発生してしまう.
   * Stackなのでサイズの増減は頻繁に発生するものだが,
   * 頻繁なreallocはパフォーマンスに影響する.
   * >>とりあえず<< サイズの下限値を設ける. LmnStackなる構造を別に作るべきかも.
   * (gocho) */
  if (this->num <= this->cap / 2 && this->cap > 1024) {
    this->reduce();
  }
  ret = this->get(this->num - 1);
  this->num--;
  return ret;
}
//pop Nth element
LmnWord Vector::pop_n(unsigned int n){
  unsigned int i;
  LmnWord ret;
  LMN_ASSERT(this->num > 0 && n >= 0 && n < this->num);

  if (this->num <= this->cap / 2) {
    this->reduce();
  }
  ret = this->get(n);
  for (i = n; i < this->num - 1; ++i) {
    this->set(i, get(i + 1));
  }
  this->num--;
  return ret;
}
LmnWord Vector::peek() const{
  return this->get(this->num - 1);
}
void Vector::set(unsigned int index, LmnWord keyp){
  LMN_ASSERT(index < this->cap);
  this->tbl[index] = keyp;    
}
void Vector::set_list(LmnWord *w){
  this->tbl = w;
}
LmnWord Vector::last() const{
  return this->tbl[this->num-1];
}
/* pop all elements from vec */
void Vector::clear(){
  this->num = 0;
}
void Vector::destroy(){
  LMN_FREE(this->tbl);
  this->tbl = (LmnWord *)NULL;
}
unsigned long Vector::space_inner() const{
  return this->cap * sizeof(vec_data_t);
}
BOOL Vector::contains(LmnWord keyp) const{
  unsigned int i = 0;
  while (i < this->num) {
    if (this->get(i++) == keyp) {
      return TRUE;
    }
  }
  return FALSE;
}
void Vector::reverse(){ /* Vectorに詰んだ要素を逆順に並べ直す */
  unsigned int r, l;
  r = 0;
  l = this->num - 1;

  while (r < l) {
    vec_data_t tmp = this->tbl[r];
    set(r, this->tbl[l]);
    set(l, tmp);
    r++;
    l--;
  }
}
void Vector::resize(unsigned int size, vec_data_t val){ 
/* ベクタのサイズを size に変更し、新規に追加された項目を val に設定する*/
  unsigned int i;
  while (size > this->cap) {
    this->extend();
  }
   /* 追加された項目を val に設定 */
  for (i = this->num; i < size; i++) {
    this->tbl[i] = val;
  }
  this->num = size;
}
void Vector::sort(int (*compare)(const void *, const void *)){
  qsort(this->tbl, this->num, sizeof(vec_data_t), compare);
}
