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
private:
  unsigned int num, cap;
public:
  Vector();
  Vector(unsigned int init_size);
  template <class T> Vector(const std::vector<T> &v);
  Vector(const Vector &vec);
  ~Vector();
  void init(unsigned int init_size);
  void extend();
  unsigned int get_num() const;
  void set_num(unsigned int n);
  unsigned int get_cap() const;
  void set_cap(unsigned int c);
  bool is_empty() const;
  void push(LmnWord keyp);
  void reduce();
  LmnWord pop();
  //pop Nth element
  LmnWord pop_n(unsigned int n);
  LmnWord peek() const;
  void set(unsigned int index, LmnWord keyp);
  LmnWord get(unsigned int index) const;
  LmnWord last() const;
/* pop all elements from vec */
  void clear();
  void destroy();
  unsigned long space_inner() const;
  BOOL contains(LmnWord keyp) const;
  void reverse();
  void resize(unsigned int size, vec_data_t val);
  void sort(int (*compare)(const void *, const void *));
};

template <class T> Vector::Vector(const std::vector<T> &v){
  static_assert(std::is_scalar<T>::value && sizeof(T) <= sizeof(LmnWord),
              "vector elements must be scalars.");
  LMN_ASSERT(v.size() > 0);
  this->init(v.size());
  memcpy(this->tbl, v.data(), sizeof(T) * v.size());
  this->num = v.size();
}

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
