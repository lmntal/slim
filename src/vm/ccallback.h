/*
 * ccallback.h
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

#ifndef LMN_CCALLBACK
#define LMN_CCALLBACK

/**
 * @ingroup VM
 * @defgroup CCallback
 * @{
 */

#include "ankerl/unordered_dense.hpp"

#include "element/element.h"
#include "lmntal.h"
#include "symbol.h"

/* LMNtalから呼ばれるCのコールバック */
class CCallback {
  void *f{};
  int   arity;

  inline static ankerl::unordered_dense::map<lmn_interned_str, CCallback *> ccallback_map{};

public:
  CCallback(void *f, int arity);
  ~CCallback() = default;
  int   get_arity() const;
  void *get_f() const;

  /**
   * @brief initialize ccallback module.
   */
  static void ccallback_init();

  /**
   * @brief get a function with its name.
   */
  static const struct CCallback *get_ccallback(lmn_interned_str name);

  /**
   * @brief register a function as a callback.
   *
   * @param name a callmack name used in LMNtal.
   * @param f a function that has a spacific signature (see \ref callback ).
   * @param arity the number of arguments of the callback.
   */
  static void lmn_register_c_fun(char const *name, void *f, int arity);

  /**
   * @brief finalize ccallback module.
   */
  static void ccallback_finalize();
};

#endif
