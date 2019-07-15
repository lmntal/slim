/**
 * @file state_dumper.h
 *
 *   Copyright (c) 2019, Ueda Laboratory LMNtal Group
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
 */

#ifndef SLIM_VERIFIER_STATE_DUMPER_H
#define SLIM_VERIFIER_STATE_DUMPER_H

class StateSpace;
class State;
class LmnMembrane;

#include "lmntal.h"

#include <cstdio>
#include <memory>
#include <string>

/**
 * A dumper class for states and statespaces.
 *
 * Usage:
 * @code
 * StateSpace *ss = ...;
 * auto sd = StateDumper::from_env(stdout);
 * sd->dump(ss);
 * @endcode
 *
 * @todo インターフェースは多少整理したので中身を綺麗にする
 */
class StateDumper {
public:
  virtual ~StateDumper() {}

  virtual void dump(StateSpace *ss) = 0;
  void dump(State *s) { dump(s, nullptr); }

  //! @todo このメンバ関数をなんとかしたい
  void state_print_mem(State *s);

  //! Factory method used to create a dumper its format depends on command-line
  //! arguments passed to SLIM.
  static std::unique_ptr<StateDumper> from_env(FILE *fp) {
    return std::unique_ptr<StateDumper>(from_env_ptr(fp));
  }

protected:
  //! use factory method instead.
  StateDumper(FILE *fp) : _fp(fp) {}
  void dump(State *s, const StateSpace *_owner);
  void state_print_label(State *s, const StateSpace *_owner);
  void state_print_transition(State *s, const StateSpace *_owner);
  virtual void print_mem(LmnMembrane *mem);

  //! output stream
  FILE *_fp;

private:
  static StateDumper *from_env_ptr(FILE *fp);

  virtual MCdumpFormat dump_format() const = 0;
  virtual bool need_id_foreach_trans() const = 0;
  virtual std::string state_separator() const = 0;
  virtual std::string trans_separator() const = 0;
  virtual std::string label_begin() const = 0;
  virtual std::string label_end() const = 0;

  virtual void dump_state_data(State *s, unsigned long print_id,
                               StateSpace *owner) {
    throw std::runtime_error("unexpected");
  }
  virtual void print_state_label(State *s, StateSpace *owner) {
    throw std::runtime_error("unexpected");
  }
};

#endif
