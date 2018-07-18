/*
 * mc.cpp
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
#ifndef LMN_GRAPHINFO_HPP
#define LMN_GRAPHINFO_HPP
#include "convertedgraph.hpp"
#include "json.hpp"
#include "lmntal.h"
#include "vm/atomlist.hpp"
#include "vm/vm.h"
#include <string>

void jsonDump(json_value * jVal);
int globalrootmem_id(json_value *jVal);
std::string mem_to_json(LmnMembraneRef mem);
std::string atom_to_json(LmnSymbolAtomRef atom);
std::string link_to_json(LmnSymbolAtomRef atom, int index);

struct Graphinfo {
  std::string json_string;
  json_value *json_val;
  int state_id;
  ConvertedGraph *cv;
  int globalRootMemID;

  void json_dump() {
    jsonDump(json_val);
  }

  Graphinfo(LmnMembraneRef mem) {
    json_string = mem_to_json(mem);
    json_val = json_parse(json_string.c_str(), json_string.size() + 1);
    state_id = -1;
    // jsonDump(json_val);
    globalRootMemID = globalrootmem_id(json_val);
    cv = new ConvertedGraph(json_val);
    // convertedGraphDump(cv);
  }
};

#endif
