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

namespace GraphInfo {
std::string mem_to_json(LmnMembraneRef mem);
std::string atom_to_json(LmnSymbolAtomRef atom);
std::string link_to_json(LmnSymbolAtomRef atom, int index);

struct Graphinfo {
  std::string json_string;
  json_value *json_val;
  int state_id;
  ConvertedGraph *cv;
  int globalRootMemID;

  Graphinfo(LmnMembraneRef mem) {
    std::string json = mem_to_json(mem);
    json_val = json_parse(json.c_str(), json.size()+1);
    state_id = -1;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  }
};

std::string mem_to_json(LmnMembraneRef mem) {
  std::string s = "";
  if (!mem)
    return s;
  s += "{";
  s += "\"id\":";
  s += std::to_string((int)lmn_mem_id(mem));
  s += ",\"name\":\"";
  s += LMN_MEM_NAME(mem);
  s += "\",";
  s += "\"atoms\":[";
  {
    AtomListEntryRef ent;
    LmnFunctor f;
    BOOL needs_comma = FALSE;
    EACH_ATOMLIST_WITH_FUNC(mem, ent, f, ({
                              LmnSymbolAtomRef atom;
                              if (LMN_IS_EX_FUNCTOR(f)) {
                                continue;
                              }
                              EACH_ATOM(atom, ent, ({
                                          if (needs_comma)
                                            s += ",";
                                          needs_comma = TRUE;
                                          s += atom_to_json(atom);
                                        }));
                            }));
  }
  std::cout << s << std::endl;
  return s;
}

std::string atom_to_json(LmnSymbolAtomRef atom) {
  std::string s = "";
  int i;
  int arity;
  s += "{";
  s += "\"id\":";
  s += std::to_string((int)LMN_SATOM_ID(atom));
  s += ",";
  s += "\"name\":\"";
  s += LMN_SATOM_STR(atom);
  s += "\",";
  s += "\"links\":[";
  BOOL needs_comma = FALSE;
  for (i = 0, arity = LMN_SATOM_GET_LINK_NUM(atom); i < arity; i++) {
    if (needs_comma)
      s += ",";
    needs_comma = TRUE;
    s += link_to_json(atom, i);
  }
  s += "]}";
  return s;
}

std::string link_to_json(LmnSymbolAtomRef atom, int index) {
  std::string s = "";
  LmnLinkAttr attr;
  void *data;

  attr = LMN_SATOM_GET_ATTR(atom, index);
  data = (void *)LMN_SATOM_GET_LINK(atom, index);

  s += "{";
  s += "\"attr\":";
  s += std::to_string((int)attr);
  s += ",";

  if (LMN_ATTR_IS_DATA(attr)) {
    switch (attr) {
    case LMN_INT_ATTR:
      s += "\"data\":";
      s += std::to_string((int)((LmnWord)data));
      break;
    case LMN_DBL_ATTR:
    case LMN_CONST_DBL_ATTR:
      s += "\"data\":";
      s += std::to_string(lmn_get_double((LmnDataAtomRef)data));
      break;
    case LMN_SP_ATOM_ATTR:
    case LMN_CONST_STR_ATTR:
      s += "\"data\":\"\\\"";
      s += lmn_string_c_str((LmnStringRef)data);
      s += "\\\"\"";
      break;
    case LMN_HL_ATTR: {
      LmnSymbolAtomRef a = (LmnSymbolAtomRef)data;
      HyperLink *root = LMN_HL_ATOM_ROOT_HL(a);
      s += "\"data\":";
      s += std::to_string((int)root->id);
    } break;
    default:
      break;
    }
  } else {
    LmnSymbolAtomRef a = (LmnSymbolAtomRef)data;
    if (a != NULL) {
      s += "\"data\":";
      s += std::to_string((int)LMN_SATOM_ID(a));
    }
  }
  s += "}";
  return s;
}
} // namespace GraphInfo

#endif
