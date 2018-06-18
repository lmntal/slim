/*
 * convertedgraph.hpp
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
#ifndef LMN_CONVERTEDGRAPH_HPP
#define LMN_CONVERTEDGRAPH_HPP
#include "lmntal.h"
#include "vm/atomlist.hpp"
#include "vm/vm.h"
#include <iostream>
#include <string>
#include <vector>
typedef enum {
  convertedNone,
  convertedAtom,
  convertedHyperLink,
  convertedNull
} ConvertedGraphVertexType;

union AtomData {
  int integer;
  double dbl;
  std::string s;
  int id;
  AtomData() {}
  ~AtomData() {}
};

struct ConvertedGraphLink {
  int attr;
  AtomData data;

public:
  ConvertedGraphLink(LmnSymbolAtomRef atom, int pos) {
    attr = LMN_SATOM_GET_ATTR(atom, pos);
    if (!LMN_ATTR_IS_DATA(attr)) {
      data.id = LMN_SATOM_ID((LmnSymbolAtomRef)LMN_SATOM_GET_LINK(atom, pos));
    } else {
      switch (attr) {
      case LMN_INT_ATTR:
        data.integer = (LmnWord)LMN_SATOM_GET_LINK(atom, pos);
        break;
      case LMN_DBL_ATTR:
        data.dbl =
            lmn_get_double((LmnDataAtomRef)LMN_SATOM_GET_LINK(atom, pos));
        break;
      }
    }
    printf("%s:%d data=%d\n", __FUNCTION__, __LINE__, data.integer);
  }
};

struct ConvertedGraphVertex {
  ConvertedGraphVertexType type;
  std::vector<ConvertedGraphLink *> links;
  int id;
  // std::string name;
  const char *name;
  bool isPushedIntoDiffInfoStack;
  bool isVisitedInBFS;

  ConvertedGraphVertex(LmnSymbolAtomRef atom) {
    LmnFunctor f = LMN_SATOM_GET_FUNCTOR(atom);
    LmnArity arity = LMN_FUNCTOR_ARITY(f);
    type = convertedAtom;
    id = LMN_SATOM_ID(atom);
    // name = std::string(lmn_id_to_name(LMN_FUNCTOR_NAME_ID(f)));
    name = lmn_id_to_name(LMN_FUNCTOR_NAME_ID(f));
    // printf("%s:%d name=%s\n", __FILE__, __LINE__, name);
    isPushedIntoDiffInfoStack = false;
    isVisitedInBFS = false;
    for (int i = 0; i < arity; i++) {
      ConvertedGraphLink *l = new ConvertedGraphLink(atom, i);
      printf("%s:%d data=%d\n", __FUNCTION__, __LINE__, l->data.integer);
      links.push_back(l);
    }
  }
};

struct ConvertedGraph {
  std::vector<ConvertedGraphVertex *> atoms;
  std::vector<ConvertedGraphVertex *> hyperlinkatoms;

  void convert_atoms(LmnMembraneRef mem,
                     std::vector<ConvertedGraphVertex *> *atoms) {
    AtomListEntryRef ent;
    EACH_ATOMLIST(
        mem, ent, ({
          LmnSymbolAtomRef atom;
          EACH_ATOM(atom, ent, ({
                      if (!LMN_IS_PROXY_FUNCTOR(LMN_SATOM_GET_FUNCTOR(atom)) &&
                          !LMN_FUNC_IS_HL(LMN_SATOM_GET_FUNCTOR(atom))) {
                        ConvertedGraphVertex *v =
                            new ConvertedGraphVertex(atom);
                        (*atoms).push_back(v);
                      }
                    }));
        }));
    // return cv;
  }

  std::vector<ConvertedGraphVertex *> convert_hyperlinks(LmnMembraneRef mem) {
    std::vector<ConvertedGraphVertex *> cv;
    return cv;
  }

  // memに対応するvertexを返す
  //  1. memに対応するvertex(m)を作成
  //  2. mem内のatomをvertexに変換, mへのハイパーリンクを張る
  //  3. mem内のハイパーリンクを変換
  //  4. 子膜に対して再帰的にconvert_memを適用
  //  5. 返ってきたvertexからmへハイパーリンクを張る
  //  return m
  void convert_mem(LmnMembraneRef mem,
                   std::vector<ConvertedGraphVertex *> *atoms,
                   std::vector<ConvertedGraphVertex *> *hyperlinkatoms) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
  }

  //  1. mem内のatomをvertexに変換，gm属性でリンクを張る
  //  2. mem内のハイパーリンクを変換
  //  3. 子膜に対してconvert_memを適用
  //  4. 返ってきたvertexにgm属性でリンクを張る
  ConvertedGraph(LmnMembraneRef mem) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    convert_atoms(mem, &atoms);
    for (auto i = atoms.begin(); i != atoms.end(); i++) {
      printf("%s:%d name = %s\n", __FUNCTION__, __LINE__, (*i)->name);
      for (auto j = (*i)->links.begin(); j != (*i)->links.end(); j++) {
        printf("<%d, %d> ", (*j)->attr, (*j)->data.integer);
      }
      printf("\n");
    }
    convert_mem(mem, &atoms, &hyperlinkatoms);
    // hyperlinkatoms = convert_hyperlinks(mem);
  }
};

#endif
