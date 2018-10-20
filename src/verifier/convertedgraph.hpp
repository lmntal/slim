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
#include "collection.hpp"
#include "json.hpp"
#include <map>
#include <vector>
#define NAME_LENGTH 256
#define INTEGER_ATTR 128
#define DOUBLE_ATTR 129
#define STRING_ATTR 131
#define HYPER_LINK_ATTR 138
#define GLOBAL_ROOT_MEM_ATTR 255
#define PROXY_ATTR 140

struct InheritedVertex;

union LMNtalData {
  int integer;
  double dbl;
  char string[NAME_LENGTH];
  int ID;
};

struct LMNtalLink {
  int attr;
  LMNtalData data;
  LMNtalLink(int a, LMNtalData d) {
    attr = a;
    data = d;
  }
  ~LMNtalLink() {}
};

typedef enum {
  convertedNone,
  convertedAtom,
  convertedHyperLink,
  convertedNull,
  convertedInProxy,
  convertedOutProxy
} ConvertedGraphVertexType;

struct ConvertedGraphVertex {
  ConvertedGraphVertexType type;
  std::vector<LMNtalLink *> *links;
  int ID;
  char name[NAME_LENGTH];
  Bool isPushedIntoDiffInfoStack;
  Bool isVisitedInBFS;
  InheritedVertex *correspondingVertexInTrie;

  ConvertedGraphVertex(ConvertedGraphVertexType t, int id, const char *Name) {
    type = t;
    ID = id;
    links = new std::vector<LMNtalLink *>;
    strcpy(name, Name);
    isPushedIntoDiffInfoStack = FALSE;
    isVisitedInBFS = FALSE;
    correspondingVertexInTrie = NULL;
  }

  ~ConvertedGraphVertex() {
    for (auto l : *links) {
      delete l;
    }
    delete links;
  }
};
Bool isEqualLinks(LMNtalLink *a, LMNtalLink *b);

struct ConvertedGraph {
public:
  std::map<size_t, ConvertedGraphVertex *> atoms;
  std::map<size_t, ConvertedGraphVertex *> hyperlinks;

private:
  int LMNtalID(json_value *jVal) {
    return jVal->u.object.values[0].value->u.integer;
  }

  void LMNtalNameCopy(json_value *jVal, char *dstString) {
    strcpy(dstString, jVal->u.object.values[1].value->u.string.ptr);
  }

  LMNtalLink *copyLink(LMNtalLink *link) {
    return new LMNtalLink(link->attr, link->data);
  }

  void convertGraphLink(json_value *jVal, ConvertedGraphVertex *cVertex,
                        int linkNumber) {
    int attr = jVal->u.object.values[0].value->u.integer;
    LMNtalData data;

    switch (attr) {
    case INTEGER_ATTR:
      data.integer = jVal->u.object.values[1].value->u.integer;
      break;
    case DOUBLE_ATTR:
      data.dbl = jVal->u.object.values[1].value->u.dbl;
      break;
    case STRING_ATTR:
      strcpy(data.string, jVal->u.object.values[1].value->u.string.ptr);
      break;
    case HYPER_LINK_ATTR:
      data.ID = jVal->u.object.values[1].value->u.integer;
      break;
    case GLOBAL_ROOT_MEM_ATTR:
      data.ID = jVal->u.object.values[1].value->u.integer;
      break;
    default:
      if (attr < 128) {
        data.ID = jVal->u.object.values[1].value->u.integer;
      } else {
        CHECKER("unexpected attr\n");
        exit(EXIT_FAILURE);
      }
      break;
    }

    LMNtalLink *link = new LMNtalLink(attr, data);
    pushStack(cVertex->links, link);

    if (link->attr == HYPER_LINK_ATTR) {
      if (!hyperlinks[link->data.ID]) {
        ConvertedGraphVertex *cv_hyper =
            new ConvertedGraphVertex(convertedHyperLink, link->data.ID, "");
        hyperlinks[link->data.ID] = cv_hyper;
      }

      LMNtalData data;
      data.integer = cVertex->ID;
      LMNtalLink *linkFromHyperLink = new LMNtalLink(linkNumber, data);
      pushStack(hyperlinks[link->data.ID]->links, copyLink(linkFromHyperLink));
    }
  }

  void convertGraphLinksArray(json_value *jVal, ConvertedGraphVertex *cVertex) {
    for (int i = 0; i < jVal->u.array.length; i++) {
      convertGraphLink(jVal->u.array.values[i], cVertex, i);
    }
  }

  void convertGraphAtom(json_value *jVal, LMNtalLink *linkToParentMem) {
    char tmpName1[NAME_LENGTH];
    strcpy(tmpName1, "ATOM_");
    char tmpName2[NAME_LENGTH];
    LMNtalNameCopy(jVal, tmpName2);
    strcat(tmpName1, tmpName2);
    ConvertedGraphVertexType t;
    if (strcmp(tmpName2, "$out") == 0)
      t = convertedOutProxy;
    else if (strcmp(tmpName2, "$in") == 0)
      t = convertedInProxy;
    else
      t = convertedAtom;

    ConvertedGraphVertex *cVertex =
        new ConvertedGraphVertex(t, LMNtalID(jVal), tmpName1);
    atoms[LMNtalID(jVal)] = cVertex;
    convertGraphLinksArray(jVal->u.object.values[2].value, cVertex);
    pushStack(cVertex->links, copyLink(linkToParentMem));
    if (linkToParentMem->attr != GLOBAL_ROOT_MEM_ATTR) {
      LMNtalData data;
      data.integer = LMNtalID(jVal);
      LMNtalLink *hl = new LMNtalLink(cVertex->links->size() - 1, data);
      pushStack(hyperlinks[linkToParentMem->data.ID]->links, hl);
    }
  }

  void convertGraphAtomsArray(json_value *jVal, LMNtalLink *linkToParentMem) {
    for (int i = 0; i < jVal->u.array.length; i++) {
      convertGraphAtom(jVal->u.array.values[i], linkToParentMem);
    }
  };

  void convertGraphMem(json_value *jVal, LMNtalLink *linkToParentMem) {
    char tmpName1[NAME_LENGTH];
    strcpy(tmpName1, "MEM_");
    char tmpName2[NAME_LENGTH];
    LMNtalNameCopy(jVal, tmpName2);
    strcat(tmpName1, tmpName2);

    ConvertedGraphVertex *cVertex =
        new ConvertedGraphVertex(convertedAtom, LMNtalID(jVal), tmpName1);
    ConvertedGraphVertex *cHyperlink =
        new ConvertedGraphVertex(convertedHyperLink, LMNtalID(jVal), "");

    atoms[LMNtalID(jVal)] = cVertex;
    hyperlinks[LMNtalID(jVal)] = cHyperlink;

    LMNtalData data;
    data.integer = LMNtalID(jVal);
    LMNtalLink *linkToThisMem = new LMNtalLink(HYPER_LINK_ATTR, data);

    pushStack(cVertex->links, copyLink(linkToThisMem));
    pushStack(cVertex->links, copyLink(linkToParentMem));
    data.integer = LMNtalID(jVal);
    LMNtalLink *hl = new LMNtalLink(0, data);
    pushStack(cHyperlink->links, hl);
    if (linkToParentMem->attr != GLOBAL_ROOT_MEM_ATTR) {
      data.integer = LMNtalID(jVal);
      LMNtalLink *newlink = new LMNtalLink(1, data);
      pushStack(hyperlinks[linkToParentMem->data.ID]->links, newlink);
    }

    convertGraphAtomsArray(jVal->u.object.values[2].value, linkToThisMem);
    convertGraphMemsArray(jVal->u.object.values[3].value, linkToThisMem);
  }

  void convertGraphMemsArray(json_value *jVal, LMNtalLink *linkToParentMem) {
    for (int i = 0; i < jVal->u.array.length; i++) {
      convertGraphMem(jVal->u.array.values[i], linkToParentMem);
    }
  }

  void connect_link(ConvertedGraphVertex *a, int ai, ConvertedGraphVertex *b,
                    int bi) {
    LMNtalLink *x = a->links->at(ai);
    ConvertedGraphVertex *target_a = atoms[x->data.ID];
    LMNtalLink *y = b->links->at(bi);
    ConvertedGraphVertex *target_b = atoms[y->data.ID];
    for (auto i = target_a->links->begin(); i != target_a->links->end(); i++) {
      if ((*i)->data.ID == a->ID) {
        delete (*i);
        *i = copyLink(y);
        break;
      }
    }
    for (auto i = target_b->links->begin(); i != target_b->links->end(); i++) {
      if ((*i)->data.ID == b->ID) {
        delete (*i);
        *i = copyLink(x);
        break;
      }
    }
  }

  void remove_hl_link(ConvertedGraphVertex *atom, int index) {
    int hl_id = atom->links->at(index)->data.ID;
    if (hyperlinks[hl_id] != NULL) {
      ConvertedGraphVertex *hl_atom = hyperlinks[hl_id];
      for (auto i = hl_atom->links->begin(); i != hl_atom->links->end(); i++) {
        if ((*i)->data.ID == atom->ID) {
          delete (*i);
          hl_atom->links->erase(i);
        }
      }
    }
  }

  void remove_proxy() {
    std::vector<std::pair<size_t, ConvertedGraphVertex *>> removed;
    for (auto &kv : atoms) {
      if (kv.second->type != convertedInProxy)
        continue;

      ConvertedGraphVertex *in_proxy = kv.second;
      int out_proxy_id = in_proxy->links->at(0)->data.ID;
      ConvertedGraphVertex *out_proxy = atoms[out_proxy_id];
      connect_link(in_proxy, 1, out_proxy, 1);
      remove_hl_link(in_proxy, 2);
      if (out_proxy->links->at(2)->attr == HYPER_LINK_ATTR) {
        remove_hl_link(out_proxy, 2);
      }
      removed.push_back(kv);
      removed.emplace_back(out_proxy_id, out_proxy);
    }

    for (auto &kv : removed) {
      delete kv.second;
      atoms.erase(kv.first);
    }
  }

public:
  ConvertedGraph(json_value *json_val) {
    LMNtalData data;
    data.integer = 0;
    LMNtalLink *gRootMemLink = new LMNtalLink(GLOBAL_ROOT_MEM_ATTR, data);
    convertGraphAtomsArray(json_val->u.object.values[2].value, gRootMemLink);
    convertGraphMemsArray(json_val->u.object.values[3].value, gRootMemLink);
    remove_proxy();
  }

  ~ConvertedGraph() {
    for (auto v : atoms)
      delete v.second;
    for (auto v : hyperlinks)
      delete v.second;
  }
};

void convertedGraphDump(ConvertedGraph *cGraph);
bool check_iso_morphism(ConvertedGraph *org, ConvertedGraph *copy,
                        std::map<int, int> &iso_m);
void LMNtalLinkDump(LMNtalLink *link);
void convertedGraphVertexDump(ConvertedGraphVertex *cVertex);
void convertedGraphVertexDumpCaster(void *const &cVertex);
template <typename S>
void pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
    S *stack, ConvertedGraphVertex *cVertex) {
  if (cVertex != NULL) {
    if (!cVertex->isPushedIntoDiffInfoStack) {
      pushStack(stack, cVertex);
      cVertex->isPushedIntoDiffInfoStack = true;
    }
  }
}
template <typename S>
ConvertedGraphVertex *
popConvertedVertexFromDiffInfoStackWithoutOverlap(S *stack) {
  ConvertedGraphVertex *ret = (ConvertedGraphVertex *)popStack(stack);
  ret->isPushedIntoDiffInfoStack = false;
  return ret;
}
template <typename S>
void checkRelink(
    ConvertedGraphVertex *beforeCAtom, ConvertedGraphVertex *afterCAtom,
    std::map<size_t, ConvertedGraphVertex *> &afterConvertedHyperLinks,
    S *relinkedVertices);
Bool isEqualLinks(LMNtalLink *linkA, LMNtalLink *linkB);
Bool isHyperLink(LMNtalLink *link);
#endif
