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
#include <algorithm>
#include <map>
#include <vector>
#include <ostream>
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
  LMNtalLink(const LMNtalLink &link) : attr(link.attr), data(link.data) {}
  ~LMNtalLink() {}

  bool is_hyper() const { return attr == HYPER_LINK_ATTR; }

  bool operator==(const LMNtalLink &link) const {
    if (attr != link.attr)
      return FALSE;

    switch (attr) {
    case INTEGER_ATTR:
      return data.integer == link.data.integer;
    case DOUBLE_ATTR:
      return data.dbl == link.data.dbl;
    case STRING_ATTR:
      return (strcmp(data.string, link.data.string) == 0);
    case HYPER_LINK_ATTR:
      return data.ID == link.data.ID;
    case GLOBAL_ROOT_MEM_ATTR:
      return data.ID == link.data.ID;
    default:
      if (attr < 128) {
        return data.ID == link.data.ID;
      } else {
        CHECKER("unexpected attr\n");
        exit(EXIT_FAILURE);
        return FALSE;
      }
    }
  }
  bool operator!=(const LMNtalLink &link) const { return !(*this == link); }
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
  std::vector<LMNtalLink> links;
  int ID;
  char name[NAME_LENGTH];
  Bool isPushedIntoDiffInfoStack;
  Bool isVisitedInBFS;
  InheritedVertex *correspondingVertexInTrie;

  ConvertedGraphVertex(ConvertedGraphVertexType t, int id, const char *Name) {
    type = t;
    ID = id;
    strcpy(name, Name);
    isPushedIntoDiffInfoStack = FALSE;
    isVisitedInBFS = FALSE;
    correspondingVertexInTrie = NULL;
  }

  ~ConvertedGraphVertex() {}
};

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

    LMNtalLink link(attr, data);
    cVertex->links.emplace_back(link);

    if (link.attr == HYPER_LINK_ATTR) {
      if (!hyperlinks[link.data.ID]) {
        auto cv_hyper =
            new ConvertedGraphVertex(convertedHyperLink, link.data.ID, "");
        hyperlinks[link.data.ID] = cv_hyper;
      }

      LMNtalData data;
      data.integer = cVertex->ID;
      hyperlinks[link.data.ID]->links.emplace_back(linkNumber, data);
    }
  }

  void convertGraphLinksArray(json_value *jVal, ConvertedGraphVertex *cVertex) {
    for (int i = 0; i < jVal->u.array.length; i++) {
      convertGraphLink(jVal->u.array.values[i], cVertex, i);
    }
  }

  void convertGraphAtom(json_value *jVal, const LMNtalLink &linkToParentMem) {
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

    auto cVertex = new ConvertedGraphVertex(t, LMNtalID(jVal), tmpName1);
    atoms[LMNtalID(jVal)] = cVertex;
    convertGraphLinksArray(jVal->u.object.values[2].value, cVertex);
    cVertex->links.emplace_back(linkToParentMem);
    if (linkToParentMem.attr != GLOBAL_ROOT_MEM_ATTR) {
      LMNtalData data;
      data.integer = LMNtalID(jVal);
      hyperlinks[linkToParentMem.data.ID]->links.emplace_back(
          cVertex->links.size() - 1, data);
    }
  }

  void convertGraphAtomsArray(json_value *jVal,
                              const LMNtalLink &linkToParentMem) {
    for (int i = 0; i < jVal->u.array.length; i++) {
      convertGraphAtom(jVal->u.array.values[i], linkToParentMem);
    }
  };

  void convertGraphMem(json_value *jVal, const LMNtalLink &linkToParentMem) {
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
    LMNtalLink linkToThisMem(HYPER_LINK_ATTR, data);

    cVertex->links.emplace_back(linkToThisMem);
    cVertex->links.emplace_back(linkToParentMem);
    data.integer = LMNtalID(jVal);
    cHyperlink->links.emplace_back(0, data);
    if (linkToParentMem.attr != GLOBAL_ROOT_MEM_ATTR) {
      data.integer = LMNtalID(jVal);
      hyperlinks[linkToParentMem.data.ID]->links.emplace_back(1, data);
    }

    convertGraphAtomsArray(jVal->u.object.values[2].value, linkToThisMem);
    convertGraphMemsArray(jVal->u.object.values[3].value, linkToThisMem);
  }

  void convertGraphMemsArray(json_value *jVal,
                             const LMNtalLink &linkToParentMem) {
    for (int i = 0; i < jVal->u.array.length; i++) {
      convertGraphMem(jVal->u.array.values[i], linkToParentMem);
    }
  }

  void connect_link(ConvertedGraphVertex *a, int ai, ConvertedGraphVertex *b,
                    int bi) {
    auto &x = a->links[ai];
    ConvertedGraphVertex *target_a = atoms[x.data.ID];
    auto &y = b->links[bi];
    ConvertedGraphVertex *target_b = atoms[y.data.ID];
    auto iter_a = std::find_if(
        target_a->links.begin(), target_a->links.end(),
        [&](const LMNtalLink &link) { return link.data.ID == a->ID; });
    *iter_a = y;

    auto iter_b = std::find_if(
        target_b->links.begin(), target_b->links.end(),
        [&](const LMNtalLink &link) { return link.data.ID == b->ID; });
    *iter_b = x;
  }

  void remove_hl_link(ConvertedGraphVertex *atom, int index) {
    auto hl_id = atom->links[index].data.ID;
    auto iter = hyperlinks.find(hl_id);
    if (iter == hyperlinks.end())
      return;

    auto hl_atom = iter->second;
    auto it = std::find_if(
        hl_atom->links.begin(), hl_atom->links.end(),
        [&](const LMNtalLink &link) { return link.data.ID == atom->ID; });
    hl_atom->links.erase(it);
  }

  void remove_proxy() {
    std::vector<std::pair<size_t, ConvertedGraphVertex *>> removed;
    for (auto &kv : atoms) {
      if (kv.second->type != convertedInProxy)
        continue;

      ConvertedGraphVertex *in_proxy = kv.second;
      int out_proxy_id = in_proxy->links.at(0).data.ID;
      ConvertedGraphVertex *out_proxy = atoms[out_proxy_id];
      connect_link(in_proxy, 1, out_proxy, 1);
      remove_hl_link(in_proxy, 2);
      if (out_proxy->links.at(2).attr == HYPER_LINK_ATTR) {
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
    LMNtalLink gRootMemLink(GLOBAL_ROOT_MEM_ATTR, data);
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

  ConvertedGraphVertex *at(const InheritedVertex &iVertex, int gapOfGlobalRootMemID);
  ConvertedGraphVertex *at(int ID, ConvertedGraphVertexType type);
  void clearReferencesFromConvertedVerticesToInheritedVertices();
};

bool check_iso_morphism(ConvertedGraph *org, ConvertedGraph *copy,
                        const std::map<int, int> &iso_m);
inline void pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
    std::vector<ConvertedGraphVertex *> *stack, ConvertedGraphVertex *cVertex) {
  if (!cVertex)
    return;
  if (cVertex->isPushedIntoDiffInfoStack)
    return;

  stack->push_back(cVertex);
  cVertex->isPushedIntoDiffInfoStack = true;
}
inline ConvertedGraphVertex *popConvertedVertexFromDiffInfoStackWithoutOverlap(
    std::vector<ConvertedGraphVertex *> *stack) {
  ConvertedGraphVertex *ret = stack->back();
  stack->pop_back();
  ret->isPushedIntoDiffInfoStack = false;
  return ret;
}
void checkRelink(
    ConvertedGraphVertex *beforeCAtom, ConvertedGraphVertex *afterCAtom,
    std::map<size_t, ConvertedGraphVertex *> &afterConvertedHyperLinks,
    std::vector<ConvertedGraphVertex *> *relinkedVertices);

inline std::ostream &operator<<(std::ostream &os, const LMNtalLink &link) {
  return os << "<" << link.attr << "," << link.data.ID << ">";
}

inline std::ostream &operator<<(std::ostream &os, const ConvertedGraphVertex &cVertex) {
  int i;

  if (cVertex.type == convertedAtom) {
    os << "type:ATOM\n";
  } else if (cVertex.type == convertedHyperLink) {
    os << "type:HYPERLINK\n";
  } else if (cVertex.type == convertedInProxy or
             cVertex.type == convertedOutProxy) {
    os << "type:PROXY\n";
  }

  os << "ID:" << cVertex.ID << "\n";
  os << "name:" << cVertex.name << "\n";

  os << "links:";
  for (i = 0; i < cVertex.links.size(); i++) {
    if (i != 0) {
      os << ",";
    }
    os << cVertex.links[i];
  }
  os << "\n";
  return os;
}

inline std::ostream &operator<<(std::ostream &os, const ConvertedGraph &cGraph) {
  os << "CONVERTED ATOMS:\n";
  for (auto &kv : cGraph.atoms) {
    os << kv.first << ":";
    os << *kv.second;
    os << "\n";
  }
  os << "CONVERTED HYPERLINKS:\n";
  for (auto &kv : cGraph.hyperlinks) {
    os << kv.first << ":";
    os << *kv.second;
    os << "\n";
  }
  return os;
}
#endif
