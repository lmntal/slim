#include "convertedgraph.hpp"
#include "collection.hpp"

#include "trie.hpp"

void checkRelink(
    ConvertedGraphVertex *beforeCAtom, ConvertedGraphVertex *afterCAtom,
    std::map<size_t, ConvertedGraphVertex *> &afterConvertedHyperLinks,
    std::vector<ConvertedGraphVertex *> *relinkedVertices) {
  if (beforeCAtom && afterCAtom) {
    assert(beforeCAtom->links.size() == afterCAtom->links.size());
    for (auto i = 0; i < beforeCAtom->links.size(); i++) {
      auto &beforeLink = beforeCAtom->links[i];
      auto &afterLink = afterCAtom->links[i];

      if (beforeLink == afterLink)
        continue;

      pushConvertedVertexIntoDiffInfoStackWithoutOverlap(relinkedVertices,
                                                         afterCAtom);
      if (beforeLink.is_hyper()) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
            relinkedVertices, afterConvertedHyperLinks[beforeLink.data.ID]);
      }
      if (afterLink.is_hyper()) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
            relinkedVertices, afterConvertedHyperLinks[afterLink.data.ID]);
      }
    }
  } else if (beforeCAtom && !afterCAtom) {
    for (auto &link : beforeCAtom->links) {
      if (!link.is_hyper())
        continue;

      pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
          relinkedVertices, afterConvertedHyperLinks[link.data.ID]);
    }
  } else if (!beforeCAtom && afterCAtom) {
    for (auto &link : afterCAtom->links) {
      if (!link.is_hyper())
        continue;

      pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
          relinkedVertices, afterConvertedHyperLinks[link.data.ID]);
    }
  }
}

void add_proxy_mapping(ConvertedGraph *org, ConvertedGraph *copy,
                       std::map<int, int> *iso_m) {
  for (auto &kv : org->atoms) {
    if (!kv.second)
      continue;

    auto &org_atom = kv.second;
    if (org_atom->type != convertedInProxy)
      continue;

    int out_prox_id = org_atom->links[0].data.ID;
    int copied_hl_id = iso_m->at(org_atom->links[2].data.ID);
    for (auto &kv : copy->atoms) {
      if (!kv.second)
        continue;

      auto &copy_atom = kv.second;
      if (copy_atom->type != convertedInProxy)
        continue;

      auto &copy_hl_link = copy_atom->links[2];
      if (copy_hl_link.data.ID != copied_hl_id)
        continue;

      iso_m->emplace(org_atom->ID, copy_atom->ID);
      iso_m->emplace(out_prox_id, copy_atom->links[0].data.ID);
    }
  }
}

bool check_corresponding_atoms(const ConvertedGraphVertex &org_atom,
                               const ConvertedGraphVertex &copy_atom,
                               const std::map<int, int> &iso_m) {
  if (org_atom.links.size() != copy_atom.links.size()) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    return false;
  }

  for (int i = 0; i < org_atom.links.size(); i++) {
    auto &org_l = org_atom.links[i];
    auto &copy_l = copy_atom.links[i];
    if (org_l.attr != copy_l.attr) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      return false;
    }
    if (org_l.attr == INTEGER_ATTR || org_l.attr == DOUBLE_ATTR ||
        org_l.attr == STRING_ATTR || org_l.attr == GLOBAL_ROOT_MEM_ATTR) {
      if (org_l != copy_l) {
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        return false;
      }
    } else if (org_l.attr < 128) {
      auto it = iso_m.find(org_l.data.ID);
      auto index = (it != iso_m.end()) ? it->second : org_l.data.ID;

      if (index != copy_l.data.ID) {
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        return false;
      }
    } else if (org_l.attr == HYPER_LINK_ATTR) {
      auto it = iso_m.find(org_l.data.ID);
      if (it != iso_m.end()) {
        if (it->second != copy_l.data.ID) {
          printf("%s:%d\n", __FUNCTION__, __LINE__);
          return false;
        }
      }
    }
  }

  return true;
}

bool check_corresponding_hlatoms(const ConvertedGraphVertex &org_hl,
                                 const ConvertedGraphVertex &copy_hl,
                                 const std::map<int, int> &iso_m) {
  if (org_hl.links.size() != copy_hl.links.size()) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    return false;
  }

  for (auto &org_l : org_hl.links) {
    bool f = false;
    for (auto &copy_l : copy_hl.links) {
      if (org_l.attr != copy_l.attr) {
        continue;
      }

      if (org_l.attr == INTEGER_ATTR || org_l.attr == DOUBLE_ATTR ||
          org_l.attr == STRING_ATTR || org_l.attr == GLOBAL_ROOT_MEM_ATTR) {
        if (org_l != copy_l) {
          continue;
        }
      } else if (org_l.attr < 128) {
        auto it = iso_m.find(org_l.data.ID);
        auto index = (it != iso_m.end()) ? it->second : org_l.data.ID;
        if (index != copy_l.data.ID)
          continue;

      } else if (org_l.attr == HYPER_LINK_ATTR) {
        auto it = iso_m.find(org_l.data.ID);
        if (it != iso_m.end()) {
          if (it->second != copy_l.data.ID) {
            continue;
          }
        }
      } else {
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        exit(1);
      }
      f = true;
      break;
    }
    if (!f)
      return false;
  }
  return true;
}

bool check_iso_morphism(ConvertedGraph *org, ConvertedGraph *copy,
                        const std::map<int, int> &iso_m) {
  for (auto &kv : org->atoms) {
    if (!kv.second)
      continue;

    auto &org_atom = *kv.second;
    auto it = iso_m.find(org_atom.ID);
    auto index = (it != iso_m.end()) ? it->second : org_atom.ID;

    if (copy->atoms.find(index) == copy->atoms.end()) {
      std::cout << __FUNCTION__ << ":" << __LINE__ << std::endl;
      return false;
    }

    auto &copy_atom = *copy->atoms[index];
    if (!check_corresponding_atoms(org_atom, copy_atom, iso_m)) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      return false;
    }
  }

  for (auto &kv : org->hyperlinks) {
    if (!kv.second)
      continue;

    auto &org_hlatom = *kv.second;
    auto it = iso_m.find(org_hlatom.ID);
    auto index = (it != iso_m.end()) ? it->second : org_hlatom.ID;

    if (copy->hyperlinks.find(index) == copy->hyperlinks.end()) {
      std::cout << __FUNCTION__ << ":" << __LINE__ << std::endl;
      return false;
    }

    auto &copy_hlatom = *copy->hyperlinks[index];
    if (!check_corresponding_hlatoms(org_hlatom, copy_hlatom, iso_m)) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      return false;
    }
  }
  return true;
}

ConvertedGraphVertex *ConvertedGraph::at(const InheritedVertex &iVertex,
                                         int gapOfGlobalRootMemID) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  int afterID = iVertex.beforeID + gapOfGlobalRootMemID;
  switch (iVertex.type) {
  case convertedAtom:
    return this->atoms[afterID];
  case convertedHyperLink:
    return this->hyperlinks[afterID];
  default:
    CHECKER("unexpected vertex type\n");
    exit(EXIT_FAILURE);
    break;
  }
}

ConvertedGraphVertex *ConvertedGraph::at(int ID,
                                         ConvertedGraphVertexType type) {
  switch (type) {
  case convertedAtom:
    return this->atoms[ID];
  case convertedHyperLink:
    return this->hyperlinks[ID];
  default:
    CHECKER("unexpected vertex type\n");
    exit(EXIT_FAILURE);
    break;
  }
}

void ConvertedGraph::clearReferencesFromConvertedVerticesToInheritedVertices() {
  for (auto &v : this->atoms) {
    auto cBeforeVertex = v.second;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    // std::cout << *cBeforeVertex << std::endl;
    if (cBeforeVertex->correspondingVertexInTrie != nullptr) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      // std::cout << "^^^^^^^^^^^^^^^FIND PTR BUG^^^^^^^^^^^^^^^^^^^^" << std::endl;
      std::cout << *cBeforeVertex << std::endl;
      // std::cout << *cBeforeVertex->correspondingVertexInTrie << std::endl;
      // cBeforeVertex->correspondingVertexInTrie = nullptr;
    }
  }

  for (auto &v : this->hyperlinks) {
    auto cBeforeVertex = v.second;
    if (!cBeforeVertex) {
      cBeforeVertex->correspondingVertexInTrie = nullptr;
    }
  }
}

void ConvertedGraph::moveReferencesToAfterCG(ConvertedGraph *cg,
                                             std::map<int, int> &iso) {
  for (auto &v : this->atoms) {
    auto cBeforeVertex = v.second;
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    if (cBeforeVertex->correspondingVertexInTrie != nullptr) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      auto cAfterVertex = cg->at(iso[v.first], convertedAtom);
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << *cAfterVertex << std::endl;
      if (cAfterVertex->correspondingVertexInTrie == nullptr) {
	printf("%s:%d\n", __FUNCTION__, __LINE__);
	std::cout << "NULLPTR" << std::endl;
	std::cout << *cAfterVertex << std::endl;
      }
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      // cAfterVertex->correspondingVertexInTrie = cBeforeVertex->correspondingVertexInTrie;
      // cBeforeVertex->correspondingVertexInTrie = nullptr;
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << *cAfterVertex << std::endl;
      std::cout << *cAfterVertex->correspondingVertexInTrie << std::endl;
    }
  }
}

void cg_trie_reference_check(ConvertedGraph *cg) {
  std::cout << "----START REFERENCE CHECK----" << std::endl;
  bool f = true;
  for (auto &v : cg->atoms) {
    std::cout << "---------------" << std::endl;
    printf("pointer:%p\n", v.second);
    // std::cout << "pointer:" << v.second << std::endl;
    std::cout << *v.second << std::endl;
    std::cout << "--->" << std::endl;
    printf("cvIT pointer:%p\n", v.second->correspondingVertexInTrie);
    if (v.second->correspondingVertexInTrie == nullptr) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << "NULLPTR!!!" << std::endl;
    }

    std::cout << *v.second->correspondingVertexInTrie << std::endl;
    printf("cvITcv pointer:%p\n", v.second->correspondingVertexInTrie->correspondingVertex);
    std::cout << *v.second->correspondingVertexInTrie->correspondingVertex << std::endl;
    if (v.second->correspondingVertexInTrie->correspondingVertex != v.second) {

      std::cout << "POINTER IS NOT CORRECT!!!" << std::endl;
      f = false;

    }

    std::cout << "---------------" << std::endl;
  }
  if (!f)
    exit(0);
  std::cout << "----FINISH REFERENCE CHECK----" << std::endl;
}
