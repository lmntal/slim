#include "convertedgraph.hpp"
#include "collection.hpp"

void checkRelink(
    ConvertedGraphVertex *beforeCAtom, ConvertedGraphVertex *afterCAtom,
    std::map<size_t, ConvertedGraphVertex *> &afterConvertedHyperLinks,
    std::vector<ConvertedGraphVertex *> *relinkedVertices) {
  if (beforeCAtom != NULL && afterCAtom != NULL) {
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
  } else if (beforeCAtom != NULL && afterCAtom == NULL) {
    for (auto &beforeLink : beforeCAtom->links) {
      if (!beforeLink.is_hyper())
        continue;

      pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
          relinkedVertices, afterConvertedHyperLinks[beforeLink.data.ID]);
    }
  } else if (beforeCAtom == NULL && afterCAtom != NULL) {
    for (auto &afterLink : afterCAtom->links) {
      if (!afterLink.is_hyper())
        continue;

      pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
          relinkedVertices, afterConvertedHyperLinks[afterLink.data.ID]);
    }
  }
}

void convertedGraphDump(ConvertedGraph *cGraph) {
  fprintf(stdout, "CONVERTED ATOMS:\n");
  for (auto &kv : cGraph->atoms) {
    fprintf(stdout, "%d:", kv.first);
    convertedGraphVertexDump(kv.second);
    printf("\n");
  }
  fprintf(stdout, "CONVERTED HYPERLINKS:\n");
  for (auto &kv : cGraph->hyperlinks) {
    fprintf(stdout, "%d:", kv.first);
    convertedGraphVertexDump(kv.second);
    printf("\n");
  }
}

void LMNtalLinkDump(LMNtalLink *link) {
  fprintf(stdout, "<%d,%d>", link->attr, link->data.ID);
  return;
}

void convertedGraphVertexDump(ConvertedGraphVertex *cVertex) {
  int i;

  if (cVertex->type == convertedAtom) {
    fprintf(stdout, "type:ATOM\n");
  } else if (cVertex->type == convertedHyperLink) {
    fprintf(stdout, "type:HYPERLINK\n");
  } else if (cVertex->type == convertedInProxy or
             cVertex->type == convertedOutProxy) {
    fprintf(stdout, "type:PROXY\n");
  }

  fprintf(stdout, "ID:%d\n", cVertex->ID);
  fprintf(stdout, "name:%s\n", cVertex->name);

  fprintf(stdout, "links:");
  for (i = 0; i < cVertex->links.size(); i++) {
    if (i != 0) {
      fprintf(stdout, ",");
    }
    LMNtalLinkDump(&cVertex->links[i]);
  }
  fprintf(stdout, "\n");
}

void convertedGraphVertexDumpCaster(void *cVertex) {
  convertedGraphVertexDump((ConvertedGraphVertex *)cVertex);
  return;
}

void add_proxy_mapping(ConvertedGraph *org, ConvertedGraph *copy,
                       std::map<int, int> *iso_m) {
  for (auto &kv : org->atoms) {
    if (kv.second == NULL)
      continue;

    ConvertedGraphVertex *org_atom = kv.second;
    if (org_atom->type != convertedInProxy)
      continue;

    int out_prox_id = org_atom->links[0].data.ID;
    int copied_hl_id = iso_m->at(org_atom->links[2].data.ID);
    for (auto &kv : copy->atoms) {
      if (kv.second == NULL)
        continue;

      ConvertedGraphVertex *copy_atom = kv.second;
      if (copy_atom->type != convertedInProxy)
        continue;

      auto &copy_hl_link = copy_atom->links[2];
      if (copy_hl_link.data.ID != copied_hl_id)
        continue;

      iso_m->insert(std::make_pair(org_atom->ID, copy_atom->ID));
      iso_m->insert(std::make_pair(out_prox_id, copy_atom->links[0].data.ID));
    }
  }
}

bool check_corresponding_atoms(ConvertedGraphVertex *org_atom,
                               ConvertedGraphVertex *copy_atom,
                               std::map<int, int> &iso_m) {
  if (copy_atom == NULL) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    return false;
  }
  if (org_atom->links.size() != copy_atom->links.size()) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    return false;
  }
  for (int i = 0; i < org_atom->links.size(); i++) {
    auto &org_l = org_atom->links[i];
    auto &copy_l = copy_atom->links[i];
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
      if (it != iso_m.end()) {
        if (it->second != copy_l.data.ID) {
          printf("%s:%d\n", __FUNCTION__, __LINE__);
          return false;
        }
      } else {
        if (org_l.data.ID != copy_l.data.ID) {
          printf("%s:%d\n", __FUNCTION__, __LINE__);
          return false;
        }
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

bool check_corresponding_hlatoms(ConvertedGraphVertex *org_hl,
                                 ConvertedGraphVertex *copy_hl,
                                 std::map<int, int> &iso_m) {
  if (copy_hl == NULL) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    return false;
  }
  if (org_hl->links.size() != copy_hl->links.size()) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    return false;
  }
  for (auto &org_l : org_hl->links) {
    bool f = false;
    for (auto &copy_l : copy_hl->links) {
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
        if (it != iso_m.end()) {
          if (it->second != copy_l.data.ID) {
            continue;
          }
        } else {
          if (org_l.data.ID != copy_l.data.ID) {
            continue;
          }
        }
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
                        std::map<int, int> &iso_m) {
  for (auto &kv : org->atoms) {
    if (kv.second != NULL) {
      ConvertedGraphVertex *org_atom = kv.second;
      ConvertedGraphVertex *copy_atom;
      auto it = iso_m.find(org_atom->ID);
      if (it != iso_m.end()) {
        copy_atom = copy->atoms[it->second];
      } else {
        copy_atom = copy->atoms[org_atom->ID];
      }
      // printf("org=%d copy=%d\n", org_atom->ID, copy_atom->ID);
      if (!check_corresponding_atoms(org_atom, copy_atom, iso_m)) {
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        return false;
      }
    }
  }
  for (auto &kv : org->hyperlinks) {
    if (kv.second != NULL) {
      ConvertedGraphVertex *org_hlatom = kv.second;
      ConvertedGraphVertex *copy_hlatom;
      // printf("%s:%d\n", __FUNCTION__, __LINE__);
      auto it = iso_m.find(org_hlatom->ID);
      if (it != iso_m.end()) {
        copy_hlatom = copy->hyperlinks[it->second];
      } else {
        copy_hlatom = copy->hyperlinks[org_hlatom->ID];
      }
      // printf("org_hl=%d copy_hl=%d\n", org_hlatom->ID, copy_hlatom->ID);
      if (!check_corresponding_hlatoms(org_hlatom, copy_hlatom, iso_m)) {
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        return false;
      }
    }
  }
  return true;
}
