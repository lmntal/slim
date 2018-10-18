#include "convertedgraph.hpp"
#include "collection.hpp"
Bool isEqualLinks(LMNtalLink *linkA, LMNtalLink *linkB) {
  if (linkA->attr != linkB->attr) {
    return FALSE;
  }

  switch (linkA->attr) {
  case INTEGER_ATTR:
    return linkA->data.integer == linkB->data.integer;
    break;
  case DOUBLE_ATTR:
    return linkA->data.dbl == linkB->data.dbl;
    break;
  case STRING_ATTR:
    if (strcmp(linkA->data.string, linkB->data.string) == 0) {
      return TRUE;
    } else {
      return FALSE;
    }
    break;
  case HYPER_LINK_ATTR:
    return linkA->data.ID == linkB->data.ID;
    break;
  case GLOBAL_ROOT_MEM_ATTR:
    return linkA->data.ID == linkB->data.ID;
    break;
  default:
    if (linkA->attr < 128) {
      return linkA->data.ID == linkB->data.ID;
    } else {
      CHECKER("unexpected attr\n");
      exit(EXIT_FAILURE);
      return FALSE;
    }
    break;
  }
}

Bool isHyperLink(LMNtalLink *link) { return link->attr == HYPER_LINK_ATTR; }

template <typename S>
void checkRelink(ConvertedGraphVertex *beforeCAtom,
                 ConvertedGraphVertex *afterCAtom,
                 unbound_vector<ConvertedGraphVertex *> *afterConvertedHyperLinks,
                 S *relinkedVertices) {
  if (beforeCAtom != NULL && afterCAtom != NULL) {
    int i;
    assert(beforeCAtom->links->size() == afterCAtom->links->size());
    for (i = 0; i < beforeCAtom->links->size(); i++) {
      LMNtalLink *beforeLink = (LMNtalLink *)readStack(beforeCAtom->links, i);
      LMNtalLink *afterLink = (LMNtalLink *)readStack(afterCAtom->links, i);

      if (!isEqualLinks(beforeLink, afterLink)) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(relinkedVertices,
                                                           afterCAtom);
        if (isHyperLink(beforeLink)) {
          pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
              relinkedVertices,
              (ConvertedGraphVertex *)afterConvertedHyperLinks->read(
                  beforeLink->data.ID));
        }
        if (isHyperLink(afterLink)) {
          pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
              relinkedVertices,
              (ConvertedGraphVertex *)afterConvertedHyperLinks->read(
                  afterLink->data.ID));
        }
      }
    }
  } else if (beforeCAtom != NULL && afterCAtom == NULL) {
    int i;
    for (i = 0; i < beforeCAtom->links->size(); i++) {
      LMNtalLink *beforeLink = (LMNtalLink *)readStack(beforeCAtom->links, i);

      if (isHyperLink(beforeLink)) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
            relinkedVertices,
            (ConvertedGraphVertex *)afterConvertedHyperLinks->read(
                beforeLink->data.ID));
      }
    }
  } else if (beforeCAtom == NULL && afterCAtom != NULL) {
    int i;
    for (i = 0; i < afterCAtom->links->size(); i++) {
      LMNtalLink *afterLink = (LMNtalLink *)readStack(afterCAtom->links, i);

      if (isHyperLink(afterLink)) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
            relinkedVertices,
            (ConvertedGraphVertex *)afterConvertedHyperLinks->read(
                afterLink->data.ID));
      }
    }
  } else {
  }
  return;
}

void convertedGraphDump(ConvertedGraph *cGraph) {
  fprintf(stdout, "CONVERTED ATOMS:\n");
  cGraph->atoms->dump(convertedGraphVertexDump);
  fprintf(stdout, "CONVERTED HYPERLINKS:\n");
  cGraph->hyperlinks->dump(convertedGraphVertexDump);
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
  for (i = 0; i < cVertex->links->size(); i++) {
    if (i != 0) {
      fprintf(stdout, ",");
    }
    LMNtalLinkDump((LMNtalLink *)readStack(cVertex->links, i));
  }
  fprintf(stdout, "\n");
}

void convertedGraphVertexDumpCaster(void *cVertex) {
  convertedGraphVertexDump((ConvertedGraphVertex *)cVertex);
  return;
}

void add_proxy_mapping(ConvertedGraph *org, ConvertedGraph *copy,
                       std::map<int, int> *iso_m) {
  for (int i = 0; i < org->atoms->size(); i++) {
    if (org->atoms->at(i) != NULL) {
      ConvertedGraphVertex *org_atom =
          (ConvertedGraphVertex *)(org->atoms->at(i));
      if (org_atom->type == convertedInProxy) {
        LMNtalLink *hl_link = (LMNtalLink *)readStack(org_atom->links, 2);
        LMNtalLink *out_prox_link =
            (LMNtalLink *)(readStack(org_atom->links, 0));
        int out_prox_id = out_prox_link->data.ID;
        int copied_hl_id = iso_m->at(hl_link->data.ID);
        for (int j = 0; j < copy->atoms->size(); j++) {
          if (copy->atoms->at(j) != NULL) {
            ConvertedGraphVertex *copy_atom =
                (ConvertedGraphVertex *)(copy->atoms->at(j));
            if (copy_atom->type == convertedInProxy) {
              LMNtalLink *copy_hl_link =
                  (LMNtalLink *)readStack(copy_atom->links, 2);
              if (copy_hl_link->data.ID == copied_hl_id) {
                iso_m->insert(std::make_pair(org_atom->ID, copy_atom->ID));
                LMNtalLink *copied_out_prox_link =
                    (LMNtalLink *)readStack(copy_atom->links, 0);
                iso_m->insert(
                    std::make_pair(out_prox_id, copied_out_prox_link->data.ID));
              }
            }
          }
        }
      }
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
  if (org_atom->links->size() != copy_atom->links->size()) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    return false;
  }
  for (int i = 0; i < org_atom->links->size(); i++) {
    LMNtalLink *org_l = (LMNtalLink *)readStack(org_atom->links, i);
    LMNtalLink *copy_l = (LMNtalLink *)readStack(copy_atom->links, i);
    if (org_l->attr != copy_l->attr) {
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      return false;
    }
    if (org_l->attr == INTEGER_ATTR || org_l->attr == DOUBLE_ATTR ||
        org_l->attr == STRING_ATTR || org_l->attr == GLOBAL_ROOT_MEM_ATTR) {
      if (!isEqualLinks(org_l, copy_l)) {
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        return false;
      }
    } else if (org_l->attr < 128) {
      auto it = iso_m.find(org_l->data.ID);
      if (it != iso_m.end()) {
        if (it->second != copy_l->data.ID) {
          printf("%s:%d\n", __FUNCTION__, __LINE__);
          return false;
        }
      } else {
        if (org_l->data.ID != copy_l->data.ID) {
          printf("%s:%d\n", __FUNCTION__, __LINE__);
          return false;
        }
      }
    } else if (org_l->attr == HYPER_LINK_ATTR) {
      auto it = iso_m.find(org_l->data.ID);
      if (it != iso_m.end()) {
        if (it->second != copy_l->data.ID) {
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
  if (org_hl->links->size() != copy_hl->links->size()) {
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    return false;
  }
  for (int i = 0; i < org_hl->links->size(); i++) {
    LMNtalLink *org_l = (LMNtalLink *)readStack(org_hl->links, i);
    bool f = false;
    for (int j = 0; j < copy_hl->links->size(); j++) {
      LMNtalLink *copy_l = (LMNtalLink *)readStack(copy_hl->links, j);
      if (org_l->attr != copy_l->attr) {
        continue;
      }
      if (org_l->attr == INTEGER_ATTR || org_l->attr == DOUBLE_ATTR ||
          org_l->attr == STRING_ATTR || org_l->attr == GLOBAL_ROOT_MEM_ATTR) {
        if (!isEqualLinks(org_l, copy_l)) {
          continue;
        }
      } else if (org_l->attr < 128) {
        auto it = iso_m.find(org_l->data.ID);
        if (it != iso_m.end()) {
          if (it->second != copy_l->data.ID) {
            continue;
          }
        } else {
          if (org_l->data.ID != copy_l->data.ID) {
            continue;
          }
        }
      } else if (org_l->attr == HYPER_LINK_ATTR) {
        auto it = iso_m.find(org_l->data.ID);
        if (it != iso_m.end()) {
          if (it->second != copy_l->data.ID) {
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
  for (int i = 0; i < org->atoms->size(); i++) {
    if (org->atoms->at(i) != NULL) {
      ConvertedGraphVertex *org_atom =
          (ConvertedGraphVertex *)(org->atoms->at(i));
      ConvertedGraphVertex *copy_atom;
      auto it = iso_m.find(org_atom->ID);
      if (it != iso_m.end()) {
        copy_atom =
            (ConvertedGraphVertex *)copy->atoms->read( it->second);
      } else {
        copy_atom =
            (ConvertedGraphVertex *)copy->atoms->read( org_atom->ID);
      }
      // printf("org=%d copy=%d\n", org_atom->ID, copy_atom->ID);
      if (!check_corresponding_atoms(org_atom, copy_atom, iso_m)) {
        printf("%s:%d\n", __FUNCTION__, __LINE__);
        return false;
      }
    }
  }
  for (int i = 0; i < org->hyperlinks->size(); i++) {
    if (org->hyperlinks->at(i) != NULL) {
      ConvertedGraphVertex *org_hlatom =
          (ConvertedGraphVertex *)(org->hyperlinks->at(i));
      ConvertedGraphVertex *copy_hlatom;
      // printf("%s:%d\n", __FUNCTION__, __LINE__);
      auto it = iso_m.find(org_hlatom->ID);
      if (it != iso_m.end()) {
        copy_hlatom = (ConvertedGraphVertex *)copy->hyperlinks->read(
                                                               it->second);
      } else {
        copy_hlatom = (ConvertedGraphVertex *)copy->hyperlinks->read(
                                                               org_hlatom->ID);
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
