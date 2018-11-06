#ifndef LMN_DIFFINFO_H
#define LMN_DIFFINFO_H

#include "collection.hpp"
#include "convertedgraph.hpp"
#include "graphinfo.hpp"
#include "json.hpp"
#include "util.hpp"

struct DiffInfo {
  std::vector<ConvertedGraphVertex *> *deletedVertices;
  std::vector<ConvertedGraphVertex *> *addedVertices;
  std::vector<ConvertedGraphVertex *> *relinkedVertices;

  void diffInfoDump() {
    printf("****DIFF INFO****\n");
    fprintf(stdout, "+++deletedVertices:\n\n");
    dump(*deletedVertices, convertedGraphVertexDump);
    fprintf(stdout, "+++addedVertices:\n\n");
    dump(*addedVertices, convertedGraphVertexDump);
    fprintf(stdout, "+++relinkedVertices:\n\n");
    dump(*relinkedVertices, convertedGraphVertexDump);
    printf("*****************\n");
    return;
  }

  void change_ref_before_graph(std::map<int, int> iso_m, Graphinfo *before_gi,
                               Graphinfo *org_gi) {
    std::vector<ConvertedGraphVertex *> *new_deletedVertices;
    new_deletedVertices = new std::vector<ConvertedGraphVertex *>();
    for (auto i = deletedVertices->begin(); i != deletedVertices->end(); ++i) {
      const auto &org_id = iso_m.find((*i)->ID);
      if (org_id == iso_m.end())
        continue;

      if ((*i)->type == convertedAtom) {
        const auto &org = org_gi->cv->atoms.find(org_id->second);
        if (org != org_gi->cv->atoms.end()) {
          new_deletedVertices->push_back(org->second);
        }
      } else if ((*i)->type == convertedHyperLink) {
        const auto &org_hl = org_gi->cv->hyperlinks.find(org_id->second);
        if (org_hl != org_gi->cv->hyperlinks.end()) {
          new_deletedVertices->push_back(org_hl->second);
        }
      }
    }
    delete deletedVertices;
    deletedVertices = new_deletedVertices;
  }

  DiffInfo(Graphinfo *init_gi) {
    auto &before_atoms = init_gi->cv->atoms;
    auto &before_hyperlinks = init_gi->cv->hyperlinks;
    deletedVertices = new std::vector<ConvertedGraphVertex *>();
    addedVertices = new std::vector<ConvertedGraphVertex *>();
    relinkedVertices = new std::vector<ConvertedGraphVertex *>();
    for (auto i = before_atoms.begin(); i != before_atoms.end(); ++i) {
      pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices,
                                                         i->second);
    }
    for (auto i = before_hyperlinks.begin(); i != before_hyperlinks.end();
         ++i) {
      pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices,
                                                         i->second);
    }
  }

  DiffInfo(Graphinfo *before_gi, Graphinfo *after_gi) {
    auto &before_atoms = before_gi->cv->atoms;
    auto &before_hyperlinks = before_gi->cv->hyperlinks;
    auto &after_atoms = after_gi->cv->atoms;
    auto &after_hyperlinks = after_gi->cv->hyperlinks;
    deletedVertices = new std::vector<ConvertedGraphVertex *>();
    addedVertices = new std::vector<ConvertedGraphVertex *>();
    relinkedVertices = new std::vector<ConvertedGraphVertex *>();

    for (auto i = before_hyperlinks.begin(); i != before_hyperlinks.end();
         ++i) {
      const auto &after_hl = after_hyperlinks.find(i->first);
      if (after_hl == after_hyperlinks.end()) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(deletedVertices,
                                                           i->second);
      }
    }
    for (auto i = after_hyperlinks.begin(); i != after_hyperlinks.end(); ++i) {
      const auto &before_hl = before_hyperlinks.find(i->first);
      if (before_hl == before_hyperlinks.end()) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices,
                                                           i->second);
      }
    }
    for (auto i = before_atoms.begin(); i != before_atoms.end(); ++i) {
      const auto &after_atom = after_atoms.find(i->first);
      if (after_atom == after_atoms.end()) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(deletedVertices,
                                                           i->second);
        for (auto &beforeLink : i->second->links) {
          if (!beforeLink.is_hyper())
            continue;
          pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
              relinkedVertices, after_hyperlinks[beforeLink.data.ID]);
        }
      } else {
        assert(i->second->links.size() == after_atom->second->links.size());
        for (auto j = 0; j < i->second->links.size(); j++) {
          const auto &beforeLink = i->second->links[j];
          const auto &afterLink = after_atom->second->links[j];

          if (beforeLink == afterLink)
            continue;

          pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
              relinkedVertices, after_atom->second);
          if (beforeLink.is_hyper()) {
            pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
                relinkedVertices, after_hyperlinks[beforeLink.data.ID]);
          }
          if (afterLink.is_hyper()) {
            pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
                relinkedVertices, after_hyperlinks[afterLink.data.ID]);
          }
        }
      }
    }
    for (auto i = after_atoms.begin(); i != after_atoms.end(); ++i) {
      const auto &before_atom = before_atoms.find(i->first);
      if (before_atom == before_atoms.end()) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices,
                                                           i->second);
        for (auto &afterLink : i->second->links) {
          if (!afterLink.is_hyper())
            continue;
          pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
              relinkedVertices, after_hyperlinks[afterLink.data.ID]);
        }
      } else {
        assert(i->second->links.size() == before_atom->second->links.size());
        for (auto j = 0; j < before_atom->second->links.size(); j++) {
          const auto &beforeLink = before_atom->second->links[j];
          const auto &afterLink = i->second->links[j];

          if (beforeLink == afterLink)
            continue;

          pushConvertedVertexIntoDiffInfoStackWithoutOverlap(relinkedVertices,
                                                             i->second);
          if (beforeLink.is_hyper()) {
            pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
                relinkedVertices, after_hyperlinks[beforeLink.data.ID]);
          }
          if (afterLink.is_hyper()) {
            pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
                relinkedVertices, after_hyperlinks[afterLink.data.ID]);
          }
        }
      }
    }

    // for (int i = begin; i < end; i++) {
    //   const auto &before_hl = before_hyperlinks.find(i);
    //   const auto &after_hl = after_hyperlinks.find(i);
    //   if (before_hl != before_hyperlinks.end() && after_hl ==
    //   after_hyperlinks.end()) {
    //     pushConvertedVertexIntoDiffInfoStackWithoutOverlap(deletedVertices,
    //                                                        before_hl->second);
    //   } else if (before_hl == before_hyperlinks.end() && after_hl !=
    //   after_hyperlinks.end()) {
    //     pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices,
    //                                                        after_hl->second);
    //   }
    // }

    // for (int i = begin; i < end; i++) {
    //   const auto &before_atom = before_atoms.find(i);
    //   const auto &after_atom = after_atoms.find(i);
    //   if (before_atom != before_atoms.end() && after_atom ==
    //   after_atoms.end()) {
    //     pushConvertedVertexIntoDiffInfoStackWithoutOverlap(deletedVertices,
    //                                                        before_atom->second);
    //     checkRelink(before_atom->second, after_atom->second,
    //     after_hyperlinks,
    //                 relinkedVertices);
    //   } else if (before_atom == before_atoms.end() && after_atom !=
    //   after_atoms.end()) {
    //     pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices,
    //                                                        after_atom->second);
    //     checkRelink(before_atom->second, after_atom->second,
    //     after_hyperlinks,
    //                 relinkedVertices);
    //   } else if (before_atom != before_atoms.end() && after_atom !=
    //   after_atoms.end()) {
    //     checkRelink(before_atom->second, after_atom->second,
    //     after_hyperlinks,
    //                 relinkedVertices);
    //   }
    // }
  }
};

void freeDiff();
Bool succRead(int *succPtr);
int scanState();
Graphinfo *makeEmptyGrpahInfo();
DiffInfo *compareGraphInfo(Graphinfo *beforeGraphInfo,
                           Graphinfo *afterGraphInfo, int gapOfGlobalRootMemID);
void freeGraphInfo(Graphinfo *graphInfo);
void freeGraphInfoCaster(void *graphInfo);
void freeDiffInfo(DiffInfo *diffInfo);
void diffInfoDump(DiffInfo *diffInfo);
void freeDiffCaster(void *d);
void diffDumpCaster(void *d);

#endif
