#ifndef LMN_DIFFINFO_H
#define LMN_DIFFINFO_H

#include "collection.hpp"
#include "convertedgraph.hpp"
#include "graphinfo.hpp"
#include "json.hpp"
#include "util.hpp"

#include <iostream>

struct DiffInfo {
  std::vector<ConvertedGraphVertex *> *deletedVertices;
  std::vector<ConvertedGraphVertex *> *addedVertices;
  std::vector<ConvertedGraphVertex *> *relinkedVertices;

  void diffInfoDump() {
    printf("****DIFF INFO****\n");
    fprintf(stdout, "+++deletedVertices:\n\n");
    for (int i = 0; i < deletedVertices->size(); i++)
      std::cout << i << ":" << *deletedVertices->at(i);
    fprintf(stdout, "+++addedVertices:\n\n");
    for (int i = 0; i < addedVertices->size(); i++)
      std::cout << i << ":" << *addedVertices->at(i);
    fprintf(stdout, "+++relinkedVertices:\n\n");
    for (int i = 0; i < relinkedVertices->size(); i++)
      std::cout << i << ":" << *relinkedVertices->at(i);
    printf("*****************\n");
    return;
  }

  void change_ref_before_graph(std::map<int, int> iso_m, Graphinfo *before_gi,
                               Graphinfo *org_gi) {
    std::vector<ConvertedGraphVertex *> *new_deletedVertices;
    new_deletedVertices = new std::vector<ConvertedGraphVertex *>();
#ifdef DIFFISO_DEB
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    for (auto &v : iso_m) {
      std::cout << v.first << " " << v.second << std::endl;
    }
    std::cout << *org_gi->cv << std::endl;
#endif
    for (auto i = deletedVertices->begin(); i != deletedVertices->end(); ++i) {
#ifdef DIFFISO_DEB
      std::cout << "deleteVertexID: " << (*i)->ID << std::endl;
      // std::cout << "MAPED ID: " << iso_m[(*i)->ID]<< std::endl;

      // const auto &org_id = iso_m.find((*i)->ID);
      // printf("%s:%d\n", __FUNCTION__, __LINE__);
      // if (org_id == iso_m.end()) {
      // 	printf("%s:%d\n", __FUNCTION__, __LINE__);
      //   continue;
      // }

      printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
      if ((*i)->type == convertedAtom) {

        // std::cout << "org_id->first: " << org_id->first << std::endl;
        // std::cout << "org_id->second: " << org_id->second << std::endl;
        const auto &org = org_gi->cv->atoms.find(iso_m[(*i)->ID]);

        if (org != org_gi->cv->atoms.end()) {

          new_deletedVertices->push_back(org->second);
        }

      } else if ((*i)->type == convertedHyperLink) {
        const auto &org_hl = org_gi->cv->hyperlinks.find(iso_m[(*i)->ID]);
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
        deletedVertices->push_back(i->second);
        // pushConvertedVertexIntoDiffInfoStackWithoutOverlap(deletedVertices,
        //                                                   i->second);
      }
    }
    for (auto i = after_hyperlinks.begin(); i != after_hyperlinks.end(); ++i) {
      const auto &before_hl = before_hyperlinks.find(i->first);
      if (before_hl == before_hyperlinks.end()) {
        addedVertices->push_back(i->second);
        // pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices,
        //                                                   i->second);
      }
    }
    for (auto i = before_atoms.begin(); i != before_atoms.end(); ++i) {
#ifdef DIFFISO_DEB
      printf("%s:%d\n", __FUNCTION__, __LINE__);
      std::cout << *i->second << std::endl;
#endif
      const auto &after_atom = after_atoms.find(i->first);
      if (after_atom == after_atoms.end()) {
#ifdef DIFFISO_DEB
        printf("%s:%d\n", __FUNCTION__, __LINE__);
#endif
        deletedVertices->push_back(i->second);
        // pushConvertedVertexIntoDiffInfoStackWithoutOverlap(deletedVertices,
        //                                                   i->second);
        for (auto &beforeLink : i->second->links) {
          if (!beforeLink.is_hyper())
            continue;
          // relinkedVertices->push_back(after_hyperlinks[beforeLink.data.ID]);
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
          // relinkedVertices->push_back(after_atom->second);
          pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
              relinkedVertices, after_atom->second);
          if (beforeLink.is_hyper()) {
            // relinkedVertices->push_back(after_hyperlinks[beforeLink.data.ID]);
            pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
                relinkedVertices, after_hyperlinks[beforeLink.data.ID]);
          }
          if (afterLink.is_hyper()) {
            relinkedVertices->push_back(after_hyperlinks[afterLink.data.ID]);
            pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
                relinkedVertices, after_hyperlinks[afterLink.data.ID]);
          }
        }
      }
    }
    for (auto i = after_atoms.begin(); i != after_atoms.end(); ++i) {
      const auto &before_atom = before_atoms.find(i->first);
      if (before_atom == before_atoms.end()) {
        addedVertices->push_back(i->second);
        // pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices,
        //                                                   i->second);
        for (auto &afterLink : i->second->links) {
          if (!afterLink.is_hyper())
            continue;
          // relinkedVertices->push_back(after_hyperlinks[afterLink.data.ID]);
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

          // relinkedVertices->push_back(i->second);
          pushConvertedVertexIntoDiffInfoStackWithoutOverlap(relinkedVertices,
                                                             i->second);
          if (beforeLink.is_hyper()) {
            // relinkedVertices->push_back(after_hyperlinks[beforeLink.data.ID]);
            pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
                relinkedVertices, after_hyperlinks[beforeLink.data.ID]);
          }
          if (afterLink.is_hyper()) {
            // relinkedVertices->push_back(after_hyperlinks[afterLink.data.ID]);
            pushConvertedVertexIntoDiffInfoStackWithoutOverlap(
                relinkedVertices, after_hyperlinks[afterLink.data.ID]);
          }
        }
      }
    }
  }
  DiffInfo() {
    deletedVertices = new std::vector<ConvertedGraphVertex *>();
    addedVertices = new std::vector<ConvertedGraphVertex *>();
    relinkedVertices = new std::vector<ConvertedGraphVertex *>();
  }
  ~DiffInfo() {
    delete deletedVertices;
    delete addedVertices;
    delete relinkedVertices;
  };
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
