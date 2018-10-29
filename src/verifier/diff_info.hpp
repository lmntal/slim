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
    fprintf(stdout, "deletedVertices:\n\n");
    dump(*deletedVertices, convertedGraphVertexDump);
    fprintf(stdout, "addedVertices:\n\n");
    dump(*addedVertices, convertedGraphVertexDump);
    fprintf(stdout, "relinkedVertices:\n\n");
    dump(*relinkedVertices, convertedGraphVertexDump);
    return;
  }

  DiffInfo(Graphinfo *init_gi) {
    auto &before_atoms = init_gi->cv->atoms;
    auto &before_hyperlinks = init_gi->cv->hyperlinks;
    deletedVertices = new std::vector<ConvertedGraphVertex *>();
    addedVertices = new std::vector<ConvertedGraphVertex *>();
    relinkedVertices = new std::vector<ConvertedGraphVertex *>();
    for(auto i = before_atoms.begin(); i!=before_atoms.end(); ++i) {
      pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices,
							 i->second);
    }
    for(auto i = before_hyperlinks.begin(); i!=before_hyperlinks.end(); ++i) {
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

    int gap_of_grootmem_id =
        after_gi->globalRootMemID - before_gi->globalRootMemID;
    printf("after_gi->globalRootMemID=%d\n", after_gi->globalRootMemID);
    printf("before_gi->globalRootMemID=%d\n", before_gi->globalRootMemID);
    int begin = std::min(0, -gap_of_grootmem_id);
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    int end = std::max(std::max(before_atoms.rbegin()->first,
                                after_hyperlinks.rbegin()->first - gap_of_grootmem_id),
                       std::max(after_atoms.rbegin()->first,
                                after_hyperlinks.rbegin()->first - gap_of_grootmem_id));
    printf("%s:%d\n", __FUNCTION__, __LINE__);
    for (int i = begin; i < end; i++) {
      const auto &before_hl = before_hyperlinks.find(i);
      const auto &after_hl = after_hyperlinks.find(i);
      if (before_hl != before_hyperlinks.end() && after_hl == after_hyperlinks.end()) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(deletedVertices,
                                                           before_hl->second);
      } else if (before_hl == before_hyperlinks.end() && after_hl != after_hyperlinks.end()) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices,
                                                           after_hl->second);
      }
    }

    for (int i = begin; i < end; i++) {
      const auto &before_atom = before_atoms.find(i);
      const auto &after_atom = after_atoms.find(i);
      if (before_atom != before_atoms.end() && after_atom == after_atoms.end()) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(deletedVertices,
                                                           before_atom->second);
        checkRelink(before_atom->second, after_atom->second, after_hyperlinks,
                    relinkedVertices);
      } else if (before_atom == before_atoms.end() && after_atom != after_atoms.end()) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices,
                                                           after_atom->second);
        checkRelink(before_atom->second, after_atom->second, after_hyperlinks,
                    relinkedVertices);
      } else if (before_atom != before_atoms.end() && after_atom != after_atoms.end()) {
        checkRelink(before_atom->second, after_atom->second, after_hyperlinks,
                    relinkedVertices);
      }
    }
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
