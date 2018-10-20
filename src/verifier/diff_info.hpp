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
    int begin = std::min(0, -gap_of_grootmem_id);
    int end = std::max(std::max(before_atoms.rbegin()->first,
                                after_hyperlinks.rbegin()->first - gap_of_grootmem_id),
                       std::max(after_atoms.rbegin()->first,
                                after_hyperlinks.rbegin()->first - gap_of_grootmem_id));

    for (int i = begin; i < end; i++) {
      auto &before_hl = before_hyperlinks[i];
      auto &after_hl = after_hyperlinks[i];
      if (before_hl != NULL && after_hl == NULL) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(deletedVertices,
                                                           before_hl);
      } else if (before_hl == NULL && after_hl != NULL) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices,
                                                           after_hl);
      }
    }

    for (int i = begin; i < end; i++) {
      ConvertedGraphVertex *before_atom =
          (ConvertedGraphVertex *)before_atoms[i];
      ConvertedGraphVertex *after_atom = (ConvertedGraphVertex *)after_atoms[i];
      if (before_atom != NULL && after_atom == NULL) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(deletedVertices,
                                                           before_atom);
        checkRelink(before_atom, after_atom, after_hyperlinks,
                    relinkedVertices);
      } else if (before_atom == NULL && after_atom != NULL) {
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices,
                                                           after_atom);
        checkRelink(before_atom, after_atom, after_hyperlinks,
                    relinkedVertices);
      } else if (before_atom != NULL && after_atom != NULL) {
        checkRelink(before_atom, after_atom, after_hyperlinks,
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
