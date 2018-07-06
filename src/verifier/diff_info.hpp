#ifndef LMN_DIFFINFO_H
#define LMN_DIFFINFO_H

#include"json.hpp"
#include"util.hpp"
#include"collection.hpp"
#include"graphinfo.hpp"
#include"convertedgraph.hpp"

struct DiffInfo{
  Stack *deletedVertices;
  Stack *addedVertices;
  Stack *relinkedVertices;

  void diffInfoDump(){
    fprintf(stdout,"deletedVertices:\n\n");
    deletedVertices->stackDump(convertedGraphVertexDumpCaster);
    fprintf(stdout,"addedVertices:\n\n");
    addedVertices->stackDump(convertedGraphVertexDumpCaster);
    fprintf(stdout,"relinkedVertices:\n\n");
    relinkedVertices->stackDump(convertedGraphVertexDumpCaster);
    return;
  }

  DiffInfo(Graphinfo *before_gi, Graphinfo *after_gi) {
    DynamicArray *before_atoms=before_gi->cv->atoms;
    DynamicArray *before_hyperlinks=before_gi->cv->hyperlinks;
    DynamicArray *after_atoms=after_gi->cv->atoms;
    DynamicArray *after_hyperlinks=after_gi->cv->hyperlinks;
    
    deletedVertices=new Stack();
    addedVertices=new Stack();
    relinkedVertices=new Stack();

    int gap_of_grootmem_id = after_gi->globalRootMemID - before_gi->globalRootMemID;
    int begin = std::min(0, -gap_of_grootmem_id);
    int end = std::max(std::max(before_atoms->cap, after_hyperlinks->cap - gap_of_grootmem_id), std::max(after_atoms->cap, after_hyperlinks->cap - gap_of_grootmem_id));
    for(int i=begin; i<end; i++) {
      ConvertedGraphVertex *before_hl = (ConvertedGraphVertex *)readDynamicArray(before_hyperlinks, i);
      ConvertedGraphVertex *after_hl = (ConvertedGraphVertex *)readDynamicArray(after_hyperlinks, i);
      if(before_hl != NULL && after_hl == NULL) {
	pushConvertedVertexIntoDiffInfoStackWithoutOverlap(deletedVertices, before_hl);
      } else if(before_hl == NULL && after_hl != NULL) {
	pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices, after_hl);
      }
    }


    for(int i=begin; i<end; i++) {
      ConvertedGraphVertex *before_atom = (ConvertedGraphVertex *)readDynamicArray(before_atoms, i);
      ConvertedGraphVertex *after_atom = (ConvertedGraphVertex *)readDynamicArray(after_atoms, i);
      if(before_atom != NULL && after_atom == NULL) {
	pushConvertedVertexIntoDiffInfoStackWithoutOverlap(deletedVertices, before_atom);
	checkRelink(before_atom, after_atom, after_hyperlinks, relinkedVertices);
      } else if(before_atom == NULL && after_atom !=NULL) {
	pushConvertedVertexIntoDiffInfoStackWithoutOverlap(addedVertices, after_atom);
	checkRelink(before_atom, after_atom, after_hyperlinks, relinkedVertices);
      } else if(before_atom != NULL && after_atom != NULL) {
	checkRelink(before_atom, after_atom, after_hyperlinks, relinkedVertices);
      }
    }
  }
};

void freeDiff();
Bool succRead(int *succPtr);
int scanState();
Graphinfo *makeEmptyGrpahInfo();
DiffInfo *compareGraphInfo(Graphinfo *beforeGraphInfo,Graphinfo *afterGraphInfo,int gapOfGlobalRootMemID);
void freeGraphInfo(Graphinfo *graphInfo);
void freeGraphInfoCaster(void *graphInfo);
void freeDiffInfo(DiffInfo *diffInfo);
void diffInfoDump(DiffInfo *diffInfo);
void freeDiffCaster(void *d);
void diffDumpCaster(void *d);

#endif
