#include"convertedgraph.hpp"
#include"collection.hpp"
Bool isEqualLinks(LMNtalLink *linkA,LMNtalLink *linkB){
  if(linkA->attr != linkB->attr){
    return FALSE;
  }

  switch(linkA->attr){
    case INTEGER_ATTR:
      return linkA->data.integer == linkB->data.integer;
      break;
    case DOUBLE_ATTR:
      return linkA->data.dbl == linkB->data.dbl;
      break;
    case STRING_ATTR:
      if(strcmp(linkA->data.string,linkB->data.string) == 0){
        return TRUE;
      }else{
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
      if(linkA->attr < 128){
        return linkA->data.ID == linkB->data.ID;
      }else{
        CHECKER("unexpected attr\n");
        exit(EXIT_FAILURE);
        return FALSE;
      }
      break;
  }

}


Bool isHyperLink(LMNtalLink *link){
  return link->attr == HYPER_LINK_ATTR;
}

template <typename S>
void checkRelink(ConvertedGraphVertex *beforeCAtom,ConvertedGraphVertex *afterCAtom,DynamicArray *afterConvertedHyperLinks, S *relinkedVertices){
  if(beforeCAtom != NULL && afterCAtom != NULL){
    int i;
    assert(beforeCAtom->links->size()==afterCAtom->links->size());
    for(i=0;i<beforeCAtom->links->size();i++){
      LMNtalLink *beforeLink = (LMNtalLink *)readStack(beforeCAtom->links,i);
      LMNtalLink *afterLink = (LMNtalLink *)readStack(afterCAtom->links,i);

      if(!isEqualLinks(beforeLink,afterLink)){
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(relinkedVertices,afterCAtom);
        if(isHyperLink(beforeLink)){
          pushConvertedVertexIntoDiffInfoStackWithoutOverlap(relinkedVertices,(ConvertedGraphVertex *)readDynamicArray(afterConvertedHyperLinks,beforeLink->data.ID));
        }
        if(isHyperLink(afterLink)){
          pushConvertedVertexIntoDiffInfoStackWithoutOverlap(relinkedVertices,(ConvertedGraphVertex *)readDynamicArray(afterConvertedHyperLinks,afterLink->data.ID));
        }
      }
    }
  }else if(beforeCAtom != NULL && afterCAtom == NULL){
    int i;
    for(i=0;i<beforeCAtom->links->size();i++){
      LMNtalLink *beforeLink = (LMNtalLink *)readStack(beforeCAtom->links,i);

      if(isHyperLink(beforeLink)){
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(relinkedVertices,(ConvertedGraphVertex *)readDynamicArray(afterConvertedHyperLinks,beforeLink->data.ID));
      }
    }
  }else if(beforeCAtom == NULL && afterCAtom != NULL){
    int i;
    for(i=0;i<afterCAtom->links->size();i++){
      LMNtalLink *afterLink = (LMNtalLink *)readStack(afterCAtom->links,i);

      if(isHyperLink(afterLink)){
        pushConvertedVertexIntoDiffInfoStackWithoutOverlap(relinkedVertices,(ConvertedGraphVertex *)readDynamicArray(afterConvertedHyperLinks,afterLink->data.ID));
      }
    }
  }else{
  }
  return;
}


void convertedGraphDump(ConvertedGraph *cGraph){
  fprintf(stdout,"CONVERTED ATOMS:\n");
  dynamicArrayDump(cGraph->atoms,convertedGraphVertexDumpCaster);
  fprintf(stdout,"CONVERTED HYPERLINKS:\n");
  dynamicArrayDump(cGraph->hyperlinks,convertedGraphVertexDumpCaster);
  return;
}

void LMNtalLinkDump(LMNtalLink *link){
  fprintf(stdout,"<%d,%d>",link->attr,link->data.ID);
  return;
}

void convertedGraphVertexDump(ConvertedGraphVertex *cVertex){
  int i;

  if(cVertex->type == convertedAtom){
    fprintf(stdout,"type:ATOM\n");
  }else if(cVertex->type == convertedHyperLink){
    fprintf(stdout,"type:HYPERLINK\n");
  }

  fprintf(stdout,"ID:%d\n",cVertex->ID);
  fprintf(stdout,"name:%s\n",cVertex->name);

  fprintf(stdout,"links:");
  for(i=0;i<cVertex->links->size();i++){
    if(i!=0){
      fprintf(stdout,",");
    }
    LMNtalLinkDump((LMNtalLink *)readStack(cVertex->links,i));
  }
  fprintf(stdout,"\n");
}

void convertedGraphVertexDumpCaster(void *cVertex){
  convertedGraphVertexDump((ConvertedGraphVertex *)cVertex);
  return;
}
