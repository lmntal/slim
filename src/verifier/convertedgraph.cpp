#include"convertedgraph.hpp"
#include"collection.hpp"
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
  for(i=0;i<cVertex->links->numStack();i++){
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
