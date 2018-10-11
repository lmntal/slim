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

bool check_corresponding_atoms(ConvertedGraphVertex* org_atom, ConvertedGraphVertex* copy_atom, std::map<int,int> iso_m) {
  if(copy_atom==NULL) {printf("%s:%d\n", __FUNCTION__, __LINE__); return false;}
  if(org_atom->links->size() != copy_atom->links->size()){printf("%s:%d\n", __FUNCTION__, __LINE__); return false;}
  for(int i=0; i<org_atom->links->size(); i++) {
    LMNtalLink* org_l = (LMNtalLink*)readStack(org_atom->links, i);
    LMNtalLink* copy_l = (LMNtalLink*)readStack(copy_atom->links, i);
    if(org_l->attr != copy_l->attr){printf("%s:%d\n", __FUNCTION__, __LINE__); return false;}
    if(org_l->attr == INTEGER_ATTR || org_l->attr == DOUBLE_ATTR ||
       org_l->attr == STRING_ATTR || org_l->attr == GLOBAL_ROOT_MEM_ATTR) {
      if(!isEqualLinks(org_l, copy_l)){printf("%s:%d\n", __FUNCTION__, __LINE__); return false;}
    }else if(org_l->attr < 128) {
      auto it = iso_m.find(org_l->data.ID);
      if(it!=iso_m.end()) {
	if(it->second != copy_l->data.ID){printf("%s:%d\n", __FUNCTION__, __LINE__); return false;}
      } else {
	if(org_l->data.ID != copy_l->data.ID){printf("%s:%d\n", __FUNCTION__, __LINE__); return false;}
      }
    }else if(org_l->attr == HYPER_LINK_ATTR) {
      printf("**HYPER**\n");
      auto it = iso_m.find(org_l->data.ID);
      if(it!=iso_m.end()) {
	printf("%s:%d\n", __FUNCTION__, __LINE__);
	if(it->second != copy_l->data.ID){printf("%s:%d\n", __FUNCTION__, __LINE__); return false;}
      }
    }
  }
  return true;
}

bool check_iso_morphism(ConvertedGraph* org, ConvertedGraph* copy, std::map<int, int> iso_m) {
  printf("%s:%d\n", __FUNCTION__, __LINE__);
  printf("===iso_m===\n");
  for(auto it=iso_m.begin(); it!=iso_m.end(); it++) {
    printf("%d %d\n", it->first, it->second);
  }

  for(int i=0; i<org->atoms->size(); i++) {
    if(org->atoms->at(i)!=NULL) {
      ConvertedGraphVertex * org_atom = (ConvertedGraphVertex *)(org->atoms->at(i));
      ConvertedGraphVertex* copy_atom;
      auto it = iso_m.find(org_atom->ID);
      if(it!=iso_m.end()) {
	copy_atom = (ConvertedGraphVertex *)readDynamicArray(copy->atoms, it->second);
      } else {
	copy_atom = (ConvertedGraphVertex *)readDynamicArray(copy->atoms, org_atom->ID);
      }
      if(!check_corresponding_atoms(org_atom, copy_atom, iso_m)){printf("%s:%d\n", __FUNCTION__, __LINE__);return false;}
    }
  }
  for(int i=0; i<org->hyperlinks->size(); i++) {
    if(org->hyperlinks->at(i)!=NULL) {
      ConvertedGraphVertex* org_hlatom = (ConvertedGraphVertex*)(org->hyperlinks->at(i));
      ConvertedGraphVertex* copy_hlatom;
      auto it = iso_m.find(org_hlatom->ID);
      if(it!=iso_m.end()) {
	copy_hlatom = (ConvertedGraphVertex*)readDynamicArray(copy->hyperlinks, it->second);
      } else {
	copy_hlatom = (ConvertedGraphVertex*)readDynamicArray(copy->hyperlinks, org_hlatom->ID);
      }
      if(!check_corresponding_atoms(org_hlatom, copy_hlatom, iso_m)){printf("%s:%d\n", __FUNCTION__, __LINE__);return false;}
    }
  }
  return true;
}
