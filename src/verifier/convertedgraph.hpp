/*
 * convertedgraph.hpp
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 *                                         <lmntal@ueda.info.waseda.ac.jp>
 *   All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions are
 *   met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *
 *    3. Neither the name of the Ueda Laboratory LMNtal Group nor the
 *       names of its contributors may be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id$
 */
#ifndef LMN_CONVERTEDGRAPH_HPP
#define LMN_CONVERTEDGRAPH_HPP
#include "collection.hpp"
#include "json.hpp"
#define NAME_LENGTH 256
#define INTEGER_ATTR 128
#define DOUBLE_ATTR 129
#define STRING_ATTR 131
#define HYPER_LINK_ATTR 138
#define GLOBAL_ROOT_MEM_ATTR 255

struct InheritedVertex;


union LMNtalData {
  int integer;
  double dbl;
  char string[NAME_LENGTH];
  int ID;
};

struct LMNtalLink {
  int attr;
  LMNtalData data;
  LMNtalLink(int a, LMNtalData d) {
    attr = a;
    data = d;
  }
  ~LMNtalLink(){
  }
};

typedef enum {
  convertedNone,
  convertedAtom,
  convertedHyperLink,
  convertedNull
} ConvertedGraphVertexType;

struct ConvertedGraphVertex{
  ConvertedGraphVertexType type;
  Stack *links;
  int ID;
  char name[NAME_LENGTH];
  Bool isPushedIntoDiffInfoStack;
  Bool isVisitedInBFS;
  InheritedVertex *correspondingVertexInTrie;

  ConvertedGraphVertex(int id, char *Name) {
    type = convertedAtom;
    links = new Stack();
    ID = id;
    strcpy(name,Name);
    isPushedIntoDiffInfoStack = FALSE;
    isVisitedInBFS = FALSE;
    correspondingVertexInTrie = NULL;
  }
};


struct ConvertedGraph {
  DynamicArray *atoms;
  DynamicArray *hyperlinks;

  int LMNtalID(json_value *jVal){
    return jVal->u.object.values[0].value->u.integer;
  }

  void LMNtalNameCopy(json_value *jVal,char *dstString){
    strcpy(dstString,jVal->u.object.values[1].value->u.string.ptr);
  }

  LMNtalLink *copyLink(LMNtalLink *link){
    return new LMNtalLink(link->attr,link->data);
  }


  void convertGraphLink(json_value *jVal,DynamicArray *atoms, DynamicArray* hyperlinks, ConvertedGraphVertex *cVertex,int linkNumber){
    int attr = jVal->u.object.values[0].value->u.integer;
    LMNtalData data;

    switch(attr){
    case INTEGER_ATTR:
      data.integer = jVal->u.object.values[1].value->u.integer;
      break;
    case DOUBLE_ATTR:
      data.dbl = jVal->u.object.values[1].value->u.dbl;
      break;
    case STRING_ATTR:
      strcpy(data.string,jVal->u.object.values[1].value->u.string.ptr);
      break;
    case HYPER_LINK_ATTR:
      data.ID = jVal->u.object.values[1].value->u.integer;
      break;
    case GLOBAL_ROOT_MEM_ATTR:
      data.ID = jVal->u.object.values[1].value->u.integer;
      break;
    default:
      if(attr < 128){
        data.ID = jVal->u.object.values[1].value->u.integer;
      }else{
        CHECKER("unexpected attr\n");
        exit(EXIT_FAILURE);
      }
      break;
    }

    LMNtalLink *link = new LMNtalLink(attr,data);
    pushStack(cVertex->links,link);

    if(link->attr == HYPER_LINK_ATTR){
      if(readDynamicArray(hyperlinks,link->data.ID) == NULL){
	ConvertedGraphVertex *cv_hyper = new ConvertedGraphVertex(link->data.ID,"");
	writeDynamicArray(hyperlinks,link->data.ID,cv_hyper);
      }

      LMNtalData data;
      data.integer = cVertex->ID;
      LMNtalLink *linkFromHyperLink = new LMNtalLink(linkNumber,data);
      pushStack(((ConvertedGraphVertex *)readDynamicArray(hyperlinks,link->data.ID))->links,copyLink(linkFromHyperLink));
    }
  }

  void convertGraphLinksArray(json_value *jVal,DynamicArray *atoms, DynamicArray* hyperlinks,ConvertedGraphVertex *cVertex){
    for(int i=0;i<jVal->u.array.length;i++){
      convertGraphLink(jVal->u.array.values[i],atoms, hyperlinks ,cVertex,i);
    }
  }


  void convertGraphAtom(json_value *jVal, DynamicArray *atoms, DynamicArray *hyperlinks, LMNtalLink *linkToParentMem) {
      char tmpName1[NAME_LENGTH];
      strcpy(tmpName1,"ATOM_");
      char tmpName2[NAME_LENGTH];
      LMNtalNameCopy(jVal,tmpName2);
      strcat(tmpName1,tmpName2);

      ConvertedGraphVertex *cVertex = new ConvertedGraphVertex(LMNtalID(jVal),tmpName1);
      writeDynamicArray(atoms,LMNtalID(jVal),cVertex);
      convertGraphLinksArray(jVal->u.object.values[2].value,atoms, hyperlinks ,cVertex);
      pushStack(cVertex->links,copyLink(linkToParentMem));
      if(linkToParentMem->attr != GLOBAL_ROOT_MEM_ATTR){
	LMNtalData data;
	data.integer = LMNtalID(jVal);
	LMNtalLink * hl = new LMNtalLink(cVertex->links->num-1, data);
	pushStack(((ConvertedGraphVertex *)readDynamicArray(hyperlinks,linkToParentMem->data.ID))->links, hl);
      }
      
  }

  void convertGraphAtomsArray(json_value *jVal, DynamicArray *atoms,
                                DynamicArray *hyperlinks,
                                LMNtalLink *linkToParentMem) {
    for (int i = 0; i < jVal->u.array.length; i++) {
      convertGraphAtom(jVal->u.array.values[i], atoms, hyperlinks,
                       linkToParentMem);
    }
  };

  void convertGraphMem(json_value *jVal,DynamicArray *atoms, DynamicArray *hyperlinks,LMNtalLink *linkToParentMem){
    char tmpName1[NAME_LENGTH];
    strcpy(tmpName1,"MEM_");
    char tmpName2[NAME_LENGTH];
    LMNtalNameCopy(jVal,tmpName2);
    strcat(tmpName1,tmpName2);

    ConvertedGraphVertex *cVertex = new ConvertedGraphVertex(LMNtalID(jVal),tmpName1);
    ConvertedGraphVertex *cHyperlink = new ConvertedGraphVertex(LMNtalID(jVal),"");

    writeDynamicArray(atoms,LMNtalID(jVal),cVertex);
    writeDynamicArray(hyperlinks,LMNtalID(jVal),cHyperlink);

    LMNtalData data;
    data.integer = LMNtalID(jVal);
    LMNtalLink *linkToThisMem = new LMNtalLink(HYPER_LINK_ATTR,data);

    pushStack(cVertex->links,copyLink(linkToThisMem));
    pushStack(cVertex->links,copyLink(linkToParentMem));
    data.integer = LMNtalID(jVal);
    LMNtalLink *hl = new LMNtalLink(0, data);
    pushStack(cHyperlink->links,hl);
    if(linkToParentMem->attr != GLOBAL_ROOT_MEM_ATTR){
      data.integer = LMNtalID(jVal);
      LMNtalLink *newlink = new LMNtalLink(1, data);
      pushStack(((ConvertedGraphVertex *)readDynamicArray(hyperlinks,linkToParentMem->data.ID))->links,newlink);
    }

    convertGraphAtomsArray(jVal->u.object.values[2].value,atoms,hyperlinks,linkToThisMem);
    convertGraphMemsArray(jVal->u.object.values[3].value,atoms,hyperlinks,linkToThisMem);
  }


  void convertGraphMemsArray(json_value *jVal,DynamicArray *atoms, DynamicArray *hyperlinks, LMNtalLink *linkToParentMem){
    for(int i=0;i<jVal->u.array.length;i++){
      convertGraphMem(jVal->u.array.values[i],atoms, hyperlinks,linkToParentMem);
    }
  }

  ConvertedGraph(json_value *json_val) {
    atoms = new DynamicArray();
    hyperlinks = new DynamicArray();
    LMNtalData data;
    data.integer = 0;
    LMNtalLink *gRootMemLink = new LMNtalLink(GLOBAL_ROOT_MEM_ATTR, data);
    convertGraphAtomsArray(json_val->u.object.values[2].value, atoms,
                             hyperlinks, gRootMemLink);
    convertGraphMemsArray(json_val->u.object.values[3].value,atoms,hyperlinks,gRootMemLink);
  }
};

void convertedGraphDump(ConvertedGraph *cGraph);
void LMNtalLinkDump(LMNtalLink *link);
void convertedGraphVertexDump(ConvertedGraphVertex *cVertex);
void convertedGraphVertexDumpCaster(void *cVertex);
#endif
