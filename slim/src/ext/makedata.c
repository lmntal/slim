/*
 * makedata.c
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
#include "lmntal.h"
#include "lmntal_ext.h"
#include "slim_header/string.h"
#include "special_atom.h"
#include "visitlog.h"


void init_makedata(void);            
 void costmake_data(LmnReactCxt *rc,
			  LmnMembrane *mem,
                          LmnAtom a0, LmnLinkAttr t0,
                          LmnAtom a1, LmnLinkAttr t1,
			  LmnAtom a2, LmnLinkAttr t2)
 {



   Vector *srcvec;
  int i, j, n;
  int start, end;
  start  = (int)a0;
  end    = (int)a1;
  //if(LMN_ATTR_GET_VALUE(t2)<2)return 0;
  srcvec = vec_make(16);
  vec_push(srcvec, (LmnWord)LinkObj_make(a2, t2));

  for (n = start; n <= end; n++) {
	 for (i = start; i <= end; i++) {
    Vector *dstlovec;
    ProcessTbl atommap;
    LinkObj l;

    lmn_mem_copy_ground(mem, srcvec, &dstlovec, &atommap);

    l = (LinkObj)vec_get(dstlovec, 0);
    lmn_mem_newlink(mem, n, LMN_INT_ATTR, 0,
                    l->ap, t2, 0);
    lmn_mem_newlink(mem, i, LMN_INT_ATTR, 0,
                    l->ap, t2, 1);
    if(i==n)lmn_mem_newlink(mem, 0, LMN_INT_ATTR, 0,
                    l->ap, t2, 2);
    else lmn_mem_newlink(mem, 10000, LMN_INT_ATTR, 0,
                    l->ap, t2, 2);

    for (j = 0; j < vec_num(dstlovec); j++) LMN_FREE(vec_get(dstlovec, j));
    vec_free(dstlovec);
    proc_tbl_free(atommap);
  }
 }

  lmn_mem_delete_atom(mem, a0, t0);
  lmn_mem_delete_atom(mem, a1, t1);

  lmn_mem_delete_ground(mem, srcvec);

  for (i = 0; i < vec_num(srcvec); i++) LMN_FREE(vec_get(srcvec, i));
  vec_free(srcvec);
 }

void costmake_data_p(LmnReactCxt *rc,
                 LmnMembrane *mem,
                 LmnAtom a0, LmnLinkAttr t0,
                 LmnAtom a1, LmnLinkAttr t1,
                 LmnAtom a2, LmnLinkAttr t2)
{
  Vector *srcvec;
  int i, j, n;
  int start, end;

  start  = (int)101;
  end    = (int)a1;
  srcvec = vec_make(16);
  vec_push(srcvec, (LmnWord)LinkObj_make(a2, t2));

  for (i = 0, n = start; n <= end; i++, n++) {
    Vector *dstlovec;
    ProcessTbl atommap;
    LinkObj l;

    lmn_mem_copy_ground(mem, srcvec, &dstlovec, &atommap);

    l = (LinkObj)vec_get(dstlovec, 0);
    lmn_mem_newlink(mem, n, LMN_INT_ATTR, 0,
                    l->ap, t2, 0);
    if(n==(int)a0)lmn_mem_newlink(mem, 0, LMN_INT_ATTR, 0,
                    l->ap, t2, 1);
    else lmn_mem_newlink(mem, 10000, LMN_INT_ATTR, 0,
                    l->ap, t2, 1);
    for (j = 0; j < vec_num(dstlovec); j++) LMN_FREE(vec_get(dstlovec, j));
    vec_free(dstlovec);
    proc_tbl_free(atommap);
  }

  lmn_mem_delete_atom(mem, a0, t0);
  lmn_mem_delete_atom(mem, a1, t1);

  lmn_mem_delete_ground(mem, srcvec);

  for (i = 0; i < vec_num(srcvec); i++) LMN_FREE(vec_get(srcvec, i));
  vec_free(srcvec);
}

int getrandom(int max,int min){
	return min + (int)(rand()*(max-min+1.0)/(1.0+RAND_MAX));
}

 void emake_data(LmnReactCxt *rc,
			  LmnMembrane *mem,
                          LmnAtom a0, LmnLinkAttr t0,
                          LmnAtom a1, LmnLinkAttr t1,
			  LmnAtom a2, LmnLinkAttr t2)
 {



   Vector *srcvec;
  int i, j, k, n,**e,x;
  int start, end;
  start  = (int)a0;
  end    = (int)a1;
  x=(end-start+1)*3/2;
  srcvec = vec_make(16);
  vec_push(srcvec, (LmnWord)LinkObj_make(a2, t2));
  e=malloc(sizeof(int*)*x);
  for(j=0;j<x;j++){
    e[j]=malloc(sizeof(int)*3);

  }

	i=0;
	while(1){
		e[i][0]=getrandom(end,start);
		e[i][1]=getrandom(end,start);
		if(e[i][0]==e[i][1])continue;
		e[i][2]=getrandom(30,1);//1 to 30
		if(i+100<end-1){
			e[i][0]=101+i;
			e[i][1]=101+i+1;
			i++;
			if(i>x||i==x)break;
			continue;
		}
		for(j=0;j<i;j++){
			if((e[j][0]==e[i][0]&&e[j][1]==e[i][1])||(e[j][0]==e[i][1]&&e[j][1]==e[i][0])){i--;break;}
		}
		i++;
		if(i>x||i==x)break;
	}


  for (n = 0; n < x; n++) {
    Vector *dstlovec;
    ProcessTbl atommap;
    LinkObj l;

    lmn_mem_copy_ground(mem, srcvec, &dstlovec, &atommap);

    l = (LinkObj)vec_get(dstlovec, 0);
    lmn_mem_newlink(mem, e[n][0], LMN_INT_ATTR, 0,
                    l->ap, t2, 0);
    lmn_mem_newlink(mem, e[n][1], LMN_INT_ATTR, 0,
                    l->ap, t2, 1);

    lmn_mem_newlink(mem, e[n][2], LMN_INT_ATTR, 0,
                    l->ap, t2, 2);

    for (j = 0; j < vec_num(dstlovec); j++) LMN_FREE(vec_get(dstlovec, j));
    vec_free(dstlovec);
    proc_tbl_free(atommap);
  }
  for (n = 0; n < x; n++) {
    Vector *dstlovec;
    ProcessTbl atommap;
    LinkObj l;

    lmn_mem_copy_ground(mem, srcvec, &dstlovec, &atommap);

    l = (LinkObj)vec_get(dstlovec, 0);
    lmn_mem_newlink(mem, e[n][1], LMN_INT_ATTR, 0,
                    l->ap, t2, 0);
    lmn_mem_newlink(mem, e[n][0], LMN_INT_ATTR, 0,
                    l->ap, t2, 1);

    lmn_mem_newlink(mem, e[n][2], LMN_INT_ATTR, 0,
                    l->ap, t2, 2);

    for (j = 0; j < vec_num(dstlovec); j++) LMN_FREE(vec_get(dstlovec, j));
    vec_free(dstlovec);
    proc_tbl_free(atommap);
  }

  lmn_mem_delete_atom(mem, a0, t0);
  lmn_mem_delete_atom(mem, a1, t1);

  lmn_mem_delete_ground(mem, srcvec);

  for (i = 0; i < vec_num(srcvec); i++) LMN_FREE(vec_get(srcvec, i));
  vec_free(srcvec);

  for(j=0;j<x;j++){
    free(e[j]);
  }
  free(e);
 }
 void init_makedata(void)
 {
   lmn_register_c_fun("costmake_data", costmake_data, 3);
   lmn_register_c_fun("costmake_data_p", costmake_data_p, 3);
   lmn_register_c_fun("emake_data", emake_data, 3);
 }
