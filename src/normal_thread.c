/*
 * normal_thread.c
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

#include "normal_thread.h"

void* normal_thread(void* arg){
  arginfo *thread_data;
  LmnRuleInstr thread_instr;
  LmnSAtom atom;
  double start_time, stop_time;
  int * idp;
  SameProcCxt *spc;
  idp= (int *)arg;

  thread_data=thread_info[*idp];

  while(1){
    op_lock(thread_data->id, 1);
    if(!lmn_env.enable_parallel)break;
    thread_instr=thread_data->instr;
    thread_data->profile->wakeup++;
    if(lmn_env.profile_level >= 1)start_time = get_cpu_time();

    EACH_ATOM_THREAD_OPT(atom, thread_data->atomlist_ent, thread_data->id, active_thread, thread_data->next_atom, ({
	  warry_set(thread_data->rc, thread_data->atomi, atom, LMN_ATTR_MAKE_LINK(0), TT_ATOM);
	  if(rc_hlink_opt(thread_data->atomi, thread_data->rc)){
	    spc = (SameProcCxt *)hashtbl_get(RC_HLINK_SPC(thread_data->rc), (HashKeyType)thread_data->atomi);
	    if (lmn_sameproccxt_all_pc_check_clone(spc, LMN_SATOM(wt(thread_data->rc, thread_data->atomi)), thread_data->atom_arity) && 
		interpret(thread_data->rc, thread_data->rule, thread_instr)) {
	      thread_data->judge=TRUE;
	      thread_data->profile->findatom_num++;
	      break;
	    }
	  }else{
	    if (interpret(thread_data->rc, thread_data->rule, thread_instr)) {
	      thread_data->judge=TRUE;
	      thread_data->profile->findatom_num++;
	      break;
	    }
	  }
	  if(lmn_env.find_atom_parallel)break;
	  thread_data->backtrack++;
	}));
    thread_data->next_atom=atom;
    if(lmn_env.profile_level >= 1){
      stop_time = get_cpu_time();
      lmn_prof.thread_cpu_time_main[thread_data->id] += stop_time - start_time;
    }
    pthread_mutex_unlock(thread_data->exec);
  }
  return NULL;
}

void normal_profile_init(normal_prof *profile){
  profile->wakeup=0;
  profile->backtrack_num=0;
  profile->findatom_num=0;
  return;
}

void normal_parallel_init(void){
  int i;
  findthread=LMN_NALLOC(pthread_t, lmn_env.core_num);
  thread_info=LMN_NALLOC(arginfo *, lmn_env.core_num);
  for(i=0;i<lmn_env.core_num;i++){
    thread_info[i]=LMN_MALLOC(arginfo);
    thread_info[i]->rc=LMN_MALLOC(LmnReactCxt);
    thread_info[i]->rc->work_arry=lmn_register_make(WARRY_DEF_SIZE);
    thread_info[i]->rc->hl_sameproccxt=NULL;
    thread_info[i]->register_size=WARRY_DEF_SIZE;
    thread_info[i]->id=i;
    thread_info[i]->next_atom=NULL;
    thread_info[i]->exec_flag=1;
    thread_info[i]->exec=LMN_MALLOC(pthread_mutex_t);
    pthread_mutex_init(thread_info[i]->exec, NULL);
    pthread_mutex_lock(thread_info[i]->exec);
    thread_info[i]->profile=LMN_MALLOC(normal_prof);
    normal_profile_init(thread_info[i]->profile);
  }
  for(i=0;i<lmn_env.core_num;i++){
    lmn_thread_create(&findthread[i],normal_thread,&(thread_info[i]->id));
  }
  temp=deq_make(1);
  walltime=0;
  success_temp_check=0;
  fail_temp_check=0;
}

void normal_parallel_free(void){
  int i;
  lmn_env.enable_parallel = FALSE;
  for(i=0;i<lmn_env.core_num;i++){
    pthread_mutex_unlock(thread_info[i]->exec);
    lmn_thread_join(findthread[i]);
    pthread_mutex_destroy(thread_info[i]->exec);
    lmn_free(thread_info[i]->exec);
    lmn_register_free(thread_info[i]->rc->work_arry);
    lmn_free(thread_info[i]->rc);
    lmn_free(thread_info[i]->profile);
    lmn_free(thread_info[i]);
  }
  lmn_env.enable_parallel = TRUE;
  lmn_free(thread_info);
  lmn_free(findthread);
  deq_free(temp);
}

void threadinfo_init(int id, LmnInstrVar atomi, LmnRule rule, LmnReactCxt *rc, LmnRuleInstr instr, AtomListEntry *atomlist_ent, int atom_arity){
  //
  thread_info[id]->judge=FALSE;
  thread_info[id]->atomi=atomi;
  thread_info[id]->rule=rule;
  thread_info[id]->backtrack=0;
  react_context_copy(thread_info[id]->rc, rc);
  if(thread_info[id]->register_size < rc->warry_num){
    lmn_register_extend(thread_info[id]->rc, rc->warry_num);
    thread_info[id]->register_size = rc->warry_num;
  }
  lmn_register_copy(thread_info[id]->rc->work_arry, rc->work_arry, rc->warry_num);
  thread_info[id]->instr=instr;
  thread_info[id]->atomlist_ent=atomlist_ent;
  thread_info[id]->atom_arity=atom_arity;
}
void op_lock(int id, int flag){
  while(thread_info[id]->exec_flag!=flag);
  pthread_mutex_lock(thread_info[id]->exec);
  thread_info[id]->exec_flag=1-thread_info[id]->exec_flag;
  return;
}

void normal_parallel_prof_dump(FILE *f){
  //parallel pattern matching profile 
  int i;
  unsigned long findatom_num;
  findatom_num=0;
  fprintf(f, "\n===Parallel Pattern Matching Profile========================\n");
  fprintf(f,   "[id]: [wakeup] [backtrack] [findatom]\n");
  for(i=0;i<lmn_prof.thread_num;i++){
    fprintf(f, "%3d : %8lu %11lu %10lu \n",i,thread_info[i]->profile->wakeup,
	                                     thread_info[i]->profile->backtrack_num,
	                                     thread_info[i]->profile->findatom_num);
    findatom_num+=thread_info[i]->profile->findatom_num;
  }
  fprintf(f, "\nfindatom num:%16lu \n",findatom_num);
  fprintf(f, "Success Check:%15lu \n",success_temp_check);
  fprintf(f, "Fail Check:%18lu \n",fail_temp_check);
  fprintf(f, "Main Rule Wall Time:%9.2lf\n", walltime);
  fprintf(f,   "============================================================\n");
  return;
}


BOOL check_exist(LmnSAtom atom, LmnFunctor f){
  if(!atom)return FALSE;
  if(LMN_SATOM_GET_FUNCTOR(atom)!=f)return FALSE;
  return TRUE;

}

void rule_wall_time_start(void){
  normal_parallel_flag=FALSE;
  walltime_temp=get_wall_time();
  return;
}

void rule_wall_time_finish(void){
  double finish;
  finish=get_wall_time();
  walltime+=finish-walltime_temp;
  return;
}
