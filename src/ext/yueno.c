/*
  * yueno.c
  *
  *  Created on: 2009/06/16
  *      Author: yueno
  */     
 #include "../lmntal_ext.h"
            
 void yueno_integer_twice(LmnMembrane *mem,
                          LmnAtom a0, LmnLinkAttr t0,
                          LmnAtom a1, LmnLinkAttr t1)
 {
   LmnWord n = a0 * 2;
   lmn_mem_newlink(mem,
                   a1, LMN_ATTR_MAKE_LINK(0), LMN_ATTR_GET_VALUE(t1),
                   n,  LMN_INT_ATTR, 0);
   lmn_mem_push_atom(mem, n, LMN_INT_ATTR);
   lmn_mem_delete_atom(mem, a0, t0);
 }
 
 void init_yueno(void)
 {
   lmn_register_c_fun("yueno_integer_twice", yueno_integer_twice, 2);
 }
