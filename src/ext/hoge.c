#include <stdio.h>
#include "../lmntal_ext.h"
#include "../atom.h"
#include "../dumper.h"

 void hoge(LmnReactCxt *rc,
 	  LmnMembrane *mem,
	   LmnAtom a0, LmnLinkAttr t0,
	   LmnAtom a1, LmnLinkAttr t1)
 {
   /* LmnSAtom s = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("proxy"), 1)); */
   /* LmnSAtom f = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("not"), 1)); */
   /* LmnSAtom success   = lmn_mem_newatom(mem, */
   /*           			   lmn_functor_intern(ANONYMOUS, lmn_intern("success"), 1)); */
   /* Lmnsatom fail   = lmn_mem_newatom(mem, */
   /*           			   lmn_functor_intern(ANONYMOUS, lmn_intern("fail"), 1)); */
   if(LMN_SATOM_IS_PROXY(a0))
     {
       LmnSAtom success = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("success"), 1));
       /* printf("PROXY!!!!!!!!!!!!!!!!!!!!!!\n"); */
       lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), LMN_ATOM(success), LMN_ATTR_MAKE_LINK(0), 0);
       /* /\* lmn_dump_cell_stdout(LMN_PROXY_GET_MEM(LMN_SATOM(LMN_SATOM_GET_LINK(LMN_SATOM(a0), 0)))); *\/ */
       lmn_mem_newlink(mem,
		       a1, t1, LMN_ATTR_GET_VALUE(t1),
		       LMN_ATOM(success), LMN_ATTR_MAKE_LINK(0), 0);
     }
   else
     {
       LmnSAtom fail = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("fail"), 1));
	/* printf("NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO\n"); */
       /* lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), LMN_ATOM(fail), LMN_ATTR_MAKE_LINK(0), 0); */
       lmn_mem_newlink(mem,
		       a1, t1, LMN_ATTR_GET_VALUE(t1),
		       LMN_ATOM(fail), LMN_ATTR_MAKE_LINK(0), 0);
     }


 /*   lmn_mem_delete_atom(mem, a0, t0); */
 /* const char *s = (const char *)lmn_string_c_str(LMN_STRING(a0)); */
   /* LmnSAtom sa   = lmn_mem_newatom(mem, */
   /*           			   lmn_functor_intern(ANONYMOUS, lmn_intern(s), 1)); */
   /* lmn_mem_newlink(mem, */
   /*                 a1, t1, LMN_ATTR_GET_VALUE(t1), */
   /*                 LMN_ATOM(success), LMN_ATTR_MAKE_LINK(0), 0); */
   lmn_mem_delete_atom(mem, a0, t0);
 }
 
 void init_hoge(void)
 {
   lmn_register_c_fun("hoge", hoge, 2);

 }
