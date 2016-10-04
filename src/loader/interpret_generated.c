 #include "translate.h"
 #include "loader/syntax.h"
 #include "arch.h"
 #include "vm/vm.h"
 #include "element/error.h"
 #include "verifier/verifier.h"
 #include <stdio.h>
 #include "so.h"
 #define TR_GFID(x) (x)
 #define TR_GSID(x) (x)
 #define TR_GRID(x) (x)
/* just for debug! */
static FILE *OUT = NULL;
BOOL interpret_generated(LmnReactCxtRef rc,
                         LmnRuleRef rule,
                         LmnRuleInstr instr)
{
  LmnInstrOp op;
  /* just for debug! */
  if(! OUT){
    /* out = stderr; */
    OUT = stdout;
    /* OUT = fopen("/dev/null", "w"); */
  }
  while (TRUE) {
  /* LOOP:; */
    READ_VAL(LmnInstrOp, instr, op);
    switch (op) {
case INSTR_SPEC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  TR_INSTR_SPEC(rc, targ1);
  break;
}
case INSTR_INSERTCONNECTORSINNULL:{
  LmnInstrVar targ0;
  LmnWord *targ1;
  int targ1_num;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL_LIST(instr, targ1);
  {
    const Vector v = vec_const_temporary_from_array(targ1_num, targ1);
    warry_set(rc, targ0, (LmnWord)insertconnectors(rc, NULL, &v), 0, TT_OTHER);
  }
  free(targ1);
  break;
}
case INSTR_INSERTCONNECTORS:{
  LmnInstrVar targ0;
  LmnWord *targ1;
  int targ1_num;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL_LIST(instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  {
    const Vector v = vec_const_temporary_from_array(targ1_num, targ1);
    warry_set(rc, targ0, (LmnWord)insertconnectors(rc, (LmnMembraneRef)wt(rc, targ2), &v), 0, TT_OTHER);
  }
  free(targ1);
  break;
}
case INSTR_COMMIT:{
  lmn_interned_str targ0;
  LmnLineNum targ1;
  READ_VAL(lmn_interned_str, instr, targ0);
  READ_VAL(LmnLineNum, instr, targ1);
  {
    LmnMembraneRef ptmp_global_root;
    LmnRegisterArray v;
    unsigned int org_next_id;
    unsigned int warry_use_org, warry_size_org;
    warry_use_org  = warry_use_size(rc);
    warry_size_org = warry_size(rc);
    org_next_id = 0;
    tr_instr_commit_ready(rc, rule, TR_GSID(targ0), targ1, &ptmp_global_root, &v, &org_next_id);
  }
  break;
}
case INSTR_FINDATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnLinkAttr targ2_attr;
  union LmnFunctorLiteral targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL_FUNC(instr, targ2);
  if (LMN_ATTR_IS_DATA(targ2_attr)) {
    lmn_fatal("I can not find data atoms.");
  } else {
    {
      AtomListEntryRef atomlist_ent = lmn_mem_get_atomlist((LmnMembraneRef)wt(rc, targ1), TR_GFID(targ2.functor_data));
      LmnSAtom atom;
      if (atomlist_ent) {
        at_set(rc, targ0, LMN_ATTR_MAKE_LINK(0));
        /* EACH_ATOMを使うとループ内コード中でコンマが使えない場合が出てくる */
        for(atom = atomlist_head(atomlist_ent);
            atom != lmn_atomlist_end(atomlist_ent);
            atom = LMN_SATOM_GET_NEXT_RAW(atom)){
          if(LMN_SATOM_GET_FUNCTOR(atom) != LMN_RESUME_FUNCTOR){
            wt_set(rc, targ0, (LmnWord)atom);
            tt_set(rc, targ0, TT_ATOM);
          }
        }
      }
    }
    return FALSE;
  }
  break;
}
case INSTR_LOCKMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  lmn_interned_str targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(lmn_interned_str, instr, targ2);
  warry_set(rc, targ0, (LmnWord)LMN_PROXY_GET_MEM(wt(rc, targ1)), 0, TT_MEM);
  if(LMN_MEM_NAME_ID((LmnMembraneRef)wt(rc, targ0)) != TR_GSID(targ2)) return FALSE;
  break;
}
case INSTR_ANYMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  lmn_interned_str targ3;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  READ_VAL(lmn_interned_str, instr, targ3);
  {
    LmnMembraneRef mp = lmn_mem_child_head((LmnMembraneRef)wt(rc, targ1));
    for (; mp; mp=lmn_mem_next(mp)) {
      warry_set(rc, targ0, (LmnWord)mp, 0, TT_MEM);
      if (LMN_MEM_NAME_ID(mp) == TR_GSID(targ3)){
      }
    }
    return FALSE;
  }
  break;
}
case INSTR_NMEMS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if (!lmn_mem_nmems((LmnMembraneRef)wt(rc, targ0), targ1)) return FALSE;
  break;
}
case INSTR_NORULES:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  if (vec_num(lmn_mem_get_rulesets((LmnMembraneRef)wt(rc, targ0)))) return FALSE;
  break;
}
case INSTR_NEWATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnLinkAttr targ2_attr;
  union LmnFunctorLiteral targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL_FUNC(instr, targ2);
  switch(targ2_attr){
  case LMN_INT_ATTR:
    wt_set(rc, targ0, targ2.long_data);
    break;
  case LMN_DBL_ATTR:
  {
    LmnAtom d;
    d = lmn_create_double_atom(targ2.double_data);
    wt_set(rc, targ0, d);
   }
    break;
  case LMN_STRING_ATTR:
    wt_set(rc, targ0, (LmnWord)lmn_string_make(lmn_id_to_name(TR_GSID(targ2.string_data))));
    break;
  default:
    wt_set(rc, targ0, LMN_ATOM(lmn_new_atom(TR_GFID(targ2.functor_data))));
    break;
  }
  at_set(rc, targ0, targ2_attr);
  tt_set(rc, targ0, TT_ATOM);
  lmn_mem_push_atom((LmnMembraneRef)wt(rc, targ1), wt(rc, targ0), targ2_attr);
  break;
}
case INSTR_NATOMS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if (!lmn_mem_natoms((LmnMembraneRef)wt(rc, targ0), targ1)) return FALSE;
  break;
}
case INSTR_NATOMSINDIRECT:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if (!lmn_mem_natoms((LmnMembraneRef)wt(rc, targ0), wt(rc, targ1))) return FALSE;
  break;
}
case INSTR_ALLOCLINK:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  TR_INSTR_ALLOCLINK(rc, targ0, targ1, targ2);
  break;
}
case INSTR_UNIFYLINKS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  TR_INSTR_UNIFYLINKS(rc, targ0, targ1, targ2);
  break;
}
case INSTR_NEWLINK:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  LmnInstrVar targ3;
  LmnInstrVar targ4;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  READ_VAL(LmnInstrVar, instr, targ3);
  READ_VAL(LmnInstrVar, instr, targ4);
  lmn_mem_newlink((LmnMembraneRef)wt(rc, targ4), wt(rc, targ0), at(rc, targ0), targ1, wt(rc, targ2), at(rc, targ2), targ3);
  break;
}
case INSTR_RELINK:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  LmnInstrVar targ3;
  LmnInstrVar targ4;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  READ_VAL(LmnInstrVar, instr, targ3);
  READ_VAL(LmnInstrVar, instr, targ4);
  TR_INSTR_RELINK(rc, targ0, targ1, targ2, targ3, targ4);
  break;
}
case INSTR_GETLINK:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, LMN_SATOM_GET_LINK(wt(rc, targ1), targ2), LMN_SATOM_GET_ATTR(wt(rc, targ1), targ2), TT_ATOM);
  break;
}
case INSTR_UNIFY:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  LmnInstrVar targ3;
  LmnInstrVar targ4;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  READ_VAL(LmnInstrVar, instr, targ3);
  READ_VAL(LmnInstrVar, instr, targ4);
  lmn_mem_unify_atom_args((LmnMembraneRef)wt(rc, targ4), LMN_SATOM(wt(rc, targ0)), targ1, LMN_SATOM(wt(rc, targ2)), targ3);
  break;
}
case INSTR_PROCEED:{
  return TRUE;
  break;
}
case INSTR_STOP:{
  return FALSE;
  break;
}
case INSTR_NOT:{
  LmnSubInstrSize targ0;
  READ_VAL(LmnSubInstrSize, instr, targ0);
  { /* not */
  }
  break;
}
case INSTR_ENQUEUEATOM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  break;
}
case INSTR_DEQUEUEATOM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  break;
}
case INSTR_NEWMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  {
    LmnMembraneRef mp = lmn_mem_make();
    lmn_mem_add_child_mem((LmnMembraneRef)wt(rc, targ1), mp);
    wt_set(rc, targ0, (LmnWord)mp);
    tt_set(rc, targ0, TT_MEM);
    lmn_mem_set_active(mp, TRUE);
    if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
      lmn_memstack_push(RC_MEMSTACK(rc), mp);
    }
  }
  break;
}
case INSTR_ALLOCMEM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  wt_set(rc, targ0, (LmnWord)lmn_mem_make());
  tt_set(rc, targ0, TT_MEM);
  break;
}
case INSTR_REMOVEATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  lmn_mem_remove_atom((LmnMembraneRef)wt(rc, targ1), wt(rc, targ0), at(rc, targ0));
  break;
}
case INSTR_FREEATOM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  lmn_free_atom(wt(rc, targ0), at(rc, targ0));
  break;
}
case INSTR_REMOVEMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  lmn_mem_remove_mem((LmnMembraneRef)wt(rc, targ1), (LmnMembraneRef)wt(rc, targ0));
  break;
}
case INSTR_FREEMEM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  lmn_mem_free((LmnMembraneRef)wt(rc, targ0));
  break;
}
case INSTR_ADDMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  lmn_mem_add_child_mem((LmnMembraneRef)wt(rc, targ0), (LmnMembraneRef)wt(rc, targ1));
  break;
}
case INSTR_ENQUEUEMEM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {
    lmn_memstack_push(RC_MEMSTACK(rc), (LmnMembraneRef)wt(rc, targ0)); /* 通常実行時 */
  }
  break;
}
case INSTR_UNLOCKMEM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  break;
}
case INSTR_LOADRULESET:{
  LmnInstrVar targ0;
  LmnRulesetId targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnRulesetId, instr, targ1);
  lmn_mem_add_ruleset((LmnMembraneRef)wt(rc, targ0), lmn_ruleset_from_id(TR_GRID(targ1)));
  break;
}
case INSTR_LOADMODULE:{
  LmnInstrVar targ0;
  lmn_interned_str targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(lmn_interned_str, instr, targ1);
  {
    LmnRuleSetRef ruleset;
    if ((ruleset = lmn_get_module_ruleset(TR_GSID(targ1)))) {
      lmn_mem_add_ruleset((LmnMembraneRef)wt(rc, targ0), ruleset);
    } else {
      fprintf(stderr, "Undefined module %s\n", lmn_id_to_name(TR_GSID(targ1)));
    }
  }
  break;
}
case INSTR_RECURSIVELOCK:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  break;
}
case INSTR_RECURSIVEUNLOCK:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  break;
}
case INSTR_DEREFATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, (LmnWord)LMN_SATOM(LMN_SATOM_GET_LINK(wt(rc, targ1), targ2)), LMN_SATOM_GET_ATTR(wt(rc, targ1), targ2), TT_ATOM);
  break;
}
case INSTR_DEREF:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  LmnInstrVar targ3;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  READ_VAL(LmnInstrVar, instr, targ3);
  {
    LmnByte attr = LMN_SATOM_GET_ATTR(wt(rc, targ1), targ2);
    if (LMN_ATTR_IS_DATA(attr)) {
      if (targ3 != 0) return FALSE;
    } else {
      if (attr != targ3) return FALSE;
    }
    warry_set(rc, targ0, LMN_SATOM_GET_LINK(wt(rc, targ1), targ2), attr, TT_ATOM);
  }
  break;
}
case INSTR_FUNC:{
  LmnInstrVar targ0;
  LmnLinkAttr targ1_attr;
  union LmnFunctorLiteral targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL_FUNC(instr, targ1);
  if (LMN_ATTR_IS_DATA(targ1_attr)) {
    if (LMN_ATTR_IS_DATA(at(rc, targ0)) && at(rc, targ0) == targ1_attr) {
      tt_set(rc, targ0, TT_ATOM);
      switch(targ1_attr) {
      case LMN_INT_ATTR:
        if (wt(rc, targ0) != targ1.long_data) return FALSE;
        break;
      case LMN_DBL_ATTR:
        if (lmn_get_double(wt(rc, targ0)) != targ1.double_data) return FALSE;
        break;
      case LMN_STRING_ATTR: {
        LmnStringRef s = lmn_string_make(lmn_id_to_name(TR_GSID(targ1.string_data)));
        if(! lmn_string_eq(s, (LmnStringRef)wt(rc, targ0))) return FALSE;
        lmn_string_free(s);
        fprintf(stderr, "string attr is not implemented.");
        break;
      }
      default:
        lmn_fatal("implementation error");
      }
    } else {
      return FALSE;
    }
  } else {
    if(LMN_ATTR_IS_DATA(at(rc, targ0)) ||
       LMN_SATOM_GET_FUNCTOR(LMN_SATOM(wt(rc, targ0))) != TR_GFID(targ1.functor_data)) return FALSE;
  }
  break;
}
case INSTR_NOTFUNC:{
  LmnInstrVar targ0;
  LmnLinkAttr targ1_attr;
  union LmnFunctorLiteral targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL_FUNC(instr, targ1);
  if (LMN_ATTR_IS_DATA(targ1_attr)) {
    if(! (LMN_ATTR_IS_DATA(at(rc, targ0)) && at(rc, targ0) == targ1_attr)){
      tt_set(rc, targ0, TT_ATOM);
      switch(targ1_attr){
      case LMN_INT_ATTR:
        if(wt(rc, targ0) == targ1.long_data) return FALSE;
        break;
      case LMN_DBL_ATTR:
        if(lmn_get_double(wt(rc, targ0)) == targ1.double_data) return FALSE;
        fprintf(stderr, "double attr is not implemented.");
        break;
      case LMN_STRING_ATTR: {
        LmnStringRef s = lmn_string_make(lmn_id_to_name(TR_GSID(targ1.string_data)));
        if(lmn_string_eq(s, (LmnStringRef)wt(rc, targ0))) return FALSE;
        lmn_string_free(s);
        fprintf(stderr, "string attr is not implemented.");
        break;
      }
      default:
        lmn_fatal("implementation error");
      }
    }
  } else {
    if(! (LMN_ATTR_IS_DATA(at(rc, targ0)) ||
          LMN_SATOM_GET_FUNCTOR(LMN_SATOM(wt(rc, targ0))) != TR_GFID(targ1.functor_data))) return FALSE;
  }
  break;
}
case INSTR_ISGROUND:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  {
    Vector *srcvec;
    Vector *avovec;
    unsigned long natoms;
    BOOL b;
    avovec = links_from_idxs((Vector *)wt(rc, targ2), rc_warry(rc));
    srcvec = links_from_idxs((Vector *)wt(rc, targ1), rc_warry(rc));
    b = lmn_mem_is_ground(srcvec, avovec, &natoms);
    free_links(srcvec);
    free_links(avovec);
    if(! b) return FALSE;
    warry_set(rc, targ0, natoms, LMN_INT_ATTR, TT_OTHER);
  }
  break;
}
case INSTR_ISUNARY:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  if (LMN_ATTR_IS_DATA(at(rc, targ0))) {
    switch (at(rc, targ0)) {
    case LMN_SP_ATOM_ATTR:
      if (!SP_ATOM_IS_GROUND(wt(rc, targ0))) return FALSE;
      break;
    default:
      break;
    }
  } else if (LMN_SATOM_GET_ARITY(wt(rc, targ0)) != 1){
    return FALSE;
  }
  break;
}
case INSTR_ISINT:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  if(at(rc, targ0) != LMN_INT_ATTR) return FALSE;
  break;
}
case INSTR_ISFLOAT:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  if(at(rc, targ0) != LMN_DBL_ATTR) return FALSE;
  break;
}
case INSTR_ISSTRING:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  if(! lmn_is_string(wt(rc, targ0), at(rc, targ0))) return FALSE;
  break;
}
case INSTR_ISINTFUNC:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  if(at(rc, targ0) != LMN_INT_ATTR) return FALSE;
  break;
}
case INSTR_ISFLOATFUNC:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  if(at(rc, targ0) != LMN_DBL_ATTR) return FALSE;
  break;
}
case INSTR_COPYATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, lmn_copy_atom(wt(rc, targ2), at(rc, targ2)), at(rc, targ2), TT_ATOM);
  lmn_mem_push_atom((LmnMembraneRef)wt(rc, targ1), wt(rc, targ0), at(rc, targ0));
  break;
}
case INSTR_EQATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if (LMN_ATTR_IS_DATA(at(rc, targ0)) ||
      LMN_ATTR_IS_DATA(at(rc, targ1)) ||
      LMN_SATOM(wt(rc, targ0)) != LMN_SATOM(wt(rc, targ1))) return FALSE;
  break;
}
case INSTR_NEQATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if (!(LMN_ATTR_IS_DATA(at(rc, targ0)) ||
        LMN_ATTR_IS_DATA(at(rc, targ1)) ||
        LMN_SATOM(wt(rc, targ0)) != LMN_SATOM(wt(rc, targ1)))) return FALSE;
  break;
}
case INSTR_EQMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(wt(rc, targ0) != wt(rc, targ1)) return FALSE;
  break;
}
case INSTR_NEQMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(wt(rc, targ0) == wt(rc, targ1)) return FALSE;
  break;
}
case INSTR_NEWLIST:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  {
    Vector *listvec = vec_make(16);
    warry_set(rc, targ0, (LmnWord)listvec, 0, TT_OTHER);
  }
  break;
}
case INSTR_ADDTOLIST:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  vec_push((Vector *)wt(rc, targ0), targ1);
  break;
}
case INSTR_GETFROMLIST:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  switch (at(rc, targ1)) {
    case LIST_AND_MAP:
      wt_set(rc, targ0, vec_get((Vector *)wt(rc, targ1), (unsigned int)targ2));
      tt_set(rc, targ0, TT_OTHER);
      if (targ2 == 0){
        at_set(rc, targ0, LINK_LIST);
      }else if (targ2 == 1){
        at_set(rc, targ0, MAP);
      }else{
        lmn_fatal("unexpected attribute @instr_getfromlist");
      }
      break;
    case LINK_LIST: /* LinkObjをfreeするのはここ？ */
    {
      LinkObjRef lo = (LinkObjRef)vec_get((Vector *)wt(rc, targ1), (unsigned int)targ2);
      warry_set(rc, targ0, LinkObjGetAtom(lo), LinkObjGetPos(lo), TT_ATOM);
      break;
    }
  }
  break;
}
case INSTR_EQGROUND:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  {
    Vector *srcvec = links_from_idxs((Vector*)wt(rc, targ0), rc_warry(rc));
    Vector *dstvec = links_from_idxs((Vector*)wt(rc, targ1), rc_warry(rc));
    BOOL same = lmn_mem_cmp_ground(srcvec, dstvec);
    free_links(srcvec);
    free_links(dstvec);
    if(! same) return FALSE;
  }
  break;
}
case INSTR_NEQGROUND:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  {
    Vector *srcvec = links_from_idxs((Vector*)wt(rc, targ0), rc_warry(rc));
    Vector *dstvec = links_from_idxs((Vector*)wt(rc, targ1), rc_warry(rc));
    BOOL same = lmn_mem_cmp_ground(srcvec, dstvec);
    free_links(srcvec);
    free_links(dstvec);
    if(same) return FALSE;
  }
  break;
}
case INSTR_COPYGROUND:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  {
    Vector *srcvec = links_from_idxs((Vector*)wt(rc, targ1), rc_warry(rc));
    Vector *dstlovec, *retvec;
    ProcessTableRef atommap;
    lmn_mem_copy_ground((LmnMembraneRef)wt(rc, targ2), srcvec, &dstlovec, &atommap);
    free_links(srcvec);
    /* 返り値の作成 */
    retvec = vec_make(2);
    vec_push(retvec, (LmnWord)dstlovec);
    vec_push(retvec, (LmnWord)atommap);
    warry_set(rc, targ0, (LmnWord)retvec, LIST_AND_MAP, TT_OTHER);
  }
  break;
}
case INSTR_REMOVEGROUND:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  {
    Vector *srcvec = links_from_idxs((Vector*)wt(rc, targ0), rc_warry(rc));
    lmn_mem_remove_ground((LmnMembraneRef)wt(rc, targ1), srcvec);
    free_links(srcvec);
  }
  break;
}
case INSTR_FREEGROUND:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  {
    Vector *srcvec = links_from_idxs((Vector*)wt(rc, targ0), rc_warry(rc));
    lmn_mem_free_ground(srcvec);
    free_links(srcvec);
  }
  break;
}
case INSTR_STABLE:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  if (lmn_mem_is_active((LmnMembraneRef)wt(rc, targ0))) return FALSE;
  break;
}
case INSTR_IADD:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, (long)wt(rc, targ1) + (long)wt(rc, targ2), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_ISUB:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, (long)wt(rc, targ1) - (long)wt(rc, targ2), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_IMUL:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, (long)wt(rc, targ1) * (long)wt(rc, targ2), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_IDIV:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, (long)wt(rc, targ1) / (long)wt(rc, targ2), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_INEG:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  warry_set(rc, targ0, -(long)wt(rc, targ1), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_IMOD:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, (long)wt(rc, targ1) % (long)wt(rc, targ2), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_INOT:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, ~(long)wt(rc, targ1), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_IAND:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, (long)wt(rc, targ1) & (long)wt(rc, targ2), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_IOR:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, (long)wt(rc, targ1) | (long)wt(rc, targ2), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_IXOR:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, (long)wt(rc, targ1) ^ (long)wt(rc, targ2), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_ILT:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!((long)wt(rc, targ0) < (long)wt(rc, targ1))) return FALSE;
  break;
}
case INSTR_ILE:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!((long)wt(rc, targ0) <= (long)wt(rc, targ1))) return FALSE;
  break;
}
case INSTR_IGT:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!((long)wt(rc, targ0) > (long)wt(rc, targ1))) return FALSE;
  break;
}
case INSTR_IGE:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!((long)wt(rc, targ0) >= (long)wt(rc, targ1))) return FALSE;
  break;
}
case INSTR_IEQ:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!((long)wt(rc, targ0) == (long)wt(rc, targ1))) return FALSE;
  break;
}
case INSTR_INE:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!((long)wt(rc, targ0) != (long)wt(rc, targ1))) return FALSE;
  break;
}
case INSTR_ILTFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!((long)wt(rc, targ0) < (long)wt(rc, targ1))) return FALSE;
  break;
}
case INSTR_ILEFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!((long)wt(rc, targ0) <= (long)wt(rc, targ1))) return FALSE;
  break;
}
case INSTR_IGTFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!((long)wt(rc, targ0) > (long)wt(rc, targ1))) return FALSE;
  break;
}
case INSTR_IGEFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!((long)wt(rc, targ0) >= (long)wt(rc, targ1))) return FALSE;
  break;
}
case INSTR_FADD:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  LmnAtom d = lmn_create_double_atom(lmn_get_double(wt(rc, targ1)) + lmn_get_double(wt(rc, targ2)));
  warry_set(rc, targ0, d, LMN_DBL_ATTR, TT_ATOM);
  break;
}
case INSTR_FSUB:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  LmnAtom d = lmn_create_double_atom(lmn_get_double(wt(rc, targ1)) - lmn_get_double(wt(rc, targ2)));
  warry_set(rc, targ0, d, LMN_DBL_ATTR, TT_ATOM);
  break;
}
case INSTR_FMUL:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  LmnAtom d = lmn_create_double_atom(lmn_get_double(wt(rc, targ1)) * lmn_get_double(wt(rc, targ2)));
  warry_set(rc, targ0, d, LMN_DBL_ATTR, TT_ATOM);
  break;
}
case INSTR_FDIV:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  LmnAtom d = lmn_create_double_atom(lmn_get_double(wt(rc, targ1)) / lmn_get_double(wt(rc, targ2)));
  warry_set(rc, targ0, d, LMN_DBL_ATTR, TT_ATOM);
  break;
}
case INSTR_FNEG:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  LmnAtom d = lmn_create_double_atom(-lmn_get_double(wt(rc, targ1)));
  warry_set(rc, targ0, d, LMN_DBL_ATTR, TT_ATOM);
  break;
}
case INSTR_FLT:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!(lmn_get_double(wt(rc, targ0)) < lmn_get_double(wt(rc, targ1)))) return FALSE;
  break;
}
case INSTR_FLE:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!(lmn_get_double(wt(rc, targ0)) <= lmn_get_double(wt(rc, targ1)))) return FALSE;
  break;
}
case INSTR_FGT:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!(lmn_get_double(wt(rc, targ0)) > lmn_get_double(wt(rc, targ1)))) return FALSE;
  break;
}
case INSTR_FGE:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!(lmn_get_double(wt(rc, targ0)) >= lmn_get_double(wt(rc, targ1)))) return FALSE;
  break;
}
case INSTR_FEQ:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!(lmn_get_double(wt(rc, targ0)) == lmn_get_double(wt(rc, targ1)))) return FALSE;
  break;
}
case INSTR_FNE:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(!(lmn_get_double(wt(rc, targ0)) != lmn_get_double(wt(rc, targ1)))) return FALSE;
  break;
}
case INSTR_ALLOCATOM:{
  LmnInstrVar targ0;
  LmnLinkAttr targ1_attr;
  union LmnFunctorLiteral targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL_FUNC(instr, targ1);
  switch(targ1_attr){
  case LMN_INT_ATTR:
    warry_set(rc, targ0, targ1.long_data, LMN_INT_ATTR, TT_ATOM);
    break;
  case LMN_DBL_ATTR:
    {
  /* 困った */
    }
    break;
  case LMN_STRING_ATTR:
    warry_set(rc, targ0, targ1.string_data, LMN_CONST_STR_ATTR, TT_ATOM);
    break;
  default:
    lmn_fatal("Implementation error");
  }
  break;
}
case INSTR_ALLOCATOMINDIRECT:{
  LmnInstrVar targ0;
  LmnFunctor targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnFunctor, instr, targ1);
  if (LMN_ATTR_IS_DATA(at(rc, targ1))) {
    warry_set(rc, targ0, lmn_copy_data_atom(wt(rc, targ1), at(rc, targ1)), at(rc, targ1), TT_ATOM);
  } else { /* symbol atom */
    fprintf(stderr, "symbol atom can't be created in GUARD\n");
    exit(EXIT_FAILURE);
  }
  break;
}
case INSTR_SAMEFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if (!lmn_eq_func(wt(rc, targ0), at(rc, targ0), wt(rc, targ1), at(rc, targ1))) return FALSE;
  break;
}
case INSTR_GETFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if(LMN_ATTR_IS_DATA(at(rc, targ1))){
    wt_set(rc, targ0, wt(rc, targ1));
  }else{
    wt_set(rc, targ0, LMN_SATOM_GET_FUNCTOR(wt(rc, targ1)));
  }
  at_set(rc, targ0, at(rc, targ1));
  tt_set(rc, targ0, TT_OTHER);
  break;
}
case INSTR_SETMEMNAME:{
  LmnInstrVar targ0;
  lmn_interned_str targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(lmn_interned_str, instr, targ1);
  lmn_mem_set_name((LmnMembraneRef)wt(rc, targ0), TR_GSID(targ1));
  break;
}
case INSTR_COPYRULES:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  TR_INSTR_COPYRULES(rc, targ0, targ1);
  break;
}
case INSTR_REMOVEPROXIES:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  lmn_mem_remove_proxies((LmnMembraneRef)wt(rc, targ0));
  break;
}
case INSTR_INSERTPROXIES:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  lmn_mem_insert_proxies((LmnMembraneRef)wt(rc, targ0), (LmnMembraneRef)wt(rc, targ1));
  break;
}
case INSTR_DELETECONNECTORS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  TR_INSTR_DELETECONNECTORS(targ0, targ1);
  break;
}
case INSTR_REMOVETOPLEVELPROXIES:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  lmn_mem_remove_toplevel_proxies((LmnMembraneRef)wt(rc, targ0));
  break;
}
case INSTR_DEREFFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  TR_INSTR_DEREFFUNC(rc, targ0, targ1, targ2);
  break;
}
case INSTR_LOADFUNC:{
  LmnInstrVar targ0;
  LmnLinkAttr targ1_attr;
  union LmnFunctorLiteral targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL_FUNC(instr, targ1);
  if(LMN_ATTR_IS_DATA(targ1_attr)){
    switch(targ1_attr){
    case LMN_INT_ATTR:
      wt_set(rc, targ0, targ1.long_data);
      at_set(rc, targ0, LMN_INT_ATTR);
      break;
    case LMN_DBL_ATTR:
      {
  /* 困った */
      }
      break;
    case LMN_STRING_ATTR:
      wt_set(rc, targ0, targ1.string_data);
      at_set(rc, targ0, LMN_CONST_STR_ATTR);
      break;
    default:
      lmn_fatal("Implementation error");
    }
  }else{
    wt_set(rc, targ0, targ1.functor_data);
    at_set(rc, targ0, targ1_attr);
  }
  tt_set(rc, targ0, TT_OTHER);
  break;
}
case INSTR_EQFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if (at(rc, targ0) != at(rc, targ1)) return FALSE;
  switch (at(rc, targ0)) {
  case LMN_INT_ATTR:
    if ((long)wt(rc, targ0) != (long)wt(rc, targ1)) return FALSE;
    break;
  case LMN_DBL_ATTR:
    if (lmn_get_double(wt(rc, targ0)) !=
        lmn_get_double(wt(rc, targ1))) return FALSE;
    break;
  default:
    if (wt(rc, targ0) != wt(rc, targ1)) return FALSE;
    break;
  }
  break;
}
case INSTR_NEQFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if (at(rc, targ0) == at(rc, targ1)) {
    switch (at(rc, targ0)) {
    case LMN_INT_ATTR:
      if ((long)wt(rc, targ0) == (long)wt(rc, targ1)) return FALSE;
      break;
    case LMN_DBL_ATTR:
      if (lmn_get_double(wt(rc, targ0)) ==
          lmn_get_double(wt(rc, targ1))) return FALSE;
      break;
    default:
      if (wt(rc, targ0) == wt(rc, targ1)) return FALSE;
      break;
    }
  }
  break;
}
case INSTR_ADDATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  lmn_mem_push_atom((LmnMembraneRef)wt(rc, targ0), wt(rc, targ1), at(rc, targ1));
  break;
}
case INSTR_MOVECELLS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  lmn_mem_move_cells((LmnMembraneRef)wt(rc, targ0), (LmnMembraneRef)wt(rc, targ1));
  break;
}
case INSTR_REMOVETEMPORARYPROXIES:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  lmn_mem_remove_temporary_proxies((LmnMembraneRef)wt(rc, targ0));
  break;
}
case INSTR_NFREELINKS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if (!lmn_mem_nfreelinks((LmnMembraneRef)wt(rc, targ0), targ1)) return FALSE;
  break;
}
case INSTR_COPYCELLS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  wt_set(rc, targ0, (LmnWord)lmn_mem_copy_cells((LmnMembraneRef)wt(rc, targ1), (LmnMembraneRef)wt(rc, targ2)));
  tt_set(rc, targ0, TT_OTHER);
  break;
}
case INSTR_LOOKUPLINK:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  TR_INSTR_LOOKUPLINK(rc, targ0, targ1, targ2);
  break;
}
case INSTR_CLEARRULES:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  vec_clear(lmn_mem_get_rulesets((LmnMembraneRef)wt(rc, targ0)));
  break;
}
case INSTR_DROPMEM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  lmn_mem_drop((LmnMembraneRef)wt(rc, targ0));
  break;
}
case INSTR_TESTMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  if (LMN_PROXY_GET_MEM(wt(rc, targ1)) != (LmnMembraneRef)wt(rc, targ0)) return FALSE;
  break;
}
case INSTR_IADDFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, wt(rc, targ1) + wt(rc, targ2), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_ISUBFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, wt(rc, targ1) - wt(rc, targ2), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_IMULFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, wt(rc, targ1) * wt(rc, targ2), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_IDIVFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, wt(rc, targ1) / wt(rc, targ2), LMN_INT_ATTR, TT_ATOM);
  break;
}
case INSTR_IMODFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  warry_set(rc, targ0, wt(rc, targ1) % wt(rc, targ2), LMN_INT_ATTR, TT_ATOM);
  break;
}
    default:
      fprintf(stderr, "interpret_generated: Unknown operation %d\n", op);
      exit(1);
    }
  }
}
