 #include "translate.h"
 #include "loader/syntax.h"
 #include "arch.h"
 #include "symbol.h"
 #include "vm/vm.h"
 #include "element/error.h"
 #include "verifier/verifier.h"
 #include <stdio.h>
const BYTE *translate_instruction_generated(const BYTE *instr,
                                            Vector *jump_points,
                                            const char *header,
                                            const char *successcode,
                                            const char *failcode,
                                            int indent,
                                            int *finishflag)
{
  LmnInstrOp op;
  const BYTE * const op_address = instr;
  READ_VAL(LmnInstrOp, instr, op);
  *finishflag = 1;
  switch (op) {
case INSTR_SPEC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  TR_INSTR_SPEC(rc, %d);\n", (int)targ1);
  return instr;
}
case INSTR_INSERTCONNECTORSINNULL:{
  LmnInstrVar targ0;
  LmnWord *targ1;
  int targ1_num;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL_LIST(instr, targ1);
  print_indent(indent++); printf("{\n");
  tr_print_list(indent, 1, targ1_num, targ1);
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    const Vector v = vec_const_temporary_from_array(targ1_num, targ1);\n");
  print_indent(indent); printf("    warry_set(rc, %d, (LmnWord)insertconnectors(rc, NULL, &v), 0, TT_OTHER);\n", (int)targ0);
    {
      char *buf_always = automalloc_sprintf("goto label_always_%p", op_address);
      instr = translate_instructions(instr, jump_points, header, buf_always, buf_always, indent+1);
      free(buf_always);
    }
  print_indent(indent); printf("  label_always_%p:\n", op_address);
  print_indent(indent); printf("    hashset_free((HashSet *)wt(rc, %d));\n", (int)targ0);
  print_indent(indent); printf("    %s;\n", successcode);
  print_indent(indent); printf("    lmn_fatal(\"translate recursive error\\n\");\n");
  print_indent(indent); printf("  }\n");
  *finishflag = 0;
  free(targ1);
  print_indent(--indent); printf("}\n");
  return instr;
}
case INSTR_INSERTCONNECTORS:{
  LmnInstrVar targ0;
  LmnWord *targ1;
  int targ1_num;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL_LIST(instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent++); printf("{\n");
  tr_print_list(indent, 1, targ1_num, targ1);
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    const Vector v = vec_const_temporary_from_array(targ1_num, targ1);\n");
  print_indent(indent); printf("    warry_set(rc, %d, (LmnWord)insertconnectors(rc, (LmnMembraneRef)wt(rc, %d), &v), 0, TT_OTHER);\n", (int)targ0, (int)targ2);
    {
      char *buf_always = automalloc_sprintf("goto label_always_%p", op_address);
      instr = translate_instructions(instr, jump_points, header, buf_always, buf_always, indent+1);
      free(buf_always);
    }
  print_indent(indent); printf("  label_always_%p:\n", op_address);
  print_indent(indent); printf("    hashset_free((HashSet *)wt(rc, %d));\n", (int)targ0);
  print_indent(indent); printf("    %s;\n", successcode);
  print_indent(indent); printf("    lmn_fatal(\"translate recursive error\\n\");\n");
  print_indent(indent); printf("  }\n");
  *finishflag = 0;
  free(targ1);
  print_indent(--indent); printf("}\n");
  return instr;
}
case INSTR_COMMIT:{
  lmn_interned_str targ0;
  LmnLineNum targ1;
  READ_VAL(lmn_interned_str, instr, targ0);
  READ_VAL(LmnLineNum, instr, targ1);
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    LmnMembraneRef ptmp_global_root;\n");
  print_indent(indent); printf("    LmnRegisterArray v;\n");
  print_indent(indent); printf("    unsigned int org_next_id;\n");
  print_indent(indent); printf("    unsigned int warry_use_org, warry_size_org;\n");
  print_indent(indent); printf("    warry_use_org  = warry_use_size(rc);\n");
  print_indent(indent); printf("    warry_size_org = warry_size(rc);\n");
  print_indent(indent); printf("    org_next_id = 0;\n");
  print_indent(indent); printf("    tr_instr_commit_ready(rc, rule, TR_GSID(%d), %d, &ptmp_global_root, &v, &org_next_id);\n", (int)targ0, (int)targ1);
    {
      char *buf_always = automalloc_sprintf("goto label_always_%p", op_address);
      instr = translate_instructions(instr, jump_points, header, buf_always, buf_always, indent+1);
      free(buf_always);
      /* 変換中についでにルール名も設定 */
      set_translating_rule_name(targ0);
    }
  print_indent(indent); printf("  label_always_%p:\n", op_address);
  print_indent(indent); printf("    if(tr_instr_commit_finish(rc, rule, TR_GSID(%d), %d, &ptmp_global_root, &v, warry_use_org, warry_size_org))\n", (int)targ0, (int)targ1);
  print_indent(indent); printf("      %s;\n", successcode);
  print_indent(indent); printf("    else\n");
  print_indent(indent); printf("      env_set_next_id(org_next_id);\n");
  print_indent(indent); printf("      %s;\n", failcode);
  print_indent(indent); printf("    lmn_fatal(\"translate recursive error\\n\");\n");
  print_indent(indent); printf("  }\n");
  *finishflag = 0;
  return instr;
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
  print_indent(indent); printf("    {\n");
  print_indent(indent); printf("      AtomListEntryRef atomlist_ent = lmn_mem_get_atomlist((LmnMembraneRef)wt(rc, %d), TR_GFID(%d));\n", (int)targ1, targ2.functor_data);
  print_indent(indent); printf("      LmnSAtom atom;\n");
  print_indent(indent); printf("      if (atomlist_ent) {\n");
  print_indent(indent); printf("        at_set(rc, %d, LMN_ATTR_MAKE_LINK(0));\n", (int)targ0);
  print_indent(indent); printf("        /* EACH_ATOMを使うとループ内コード中でコンマが使えない場合が出てくる */\n");
  print_indent(indent); printf("        for(atom = atomlist_head(atomlist_ent);\n");
  print_indent(indent); printf("            atom != lmn_atomlist_end(atomlist_ent);\n");
  print_indent(indent); printf("            atom = LMN_SATOM_GET_NEXT_RAW(atom)){\n");
  print_indent(indent); printf("          if(LMN_SATOM_GET_FUNCTOR(atom) != LMN_RESUME_FUNCTOR){\n");
  print_indent(indent); printf("            wt_set(rc, %d, (LmnWord)atom);\n", (int)targ0);
  print_indent(indent); printf("            tt_set(rc, %d, TT_ATOM);\n", (int)targ0);
            {
              char *buf_fail = automalloc_sprintf("goto label_fail_%p", op_address);
              instr = translate_instructions(instr, jump_points, header, successcode, buf_fail, indent+1);
              free(buf_fail);
            }
  print_indent(indent); printf("          }\n");
  print_indent(indent); printf("        label_fail_%p:\n", op_address);
  print_indent(indent); printf("          ; /* PROFILEでない場合に必要 */\n");
  print_indent(indent); printf(" #ifdef PROFILE\n");
  print_indent(indent); printf("          if (lmn_env.profile_level >= 2) {\n");
  print_indent(indent); printf("          }\n");
  print_indent(indent); printf(" #endif\n");
  print_indent(indent); printf("        }\n");
  print_indent(indent); printf("      }\n");
  print_indent(indent); printf("    }\n");
  print_indent(indent); printf("    %s;\n", failcode);
  }
  *finishflag = 0;
  return instr;
}
case INSTR_LOCKMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  lmn_interned_str targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(lmn_interned_str, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, (LmnWord)LMN_PROXY_GET_MEM(wt(rc, %d)), 0, TT_MEM);\n", (int)targ0, (int)targ1);
  print_indent(indent); printf("  if(LMN_MEM_NAME_ID((LmnMembraneRef)wt(rc, %d)) != TR_GSID(%d)) %s;\n", (int)targ0, (int)targ2, failcode);
  return instr;
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
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    LmnMembraneRef mp = lmn_mem_child_head((LmnMembraneRef)wt(rc, %d));\n", (int)targ1);
  print_indent(indent); printf("    for (; mp; mp=lmn_mem_next(mp)) {\n");
  print_indent(indent); printf("      warry_set(rc, %d, (LmnWord)mp, 0, TT_MEM);\n", (int)targ0);
  print_indent(indent); printf("      if (LMN_MEM_NAME_ID(mp) == TR_GSID(%d)){\n", (int)targ3);
        {
          char *buf_fail = automalloc_sprintf("goto label_fail_%p", op_address);
          instr = translate_instructions(instr, jump_points, header, successcode, buf_fail, indent+1);
          free(buf_fail);
        }
  print_indent(indent); printf("      }\n");
  print_indent(indent); printf("    label_fail_%p:\n", op_address);
  print_indent(indent); printf("      ; /* PROFILEでない場合に必要 */\n");
  print_indent(indent); printf(" #ifdef PROFILE\n");
  print_indent(indent); printf("      if (lmn_env.profile_level >= 2) {\n");
  print_indent(indent); printf("      }\n");
  print_indent(indent); printf(" #endif\n");
  print_indent(indent); printf("    }\n");
  print_indent(indent); printf("    %s;\n", failcode);
  print_indent(indent); printf("  }\n");
  *finishflag = 0;
  return instr;
}
case INSTR_NMEMS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if (!lmn_mem_nmems((LmnMembraneRef)wt(rc, %d), %d)) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_NORULES:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  if (vec_num(lmn_mem_get_rulesets((LmnMembraneRef)wt(rc, %d)))) %s;\n", (int)targ0, failcode);
  return instr;
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
  print_indent(indent); printf("    wt_set(rc, %d, %ld);\n", (int)targ0, targ2.long_data);
    break;
  case LMN_DBL_ATTR:
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    LmnAtom d;\n");
  print_indent(indent); printf("    d = lmn_create_double_atom(%lf);\n", targ2.double_data);
  print_indent(indent); printf("    wt_set(rc, %d, d);\n", (int)targ0);
  print_indent(indent); printf("   }\n");
    break;
  case LMN_STRING_ATTR:
  print_indent(indent); printf("    wt_set(rc, %d, (LmnWord)lmn_string_make(lmn_id_to_name(TR_GSID(%d))));\n", (int)targ0, targ2.string_data);
    break;
  default:
  print_indent(indent); printf("    wt_set(rc, %d, LMN_ATOM(lmn_new_atom(TR_GFID(%d))));\n", (int)targ0, targ2.functor_data);
    break;
  }
  print_indent(indent); printf("  at_set(rc, %d, %d);\n", (int)targ0, targ2_attr);
  print_indent(indent); printf("  tt_set(rc, %d, TT_ATOM);\n", (int)targ0);
  print_indent(indent); printf("  lmn_mem_push_atom((LmnMembraneRef)wt(rc, %d), wt(rc, %d), %d);\n", (int)targ1, (int)targ0, targ2_attr);
  return instr;
}
case INSTR_NATOMS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if (!lmn_mem_natoms((LmnMembraneRef)wt(rc, %d), %d)) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_NATOMSINDIRECT:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if (!lmn_mem_natoms((LmnMembraneRef)wt(rc, %d), wt(rc, %d))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_ALLOCLINK:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  TR_INSTR_ALLOCLINK(rc, %d, %d, %d);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_UNIFYLINKS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  TR_INSTR_UNIFYLINKS(rc, %d, %d, %d);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
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
  print_indent(indent); printf("  lmn_mem_newlink((LmnMembraneRef)wt(rc, %d), wt(rc, %d), at(rc, %d), %d, wt(rc, %d), at(rc, %d), %d);\n", (int)targ4, (int)targ0, (int)targ0, (int)targ1, (int)targ2, (int)targ2, (int)targ3);
  return instr;
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
  print_indent(indent); printf("  TR_INSTR_RELINK(rc, %d, %d, %d, %d, %d);\n", (int)targ0, (int)targ1, (int)targ2, (int)targ3, (int)targ4);
  return instr;
}
case INSTR_GETLINK:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, LMN_SATOM_GET_LINK(wt(rc, %d), %d), LMN_SATOM_GET_ATTR(wt(rc, %d), %d), TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2, (int)targ1, (int)targ2);
  return instr;
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
  print_indent(indent); printf("  lmn_mem_unify_atom_args((LmnMembraneRef)wt(rc, %d), LMN_SATOM(wt(rc, %d)), %d, LMN_SATOM(wt(rc, %d)), %d);\n", (int)targ4, (int)targ0, (int)targ1, (int)targ2, (int)targ3);
  return instr;
}
case INSTR_PROCEED:{
  print_indent(indent); printf("  %s;\n", successcode);
  *finishflag = 0;
  return instr;
}
case INSTR_STOP:{
  print_indent(indent); printf("  %s;\n", failcode);
  *finishflag = 0;
  return instr;
}
case INSTR_NOT:{
  LmnSubInstrSize targ0;
  READ_VAL(LmnSubInstrSize, instr, targ0);
  print_indent(indent); printf("  { /* not */\n");
    {
      char *buf_success = automalloc_sprintf("goto label_success_%p", op_address);
      char *buf_fail = automalloc_sprintf("goto label_fail_%p", op_address);
      const BYTE *next = translate_instructions(instr, jump_points, header, buf_success, buf_fail, indent+1);
      LMN_ASSERT(next == instr+targ0);
      instr = next;
    }
  print_indent(indent); printf("  label_success_%p: /* not */\n", op_address);
  print_indent(indent); printf("    %s;\n", failcode);
  print_indent(indent); printf("  label_fail_%p: /* not */\n", op_address);
  print_indent(indent); printf("    ;\n");
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_ENQUEUEATOM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  return instr;
}
case INSTR_DEQUEUEATOM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  return instr;
}
case INSTR_NEWMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    LmnMembraneRef mp = lmn_mem_make();\n");
  print_indent(indent); printf("    lmn_mem_add_child_mem((LmnMembraneRef)wt(rc, %d), mp);\n", (int)targ1);
  print_indent(indent); printf("    wt_set(rc, %d, (LmnWord)mp);\n", (int)targ0);
  print_indent(indent); printf("    tt_set(rc, %d, TT_MEM);\n", (int)targ0);
  print_indent(indent); printf("    lmn_mem_set_active(mp, TRUE);\n");
  print_indent(indent); printf("    if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {\n");
  print_indent(indent); printf("      lmn_memstack_push(RC_MEMSTACK(rc), mp);\n");
  print_indent(indent); printf("    }\n");
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_ALLOCMEM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  wt_set(rc, %d, (LmnWord)lmn_mem_make());\n", (int)targ0);
  print_indent(indent); printf("  tt_set(rc, %d, TT_MEM);\n", (int)targ0);
  return instr;
}
case INSTR_REMOVEATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  lmn_mem_remove_atom((LmnMembraneRef)wt(rc, %d), wt(rc, %d), at(rc, %d));\n", (int)targ1, (int)targ0, (int)targ0);
  return instr;
}
case INSTR_FREEATOM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  lmn_free_atom(wt(rc, %d), at(rc, %d));\n", (int)targ0, (int)targ0);
  return instr;
}
case INSTR_REMOVEMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  lmn_mem_remove_mem((LmnMembraneRef)wt(rc, %d), (LmnMembraneRef)wt(rc, %d));\n", (int)targ1, (int)targ0);
  return instr;
}
case INSTR_FREEMEM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  lmn_mem_free((LmnMembraneRef)wt(rc, %d));\n", (int)targ0);
  return instr;
}
case INSTR_ADDMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  lmn_mem_add_child_mem((LmnMembraneRef)wt(rc, %d), (LmnMembraneRef)wt(rc, %d));\n", (int)targ0, (int)targ1);
  return instr;
}
case INSTR_ENQUEUEMEM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  if (RC_GET_MODE(rc, REACT_MEM_ORIENTED)) {\n");
  print_indent(indent); printf("    lmn_memstack_push(RC_MEMSTACK(rc), (LmnMembraneRef)wt(rc, %d)); /* 通常実行時 */\n", (int)targ0);
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_UNLOCKMEM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  return instr;
}
case INSTR_LOADRULESET:{
  LmnInstrVar targ0;
  LmnRulesetId targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnRulesetId, instr, targ1);
  print_indent(indent); printf("  lmn_mem_add_ruleset((LmnMembraneRef)wt(rc, %d), lmn_ruleset_from_id(TR_GRID(%d)));\n", (int)targ0, (int)targ1);
  return instr;
}
case INSTR_LOADMODULE:{
  LmnInstrVar targ0;
  lmn_interned_str targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(lmn_interned_str, instr, targ1);
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    LmnRuleSetRef ruleset;\n");
  print_indent(indent); printf("    if ((ruleset = lmn_get_module_ruleset(TR_GSID(%d)))) {\n", (int)targ1);
  print_indent(indent); printf("      lmn_mem_add_ruleset((LmnMembraneRef)wt(rc, %d), ruleset);\n", (int)targ0);
  print_indent(indent); printf("    } else {\n");
  print_indent(indent); printf("      fprintf(stderr, \"Undefined module %%s\\n\", lmn_id_to_name(TR_GSID(%d)));\n", (int)targ1);
  print_indent(indent); printf("    }\n");
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_RECURSIVELOCK:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  return instr;
}
case INSTR_RECURSIVEUNLOCK:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  return instr;
}
case INSTR_DEREFATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, (LmnWord)LMN_SATOM(LMN_SATOM_GET_LINK(wt(rc, %d), %d)), LMN_SATOM_GET_ATTR(wt(rc, %d), %d), TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2, (int)targ1, (int)targ2);
  return instr;
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
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    LmnByte attr = LMN_SATOM_GET_ATTR(wt(rc, %d), %d);\n", (int)targ1, (int)targ2);
  print_indent(indent); printf("    if (LMN_ATTR_IS_DATA(attr)) {\n");
  print_indent(indent); printf("      if (%d != 0) %s;\n", (int)targ3, failcode);
  print_indent(indent); printf("    } else {\n");
  print_indent(indent); printf("      if (attr != %d) %s;\n", (int)targ3, failcode);
  print_indent(indent); printf("    }\n");
  print_indent(indent); printf("    warry_set(rc, %d, LMN_SATOM_GET_LINK(wt(rc, %d), %d), attr, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_FUNC:{
  LmnInstrVar targ0;
  LmnLinkAttr targ1_attr;
  union LmnFunctorLiteral targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL_FUNC(instr, targ1);
  if (LMN_ATTR_IS_DATA(targ1_attr)) {
  print_indent(indent); printf("    if (LMN_ATTR_IS_DATA(at(rc, %d)) && at(rc, %d) == %d) {\n", (int)targ0, (int)targ0, targ1_attr);
  print_indent(indent); printf("      tt_set(rc, %d, TT_ATOM);\n", (int)targ0);
      switch(targ1_attr) {
      case LMN_INT_ATTR:
  print_indent(indent); printf("        if (wt(rc, %d) != %ld) %s;\n", (int)targ0, targ1.long_data, failcode);
        break;
      case LMN_DBL_ATTR:
  print_indent(indent); printf("        if (lmn_get_double(wt(rc, %d)) != %lf) %s;\n", (int)targ0, targ1.double_data, failcode);
        break;
      case LMN_STRING_ATTR: {
  print_indent(indent); printf("        LmnStringRef s = lmn_string_make(lmn_id_to_name(TR_GSID(%d)));\n", targ1.string_data);
  print_indent(indent); printf("        if(! lmn_string_eq(s, (LmnStringRef)wt(rc, %d))) %s;\n", (int)targ0, failcode);
  print_indent(indent); printf("        lmn_string_free(s);\n");
        fprintf(stderr, "string attr is not implemented.");
        break;
      }
      default:
        lmn_fatal("implementation error");
      }
  print_indent(indent); printf("    } else {\n");
  print_indent(indent); printf("      %s;\n", failcode);
  print_indent(indent); printf("    }\n");
  } else {
  print_indent(indent); printf("    if(LMN_ATTR_IS_DATA(at(rc, %d)) ||\n", (int)targ0);
  print_indent(indent); printf("       LMN_SATOM_GET_FUNCTOR(LMN_SATOM(wt(rc, %d))) != TR_GFID(%d)) %s;\n", (int)targ0, targ1.functor_data, failcode);
  }
  return instr;
}
case INSTR_NOTFUNC:{
  LmnInstrVar targ0;
  LmnLinkAttr targ1_attr;
  union LmnFunctorLiteral targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL_FUNC(instr, targ1);
  if (LMN_ATTR_IS_DATA(targ1_attr)) {
  print_indent(indent); printf("    if(! (LMN_ATTR_IS_DATA(at(rc, %d)) && at(rc, %d) == %d)){\n", (int)targ0, (int)targ0, targ1_attr);
  print_indent(indent); printf("      tt_set(rc, %d, TT_ATOM);\n", (int)targ0);
      switch(targ1_attr){
      case LMN_INT_ATTR:
  print_indent(indent); printf("        if(wt(rc, %d) == %ld) %s;\n", (int)targ0, targ1.long_data, failcode);
        break;
      case LMN_DBL_ATTR:
  print_indent(indent); printf("        if(lmn_get_double(wt(rc, %d)) == %lf) %s;\n", (int)targ0, targ1.double_data, failcode);
        fprintf(stderr, "double attr is not implemented.");
        break;
      case LMN_STRING_ATTR: {
  print_indent(indent); printf("        LmnStringRef s = lmn_string_make(lmn_id_to_name(TR_GSID(%d)));\n", targ1.string_data);
  print_indent(indent); printf("        if(lmn_string_eq(s, (LmnStringRef)wt(rc, %d))) %s;\n", (int)targ0, failcode);
  print_indent(indent); printf("        lmn_string_free(s);\n");
        fprintf(stderr, "string attr is not implemented.");
        break;
      }
      default:
        lmn_fatal("implementation error");
      }
  print_indent(indent); printf("    }\n");
  } else {
  print_indent(indent); printf("    if(! (LMN_ATTR_IS_DATA(at(rc, %d)) ||\n", (int)targ0);
  print_indent(indent); printf("          LMN_SATOM_GET_FUNCTOR(LMN_SATOM(wt(rc, %d))) != TR_GFID(%d))) %s;\n", (int)targ0, targ1.functor_data, failcode);
  }
  return instr;
}
case INSTR_ISGROUND:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    Vector *srcvec;\n");
  print_indent(indent); printf("    Vector *avovec;\n");
  print_indent(indent); printf("    unsigned long natoms;\n");
  print_indent(indent); printf("    BOOL b;\n");
  print_indent(indent); printf("    avovec = links_from_idxs((Vector *)wt(rc, %d), rc_warry(rc));\n", (int)targ2);
  print_indent(indent); printf("    srcvec = links_from_idxs((Vector *)wt(rc, %d), rc_warry(rc));\n", (int)targ1);
  print_indent(indent); printf("    b = lmn_mem_is_ground(srcvec, avovec, &natoms);\n");
  print_indent(indent); printf("    free_links(srcvec);\n");
  print_indent(indent); printf("    free_links(avovec);\n");
  print_indent(indent); printf("    if(! b) %s;\n", failcode);
  print_indent(indent); printf("    warry_set(rc, %d, natoms, LMN_INT_ATTR, TT_OTHER);\n", (int)targ0);
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_ISUNARY:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  if (LMN_ATTR_IS_DATA(at(rc, %d))) {\n", (int)targ0);
  print_indent(indent); printf("    switch (at(rc, %d)) {\n", (int)targ0);
  print_indent(indent); printf("    case LMN_SP_ATOM_ATTR:\n");
  print_indent(indent); printf("      if (!SP_ATOM_IS_GROUND(wt(rc, %d))) %s;\n", (int)targ0, failcode);
  print_indent(indent); printf("      break;\n");
  print_indent(indent); printf("    default:\n");
  print_indent(indent); printf("      break;\n");
  print_indent(indent); printf("    }\n");
  print_indent(indent); printf("  } else if (LMN_SATOM_GET_ARITY(wt(rc, %d)) != 1){\n", (int)targ0);
  print_indent(indent); printf("    %s;\n", failcode);
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_ISINT:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  if(at(rc, %d) != LMN_INT_ATTR) %s;\n", (int)targ0, failcode);
  return instr;
}
case INSTR_ISFLOAT:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  if(at(rc, %d) != LMN_DBL_ATTR) %s;\n", (int)targ0, failcode);
  return instr;
}
case INSTR_ISSTRING:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  if(! lmn_is_string(wt(rc, %d), at(rc, %d))) %s;\n", (int)targ0, (int)targ0, failcode);
  return instr;
}
case INSTR_ISINTFUNC:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  if(at(rc, %d) != LMN_INT_ATTR) %s;\n", (int)targ0, failcode);
  return instr;
}
case INSTR_ISFLOATFUNC:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  if(at(rc, %d) != LMN_DBL_ATTR) %s;\n", (int)targ0, failcode);
  return instr;
}
case INSTR_COPYATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, lmn_copy_atom(wt(rc, %d), at(rc, %d)), at(rc, %d), TT_ATOM);\n", (int)targ0, (int)targ2, (int)targ2, (int)targ2);
  print_indent(indent); printf("  lmn_mem_push_atom((LmnMembraneRef)wt(rc, %d), wt(rc, %d), at(rc, %d));\n", (int)targ1, (int)targ0, (int)targ0);
  return instr;
}
case INSTR_EQATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if (LMN_ATTR_IS_DATA(at(rc, %d)) ||\n", (int)targ0);
  print_indent(indent); printf("      LMN_ATTR_IS_DATA(at(rc, %d)) ||\n", (int)targ1);
  print_indent(indent); printf("      LMN_SATOM(wt(rc, %d)) != LMN_SATOM(wt(rc, %d))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_NEQATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if (!(LMN_ATTR_IS_DATA(at(rc, %d)) ||\n", (int)targ0);
  print_indent(indent); printf("        LMN_ATTR_IS_DATA(at(rc, %d)) ||\n", (int)targ1);
  print_indent(indent); printf("        LMN_SATOM(wt(rc, %d)) != LMN_SATOM(wt(rc, %d)))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_EQMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(wt(rc, %d) != wt(rc, %d)) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_NEQMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(wt(rc, %d) == wt(rc, %d)) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_NEWLIST:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    Vector *listvec = vec_make(16);\n");
  print_indent(indent); printf("    warry_set(rc, %d, (LmnWord)listvec, 0, TT_OTHER);\n", (int)targ0);
    {
      char *buf_success = automalloc_sprintf("goto label_success_%p", op_address);
      char *buf_fail = automalloc_sprintf("goto label_fail_%p", op_address);
      instr = translate_instructions(instr, jump_points, header, buf_success, buf_fail, indent+1);
      free(buf_success);
      free(buf_fail);
    }
  print_indent(indent); printf("  label_success_%p:\n", op_address);
  print_indent(indent); printf("    vec_free(listvec);\n");
  print_indent(indent); printf("    %s;\n", successcode);
  print_indent(indent); printf("    lmn_fatal(\"translate recursive error\\n\");\n");
  print_indent(indent); printf("  label_fail_%p:\n", op_address);
  print_indent(indent); printf("    vec_free(listvec);\n");
  print_indent(indent); printf("    %s;\n", failcode);
  print_indent(indent); printf("    lmn_fatal(\"translate recursive error\\n\");\n");
  print_indent(indent); printf("  }\n");
  *finishflag = 0;
  return instr;
}
case INSTR_ADDTOLIST:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  vec_push((Vector *)wt(rc, %d), %d);\n", (int)targ0, (int)targ1);
  return instr;
}
case INSTR_GETFROMLIST:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  switch (at(rc, %d)) {\n", (int)targ1);
  print_indent(indent); printf("    case LIST_AND_MAP:\n");
  print_indent(indent); printf("      wt_set(rc, %d, vec_get((Vector *)wt(rc, %d), (unsigned int)%d));\n", (int)targ0, (int)targ1, (int)targ2);
  print_indent(indent); printf("      tt_set(rc, %d, TT_OTHER);\n", (int)targ0);
  print_indent(indent); printf("      if (%d == 0){\n", (int)targ2);
  print_indent(indent); printf("        at_set(rc, %d, LINK_LIST);\n", (int)targ0);
  print_indent(indent); printf("      }else if (%d == 1){\n", (int)targ2);
  print_indent(indent); printf("        at_set(rc, %d, MAP);\n", (int)targ0);
  print_indent(indent); printf("      }else{\n");
  print_indent(indent); printf("        lmn_fatal(\"unexpected attribute @instr_getfromlist\");\n");
  print_indent(indent); printf("      }\n");
  print_indent(indent); printf("      break;\n");
  print_indent(indent); printf("    case LINK_LIST: /* LinkObjをfreeするのはここ？ */\n");
  print_indent(indent); printf("    {\n");
  print_indent(indent); printf("      LinkObjRef lo = (LinkObjRef)vec_get((Vector *)wt(rc, %d), (unsigned int)%d);\n", (int)targ1, (int)targ2);
  print_indent(indent); printf("      warry_set(rc, %d, LinkObjGetAtom(lo), LinkObjGetPos(lo), TT_ATOM);\n", (int)targ0);
  print_indent(indent); printf("      break;\n");
  print_indent(indent); printf("    }\n");
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_EQGROUND:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    Vector *srcvec = links_from_idxs((Vector*)wt(rc, %d), rc_warry(rc));\n", (int)targ0);
  print_indent(indent); printf("    Vector *dstvec = links_from_idxs((Vector*)wt(rc, %d), rc_warry(rc));\n", (int)targ1);
  print_indent(indent); printf("    BOOL same = lmn_mem_cmp_ground(srcvec, dstvec);\n");
  print_indent(indent); printf("    free_links(srcvec);\n");
  print_indent(indent); printf("    free_links(dstvec);\n");
  print_indent(indent); printf("    if(! same) %s;\n", failcode);
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_NEQGROUND:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    Vector *srcvec = links_from_idxs((Vector*)wt(rc, %d), rc_warry(rc));\n", (int)targ0);
  print_indent(indent); printf("    Vector *dstvec = links_from_idxs((Vector*)wt(rc, %d), rc_warry(rc));\n", (int)targ1);
  print_indent(indent); printf("    BOOL same = lmn_mem_cmp_ground(srcvec, dstvec);\n");
  print_indent(indent); printf("    free_links(srcvec);\n");
  print_indent(indent); printf("    free_links(dstvec);\n");
  print_indent(indent); printf("    if(same) %s;\n", failcode);
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_COPYGROUND:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    Vector *srcvec = links_from_idxs((Vector*)wt(rc, %d), rc_warry(rc));\n", (int)targ1);
  print_indent(indent); printf("    Vector *dstlovec, *retvec;\n");
  print_indent(indent); printf("    ProcessTableRef atommap;\n");
  print_indent(indent); printf("    lmn_mem_copy_ground((LmnMembraneRef)wt(rc, %d), srcvec, &dstlovec, &atommap);\n", (int)targ2);
  print_indent(indent); printf("    free_links(srcvec);\n");
  print_indent(indent); printf("    /* 返り値の作成 */\n");
  print_indent(indent); printf("    retvec = vec_make(2);\n");
  print_indent(indent); printf("    vec_push(retvec, (LmnWord)dstlovec);\n");
  print_indent(indent); printf("    vec_push(retvec, (LmnWord)atommap);\n");
  print_indent(indent); printf("    warry_set(rc, %d, (LmnWord)retvec, LIST_AND_MAP, TT_OTHER);\n", (int)targ0);
    {
      char *buf_always = automalloc_sprintf("goto label_always_%p", op_address);
      instr = translate_instructions(instr, jump_points, header, buf_always, buf_always, indent+1);
      free(buf_always);
    }
  print_indent(indent); printf("  label_always_%p:\n", op_address);
  print_indent(indent); printf("    free_links(dstlovec);\n");
  print_indent(indent); printf("    vec_free(retvec);\n");
  print_indent(indent); printf("    %s;\n", successcode);
  print_indent(indent); printf("    lmn_fatal(\"translate recursive error\\n\");\n");
  print_indent(indent); printf("  }\n");
  *finishflag = 0;
  return instr;
}
case INSTR_REMOVEGROUND:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    Vector *srcvec = links_from_idxs((Vector*)wt(rc, %d), rc_warry(rc));\n", (int)targ0);
  print_indent(indent); printf("    lmn_mem_remove_ground((LmnMembraneRef)wt(rc, %d), srcvec);\n", (int)targ1);
  print_indent(indent); printf("    free_links(srcvec);\n");
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_FREEGROUND:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  {\n");
  print_indent(indent); printf("    Vector *srcvec = links_from_idxs((Vector*)wt(rc, %d), rc_warry(rc));\n", (int)targ0);
  print_indent(indent); printf("    lmn_mem_free_ground(srcvec);\n");
  print_indent(indent); printf("    free_links(srcvec);\n");
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_STABLE:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  if (lmn_mem_is_active((LmnMembraneRef)wt(rc, %d))) %s;\n", (int)targ0, failcode);
  return instr;
}
case INSTR_IADD:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, (long)wt(rc, %d) + (long)wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_ISUB:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, (long)wt(rc, %d) - (long)wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_IMUL:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, (long)wt(rc, %d) * (long)wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_IDIV:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, (long)wt(rc, %d) / (long)wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_INEG:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  warry_set(rc, %d, -(long)wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1);
  return instr;
}
case INSTR_IMOD:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, (long)wt(rc, %d) %% (long)wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_INOT:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, ~(long)wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1);
  return instr;
}
case INSTR_IAND:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, (long)wt(rc, %d) & (long)wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_IOR:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, (long)wt(rc, %d) | (long)wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_IXOR:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, (long)wt(rc, %d) ^ (long)wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_ILT:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!((long)wt(rc, %d) < (long)wt(rc, %d))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_ILE:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!((long)wt(rc, %d) <= (long)wt(rc, %d))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_IGT:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!((long)wt(rc, %d) > (long)wt(rc, %d))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_IGE:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!((long)wt(rc, %d) >= (long)wt(rc, %d))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_IEQ:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!((long)wt(rc, %d) == (long)wt(rc, %d))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_INE:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!((long)wt(rc, %d) != (long)wt(rc, %d))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_ILTFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!((long)wt(rc, %d) < (long)wt(rc, %d))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_ILEFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!((long)wt(rc, %d) <= (long)wt(rc, %d))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_IGTFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!((long)wt(rc, %d) > (long)wt(rc, %d))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_IGEFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!((long)wt(rc, %d) >= (long)wt(rc, %d))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_FADD:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  LmnAtom d = lmn_create_double_atom(lmn_get_double(wt(rc, %d)) + lmn_get_double(wt(rc, %d)));\n", (int)targ1, (int)targ2);
  print_indent(indent); printf("  warry_set(rc, %d, d, LMN_DBL_ATTR, TT_ATOM);\n", (int)targ0);
  return instr;
}
case INSTR_FSUB:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  LmnAtom d = lmn_create_double_atom(lmn_get_double(wt(rc, %d)) - lmn_get_double(wt(rc, %d)));\n", (int)targ1, (int)targ2);
  print_indent(indent); printf("  warry_set(rc, %d, d, LMN_DBL_ATTR, TT_ATOM);\n", (int)targ0);
  return instr;
}
case INSTR_FMUL:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  LmnAtom d = lmn_create_double_atom(lmn_get_double(wt(rc, %d)) * lmn_get_double(wt(rc, %d)));\n", (int)targ1, (int)targ2);
  print_indent(indent); printf("  warry_set(rc, %d, d, LMN_DBL_ATTR, TT_ATOM);\n", (int)targ0);
  return instr;
}
case INSTR_FDIV:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  LmnAtom d = lmn_create_double_atom(lmn_get_double(wt(rc, %d)) / lmn_get_double(wt(rc, %d)));\n", (int)targ1, (int)targ2);
  print_indent(indent); printf("  warry_set(rc, %d, d, LMN_DBL_ATTR, TT_ATOM);\n", (int)targ0);
  return instr;
}
case INSTR_FNEG:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  LmnAtom d = lmn_create_double_atom(-lmn_get_double(wt(rc, %d)));\n", (int)targ1);
  print_indent(indent); printf("  warry_set(rc, %d, d, LMN_DBL_ATTR, TT_ATOM);\n", (int)targ0);
  return instr;
}
case INSTR_FLT:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!(lmn_get_double(wt(rc, %d)) < lmn_get_double(wt(rc, %d)))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_FLE:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!(lmn_get_double(wt(rc, %d)) <= lmn_get_double(wt(rc, %d)))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_FGT:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!(lmn_get_double(wt(rc, %d)) > lmn_get_double(wt(rc, %d)))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_FGE:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!(lmn_get_double(wt(rc, %d)) >= lmn_get_double(wt(rc, %d)))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_FEQ:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!(lmn_get_double(wt(rc, %d)) == lmn_get_double(wt(rc, %d)))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_FNE:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(!(lmn_get_double(wt(rc, %d)) != lmn_get_double(wt(rc, %d)))) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_ALLOCATOM:{
  LmnInstrVar targ0;
  LmnLinkAttr targ1_attr;
  union LmnFunctorLiteral targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL_FUNC(instr, targ1);
  switch(targ1_attr){
  case LMN_INT_ATTR:
  print_indent(indent); printf("    warry_set(rc, %d, %ld, LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, targ1.long_data);
    break;
  case LMN_DBL_ATTR:
  print_indent(indent); printf("    {\n");
  print_indent(indent); printf("      static const double d = %lf;\n", targ1.double_data);
  print_indent(indent); printf("      warry_set(rc, %d, &d, LMN_CONST_DBL_ATTR, TT_ATOM);\n", (int)targ0);
  print_indent(indent); printf("    }\n");
    break;
  case LMN_STRING_ATTR:
  print_indent(indent); printf("    warry_set(rc, %d, %d, LMN_CONST_STR_ATTR, TT_ATOM);\n", (int)targ0, targ1.string_data);
    break;
  default:
    lmn_fatal("Implementation error");
  }
  return instr;
}
case INSTR_ALLOCATOMINDIRECT:{
  LmnInstrVar targ0;
  LmnFunctor targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnFunctor, instr, targ1);
  print_indent(indent); printf("  if (LMN_ATTR_IS_DATA(at(rc, %d))) {\n", (int)targ1);
  print_indent(indent); printf("    warry_set(rc, %d, lmn_copy_data_atom(wt(rc, %d), at(rc, %d)), at(rc, %d), TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ1, (int)targ1);
  print_indent(indent); printf("  } else { /* symbol atom */\n");
  print_indent(indent); printf("    fprintf(stderr, \"symbol atom can't be created in GUARD\\n\");\n");
  print_indent(indent); printf("    exit(EXIT_FAILURE);\n");
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_SAMEFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if (!lmn_eq_func(wt(rc, %d), at(rc, %d), wt(rc, %d), at(rc, %d))) %s;\n", (int)targ0, (int)targ0, (int)targ1, (int)targ1, failcode);
  return instr;
}
case INSTR_GETFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if(LMN_ATTR_IS_DATA(at(rc, %d))){\n", (int)targ1);
  print_indent(indent); printf("    wt_set(rc, %d, wt(rc, %d));\n", (int)targ0, (int)targ1);
  print_indent(indent); printf("  }else{\n");
  print_indent(indent); printf("    wt_set(rc, %d, LMN_SATOM_GET_FUNCTOR(wt(rc, %d)));\n", (int)targ0, (int)targ1);
  print_indent(indent); printf("  }\n");
  print_indent(indent); printf("  at_set(rc, %d, at(rc, %d));\n", (int)targ0, (int)targ1);
  print_indent(indent); printf("  tt_set(rc, %d, TT_OTHER);\n", (int)targ0);
  return instr;
}
case INSTR_SETMEMNAME:{
  LmnInstrVar targ0;
  lmn_interned_str targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(lmn_interned_str, instr, targ1);
  print_indent(indent); printf("  lmn_mem_set_name((LmnMembraneRef)wt(rc, %d), TR_GSID(%d));\n", (int)targ0, (int)targ1);
  return instr;
}
case INSTR_COPYRULES:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  TR_INSTR_COPYRULES(rc, %d, %d);\n", (int)targ0, (int)targ1);
  return instr;
}
case INSTR_REMOVEPROXIES:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  lmn_mem_remove_proxies((LmnMembraneRef)wt(rc, %d));\n", (int)targ0);
  return instr;
}
case INSTR_INSERTPROXIES:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  lmn_mem_insert_proxies((LmnMembraneRef)wt(rc, %d), (LmnMembraneRef)wt(rc, %d));\n", (int)targ0, (int)targ1);
  return instr;
}
case INSTR_DELETECONNECTORS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  TR_INSTR_DELETECONNECTORS(%d, %d);\n", (int)targ0, (int)targ1);
  return instr;
}
case INSTR_REMOVETOPLEVELPROXIES:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  lmn_mem_remove_toplevel_proxies((LmnMembraneRef)wt(rc, %d));\n", (int)targ0);
  return instr;
}
case INSTR_DEREFFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  TR_INSTR_DEREFFUNC(rc, %d, %d, %d);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
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
  print_indent(indent); printf("      wt_set(rc, %d, %ld);\n", (int)targ0, targ1.long_data);
  print_indent(indent); printf("      at_set(rc, %d, LMN_INT_ATTR);\n", (int)targ0);
      break;
    case LMN_DBL_ATTR:
  print_indent(indent); printf("      {\n");
  print_indent(indent); printf("        const static double x = %lf;\n", targ1.double_data);
  print_indent(indent); printf("        wt_set(rc, %d, &x);\n", (int)targ0);
  print_indent(indent); printf("        at_set(rc, %d, LMN_CONST_DBL_ATTR);\n", (int)targ0);
  print_indent(indent); printf("      }\n");
      break;
    case LMN_STRING_ATTR:
  print_indent(indent); printf("      wt_set(rc, %d, %d);\n", (int)targ0, targ1.string_data);
  print_indent(indent); printf("      at_set(rc, %d, LMN_CONST_STR_ATTR);\n", (int)targ0);
      break;
    default:
      lmn_fatal("Implementation error");
    }
  }else{
  print_indent(indent); printf("    wt_set(rc, %d, %d);\n", (int)targ0, targ1.functor_data);
  print_indent(indent); printf("    at_set(rc, %d, %d);\n", (int)targ0, targ1_attr);
  }
  print_indent(indent); printf("  tt_set(rc, %d, TT_OTHER);\n", (int)targ0);
  return instr;
}
case INSTR_EQFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if (at(rc, %d) != at(rc, %d)) %s;\n", (int)targ0, (int)targ1, failcode);
  print_indent(indent); printf("  switch (at(rc, %d)) {\n", (int)targ0);
  print_indent(indent); printf("  case LMN_INT_ATTR:\n");
  print_indent(indent); printf("    if ((long)wt(rc, %d) != (long)wt(rc, %d)) %s;\n", (int)targ0, (int)targ1, failcode);
  print_indent(indent); printf("    break;\n");
  print_indent(indent); printf("  case LMN_DBL_ATTR:\n");
  print_indent(indent); printf("    if (lmn_get_double(wt(rc, %d)) !=\n", (int)targ0);
  print_indent(indent); printf("        lmn_get_double(wt(rc, %d))) %s;\n", (int)targ1, failcode);
  print_indent(indent); printf("    break;\n");
  print_indent(indent); printf("  default:\n");
  print_indent(indent); printf("    if (wt(rc, %d) != wt(rc, %d)) %s;\n", (int)targ0, (int)targ1, failcode);
  print_indent(indent); printf("    break;\n");
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_NEQFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if (at(rc, %d) == at(rc, %d)) {\n", (int)targ0, (int)targ1);
  print_indent(indent); printf("    switch (at(rc, %d)) {\n", (int)targ0);
  print_indent(indent); printf("    case LMN_INT_ATTR:\n");
  print_indent(indent); printf("      if ((long)wt(rc, %d) == (long)wt(rc, %d)) %s;\n", (int)targ0, (int)targ1, failcode);
  print_indent(indent); printf("      break;\n");
  print_indent(indent); printf("    case LMN_DBL_ATTR:\n");
  print_indent(indent); printf("      if (lmn_get_double(wt(rc, %d)) ==\n", (int)targ0);
  print_indent(indent); printf("          lmn_get_double(wt(rc, %d))) %s;\n", (int)targ1, failcode);
  print_indent(indent); printf("      break;\n");
  print_indent(indent); printf("    default:\n");
  print_indent(indent); printf("      if (wt(rc, %d) == wt(rc, %d)) %s;\n", (int)targ0, (int)targ1, failcode);
  print_indent(indent); printf("      break;\n");
  print_indent(indent); printf("    }\n");
  print_indent(indent); printf("  }\n");
  return instr;
}
case INSTR_ADDATOM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  lmn_mem_push_atom((LmnMembraneRef)wt(rc, %d), wt(rc, %d), at(rc, %d));\n", (int)targ0, (int)targ1, (int)targ1);
  return instr;
}
case INSTR_MOVECELLS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  lmn_mem_move_cells((LmnMembraneRef)wt(rc, %d), (LmnMembraneRef)wt(rc, %d));\n", (int)targ0, (int)targ1);
  return instr;
}
case INSTR_REMOVETEMPORARYPROXIES:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  lmn_mem_remove_temporary_proxies((LmnMembraneRef)wt(rc, %d));\n", (int)targ0);
  return instr;
}
case INSTR_NFREELINKS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if (!lmn_mem_nfreelinks((LmnMembraneRef)wt(rc, %d), %d)) %s;\n", (int)targ0, (int)targ1, failcode);
  return instr;
}
case INSTR_COPYCELLS:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  wt_set(rc, %d, (LmnWord)lmn_mem_copy_cells((LmnMembraneRef)wt(rc, %d), (LmnMembraneRef)wt(rc, %d)));\n", (int)targ0, (int)targ1, (int)targ2);
  print_indent(indent); printf("  tt_set(rc, %d, TT_OTHER);\n", (int)targ0);
  return instr;
}
case INSTR_LOOKUPLINK:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  TR_INSTR_LOOKUPLINK(rc, %d, %d, %d);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_CLEARRULES:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  vec_clear(lmn_mem_get_rulesets((LmnMembraneRef)wt(rc, %d)));\n", (int)targ0);
  return instr;
}
case INSTR_DROPMEM:{
  LmnInstrVar targ0;
  READ_VAL(LmnInstrVar, instr, targ0);
  print_indent(indent); printf("  lmn_mem_drop((LmnMembraneRef)wt(rc, %d));\n", (int)targ0);
  return instr;
}
case INSTR_TESTMEM:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  print_indent(indent); printf("  if (LMN_PROXY_GET_MEM(wt(rc, %d)) != (LmnMembraneRef)wt(rc, %d)) %s;\n", (int)targ1, (int)targ0, failcode);
  return instr;
}
case INSTR_IADDFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, wt(rc, %d) + wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_ISUBFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, wt(rc, %d) - wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_IMULFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, wt(rc, %d) * wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_IDIVFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, wt(rc, %d) / wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
case INSTR_IMODFUNC:{
  LmnInstrVar targ0;
  LmnInstrVar targ1;
  LmnInstrVar targ2;
  READ_VAL(LmnInstrVar, instr, targ0);
  READ_VAL(LmnInstrVar, instr, targ1);
  READ_VAL(LmnInstrVar, instr, targ2);
  print_indent(indent); printf("  warry_set(rc, %d, wt(rc, %d) %% wt(rc, %d), LMN_INT_ATTR, TT_ATOM);\n", (int)targ0, (int)targ1, (int)targ2);
  return instr;
}
  default:
    *finishflag = -1;
    return instr;
  }
}
