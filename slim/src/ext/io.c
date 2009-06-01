/*
 * io.c - io関連のコールバック
 */

#include <stdio.h>
#include "../lmntal_ext.h"

void init_print(void);

/* 第一引数のアトムの名前を出力する */
void print_any(LmnMembrane *mem, LmnWord a0, LmnLinkAttr t0)
{
  if (LMN_ATTR_IS_DATA(t0)) {
    switch (t0) {
    case LMN_INT_ATTR:
      printf("%d\n", (int)a0);
      break;
    case LMN_DBL_ATTR:
      printf("%f\n", *((double *)a0));
      break;
    default:
      fprintf(stderr, "PRINT.C: uhknown data type\n");
      break;
    }
  } else { /* symbol atom */
    printf("%s\n", LMN_ATOM_STR(a0));
  }

  lmn_mem_remove_atom(mem, a0, t0);
  lmn_free_atom(a0, t0);
}

/* 第一引数のアトムの名前を出力し、改行する */
void print_line(LmnMembrane *mem, LmnWord a0, LmnLinkAttr t0)
{
  print_any(mem, a0, t0);
}

void init_io(void)
{
  lmn_register_c_fun("print_any", print_line, 1);
  lmn_register_c_fun("print_line", print_line, 1);
}
