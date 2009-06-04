/*
 * io.c - io関連のコールバック
 */

#include <stdio.h>
#include "../lmntal_ext.h"
#include "../slim_header/string.h"

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

void input_line(LmnMembrane *mem,
                LmnWord a0, LmnLinkAttr t0,
                LmnWord a1, LmnLinkAttr t1)
{
  const int N = 256;
  char buf[N], *s=NULL, *p; /* sは行の文字列の先頭、pは作業用 */
  int size;

  size =  0;
  p = s;
  while (fgets(buf, N, stdin)) {
    int len = strlen(buf);
    if (s == NULL) {
      s = p = LMN_NALLOC(char, len+1);
      s[0] = '\0';
      size = len + 1;
    } else {
      s = LMN_REALLOC(char, s, size += len + 1);
    }
    strcat(p, buf);
    p += len;
    if (len < N-1) break;
  }
  
  if (ferror(stdin)) {/* error */
    LmnAtomPtr atom = lmn_new_atom(lmn_functor_intern(ANONYMOUS, lmn_intern("error"), 1));
    lmn_mem_newlink(mem,
                    a0, t0, LMN_ATTR_GET_VALUE(t0),
                    (LmnWord)atom, LMN_ATTR_MAKE_LINK(0), 0);
    mem_push_symbol_atom(mem, atom);
    if (s) LMN_FREE(s);
  }
  else if (feof(stdin) && s == NULL) { /* eof */
    LmnAtomPtr atom = lmn_new_atom(lmn_functor_intern(ANONYMOUS, lmn_intern("eof"), 1));
    lmn_mem_newlink(mem,
                    a0, t0, LMN_ATTR_GET_VALUE(t0),
                    (LmnWord)atom, LMN_ATTR_MAKE_LINK(0), 0);
    mem_push_symbol_atom(mem, atom);
  }
  else {
    LmnWord a;
    
    if (*(p-2) == '\n' || *(p-2)=='\r') p -= 2;
    else if (*(p-1) == '\n' || *(p-1)=='\r') p -= 1;
    *p = '\0';

    a = lmn_string_make(s);
    lmn_mem_push_atom(mem, a, LMN_STRING_ATTR);

    lmn_mem_newlink(mem,
                    a0, t0, LMN_ATTR_GET_VALUE(t0),
                    a, LMN_STRING_ATTR, 0);
  }
}

void init_io(void)
{
  lmn_register_c_fun("print_any", print_any, 1);
  lmn_register_c_fun("print_line", print_line, 1);
  lmn_register_c_fun("input_line", input_line, 1);
}
