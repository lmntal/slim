/*
 * io.c - io関連のコールバック
 */

#include <stdio.h>
#include "../lmntal_ext.h"
#include "../slim_header/port.h"
#include "../slim_header/string.h"

void init_print(void);

/* 改行を出力する
 * +a0     : ポート
 * -a1     : 返すポート
 */
void cb_print_newline_with_port(LmnMembrane *mem,
                                LmnWord a0, LmnLinkAttr t0,
                                LmnWord a1, LmnLinkAttr t1)
{
  port_put_raw_s(LMN_PORT(a0), "\n");
  
  lmn_mem_newlink(mem,
                  a1, t1, LMN_ATTR_GET_VALUE(t1),
                  a0, t0, 0);
}

/* 文字列a1を出力し、次に改行を出力する
 * +a0     : ポート
 * +a1     : 文字列
 * -a2     : 返すポート
 */
void cb_print_line_with_port(LmnMembrane *mem,
                             LmnWord a0, LmnLinkAttr t0,
                             LmnWord a1, LmnLinkAttr t1,
                             LmnWord a2, LmnLinkAttr t2)
{
  port_puts(LMN_PORT(a0), LMN_STRING(a1));
  port_put_raw_s(LMN_PORT(a0), "\n");
  
  lmn_mem_newlink(mem,
                  a2, t2, LMN_ATTR_GET_VALUE(t2),
                  a0, t0, 0);
  lmn_mem_remove_atom(mem, a1, t1);
  lmn_free_atom(a1, t1);
  
}


/* ポートa0から一行読み込む
 * +a0     : ポート
 * -a1     : 返すポート
 * -a2     : 読み込んだ文字列 | eof()
 */
void cb_input_line_with_oprt(LmnMembrane *mem,
                             LmnWord a0, LmnLinkAttr t0,
                             LmnWord a1, LmnLinkAttr t1,
                             LmnWord a2, LmnLinkAttr t2)
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
    LmnString a;
    
    if (*(p-2) == '\n' || *(p-2)=='\r') p -= 2;
    else if (*(p-1) == '\n' || *(p-1)=='\r') p -= 1;
    *p = '\0';

    a = lmn_string_make(s);
    lmn_mem_push_atom(mem, (LmnWord)a, LMN_STRING_ATTR);

    lmn_mem_newlink(mem,
                    a0, t0, LMN_ATTR_GET_VALUE(t0),
                    (LmnWord)a, LMN_STRING_ATTR, 0);
  }
}

void init_io(void)
{
  lmn_register_c_fun("cb_print_newline_with_port", cb_print_newline_with_port, 2);
  lmn_register_c_fun("cb_print_line_with_port", cb_print_line_with_port, 3);
  lmn_register_c_fun("cb_read_line", cb_input_line_with_oprt, 3);
}
