#include "lmntal.h"
#include "atom.h"
#include "membrane.h"
#include "ccallback.h"
#include "functor.h"
#include "special_atom.h"
#include "slim_header/string.h"
#include <stdio.h>

struct LmnString {
  unsigned long buf_size, len;
  char *buf; /* Cの文字列形式 */
};

#define LMN_STRING(obj) ((struct LmnString *)obj)
#define LMN_STRING_LEN(obj) (LMN_STRING(obj)->len)
#define LMN_STRING_BUF(obj) (LMN_STRING(obj)->buf)
#define LMN_STRING_BUF_SIZE(obj) (LMN_STRING(obj)->buf_size)


#define LINK_STR(MEM, TO_ATOM, TO_ATTR, STR_ATOM)    \
  lmn_mem_newlink(MEM,                                                  \
                  TO_ATOM, TO_ATTR, LMN_ATTR_GET_VALUE(TO_ATTR), \
                  STR_ATOM, LMN_SP_ATOM_ATTR, 0)
  


int string_atom_type;

BOOL lmn_is_string(LmnWord atom, LmnLinkAttr attr)
{
  return
    attr == LMN_SP_ATOM_ATTR &&
    LMN_SP_ATOM_TYPE(atom) == string_atom_type;
}

/* 文字列をCの文字列の形式で返す */
const char* lmn_string_c_str(LmnWord atom)
{
  return (const char *)LMN_STRING_BUF(LMN_SP_ATOM_DATA(atom));
}

char *int_to_str(int n)
{
  char *s;
  int keta = 0;
  
  if (n == 0) keta = 1;
  else {
    int m = n;
    keta = 0;
    if (m < 0) { m = - m, keta = 1; }
    while (m > 0) {
      m /= 10;
      keta++;
    }
  }

  s = LMN_NALLOC(char, keta + 1);
  sprintf(s, "%d", n);

  return s;
}

LmnWord lmn_string_make(const char *s)
{
  struct LmnString *p = LMN_MALLOC(struct LmnString);
  p->len = strlen(s);
  p->buf_size = p->len + 1;
  p->buf = LMN_NALLOC(char, p->buf_size);
  strcpy(p->buf, s);
  return lmn_sp_atom_make(string_atom_type, p);
}

LmnWord lmn_string_make_empty(unsigned long buf_size)
{
  struct LmnString *p = LMN_MALLOC(struct LmnString);
  p->len = 0;
  p->buf_size = buf_size > 0 ? buf_size : 1;
  p->buf = LMN_NALLOC(char, p->buf_size);
  p->buf[0] = '\0';
  return lmn_sp_atom_make(string_atom_type, p);
}

void lmn_string_push_raw_c(LmnWord str_atom, int c)
{
  
}

LmnWord lmn_string_concat(LmnWord str_atom0, LmnWord str_atom1)
{
  LmnWord ret_atom =
    lmn_string_make_empty(LMN_STRING_LEN(LMN_SP_ATOM_DATA(str_atom0)) +
                          LMN_STRING_LEN(LMN_SP_ATOM_DATA(str_atom1)));
                                               
  char *s = LMN_NALLOC(char,
                       strlen(LMN_SP_ATOM_DATA(str_atom0)) +
                       strlen(LMN_SP_ATOM_DATA(str_atom1)) +
                       1);
  sprintf(s, "%s%s", (char *)LMN_SP_ATOM_DATA(str_atom0), (char *)LMN_SP_ATOM_DATA(str_atom1));
}

/*----------------------------------------------------------------------
 * Callbacks
 */

void cb_string_make(LmnMembrane *mem,
                    LmnWord a0, LmnLinkAttr t0,
                    LmnWord a1, LmnLinkAttr t1)
{
  char *s;
  
  if (LMN_ATTR_IS_DATA(t0)) {
    switch (t0) {
    case LMN_INT_ATTR:
      s = int_to_str(a0);
      break;
    case LMN_DBL_ATTR:
/*       s = double_to_str(a0); */
      s = strdup("not implemented");
      break;
    default:
      fprintf(stderr, "STRING.C: unexpected argument");
      s = strdup("error");
      break;
    }
  } else { /* symbol atom */
    s = strdup(LMN_ATOM_STR(a0));
  }

  LINK_STR(mem, a1, t1, lmn_sp_atom_make(string_atom_type, s));
  lmn_mem_remove_atom(mem, a0, t0);
  lmn_free_atom(a0, t0);
}

void cb_string_concat(LmnMembrane *mem,
                      LmnWord a0, LmnLinkAttr t0,
                      LmnWord a1, LmnLinkAttr t1,
                      LmnWord a2, LmnLinkAttr t2)
{
  char *s = LMN_NALLOC(char,
                       strlen(LMN_SP_ATOM_DATA(a0)) +
                       strlen(LMN_SP_ATOM_DATA(a1)) +
                       1);
  sprintf(s, "%s%s", (char *)LMN_SP_ATOM_DATA(a0), (char *)LMN_SP_ATOM_DATA(a1));

  LINK_STR(mem, a2, t2, lmn_sp_atom_make(string_atom_type, s));

  lmn_mem_remove_atom(mem, a0, t0);
  lmn_free_atom(a0, t0);
  lmn_mem_remove_atom(mem, a1, t1);
  lmn_free_atom(a1, t1);
}

void cb_string_length(LmnMembrane *mem,
                      LmnWord a0, LmnLinkAttr t0,
                      LmnWord a1, LmnLinkAttr t1)
{
  int len = strlen((char *)LMN_SP_ATOM_DATA(a0));

  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1),
                  len, LMN_INT_ATTR, 0);

  lmn_mem_remove_atom(mem, a0, t0);
  lmn_free_atom(a0, t0);
}

void cb_string_reverse(LmnMembrane *mem,
                       LmnWord a0, LmnLinkAttr t0,
                       LmnWord a1, LmnLinkAttr t1)
{
  int i, j;
  char *s = (char *)LMN_SP_ATOM_DATA(a0);
  
  for (i = 0, j = strlen(s)-1; i < j; i++, j--) {
    char t = s[i];
    s[i] = s[j];
    s[j] = t;
  }
  
  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1),
                  a0, t0, 0);
}

void cb_string_substr(LmnMembrane *mem,
                      LmnWord a0, LmnLinkAttr t0,
                      long begin, LmnLinkAttr t1,
                      long end, LmnLinkAttr t2,
                      LmnWord a3, LmnLinkAttr t3)
{
  const char *src = (const char *)LMN_SP_ATOM_DATA(a0);
  char *s;

  if (begin <= end) {
    int len = strlen(src);
    if (begin < 0) begin = 0;
    if (end > len) end = len;
    s = LMN_NALLOC(char, end - begin + 1);
    snprintf(s, end - begin + 1, "%s", src+begin);
  } else {
    s = strdup("");
  }

  LINK_STR(mem, a3, t3,
           lmn_sp_atom_make(string_atom_type, s));

  lmn_mem_remove_atom(mem, a0, t0);
  lmn_free_atom(a0, t0);
  lmn_mem_remove_atom(mem, begin, t1);
  lmn_free_atom(begin, t1);
  lmn_mem_remove_atom(mem, end, t2);
  lmn_free_atom(end, t2);
}

void cb_string_substr_right(LmnMembrane *mem,
                            LmnWord a0, LmnLinkAttr t0,
                            long begin, LmnLinkAttr t1,
                            LmnWord a2, LmnLinkAttr t2)
{
  const char *src = (const char *)LMN_SP_ATOM_DATA(a0);
  char *s;
  int len = strlen(src);

  if (begin < 0) {printf("hgoe\n"); begin = 0;}
  if (begin > len) begin = len;
  s = LMN_NALLOC(char, len - begin + 1);
  snprintf(s, len - begin + 1, "%s", src+begin);

  LINK_STR(mem, a2, t2,
           lmn_sp_atom_make(string_atom_type, s));

  lmn_mem_remove_atom(mem, a0, t0);
  lmn_free_atom(a0, t0);
  lmn_mem_remove_atom(mem, begin, t1);
  lmn_free_atom(begin, t1);
}


void *sp_cb_string_copy(void *data)
{
  return strdup((char *)data);
}

void sp_cb_string_free(void *data)
{
  LMN_FREE((char *)data);
}

void sp_cb_string_dump(void *data, FILE *stream)
{
  fprintf(stream, "\"%s\"", (char *)data);
}

BOOL sp_cb_string_is_ground(void *data)
{
  return TRUE;
}

void string_init()
{
  string_atom_type = lmn_sp_atom_register("string",
                                          sp_cb_string_copy,
                                          sp_cb_string_free,
                                          sp_cb_string_dump,
                                          sp_cb_string_is_ground);
  lmn_register_c_fun("string_make", cb_string_make, 2);
  lmn_register_c_fun("string_concat", cb_string_concat, 3);
  lmn_register_c_fun("string_length", cb_string_length, 2);
  lmn_register_c_fun("string_reverse", cb_string_reverse, 2);
  lmn_register_c_fun("string_substr", cb_string_substr, 4);
  lmn_register_c_fun("string_substr_right", cb_string_substr_right, 3);
}

void string_finalize()
{
}
