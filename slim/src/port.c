/*
 * port.c - port implementation
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
#include "membrane.h"
#include "ccallback.h"
#include "atom.h"
#include "functor.h"
#include "error.h"
#include "special_atom.h"
#include "symbol.h"
#include "slim_header/string.h"
#include "slim_header/port.h"


static int port_atom_type; /* special atom type */
static LmnFunctor eof_functor;

#define LMN_PORT_DATA(obj) (LMN_PORT(obj)->data)
#define LMN_PORT_TYPE(obj) (LMN_PORT(obj)->type)
#define LMN_PORT_OWNER(obj) (LMN_PORT(obj)->owner)
#define LMN_PORT_DIR(obj) (LMN_PORT(obj)->direction)


#define FILE_PORT_FP(obj) ((FILE *)LMN_PORT_DATA(obj))

static LmnPort port_copy_sub(LmnPort port);

/*
 * Internal Constructor.
 */
static LmnPort make_port(LmnPortDirection dir, LmnPortType type, const char *name)
{
  struct LmnPort *port = LMN_MALLOC(struct LmnPort);
  LMN_SP_ATOM_SET_TYPE(port, port_atom_type);
  port->direction = dir;
  port->type = type;
  port->closed = FALSE;
  port->error = FALSE;
  port->name = lmn_intern(name);
  port->data = NULL;
  port->owner = TRUE;
  return port;
}

void lmn_port_free(LmnPort port)
{
  if (port->owner) {
    switch (LMN_PORT_TYPE(port)) {
    case LMN_PORT_FILE:
      break;
    case LMN_PORT_OSTR:
      lmn_string_free(LMN_PORT_DATA(port));
      break;
    case LMN_PORT_ISTR:
      lmn_port_close(port);
      break;
    default:
      lmn_fatal("not implemented");
    }
  }
  LMN_FREE(port);
}

/* ownerが真の場合、生成したポートがオーナーとなる。ただし、portがオー
   ナーでなかった場合はオーナーにはならない*/
LmnPort lmn_port_copy(LmnPort port, BOOL owner)
{
  LmnPort new_port = port_copy_sub(port);
  new_port->owner = owner && LMN_PORT_OWNER(port);
  if (new_port->owner) port->owner = FALSE;
  
  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE:
    break;
  case LMN_PORT_OSTR:
    break;
  case LMN_PORT_ISTR:
    break;
  }
  return new_port;
}

static LmnPort port_copy_sub(LmnPort port)
{
  LmnPort new = LMN_MALLOC(struct LmnPort);
  memcpy(new, port, sizeof(struct LmnPort));
  return new;
}

static LmnPort lmn_stdin;
static LmnPort lmn_stdout;
static LmnPort lmn_stderr;

LmnPort lmn_stdin_port()
{
  return lmn_port_copy(lmn_stdin, FALSE);
}

LmnPort lmn_stdout_port()
{
  return lmn_port_copy(lmn_stdout, FALSE);
}

LmnPort lmn_stderr_port()
{
  return lmn_port_copy(lmn_stderr, FALSE);
}

LmnPort lmn_make_file_port(FILE *file,
                           const char *name,
                           LmnPortDirection dir,
                           BOOL owner)
{
  LmnPort port = make_port(dir, LMN_PORT_FILE, name);

  port->data = file;
  port->owner = owner;

  return port;
}

/* 入力文字列ポートを作成する。引数sの解放の責任は個の関数が持つ */
LmnPort lmn_make_input_string_port(LmnString s)
{
  LmnPort port = make_port(LMN_PORT_INPUT, LMN_PORT_ISTR, "input string port");
  struct IStrPortData *d = LMN_MALLOC(struct IStrPortData);

  d->s = s;
  d->i = 0;
  port->data = d;
  port->owner = TRUE;

  return port;
}

LmnPort lmn_make_output_string_port()
{
  LmnPort port = make_port(LMN_PORT_OUTPUT, LMN_PORT_OSTR, "output string port");

  port->data = lmn_string_make_empty();
  port->owner = TRUE;

  return port;
}

void lmn_port_close(LmnPort port)
{
  if (lmn_port_closed(port)) return;

  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE:
    fclose((FILE *)LMN_PORT_DATA(port));
    break;
  case LMN_PORT_OSTR:
    break;
  case LMN_PORT_ISTR:
    lmn_string_free(((struct IStrPortData *)LMN_PORT_DATA(port))->s);
    ((struct IStrPortData *)LMN_PORT_DATA(port))->s = NULL;
    LMN_FREE(LMN_PORT_DATA(port));
    LMN_PORT_DATA(port) = NULL;
    break;
  default:
    lmn_fatal("not implemented");
  }
  port->closed = TRUE;
}

BOOL lmn_port_closed(LmnPort port_atom)
{
  return LMN_PORT(port_atom)->closed;
}

BOOL lmn_port_error_occurred(LmnPort port_atom)
{
  return LMN_PORT(port_atom)->error;
}

LmnPortDirection lmn_port_dir(LmnPort port_atom)
{
  return LMN_PORT(port_atom)->direction;
}

lmn_interned_str lmn_port_name(LmnPort port_atom)
{
  return LMN_PORT(port_atom)->name;
}

/*----------------------------------------------------------------------
 * Read & Write
 */

/* ポートから一文字読み込み返す。もしくは、ポートの終わりに達するか、エ
   ラーが起きた場合には、EOFを返す */
int port_get_raw_c(LmnPort port)
{
  if (lmn_port_closed(port)) return EOF;
  if (LMN_PORT_DIR(port) != LMN_PORT_INPUT) return EOF;
  
  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE:
    {
      FILE *f = (FILE *)LMN_PORT_DATA(port);
      if (feof(f)) return EOF;
      return fgetc(f);
    }
    break;
  case LMN_PORT_ISTR:
    {
      struct IStrPortData *d = (struct IStrPortData *)LMN_PORT_DATA(port);
      if (d->i >= lmn_string_len(d->s)) return EOF;
      else return lmn_string_get(d->s, d->i++);
    }
    break;
  default:
    return EOF;
    break;
  }
}

/* ポートに一文字を戻す。戻した文字は次に読み込む際に読み込まれる。戻す
   文字は一文字だけしか保証されない。正常に実行された場合にはcを返し，
   エラーが起きた場合にはEOFを返す */
int port_unget_raw_c(LmnPort port, int c)
{
  if (lmn_port_closed(port)) return EOF;
  if (LMN_PORT_DIR(port) != LMN_PORT_INPUT) return EOF;

  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE:
    {
      FILE *f = (FILE *)LMN_PORT_DATA(port);
      return ungetc(c, f);
    }
    break;
  case LMN_PORT_ISTR:
    {
      struct IStrPortData *d = (struct IStrPortData *)LMN_PORT_DATA(port);
      if (d->i > 0) {
        if (lmn_string_get(d->s, d->i - 1) == c) {
          d->i--;
          return c;
        }
      }
      return EOF;
    }
    break;
  default:
    return EOF;
  }
}

/* ポートから一行読み込み、読み込んだ文字列を返す。ファイルの終わりに達
   していたり，エラーが起きた場合はNULLを返す */
LmnString port_read_line(LmnPort port)
{
  int c0, c1;
  LmnString s;
  
  if (lmn_port_closed(port)) return NULL;
  if (LMN_PORT_DIR(port) != LMN_PORT_INPUT) return NULL;

  c0 = port_get_raw_c(port);
  if (c0 == EOF) return NULL;
  s  = lmn_string_make_empty();
  for (;;) {
    if (c0 == EOF) return s;
    if (c0 == '\n') break;
    if (c0 == '\r') {
      c1 = port_get_raw_c(port);
      if (c1 == EOF || c1 == '\n') break;
      port_unget_raw_c(port, c1);
      break;
    }
    lmn_string_push_raw_c(s, c0);
    c0 = port_get_raw_c(port);
  }
  return s;
}

/* 文字はunaryアトムで表現している */
int port_putc(LmnPort port, LmnSAtom unary_atom)
{
  return port_put_raw_s(port, LMN_SATOM_STR(unary_atom));
}

/* Cの文字列をポートに出力する。エラーが起きた場合はEOFを返す。 正常に
   処理された場合は負でない数を返す*/
int port_put_raw_c(LmnPort port, int c)
{
  if (lmn_port_closed(port)) return EOF;
  if (LMN_PORT_DIR(port) != LMN_PORT_OUTPUT) return EOF;

  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE:
    {
      FILE *f = (FILE *)LMN_PORT_DATA(port);
      return fputc(c, f);
    }
    break;
  case LMN_PORT_OSTR:
    lmn_string_push_raw_c((LmnString)LMN_PORT_DATA(port), c);
    return 1;
  default:
    lmn_fatal("unexpected port type");
    break;
  }
}

/* Cの文字列をポートに出力する。エラーが起きた場合はEOFを返す。 正常に
   処理された場合は負でない数を返す*/
int port_put_raw_s(LmnPort port, const char *str)
{
  if (lmn_port_closed(port)) return EOF;
  if (LMN_PORT_DIR(port) != LMN_PORT_OUTPUT) return EOF;


  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE:
    {
      FILE *f = (FILE *)LMN_PORT_DATA(port);
      return fputs(str, f);
    }
    break;
  case LMN_PORT_OSTR:
    lmn_string_push_raw_s((LmnString)LMN_PORT_DATA(port), str);
    return 1;
  default:
    lmn_fatal("unexpected port type");
    break;
  }
}

/* 文字列をポートに出力する。エラーが起きた場合はEOFを返す。 正常に
   処理された場合は負でない数を返す*/
int port_puts(LmnPort port, LmnString str)
{
  return port_put_raw_s(port, lmn_string_c_str(str));
}

/*----------------------------------------------------------------------
 * Callbacks
 */

/*
 * ポートを閉じる。閉じたポートに対する書き込み、読み込み操作はエラーになる
 *
 * +a0: ポート
 * -a1: ポートを返す
 */
void cb_port_close(LmnMembrane *mem,
                   LmnAtom a0, LmnLinkAttr t0,
                   LmnAtom a1, LmnLinkAttr t1)
{
  LmnPort port = LMN_PORT(a0);

  lmn_port_close(port);
  lmn_mem_newlink(mem,
                  a0, t0, 0,
                  a1, t1, LMN_ATTR_GET_VALUE(t1));
}

/*
 * ポートを解放する
 *
 * +a0: ポート
 */
void cb_port_free(LmnMembrane *mem,
                  LmnAtom a0, LmnLinkAttr t0)
{
  lmn_port_free(LMN_PORT(a0));
  lmn_mem_remove_data_atom(mem, a0, t0);
}

/*
 * -a0: 標準入力ポートを返す
 */
void cb_stdin_port(LmnMembrane *mem,
                   LmnAtom a0, LmnLinkAttr t0)
{
  LmnPort atom = lmn_stdin_port();
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;

  lmn_mem_push_atom(mem, LMN_ATOM(atom), attr);
  lmn_mem_newlink(mem,
                  a0, t0, LMN_ATTR_GET_VALUE(t0),
                  LMN_ATOM(atom), attr, 0);
}

/*
 * -a0: 標準出力ポートを返す
 */
void cb_stdout_port(LmnMembrane *mem,
                    LmnAtom a0, LmnLinkAttr t0)
{
  LmnPort atom = lmn_stdout_port();
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;

  lmn_mem_push_atom(mem, LMN_ATOM(atom), attr);
  lmn_mem_newlink(mem,
                  a0, t0, LMN_ATTR_GET_VALUE(t0),
                  LMN_ATOM(atom), attr, 0);
}

/*
 * -a0: 標準エラーポートを返す
 */
void cb_stderr_port(LmnMembrane *mem,
                    LmnAtom a0, LmnLinkAttr t0)
{
  LmnPort atom = lmn_stderr_port();
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;

  lmn_mem_push_atom(mem, LMN_ATOM(atom), attr);
  lmn_mem_newlink(mem,
                  a0, t0, LMN_ATTR_GET_VALUE(t0),
                  LMN_ATOM(atom), attr, 0);
}

/*
 * +a0: ポート
 * -a1: ポートを返す
 * -a2: 文字
 */
void cb_port_getc(LmnMembrane *mem,
                  LmnAtom a0, LmnLinkAttr t0,
                  LmnAtom a1, LmnLinkAttr t1,
                  LmnAtom a2, LmnLinkAttr t2)
{
  int c;
  char buf[8];
  LmnSAtom a;

  c = port_get_raw_c(LMN_PORT(a0));
  if (c == EOF) sprintf(buf, "eof");
  else sprintf(buf, "%c", c);
  a = lmn_new_atom(lmn_functor_intern(ANONYMOUS, lmn_intern(buf), 1));
  mem_push_symbol_atom(mem, a);
  lmn_mem_newlink(mem,
                  a2, t2, LMN_ATTR_GET_VALUE(t2),
                  LMN_ATOM(a), LMN_ATTR_MAKE_LINK(0), 0);
    
  lmn_mem_newlink(mem,
                  a1, t1, LMN_ATTR_GET_VALUE(t1),
                  a0, t0, 0);
  
}

/*
 * +a0: ポート
 * +a1: unaryアトム
 * -a2: ポートを返す
 */
void cb_port_putc(LmnMembrane *mem,
                  LmnAtom a0, LmnLinkAttr t0,
                  LmnAtom a1, LmnLinkAttr t1,
                  LmnAtom a2, LmnLinkAttr t2)
{
  port_puts(LMN_PORT(a0), LMN_STRING(a1));

  lmn_mem_delete_atom(mem, a1, t1);
  lmn_mem_newlink(mem,
                  a2, t2, LMN_ATTR_GET_VALUE(t2),
                  a0, t0, 0);
  
}

/*
 * a0: ポート
 * a1: 文字列
 * a2: ポートを返す
 */
void cb_port_puts(LmnMembrane *mem,
                  LmnAtom a0, LmnLinkAttr t0,
                  LmnAtom a1, LmnLinkAttr t1,
                  LmnAtom a2, LmnLinkAttr t2)
{
  port_puts(LMN_PORT(a0), LMN_STRING(a1));

  lmn_mem_delete_atom(mem, a1, t1);
  lmn_mem_newlink(mem,
                  a2, t2, LMN_ATTR_GET_VALUE(t2),
                  a0, t0, 0);
  
}

/*
 * +a0: ポート
 * -a1: ポートを返す
 * -a2: 文字列
 */
void cb_port_read_line(LmnMembrane *mem,
                       LmnAtom a0, LmnLinkAttr t0,
                       LmnAtom a1, LmnLinkAttr t1,
                       LmnAtom a2, LmnLinkAttr t2)
{
  LmnString s = port_read_line(LMN_PORT(a0));

  if (s != NULL) {
    lmn_mem_push_atom(mem, LMN_ATOM(s), LMN_SP_ATOM_ATTR);
    lmn_mem_newlink(mem,
                    a2, t2, LMN_ATTR_GET_VALUE(t2),
                    LMN_ATOM(s), LMN_SP_ATOM_ATTR, 0);
  } else {
    LmnSAtom eof = lmn_new_atom(eof_functor);
    mem_push_symbol_atom(mem, LMN_SATOM(eof));
    lmn_mem_newlink(mem,
                    a2, t2, LMN_ATTR_GET_VALUE(t2),
                    LMN_ATOM(eof), LMN_ATTR_MAKE_LINK(0), 0);
  }

  lmn_mem_newlink(mem,
                  a1, t1, LMN_ATTR_GET_VALUE(t2),
                  a0, t0, 0);
}

/*
 * 出力文字列ポートを作成する
 %
 * -a0: 出力文字列ポート
 */
void cb_make_output_string(LmnMembrane *mem,
                           LmnAtom a0, LmnLinkAttr t0)
{
  LmnPort port = lmn_make_output_string_port();
  lmn_mem_push_atom(mem, LMN_ATOM(port), LMN_SP_ATOM_ATTR);
  lmn_mem_newlink(mem,
                  LMN_ATOM(port), LMN_SP_ATOM_ATTR, 0,
                  a0, t0, LMN_ATTR_GET_VALUE(t0));
}

/*
 * 文字列から入力文字列ポートを作成する
 %
 * +a0: 文字列
 * -a1: 入力文字列ポート
 */
void cb_make_input_string(LmnMembrane *mem,
                          LmnAtom a0, LmnLinkAttr t0,
                          LmnAtom a1, LmnLinkAttr t1)
{
  LmnPort port = lmn_make_input_string_port(LMN_STRING(a0));

  lmn_mem_push_atom(mem, LMN_ATOM(port), LMN_SP_ATOM_ATTR);
  lmn_mem_newlink(mem,
                  LMN_ATOM(port), LMN_SP_ATOM_ATTR, 0,
                  a1, t1, LMN_ATTR_GET_VALUE(t1));
}

/*
 * 出力文字列ポートに書き込まれた文字列を返す
 %
 * +a0: ポート
 * -a1: ポートを返す
 * -a2: 文字列
 */
void cb_port_output_string(LmnMembrane *mem,
                           LmnAtom a0, LmnLinkAttr t0,
                           LmnAtom a1, LmnLinkAttr t1,
                           LmnAtom a2, LmnLinkAttr t2)
{
  LmnPort port = LMN_PORT(a0);

  if (LMN_PORT_TYPE(port) == LMN_PORT_OSTR) {
    LmnString s = lmn_string_copy(LMN_STRING(LMN_PORT_DATA(port)));
    lmn_mem_push_atom(mem, LMN_ATOM(s), LMN_STRING_ATTR);
    lmn_mem_newlink(mem,
                    a2, t2, LMN_ATTR_GET_VALUE(t2),
                    LMN_ATOM(s), LMN_STRING_ATTR, 0);
  } else {
    LmnSAtom a = lmn_mem_newatom(mem, lmn_functor_intern(ANONYMOUS, lmn_intern("error"), 1));
    lmn_mem_newlink(mem,
                    a2, t2, LMN_ATTR_GET_VALUE(t2),
                    LMN_ATOM(a), LMN_ATTR_MAKE_LINK(0), 0);
  }
  lmn_mem_newlink(mem,
                  a1, t1, LMN_ATTR_GET_VALUE(t2),
                  a0, t0, 0);
}

/*----------------------------------------------------------------------
 * Initialization
 */

void *sp_cb_port_copy(void *data)
{
  return lmn_port_copy(LMN_PORT(data), FALSE);
}

void sp_cb_port_free(void *data)
{
  lmn_port_free(LMN_PORT(data));
}

/* てきとーに定義した */
BOOL sp_cb_port_eq(void *_p1, void *_p2)
{
  return FALSE;
}

void sp_cb_port_dump(void *data, LmnPort port)
{
  port_put_raw_s(port, "<");
  port_put_raw_s(port, LMN_SYMBOL_STR(LMN_PORT(data)->name));
  port_put_raw_s(port, ">");
}

BOOL sp_cp_port_is_ground(void *data)
{
  return FALSE;
}

void port_init()
{
  eof_functor = lmn_functor_intern(ANONYMOUS, lmn_intern("eof"), 1);

  port_atom_type = lmn_sp_atom_register("port",
                                        sp_cb_port_copy,
                                        sp_cb_port_free,
                                        sp_cb_port_eq,
                                        sp_cb_port_dump,
                                        sp_cp_port_is_ground);

  lmn_stdin = lmn_make_file_port(stdin, "stdin", LMN_PORT_INPUT, TRUE);
  lmn_stdout = lmn_make_file_port(stdout, "stdout", LMN_PORT_OUTPUT, TRUE);
  lmn_stderr = lmn_make_file_port(stderr, "stderr", LMN_PORT_OUTPUT, TRUE);

  lmn_register_c_fun("cb_port_stdin", cb_stdin_port, 1);
  lmn_register_c_fun("cb_port_stdout", cb_stdout_port, 1);
  lmn_register_c_fun("cb_port_stderr", cb_stderr_port, 1);
  lmn_register_c_fun("cb_port_getc", cb_port_getc, 3);
  lmn_register_c_fun("cb_port_putc", cb_port_putc, 3);
  lmn_register_c_fun("cb_port_puts", cb_port_puts, 3);
  lmn_register_c_fun("cb_port_read_line", cb_port_read_line, 3);
  lmn_register_c_fun("cb_make_output_string", cb_make_output_string, 1);
  lmn_register_c_fun("cb_make_input_string", cb_make_input_string, 2);
  lmn_register_c_fun("cb_port_output_string", cb_port_output_string, 3);
  lmn_register_c_fun("cb_port_close", cb_port_close, 2);
  lmn_register_c_fun("cb_port_free", cb_port_free, 1);
}

void port_finalize()
{
  lmn_port_free(lmn_stdin);
  lmn_port_free(lmn_stdout);
  lmn_port_free(lmn_stderr);
}

