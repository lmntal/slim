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

static void free_port(LmnPort port)
{
  LMN_FREE(port);
}


static LmnPort lmn_stdin;
static LmnPort lmn_stdout;
static LmnPort lmn_stderr;

LmnPort lmn_stdin_port()
{
  return lmn_stdin;
}

LmnPort lmn_stdout_port()
{
  return lmn_stdout;
}

LmnPort lmn_stderr_port()
{
  return lmn_stderr;
}

LmnPort lmn_make_file_port(FILE *file,
                           char *name, /* move owner */
                           LmnPortDirection dir,
                           BOOL owner)
{
  LmnPort port = make_port(dir, LMN_PORT_FILE, name);

  port->data = file;
  port->owner = owner;

  return port;
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
  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE:
    {
      FILE *f = (FILE *)LMN_PORT_DATA(port);
      if (feof(f)) return EOF;
      return fgetc(f);
    }
    break;
  case LMN_PORT_ISTR:
    /* TODO */
    break;
  default:
    lmn_fatal("PORT>C: unexpected port type");
    break;
  }
  return EOF;
}

/* ポートに一文字を戻す。戻した文字は次に読み込む際に読み込まれる。戻す
   文字は一文字だけしか保証されない。正常に実行された場合にはcを返し，
   エラーが起きた場合にはEOFを返す */
int port_unget_raw_c(LmnPort port, int c)
{
  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE:
    {
      FILE *f = (FILE *)LMN_PORT_DATA(port);
      return ungetc(c, f);
    }
    break;
  case LMN_PORT_ISTR:
    /* TODO */
    break;
  default:
    lmn_fatal("unexpected port type");
    break;
  }
  return EOF;
}

/* ポートから一行読み込み、読み込んだ文字列を返す。ファイルの終わりに達
   していたり，エラーが起きた場合はNULLを返す */
LmnString port_read_line(LmnPort port)
{
  int c0, c1;
  LmnString s;
  
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
  const char *s = LMN_SATOM_STR(unary_atom);
    
  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE:
    {
      FILE *f = (FILE *)LMN_PORT_DATA(port);
      return fputs(s, f);
    }
    break;
  case LMN_PORT_ISTR:
    /* TODO */
    break;
  default:
    lmn_fatal("unexpected port type");
    break;
  }
  return EOF;
}

/* Cの文字列をポートに出力する */
void port_put_raw_s(LmnPort port, const char *str)
{
  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE:
    {
      FILE *f = (FILE *)LMN_PORT_DATA(port);
      fputs(str, f);
      return;
    }
    break;
  case LMN_PORT_ISTR:
    /* TODO */
    break;
  default:
    lmn_fatal("PORT>C: unexpected port type");
    break;
  }
}

/* 文字列をポートに出力する */
void port_puts(LmnPort port, LmnString str)
{
  port_put_raw_s(port, lmn_string_c_str(str));
  return;
}

/*----------------------------------------------------------------------
 * Callbacks
 */

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
  if (lmn_port_closed(LMN_PORT(a0))) {
    LmnSAtom a = lmn_new_atom(lmn_functor_intern(ANONYMOUS, lmn_intern("error"), 1));
    mem_push_symbol_atom(mem, a);
    lmn_mem_newlink(mem,
                    a1, t1, LMN_ATTR_GET_VALUE(t1),
                    LMN_ATOM(a), LMN_ATTR_MAKE_LINK(0), 0);
  } else {
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
    
  }

  lmn_mem_newlink(mem,
                  a1, t1, LMN_ATTR_GET_VALUE(t2),
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
  if (lmn_port_closed(LMN_PORT(a0))) {
    /* do nothing */
  } else {
    port_puts(LMN_PORT(a0), LMN_STRING(a1));
  }

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
  if (lmn_port_closed(LMN_PORT(a0))) {
    /* do nothing */
  } else {
    port_puts(LMN_PORT(a0), LMN_STRING(a1));
  }

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

/*----------------------------------------------------------------------
 * Initialization
 */

void *sp_cb_port_copy(void *data)
{
  lmn_fatal("PORT.C: unexpected");
}

void sp_cb_port_free(void *data)
{
  if (LMN_PORT_OWNER(LMN_PORT(data))) {
    free_port(LMN_PORT(data));
  }
}

/* てきとーに定義した */
BOOL sp_cb_port_eq(void *_p1, void *_p2)
{
  return FALSE;
}

void sp_cb_port_dump(void *data, FILE *stream)
{
  fprintf(stream, "<%s>", LMN_SYMBOL_STR(LMN_PORT(data)->name));
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

  lmn_stdin = lmn_make_file_port(stdin, "stdin", LMN_PORT_INPUT, FALSE);
  lmn_stdout = lmn_make_file_port(stdout, "stdout", LMN_PORT_OUTPUT, FALSE);
  lmn_stderr = lmn_make_file_port(stderr, "stderr", LMN_PORT_OUTPUT, FALSE);

  lmn_register_c_fun("port_stdin", cb_stdin_port, 1);
  lmn_register_c_fun("port_stdout", cb_stdout_port, 1);
  lmn_register_c_fun("port_stderr", cb_stderr_port, 1);
  lmn_register_c_fun("port_getc", cb_port_getc, 3);
  lmn_register_c_fun("port_putc", cb_port_putc, 3);
  lmn_register_c_fun("port_puts", cb_port_puts, 3);
  lmn_register_c_fun("port_read_line", cb_port_read_line, 3);
}

void port_finalize()
{
  free_port(lmn_stdin);
  free_port(lmn_stdout);
  free_port(lmn_stderr);
}

