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
#include "port.h"
#include "error.h"
#include "lmnstring.h"
#include "lmntal.h"
#include "util.h"
#include "vm/vm.h"

static int        port_atom_type; /* special atom type */
static LmnFunctor eof_functor;

#define LMN_PORT_DATA(obj) (LMN_PORT(obj)->data)
#define LMN_PORT_TYPE(obj) (LMN_PORT(obj)->type)
#define LMN_PORT_OWNER(obj) (LMN_PORT(obj)->owner)
#define LMN_PORT_DIR(obj) (LMN_PORT(obj)->direction)

#define FILE_PORT_FP(obj) ((FILE *)LMN_PORT_DATA(obj))

static LmnPortRef port_copy_sub(LmnPortRef port);

/*
 * Internal Constructor.
 */
static LmnPortRef make_port(LmnPortDirection dir, LmnPortType type, char const *name) {
  struct LmnPort *port = LMN_MALLOC<struct LmnPort>();
  LMN_SP_ATOM_SET_TYPE(port, port_atom_type);
  port->direction = dir;
  port->type      = type;
  port->closed    = FALSE;
  port->error     = FALSE;
  port->name      = lmn_intern(name);
  port->data      = NULL;
  port->owner     = TRUE;
  return port;
}

LmnPortRef lmn_make_port(LmnPortDirection dir, LmnPortType type, char const *name) {
  return make_port(dir, type, name);
}

void lmn_port_free(LmnPortRef port) {
  if (port->owner) {
    switch (LMN_PORT_TYPE(port)) {
    case LMN_PORT_FILE:
      break;
    case LMN_PORT_OSTR:
      delete ((LmnStringRef)LMN_PORT_DATA(port));
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
LmnPortRef lmn_port_copy(LmnPortRef port, BOOL owner) {
  LmnPortRef new_port = port_copy_sub(port);
  new_port->owner     = owner && LMN_PORT_OWNER(port);

  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE:
    if (new_port->owner)
      port->owner = FALSE;
    break;
  case LMN_PORT_OSTR:
    new_port->data = new LmnString(*reinterpret_cast<LmnString *>(port->data));
    port->owner    = TRUE;
    break;
  case LMN_PORT_ISTR:
    if (new_port->owner)
      port->owner = FALSE;
    break;
  }
  return new_port;
}

static LmnPortRef port_copy_sub(LmnPortRef port) {
  LmnPortRef result = LMN_MALLOC<struct LmnPort>();
  memcpy(result, port, sizeof(struct LmnPort));
  return result;
}

static LmnPortRef lmn_stdin;
static LmnPortRef lmn_stdout;
static LmnPortRef lmn_stderr;

LmnPortRef lmn_stdin_port() { return lmn_port_copy(lmn_stdin, FALSE); }

LmnPortRef lmn_stdout_port() { return lmn_port_copy(lmn_stdout, FALSE); }

LmnPortRef lmn_stderr_port() { return lmn_port_copy(lmn_stderr, FALSE); }

LmnPortRef lmn_make_file_port(FILE *file, char const *name, LmnPortDirection dir, BOOL owner) {
  LmnPortRef port = make_port(dir, LMN_PORT_FILE, name);

  port->data  = file;
  port->owner = owner;

  return port;
}

/* 入力文字列ポートを作成する。引数sの解放の責任は個の関数が持つ */
LmnPortRef lmn_make_input_string_port(LmnStringRef s) {
  LmnPortRef           port = make_port(LMN_PORT_INPUT, LMN_PORT_ISTR, "input string port");
  struct IStrPortData *d    = LMN_MALLOC<struct IStrPortData>();

  d->s        = s;
  d->i        = 0;
  port->data  = d;
  port->owner = TRUE;

  return port;
}

LmnPortRef lmn_make_output_string_port() {
  LmnPortRef port = make_port(LMN_PORT_OUTPUT, LMN_PORT_OSTR, "output string port");

  port->data  = new LmnString();
  port->owner = TRUE;
  return port;
}

void lmn_port_close(LmnPortRef port) {
  if (lmn_port_closed(port))
    return;

  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE:
    fclose((FILE *)LMN_PORT_DATA(port));
    break;
  case LMN_PORT_OSTR:
    break;
  case LMN_PORT_ISTR:
    delete (((struct IStrPortData *)LMN_PORT_DATA(port))->s);
    ((struct IStrPortData *)LMN_PORT_DATA(port))->s = NULL;
    LMN_FREE(LMN_PORT_DATA(port));
    LMN_PORT_DATA(port) = NULL;
    break;
  default:
    lmn_fatal("not implemented");
  }
  port->closed = TRUE;
}

BOOL lmn_port_closed(LmnPortRef port_atom) { return LMN_PORT(port_atom)->closed; }

BOOL lmn_port_error_occurred(LmnPortRef port_atom) { return LMN_PORT(port_atom)->error; }

LmnPortDirection lmn_port_dir(LmnPortRef port_atom) { return (LmnPortDirection)LMN_PORT(port_atom)->direction; }

lmn_interned_str lmn_port_name(LmnPortRef port_atom) { return LMN_PORT(port_atom)->name; }

/* 出力文字列ポートに書き込まれた文字列のコピー返す。 */
LmnStringRef lmn_port_output_string(LmnPortRef ostr_port) {
  return new LmnString(*reinterpret_cast<LmnString *>(LMN_PORT_DATA(ostr_port)));
}

/*----------------------------------------------------------------------
 * Read & Write
 */

/* ポートから一文字読み込み返す。もしくは、ポートの終わりに達するか、エ
   ラーが起きた場合には、EOFを返す */
int port_get_raw_c(LmnPortRef port) {
  if (lmn_port_closed(port))
    return EOF;
  if (LMN_PORT_DIR(port) != LMN_PORT_INPUT)
    return EOF;

  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE: {
    FILE *f = (FILE *)LMN_PORT_DATA(port);
    if (feof(f))
      return EOF;
    return fgetc(f);
  } break;
  case LMN_PORT_ISTR: {
    struct IStrPortData *d = (struct IStrPortData *)LMN_PORT_DATA(port);
    if (d->i < 0 || d->i >= d->s->size())
      return EOF;
    else
      return (*d->s)[d->i++];
  } break;
  default:
    return EOF;
    break;
  }
}

/* ポートに一文字を戻す。戻した文字は次に読み込む際に読み込まれる。戻す
   文字は一文字だけしか保証されない。正常に実行された場合にはcを返し，
   エラーが起きた場合にはEOFを返す */
int port_unget_raw_c(LmnPortRef port, int c) {
  if (lmn_port_closed(port))
    return EOF;
  if (LMN_PORT_DIR(port) != LMN_PORT_INPUT)
    return EOF;

  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE: {
    FILE *f = (FILE *)LMN_PORT_DATA(port);
    return ungetc(c, f);
  } break;
  case LMN_PORT_ISTR: {
    struct IStrPortData *d = (struct IStrPortData *)LMN_PORT_DATA(port);
    if (d->i > 0 && d->i < d->s->size()) {
      if ((*d->s)[d->i - 1] == c) {
        d->i--;
        return c;
      }
    }
    return EOF;
  } break;
  default:
    return EOF;
  }
}

/* ポートから一行読み込み、読み込んだ文字列を返す。ファイルの終わりに達
   していたり，エラーが起きた場合はNULLを返す */
LmnStringRef port_read_line(LmnPortRef port) {
  int          c0, c1;
  LmnStringRef s;

  if (lmn_port_closed(port))
    return NULL;
  if (LMN_PORT_DIR(port) != LMN_PORT_INPUT)
    return NULL;

  c0 = port_get_raw_c(port);
  if (c0 == EOF)
    return NULL;
  s = new LmnString();
  for (;;) {
    if (c0 == EOF)
      return s;
    if (c0 == '\n')
      break;
    if (c0 == '\r') {
      c1 = port_get_raw_c(port);
      if (c1 == EOF || c1 == '\n')
        break;
      port_unget_raw_c(port, c1);
      break;
    }
    s->push_back(c0);
    c0 = port_get_raw_c(port);
  }
  return s;
}

/* ポートから空白またはタブまたは改行で区切られたトークンを読み込み、
   読み込んだ文字列を返す。ファイルの終わりに達
   していたり，エラーが起きた場合はNULLを返す */
LmnStringRef port_read_token(LmnPortRef port) {
  int          c0, c1;
  LmnStringRef s;

  if (lmn_port_closed(port))
    return NULL;
  if (LMN_PORT_DIR(port) != LMN_PORT_INPUT)
    return NULL;

  c0 = port_get_raw_c(port);
  for (;;) {
    if (c0 == EOF)
      break;
    if (c0 == ' ' || c0 == '\t' || c0 == '\n') {
      c0 = port_get_raw_c(port);
    } else if (c0 == '\r') {
      c1 = port_get_raw_c(port);
      if (c1 == EOF)
        break;
      if (c1 == '\n') {
        c0 = port_get_raw_c(port);
      } else {
        port_unget_raw_c(port, c1);
      }
    } else
      break;
  }
  if (c0 == EOF)
    return NULL;
  s = new LmnString();
  for (;;) {
    if (c0 == EOF)
      return s;
    if (c0 == ' ' || c0 == '\t' || c0 == '\n')
      break;
    if (c0 == '\r') {
      c1 = port_get_raw_c(port);
      if (c1 == EOF || c1 == '\n')
        break;
      port_unget_raw_c(port, c1);
      break;
    }
    s->push_back(c0);
    c0 = port_get_raw_c(port);
  }
  return s;
}

/* 文字はunaryアトムで表現している */
int port_putc(LmnPortRef port, LmnSAtom unary_atom) {
  return port_put_raw_s(port, ((LmnSymbolAtomRef)unary_atom)->str());
}

/* Cの文字をポートに出力する。エラーが起きた場合はEOFを返す。 正常に
   処理された場合は負でない数を返す*/
int port_put_raw_c(LmnPortRef port, int c) {
  if (lmn_port_closed(port))
    return EOF;
  if (LMN_PORT_DIR(port) != LMN_PORT_OUTPUT)
    return EOF;

  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE: {
    FILE *f = (FILE *)LMN_PORT_DATA(port);
    return fputc(c, f);
  } break;
  case LMN_PORT_OSTR:
    ((LmnStringRef)LMN_PORT_DATA(port))->push_back(c);
    return 1;
  default:
    lmn_fatal("unexpected port type");
    break;
  }
}

/* Cの文字列をポートに出力する。エラーが起きた場合はEOFを返す。 正常に
   処理された場合は負でない数を返す*/
int port_put_raw_s(LmnPortRef port, char const *str) {
  if (lmn_port_closed(port))
    return EOF;
  if (LMN_PORT_DIR(port) != LMN_PORT_OUTPUT)
    return EOF;

  switch (LMN_PORT_TYPE(port)) {
  case LMN_PORT_FILE: {
    FILE *f = (FILE *)LMN_PORT_DATA(port);
    return fputs(str, f);
  } break;
  case LMN_PORT_OSTR:
    ((LmnStringRef)LMN_PORT_DATA(port))->append(str);
    return 1;
  default:
    lmn_fatal("unexpected port type");
    break;
  }
}

/* 文字列をポートに出力する。エラーが起きた場合はEOFを返す。 正常に
   処理された場合は負でない数を返す*/
int port_puts(LmnPortRef port, LmnStringRef str) { return port_put_raw_s(port, str->c_str()); }

/*----------------------------------------------------------------------
 * Callbacks
 */

/*
 * ポートを閉じる。閉じたポートに対する書き込み、読み込み操作はエラーになる
 *
 * +a0: ポート
 * -a1: ポートを返す
 */
void cb_port_close(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                   LmnLinkAttr t1) {
  LmnPortRef port = LMN_PORT(a0);

  lmn_port_close(port);
  lmn_mem_newlink(mem, a0, t0, 0, a1, t1, LMN_ATTR_GET_VALUE(t1));
}

/*
 * ポートを解放する
 *
 * +a0: ポート
 */
void cb_port_free(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0) {
  lmn_port_free(LMN_PORT(a0));
  lmn_mem_remove_data_atom(mem, (LmnDataAtomRef)a0, t0);
}

/*
 * -a0: 標準入力ポートを返す
 */
void cb_stdin_port(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0) {
  LmnPortRef  atom = lmn_stdin_port();
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;

  lmn_mem_push_atom(mem, atom, attr);
  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), atom, attr, 0);
}

/*
 * -a0: 標準出力ポートを返す
 */
void cb_stdout_port(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0) {
  LmnPortRef  atom = lmn_stdout_port();
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;

  lmn_mem_push_atom(mem, atom, attr);
  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), atom, attr, 0);
}

/*
 * -a0: 標準エラーポートを返す
 */
void cb_stderr_port(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0) {
  LmnPortRef  atom = lmn_stderr_port();
  LmnLinkAttr attr = LMN_SP_ATOM_ATTR;

  lmn_mem_push_atom(mem, atom, attr);
  lmn_mem_newlink(mem, a0, t0, LMN_ATTR_GET_VALUE(t0), atom, attr, 0);
}

/*
 * +a0: ポート
 * -a1: ポートを返す
 * -a2: 文字
 */
void cb_port_getc(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1,
                  LmnAtomRef a2, LmnLinkAttr t2) {
  int      c;
  char     buf[8];
  LmnSAtom a;

  c = port_get_raw_c(LMN_PORT(a0));
  if (c == EOF)
    sprintf(buf, "eof");
  else
    sprintf(buf, "%c", c);
  a = lmn_new_atom(lmn_functor_table->intern(ANONYMOUS, lmn_intern(buf), 1));
  mem_push_symbol_atom(mem, (LmnSymbolAtomRef)a);
  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), a, LMN_ATTR_MAKE_LINK(0), 0);

  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), a0, t0, 0);
}

/*
 * +a0: ポート
 * -a1: ポートを返す
 * -a2: バイト（文字コード）
 */
void cb_port_get_byte(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                      LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2) {
  LmnWord c = port_get_raw_c(LMN_PORT(a0));

  lmn_mem_push_atom(mem, (LmnAtomRef)c, LMN_INT_ATTR);
  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), (LmnAtomRef)c, LMN_INT_ATTR, 0);
  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), a0, t0, 0);
}

/*
 * +a0: ポート
 * -a1: ポートを返す
 * +a2: バイト（文字コード）
 */
void cb_port_unget_byte(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                        LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2) {
  port_unget_raw_c(LMN_PORT(a0), (LmnWord)a1);

  lmn_mem_delete_atom(mem, a1, t1);
  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), a0, t0, 0);
}

/*
 * +a0: ポート
 * +a1: unaryアトム
 * -a2: ポートを返す
 */
void cb_port_putc(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1,
                  LmnAtomRef a2, LmnLinkAttr t2) {
  if (LMN_ATTR_IS_DATA_WITHOUT_EX(t1)) {
    switch (t1) {
    case LMN_INT_ATTR: {
      char *s = int_to_str((long)a1);
      port_put_raw_s(LMN_PORT(a0), s);
    } break;

    case LMN_DBL_ATTR: {
      char buf[64];
      sprintf(buf, "%#g", lmn_get_double((LmnDataAtomRef)a1));
      port_put_raw_s(LMN_PORT(a0), buf);
    } break;

    case LMN_SP_ATOM_ATTR:
      port_puts(LMN_PORT(a0), reinterpret_cast<LmnString *>(a1));
      break;

    case LMN_HL_ATTR:
      port_putc(LMN_PORT(a0), reinterpret_cast<LmnString *>(a1));
      break;

    default:
      lmn_fatal("unexpected attr");
    }
  } else { /* symbol atom */
    port_putc(LMN_PORT(a0), reinterpret_cast<LmnString *>(a1));
  }

  lmn_mem_delete_atom(mem, a1, t1);
  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), a0, t0, 0);
}

/*
 * a0: ポート
 * a1: バイト（文字コード）
 * a2: ポートを返す
 */
void cb_port_put_byte(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                      LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2) {
  port_put_raw_c(LMN_PORT(a0), (LmnWord)a1);

  lmn_mem_delete_atom(mem, a1, t1);
  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), a0, t0, 0);
}

/*
 * a0: ポート
 * a1: 文字列
 * a2: ポートを返す
 */
void cb_port_puts(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1, LmnLinkAttr t1,
                  LmnAtomRef a2, LmnLinkAttr t2) {
  port_puts(LMN_PORT(a0), reinterpret_cast<LmnString *>(a1));

  lmn_mem_delete_atom(mem, a1, t1);
  lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), a0, t0, 0);
}

/*
 * +a0: ポート
 * -a1: ポートを返す
 * -a2: 文字列
 */
void cb_port_read_line(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                       LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2) {
  LmnStringRef s = port_read_line(LMN_PORT(a0));

  if (s != NULL) {
    lmn_mem_push_atom(mem, s, LMN_SP_ATOM_ATTR);
    lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), s, LMN_SP_ATOM_ATTR, 0);
  } else {
    LmnSAtom eof = lmn_new_atom(eof_functor);
    mem_push_symbol_atom(mem, (LmnSymbolAtomRef)LMN_SATOM(eof));
    lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), eof, LMN_ATTR_MAKE_LINK(0), 0);
  }

  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), a0, t0, 0);
}

/*
 * +a0: ポート
 * -a1: ポートを返す
 * -a2: 文字列
 */
void cb_port_read_token(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                        LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2) {
  LmnStringRef s = port_read_token(LMN_PORT(a0));

  if (s != NULL) {
    lmn_mem_push_atom(mem, s, LMN_SP_ATOM_ATTR);
    lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), s, LMN_SP_ATOM_ATTR, 0);
  } else {
    LmnSAtom eof = lmn_new_atom(eof_functor);
    mem_push_symbol_atom(mem, (LmnSymbolAtomRef)LMN_SATOM(eof));
    lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), eof, LMN_ATTR_MAKE_LINK(0), 0);
  }

  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), a0, t0, 0);
}

/*
 * 出力文字列ポートを作成する
 %
 * -a0: 出力文字列ポート
 */
void cb_make_output_string(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0) {
  LmnPortRef port = lmn_make_output_string_port();
  lmn_mem_push_atom(mem, port, LMN_SP_ATOM_ATTR);
  lmn_mem_newlink(mem, port, LMN_SP_ATOM_ATTR, 0, a0, t0, LMN_ATTR_GET_VALUE(t0));
}

/*
 * 文字列から入力文字列ポートを作成する
 %
 * +a0: 文字列
 * -a1: 入力文字列ポート
 */
void cb_make_input_string(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                          LmnLinkAttr t1) {
  LmnPortRef port = lmn_make_input_string_port(reinterpret_cast<LmnString *>(a0));

  lmn_mem_push_atom(mem, port, LMN_SP_ATOM_ATTR);
  lmn_mem_newlink(mem, port, LMN_SP_ATOM_ATTR, 0, a1, t1, LMN_ATTR_GET_VALUE(t1));
}

/*
 * 出力文字列ポートに書き込まれた文字列を返す
 %
 * +a0: ポート
 * -a1: ポートを返す
 * -a2: 文字列
 */
void cb_port_output_string(LmnReactCxtRef rc, LmnMembraneRef mem, LmnAtomRef a0, LmnLinkAttr t0, LmnAtomRef a1,
                           LmnLinkAttr t1, LmnAtomRef a2, LmnLinkAttr t2) {
  LmnPortRef port = LMN_PORT(a0);

  if (LMN_PORT_TYPE(port) == LMN_PORT_OSTR) {
    LmnStringRef s = lmn_port_output_string(port);
    lmn_mem_push_atom(mem, s, LMN_STRING_ATTR);
    lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), s, LMN_STRING_ATTR, 0);
  } else {
    LmnSAtom a = lmn_mem_newatom(mem, lmn_functor_table->intern(ANONYMOUS, lmn_intern("error"), 1));
    lmn_mem_newlink(mem, a2, t2, LMN_ATTR_GET_VALUE(t2), a, LMN_ATTR_MAKE_LINK(0), 0);
  }
  lmn_mem_newlink(mem, a1, t1, LMN_ATTR_GET_VALUE(t1), /* debugged */
                  a0, t0, 0);
}

/*----------------------------------------------------------------------
 * Initialization
 */

void *sp_cb_port_copy(void *data) { return lmn_port_copy(LMN_PORT(data), TRUE); }

void sp_cb_port_free(void *data) { lmn_port_free(LMN_PORT(data)); }

/* てきとーに定義した */
BOOL sp_cb_port_eq(void *_p1, void *_p2) { return FALSE; }

void sp_cb_port_dump(void *data, LmnPortRef port) {
  port_put_raw_s(port, "<");
  port_put_raw_s(port, LMN_SYMBOL_STR(LMN_PORT(data)->name));
  port_put_raw_s(port, ">");
}

BOOL sp_cp_port_is_ground(void *data) { return FALSE; }

void port_init() {
  eof_functor = lmn_functor_table->intern(ANONYMOUS, lmn_intern("eof"), 1);

  port_atom_type = lmn_sp_atom_register("port", sp_cb_port_copy, sp_cb_port_free, sp_cb_port_eq, sp_cb_port_dump,
                                        sp_cp_port_is_ground);

  lmn_stdin  = lmn_make_file_port(stdin, "stdin", LMN_PORT_INPUT, TRUE);
  lmn_stdout = lmn_make_file_port(stdout, "stdout", LMN_PORT_OUTPUT, TRUE);
  lmn_stderr = lmn_make_file_port(stderr, "stderr", LMN_PORT_OUTPUT, TRUE);

  CCallback::lmn_register_c_fun("cb_port_stdin", (void *)cb_stdin_port, 1);
  CCallback::lmn_register_c_fun("cb_port_stdout", (void *)cb_stdout_port, 1);
  CCallback::lmn_register_c_fun("cb_port_stderr", (void *)cb_stderr_port, 1);
  CCallback::lmn_register_c_fun("cb_port_getc", (void *)cb_port_getc, 3);
  CCallback::lmn_register_c_fun("cb_port_get_byte", (void *)cb_port_get_byte, 3);
  CCallback::lmn_register_c_fun("cb_port_unget_byte", (void *)cb_port_unget_byte, 3);
  CCallback::lmn_register_c_fun("cb_port_putc", (void *)cb_port_putc, 3);
  CCallback::lmn_register_c_fun("cb_port_put_byte", (void *)cb_port_put_byte, 3);
  CCallback::lmn_register_c_fun("cb_port_puts", (void *)cb_port_puts, 3);
  CCallback::lmn_register_c_fun("cb_port_read_line", (void *)cb_port_read_line, 3);
  CCallback::lmn_register_c_fun("cb_port_read_token", (void *)cb_port_read_token, 3);
  CCallback::lmn_register_c_fun("cb_make_output_string", (void *)cb_make_output_string, 1);
  CCallback::lmn_register_c_fun("cb_make_input_string", (void *)cb_make_input_string, 2);
  CCallback::lmn_register_c_fun("cb_port_output_string", (void *)cb_port_output_string, 3);
  CCallback::lmn_register_c_fun("cb_port_close", (void *)cb_port_close, 2);
  CCallback::lmn_register_c_fun("cb_port_free", (void *)cb_port_free, 1);
}

void port_finalize() {
  lmn_port_free(lmn_stdin);
  lmn_port_free(lmn_stdout);
  lmn_port_free(lmn_stderr);
}
