/*
 * port.h - Port API
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


/* ポートはSLIMにおける入出力が抽象されたものである。CのFILEポインタの
   ような働きをする。
 */

#ifndef LMN_PORT_H
#define LMN_PORT_H

/* cldoc:begin-category(Lmntal::Port) */

typedef struct LmnPort *LmnPortRef;


#include "../lmntal.h"
#include "lmnstring.h"

struct LmnPort {
  LMN_SP_ATOM_HEADER;

  BOOL direction;
  LmnByte type;   /* LMN_PORT_{FILE|ISTR|OST|PROC} */
  BOOL closed;    /* TRUE if this port is closed */
  BOOL error;     /* error has occurred */
  BOOL owner;     /* TRUE if this port owns underlying
                     file pointer */
  lmn_interned_str name;

  void *data;    /* used internally */
};

struct IStrPortData {
  LmnStringRef s;
  int i;
};


#define LMN_PORT(obj) ((LmnPortRef)(obj))

typedef enum LmnPortDirection {
  LMN_PORT_INPUT, /* 入力ポート */
  LMN_PORT_OUTPUT /* 出力ポート */
} LmnPortDirection;


typedef enum LmnPortType {
  LMN_PORT_FILE, /* CのFILE*の代替のポート */
  LMN_PORT_ISTR, /* 文字列からの入力ポート */
  LMN_PORT_OSTR, /* 文字列からの出力ポート */
/*   LMN_PORT_PROC /\* virtual port *\/ */
} LmnPortType;

void port_init(void);
void port_finalize(void);

LmnPortRef lmn_stdin_port(void);
LmnPortRef lmn_stdout_port(void);
LmnPortRef lmn_stderr_port(void);

LmnPortRef lmn_make_input_string_port(LmnStringRef s);
LmnPortRef lmn_make_output_string_port();

LmnPortRef lmn_make_port(LmnPortDirection dir, LmnPortType type, const char *name);
void lmn_port_free(LmnPortRef port);
void lmn_port_close(LmnPortRef port);
BOOL lmn_port_closed(LmnPortRef port_atom);
BOOL lmn_port_error_occurred(LmnPortRef port_atom);
lmn_interned_str lmn_port_name(LmnPortRef port_atom);


int port_get_raw_c(LmnPortRef port_atom);
int port_unget_raw_c(LmnPortRef port_atom, int c);
int port_putc(LmnPortRef port_atom, LmnSAtom unary_atom);
int port_puts(LmnPortRef port_atom, LmnStringRef str);
int port_put_raw_c(LmnPortRef port_atom, int c);
int port_put_raw_s(LmnPortRef port_atom, const char *str);

/* cldoc:end-category() */

#endif
