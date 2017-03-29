/*
 * jni_lmntal.h
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

/** @author Yusuke Ueno
 *  @see http://www.ueda.info.waseda.ac.jp/lmntal/local/pukiwiki.php?JNI
 */

#ifndef LMNTAL_JNI_H
#define LMNTAL_JNI_H

/**
 * @defgroup FFI
 * @{
 */

#include "lmntal.h"

/** TODO: configure時にjni.hの場所を探させたりして使用可能にしてあげたい */
#ifdef USE_JNI
#  include <jni.h>
#  define HAVE_JNI_H (1)
#endif

#define JNI_ENV_SHUFFLE_INIT    0
#define JNI_ENV_SHUFFLE_DEFUALT 3
#define JNI_ENV_SHUFFLE_MAX     3

struct JniContextLmntal {
	/* 情報 */
	int shuffle_level;
	/* 標準入出力の操作に必要なクラス、メソッド、フィールド */
#ifdef HAVE_JNI_H
	jclass c_System;
	jmethodID m_System_setOut;
	jfieldID f_System_out;
	jclass c_PrintStream;
	jmethodID m_PrintStream_constructor;
	jclass c_ByteArrayOutputStream;
	jmethodID m_ByteArrayOutputStream_constructor;
	jmethodID m_ByteArrayOutputStream_toString;
	jmethodID m_ByteArrayOutputStream_reset;
	/* lmntal環境変数の操作に必要なクラス、メソッド、フィールド */
	jclass c_Env;
	jfieldID f_Env_slimcode;
	jfieldID f_Env_compileonly;
	jfieldID f_Env_fInterpret;
	jfieldID f_Env_findatom2;
	jfieldID f_Env_shuffle;
	jfieldID f_Env_fGUI;
	jfieldID f_Env_nErrors;
	/* lmntal処理系の実行に必要なクラス、メソッド、フィールド */
	jclass c_FrontEnd;
	jmethodID m_FrontEnd_run;
	jclass c_StringReader;
	jmethodID m_StringReader_constructor;
	/* オブジェクト */
	jobject oldOut;
	jobject byteArray;
#endif
};

#ifdef _CYGWIN__
char* run_cygpath(const char* option, const char* arg);
#endif
void run_jni_interactive();

/* @} */

#endif /* LMNTAL_JNI_H */

