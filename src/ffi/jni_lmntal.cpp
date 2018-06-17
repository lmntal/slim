/*
 * jni_lmntal.c
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

#include "jni_lmntal.h"
#include "loader/loader.h"
#include "vm/vm.h"
#include "verifier/verifier.h"
#include <ctype.h>

#define JNI_CYGPATH                "/usr/bin/cygpath"
#define JNI_LMNTAL_JAR_REL_PATH    "/bin/lmntal.jar"
#define JNI_OPTION_JAVA_CLASS_PATH "-Djava.class.path="
#define JNI_OPTION_LMNTAL_HOME     "-DLMNTAL_HOME="

#define JNI_LMNTAL_COMPILATION_FAILED "Compilation Failed"

#ifdef HAVE_JNI_H
static struct JniContextLmntal jc_lmntal;
static JNIEnv *env;
static JavaVM *jvm;
#endif

#ifdef __CYGWIN__
char* run_cygpath(const char* option, const char* arg)
{
  int size;
  char *cmd, *result, buf[128];
  FILE *fp;

  cmd = LMN_CALLOC(char, strlen(JNI_CYGPATH) + strlen(option) + strlen(arg) + 3);
  sprintf(cmd, "%s %s %s", JNI_CYGPATH, option, arg);
  if ((fp = popen(cmd, "r")) == NULL) {
    result = NULL;
  } else {
    size = 128;
    result = LMN_CALLOC(char, size);
    while((fgets(buf, 128, fp)) != NULL){
      size+=128;
      result = LMN_REALLOC(char, result, size);
      strcat(result, buf);
    }
  }

  LMN_FREE(cmd);
  return result;
}
#endif

#ifdef HAVE_JNI_H
static void jni_load_context_lmntal()
{
  // TODO: マクロ関数をいくつか用意してもっとスマートに記述できないだろうか
  /**
   * 標準入出力操作系
   */
  if (NULL == (jc_lmntal.c_System = (*env)->FindClass(env, "Ljava/lang/System;"))) {
    (*env)->FatalError(env, "FindClass : java/lang/System");
    return;
  }

  if (NULL == (jc_lmntal.m_System_setOut = (*env)->GetStaticMethodID(env, jc_lmntal.c_System, "setOut", "(Ljava/io/PrintStream;)V"))) {
    (*env)->FatalError(env, "GetStaticMethodID : System.setOut(Ljava/io/PrintStream;)V");
    return;
  }

  if (NULL == (jc_lmntal.f_System_out = (*env)->GetStaticFieldID(env, jc_lmntal.c_System, "out", "Ljava/io/PrintStream;"))) {
    (*env)->FatalError(env, "GetStaticFieldID : System.out");
    return;
  }

  if (NULL == (jc_lmntal.c_PrintStream = (*env)->FindClass(env, "Ljava/io/PrintStream;"))) {
    (*env)->FatalError(env, "FindClass : java.io.PrintStream");
    return;
  }

  if (NULL == (jc_lmntal.m_PrintStream_constructor = (*env)->GetMethodID(env, jc_lmntal.c_PrintStream, "<init>", "(Ljava/io/OutputStream;)V"))) {
    (*env)->FatalError(env, "GetStaticMethodID : PrintStream.PrintStream()");
    return;
  }

  if (NULL == (jc_lmntal.c_ByteArrayOutputStream = (*env)->FindClass(env, "Ljava/io/ByteArrayOutputStream;"))) {
    (*env)->FatalError(env, "FindClass : java.io.ByteArrayOutputStream");
    return;
  }

  if (NULL == (jc_lmntal.m_ByteArrayOutputStream_constructor = (*env)->GetMethodID(env, jc_lmntal.c_ByteArrayOutputStream, "<init>", "()V"))) {
    (*env)->FatalError(env, "GetStaticMethodID : ByteArrayOutputStream.ByteArrayOutputStream()");
    return;
  }

  if (NULL == (jc_lmntal.m_ByteArrayOutputStream_toString = (*env)->GetMethodID(env, jc_lmntal.c_ByteArrayOutputStream, "toString", "()Ljava/lang/String;"))) {
    (*env)->FatalError(env, "GetStaticMethodID : ByteArrayOutputStream.toString()");
    return;
  }

  if (NULL == (jc_lmntal.m_ByteArrayOutputStream_reset = (*env)->GetMethodID(env, jc_lmntal.c_ByteArrayOutputStream, "reset", "()V"))) {
    (*env)->FatalError(env, "GetStaticMethodID : ByteArrayOutputStream.reset()");
    return;
  }

  /**
   * Envクラス系
   */
  if (NULL == (jc_lmntal.c_Env = (*env)->FindClass(env, "Lruntime/Env;"))) {
    (*env)->FatalError(env, "FindClass : runtime.Env");
    return;
  }

  if (NULL == (jc_lmntal.f_Env_slimcode = (*env)->GetStaticFieldID(env, jc_lmntal.c_Env, "slimcode", "Z"))) {
    (*env)->FatalError(env, "GetStaticFieldID : Env.slimcode");
    return;
  }

  if (NULL == (jc_lmntal.f_Env_compileonly = (*env)->GetStaticFieldID(env, jc_lmntal.c_Env, "compileonly", "Z"))) {
    (*env)->FatalError(env, "GetStaticFieldID : Env.compileonly");
    return;
  }

  if (NULL == (jc_lmntal.f_Env_fInterpret = (*env)->GetStaticFieldID(env, jc_lmntal.c_Env, "fInterpret", "Z"))) {
    (*env)->FatalError(env, "GetStaticFieldID : Env.fInterpret");
    return;
  }

  if (NULL == (jc_lmntal.f_Env_findatom2 = (*env)->GetStaticFieldID(env, jc_lmntal.c_Env, "findatom2", "Z"))) {
    (*env)->FatalError(env, "GetStaticFieldID : Env.findatom2");
    return;
  }

  if (NULL == (jc_lmntal.f_Env_fGUI = (*env)->GetStaticFieldID(env, jc_lmntal.c_Env, "fGUI", "Z"))) {
    (*env)->FatalError(env, "GetStaticFieldID : Env.fGUI");
    return;
  }

  if (NULL == (jc_lmntal.f_Env_shuffle = (*env)->GetStaticFieldID(env, jc_lmntal.c_Env, "shuffle", "I"))) {
    (*env)->FatalError(env, "GetStaticFieldID : Env.shuffle");
    return;
  }

  if (NULL == (jc_lmntal.f_Env_nErrors = (*env)->GetStaticFieldID(env, jc_lmntal.c_Env, "nErrors", "I"))) {
    (*env)->FatalError(env, "GetStaticFieldID : Env.nErrors");
    return;
  }

  /**
   * lmntal処理系系
   */
  if (NULL == (jc_lmntal.c_FrontEnd = (*env)->FindClass(env, "Lruntime/FrontEnd;"))) {
    (*env)->FatalError(env, "FindClass : runtime.FrontEnd");
    return;
  }

  if (NULL == (jc_lmntal.m_FrontEnd_run = (*env)->GetStaticMethodID(env, jc_lmntal.c_FrontEnd, "run", "(Ljava/io/Reader;)V"))) {
    (*env)->FatalError(env, "GetStaticMethodID : FrontEnd.run(Ljava/io/Reader;)V");
    return;
  }

  if (NULL == (jc_lmntal.c_StringReader = (*env)->FindClass(env, "Ljava/io/StringReader;"))) {
    (*env)->FatalError(env, "FindClass : java.io.StringReader");
    return;
  }

  if (NULL == (jc_lmntal.m_StringReader_constructor = (*env)->GetMethodID(env, jc_lmntal.c_StringReader, "<init>", "(Ljava/lang/String;)V"))) {
    (*env)->FatalError(env, "GetStaticMethodID : StringReader.StringReader(java.lang.String)");
    return;
  }
}
#endif

static BOOL jni_initialize_lmntal()
{
#ifdef HAVE_JNI_H
  JavaVMInitArgs vm_args;
  JavaVMOption options[4];

  char *arg, *path, *lmntal_home;

  /* LMNTAL_HOME環境変数の取得 */
  lmntal_home = NULL;
  if ((lmntal_home = getenv("LMNTAL_HOME")) == NULL) {
    return FALSE;
  }

  /**
   * JavaVM起動オプションの指定
   */
  // TODO: cygwin以外での動作確認

  /* -Djava.class.pathオプション */
  arg = LMN_CALLOC(char, strlen(lmntal_home) + strlen(JNI_LMNTAL_JAR_REL_PATH) + 2 + 1);
  sprintf(arg, "%s%s%s", lmntal_home, JNI_LMNTAL_JAR_REL_PATH, ":.");
#ifdef __CYGWIN__
  path = run_cygpath("-wp", arg);
#else
  path = LMN_CALLOC(char, strlen(arg) + 1);
  strcpy(path, arg);
#endif
  LMN_FREE(arg);

  options[0].optionString = LMN_CALLOC(char, strlen(JNI_OPTION_JAVA_CLASS_PATH) + strlen(path) + 1);
  sprintf(options[0].optionString, "%s%s", JNI_OPTION_JAVA_CLASS_PATH, path);
  LMN_FREE(path);

  /* DLMNTAL_HOMEオプション */
#ifdef __CYGWIN__
  path = run_cygpath("-wp", lmntal_home);
#else
  path = LMN_CALLOC(char, strlen(lmntal_home) + 1);
  strcpy(path, lmntal_home);
#endif
  options[1].optionString = LMN_CALLOC(char, strlen(JNI_OPTION_LMNTAL_HOME) + strlen(path) + 1);
  sprintf(options[1].optionString, "%s%s", JNI_OPTION_LMNTAL_HOME, path);
  LMN_FREE(path);

  /* verboseオプション */
  /* options[2].optionString = (char *) "-verbose:(*env)"; //class, (*env), gc */
  vm_args.version = JNI_VERSION_1_6;
  vm_args.nOptions = 2;
  vm_args.options = options;
  vm_args.ignoreUnrecognized = JNI_FALSE;

  /**
   * JavaVMを初期化、起動する
   */
  JNI_CreateJavaVM(&jvm, (void **) &env, (void *) &vm_args);

  LMN_FREE(options[1].optionString);
  LMN_FREE(options[0].optionString);

  jni_load_context_lmntal();


  /**
   * JavaVMの標準入出力を付け替える
   * @see http://blog.goo.ne.jp/xmldtp/e/05e8acb765b41d8a5da238386c8161b6
   */
  jc_lmntal.oldOut = (*env)->GetStaticObjectField(env, jc_lmntal.c_System, jc_lmntal.f_System_out);
  jc_lmntal.byteArray = (*env)->NewObject(env, jc_lmntal.c_ByteArrayOutputStream, jc_lmntal.m_ByteArrayOutputStream_constructor);
  jobject newOut = (*env)->NewObject(env, jc_lmntal.c_PrintStream, jc_lmntal.m_PrintStream_constructor, jc_lmntal.byteArray);
  (*env)->CallStaticVoidMethod(env, jc_lmntal.c_System, jc_lmntal.m_System_setOut, newOut);

  /**
   * lmntal処理系の環境変数を設定する
   *
   *  --slimcode    = true
   *  --compileonly = true
   *  --fInterpret  = true
   *  --findatom2   = false
   *  -s0
   */
  jc_lmntal.shuffle_level = JNI_ENV_SHUFFLE_INIT;
  (*env)->SetStaticBooleanField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_slimcode, JNI_TRUE);
  (*env)->SetStaticBooleanField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_compileonly, JNI_TRUE);
  (*env)->SetStaticBooleanField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_fInterpret, JNI_TRUE);
  (*env)->SetStaticBooleanField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_findatom2, JNI_FALSE);
  (*env)->SetStaticBooleanField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_fGUI, JNI_FALSE);
  (*env)->SetStaticIntField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_shuffle, jc_lmntal.shuffle_level);

#endif
  return TRUE;
}

static void jni_finalize_lmntal()
{
#ifdef USE_JNI
  /**
   * JavaVMの標準入出力をもとに戻す
   */
  (*env)->CallStaticVoidMethod(env, jc_lmntal.c_System, jc_lmntal.m_System_setOut, jc_lmntal.oldOut);

  /**
   * JavaVMの解放
   */
  (*jvm)->DestroyJavaVM(jvm);
#endif
}

static BOOL jni_lmntal_compile(char **result, const char *code)
{
#ifdef USE_JNI
  /**
   * ■lmntal処理系を実行する
   */
  jstring line = (*env)->NewStringUTF(env, code); // lmntalコードのString型
  jobject stringReader = (*env)->NewObject(env, jc_lmntal.c_StringReader, jc_lmntal.m_StringReader_constructor, line); // String to StringReader
  (*env)->CallStaticObjectMethod(env, jc_lmntal.c_FrontEnd, jc_lmntal.m_FrontEnd_run, stringReader); // run

  /* get compiled slimcode from pseudo output */
  jstring slimcode = (*env)->CallObjectMethod(env, jc_lmntal.byteArray, jc_lmntal.m_ByteArrayOutputStream_toString);
  (*env)->CallObjectMethod(env, jc_lmntal.byteArray, jc_lmntal.m_ByteArrayOutputStream_reset);
  const char *r = (*env)->GetStringUTFChars(env, slimcode, NULL);
  *result = (char*)LMN_CALLOC(char, strlen(r)+1);
  strcpy(*result, r);
  (*env)->ReleaseStringUTFChars(env, slimcode, r);

  /* processing Compilation Errors [1] */
  jint nErrors = (*env)->GetStaticIntField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_nErrors);
  if (nErrors > 0) {
    printf("Compilation Failed\n\n");
    return FALSE;
  }

  /* processing Compilation Errors [2] */
  if (strncmp(*result, JNI_LMNTAL_COMPILATION_FAILED, strlen(JNI_LMNTAL_COMPILATION_FAILED)) == 0) {
    printf("Compilation Failed\n\n");
    return FALSE;
  }
#endif
  return TRUE;
}

using file_ptr = std::unique_ptr<FILE, decltype(&fclose)>;

static void _run(file_ptr fp)
{
  Vector *start_rulesets;
  LmnRuleSetRef t;

  t = load(std::move(fp));
  start_rulesets = vec_make(2);

  vec_push(start_rulesets, (vec_data_t)t);

  /* まともな入力ファイルが無ければ抜ける */
  if(vec_is_empty(start_rulesets)){
    fprintf(stderr, "bad script.\n");
    return;
  }

  if (lmn_env.nd) {
    run_mc(start_rulesets, NULL, NULL);
  } else {
    lmn_run(start_rulesets);
  }

  vec_free(start_rulesets);
}

static void show_help()
{
  printf("Commands:\n");
  printf("  :nd            - Set nondeterministic execution mode ON, print all execution paths\n");
  printf("  :nd_result     - Set nondeterministic execution mode ON, print only deadlock paths\n");
  printf("  :nd_dump       - Set nondeterministic execution mode ON, print all state instantly\n");
  printf("  :nond          - Set nondeterministic execution mode OFF\n");
  printf("  :remain        - Set remain mode ON\n");
  printf("  :noremain      - Set remain mode OFF\n");
  printf("  :clear         - Clean the remain atoms (if remain mode ON)\n");
  printf("  :gui           - Set gui mode ON (BUT able to run only once)\n");
  printf("  :nogui         - Set gui mode OFF\n");
  printf("  :trace         - Set trace mode ON\n");
  printf("  :notrace       - Set trace mode OFF\n");
  printf("  :showproxy     - Show proxy atoms\n");
  printf("  :hideproxy     - Hide proxy atoms\n");
  //printf("  :O[0-3]        - Set optimize level\n");
  printf("  :s[0-3]        - Set shuffle level\n");
  printf("  :h             - help\n");
  printf("  :q             - quit\n");
  printf("\n");
}

void run_jni_interactive()
{
  BOOL is_processing = TRUE;
  char line[1024];
  char *result = NULL;

  show_help();

  is_processing = jni_initialize_lmntal();

  while (is_processing) {
    printf("> ");
    if (fgets(line, 1024, stdin) == NULL) {
      fprintf(stderr, "FATAL ERROR: Can't read input stream.");
      break;
    }
    if (strchr(line, '\n') != NULL) {
      /* 改行文字を終端記号に置換する */
      line[strlen(line) - 1] = '\0';
    } else {
      /* 入力ストリームをクリアする */
      while(getchar() != '\r');
    }

    // TODO: showrulesオプション
    if (strlen(line) == 0) {
      printf("no-input\n");
    } else if (line[0] == ':') {
      if (line[1] == 'q' && strlen(line) == 2) {
        /* 終了 */
        is_processing = FALSE;
        printf("halt...\n");

      } else if (line[1] == 'h' && strlen(line) == 2) {
        /* ヘルプ表示 */
        show_help();

//      // TODO: compile.Optimizerクラスのロードが必要
//      } else if (line[1] == 'O' && strlen(line) == 3) {
//        /* 最適化レベルの指定 */
//        if (isdigit(line[2])) {
//          int l = line[2] - '0';
//          lmn_env.optimization_level = l <= OPTIMIZE_LEVEL_MAX ? l : OPTIMIZE_LEVEL_MAX;
//          printf("set optimize level %d \n", lmn_env.optimization_level);
//        }
#ifdef HAVE_JNI_H
      } else if (line[1] == 's' && strlen(line) == 3) {
        /* シャッフルレベルの指定 */
        if (isdigit(line[2])) {
          int s = line[2] - '0';
          jc_lmntal.shuffle_level = s <= JNI_ENV_SHUFFLE_MAX ? s : JNI_ENV_SHUFFLE_MAX;
          (*env)->SetStaticIntField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_shuffle, jc_lmntal.shuffle_level);
          printf("set optimize level %d \n", jc_lmntal.shuffle_level);
        }

      } else if (strcmp(&line[1], "gui") == 0) {
        /* guiモードON */
        /* guiモードでも、fInterpret=TRUE */
        /* 一度GUIを起動して、GUIの[x]ボタンを閉じると、Javaが終了し、SLIMも終了してしまう */
        (*env)->SetStaticBooleanField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_slimcode, JNI_FALSE);
        (*env)->SetStaticBooleanField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_compileonly, JNI_FALSE);
        (*env)->SetStaticBooleanField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_fGUI, JNI_TRUE);
        printf("gui mode on\n");

      } else if (strcmp(&line[1], "nogui") == 0) {
        /* guiモードOFF */
        (*env)->SetStaticBooleanField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_slimcode, JNI_TRUE);
        (*env)->SetStaticBooleanField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_compileonly, JNI_TRUE);
        (*env)->SetStaticBooleanField(env, jc_lmntal.c_Env, jc_lmntal.f_Env_fGUI, JNI_FALSE);
        printf("gui mode off\n");

      } else if (strcmp(&line[1], "nd") == 0) {
        /* ndモード、全実行パス表示 */
        lmn_env.nd = TRUE;
        printf("nd mode on\n");

      } else if (strcmp(&line[1], "nd_result") == 0) {
        /* ndモード、デッドロックパスのみ表示 */
        lmn_env.nd = TRUE;
        lmn_env.nd_result = TRUE;
        printf("nd_result mode on\n");

      } else if (strcmp(&line[1], "nd_dump") == 0) {
        /* ndモード、全状態逐次表示 */
        lmn_env.nd = TRUE;
        lmn_env.nd_dump = TRUE;
        printf("nd_dump mode on\n");

      } else if (strcmp(&line[1], "nond") == 0) {
        /* ndモード全解除 */
        lmn_env.nd = FALSE;
        lmn_env.nd_dump = FALSE;
        lmn_env.nd_result = FALSE;
        printf("nd(nd_dump,nd_result) mode off\n");

      } else if (strcmp(&line[1], "clear") == 0) {
        if (lmn_env.nd) {
          if (lmn_env.nd_remain) {
            lmn_env.nd_remaining = FALSE;
            lmn_env.nd_cleaning = TRUE;
            printf("clear remaining atoms (nd)\n");
          } else {
            printf("can't clear atoms in normal(=not remain) mode (nd)\n");
          }
        } else {
          if (lmn_env.normal_remain) {
            lmn_env.normal_remaining = FALSE;
            lmn_env.normal_cleaning = TRUE;
            printf("clear remaining atoms (normal)\n");
          } else {
            printf("can't clear atoms in normal(=not remain) mode (normal)\n");
          }
        }

      } else if (strcmp(&line[1], "remain") == 0) {
        /* remainモードON */
        if (lmn_env.nd) {
          lmn_env.nd_remain = TRUE;
          printf("remain mode on (nd)\n");
        } else {
          lmn_env.normal_remain = TRUE;
          printf("remain mode on (normal)\n");
        }

      } else if (strcmp(&line[1], "noremain") == 0) {
        /* remainモードON */
        if (lmn_env.nd) {
          lmn_env.nd_remain = FALSE;
          lmn_env.nd_cleaning = TRUE;
          printf("remain mode off (nd)\n");
        } else {
          lmn_env.normal_remain = FALSE;
          lmn_env.normal_cleaning = TRUE;
          printf("remain mode off (normal)\n");
        }
#endif
      } else if (strcmp(&line[1], "trace") == 0) {
        /* traceモードON */
        lmn_env.trace = TRUE;
        printf("trace mode on\n");

      } else if (strcmp(&line[1], "notrace") == 0) {
        /* traceモードOFF */
        lmn_env.trace = FALSE;
        printf("trace mode off\n");

      } else if (strcmp(&line[1], "showproxy") == 0) {
        /* プロキシ表示ON */
        lmn_env.show_proxy = TRUE;
        printf("show_proxy mode on\n");

      } else if (strcmp(&line[1], "hideproxy") == 0) {
        /* プロキシ表示OFF */
        lmn_env.show_proxy = FALSE;
        printf("show_proxy mode off\n");
      } else {
        printf("unknown command: %s\n",line);
      }
    } else if (jni_lmntal_compile(&result, line)) {
      auto tmpfp = file_ptr(tmpfile(), fclose); /* il_parserはFILE*しか受け取らない(?)ので、一時ファイルで対応する */
      if (tmpfp == NULL) {
        is_processing = FALSE; // 終了
      } else {
        fprintf(tmpfp.get(), "%s", result);
        rewind(tmpfp.get()); // ポインタを先頭に
        _run(std::move(tmpfp));
      }

      /* slimcode文字列の解放 */
      LMN_FREE(result);
    }
  }
  jni_finalize_lmntal();
}
