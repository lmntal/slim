#ifdef USE_JNI

#ifndef HAVE___INT64
#define __int64 long long
#endif

#include <jni.h>

#define JNI_ENV_SHUFFLE_INIT    0
#define JNI_ENV_SHUFFLE_DEFUALT 3
#define JNI_ENV_SHUFFLE_MAX     3

struct JniContextLmntal {
	/* 情報 */
	int shuffle_level;
	/* 標準入出力の操作に必要なクラス、メソッド、フィールド */
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
};

char* run_cygpath(const char* option, const char* arg);
void run_jni_interactive();

#endif /* USE_JNI */
