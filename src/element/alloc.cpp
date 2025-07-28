/*
 * alloc.c -- memory management
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
 * <lmntal@ueda.info.waseda.ac.jp> All rights reserved.
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
 * $Id: alloc.c,v 1.3 2008/09/19 05:18:17 taisuke Exp $
 */

#include "error.h"
#include "lmntal.h"
#include "lmntal_thread.h"
#include "memory_pool.h"
#include "util.h"
#include "vector.h"
#include "vm/vm.h"
#include <fstream>
#include <mutex>
#include <chrono>
#include <iomanip>
#include <sstream>
#include <string>
#include <cstring>

/*----------------------------------------------------------------------
 * memory allocation for atom
 */

static memory_pool **atom_memory_pools[128];

// ログ出力ON/OFF用のグローバル変数を追加（コマンドラインオプションで制御可能に）
bool enable_atom_pool_log = false;
bool enable_memory_pool_log = false;
bool enable_alloc_log = false;
bool enable_memory_log = false;

// ミリ秒精度のタイムスタンプを取得する関数を追加
static std::string now_str() {
    using namespace std::chrono;
    auto now = system_clock::now();
    auto t = system_clock::to_time_t(now);
    auto ms = duration_cast<milliseconds>(now.time_since_epoch()) % 1000;
    std::ostringstream oss;
    oss << std::put_time(std::localtime(&t), "%F %T")
        << "." << std::setfill('0') << std::setw(3) << ms.count();
    return oss.str();
}

// ログ関数のマクロ定義
#define LOG_ALLOC_EVENT(event, ptr, size) log_alloc_event(event, ptr, size, __FILE__, __LINE__, __func__)
#define LOG_ALLOC_FUNC(func, fmt, ...) log_alloc_func(func, __FILE__, __LINE__, __func__, fmt, ##__VA_ARGS__)

// アトムプール専用ログ関数
static void log_atom_pool_event(const char* event, int arity, int cid, void* ptr, const char* details) {
  static int in_log = 0;
  if (in_log || !enable_atom_pool_log) return;
  in_log = 1;
  FILE* log = fopen("atom_pool_log.txt", "a");
  if (log) {
    std::string ts = now_str();
    fprintf(log, "[%s] %s: arity=%d, thread=%d, ptr=%p %s\n", ts.c_str(), event, arity, cid, ptr, details);
    fclose(log);
  }
  in_log = 0;
}

// 再利用追跡用のログ
static void log_atom_reuse_event(const char* event, void* ptr, void* old_ptr, int arity, int cid) {
  static int in_log = 0;
  if (in_log || !enable_atom_pool_log) return;
  in_log = 1;
  FILE* log = fopen("atom_reuse_log.txt", "a");
  if (log) {
    std::string ts = now_str();
    fprintf(log, "[%s] %s: ptr=%p, old_ptr=%p, arity=%d, thread=%d\n", ts.c_str(), event, ptr, old_ptr, arity, cid);
    fclose(log);
  }
  in_log = 0;
}

// log_alloc_event: 呼び出し元情報付き
static void log_alloc_event(const char* event, void* ptr, size_t size, const char* file, int line, const char* caller) {
  static int in_log = 0;
  if (in_log || !enable_memory_log) return; // memory_log.txt用
  in_log = 1;
  FILE* log = fopen("memory_log.txt", "a");
  if (log) {
    std::string ts = now_str();
    if (size)
      fprintf(log, "[%s] %s: ptr=%p, size=%zu [at %s:%d %s]\n", ts.c_str(), event, ptr, size, file, line, caller);
    else
      fprintf(log, "[%s] %s: ptr=%p [at %s:%d %s]\n", ts.c_str(), event, ptr, file, line, caller);
    fclose(log);
  }
  in_log = 0;
}

// log_alloc_func: 呼び出し元情報付き
static void log_alloc_func(const char* func, const char* file, int line, const char* caller, const char* fmt, ...) {
  static int in_log = 0;
  if (in_log || !enable_alloc_log) return; // alloc_log.txt用
  in_log = 1;
  FILE* log = fopen("alloc_log.txt", "a");
  if (log) {
    std::string ts = now_str();
    fprintf(log, "[%s] %s: ", ts.c_str(), func);
    va_list args;
    va_start(args, fmt);
    vfprintf(log, fmt, args);
    va_end(args);
    fprintf(log, " [at %s:%d %s]\n", file, line, caller);
    fclose(log);
  }
  in_log = 0;
}

void mpool_init() {
  LOG_ALLOC_FUNC("mpool_init", "called");
  int i, core_num, arity_num;
  arity_num = ARY_SIZEOF(atom_memory_pools);
  core_num = lmn_env.core_num;
  for (i = 0; i < arity_num; i++) {
    atom_memory_pools[i] =
        (memory_pool **)malloc(sizeof(memory_pool *) * core_num);
    memset(atom_memory_pools[i], 0, sizeof(memory_pool *) * core_num);
  }
  log_atom_pool_event("POOL_INIT", 0, 0, 0, "initialized all pools");
}

LmnSymbolAtomRef lmn_new_atom(LmnFunctor f) {
  LOG_ALLOC_FUNC("lmn_new_atom", "f=%u", (unsigned)f);
  LmnSymbolAtomRef ap;
  int arity, cid;
  arity = LMN_FUNCTOR_ARITY(lmn_functor_table, f);
  cid = env_my_thread_id();

  if (atom_memory_pools[arity][cid] == 0) {
    atom_memory_pools[arity][cid] = memory_pool_new(LMN_SATOM_SIZE(arity));
    log_atom_pool_event("POOL_CREATE", arity, cid, 0, "created new pool");
  }
  ap = (LmnSymbolAtomRef)memory_pool_malloc(atom_memory_pools[arity][cid]);
  ap->set_functor(f);
  ap->set_id(0);

  ap->record_flag = false;
  log_atom_pool_event("ATOM_NEW", arity, cid, ap, "allocated new atom");
  return ap;
}

void lmn_delete_atom(LmnSymbolAtomRef ap) {
  LOG_ALLOC_FUNC("lmn_delete_atom", "ap=%p", ap);
  int arity, cid;

  env_return_id(ap->get_id());

  arity = LMN_FUNCTOR_ARITY(lmn_functor_table, ap->get_functor());
  cid = env_my_thread_id();
  log_atom_pool_event("ATOM_DELETE", arity, cid, ap, "deleting atom");
  memory_pool_free(atom_memory_pools[arity][cid], ap);
}

void free_atom_memory_pools(void) {
  LOG_ALLOC_FUNC("free_atom_memory_pools", "called");
  unsigned int i, j, arity_num, core_num;

  arity_num = ARY_SIZEOF(atom_memory_pools);
  core_num = lmn_env.core_num;
  for (i = 0; i < arity_num; i++) {
    for (j = 0; j < core_num; j++) {
      if (atom_memory_pools[i][j]) {
        log_atom_pool_event("POOL_DELETE", i, j, 0, "deleting pool");
        memory_pool_delete(atom_memory_pools[i][j]);
      }
    }
    lmn_free(atom_memory_pools[i]);
  }
  log_atom_pool_event("POOL_CLEANUP", 0, 0, 0, "all pools cleaned up");
}

/*----------------------------------------------------------------------
 * memory allocation for membrane
 */

/* in membrane.c */
/* lmn_mem_make / lmn_mem_delete */

/*----------------------------------------------------------------------
 * low level allocation
 */

/* TODO:
 *   headerに持っていってstatic inlineにした方が良い?
 *   memory exhausted時にもprofile情報をdumpさせたい */

void *lmn_calloc(size_t num, size_t size) {
  LOG_ALLOC_FUNC("lmn_calloc", "num=%zu, size=%zu", num, size);
  void *result;
#if HAVE_DECL_CALLOC
  result = calloc(num, size);
  if (!result) {
    lmn_fatal("Memory exhausted");
  }
#else
  result = lmn_malloc(num * size);
  memset(result, 0x00, num * size);
#endif
  LOG_ALLOC_EVENT("calloc", result, num * size);
  return result;
}

void *lmn_malloc(size_t num) {
  LOG_ALLOC_FUNC("lmn_malloc", "num=%zu", num);
  LMN_ASSERT(num > 0);
  void *result = malloc(num);
  if (!result)
    lmn_fatal("Memory exhausted");
  LOG_ALLOC_EVENT("malloc", result, num);
  return result;
}

void *lmn_realloc(void *p, size_t num) {
  LOG_ALLOC_FUNC("lmn_realloc", "p=%p, num=%zu", p, num);
  void *result;
  if (!p) {
    result = lmn_malloc(num);
    LOG_ALLOC_EVENT("realloc(new)", result, num);
    return result;
  }
  result = realloc(p, num);
  if (!result)
    lmn_fatal("Memory exhausted");
  LOG_ALLOC_EVENT("realloc", result, num);
  return result;
}

void lmn_free(void *p) {
  LOG_ALLOC_FUNC("lmn_free", "p=%p", p);
  LOG_ALLOC_EVENT("free", p, 0);
  free(p);
}

void* operator new(std::size_t num) {
  LOG_ALLOC_FUNC("operator new", "num=%zu", num);
  void* p = lmn_malloc(num);
  LOG_ALLOC_EVENT("operator new", p, num);
  return p;
}

void* operator new[](std::size_t num) {
  LOG_ALLOC_FUNC("operator new[]", "num=%zu", num);
  void* p = lmn_malloc(num);
  LOG_ALLOC_EVENT("operator new[]", p, num);
  return p;
}

void operator delete(void* p) noexcept {
  LOG_ALLOC_FUNC("operator delete", "p=%p", p);
  LOG_ALLOC_EVENT("operator delete", p, 0);
  lmn_free(p);
}

void operator delete[](void* p) noexcept {
  LOG_ALLOC_FUNC("operator delete[]", "p=%p", p);
  LOG_ALLOC_EVENT("operator delete[]", p, 0);
  lmn_free(p);
}

void operator delete(void* p, std::size_t num) noexcept {
  LOG_ALLOC_FUNC("operator delete(size)", "p=%p, num=%zu", p, num);
  LOG_ALLOC_EVENT("operator delete(size)", p, num);
  lmn_free(p);
}

void operator delete[](void* p, std::size_t num) noexcept {
  LOG_ALLOC_FUNC("operator delete[](size)", "p=%p, num=%zu", p, num);
  LOG_ALLOC_EVENT("operator delete[](size)", p, num);
  lmn_free(p);
}

