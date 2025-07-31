/*
 * memory_pool.c
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
 * $Id: memory_pool.c,v 1.2 2008/09/19 05:18:17 taisuke Exp $
 */

#include "memory_pool.h"
#include "lmntal.h"
#include <chrono>
#include <iomanip>
#include <sstream>
#include <string>
// ログ出力ON/OFF用のグローバル変数をexternで参照
extern bool enable_memory_pool_log; // alloc.cppで定義

/*
[変更履歴]
- enable_memory_pool_logフラグをexternで参照し、コマンドラインオプションでメモリプールログ出力をON/OFFできるようにした。
- 各種メモリプール操作ログ（log_memory_pool_event）にミリ秒精度のタイムスタンプを付与。
- memory_pool_new, memory_pool_malloc, memory_pool_free, memory_pool_deleteの各操作で詳細な動作を記録するようにした。
- 再帰呼び出し防止のためin_log変数を導入。
*/
#define REF_CAST(T, X) (*(T *)&(X))

/* each element must be bigger than void*, so align everything in sizeof(void*)
 * !! */
/* after alignment, X byte object needs ALIGNED_SIZE(X) byte. */
#define ALIGNED_SIZE(X)                                                        \
  (((X + sizeof(void *) - 1) / sizeof(void *)) * sizeof(void *))

// メモリプール専用ログ関数
static std::string now_str() {
    using namespace std::chrono;
    auto now = std::chrono::system_clock::now();
    auto t = std::chrono::system_clock::to_time_t(now);
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch()) % 1000;
    std::ostringstream oss;
    oss << std::put_time(std::localtime(&t), "%F %T")
        << "." << std::setfill('0') << std::setw(3) << ms.count();
    return oss.str();
}

static void log_memory_pool_event(const char* event, memory_pool* p, void* ptr, const char* details) {
  static int in_log = 0;
  if (in_log || !enable_memory_pool_log) return;
  in_log = 1;
  FILE* log = fopen("memory_pool.log", "a");
  if (log) {
    std::string ts = now_str();
    fprintf(log, "[%s] %s: pool=%p, ptr=%p, element_size=%d, block_head=%p, free_head=%p %s\n", 
            ts.c_str(), event, p, ptr, p ? p->sizeof_element : 0, p ? p->block_head : 0, p ? p->free_head : 0, details);
    fclose(log);
  }
  in_log = 0;
}

// 再利用追跡用のログ
static void log_reuse_event(const char* event, void* ptr, void* old_ptr) {
  static int in_log = 0;
  if (in_log) return;
  in_log = 1;
  FILE* log = fopen("memory_reuse_log.txt", "a");
  if (log) {
    fprintf(log, "%s: ptr=%p, old_ptr=%p\n", event, ptr, old_ptr);
    fclose(log);
  }
  in_log = 0;
}

memory_pool *memory_pool_new(int s) {
  memory_pool *res = LMN_MALLOC(memory_pool);

  res->sizeof_element = ALIGNED_SIZE(s);
  res->block_head = 0;
  res->free_head = 0;

  log_memory_pool_event("POOL_NEW", res, 0, "");
  /* fprintf(stderr, "this memory_pool allocate %d, aligned as %d\n", s,
   * res->sizeof_element); */

  return res;
}

static const int blocksize = 8;
void *memory_pool_malloc(memory_pool *p) {
  void *res;

  if (p->free_head == 0) {
    char *rawblock;
    int i;

    log_memory_pool_event("POOL_ALLOC_NEW_BLOCK", p, 0, "creating new block");
    /* fprintf(stderr, "no more free space, so allocate new block\n"); */

    /* top of block is used as pointer to head of next block */
    rawblock = (char *)lmn_malloc(ALIGNED_SIZE(sizeof(void *)) +
                                  p->sizeof_element * blocksize);
    *(void **)rawblock = p->block_head;
    p->block_head = rawblock;

    /* rest is used as space for elements */
    /* skip top of block */
    rawblock = rawblock + ALIGNED_SIZE(sizeof(void *));
    p->free_head = rawblock;

    for (i = 0; i < (blocksize - 1); i++) {
      /* top of each empty elements is used as pointer to next empty element */
      REF_CAST(void *, rawblock[p->sizeof_element * i]) =
          &rawblock[p->sizeof_element * (i + 1)];
    }
    REF_CAST(void *, rawblock[p->sizeof_element * (blocksize - 1)]) = 0;
  }

  res = p->free_head;
  p->free_head = *(void **)p->free_head;

  log_memory_pool_event("POOL_MALLOC", p, res, "allocated from pool");
  return res;
}

void memory_pool_free(memory_pool *p, void *e) {
  if (p) {
    log_memory_pool_event("POOL_FREE", p, e, "returning to pool");
    *(void **)e = p->free_head;
    p->free_head = e;
  }
}

void memory_pool_delete(memory_pool *p) {
  void *blockhead = p->block_head;

  log_memory_pool_event("POOL_DELETE", p, 0, "deleting pool");
  while (blockhead) {
    void *next_blockhead = *(void **)blockhead;
    lmn_free(blockhead);
    blockhead = next_blockhead;
  }

  lmn_free(p);
}

/*
int main()
{
  memory_pool *p = memory_pool_new(5);

  int i;
  void *x[20];
  for(i=0; i<10; ++i){
    x[i] = memory_pool_malloc(p);
    printf("ok. allocate %dth element on %p\n", i, x[i]);
  }

  for(i=0; i<10; ++i){
    memory_pool_free(p, x[i]);
    printf("ok. free %dth element on %p\n", i, x[i]);
  }

  for(i=0; i<20; ++i){
    x[i] = memory_pool_malloc(p);
    printf("ok. allocate %dth element on %p\n", i, x[i]);
  }

  memory_pool_delete(p);
  return 0;
}
*/
