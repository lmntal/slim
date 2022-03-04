/* This is a public domain general purpose hash table package written by Peter
 * Moore @ UCB. */

/* @(#) st.h 5.1 89/12/14 */

/** http://sobjc.googlecode.com/svn/trunk/runtime/st.cを元に変更した。
 *  st.h,st.cはパブリックドメインのハッシュテーブルライブラリで、rubyでも使われている。
 *  オリジナルのst.h,st.cってどこから入手できるのだろうか？
 */

#ifndef ST_INCLUDED
#define ST_INCLUDED

/**
 * @ingroup Element
 * @defgroup StateTable
 * @{
 */

#include <stddef.h>

#include <iterator>
#include <utility>

#ifndef _
#define _(args) args
#endif
#ifndef ANYARGS
#ifdef __cplusplus
#define ANYARGS ...
#else
#define ANYARGS
#endif
#endif

typedef unsigned long st_data_t;
typedef struct st_table *st_table_t;

typedef int (*st_cmp_func)(void *, void *);
typedef long (*st_hash_func)(void *);
typedef int (*st_iter_func)(ANYARGS);

struct st_hash_type {
  int (*compare)(
      void *,
      void
          *); /* 対象の2つのエントリー(st_table_entry)が同じチェインに属するならば偽、そうでなければ真を返す関数
               */
  long (*hash)(void *); /* ハッシュ関数 */
};

/* num_bins = 5, num_entries = 3 なる struct st_table_entry **bins の例
 * ("→"はポインタ, NULLはポインタの指し示す先が存在しないことを表す)
 *
 *  bins[0]→(st_table_entry)→(st_table_entry)
 *  bins[1]→ NULL
 *  bins[2]→(st_table_entry)
 *  bins[3]→ NULL
 *  bins[4]→ NULL
 */
struct st_table {
  struct st_hash_type *type;
  unsigned long num_bins;       /* ハッシュ表のサイズ(スロット数) */
  unsigned long num_entries;    /* ハッシュ表に放り込まれた要素の個数
                         (各スロットは同一のハッシュ値を持つ要素を格納する(Linked)リスト構造(struct
                         st_table_entry *)を持ち、
                          同じリスト内に放り込まれた各要素は別個にカウントする) */
  struct st_table_entry **bins; /* チェイン法に基づくハッシュ表本体 */
};

#define st_is_member(table, key) st_lookup(table, key, (st_data_t *)0)

enum st_retval { ST_CONTINUE, ST_STOP, ST_DELETE, ST_CHECK };

#include "../lmntal.h"
#include "lmntal_thread.h"

static inline unsigned long st_num(st_table_t table) {
  return table->num_entries;
}

static inline unsigned long st_cap(st_table_t table) {
  return table->num_bins;
}

st_table_t st_init_table(struct st_hash_type *t);
st_table_t st_init_table_with_size(struct st_hash_type *t, int size);
st_table_t st_init_statetable(void);
st_table_t st_init_statetable_with_size(int size);
st_table_t st_init_numtable(void);
st_table_t st_init_numtable_with_size(int size);
st_table_t st_init_strtable(void);
st_table_t st_init_strtable_with_size(int size);
st_table_t st_init_ptrtable(void);
st_table_t st_init_ptrtable_with_size(int size);
int st_delete(st_table_t tbl, st_data_t key, st_data_t *value);
int st_delete_safe(st_table_t tbl, st_data_t *key, st_data_t *value, st_data_t never);
int st_insert(st_table_t tbl, st_data_t key, st_data_t value);
int st_insert_safe(st_table_t tbl, st_data_t key, st_data_t value);
int st_lookup(st_table_t tbl, st_data_t key, st_data_t *value);
int st_lookup_with_col(st_table_t tbl, st_data_t key, st_data_t *value, long *n_col);
int st_contains(st_table_t tbl, st_data_t key);
int st_foreach(st_table_t tbl, st_iter_func f, st_data_t arg);
void st_add_direct(st_table_t tbl, st_data_t key, st_data_t value);
unsigned long st_table_space(st_table_t tbl);
void st_free_table(st_table_t tbl);
void st_cleanup_safe(st_table_t tbl, st_data_t never);
void st_clear(st_table_t tbl);
st_table_t st_copy(st_table_t tbl);
void st_print(st_table_t tbl);
void st_get_entries_key(st_table_t tbl, Vector *vec);
void st_get_entries_value(st_table_t tbl, Vector *vec);
int st_equals(st_table_t tbl1, st_table_t tbl2);
long st_strhash(const char *str);
int st_numcmp(long num1, long num2);
long st_numhash(long num);
long st_statehash(LmnWord state);
int st_foreach_hash(st_table_t table, st_data_t hash, int (*func)(ANYARGS), st_data_t arg);

/* tbl1にtbl2のすべてのエントリを追加する。tbl1とtbl2に同じキーを持つエ
   ントリが存在する場合の動作は（とりあえず）未定義。 */
void st_concat(st_table_t tbl1, const st_table_t tbl2);

unsigned long st_table_space(st_table_t table);

class st_iterator {
 public:
  using iterator_category = std::input_iterator_tag;
  using value_type = std::pair<st_data_t, st_data_t>;
  using difference_type = std::ptrdiff_t;
  using pointer = std::pair<st_data_t, st_data_t> *;
  using reference = std::pair<st_data_t, st_data_t> &;

 private:
  st_table_t table;
  size_t bin_index;
  st_table_entry *ptr;
  value_type value;

 public:
  st_iterator() : ptr(nullptr) {
  }
  st_iterator(st_table_t table);
  st_iterator(const st_iterator &it);
  ~st_iterator() noexcept = default;

  st_iterator &operator=(const st_iterator &it) noexcept;
  reference operator*() {
    return value;
  }
  pointer operator->() {
    return &value;
  }
  st_iterator &operator++();
  st_iterator operator++(int i) {
    auto s = st_iterator(*this);
    ++(*this);
    return s;
  }

  bool operator!=(const st_iterator &a) {
    return !(*this == a);
  }
  bool operator==(const st_iterator &a) {
    return ptr == a.ptr;
  }
};

inline st_iterator begin(const st_table_t &table) {
  return (table) ? st_iterator(table) : st_iterator();
}
inline st_iterator end(const st_table_t &table) {
  return st_iterator();
}

/* @} */

#endif /* ST_INCLUDED */
