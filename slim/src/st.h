/* This is a public domain general purpose hash table package written by Peter Moore @ UCB. */

/* @(#) st.h 5.1 89/12/14 */

/* http://sobjc.googlecode.com/svn/trunk/runtime/st.c
 を元に変更した。
 st.h,st.cはパブリックドメインのハッシュテーブルライブラリで、
 rubyでも使われている。オリジナルのst.h,st.cってどこから入手
 できるのだろうか？ */

#ifndef ST_INCLUDED
#define ST_INCLUDED

#include <stddef.h>

typedef long st_data_t;
typedef struct st_table st_table, *st_table_t;

struct st_hash_type {
  int (*compare)(); /* 対象の2つのエントリー(st_table_entry)が同じチェインに属するならば偽、そうでなければ真を返す関数 */
  long (*hash)();    /* ハッシュ関数 */
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
  int num_bins; /* ハッシュ表のサイズ(スロット数) */
  int num_entries; /* ハッシュ表に放り込まれた要素の個数
                      (各スロットは同一のハッシュ値を持つ要素を格納する(Linked)リスト構造(struct st_table_entry *)を持ち、
                       同じリスト内に放り込まれた各要素は別個にカウントする) */
  struct st_table_entry **bins; /* チェイン法に基づくハッシュ表本体 */
};

#define st_is_member(table,key) st_lookup(table,key,(st_data_t *)0)

enum st_retval {
  ST_CONTINUE, ST_STOP, ST_DELETE, ST_CHECK
};

#ifndef _
# define _(args) args
#endif
#ifndef ANYARGS
# ifdef __cplusplus
#   define ANYARGS ...
# else
#   define ANYARGS
# endif
#endif

st_table *st_init_table(struct st_hash_type *);
st_table *st_init_table_with_size(struct st_hash_type *, int);
st_table *st_init_numtable(void);
st_table *st_init_numtable_with_size(int);
st_table *st_init_strtable(void);
st_table *st_init_strtable_with_size(int);
st_table *st_init_ptrtable(void);
st_table *st_init_ptrtable_with_size(int);
int st_delete(st_table *, st_data_t , st_data_t *);
int st_delete_safe(st_table *, st_data_t *, st_data_t *, st_data_t);
int st_insert(st_table *, st_data_t, st_data_t);
int st_lookup(st_table *, st_data_t, st_data_t *);
int st_foreach(st_table *, int(*)(ANYARGS), st_data_t);
void st_add_direct(st_table *, st_data_t, st_data_t);
unsigned int st_num(st_table *);
void st_free_table(st_table *);
void st_cleanup_safe(st_table *, st_data_t);
st_table *st_copy(st_table *);
void st_print(st_table *st);
long st_strhash(const char *);
int st_numcmp(long, long);
long st_numhash(long);

#endif /* ST_INCLUDED */
