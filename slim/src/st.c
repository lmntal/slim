/* This is a public domain general purpose hash table package written by Peter Moore @ UCB. */

/* static char  sccsid[] = "@(#) st.c 5.1 89/12/14 Crucible"; */

#include "config.h"
#include "string.h"
#include <stdio.h>
#include <stdlib.h>
#include "st.h"

typedef struct st_table_entry st_table_entry;

struct st_table_entry {
  unsigned long hash;
  st_data_t key;
  st_data_t record;
  st_table_entry *next;
};

#define ST_DEFAULT_MAX_DENSITY 5
#define ST_DEFAULT_INIT_TABLE_SIZE 11

/*
 * DEFAULT_MAX_DENSITY is the default for the largest we allow the
 * average number of items per bin before increasing the number of
 * bins
 *
 * DEFAULT_INIT_TABLE_SIZE is the default for the number of bins
 * allocated initially
 *
 */

static struct st_hash_type type_numhash = { st_numcmp, st_numhash, };

/* extern int strcmp(const char *, const char *); */
static struct st_hash_type type_strhash = { strcmp, st_strhash, };

/* ST Objective-C additions */
static int st_ptrcmp(void *, void *);
static long st_ptrhash(void *);
static struct st_hash_type type_ptrhash = { st_ptrcmp, st_ptrhash };

static void rehash(st_table *);

#define alloc(type) (type*)malloc((size_t)sizeof(type))
#define Calloc(n,s) (char*)calloc((n),(s))

#define EQUAL(table,x,y) ((x)==(y) || (*table->type->compare)((x),(y)) == 0)

#define do_hash(key,table) (unsigned long)(*(table)->type->hash)((key))
#define do_hash_bin(key,table) (do_hash(key, table)%(table)->num_bins)

/*
 * MINSIZE is the minimum size of a dictionary.
 */

#define MINSIZE 8

/*
 Table of prime numbers 2^n+a, 2<=n<=30.
 */
static long primes[] = {
  8 + 3,
  16 + 3,
  32 + 5,
  64 + 3,
  128 + 3,
  256 + 27,
  512 + 9,
  1024 + 9,
  2048 + 5,
  4096 + 3,
  8192 + 27,
  16384 + 43,
  32768 + 3,
  65536 + 45,
  131072 + 29,
  262144 + 3,
  524288 + 21,
  1048576 + 7,
  2097152 + 17,
  4194304 + 15,
  8388608 + 9,
  16777216 + 43,
  33554432 + 35,
  67108864 + 15,
  134217728 + 29,
  268435456 + 3,
  536870912 + 11,
  1073741824 + 85,
  0 };

static int new_size(int size) {
  int i;

#if 0
  for (i=3; i<31; i++) {
    if ((1<<i)> size) return 1<<i;
  }
  return -1;
#else
  int newsize;

  for (i = 0, newsize = MINSIZE; i < (int) (sizeof(primes) / sizeof(primes[0])); i++, newsize <<= 1) {
    if (newsize > size)
      return primes[i];
  }
  /* Ran out of polynomials */
  return -1; /* should raise exception */
#endif
}

#ifdef HASH_LOG
static int collision = 0;
static int init_st = 0;

static void
stat_col()
{
  FILE *f = fopen("/tmp/col", "w");
  fprintf(f, "collision: %d\n", collision);
  fclose(f);
}
#endif

st_table*
st_init_table_with_size(struct st_hash_type *type, int size) {
  st_table *tbl;

#ifdef HASH_LOG
  if (init_st == 0) {
    init_st = 1;
    atexit(stat_col);
  }
#endif

  size = new_size(size); /* round up to prime number */

  tbl = alloc(st_table);
  tbl->type = type;
  tbl->num_entries = 0;
  tbl->num_bins = size;
  tbl->bins = (st_table_entry **) Calloc(size, sizeof(st_table_entry*));

  return tbl;
}

st_table*
st_init_table(struct st_hash_type *type) {
  return st_init_table_with_size(type, 0);
}

st_table*
st_init_numtable(void) {
  return st_init_table(&type_numhash);
}

st_table*
st_init_numtable_with_size(int size) {
  return st_init_table_with_size(&type_numhash, size);
}

st_table*
st_init_strtable(void) {
  return st_init_table(&type_strhash);
}

st_table*
st_init_strtable_with_size(int size) {
  return st_init_table_with_size(&type_strhash, size);
}

st_table*
st_init_ptrtable(void) {
  return st_init_table(&type_ptrhash);
}

st_table*
st_init_ptrtable_with_size(int size) {
  return st_init_table_with_size(&type_ptrhash, size);
}

void st_free_table(st_table *table) {
  register st_table_entry *ptr, *next;
  int i;

  for (i = 0; i < table->num_bins; i++) {
    ptr = table->bins[i];
    while (ptr != 0) {
      next = ptr->next;
      free(ptr);
      ptr = next;
    }
  }
  free(table->bins);
  free(table);
}

#define PTR_NOT_EQUAL(table, ptr, hash_val, key) \
((ptr) != 0 && (ptr->hash != (hash_val) || !EQUAL((table), (key), (ptr)->key)))

#ifdef HASH_LOG
#define COLLISION collision++
#else
#define COLLISION
#endif

#define FIND_ENTRY(table, ptr, hash_val, bin_pos) do {\
    bin_pos = hash_val%(table)->num_bins;\
    ptr = (table)->bins[bin_pos];\
    if (PTR_NOT_EQUAL(table, ptr, hash_val, key)) {\
      COLLISION;\
      while (PTR_NOT_EQUAL(table, ptr->next, hash_val, key)) {\
          ptr = ptr->next;\
      }\
      ptr = ptr->next;\
    }\
} while (0)

#define FIND_ENTRY_WITH_COL(table, ptr, hash_val, bin_pos, collision) do {   \
    bin_pos = hash_val%(table)->num_bins;\
    ptr = (table)->bins[bin_pos];\
    if (PTR_NOT_EQUAL(table, ptr, hash_val, key)) {\
      if ((ptr) != 0 && (ptr->hash == (hash_val))) (collision)++;   \
      while (PTR_NOT_EQUAL(table, ptr->next, hash_val, key)) {\
          ptr = ptr->next;\
          if ((ptr) != 0 && (ptr->hash == (hash_val))) (collision)++;   \
      }\
      ptr = ptr->next;\
    }\
} while (0)

/* キーがkeyであるテーブルの値をvalueに設定する。
 キーが見つからなければ0を返し，見つかれば1を返す。*/
int st_lookup(st_table *table, register st_data_t key, st_data_t *value) {
  unsigned long hash_val, bin_pos;
  register st_table_entry *ptr;

  hash_val = do_hash(key, table);
  FIND_ENTRY(table, ptr, hash_val, bin_pos);

  if (ptr == 0) {
    return 0;
  } else {
    if (value != 0)
      *value = ptr->record;
    return 1;
  }
}


/* キーがkeyであるテーブルの値をvalueに設定する。
 キーが見つからなければ0を返し，見つかれば1を返す。*/
int st_lookup_with_col(st_table *table, register st_data_t key, st_data_t *value, long *n_col) {
  unsigned long hash_val, bin_pos;
  register st_table_entry *ptr;

  *n_col = 0;
  hash_val = do_hash(key, table);
  FIND_ENTRY_WITH_COL(table, ptr, hash_val, bin_pos, *n_col);

  if (ptr == 0) {
    return 0;
  } else {
    if (value != 0)
      *value = ptr->record;
    return 1;
  }
}

int st_contains(st_table *table, st_data_t key) {
  st_data_t t;
  return st_lookup(table, key, &t);
}

#define ADD_DIRECT(table, key, value, hash_val, bin_pos)\
do {\
    st_table_entry *entry;\
    if (table->num_entries/(table->num_bins) > ST_DEFAULT_MAX_DENSITY) {\
        rehash(table);\
        bin_pos = hash_val % table->num_bins;\
    }\
    \
    entry = alloc(st_table_entry);\
    \
    entry->hash = hash_val;\
    entry->key = key;\
    entry->record = value;\
    entry->next = table->bins[bin_pos];\
    table->bins[bin_pos] = entry;\
    table->num_entries++;\
} while (0)

/* ハッシュ表に新たなエントリーを追加する。エントリが存在した場合はその
   エントリの値のみを更新し、キーは元々のものを更新しない。 */
int st_insert(register st_table *table, register st_data_t key, st_data_t value) {
  unsigned long hash_val, bin_pos;
  register st_table_entry *ptr;

  hash_val = do_hash(key, table);
  FIND_ENTRY(table, ptr, hash_val, bin_pos);

  if (ptr == 0) {
    ADD_DIRECT(table, key, value, hash_val, bin_pos);
    return 0;
  } else {
    ptr->record = value;
    return 1;
  }
}

/* ハッシュ表に新たなエントリーを追加し正の値を返す。エントリが存在した
   場合は、テーブルを変更せずに、0を返す。*/
int st_insert_safe(register st_table *table, register st_data_t key, st_data_t value) {
  unsigned long hash_val, bin_pos;
  register st_table_entry *ptr;

  hash_val = do_hash(key, table);
  FIND_ENTRY(table, ptr, hash_val, bin_pos);

  if (ptr == 0) {
    ADD_DIRECT(table, key, value, hash_val, bin_pos);
    return 1;
  } else {
    return 0;
  }
}

/* 値の重複をチェックせずにハッシュ表に新たなエントリーを追加する */
void st_add_direct(st_table *table, st_data_t key, st_data_t value) {
  unsigned long hash_val, bin_pos;

  hash_val = do_hash(key, table);
  bin_pos = hash_val % table->num_bins;
  ADD_DIRECT(table, key, value, hash_val, bin_pos);
}

static void rehash(register st_table *table) {
  register st_table_entry *ptr, *next, **new_bins;
  int i, old_num_bins = table->num_bins, new_num_bins;
  unsigned long hash_val;

  new_num_bins = new_size(old_num_bins + 1);
  new_bins = (st_table_entry**) Calloc(new_num_bins, sizeof(st_table_entry*));

  for (i = 0; i < old_num_bins; i++) {
    ptr = table->bins[i];
    while (ptr != 0) {
      next = ptr->next;
      hash_val = ptr->hash % new_num_bins;
      ptr->next = new_bins[hash_val];
      new_bins[hash_val] = ptr;
      ptr = next;
    }
  }
  free(table->bins);
  table->num_bins = new_num_bins;
  table->bins = new_bins;
}

st_table*
st_copy(st_table *old_table) {
  st_table *new_table;
  st_table_entry *ptr, *entry;
  int i, num_bins = old_table->num_bins;

  new_table = alloc(st_table);
  if (new_table == 0) {
    return 0;
  }

  *new_table = *old_table;
  new_table->bins = (st_table_entry**) Calloc((unsigned)num_bins, sizeof(st_table_entry*));

  if (new_table->bins == 0) {
    free(new_table);
    return 0;
  }

  for (i = 0; i < num_bins; i++) {
    new_table->bins[i] = 0;
    ptr = old_table->bins[i];
    while (ptr != 0) {
      entry = alloc(st_table_entry);
      if (entry == 0) {
        free(new_table->bins);
        free(new_table);
        return 0;
      }
      *entry = *ptr;
      entry->next = new_table->bins[i];
      new_table->bins[i] = entry;
      ptr = ptr->next;
    }
  }
  return new_table;
}

int st_delete(register st_table *table, register st_data_t key,
    st_data_t *value) {
  unsigned long hash_val;
  st_table_entry *tmp;
  register st_table_entry *ptr;

  hash_val = do_hash_bin(key, table);
  ptr = table->bins[hash_val];

  if (ptr == 0) {
    if (value != 0)
      *value = 0;
    return 0;
  }

  if (EQUAL(table, key, ptr->key)) {
    table->bins[hash_val] = ptr->next;
    table->num_entries--;
    if (value != 0)
      *value = ptr->record;
    free(ptr);
    return 1;
  }

  for (; ptr->next != 0; ptr = ptr->next) {
    if (EQUAL(table, ptr->next->key, key)) {
      tmp = ptr->next;
      ptr->next = ptr->next->next;
      table->num_entries--;
      if (value != 0)
        *value = tmp->record;
      free(tmp);
      return 1;
    }
  }

  return 0;
}

int st_delete_safe(register st_table *table, register st_data_t *key,
    st_data_t *value, st_data_t never) {
  unsigned long hash_val;
  register st_table_entry *ptr;

  hash_val = do_hash_bin(*key, table);
  ptr = table->bins[hash_val];

  if (ptr == 0) {
    if (value != 0)
      *value = 0;
    return 0;
  }

  for (; ptr != 0; ptr = ptr->next) {
    if ((ptr->key != never) && EQUAL(table, ptr->key, *key)) {
      table->num_entries--;
      *key = ptr->key;
      if (value != 0)
        *value = ptr->record;
      ptr->key = ptr->record = never;
      return 1;
    }
  }

  return 0;
}

static int delete_never(st_data_t key __attribute__((unused)), st_data_t value, st_data_t never) {
  if (value == never)
    return ST_DELETE;
  return ST_CONTINUE;
}

void st_cleanup_safe(st_table *table, st_data_t never) {
  int num_entries = table->num_entries;

  st_foreach(table, delete_never, never);
  table->num_entries = num_entries;
}

/* テーブルの各要素に対し，第一引数にキー，第二引数に値，
 * 第三引数にargでfuncを呼び出す．
 *
 * funcの戻り値にST_CHECK，ST_CONTINUE，ST_STOP，ST_DELETE
 * のいずれを指定するかで，テーブルの要素を対象に任意の処理を実行することができる．
 * 例えばST_DELETEを戻り値に指定した場合は，テーブル内のすべてのエントリーを利用して
 * 任意の処理を実行した後，テーブルを空にすることができる．
 *
 * なお，第三引数argは，funcの処理にテーブル内の要素以外のデータが必要な場合に利用する．
 */
int st_foreach(st_table *table, int(*func)( ANYARGS), st_data_t arg) {
  st_table_entry *ptr, *last, *tmp;
  enum st_retval retval;
  int i;

  for (i = 0; i < table->num_bins; i++) {
    last = 0;
    for (ptr = table->bins[i]; ptr != 0;) { /* エントリーの存在するチェインでのみfuncが呼ばれる */
      retval = (*func)(ptr->key, ptr->record, arg);
      switch (retval) {
      case ST_CHECK: /* check if hash is modified during iteration */
        tmp = 0;
        if (i < table->num_bins) {
          for (tmp = table->bins[i]; tmp; tmp = tmp->next) {
            if (tmp == ptr)
              break;
          }
        }
        if (!tmp) {
          /* call func with error notice */
          return 1;
        }
        /* fall through */
      case ST_CONTINUE:
        last = ptr;
        ptr = ptr->next;
        break;
      case ST_STOP:
        return 0;
      case ST_DELETE:
        tmp = ptr;
        if (last == 0) {
          table->bins[i] = ptr->next;
        } else {
          last->next = ptr->next;
        }
        ptr = ptr->next;
        free(tmp);
        table->num_entries--;
      }
    }
  }
  return 0;
}

/* ハッシュ値に hash を持つ状態すべてに対して関数 func を適用する */
int st_foreach_hash(st_table *table, st_data_t hash, int(*func)( ANYARGS), st_data_t arg) {
  st_table_entry *ptr, *last, *tmp;
  enum st_retval retval;
  st_data_t hash_val = hash % table->num_bins;

  last = 0;
  for (ptr = table->bins[hash_val]; ptr != 0;) { /* エントリーの存在するチェインでのみfuncが呼ばれる */
    if (ptr->hash != hash) {
      ptr = ptr->next;
      continue;
    }
    retval = (*func)(ptr->key, ptr->record, arg);
    switch (retval) {
    case ST_CHECK: /* check if hash is modified during iteration */
      tmp = 0;
      for (tmp = table->bins[hash_val]; tmp; tmp = tmp->next) {
        if (tmp == ptr)
          break;
      }
      if (!tmp) {
        /* call func with error notice */
        return 1;
      }
      /* fall through */
    case ST_CONTINUE:
      last = ptr;
      ptr = ptr->next;
      break;
    case ST_STOP:
      return 0;
    case ST_DELETE:
      tmp = ptr;
      if (last == 0) {
        table->bins[hash_val] = ptr->next;
      } else {
        last->next = ptr->next;
      }
      ptr = ptr->next;
      free(tmp);
      table->num_entries--;
    }
  }
  return 0;
}

unsigned int st_num(st_table *table) {
  return table->num_entries;
}

static int insert_f(st_data_t key, st_data_t value, st_data_t tbl1)
{
  st_insert((st_table_t)tbl1, key, value);
  return ST_CONTINUE;
}

void st_concat(st_table *tbl1, const st_table *tbl2)
{
  st_foreach((st_table *)tbl2, insert_f, (st_data_t)tbl1);
}

/*　st_tableが持つ要素を表示する　*/
void st_print(st_table *st){
  printf("st_pt = %p\n", st);
  st_table_entry *entry;
  unsigned int nb = st->num_bins, ne = st->num_entries;
  printf("st->num_bins = %d, st->num_entries = %d\n", nb, ne);
  unsigned int i = 0;
  for(; i<nb; i++){
    entry = st->bins[i];
    if(entry!=NULL){
      while(entry!=NULL){
        /* デフォルトでは要素をすべて数値で出力 */
        printf("%u entry->key = %ld, ->record = %lu, ->hash = %lu\n",
            i, (long)entry->key, (long)entry->record, (unsigned long)entry->hash);
        entry = entry->next;
      }
    }else{
      /* このprintf文を消せばnull entryは表示されない */
      //printf("%d null entry\n", i);
    }
  }
}

/*　st_tableが持つ要素をVectorに昇順に格納する　*/
void st_get_entrys(st_table *st, Vector *vec){
  st_table_entry *entry;
  unsigned int nb = st->num_bins;
  int i;
  //long pre_id, cur_id;
  //int i, j, k, n;
  for (i = 0; i < nb; i++) {
    entry = st->bins[i];

    while (entry != NULL) {
      /* とりあえずの実装 */
      /*n = vec_num(vec);
      for (j = 0; j < n; j++) {
        if ((long)entry->key > (long)vec_get(vec, j)) {
          pre_id = entry->key;
          vec_push(vec, 0);
          for (k = j; k < (n + 1); k++) {
            cur_id = (long)vec_get(vec, k);
            vec_set(vec, j, (LmnWord)pre_id);
            pre_id = cur_id;
          }
          printf("push1\n");
          break;
        }
        if (j == n) {
          vec_push(vec, (LmnWord)entry->key);
          printf("push2\n");
        }
      }*/
      vec_push(vec, (LmnWord)entry->key);
      entry = entry->next;
    }


  }
}

/* 2つのst_tableが同じ要素を持つか判定　同じなら1を返す */
int st_equals(st_table *tbl1, st_table *tbl2){
  st_table *st1 = tbl1, *st2 = tbl2;
  unsigned int n1 = st_num(st1), n2 = st_num(st2);
  st_table_entry *entry;

  if(n1==n2){
    if (n1 == 0) return 1;
    /* st1 --> st2 */
    unsigned int nb = st1->num_bins, ne = st1->num_entries; // neいらないかも
    //printf("nb1 = %d, ne1 = %d\n", nb, ne);
    unsigned int i = 0;
    int re;
    for(; i<nb; i++){
      entry = st1->bins[i];
      if(entry!=NULL){
        while(entry->next!=NULL){
          re = st_is_member(st2, entry->key);

          if(re==0) return 0;
          entry = entry->next;
        }
        re = st_is_member(st2, entry->key);

        if(re==0) return 0;
      }
    }

    /* st2 --> st1 */
    nb = st2->num_bins, ne = st2->num_entries; // neいらないかも
    //printf("nb2 = %d, ne2 = %d\n", nb, ne);
    i = 0;
    for(; i<nb; i++){
      entry = st2->bins[i];
      if(entry!=NULL){
        while(entry->next!=NULL){
          re = st_is_member(st1, entry->key);

          if(re==0) return 0;
          entry = entry->next;
        }
        re = st_is_member(st1, entry->key);

        if(re==0) return 0;
      }
    }
    return 1;
  }
  return 0;
}

/*
 * hash_32 - 32 bit Fowler/Noll/Vo FNV-1a hash code
 *
 ***
 *
 * Fowler/Noll/Vo hash
 *
 * The basis of this hash algorithm was taken from an idea sent
 * as reviewer comments to the IEEE POSIX P1003.2 committee by:
 *
 *      Phong Vo (http://www.research.att.com/info/kpv/)
 *      Glenn Fowler (http://www.research.att.com/~gsf/)
 *
 * In a subsequent ballot round:
 *
 *      Landon Curt Noll (http://www.isthe.com/chongo/)
 *
 * improved on their algorithm.  Some people tried this hash
 * and found that it worked rather well.  In an EMail message
 * to Landon, they named it the ``Fowler/Noll/Vo'' or FNV hash.
 *
 * FNV hashes are designed to be fast while maintaining a low
 * collision rate. The FNV speed allows one to quickly hash lots
 * of data while maintaining a reasonable collision rate.  See:
 *
 *      http://www.isthe.com/chongo/tech/comp/fnv/index.html
 *
 * for more details as well as other forms of the FNV hash.
 ***
 *
 * To use the recommended 32 bit FNV-1a hash, pass FNV1_32A_INIT as the
 * Fnv32_t hashval argument to fnv_32a_buf() or fnv_32a_str().
 *
 ***
 *
 * Please do not copyright this code.  This code is in the public domain.
 *
 * LANDON CURT NOLL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO
 * EVENT SHALL LANDON CURT NOLL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * By:
 *  chongo <Landon Curt Noll> /\oo/\
 *      http://www.isthe.com/chongo/
 *
 * Share and Enjoy! :-)
 */

/*
 * 32 bit FNV-1 and FNV-1a non-zero initial basis
 *
 * The FNV-1 initial basis is the FNV-0 hash of the following 32 octets:
 *
 *              chongo <Landon Curt Noll> /\../\
 *
 * NOTE: The \'s above are not back-slashing escape characters.
 * They are literal ASCII  backslash 0x5c characters.
 *
 * NOTE: The FNV-1a initial basis is the same value as FNV-1 by definition.
 */
#define FNV1_32A_INIT 0x811c9dc5

/*
 * 32 bit magic FNV-1a prime
 */
#define FNV_32_PRIME 0x01000193

long st_strhash(register const char *string) {
  register unsigned long hval = FNV1_32A_INIT;

  /*
   * FNV-1a hash each octet in the buffer
   */
  while (*string) {
    /* xor the bottom with the current octet */
    hval ^= (unsigned long) *string++;

    /* multiply by the 32 bit FNV magic prime mod 2^32 */
    hval *= FNV_32_PRIME;
  }
  return hval;
}

int st_numcmp(long x, long y) {
  return x != y;
}

long st_numhash(long n) {
  return n;
}

/* ST Objective-C additions */
int st_ptrcmp(void *x, void *y) {
  return x != y;
}

long st_ptrhash(void *n) {
  return (long) n;
}

unsigned long st_table_space(st_table *table)
{
  return
    sizeof(struct st_table) +
    table->num_bins * sizeof(st_table_entry*) +
    table->num_entries * sizeof(st_table_entry);
}
