#include <string.h>
#include "mc.h"
#include "mhash.h"
#include "propositional_symbol.h"
#include "ltl2ba_adapter.h"
#include "error.h"

enum MC_ERRORNO {
  MC_ERR_NC_ENV,
  MC_ERR_PROP_ENV,
  MC_NC_OPEN_ERROR,
  MC_NC_LOAD_ERROR,
  MC_PROP_OPEN_ERROR,
  MC_PROP_LOAD_ERROR,
};
  
/* 状態IDが本来不必要な場合に使用する状態ID */
#define DEFAULT_STATE_ID 0

/**
 * コンストラクタ
 */
State *state_make(LmnMembrane *mem, BYTE state_name, lmn_interned_str rule) {
  State *new = LMN_MALLOC(State);
  new->mem = mem;
  new->state_name = state_name;
  new->flags = 0x00U;
  /* successorの記憶域をゼロクリアする */
  memset(&new->successor, 0x00U, sizeof(Vector));
  /* ハッシュ値はあらかじめ計算しておく */
  new->hash = mhash(new->mem);
  new->rule_name = rule;
  return new;
}

/**
 * コンストラクタ
 */
State *state_make_for_nd(LmnMembrane *mem, lmn_interned_str rule) {
  return state_make(mem, DEFAULT_STATE_ID, rule);
}

/**
 * 記憶域sの最初のnバイトがゼロであることを確認する
 */
static inline BOOL mem_is_zero(const void *s, size_t n) {
  const unsigned char *p = (const unsigned char *)s;
  while (n-- > 0 ) {
    if(*p != 0x00U) return FALSE;
    p++;
  }
  return TRUE;
}

inline void state_succ_init(State *s, int init_size) {
  vec_init(&s->successor, init_size);
}

/**
 * デストラクタ
 */
void state_free(State *s) {
  lmn_mem_drop(s->mem);
  lmn_mem_free(s->mem);
  if (!mem_is_zero(&s->successor, sizeof(Vector))) {
    vec_destroy(&s->successor);
  }
  LMN_FREE(s);
}

inline int state_hash(State *s) {
  return s->hash;
}

BYTE state_property_state(State *state)
{
  return state->state_name;
}

/**
 * 引数としてあたえられたStateが等しいかどうかを判定する
 * ハッシュ値が等しい場合は同型判定を行う
 */
static int state_equals(HashKeyType k1, HashKeyType k2) {
  State *s1 = (State *)k1;
  State *s2 = (State *)k2;

  int t;
  t = 
    s1->state_name == s2->state_name &&
    state_hash(s1) == state_hash(s2) &&
    lmn_mem_equals(s1->mem, s2->mem);
  return t;
}

/**
 * 与えられた2つの状態が互いに異なっていれば真を、逆に等しい場合は偽を返す
 */
int state_cmp(HashKeyType s1, HashKeyType s2) {
  return !state_equals(s1, s2);
}

/**
 * 膜スタックの代替品
 * 自身を含めた全ての先祖膜を起こす
 */
inline void activate_ancestors(LmnMembrane *mem) {
  LmnMembrane *cur;
  for (cur=mem; cur; cur=cur->parent) {
    cur->is_activated = TRUE;
  }
}

/* 成功ならば0, そうでないならば0以外を返す */
int mc_load_property(Automata *a, PVector *prop_defs)
{
  FILE *nc_fp, *prop_fp;
  int r;

  *a = NULL;
  *prop_defs = NULL;
  nc_fp = prop_fp = NULL;

  if (lmn_env.ltl_exp) {
    nc_fp = ltl2ba_str(lmn_env.ltl_exp);
  } else {
    if (!lmn_env.automata_file) goto NC_ENV;
    if (!(nc_fp = fopen(lmn_env.automata_file, "r"))) goto NC_OPEN_ERROR;
  }
  if (never_claim_load(nc_fp, a)) goto NC_LOAD_ERROR;
  
  if (!lmn_env.propositional_symbol) goto PROP_ENV;
  if (!(prop_fp = fopen(lmn_env.propositional_symbol, "r"))) goto PROP_OPEN_ERROR;
  if (propsym_load_file(prop_fp, *a, prop_defs)) goto PROP_LOAD_ERROR;

  r = 0;
  goto RET;
  
NC_ENV: r = MC_ERR_NC_ENV; goto FINALLY;
PROP_ENV: r = MC_ERR_PROP_ENV; goto FINALLY;
NC_OPEN_ERROR: r = MC_NC_OPEN_ERROR; goto FINALLY;
NC_LOAD_ERROR:
  {
    char c;
    rewind(nc_fp);
    while ((c = fgetc(nc_fp)) != EOF) {
      fputc(c, stderr);
    }
    r = MC_NC_LOAD_ERROR;
    goto FINALLY;
  }
PROP_OPEN_ERROR: r = MC_PROP_OPEN_ERROR; goto FINALLY;
PROP_LOAD_ERROR: r = MC_PROP_LOAD_ERROR; goto FINALLY;

FINALLY:
  LMN_FREE(*a);
  LMN_FREE(*prop_defs);

 RET:
  if (prop_fp) fclose(prop_fp);
  if (nc_fp) fclose(nc_fp);
  return r;
}

void mc_explain_error(int error_id)
{
  lmn_report(mc_error_msg(error_id));
}

char *mc_error_msg(int error_id)
{
  switch (error_id) {
  case MC_ERR_NC_ENV:
    return "specify never claim file";
  case MC_ERR_PROP_ENV:
    return "specify propositional symbol definition file";
  case MC_NC_OPEN_ERROR:
    return "cannot open never claim file";
  case  MC_NC_LOAD_ERROR:
    return "error while parsing never claim file";
  case MC_PROP_OPEN_ERROR:
    return "cannot open propositional symbol definition file";
  case MC_PROP_LOAD_ERROR:
    return "error while parsing propositional symbol definition file";
  default:
    lmn_fatal("implementation error\n");
    return NULL;
  }
}
