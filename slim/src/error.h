

/* Error */
#define lmn_fatal(Msg) \
  do {                 \
    do_lmn_fatal(Msg); \
    exit(EXIT_FAILURE); \
  } while(0);

void do_lmn_fatal(const char *msg);
LMN_EXTERN void lmn_report(const char *msg, ...);

