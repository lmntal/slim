

/* Error */
#define lmn_fatal(Msg) \
  do {                 \
    do_lmn_fatal(__FILE__, __LINE__, Msg);       \
    exit(EXIT_FAILURE); \
  } while(0);

void do_lmn_fatal(const char *file, int line, const char *msg);
LMN_EXTERN void lmn_report(const char *msg, ...);

