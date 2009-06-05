
#ifndef LMN_STRING_H
#define LMN_STRING_H

void string_init(void);
void string_finalize(void);

LmnWord lmn_string_make(const char *s);
BOOL lmn_is_string(LmnWord atom, LmnLinkAttr attr);
const char *lmn_string_c_str(LmnWord atom);

#endif
