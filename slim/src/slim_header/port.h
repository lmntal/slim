
#ifndef LMN_PORT_H
#define LMN_PORT_H

struct LmnPort {
  BOOL direction;
  LmnByte type;   /* LMN_PORT_{FILE|ISTR|OST|PROC} */
  BOOL closed;    /* TRUE if this port is closed */
  BOOL error;     /* error has occurred */
  BOOL owner;     /* TRUE if this port owns underlying
                     file pointer */
  unsigned long line;
  
  lmn_interned_str name;

  void *data;    /* used internally */
};

typedef struct LmnPort *LmnPort;

typedef enum LmnPortDirection {
  LMN_PORT_INPUT,
  LMN_PORT_OUTPUT
} LmnPortDirection;
  

typedef enum LmnPortType {
  LMN_PORT_FILE,
  LMN_PORT_ISTR,
  LMN_PORT_OSTR,
/*   LMN_PORT_PROC /\* virtual port *\/ */
} LmnPortType;

void port_init(void);
void port_finalize(void);

LmnPort lmn_stdin_port(void);
LmnPort lmn_stdout_port(void);
LmnPort lmn_stderr_port(void);

BOOL lmn_port_closed(LmnWord port_atom);
BOOL lmn_port_error_occurred(LmnWord port_atom);
lmn_interned_str lmn_port_name(LmnWord port_atom);


int port_get_raw_c(LmnWord port_atom);
int port_unget_raw_c(LmnWord port_atom, int c);
int port_putc(LmnWord port_atom, LmnAtomPtr unary_atom);
void port_puts(LmnWord port_atom, LmnWord str);
void port_put_raw_s(LmnWord port_atom, const char *str);

#endif
