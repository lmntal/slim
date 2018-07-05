#ifndef _UTIL_H
#define _UTIL_H

#include<stdio.h>
#include<assert.h>
#include<stdlib.h>

#define FALSE (0)
#define TRUE (!FALSE)

typedef int Bool;

//#define BUFF_LENGTH 65536
#define BUFF_LENGTH 10000000
#define CHECKER(MESSAGE) (fprintf(stderr,MESSAGE),fprintf(stderr,"FILE;%s\nfunc;%s\nLINE:%d\n",__FILE__,__func__,__LINE__))

#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

void richGets(char *buff);

#endif


