#ifndef _OMEGA_ARRAY_H
#define _OMEGA_ARRAY_H

#include"collection.hpp"
#include"limits.h"

#define OMEGA (INT_MAX)

#define EXTERNAL2INTERNAL(EXTERNAL_INDEX) ((EXTERNAL_INDEX) == OMEGA ? 0 : (EXTERNAL_INDEX) + 1)
//#define INTERNAL2EXTERNAL(INTERNAL_INDEX) ((INTERNAL_INDEX) == 0 ? OMEGA : (INTERNAL_INDEX) - 1)

typedef struct _IntContainer{
  int value;
}IntContainer;

struct OmegaArray{
  int maxFiniteIndex;
  unbound_vector<IntContainer *> *body;
  OmegaArray () {
    maxFiniteIndex = -1;
    body = new unbound_vector<IntContainer *>();
  }
};

OmegaArray *makeOmegaArray();
void freeOmegaArray(OmegaArray *oArray);

void incrementOmegaArray(OmegaArray *oArray,int externalIndex);
void decrementOmegaArray(OmegaArray *oArray,int externalIndex);
int readOmegaArray(OmegaArray *oArray,int externalIndex);
int maxIndex(OmegaArray *oArray);
void omegaArrayDump(OmegaArray *oArray);
Bool isEqualOmegaArray(OmegaArray *a,OmegaArray * b);



#endif
