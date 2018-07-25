#include "omegaArray.hpp"

OmegaArray *makeOmegaArray(){
  OmegaArray *ret = (OmegaArray *)malloc(sizeof(OmegaArray));
  ret->maxFiniteIndex = -1;
  ret->body = makeDynamicArray();

  return ret;
}

void freeOmegaArray(OmegaArray *oArray){
  freeDynamicArrayAndValues(oArray->body,free);
  free(oArray);

  return;
}

IntContainer *makeInt(){
  IntContainer *ret = (IntContainer *)malloc(sizeof(IntContainer));
  ret->value = 0;

  return ret;
}

int readOmegaArray(OmegaArray *oArray,int externalIndex){
  int internalIndex = EXTERNAL2INTERNAL(externalIndex);

  IntContainer *tmp = (IntContainer *)readDynamicArray(oArray->body,internalIndex);

  if(tmp == NULL){
    return 0;
  }else{
    return tmp->value;
  }
}

/*
   void writeOmegaArray(OmegaArray *oArray,int externalIndex,int value){
   int internalIndex = EXTERNAL2INTERNAL(externalIndex);

   IntContainer *tmp = readDynamicArray(oArray->body,internalIndex);

   if(tmp == NULL){
   tmp = makeInt();
   }

   tmp->value = value;

   return;
   }
//*/

void incrementOmegaArray(OmegaArray *oArray,int externalIndex){
  if(externalIndex >= 0){
    int internalIndex = EXTERNAL2INTERNAL(externalIndex);

    if(externalIndex != OMEGA){
      oArray->maxFiniteIndex = MAX(externalIndex,oArray->maxFiniteIndex);
    }

    IntContainer *tmp = (IntContainer *)readDynamicArray(oArray->body,internalIndex);

    if(tmp == NULL){
      tmp = makeInt();
      writeDynamicArray(oArray->body,internalIndex,tmp);
      tmp->value++;
    }else{
      tmp->value++;
    }

    return;
  }else{
    CHECKER("INCREMENT ERROR\n");
    exit(EXIT_FAILURE);
  }
}

void decrementOmegaArray(OmegaArray *oArray,int externalIndex){
  if(externalIndex >= 0){
    int internalIndex = EXTERNAL2INTERNAL(externalIndex);

    IntContainer *tmp = (IntContainer *)readDynamicArray(oArray->body,internalIndex);

    if(tmp == NULL){
      tmp = makeInt();
      writeDynamicArray(oArray->body,internalIndex,tmp);
      tmp->value--;
    }else{
      tmp->value--;
    }

    if(tmp->value < 0){
      CHECKER("DECREMENT ERROR\n");
      if(externalIndex != OMEGA){
        fprintf(stderr,"error is in index of %d\n",externalIndex);
      }else{
        fprintf(stderr,"error is in index of OMEGA\n");
      }
      exit(EXIT_FAILURE);
    }

    if(externalIndex == oArray->maxFiniteIndex){
      while(readOmegaArray(oArray,oArray->maxFiniteIndex) == 0 && oArray->maxFiniteIndex >= 0){
        oArray->maxFiniteIndex--;
      }
    }

    return;
  }else{
    CHECKER("DECREMENT ERROR\n");
    exit(EXIT_FAILURE);
  }
}

int maxIndex(OmegaArray *oArray){
  if(readOmegaArray(oArray,OMEGA) == 0){
    return oArray->maxFiniteIndex;
  }else{
    return OMEGA;
  }
}

void omegaArrayDump(OmegaArray *oArray){
  fprintf(stdout,"[");

  int i;
  for(i=0;i<=oArray->maxFiniteIndex;i++){
    fprintf(stdout,"%2d,",readOmegaArray(oArray,i));
  }

  fprintf(stdout," 0, 0, 0,...,%2d]",readOmegaArray(oArray,OMEGA));

  return;
}

Bool isEqualOmegaArray(OmegaArray *a,OmegaArray *b){
  if(a->maxFiniteIndex != b->maxFiniteIndex){
    return FALSE;
  }

  int i;
  for(i=0;i<=a->maxFiniteIndex;i++){
    if(readOmegaArray(a,i) != readOmegaArray(b,i)){
      return FALSE;
    }
  }

  if(readOmegaArray(a,OMEGA) != readOmegaArray(b,OMEGA)){
    return FALSE;
  }

  return TRUE;
}



