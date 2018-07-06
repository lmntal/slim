#include "collection.hpp"
unsigned int round2up(unsigned int n){
  unsigned int ret = 1;
  while(ret && ret < n){
    ret <<= 1;
  }
  if(ret == 0){
    CHECKER("LARGE SIZE ERROR");
    exit(EXIT_FAILURE);
  }
  return ret;
}

DynamicArray *assureSizeOfDynamicArray(DynamicArray *DArray,int index){
  if(index >= DArray->cap){
    int newCap = round2up((unsigned int)(index+1));
    void **newBody = (void **)realloc(DArray->body,newCap*sizeof(void *));
    if(newBody == NULL){
      CHECKER("REALLOC ERROR");
      exit(EXIT_FAILURE);
    }
    memset(newBody+DArray->cap,0,(newCap-DArray->cap)*sizeof(void *));
    DArray->cap = newCap;
    DArray->body = newBody;
    return DArray;
  }else{
    return DArray;
  }
}

void *writeDynamicArray(DynamicArray *DArray,int index,void *value){
  assureSizeOfDynamicArray(DArray,index);
  void *ret = DArray->body[index];
  DArray->body[index] = value;
  return ret;
}
void *readDynamicArray(DynamicArray *DArray,int index){
  if(index < 0){
    return NULL;
  }else if(index < DArray->cap){
    return DArray->body[index];
  }else{
    return NULL;
  }
}

DynamicArray *makeDynamicArray(){
  DynamicArray *ret = (DynamicArray *)malloc(sizeof(DynamicArray));
  ret->cap = INIT_CAP;
  if((ret->body = (void **)calloc(sizeof(void *),INIT_CAP)) == NULL){
    CHECKER("CALLOC ERROR");
    exit(EXIT_FAILURE);
  }
  return ret;
}

void *pushStack(Stack *stack,void *value){
  void *ret = writeDynamicArray(stack->body,stack->num,value);
  stack->num++;
  return ret;
}

void dynamicArrayDump(DynamicArray *DArray,void valueDump(void *)){
  int i;
  for(i=0;i<DArray->cap;i++){
    if(DArray->body[i] == NULL){
      //fprintf(stdout,"%d:NULL\n",i);
    }else{
      fprintf(stdout,"%d:",i);
      valueDump(DArray->body[i]);
      printf("\n");
    }
  }
}

void *readStack(Stack *stack,int index){
  return readDynamicArray(stack->body,index);
}
