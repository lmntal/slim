#include "collection.hpp"
DynamicArray *makeDynamicArray(){
  DynamicArray *ret = (DynamicArray *)malloc(sizeof(DynamicArray));
  ret->cap = INIT_CAP;
  if((ret->body = (void **)calloc(sizeof(void *),INIT_CAP)) == NULL){
    CHECKER("CALLOC ERROR");
    exit(EXIT_FAILURE);
  }
  return ret;
}
