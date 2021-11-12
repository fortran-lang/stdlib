#include "waterhash.h"

int32_t waterhash_test ( const void * key, uint32_t len, uint64_t seed ) {
  return waterhash (key, len, seed);
}

