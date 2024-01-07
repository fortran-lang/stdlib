#include "nmhash.h"
int32_t nmhash32_test ( const void * key, size_t len, uint32_t seed ) {
  return NMHASH32 (key, (const size_t) len, seed);
}

int32_t nmhash32x_test ( const void * key, size_t len, uint32_t seed ) {
  return NMHASH32X (key, (const size_t) len, seed);
}
