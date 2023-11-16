#include "SpookyV2.h"

#ifdef __cplusplus
extern "C" {
#endif

void SpookyHash32_with_state_test(const void *key, size_t len, const void *state, void *out) {
  uint64 *state64= (uint64 *)state;
  uint64 s0 = state64[0];
  uint64 s1 = state64[1];
  SpookyHash::Hash128(key, len, &s0, &s1);
  ((uint32 *)out)[0]= (uint32)s0;
}

void SpookyHash64_with_state_test(const void *key, size_t len, const void *state, void *out) {
  uint64 *state64= (uint64 *)state;
  uint64 *out64= (uint64 *)out;
  out64[0] = state64[0];
  uint64 s1 = state64[1];
  SpookyHash::Hash128(key, len, out64, &s1);
}

void SpookyHash128_with_state_test(const void *key, size_t len, const void *state, void *out) {
  uint64 *state64= (uint64 *)state;
  uint64 *out64= (uint64 *)out;
  out64[0] = state64[0];
  out64[1] = state64[1];
  SpookyHash::Hash128(key, len, out64, out64+1);
}

void SpookyHash_seed_state_test(int in_bits, const void *seed, void *state) {
    uint64 *state64= (uint64 *)state;
    if (in_bits == 32) {
        state64[0]= state64[1]= ((uint32*)seed)[0];
    }
    else {
        uint64 *seed64= (uint64 *)seed;
        if (in_bits == 64) {
            state64[0]= state64[1]= seed64[0];
        }
        else
        if (in_bits == 128) {
            state64[0]= seed64[0];
            state64[1]= seed64[1];
        }
    }
}


#ifdef __cplusplus
}
#endif
