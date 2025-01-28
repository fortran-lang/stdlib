#include <stdio.h>

extern "C" {
  #include "nmhash.h"
  #include "pengyhash.h"
  #include "waterhash.h"
  int generate_all_c_hash();
}

#include "SpookyV2.h"

void SpookyHash32_with_state_test(const void *key, size_t len, const void *state, void *out) {
  uint64_t *state64= (uint64_t *)state;
  uint64_t s0 = state64[0];
  uint64_t s1 = state64[1];
  SpookyHash::Hash128(key, len, &s0, &s1);
  ((uint32_t *)out)[0]= (uint32_t)s0;
}

void SpookyHash64_with_state_test(const void *key, size_t len, const void *state, void *out) {
  uint64_t *state64= (uint64_t *)state;
  uint64_t *out64= (uint64_t *)out;
  out64[0] = state64[0];
  uint64_t s1 = state64[1];
  SpookyHash::Hash128(key, len, out64, &s1);
}

void SpookyHash128_with_state_test(const void *key, size_t len, const void *state, void *out) {
  uint64_t *state64= (uint64_t *)state;
  uint64_t *out64= (uint64_t *)out;
  out64[0] = state64[0];
  out64[1] = state64[1];
  SpookyHash::Hash128(key, len, out64, out64+1);
}

void SpookyHash_seed_state_test(int in_bits, const void *seed, void *state) {
    uint64_t *state64= (uint64_t *)state;
    if (in_bits == 32) {
        state64[0]= state64[1]= ((uint32_t*)seed)[0];
    }
    else {
        uint64_t *seed64= (uint64_t *)seed;
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

static const int SIZE = 2048;
char * key_array = new char[SIZE];
static const uint32_t NM_SEED = 0xdeadbeef;
static const uint64_t WATER_SEED = 0xdeadbeef1eadbeef;
static const uint32_t PENGY_SEED = 0xdeadbeef;
static const uint64_t SPOOKY_SEED[2] = { WATER_SEED, WATER_SEED };

int read_keys() {
    const char *inFileName = "key_array.bin";
    FILE *fin = fopen(inFileName, "rb");

    if (!fin) {
        fprintf(stderr, "Cannot open key_array.bin!\n");
        return 1;
    }

    size_t bytesRead = fread(key_array, 1, SIZE, fin);
    if (bytesRead != SIZE) {
        fprintf(stderr, "Error reading key_array.bin! Only %zu bytes read.\n", bytesRead);
        fclose(fin);
        return 1;
    }

    fclose(fin);
    return 0;
}

int write_nmhash32() {
    size_t i;
    uint32_t hash;
    const char *outFileName = "c_nmhash32_array.bin";
    FILE *fout = fopen(outFileName, "wb");

    if (!fout) {
        fprintf(stderr, "Cannot open c_nmhash32_array.bin!\n");
        return 1;
    }

    for (i = 0; i <= SIZE; i++) {
        hash = NMHASH32((const void *)key_array, i, NM_SEED);
        fwrite(&hash, sizeof(uint32_t), 1, fout); // Write 4 bytes (1 uint32_t) to the file
    }

    fclose(fout);
    return 0;
}

int write_nmhash32x() {
    size_t i;
    uint32_t hash;
    const char *outFileName = "c_nmhash32x_array.bin";
    FILE *fout = fopen(outFileName, "wb");

    if (!fout) {
        fprintf(stderr, "Cannot open c_nmhash32x_array.bin!\n");
        return 1;
    }

    for (i = 0; i <= SIZE; i++) {
        hash = NMHASH32X((const void *)key_array, i, NM_SEED);
        fwrite(&hash, sizeof(uint32_t), 1, fout); // Write 4 bytes (1 uint32_t) to the file
    }

    fclose(fout);
    return 0;
}

int write_water() {
    uint32_t i;
    uint32_t hash;
    const char *outFileName = "c_water_hash_array.bin";
    FILE *fout = fopen(outFileName, "wb");

    if (!fout) {
        fprintf(stderr, "Cannot open c_water_hash_array.bin!\n");
        return 1;
    }

    for (i = 0; i <= SIZE; i++) {
        hash = waterhash((const void *)key_array, i, WATER_SEED);
        fwrite(&hash, sizeof(uint32_t), 1, fout); // Write 4 bytes (1 uint32_t) to the file
    }

    fclose(fout);
    return 0;
}

int write_pengy(){
    size_t i;
    uint64_t hash;
    const char *outFileName = "c_pengy_hash_array.bin";
    FILE *fout = fopen(outFileName, "wb");

    if (!fout) {
        fprintf(stderr, "Cannot open c_pengy_hash_array.bin!\n");
        return 1;
    }

    for (i = 0; i <= SIZE; i++) {
        hash = pengyhash((const void *)key_array, i, PENGY_SEED);
        fwrite(&hash, sizeof(uint64_t), 1, fout); // Write 8 bytes (1 uint64_t) to the file
    }

    fclose(fout);
    return 0;
}

int write_spooky() {
    size_t i;
    uint64_t hash[2];
    const char *outFileName = "c_spooky_hash_array.bin";
    FILE *fout = fopen(outFileName, "wb");

    if (!fout) {
        fprintf(stderr, "Cannot open c_spooky_hash_array.bin!\n");
        return 1;
    }

    for (i = 0; i <= SIZE; i++) {
        SpookyHash128_with_state_test((const void *)key_array, i, (const void *)SPOOKY_SEED, (void *)hash);
        fwrite(hash, sizeof(uint64_t), 2, fout); // Write 16 bytes (2 * 8 bytes) to the file
    }

    fclose(fout);
    return 0;
}

int generate_all_c_hash(){
    if (read_keys()==1){return 1;};
    if (write_nmhash32()==1){return 1;};
    if (write_nmhash32x()==1){return 1;};
    if (write_water()==1){return 1;};
    if (write_pengy()==1){return 1;};
    if (write_spooky()==1){return 1;};
    return 0;
}


/*
int main(){
    if (read_keys()==1){return 1;};
    if (write_nmhash32()==1){return 1;};
    if (write_nmhash32x()==1){return 1;};
    if (write_water()==1){return 1;};
    if (write_pengy()==1){return 1;};
    if (write_spooky()==1){return 1;};
    return 0;
}
*/
