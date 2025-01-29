#include <iostream>
#include <fstream>

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

using namespace std;

static const int SIZE = 2048;
char * key_array = new char[SIZE];
static const uint32_t NM_SEED = 0xdeadbeef;
static const uint64_t WATER_SEED = 0xdeadbeef1eadbeef;
static const uint32_t PENGY_SEED = 0xdeadbeef;
static const uint64_t SPOOKY_SEED[2] = { WATER_SEED, WATER_SEED };

int read_keys(){
    string inFileName = "key_array.bin";
    std::ifstream fin( inFileName, ios::in | ios::binary );
    if (!fin){
        cout << "Cannot open key_array.bin!" << endl;
        return 1;
    }
    fin.read(key_array, SIZE);
    fin.close();
    return 0;
}

int write_nmhash32(){
    size_t i;
    uint32_t hash;
    string outFileName = "c_nmhash32_array.bin";
    std::ofstream fout( outFileName, ios::out | ios::binary );

    if (!fout){
        cout << "Cannot open c_nmhash32_array.bin!" << endl;
        return 1;
    }
    for( i=0; i<=SIZE; i+=1 ){
        hash = NMHASH32((void *) key_array, i, NM_SEED);
        fout.write((char *) &hash, 4);
    }
    fout.close();
    return 0;
}

int write_nmhash32x(){
    size_t i;
    uint32_t hash;
    string outFileName = "c_nmhash32x_array.bin";
    std::ofstream fout( outFileName, ios::out | ios::binary );

    if (!fout){
        cout << "Cannot open c_nmhash32x_array.bin!" << endl;
        return 1;
    }
    for( i=0; i<=SIZE; i+=1 ){
        hash = NMHASH32X((void *) key_array, i, NM_SEED);
        fout.write((char *) &hash, 4);
    }
    fout.close();
    return 0;
}

int write_water(){
    uint32_t i;
    uint32_t hash;
    string outFileName = "c_water_hash_array.bin";
    std::ofstream fout( outFileName, ios::out | ios::binary );

    if (!fout){
        cout << "Cannot open c_water_hash_array.bin!" << endl;
        return 1;
    }
    for( i=0; i<=SIZE; i+=1 ){
        hash = waterhash((void *) key_array, i, WATER_SEED);
        fout.write((char *) &hash, 4);
    }
    fout.close();
    return 0;
}

int write_pengy(){
    size_t i;
    uint64_t hash;
    string outFileName = "c_pengy_hash_array.bin";
    std::ofstream fout( outFileName, ios::out | ios::binary );

    if (!fout){
        cout << "Cannot open c_pengy_hash_array.bin!" << endl;
        return 1;
    }
    for( i=0; i<=SIZE; i+=1 ){
        hash = pengyhash((void *) key_array, i, PENGY_SEED);
        fout.write((char *) &hash, 8);
    }
    fout.close();
    return 0;
}

int write_spooky(){
    size_t i;
    uint64_t hash[2];
    string outFileName = "c_spooky_hash_array.bin";
    std::ofstream fout( outFileName, ios::out | ios::binary );

    if (!fout){
        cout << "Cannot open c_spooky_hash_array.bin!" << endl;
        return 1;
    }
    for( i=0; i<=SIZE; i+=1 ){
        SpookyHash128_with_state_test((void *) key_array, i, (void *) SPOOKY_SEED, (void *) hash);
        fout.write((char *) hash, 16);
    }
    fout.close();
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
