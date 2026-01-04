set(hashmaps_fppFiles
    ../stdlib_kinds.fypp
    ../stdlib_hash_32bit_fnv.fypp
    ../stdlib_hash_32bit.fypp
    ../stdlib_hash_32bit_nm.fypp
    ../stdlib_hash_32bit_water.fypp
    )

set(hashmaps_f90Files
    stdlib_hashmap_chaining.f90
    stdlib_hashmap_open.f90
    stdlib_hashmaps.f90
    stdlib_hashmap_wrappers.f90
    )

configure_stdlib_target(hashmaps hashmaps_f90Files hashmaps_fppFiles "")
