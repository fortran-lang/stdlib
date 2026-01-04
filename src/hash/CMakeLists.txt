set(hash_fppFiles
    ../stdlib_kinds.fypp
    stdlib_hash_32bit_fnv.fypp
    stdlib_hash_32bit.fypp
    stdlib_hash_32bit_nm.fypp
    stdlib_hash_32bit_water.fypp
    stdlib_hash_64bit_fnv.fypp
    stdlib_hash_64bit.fypp
    stdlib_hash_64bit_pengy.fypp
    stdlib_hash_64bit_spookyv2.fypp
    )

configure_stdlib_target(hash "" hash_fppFiles "")
