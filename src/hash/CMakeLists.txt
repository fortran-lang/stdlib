set(hash_fppFiles
    stdlib_hash_32bit_fnv.fypp
    stdlib_hash_32bit.fypp
    stdlib_hash_32bit_nm.fypp
    stdlib_hash_32bit_water.fypp
    stdlib_hash_64bit_fnv.fypp
    stdlib_hash_64bit.fypp
    stdlib_hash_64bit_pengy.fypp
    stdlib_hash_64bit_spookyv2.fypp
    )

configure_stdlib_target(${PROJECT_NAME}_hash "" hash_fppFiles "")

target_link_libraries(${PROJECT_NAME}_hash PUBLIC ${PROJECT_NAME}_core)
