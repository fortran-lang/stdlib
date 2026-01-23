set(hashmaps_f90Files
    stdlib_hashmap_chaining.f90
    stdlib_hashmap_open.f90
    stdlib_hashmaps.f90
    stdlib_hashmap_wrappers.f90
    )

configure_stdlib_target(hashmaps hashmaps_f90Files "" "")

target_link_libraries(hashmaps PUBLIC hash)
