set(hashmaps_f90Files
    stdlib_hashmap_chaining.f90
    stdlib_hashmap_open.f90
    stdlib_hashmaps.f90
    stdlib_hashmap_wrappers.f90
    )

configure_stdlib_target(${PROJECT_NAME}_hashmaps hashmaps_f90Files "" "")

target_link_libraries(${PROJECT_NAME}_hashmaps PUBLIC ${PROJECT_NAME}_hash)
