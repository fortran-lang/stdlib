set(stringlist_fppFiles
    )

set(stringlist_cppFiles
    )

set(stringlist_f90Files
    stdlib_stringlist_type.f90
    )

configure_stdlib_target(${PROJECT_NAME}_stringlist stringlist_f90Files stringlist_fppFiles stringlist_cppFiles)

target_link_libraries(${PROJECT_NAME}_stringlist PUBLIC ${PROJECT_NAME}_math ${PROJECT_NAME}_strings)
