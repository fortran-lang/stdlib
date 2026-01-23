set(stringlist_fppFiles
    )

set(stringlist_cppFiles
    )

set(stringlist_f90Files
    stdlib_stringlist_type.f90
    )

configure_stdlib_target(stringlist stringlist_f90Files stringlist_fppFiles stringlist_cppFiles)

target_link_libraries(stringlist PUBLIC math strings)
