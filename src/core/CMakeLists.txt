set(core_fppFiles
    stdlib_version.fypp
    )

set(core_cppFiles
    )

set(core_f90Files
    )

configure_stdlib_target(core core_f90Files core_fppFiles core_cppFiles)
