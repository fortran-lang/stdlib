set(core_fppFiles
    stdlib_ascii.fypp
    stdlib_version.fypp
    stdlib_kinds.fypp
    stdlib_optval.fypp
    )

set(core_cppFiles
    )

set(core_f90Files
    )

configure_stdlib_target(core core_f90Files core_fppFiles core_cppFiles)
