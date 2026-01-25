set(core_fppFiles
    stdlib_ascii.fypp
    stdlib_error.fypp
    stdlib_kinds.fypp
    stdlib_optval.fypp
    )

set(core_cppFiles
    )

set(core_f90Files
    $<IF:$<BOOL:${f18errorstop}>,f18estop.f90,f08estop.f90>
    stdlib_io_aux.f90
    )

configure_stdlib_target(${PROJECT_NAME}_core core_f90Files core_fppFiles core_cppFiles)
