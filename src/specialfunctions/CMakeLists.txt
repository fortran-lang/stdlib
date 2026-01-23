set(specialfunctions_f90Files
    stdlib_specialfunctions_legendre.f90
    )

set(specialfunctions_fppFiles
    stdlib_specialfunctions_activations.fypp
    stdlib_specialfunctions.fypp
    stdlib_specialfunctions_gamma.fypp
   )

configure_stdlib_target(specialfunctions specialfunctions_f90Files specialfunctions_fppFiles "")

target_link_libraries(specialfunctions PUBLIC intrinsics)
