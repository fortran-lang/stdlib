set(quadrature_fppFiles
    ../stdlib_error.fypp
    stdlib_quadrature.fypp
    stdlib_quadrature_trapz.fypp
    stdlib_quadrature_simps.fypp
    )

set(quadrature_cppFiles
    )

set(quadrature_f90Files
    stdlib_quadrature_gauss.f90
    )

configure_stdlib_target(quadrature quadrature_f90Files quadrature_fppFiles quadrature_cppFiles)

target_link_libraries(quadrature PUBLIC core specialfunctions)
