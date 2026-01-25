set(quadrature_fppFiles
    stdlib_quadrature.fypp
    stdlib_quadrature_trapz.fypp
    stdlib_quadrature_simps.fypp
    )

set(quadrature_cppFiles
    )

set(quadrature_f90Files
    stdlib_quadrature_gauss.f90
    )

configure_stdlib_target(${PROJECT_NAME}_quadrature quadrature_f90Files quadrature_fppFiles quadrature_cppFiles)

target_link_libraries(${PROJECT_NAME}_quadrature PUBLIC ${PROJECT_NAME}_core ${PROJECT_NAME}_specialfunctions)
