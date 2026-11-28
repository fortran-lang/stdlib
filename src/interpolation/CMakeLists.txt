set(interpolation_fppFiles
    stdlib_interpolation.fypp
    stdlib_interpolation_search.fypp
    stdlib_interpolation_linear.fypp
    )

set(interpolation_cppFiles
    )

set(interpolation_f90Files
    )

configure_stdlib_target(${PROJECT_NAME}_interpolation interpolation_f90Files interpolation_fppFiles interpolation_cppFiles)

target_link_libraries(${PROJECT_NAME}_interpolation PUBLIC ${PROJECT_NAME}_core)
