set(lapack_extended_fppFiles
    stdlib_lapack_extended_base.fypp
    stdlib_lapack_extended.fypp
)
set(lapack_extended_cppFiles
)

configure_stdlib_target(${PROJECT_NAME}_lapack_extended "" lapack_extended_fppFiles lapack_extended_cppFiles)

target_link_libraries(${PROJECT_NAME}_lapack_extended PUBLIC ${PROJECT_NAME}_core ${PROJECT_NAME}_linalg_core)
