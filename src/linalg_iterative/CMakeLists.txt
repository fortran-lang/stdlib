set(linalg_iterative_fppFiles
    stdlib_linalg_iterative_solvers_bicgstab.fypp
    stdlib_linalg_iterative_solvers_cg.fypp
    stdlib_linalg_iterative_solvers.fypp
    stdlib_linalg_iterative_solvers_pcg.fypp
    )

set(linalg_iterative_cppFiles
    )

set(linalg_iterative_f90Files
    )

configure_stdlib_target(${PROJECT_NAME}_linalg_iterative linalg_iterative_f90Files linalg_iterative_fppFiles linalg_iterative_cppFiles)

target_link_libraries(${PROJECT_NAME}_linalg_iterative PUBLIC ${PROJECT_NAME}_linalg ${PROJECT_NAME}_sparse)
