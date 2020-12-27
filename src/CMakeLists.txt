### Pre-process: .fpp -> .f90 via Fypp

# Create a list of the files to be preprocessed
set(fppFiles
    stdlib_bitsets.fypp
    stdlib_bitsets_64.fypp
    stdlib_bitsets_large.fypp
    stdlib_io.fypp
    stdlib_linalg.fypp
    stdlib_linalg_diag.fypp
    stdlib_optval.fypp
    stdlib_stats.fypp
    stdlib_stats_corr.fypp
    stdlib_stats_cov.fypp
    stdlib_stats_mean.fypp
    stdlib_stats_moment.fypp
    stdlib_stats_var.fypp
    stdlib_quadrature.fypp
    stdlib_quadrature_trapz.fypp
    stdlib_quadrature_simps.fypp
    stdlib_stats_distribution_PRNG.fypp
    stdlib_stats_distribution_uniform.fypp
    stdlib_stats_distribution_exponential.fypp
)


# Custom preprocessor flags
if(DEFINED CMAKE_MAXIMUM_RANK)
  set(fyppFlags "-DMAXRANK=${CMAKE_MAXIMUM_RANK}")
elseif(f03rank)
  set(fyppFlags)
else()
  set(fyppFlags "-DVERSION90")
endif()

fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

set(SRC
    stdlib_ascii.f90
    stdlib_error.f90
    stdlib_kinds.f90
    stdlib_logger.f90
    stdlib_system.F90
    ${outFiles}
)

add_library(fortran_stdlib ${SRC})

set(LIB_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod_files/)
set_target_properties(fortran_stdlib PROPERTIES
    Fortran_MODULE_DIRECTORY ${LIB_MOD_DIR})
target_include_directories(fortran_stdlib PUBLIC
    $<BUILD_INTERFACE:${LIB_MOD_DIR}>
    $<INSTALL_INTERFACE:include>
)

if(f18errorstop)
  target_sources(fortran_stdlib PRIVATE f18estop.f90)
else()
  target_sources(fortran_stdlib PRIVATE f08estop.f90)
endif()

add_subdirectory(tests)

install(TARGETS fortran_stdlib
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
    )
install(DIRECTORY ${LIB_MOD_DIR} DESTINATION include)
