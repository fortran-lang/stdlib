### Pre-process: .fpp -> .f90 via Fypp

# Create a list of the files to be preprocessed
set(fppFiles
    stdlib_ascii.fypp
    stdlib_bitsets.fypp
    stdlib_bitsets_64.fypp
    stdlib_bitsets_large.fypp
    stdlib_io.fypp
    stdlib_linalg.fypp
    stdlib_linalg_diag.fypp
    stdlib_linalg_outer_product.fypp
    stdlib_optval.fypp
    stdlib_sorting.fypp
    stdlib_sorting_ord_sort.fypp
    stdlib_sorting_sort.fypp
    stdlib_sorting_sort_index.fypp 
    stdlib_stats.fypp
    stdlib_stats_corr.fypp
    stdlib_stats_cov.fypp
    stdlib_stats_mean.fypp
    stdlib_stats_median.fypp
    stdlib_stats_moment.fypp
    stdlib_stats_moment_all.fypp
    stdlib_stats_moment_mask.fypp
    stdlib_stats_moment_scalar.fypp
    stdlib_stats_var.fypp
    stdlib_quadrature.fypp
    stdlib_quadrature_trapz.fypp
    stdlib_quadrature_simps.fypp
    stdlib_stats_distribution_PRNG.fypp
    stdlib_math.fypp
    stdlib_math_linspace.fypp
    stdlib_math_logspace.fypp
    stdlib_math_arange.fypp
    stdlib_string_type.fypp
    stdlib_string_type_constructor.fypp
    stdlib_strings_to_string.fypp
    stdlib_strings.fypp
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
    stdlib_error.f90
    stdlib_kinds.f90
    stdlib_logger.f90
    stdlib_system.F90
    stdlib_specialfunctions.f90
    stdlib_specialfunctions_legendre.f90
    stdlib_quadrature_gauss.f90
    ${outFiles}
)

add_library(${PROJECT_NAME} ${SRC})

set(LIB_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod_files/)
set_target_properties(${PROJECT_NAME} PROPERTIES
    Fortran_MODULE_DIRECTORY ${LIB_MOD_DIR})
target_include_directories(${PROJECT_NAME} PUBLIC
    $<BUILD_INTERFACE:${LIB_MOD_DIR}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}>
)

if(f18errorstop)
  target_sources(${PROJECT_NAME} PRIVATE f18estop.f90)
else()
  target_sources(${PROJECT_NAME} PRIVATE f08estop.f90)
endif()

add_subdirectory(tests)

install(TARGETS ${PROJECT_NAME}
        EXPORT ${PROJECT_NAME}-targets
        RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
        ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
        LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
)
install(DIRECTORY ${LIB_MOD_DIR} DESTINATION "${CMAKE_INSTALL_MODULEDIR}")
