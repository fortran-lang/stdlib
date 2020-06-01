### Pre-process: .fpp -> .f90 via Fypp

# Create a list of the files to be preprocessed
set(fppFiles
    stdlib_experimental_io.fypp
    stdlib_experimental_linalg.fypp
    stdlib_experimental_linalg_diag.fypp
    stdlib_experimental_optval.fypp
    stdlib_experimental_stats.fypp
    stdlib_experimental_stats_cov.fypp
    stdlib_experimental_stats_mean.fypp
    stdlib_experimental_stats_moment.fypp
    stdlib_experimental_stats_var.fypp
    stdlib_experimental_quadrature.fypp
    stdlib_experimental_quadrature_trapz.fypp
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
    stdlib_experimental_ascii.f90
    stdlib_experimental_error.f90
    stdlib_experimental_kinds.f90
    stdlib_experimental_system.F90
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

# relax standard checks
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10.0)
    set_source_files_properties("stdlib_experimental_ascii.f90" PROPERTIES COMPILE_FLAGS "-fallow-invalid-boz")
  endif()
endif()

install(TARGETS fortran_stdlib
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
    )
install(DIRECTORY ${LIB_MOD_DIR} DESTINATION include)
