set(SRC
    stdlib_experimental_ascii.f90
    stdlib_experimental_io.f90
    stdlib_experimental_error.f90
    stdlib_experimental_kinds.f90
    stdlib_experimental_optval.f90
    stdlib_experimental_system.F90
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

if(f03rank)
  target_sources(fortran_stdlib PRIVATE f03_stdlib_experimental_stats.f90)
  target_sources(fortran_stdlib PRIVATE f03_stdlib_experimental_stats_mean.f90)
else()
  target_sources(fortran_stdlib PRIVATE f90_stdlib_experimental_stats.f90)
  target_sources(fortran_stdlib PRIVATE f90_stdlib_experimental_stats_mean.f90)
endif()

add_subdirectory(tests)

install(TARGETS fortran_stdlib
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
    )
install(DIRECTORY ${LIB_MOD_DIR} DESTINATION include)
