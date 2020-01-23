### Pre-process: .fpp -> .f90 via Fypp

# Create a list of the files to be preprocessed
set(fppFiles
    stdlib_experimental_stats.fypp
    stdlib_experimental_stats_mean.fypp
)

# Pre-process
foreach(infileName IN LISTS fppFiles)

    # Generate output file name
    string(REGEX REPLACE ".fypp\$" ".f90" outfileName "${infileName}")

    # Create the full path for the new file
    set(outfile "${CMAKE_CURRENT_BINARY_DIR}/${outfileName}")

    # Generate input file name
    set(infile "${CMAKE_CURRENT_SOURCE_DIR}/${infileName}")

    # Custom command to do the processing
if(f03rank)
    add_custom_command(
        OUTPUT "${outfile}"
        COMMAND fypp "${infile}" "${outfile}"
        MAIN_DEPENDENCY "${infile}"
        VERBATIM)
else()
    add_custom_command(
        OUTPUT "${outfile}"
        COMMAND fypp -DVERSION90 "${infile}" "${outfile}"
        MAIN_DEPENDENCY "${infile}"
        VERBATIM)
endif()

    # Finally add output file to a list
    set(outFiles ${outFiles} "${outfile}")

endforeach(infileName)



set(SRC
    stdlib_experimental_ascii.f90
    stdlib_experimental_io.f90
    stdlib_experimental_error.f90
    stdlib_experimental_kinds.f90
    stdlib_experimental_optval.f90
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

install(TARGETS fortran_stdlib
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
    )
install(DIRECTORY ${LIB_MOD_DIR} DESTINATION include)
