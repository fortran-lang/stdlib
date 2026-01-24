set(math_fppFiles
    stdlib_math_all_close.fypp
    stdlib_math_arange.fypp
    stdlib_math_diff.fypp
    stdlib_math_is_close.fypp
    stdlib_math_linspace.fypp
    stdlib_math_logspace.fypp
    stdlib_math_meshgrid.fypp
    )

set(math_cppFiles
    stdlib_math.fypp
    )

configure_stdlib_target(${PROJECT_NAME}_math "" math_fppFiles math_cppFiles)

if(STDLIB_BITSET)
  target_link_libraries(${PROJECT_NAME}_math PUBLIC ${PROJECT_NAME}_bitsets)
endif()

target_link_libraries(${PROJECT_NAME}_math PUBLIC ${PROJECT_NAME}_core ${PROJECT_NAME}_strings)
