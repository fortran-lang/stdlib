set(math_fppFiles
    ../stdlib_ascii.fypp
    ../stdlib_kinds.fypp
    ../stdlib_optval.fypp
    ../stdlib_error.fypp
    ../stdlib_string_type.fypp
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

configure_stdlib_target(math "" math_fppFiles math_cppFiles)

target_link_libraries(math PUBLIC $<$<NOT:$<BOOL:${STDLIB_NO_BITSET}>>:bitsets>)
