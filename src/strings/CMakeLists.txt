set(strings_fppFiles
    stdlib_string_type.fypp
    stdlib_string_type_constructor.fypp
    stdlib_str2num.fypp
    stdlib_strings_to_string.fypp
    stdlib_strings.fypp
    )

set(strings_cppFiles
    )

set(strings_f90Files
    )

configure_stdlib_target(strings strings_f90Files strings_fppFiles strings_cppFiles)

target_link_libraries(strings PUBLIC core)
