set(bitsets_fppFiles
    stdlib_bitsets.fypp
    stdlib_bitsets_64.fypp
    stdlib_bitsets_large.fypp
)

configure_stdlib_target(bitsets "" bitsets_fppFiles "")

target_link_libraries(bitsets PUBLIC core)
