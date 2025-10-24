set(bitsets_fppFiles
    ../stdlib_kinds.fypp
    ../stdlib_optval.fypp
    stdlib_bitsets.fypp
    stdlib_bitsets_64.fypp
    stdlib_bitsets_large.fypp
)

configure_stdlib_target(bitsets "" bitsets_fppFiles "")
