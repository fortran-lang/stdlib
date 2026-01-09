set(sorting_fppFiles
    )

set(sorting_cppFiles
    stdlib_sorting.fypp
    stdlib_sorting_ord_sort.fypp
    stdlib_sorting_sort_adjoint.fypp
    stdlib_sorting_sort.fypp
    )

set(sorting_f90Files
    stdlib_sorting_radix_sort.f90
    )

configure_stdlib_target(sorting sorting_f90Files sorting_fppFiles sorting_cppFiles)

target_link_libraries(sorting PUBLIC core strings $<$<NOT:$<BOOL:${STDLIB_NO_BITSET}>>:bitsets>)
