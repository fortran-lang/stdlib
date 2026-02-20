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

configure_stdlib_target(${PROJECT_NAME}_sorting sorting_f90Files sorting_fppFiles sorting_cppFiles)

if(STDLIB_BITSETS)
  target_link_libraries(${PROJECT_NAME}_sorting PUBLIC ${PROJECT_NAME}_bitsets)
endif()

target_link_libraries(${PROJECT_NAME}_sorting PUBLIC ${PROJECT_NAME}_core ${PROJECT_NAME}_strings)
