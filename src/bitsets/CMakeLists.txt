set(bitsets_fppFiles
    stdlib_bitsets.fypp
    stdlib_bitsets_64.fypp
    stdlib_bitsets_large.fypp
)

configure_stdlib_target(${PROJECT_NAME}_bitsets "" bitsets_fppFiles "")

target_link_libraries(${PROJECT_NAME}_bitsets PUBLIC ${PROJECT_NAME}_core)
