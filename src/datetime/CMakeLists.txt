set(datetime_f90Files
    stdlib_datetime.f90
    )

configure_stdlib_target(${PROJECT_NAME}_datetime datetime_f90Files "" "")

target_link_libraries(${PROJECT_NAME}_datetime PUBLIC ${PROJECT_NAME}_core)
