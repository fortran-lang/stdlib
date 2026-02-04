set(spatial_fppFiles
    stdlib_spatial.fypp
    stdlib_spatial_kabsch.fypp
    )

set(spatial_cppFiles
    )

set(spatial_f90Files
    )

configure_stdlib_target(${PROJECT_NAME}_spatial spatial_f90Files spatial_fppFiles spatial_cppFiles)

target_link_libraries(${PROJECT_NAME}_spatial PUBLIC ${PROJECT_NAME}_constants ${PROJECT_NAME}_linalg_core ${PROJECT_NAME}_linalg ${PROJECT_NAME}_intrinsics)