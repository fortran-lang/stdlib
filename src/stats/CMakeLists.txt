set(stats_cppFiles
    )

set(stats_fppFiles
    stdlib_random.fypp
    stdlib_stats_corr.fypp
    stdlib_stats_cov.fypp
    stdlib_stats_distribution_exponential.fypp
    stdlib_stats_distribution_normal.fypp
    stdlib_stats_distribution_uniform.fypp
    stdlib_stats.fypp
    stdlib_stats_mean.fypp
    stdlib_stats_median.fypp
    stdlib_stats_moment_all.fypp
    stdlib_stats_moment.fypp
    stdlib_stats_moment_mask.fypp
    stdlib_stats_moment_scalar.fypp
    stdlib_stats_var.fypp
)

set(stats_f90Files
    )

configure_stdlib_target(${PROJECT_NAME}_stats stats_f90Files stats_fppFiles stats_cppFiles)

target_link_libraries(${PROJECT_NAME}_stats PUBLIC ${PROJECT_NAME}_core ${PROJECT_NAME}_linalg_core ${PROJECT_NAME}_linalg ${PROJECT_NAME}_selection ${PROJECT_NAME}_strings)
