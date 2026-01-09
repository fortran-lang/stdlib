set(stats_cppFiles
    ../stdlib_linalg_constants.fypp
    )

set(stats_fppFiles
    ../stdlib_ascii.fypp
    ../stdlib_io.fypp
    ../stdlib_kinds.fypp
    ../stdlib_optval.fypp
    ../stdlib_error.fypp
    ../stdlib_linalg.fypp
    ../stdlib_linalg_diag.fypp
    ../stdlib_linalg_state.fypp
    ../stdlib_random.fypp
    ../stdlib_selection.fypp
    ../stdlib_string_type.fypp
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

configure_stdlib_target(stats "" stats_fppFiles stats_cppFiles)
