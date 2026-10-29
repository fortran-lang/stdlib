module stdlib_stats
  use stdlib_random, only: random_seed, dist_rand
  use stdlib_stats_descriptive, only: corr, cov, mean, median, moment, var, &
                                      pca, pca_transform, pca_inverse_transform
  use stdlib_stats_distribution_beta, only: rvs_beta, pdf_beta, cdf_beta
  use stdlib_stats_distribution_exponential, only: rvs_exp, pdf_exp, cdf_exp
  use stdlib_stats_distribution_gamma, only: rvs_gamma, pdf_gamma, cdf_gamma
  use stdlib_stats_distribution_normal, only: rvs_normal, pdf_normal, cdf_normal
  use stdlib_stats_distribution_uniform, only: rvs_uniform, pdf_uniform, cdf_uniform, shuffle
  implicit none(type, external)
  private
  public :: random_seed, dist_rand
  public :: corr, cov, mean, median, moment, var
  public :: pca, pca_transform, pca_inverse_transform
  public :: rvs_beta, pdf_beta, cdf_beta
  public :: rvs_exp, pdf_exp, cdf_exp
  public :: rvs_gamma, pdf_gamma, cdf_gamma
  public :: rvs_normal, pdf_normal, cdf_normal
  public :: rvs_uniform, pdf_uniform, cdf_uniform, shuffle
 
end module stdlib_stats
