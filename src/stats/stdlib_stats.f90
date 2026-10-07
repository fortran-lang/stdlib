module stdlib_stats
  use stdlib_stats_descriptive, only: corr, cov, mean, median, moment, var, &
                                      pca, pca_transform, pca_inverse_transform
  implicit none(type, external)
  private
  public :: corr, cov, mean, median, moment, var
  public :: pca, pca_transform, pca_inverse_transform

end module stdlib_stats
