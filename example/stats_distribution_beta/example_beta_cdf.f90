program example_beta_cdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_beta, only: rbeta => rvs_beta, &
                                             beta_cdf => cdf_beta

  implicit none
  real, parameter :: a = 2.0, b = 5.0
  real :: xarr(2, 5)
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  ! cumulative probability at x=0.3 for beta(2,5) distribution
  print *, beta_cdf(0.3, a, b)
  ! 0.579824865

  ! cumulative probability at x=1.3 with loc=1.0 for beta(2,5) distribution
  print *, beta_cdf(1.3, a, b, 1.0)
  ! 0.579824746

  ! generate random variates and compute their cdf
  xarr = reshape(rbeta(a, b, 10), [2, 5])

  print *, beta_cdf(xarr, a, b)
  ! 0.686331749  0.625633657  0.625057578  0.158218294  0.786031485
  ! 7.17176571E-02  0.136123925  0.909627795  0.245356008  0.865481198

end program example_beta_cdf
