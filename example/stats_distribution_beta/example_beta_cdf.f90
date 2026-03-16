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
  ! 0.471808970

  ! cumulative probability at x=1.3 with loc=1.0 for beta(2,5) distribution
  print *, beta_cdf(1.3, a, b, 1.0)
  ! 0.471808970

  ! generate random variates and compute their cdf
  xarr = reshape(rbeta(a, b, 10), [2, 5])

  print *, beta_cdf(xarr, a, b)
  ! 0.374293357  0.136472717  0.153627276  0.166885555  0.535913110
  ! 4.47619371E-02  0.161991328  0.524897814  7.37934634E-02  8.41872990E-02

end program example_beta_cdf
