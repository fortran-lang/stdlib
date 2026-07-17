program example_beta_pdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_beta, only: rbeta => rvs_beta, &
                                             beta_pdf => pdf_beta

  implicit none
  real, parameter :: a = 2.0, b = 5.0
  real :: xarr(2, 5)
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  ! probability density at x=0.3 for beta(2,5) distribution
  print *, beta_pdf(0.3, a, b)
  ! 2.16089988

  ! probability density at x=1.3 with loc=1.0 for beta(2,5) distribution
  print *, beta_pdf(1.3, a, b, 1.0)
  ! 2.16090035

  ! generate random variates and compute their pdf
  xarr = reshape(rbeta(a, b, 10), [2, 5])

  print *, beta_pdf(xarr, a, b)
  ! 1.85633695  2.04246974  2.04407597  2.16859674  1.47261345
  ! 1.67244565  2.07803488  0.819388986  2.38697886  1.08206940

end program example_beta_pdf
