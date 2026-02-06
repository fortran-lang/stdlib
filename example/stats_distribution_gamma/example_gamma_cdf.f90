program example_gamma_cdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_gamma, only: rgamma => rvs_gamma, &
                                               gamma_cdf => cdf_gamma

  implicit none
  real :: x(2, 3, 4), g(2, 3, 4), s(2, 3, 4)
  complex :: shape, rate
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  ! a standard gamma cumulative at x=1.0 with loc=0.0, shape=0.5, rate=1.0
  print *, gamma_cdf(1.0, 0.0, 0.5, 1.0)
  ! 0.842700839

  ! a cumulative at x=2.0 with loc=0.0, shape=1.5, rate=2.0
  print *, gamma_cdf(2.0, 0.0, 1.5, 2.0)
  ! 0.953988254

  ! cumulative at x=0.5 with loc=1.0 (x < loc)
  print *, gamma_cdf(0.5, 1.0, 2.0, 1.0)
  ! 0.00000000

  ! cumulative at x=1.0 with invalid rate (rate <= 0)
  print *, gamma_cdf(1.0, 0.0, 2.0, -1.0)
  ! NaN

  ! gamma random variates array with shape=1.0, rate=1.0
  g(:, :, :) = 1.0
  s(:, :, :) = 1.0
  x = reshape(rgamma(0.0, 1.0, 1.0, 24), [2, 3, 4])

  ! a rank 3 standard gamma cumulative array
  print *, gamma_cdf(x, 0.0, g, s)
  ! 0.710880339  0.472411335  0.578345954  0.383050948  0.870905757
  ! 0.870430350  0.170215249  0.677347481  0.620089889  0.161825046
  ! 4.17549349E-02  0.510665894  0.252201647  0.911497891  0.984424412
  ! 0.635621786  0.177783430  0.414842933  0.871342421  0.338317066
  ! 2.06879266E-02  0.335232288  0.907408893  0.624871135

  shape = (0.7, 2.1)
  rate = (0.5, 1.0)
  ! complex gamma cumulative distribution at (0.5, 0.5) with real part of
  ! shape=0.7, rate=0.5 and imaginary part of shape=2.1, rate=1.0
  print *, gamma_cdf((0.5, 0.5), (0.0, 0.0), shape, rate)
  ! 2.87349485E-02

end program example_gamma_cdf
