program example_gamma_rvs
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_gamma, only: rgamma => rvs_gamma

  implicit none
  real :: shape_arr(2, 3, 4)
  complex :: cloc, cshape, crate
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  ! single standard gamma random variate with shape=2.0, rate=1.0
  print *, rgamma(2.0)
  ! 2.50538206

  ! gamma random variate with loc=0.0, shape=3.0, rate=2.0
  print *, rgamma(0.0, 3.0, 2.0)
  ! 1.30591583

  ! a rank-3 array of 60 standard gamma random variates with shape=0.5
  shape_arr(:, :, :) = 0.5
  print *, rgamma(shape_arr)
  ! 1.03841162  1.33044529  0.912742674  0.131288037  0.638593793
  ! 1.03565669E-02  0.624804378  1.12179172  4.91380468E-02  6.69969944E-03
  ! 6.67014271E-02  0.132111162  0.101102419  0.648416579  1.14922595
  ! 2.29003578E-02  1.85964716E-04  1.21213868E-02  1.69112933
  ! 7.30440915E-02  0.395139128  0.182758048  0.427981257  0.985665262

  ! an array of 10 random variates with loc=0.0, shape=0.5, rate=1.0
  print *, rgamma(0.0, 0.5, 1.0, 10)
  ! 1.39297554E-04  0.296419382  0.352113068  2.80515051  3.65264394E-04
  ! 0.197743446  5.54569438E-02  9.30598825E-02  1.02596343  1.85311246

  cloc = (0.0, 0.0)
  cshape = (3.0, 4.0)
  crate = (2.0, 0.7)
  ! single complex gamma random variate with real part of shape=3.0, rate=2.0;
  ! imaginary part of shape=4.0, rate=0.7
  print *, rgamma(cloc, cshape, crate)
  ! (0.826188326,3.54749799)

end program example_gamma_rvs
