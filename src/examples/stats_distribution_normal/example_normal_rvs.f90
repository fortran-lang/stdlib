program example_normal_rvs
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_normal, only: norm => rvs_normal

  implicit none
  real ::  a(2, 3, 4), b(2, 3, 4)
  complex :: loc, scale
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  print *, norm()           !single standard normal random variate

! 0.563655198

  print *, norm(1.0, 2.0)
!normal random variate mu=1.0, sigma=2.0

! -0.633261681

  print *, norm(0.0, 1.0, 10)       !an array of 10 standard norml random variates

! -3.38123664E-02  -0.190365672  0.220678389  -0.424612164  -0.249541596
!  0.865260184  1.11086845  -0.328349441  1.10873628  1.27049923

  a(:, :, :) = 1.0
  b(:, :, :) = 1.0
  print *, norm(a, b)         ! a rank 3 random variates array

!0.152776539  -7.51764774E-02  1.47208166  0.180561781  1.32407105
! 1.20383692  0.123445868  -0.455737948  -0.469808221  1.60750175
! 1.05748117  0.720934749  0.407810807  1.48165631  2.31749439
! 0.414566994  3.06084275  1.86505437  1.36338580  7.26878643E-02
! 0.178585172  1.39557445  0.828021586  0.872084975

  loc = (-1.0, 2.0)
  scale = (2.0, 1.0)
  print *, norm(loc, scale)
!single complex normal random variate with real part of mu=-1, sigma=2;
  !imagainary part of mu=2.0 and sigma=1.0

! (1.22566295,2.12518454)

end program example_normal_rvs
