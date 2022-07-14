program example_norm_cdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_normal, only: norm_cdf => cdf_normal, &
                                              norm => rvs_normal

  implicit none
  real :: x(2, 3, 4), a(2, 3, 4), b(2, 3, 4)
  complex :: loc, scale
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  print *, norm_cdf(1.0, 0.0, 1.0)  ! a standard normal cumulative at 1.0

! 0.841344714

  print *, norm_cdf(2.0, -1.0, 2.0)
! a cumulative at 2.0 with mu=-1 sigma=2

! 0.933192849

  x = reshape(norm(0.0, 1.0, 24), [2, 3, 4])
! standard normal random variates array

  a(:, :, :) = 0.0
  b(:, :, :) = 1.0
  print *, norm_cdf(x, a, b)        ! standard normal cumulative array

!  0.713505626  0.207069695  0.486513376  0.424511284  0.587328553
!  0.335559726  0.401470929  0.806552052  0.866687536  0.371323735
!  0.866228044  0.898046613  0.198435277  0.141147852  0.681565762
!  0.206268221  0.627057910  0.580759525  0.190364420  7.27325380E-02
!  7.08068311E-02  0.728241026  0.522919059  0.390097380

  loc = (1.0, 0.0)
  scale = (0.5, 1.0)
  print *, norm_cdf((0.5, -0.5), loc, scale)
!complex normal cumulative distribution at (0.5,-0.5) with real part of
  !mu=1.0, sigma=0.5 and imaginary part of mu=0.0, sigma=1.0

!4.89511043E-02

end program example_norm_cdf

