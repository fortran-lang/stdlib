program example_exponential_cdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_exponential, only: exp_cdf => cdf_exp, &
                                                   rexp => rvs_exp

  implicit none
  real :: x(2, 3, 4), a(2, 3, 4)
  complex :: scale
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  print *, exp_cdf(1.0, 1.0)  ! a standard exponential cumulative at 1.0

! 0.632120550

  print *, exp_cdf(2.0, 2.0) ! a cumulative at 2.0 with lambda=2

! 0.981684387

  x = reshape(rexp(0.5, 24), [2, 3, 4])
! standard exponential random variates array
  a(:, :, :) = 0.5
  print *, exp_cdf(x, a)  ! a rank 3 array of standard exponential cumulative

!  8.57694745E-02  9.70223546E-02  1.52170658E-02  2.95336246E-02
!  0.107568979  0.196659625  2.97447443E-02  0.366151094  0.163051903
!  3.36527228E-02  0.385267735  0.428375721  0.103964329  0.147119939
!  0.192206264  0.330693483  0.179247737  2.92580128E-02  0.332765043
!  0.472417951  0.500440359  8.56802464E-02  8.72612000E-03  3.55126858E-02

  scale = (0.5, 1.0)
  print *, exp_cdf((0.5, 0.5), scale)
!complex exponential cumulative distribution at (0.5,0.5) with real part of
!lambda=0.5 and imaginary part of lambda=1.0

! 8.70351046E-02

end program example_exponential_cdf

