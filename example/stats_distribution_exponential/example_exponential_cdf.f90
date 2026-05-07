program example_exponential_cdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_exponential, only: exp_cdf => cdf_exp, &
                                                   rexp => rvs_exp

  implicit none
  real, dimension(2, 3, 4) :: x, loc, scale
  real :: xsum
  complex :: cloc, cscale
  integer :: seed_put, seed_get, i

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  ! standard exponential cumulative distribution at x=1.0 with loc=0.0, scale=1.0
  print *, exp_cdf(1.0, 0.0, 1.0)
  ! 0.632120550

  ! standard exponential cumulative distribution at x=1.0 with lambda=1.0
  print *, exp_cdf(1.0, 1.0)
  ! 0.632120550

  ! cumulative distribution at x=2.0 with lambda=2
  print *, exp_cdf(2.0, 2.0)
  ! 0.981684387

  ! cumulative distribution at x=2.0 with loc=0.0 and scale=0.5 (equivalent of lambda=2)
  print *, exp_cdf(2.0, 0.0, 0.5)
  ! 0.981684387

  ! cumulative distribution at x=2.5 with loc=0.5 and scale=0.5 (equivalent of lambda=2)
  print *, exp_cdf(2.5, 0.5, 0.5)
  ! 0.981684387

  ! cumulative distribution at x=2.0 with loc=0.0 and scale=-1.0 (out of range)
  print *, exp_cdf(2.0, 0.0, -1.0)
  ! NaN

  ! cumulative distribution at x=0.5 with loc=1.0 and scale=1.0, putting x below the minimum
  print *, exp_cdf(0.5, 1.0, 1.0)
  ! 0.00000000

   ! standard exponential random variates array
  x = reshape(rexp(0.0, 2.0, 24), [2, 3, 4])

  ! a rank-3 exponential cumulative distribution
  loc(:, :, :)   = 0.0
  scale(:, :, :) = 2.0
  print *, exp_cdf(x, loc, scale)
  ! 0.301409245  0.335173965  5.94930053E-02  0.113003314
  ! 0.365694344  0.583515942  0.113774836     0.838585377
  ! 0.509324908  0.127967060  0.857194781     0.893231630
  ! 0.355383813  0.470882893  0.574203610     0.799321830
  ! 0.546216846  0.111995399  0.801794767     0.922525287
  ! 0.937719882  0.301136374  3.44503522E-02  0.134661376


  ! cumulative distribution array where scale<=0.0 for certain elements
  print *, exp_cdf([1.0, 1.0, 1.0], [0.0, 0.0, 0.0], [1.0, 0.0, -1.0])
  ! 0.632120550  NaN NaN

  ! `cdf_exp` is pure and, thus, can be called concurrently
  xsum = 0.0
  do concurrent (i=1:size(x,3))
    xsum = xsum + sum(exp_cdf(x(:,:,i), loc(:,:,i), scale(:,:,i)))
  end do
  print *, xsum
  ! 11.0886612

  ! complex exponential cumulative distribution at (0.5, 0.0, 2) with real part of
  ! scale=2 and imaginary part of scale=1.0
  cloc   = (0.0, 0.0)
  cscale = (2, 1.0)
  print *, exp_cdf((0.5, 0.5), cloc, cscale)
  ! 8.70351046E-02

  ! As above, but with scale%im < 0
  cloc   = (0.0, 0.0)
  cscale = (1.0, -2.0)
  print *, exp_cdf((1.5, 1.0), cloc, cscale)
  ! NaN

end program example_exponential_cdf

