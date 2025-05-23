program example_exponential_pdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_exponential, only: exp_pdf => pdf_exp, &
                                                    rexp => rvs_exp

  implicit none
  real, dimension(2, 3, 4) :: x, loc, scale
  real :: xsum
  complex :: cloc, cscale
  integer :: seed_put, seed_get, i

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  ! probability density at x=1.0 with loc=0 and scale=1.0
  print *, exp_pdf(1.0, 0.0, 1.0)
  ! 0.367879450

  ! probability density at x=1.0 with lambda=1.0
  print *, exp_pdf(1.0, 1.0)
  ! 0.367879450

  ! probability density at x=2.0 with lambda=2.0
  print *, exp_pdf(2.0, 2.0)
  ! 3.66312787E-02

  ! probability density at x=2.0 with loc=0.0 and scale=0.5 (lambda=2.0)
  print *, exp_pdf(2.0, 0.0, 0.5)
  ! 3.66312787E-02

  ! probability density at x=1.5 with loc=0.5 and scale=0.5 (lambda=2.0)
  print *, exp_pdf(2.5, 0.5, 0.5)
  ! 3.66312787E-02

  ! probability density at x=2.0 with loc=0.0 and scale=-1.0 (out of range)
  print *, exp_pdf(2.0, 0.0, -1.0)
  ! NaN

  ! standard exponential random variates array
  x = reshape(rexp(0.0, 2.0, 24), [2, 3, 4])

  ! a rank-3 exponential probability density
  loc(:, :, :)   = 0.0
  scale(:, :, :) = 2.0
  print *, exp_pdf(x, loc, scale)
  ! 0.349295378      0.332413018     0.470253497     0.443498343      0.317152828
  ! 0.208242029      0.443112582     8.07073265E-02  0.245337561      0.436016470
  ! 7.14025944E-02   5.33841923E-02  0.322308093     0.264558554      0.212898195
  ! 0.100339092      0.226891592     0.444002301     9.91026312E-02   3.87373678E-02
  ! 3.11400592E-02   0.349431813     0.482774824     0.432669312

  ! probability density array where scale<=0.0 for certain elements (loc = 0.0)
  print *, exp_pdf([1.0, 1.0, 1.0], [0.0, 0.0, 0.0], [1.0, 0.0, -1.0])
  ! 0.367879450  NaN NaN

  ! `pdf_exp` is pure and, thus, can be called concurrently
  xsum = 0.0
  do concurrent (i=1:size(x,3))
    xsum = xsum + sum(exp_pdf(x(:,:,i), loc(:,:,i), scale(:,:,i)))
  end do
  print *, xsum
  ! 6.45566940

  ! complex exponential probability density function at (1.5, 0.0, 1.0) with real part
  ! of scale=1.0 and imaginary part of scale=0.5
  cloc   = (0.0, 0.0)
  cscale = (1.0, 0.5)
  print *, exp_pdf((1.5, 1.0), cloc, cscale)
  ! 6.03947677E-02

  ! As above, but with scale%re < 0
  cloc   = (0.0, 0.0)
  cscale = (-1.0, 2.0)
  print *, exp_pdf((1.5, 1.0), cloc, cscale)
  ! NaN

end program example_exponential_pdf
