program example_gamma_pdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_gamma, only: rgamma => rvs_gamma, &
                                               gamma_pdf => pdf_gamma

  implicit none
  real :: x(2, 3, 4), loc(2, 3, 4), shape(2, 3, 4), rate(2, 3, 4)
  complex :: cloc, cshape, crate
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  ! probability density at x=1.0 with loc=0.0, shape=1.0, rate=1.0
  print *, gamma_pdf(1.0, 0.0, 1.0, 1.0)
  ! 0.367879450

  ! probability density at x=2.0 with loc=0.0, shape=2.0, rate=2.0
  print *, gamma_pdf(2.0, 0.0, 2.0, 2.0)
  ! 0.270670563

  ! probability density at x=0.5 with loc=1.0 (x < loc)
  print *, gamma_pdf(0.5, 1.0, 2.0, 1.0)
  ! 0.00000000

  ! probability density at x=1.0 with invalid shape (shape <= 0)
  print *, gamma_pdf(1.0, 0.0, -1.0, 1.0)
  ! NaN

  ! gamma random variates array with loc=0.0, shape=2.0, rate=1.0
  loc(:, :, :) = 0.0
  shape(:, :, :) = 2.0
  rate(:, :, :) = 1.0
  x = reshape(rgamma(0.0, 2.0, 1.0, 24), [2, 3, 4])

  ! a rank 3 gamma probability density array
  print *, gamma_pdf(x, loc, shape, rate)
  ! 0.204550430  0.320178866  0.274986655  0.348611295  0.101865448
  ! 0.102199331  0.358981341  0.223676488  0.254329354  0.356714427
  ! 0.267390072  0.305148095  0.367848188  7.26194456E-02  1.49471285E-02
  ! 0.246272027  0.360770017  0.339665830  0.101558588  0.358678699
  ! 0.224196941  0.359253854  7.56355673E-02  0.251869917

  cloc = (0.0, 0.0)
  cshape = (1.0, 1.5)
  crate = (1.0, 2.0)
  ! complex gamma probability density function at (1.5, 1.0) with real part
  ! of shape=1.0, rate=1.0 and imaginary part of shape=1.5, rate=2.0
  print *, gamma_pdf((1.5, 1.0), cloc, cshape, crate)
  ! 9.63761061E-02

end program example_gamma_pdf
