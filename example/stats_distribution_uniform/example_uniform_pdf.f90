program example_uniform_pdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_uniform, only: uni_pdf => pdf_uniform, &
                                               uni => rvs_uniform

  implicit none
  complex :: loc, scale
  real :: a(3, 4, 5), b(3, 4, 5), x(3, 4, 5)
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  print *, uni_pdf(3, 2, 10)       !probability density at 3 in range [2, 10]

! 9.09090936E-02

  print *, uni_pdf(0.5, 0.0, 1.0)    !a probability density at 0.5 in [0., 1.]

! 1.00000000

  print *, uni_pdf(0.7, -1.0, 2.0)   !a probability density at 0.7 in [-1., 1.]

! 0.500000000

  a(:, :, :) = 0.0
  b(:, :, :) = 2.0
  x = reshape(uni(0., 2., 60), [3, 4, 5])! uniform random variates array in [0., 2.]
  print *, uni_pdf(x, a, b)         ! probability density array in [0., 2.]

! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000
! 0.500000000  0.500000000  0.500000000  0.500000000  0.500000000  0.500000000

  loc = (-0.5, -0.5)
  scale = (1.0, 1.0)
  print *, uni_pdf((-0.1, 0.2), loc, scale)
! joint probability density at (-0.1,0.2) in [(-0.5, -0.5), (0.5, 0.5)]

! 1.00000000
end program example_uniform_pdf

