program example_normal_pdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_normal, only: norm_pdf => pdf_normal, &
                                              norm => rvs_normal

  implicit none
  real :: x(3, 4, 5), a(3, 4, 5), b(3, 4, 5)
  complex :: loc, scale
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  print *, norm_pdf(1.0, 0., 1.) !a probability density at 1.0 in standard normal

! 0.241970733

  print *, norm_pdf(2.0, -1.0, 2.0)
!a probability density at 2.0 with mu=-1.0 sigma=2.0

!6.47588000E-02

  x = reshape(norm(0.0, 1.0, 60), [3, 4, 5])
! standard normal random variates array

  a(:, :, :) = 0.0
  b(:, :, :) = 1.0
  print *, norm_pdf(x, a, b)  ! standard normal probability density array

!  0.340346158  0.285823315  0.398714304  0.391778737  0.389345556
!  0.364551932  0.386712372  0.274370432  0.215250477  0.378006011
!  0.215760440  0.177990928  0.278640658  0.223813817  0.356875211
!  0.285167664  0.378533930  0.390739858  0.271684974  0.138273031
!  0.135456234  0.331718773  0.398283750  0.383706540

  loc = (1.0, -0.5)
  scale = (1.0, 2.)
  print *, norm_pdf((1.5, 1.0), loc, scale)
! a complex normal probability density function at (1.5,1.0) with real part
  ! of mu=1.0, sigma=1.0 and imaginary part of mu=-0.5, sigma=2.0

! 5.30100204E-02

end program example_normal_pdf
