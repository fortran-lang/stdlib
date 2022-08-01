program example_exponential_pdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_exponential, only: exp_pdf => pdf_exp, &
                                                   rexp => rvs_exp

  implicit none
  real :: x(2, 3, 4), a(2, 3, 4)
  complex :: scale
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  print *, exp_pdf(1.0, 1.0) !a probability density at 1.0 in standard expon

! 0.367879450

  print *, exp_pdf(2.0, 2.0) !a probability density at 2.0 with lambda=2.0

! 3.66312787E-02

  x = reshape(rexp(0.5, 24), [2, 3, 4]) ! standard expon random variates array
  a(:, :, :) = 0.5
  print *, exp_pdf(x, a)     ! a rank 3 standard expon probability density

!  0.457115263  0.451488823  0.492391467  0.485233188  0.446215510
!  0.401670188  0.485127628  0.316924453  0.418474048  0.483173639
!  0.307366133  0.285812140  0.448017836  0.426440030  0.403896868
!  0.334653258  0.410376132  0.485370994  0.333617479  0.263791025
!  0.249779820  0.457159877  0.495636940  0.482243657

  scale = (1.0, 2.)
  print *, exp_pdf((1.5, 1.0), scale)
! a complex expon probability density function at (1.5,1.0) with real part
!of lambda=1.0 and imaginary part of lambda=2.0

! 6.03947677E-02

end program example_exponential_pdf
