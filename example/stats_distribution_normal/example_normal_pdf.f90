program example_normal_pdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_normal, only: norm_pdf => pdf_normal, &
                                              norm => rvs_normal

  implicit none
  real, dimension(3, 4, 5) :: x, mu, sigma
  real :: xsum
  complex :: loc, scale
  integer :: seed_put, seed_get, i

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  ! probability density at x=1.0 in standard normal
  print *, norm_pdf(1.0, 0., 1.) 
  ! 0.241970733

  ! probability density at x=2.0 with mu=-1.0 and sigma=2.0 
  print *, norm_pdf(2.0, -1.0, 2.0)
  ! 6.47588000E-02

  ! probability density at x=1.0 with mu=1.0 and sigma=-1.0 (out of range) 
  print *, norm_pdf(1.0, 1.0, -1.0)
  ! NaN

  ! standard normal random variates array
  x = reshape(norm(0.0, 1.0, 60), [3, 4, 5])
  
  ! standard normal probability density array
  mu(:, :, :) = 0.0
  sigma(:, :, :) = 1.0
  print *, norm_pdf(x, mu, sigma)  
  !  0.340346158  0.285823315  0.398714304  0.391778737  0.389345556
  !  0.364551932  0.386712372  0.274370432  0.215250477  0.378006011
  !  0.215760440  0.177990928  0.278640658  0.223813817  0.356875211
  !  0.285167664  0.378533930  0.390739858  0.271684974  0.138273031
  !  0.135456234  0.331718773  0.398283750  0.383706540

  ! probability density array where sigma<=0.0 for certain elements 
  print *, norm_pdf([1.0, 1.0, 1.0], [1.0, 1.0, 1.0], [1.0, 0.0, -1.0])
  ! 0.398942292  NaN NaN

  ! `pdf_normal` is pure and, thus, can be called concurrently 
  xsum = 0.0
  do concurrent (i=1:size(x,3))
    xsum = xsum + sum(norm_pdf(x(:,:,i), mu(:,:,i), sigma(:,:,i)))
  end do
  print *, xsum
  ! 18.0754433

  ! complex normal probability density function at x=(1.5,1.0) with real part
  ! of mu=1.0, sigma=1.0 and imaginary part of mu=-0.5, sigma=2.0
  loc = (1.0, -0.5)
  scale = (1.0, 2.)
  print *, norm_pdf((1.5, 1.0), loc, scale)
  ! 5.30100204E-02

end program example_normal_pdf
