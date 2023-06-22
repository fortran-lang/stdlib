program example_normal_cdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_normal, only: norm_cdf => cdf_normal, &
                                              norm => rvs_normal

  implicit none
  real, dimension(2, 3, 4) :: x, mu, sigma
  real :: xsum
  complex :: loc, scale
  integer :: seed_put, seed_get, i

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  ! standard normal cumulative probability at x=0.0
  print *, norm_cdf(0.0, 0.0, 1.0)
  ! 0.500000000

  ! cumulative probability at x=2.0 with mu=-1.0 sigma=2.0
  print *, norm_cdf(2.0, -1.0, 2.0)
  ! 0.933192849

  ! cumulative probability at x=1.0 with mu=1.0 and sigma=-1.0 (out of range) 
  print *, norm_cdf(1.0, 1.0, -1.0)
  ! NaN

  ! standard normal random variates array
  x = reshape(norm(0.0, 1.0, 24), [2, 3, 4])

  ! standard normal cumulative array
  mu(:, :, :) = 0.0
  sigma(:, :, :) = 1.0
  print *, norm_cdf(x, mu, sigma)        
  !  0.713505626     0.207069695  0.486513376  0.424511284  0.587328553
  !  0.335559726     0.401470929  0.806552052  0.866687536  0.371323735
  !  0.866228044     0.898046613  0.198435277  0.141147852  0.681565762
  !  0.206268221     0.627057910  0.580759525  0.190364420  7.27325380E-02
  !  7.08068311E-02  0.728241026  0.522919059  0.390097380

   ! cumulative probability array where sigma<=0.0 for certain elements 
  print *, norm_cdf([1.0, 1.0, 1.0], [1.0, 1.0, 1.0], [1.0, 0.0, -1.0])
  ! 0.500000000  NaN NaN

  ! `cdf_normal` is pure and, thus, can be called concurrently 
  xsum = 0.0
  do concurrent (i=1:size(x,3))
    xsum = xsum + sum(norm_cdf(x(:,:,i), mu(:,:,i), sigma(:,:,i)))
  end do
  print *, xsum
  ! 11.3751936

  ! complex normal cumulative distribution at x=(0.5,-0.5) with real part of
  ! mu=1.0, sigma=0.5 and imaginary part of mu=0.0, sigma=1.0
  loc = (1.0, 0.0)
  scale = (0.5, 1.0)
  print *, norm_cdf((0.5, -0.5), loc, scale)
  ! 4.89511043E-02

end program example_normal_cdf

