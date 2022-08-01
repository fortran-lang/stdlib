program example_uniform_cdf
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_uniform, only: uni_cdf => cdf_uniform, &
                                               uni => rvs_uniform

  implicit none
  real :: x(3, 4, 5), a(3, 4, 5), b(3, 4, 5)
  complex :: loc, scale
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  print *, uni_cdf(0.5, 0., 1.)      ! a cumulative at 0.5 in [0., 1.]

!0.500000000

  print *, uni_cdf(0.7, -1.0, 2.0)   ! a cumulative at 0.7 in [-1.0, 1.0]

! 0.850000024

  print *, uni_cdf(6, 2, 10)       ! a cumulative at 6 in [2, 10]

! 0.454545468

  a(:, :, :) = -1.0
  b(:, :, :) = 2.0
  x = reshape(uni(-1.0, 2.0, 60), [3, 4, 5]) ! uniform random variates array
  print *, uni_cdf(x, a, b)        ! cumulative array in [-1.0, 1.0]

!0.161520004  0.553248405  0.986900032  0.942091405  0.114239901  0.780188501
! 0.854656875  0.464386612  0.284466714  0.748768032  0.301834047  0.337008357
!0.568843365  0.596165061  0.180993259  0.614166319  0.214835495 7.98164606E-02
!0.641274095  0.607101977  0.701139212  0.230517209  1.97925568E-02 0.857982159
!0.712761045  0.139202654  0.361759573  0.796536088  0.356012046  0.197665215
!9.80764329E-02 0.781620383  0.595349193  0.125651121  0.957528770  0.942990601
!0.259489566  7.84273148E-02  0.779313922  0.317909390  0.559013724 0.421358019
!0.878484428  7.67416358E-02  0.298707575  0.693327367  0.146014273 0.102338850
!0.855926156  0.250811368  0.300751567  0.110186398  0.502883077  0.738479793
!0.764856219  0.294822574  1.90783739E-02 0.631218433 0.752170086  0.196848959

  loc = (0., 0.)
  scale = (2., 1.)
  print *, uni_cdf((1.2, 0.5), loc, scale)
! joint cumulative distribution at (1.2,0.5) in [(0.,0.), (2.,1.)]

! 0.300000012
end program example_uniform_cdf

