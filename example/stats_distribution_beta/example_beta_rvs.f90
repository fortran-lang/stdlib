program example_beta_rvs
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_beta, only: rbeta => rvs_beta

  implicit none
  real :: a_arr(2, 3, 4)
  complex :: ca, cb
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  ! single beta random variate with a=2.0, b=5.0 (loc=0.0 by default)
  print *, rbeta(2.0, 5.0)
  ! 0.235164985

  ! beta random variate with a=2.0, b=5.0, loc=1.0
  print *, rbeta(2.0, 5.0, 1.0)
  ! 1.23516498

  ! a rank-3 array of 24 beta random variates with a=0.5, b=0.5
  a_arr(:, :, :) = 0.5
  print *, rbeta(a_arr, a_arr)
  ! 0.894186497  0.948506236  0.899142742  0.293822825  0.751733482
  ! 0.170928627  0.742042720  0.921871543  0.112629898  0.153393656
  ! 0.188625366  0.291826040  0.238829076  0.764039755  0.935611486
  ! 0.454867721  8.74810152E-03  0.258653969  0.963788986
  ! 0.202841997  0.689699173  0.537226677  0.721585333  0.891451001

  ! an array of 10 random variates with a=2.0, b=5.0 (loc=0.0 by default)
  print *, rbeta(2.0, 5.0, 10)
  ! 2.59639323E-02  0.401881814  0.451093256  0.863215625  6.78956718E-03
  ! 0.316774905  0.141516894  0.199765816  0.616839588  0.555854380

  ! an array of 10 random variates with a=2.0, b=5.0, loc=1.0
  print *, rbeta(2.0, 5.0, 10, 1.0)
  ! 1.02596393  1.40188181  1.45109326  1.86321562  1.00678957
  ! 1.31677490  1.14151689  1.19976582  1.61683959  1.55585438

  ca = (2.0, 3.0)
  cb = (5.0, 4.0)
  ! single complex beta random variate with real part a=2.0, b=5.0;
  ! imaginary part a=3.0, b=4.0 (loc=(0,0) by default)
  print *, rbeta(ca, cb)
  ! (0.247691274,0.337867618)

end program example_beta_rvs
