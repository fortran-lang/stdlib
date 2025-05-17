program example_exponential_rvs
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_exponential, only: rexp => rvs_exp

  implicit none
  complex :: cloc, cscale
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  ! single standard exponential random variate
  print *, rexp()
  ! 0.358690143

  ! exponential random variate with loc=0 and scale=0.5 (lambda=2)
  print *, rexp(0.0, 0.5)
  ! 0.122672431

  ! exponential random variate with lambda=2
   print *, rexp(2.0)
  ! 0.204114929

  ! exponential random variate with loc=0.6 and scale=0.2 (lambda=5)
  print *, rexp(0.6, 0.2)
  ! 0.681645989

  ! an array of 10 variates with loc=0.0 and scale=3.0 (lambda=1/3)
  print *, rexp(0.0, 3.0, 10)
  ! 1.36567295       2.62772131      0.362352759       5.47133636       2.13591909
  ! 0.410784155      5.83882189      6.71128035        1.31730068       1.90963650

  ! single complex exponential random variate with real part of scale=0.5 (lambda=2.0);
  ! imagainary part of scale=1.6 (lambda=0.625)
  cloc   = (0.0, 0.0)
  cscale = (0.5, 1.6)
  print *, rexp(cloc, cscale)
  ! (0.426896989,2.56968451)

end program example_exponential_rvs
