program example_exponential_rvs
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_exponential, only: rexp => rvs_exp

  implicit none
  complex :: scale
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)

  print *, rexp()         !single standard exponential random variate

! 0.358690143

  print *, rexp(2.0)       !exponential random variate with lambda=2.0

! 0.816459715

  print *, rexp(0.3, 10)   !an array of 10 variates with lambda=0.3

!  1.84008647E-02  3.59742008E-02  0.136567295  0.262772143  3.62352766E-02
!  0.547133625  0.213591918  4.10784185E-02  0.583882213  0.671128035

  scale = (2.0, 0.7)
  print *, rexp(scale)
!single complex exponential random variate with real part of lambda=2.0;
!imagainary part of lambda=0.7

! (1.41435969,4.081114382E-02)

end program example_exponential_rvs
