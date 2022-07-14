program example_random_seed
  use stdlib_random, only: random_seed
  implicit none
  integer :: seed_put, seed_get

  seed_put = 1234567
  call random_seed(seed_put, seed_get)     ! set and get current value of seed
end program example_random_seed
