program example_shuffle
  use stdlib_random, only: random_seed
  use stdlib_stats_distribution_uniform, only: shuffle
  implicit none
  integer :: seed_put, seed_get, i
  real :: x(10)
  integer :: n(10)
  complex :: z(10)

  do i = 1, 10
    n(i) = i
    x(i) = real(i)
    z(i) = cmplx(real(i), real(i))
  end do
  seed_put = 32165498
  call random_seed(seed_put, seed_get)    ! set and get current value of seed
  print *, shuffle(n)                          ! get randomized n

!10   6   9   2   8   1   3   5   7   4

  print *, shuffle(x)                          ! get randomized x

!5.0   10.0   9.0   4.0   3.0   8.0   2.0   1.0   7.0   6.0

  print *, shuffle(z)                          ! get randomized z

!(8.0, 8.0)    (7.0, 7.0)    (4.0, 4.0)    (1.0, 1.0)    (5.0, 5.0)
!(9.0, 9.0)    (6.0, 6.0)    (3.0, 3.0)    (2.0, 2.0)    (10.0, 10.0)

end program example_shuffle
