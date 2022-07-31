program example_dist_rand
  use stdlib_kinds, only: int8, int16, int32, int64
  use stdlib_random, only: dist_rand, random_seed
  implicit none
  integer :: put, get

  put = 135792468
  call random_seed(put, get)     ! set and get current value of seed
  print *, dist_rand(1_int8)     ! random integer in [-2^7, 2^7 - 1]
! -90
  print *, dist_rand(1_int16)    ! random integer in [-2^15, 2^15 - 1]
! -32725
  print *, dist_rand(1_int32)    ! random integer in [-2^31, 2^31 - 1]
! -1601563881
  print *, dist_rand(1_int64)    ! random integer in [-2^63, 2^63 - 1]
! 180977695517992208
end program example_dist_rand
