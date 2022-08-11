program example_universal_mult_hash_64
  use stdlib_hash_64bit, only: odd_random_integer, &
                               universal_mult_hash
  use iso_fortran_env, only: int64
  implicit none
  integer, allocatable :: array1(:)
  integer(int64) :: hash, seed, source
  seed = 0
  allocate (array1(0:2**6 - 1))
  array1 = 0
  call odd_random_integer(seed)
  source = 42_int64
  hash = universal_mult_hash(source, seed, 6)
  array1(hash) = source
  print *, seed, hash, array1
end program example_universal_mult_hash_64
