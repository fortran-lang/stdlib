program example_universal_mult_hash
  use stdlib_hash_32bit, only: odd_random_integer, &
                               universal_mult_hash
  use iso_fortran_env, only: int32
  implicit none
  integer, allocatable :: array1(:)
  integer(int32) :: hash, i, seed, source
  seed = 0
  allocate (array1(0:2**6 - 1))
  do i = 0, 2**6 - 1
    array1(i) = i
  end do
  call odd_random_integer(seed)
  source = 42_int32
  hash = universal_mult_hash(source, seed, 6)
  array1(hash) = source
  print *, seed, hash, array1
end program example_universal_mult_hash
