program example_fibonacci_hash_64
  use stdlib_hash_64bit, only: fibonacci_hash
  use iso_fortran_env, only: int64
  implicit none
  integer, allocatable :: array1(:)
  integer(int64) :: hash, source
  allocate (array1(0:2**6 - 1))
  array1(:) = 0
  source = int(Z'1FFFFFFFF', int64)
  hash = fibonacci_hash(source, 6)
  array1(hash) = source
  print *, hash
end program example_fibonacci_hash_64
