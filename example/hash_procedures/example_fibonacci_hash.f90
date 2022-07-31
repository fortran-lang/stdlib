program example_fibonacci_hash
  use stdlib_hash_32bit, only: fibonacci_hash
  use iso_fortran_env, only: int32
  implicit none
  integer, allocatable :: array1(:)
  integer(int32) :: hash, source
  allocate (array1(0:2**6 - 1))
  array1(:) = 0
  source = 42_int32
  hash = fibonacci_hash(source, 6)
  array1(hash) = source
  print *, hash
end program example_fibonacci_hash
