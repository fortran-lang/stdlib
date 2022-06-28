program example_fnv_1a_hash_64
  use stdlib_hash_64bit, only: fnv_1a_hash
  use iso_fortran_env, only: int64
  implicit none
  integer, allocatable :: array1(:)
  integer(int64) :: hash
  array1 = [5, 4, 3, 1, 10, 4, 9]
  hash = fnv_1a_hash(array1)
  print *, hash
end program example_fnv_1a_hash_64
