program example_fnv_1a_hash
  use stdlib_hash_32bit, only: fnv_1a_hash
  use iso_fortran_env, only: int32
  implicit none
  integer(int32) :: hash
  hash = fnv_1a_hash([5, 4, 3, 1, 10, 4, 9])
  print *, hash
end program example_fnv_1a_hash
