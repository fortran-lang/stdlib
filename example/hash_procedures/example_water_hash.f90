program example_water_hash
  use stdlib_hash_32bit, only: water_hash, &
                               new_water_hash_seed
  use iso_fortran_env, only: int32, int64
  implicit none
  integer(int32) :: hash
  integer(int64) :: seed = 42_int64
  call new_water_hash_seed(seed)
  hash = water_hash([5, 4, 3, 1, 10, 4, 9], seed)
  print *, hash, seed
end program example_water_hash
