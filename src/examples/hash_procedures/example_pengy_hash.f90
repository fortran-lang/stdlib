program example_pengy_hash
  use stdlib_hash_64bit, only: new_pengy_hash_seed, pengy_hash
  use iso_fortran_env, only: int32, int64
  implicit none
  integer, allocatable :: key(:)
  integer(int64) :: hash
  integer(int32)  ::  seed
  key = [0, 1, 2, 3]
  seed = 0_int32
  call new_pengy_hash_seed(seed)
  hash = pengy_hash(key, seed)
  print *, seed, hash
end program example_pengy_hash
