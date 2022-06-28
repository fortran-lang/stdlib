program example_spooky_hash
  use stdlib_hash_64bit, only: new_spooky_hash_seed, &
                               spooky_hash
  use iso_fortran_env, only: int64
  implicit none
  integer, allocatable :: key(:)
  integer(int64) :: hash(2), seed(2)
  key = [0, 1, 2, 3]
  seed = [119_int64, 2_int64**41 - 1]
  call new_spooky_hash_seed(seed)
  hash = spooky_hash(key, seed)
  print *, seed, hash
end program example_spooky_hash
