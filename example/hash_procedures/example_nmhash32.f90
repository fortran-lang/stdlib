program example_nmhash32
  use stdlib_hash_32bit, only: nmhash32, &
                               new_nmhash32_seed
  use iso_fortran_env, only: int32
  implicit none
  integer(int32) :: hash
  integer(int32) :: seed = 42_int32
  call new_nmhash32_seed(seed)
  hash = nmhash32([5, 4, 3, 1, 10, 4, 9], seed)
  print *, seed, hash
end program example_nmhash32
