program example_nmhash32x
  use stdlib_hash_32bit, only: nmhash32x, &
                               new_nmhash32x_seed
  use iso_fortran_env, only: int32
  implicit none
  integer(int32) :: hash
  integer(int32) :: seed = 42_int32
  call new_nmhash32x_seed(seed)
  hash = nmhash32x([5, 4, 3, 1, 10, 4, 9], seed)
  print *, seed, hash
end program example_nmhash32x
