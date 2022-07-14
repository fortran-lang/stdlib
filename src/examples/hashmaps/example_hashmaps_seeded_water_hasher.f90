program example_seeded_water_hasher
  use stdlib_hashmap_wrappers, only: &
    seeded_water_hasher, key_type, set
  use iso_fortran_env, only: int8, int32
  implicit none
  integer(int8), allocatable :: array1(:)
  integer(int32) :: hash
  type(key_type) :: key
  array1 = [5_int8, 4_int8, 3_int8, 1_int8, 10_int8, 4_int8]
  call set(key, array1)
  hash = seeded_water_hasher(key)
  print *, hash
end program example_seeded_water_hasher
