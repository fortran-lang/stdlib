program example_seeded_nmhash32x_hasher
  use stdlib_kinds, only: int8, int32
  use stdlib_hashmap_wrappers, only: &
    seeded_nmhash32x_hasher, key_type, set
  implicit none
  integer(int8), allocatable :: array1(:)
  integer(int32) :: hash
  type(key_type) :: key
  array1 = [5_int8, 4_int8, 3_int8, 1_int8, 10_int8, 4_int8]
  call set(key, array1)
  hash = seeded_nmhash32x_hasher(key)
  print *, hash
end program example_seeded_nmhash32x_hasher
