program example_hasher_fun
  use stdlib_hashmap_wrappers, only: fnv_1a_hasher, hasher_fun, set, key_type
  use stdlib_kinds, only: int8, int32
  implicit none
  procedure(hasher_fun), pointer :: hasher_pointer
  integer(int8), allocatable :: array1(:)
  integer(int32) :: hash
  type(key_type) :: key
  hasher_pointer => fnv_1a_hasher
  array1 = [5_int8, 4_int8, 3_int8, 1_int8, 10_int8, 4_int8]
  call set(key, array1)
  hash = hasher_pointer(key)
  print *, hash
end program example_hasher_fun
