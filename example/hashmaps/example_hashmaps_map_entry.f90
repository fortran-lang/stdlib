program example_map_entry
  use, intrinsic:: iso_fortran_env, only: int8
  use stdlib_hashmaps, only: chaining_hashmap_type
  use stdlib_hashmap_wrappers, only: fnv_1_hasher, key_type, other_type, set
  type(chaining_hashmap_type) :: map
  type(key_type)      :: key
  logical             :: conflict
  type(other_type)    :: other
  class(*), allocatable :: dummy
  allocate (dummy, source=4)
  call map%init(fnv_1_hasher, slots_bits=10)
  call set(key, [5_int8, 7_int8, 4_int8, 13_int8])
  call set(other, dummy)
  call map%map_entry(key, other, conflict)
  print *, 'CONFLICT = ', conflict
end program example_map_entry
