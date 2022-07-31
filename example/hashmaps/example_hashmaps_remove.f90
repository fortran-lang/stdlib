program example_remove
  use stdlib_kinds, only: int8
  use stdlib_hashmaps, only: open_hashmap_type, int_index
  use stdlib_hashmap_wrappers, only: fnv_1_hasher, &
                                     fnv_1a_hasher, key_type, other_type, set
  type(open_hashmap_type) :: map
  type(key_type)      :: key
  type(other_type)    :: other
  logical             :: existed
  class(*), allocatable :: dummy
  allocate (dummy, source=4.0)
  call map%init(fnv_1_hasher, slots_bits=10)
  call set(key, [5_int8, 7_int8, 4_int8, 13_int8])
  call set(other, dummy)
  call map%map_entry(key, other)
  call map%remove(key, existed)
  print *, "Removed key existed = ", existed
end program example_remove
