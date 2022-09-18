program example_set_other_data
  use stdlib_kinds, only: int8
  use stdlib_hashmaps, only: open_hashmap_type
  use stdlib_hashmap_wrappers, only: fnv_1_hasher, &
                                     fnv_1a_hasher, key_type, other_type, set
  implicit none
  logical :: exists
  type(open_hashmap_type) :: map
  type(key_type)      :: key
  type(other_type)    :: other
  class(*), allocatable :: dummy
  call map%init(fnv_1_hasher, slots_bits=10)
  allocate (dummy, source='A value')
  call set(key, [5_int8, 7_int8, 4_int8, 13_int8])
  call set(other, dummy)
  call map%map_entry(key, other)
  deallocate (dummy)
  allocate (dummy, source='Another value')
  call set(other, dummy)
  call map%set_other_data(key, other, exists)
  print *, 'The entry to have its other data replaced exists = ', exists
end program example_set_other_data
