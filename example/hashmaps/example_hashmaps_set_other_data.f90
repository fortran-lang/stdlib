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
    
  ! Initialize hashmap with 2^10 slots.
  ! Hashmap will dynamically increase size if needed.
  call map%init(fnv_1_hasher, slots_bits=10)
  call set(key, [5, 7, 4, 13])
  call set(other, 'A value')
  call map%map_entry(key, other)
  
  call set(other, 'Another value')
  call map%set_other_data(key, other, exists)
  print *, 'The entry to have its other data replaced exists = ', exists
  
end program example_set_other_data
