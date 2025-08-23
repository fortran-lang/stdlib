program example_rehash
  use stdlib_kinds, only: int8
  use stdlib_hashmaps, only: open_hashmap_type
  use stdlib_hashmap_wrappers, only: fnv_1a_hasher, &
                                     key_type, set
  implicit none
  type(open_hashmap_type) :: map
  type(key_type)      :: key
  call map%init(slots_bits=10)
  call set(key, [5_int8, 7_int8, 4_int8, 13_int8])
  call map%map_entry(key, 'A value')
  call map%rehash(fnv_1a_hasher)
end program example_rehash
