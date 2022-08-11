program example_rehash
  use stdlib_kinds, only: int8
  use stdlib_hashmaps, only: open_hashmap_type
  use stdlib_hashmap_wrappers, only: fnv_1_hasher, fnv_1a_hasher, &
                                     key_type, other_type, set
  implicit none
  type(open_hashmap_type) :: map
  type(key_type)      :: key
  type(other_type)    :: other
  class(*), allocatable :: dummy
  allocate (dummy, source='a dummy value')
  call map%init(fnv_1_hasher, slots_bits=10)
  call set(key, [5_int8, 7_int8, 4_int8, 13_int8])
  call set(other, dummy)
  call map%map_entry(key, other)
  call map%rehash(fnv_1a_hasher)
end program example_rehash
