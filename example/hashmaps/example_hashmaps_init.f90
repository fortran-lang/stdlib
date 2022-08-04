program example_init
  use stdlib_hashmaps, only: chaining_hashmap_type
  use stdlib_hashmap_wrappers, only: fnv_1_hasher
  implicit none
  type(chaining_hashmap_type) :: map
  call map%init(fnv_1_hasher, slots_bits=10)
end program example_init
