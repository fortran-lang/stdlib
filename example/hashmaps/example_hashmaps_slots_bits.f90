program example_slots_bits
  use stdlib_hashmaps, only: chaining_hashmap_type
  use stdlib_hashmap_wrappers, only: fnv_1_hasher
  implicit none
  type(chaining_hashmap_type) :: map
  integer :: bits
  call map%init(fnv_1_hasher)
  bits = map%slots_bits()
  print *, "Initial slot bits =  ", bits
end program example_slots_bits
