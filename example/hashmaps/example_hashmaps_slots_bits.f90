program example_slots_bits
  use stdlib_hashmaps, only: chaining_hashmap_type
  implicit none
  type(chaining_hashmap_type) :: map
  integer :: bits
  call map%init()
  bits = map%slots_bits()
  print *, "Initial slot bits =  ", bits
end program example_slots_bits
