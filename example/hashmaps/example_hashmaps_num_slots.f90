program example_num_slots
  use stdlib_hashmaps, only: chaining_hashmap_type, int_index
  use stdlib_hashmap_wrappers, only: fnv_1_hasher
  implicit none
  type(chaining_hashmap_type) :: map
  integer(int_index) :: initial_slots
  call map%init(fnv_1_hasher)
  initial_slots = map%num_slots()
  print *, "Initial slots =  ", initial_slots
end program example_num_slots
