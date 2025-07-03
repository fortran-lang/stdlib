program example_num_slots
  use stdlib_hashmaps, only: chaining_hashmap_type, int_index
  implicit none
  type(chaining_hashmap_type) :: map
  integer(int_index) :: initial_slots
  call map%init()
  initial_slots = map%num_slots()
  print *, "Initial slots =  ", initial_slots
end program example_num_slots
