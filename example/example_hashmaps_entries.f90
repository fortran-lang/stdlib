program example_entries
  use stdlib_hashmaps, only: open_hashmap_type, int_index
  implicit none
  type(open_hashmap_type) :: map
  integer(int_index) :: initial_entries
  call map%init()
  initial_entries = map%entries()
  print *, "INITIAL_ENTRIES =  ", initial_entries
end program example_entries
