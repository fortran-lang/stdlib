program example_entries
  use stdlib_hashmaps, only: open_hashmap_type, int_index
  use stdlib_hashmap_wrappers, only: fnv_1_hasher
  implicit none
  type(open_hashmap_type) :: map
  integer(int_index) :: initial_entries
  call map%init(fnv_1_hasher)
  initial_entries = map%entries()
  print *, "INITIAL_ENTRIES =  ", initial_entries
end program example_entries
