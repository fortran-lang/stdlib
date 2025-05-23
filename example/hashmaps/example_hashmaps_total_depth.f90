program example_total_depth
  use stdlib_hashmaps, only: chaining_hashmap_type, int_depth
  implicit none
  type(chaining_hashmap_type) :: map
  integer(int_depth) :: initial_depth
  call map%init()
  initial_depth = map%total_depth()
  print *, "Initial total depth =  ", initial_depth
end program example_total_depth
