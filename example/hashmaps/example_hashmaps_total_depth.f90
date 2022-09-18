program example_total_depth
  use stdlib_hashmaps, only: chaining_hashmap_type, int_depth
  use stdlib_hashmap_wrappers, only: fnv_1_hasher
  implicit none
  type(chaining_hashmap_type) :: map
  integer(int_depth) :: initial_depth
  call map%init(fnv_1_hasher)
  initial_depth = map%total_depth()
  print *, "Initial total depth =  ", initial_depth
end program example_total_depth
