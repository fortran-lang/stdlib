program example_calls
  use stdlib_hashmaps, only: chaining_hashmap_type, int_calls
  use stdlib_hashmap_wrappers, only: fnv_1_hasher
  implicit none
  type(chaining_hashmap_type) :: map
  integer(int_calls) :: initial_calls
  call map%init(fnv_1_hasher)
  initial_calls = map%calls()
  print *, "INITIAL_CALLS =  ", initial_calls
end program example_calls
