program example_calls
  use stdlib_hashmaps, only: chaining_hashmap_type, int_calls
  implicit none
  type(chaining_hashmap_type) :: map
  integer(int_calls) :: initial_calls
  call map%init()
  initial_calls = map%calls()
  print *, "INITIAL_CALLS =  ", initial_calls
end program example_calls
