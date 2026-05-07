program example_key_test
  use stdlib_kinds, only: int8
  use stdlib_hashmaps, only: chaining_hashmap_type
  use stdlib_hashmap_wrappers, only: key_type, set
  implicit none
  type(chaining_hashmap_type) :: map
  type(key_type) :: key
  logical :: present
  
  call map%init()
  call set(key, [0_int8, 1_int8])
  call map%key_test(key, present)
  print *, "Initial key of 10 present for empty map =  ", present
  
end program example_key_test
