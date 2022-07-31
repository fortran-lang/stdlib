program example_loading
  use stdlib_hashmaps, only: open_hashmap_type
  use stdlib_hashmap_wrappers, only: fnv_1_hasher
  implicit none
  type(open_hashmap_type) :: map
  real :: ratio
  call map%init(fnv_1_hasher)
  ratio = map%loading()
  print *, "Initial loading =  ", ratio
end program example_loading
