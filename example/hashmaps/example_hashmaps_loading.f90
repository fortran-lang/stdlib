program example_loading
  use stdlib_hashmaps, only: open_hashmap_type
  implicit none
  type(open_hashmap_type) :: map
  real :: ratio
  call map%init()
  ratio = map%loading()
  print *, "Initial loading =  ", ratio
end program example_loading
