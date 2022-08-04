program example_hashmaps_get_other_scalar
  use stdlib_hashmap_wrappers, only: &
    get, other_type, set
  implicit none
  integer :: value, result
  type(other_type) :: other
  value = 15
  call set( other, value )
  call get( other, result )
  print *, 'RESULT == VALUE = ', ( result == value )
end program example_hashmaps_get_other_scalar
