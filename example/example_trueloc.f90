program example_trueloc
  use stdlib_array, only: trueloc
  implicit none
  real, allocatable :: array(:)
  allocate (array(500))
  call random_number(array)
  array(trueloc(array > 0.5)) = 0.0
end program example_trueloc
