program example_falseloc
  use stdlib_array, only: falseloc
  implicit none
  real, allocatable :: array(:)
  allocate (array(-200:200))
  call random_number(array)
  array(falseloc(array < 0.5, lbound(array, 1))) = 0.0
end program example_falseloc
