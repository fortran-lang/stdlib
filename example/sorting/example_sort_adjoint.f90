program example_sort_adjoint
  use stdlib_sorting, only: sort_adjoint
  implicit none
  integer, allocatable :: array(:)
  real, allocatable :: adjoint(:)

  array = [5, 4, 3, 1, 10, 4, 9]
  allocate(adjoint, source=real(array))

  call sort_adjoint(array, adjoint)

  print *, array   !print [1, 3, 4, 4, 5, 9, 10]
  print *, adjoint   !print [1., 3., 4., 4., 5., 9., 10.]

end program example_sort_adjoint
