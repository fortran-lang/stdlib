program example_sort_adj
  use stdlib_sorting, only: sort_adj
  implicit none
  integer, allocatable :: array(:)
  real, allocatable :: adj(:)

  array = [5, 4, 3, 1, 10, 4, 9]
  allocate(adj, source=real(array))

  call sort_adj(array, adj)

  print *, array   !print [1, 3, 4, 4, 5, 9, 10]
  print *, adj   !print [1., 3., 4., 4., 5., 9., 10.]

end program example_sort_adj
