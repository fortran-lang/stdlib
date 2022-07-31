program example_sort
  use stdlib_sorting, only: sort
  implicit none
  integer, allocatable :: array(:)

  array = [5, 4, 3, 1, 10, 4, 9]
  call sort(array)
  print *, array   !print [1, 3, 4, 4, 5, 9, 10]
end program example_sort
