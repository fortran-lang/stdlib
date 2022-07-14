program example_ord_sort
  use stdlib_sorting, only: ord_sort
  implicit none
  integer, allocatable :: array1(:), work(:)

  array1 = [5, 4, 3, 1, 10, 4, 9]
  allocate (work, mold=array1)
  call ord_sort(array1, work)
  print *, array1   !print [1, 3, 4, 4, 5, 9, 10]
end program example_ord_sort
