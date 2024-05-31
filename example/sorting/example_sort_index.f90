program example_sort_index
  use stdlib_sorting, only: sort_index
  implicit none
  integer, allocatable :: array(:)
  integer, allocatable :: index(:)

  array = [5, 4, 3, 1, 10, 4, 9]
  allocate(index, mold=array)

  call sort_index(array, index)

  print *, array   !print [1, 3, 4, 4, 5, 9, 10]
  print *, index   !print [4, 3, 2, 6, 1, 7, 5]

end program example_sort_index
