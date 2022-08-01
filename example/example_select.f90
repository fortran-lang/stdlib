program example_select
  use stdlib_selection, only: select
  implicit none

  real, allocatable :: array(:)
  real :: kth_smallest
  integer :: k

  array = [3., 2., 7., 4., 5., 1., 4., -1.]

  k = 2
  call select(array, k, kth_smallest)
  print *, kth_smallest ! print 1.0

  k = 7
! Due to the previous call to select, we know for sure this is in an
! index >= 2
  call select(array, k, kth_smallest, left=2)
  print *, kth_smallest ! print 5.0

  k = 6
! Due to the previous two calls to select, we know for sure this is in
! an index >= 2 and <= 7
  call select(array, k, kth_smallest, left=2, right=7)
  print *, kth_smallest ! print 4.0

end program example_select
