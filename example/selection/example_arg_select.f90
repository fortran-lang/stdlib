program example_arg_select
  use stdlib_selection, only: arg_select
  implicit none

  real, allocatable :: array(:)
  integer, allocatable :: indx(:)
  integer :: kth_smallest
  integer :: k

  array = [3., 2., 7., 4., 5., 1., 4., -1.]
  indx = [(k, k=1, size(array))]

  k = 2
  call arg_select(array, indx, k, kth_smallest)
  print *, array(kth_smallest) ! print 1.0

  k = 7
! Due to the previous call to arg_select, we know for sure this is in an
! index >= 2
  call arg_select(array, indx, k, kth_smallest, left=2)
  print *, array(kth_smallest) ! print 5.0

  k = 6
! Due to the previous two calls to arg_select, we know for sure this is in
! an index >= 2 and <= 7
  call arg_select(array, indx, k, kth_smallest, left=2, right=7)
  print *, array(kth_smallest) ! print 4.0

end program example_arg_select
