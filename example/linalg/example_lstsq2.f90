! Demonstrate expert subroutine interface with pre-allocated arrays
program example_lstsq2
  use stdlib_linalg_constants, only: dp,ilp
  use stdlib_linalg, only: solve_lstsq, lstsq_space, linalg_state_type
  implicit none

  integer, allocatable :: x(:),y(:)
  real(dp), allocatable :: A(:,:),b(:),coef(:),real_space(:),singvals(:)
  integer(ilp), allocatable :: int_space(:)
  integer(ilp) :: lrwork,liwork,arank

  ! Data set
  x = [1, 2, 2]
  y = [5, 13, 25]

  ! Fit three points using a parabola, least squares method
  ! A = [1 x x**2]
  A = reshape([[1,1,1],x,x**2],[3,3])
  b = y
  
  ! Get storage sizes for the arrays and pre-allocate data
  call lstsq_space(A,b,lrwork,liwork)
  allocate(coef(size(x)),real_space(lrwork),int_space(liwork),singvals(minval(shape(A))))
  
  ! Solve coefficients of y = coef(1) + x*coef(2) + x^2*coef(3)
  ! with no internal allocations
  call solve_lstsq(A,b,x=coef,              &
                   real_storage=real_space, &
                   int_storage=int_space,   &
                   singvals=singvals,       &
                   overwrite_a=.true.,      &
                   rank=arank)
  
  print *, 'parabola: ',coef
  ! parabola:  -0.42857142857141695        1.1428571428571503        4.2857142857142811 
  print *, 'rank: ',arank
  ! rank: 2


end program example_lstsq2
