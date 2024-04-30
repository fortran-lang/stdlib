! Least-squares solver: functional interface
program example_lstsq1
  use stdlib_linalg_constants, only: dp
  use stdlib_linalg, only: lstsq
  implicit none

  integer, allocatable :: x(:),y(:)
  real(dp), allocatable :: A(:,:),b(:),coef(:)

  ! Data set
  x = [1, 2, 2]
  y = [5, 13, 25]

  ! Fit three points using a parabola, least squares method
  ! A = [1 x x**2]
  A = reshape([[1,1,1],x,x**2],[3,3])
  b = y

  ! Get coefficients of y = coef(1) + x*coef(2) + x^2*coef(3)
  coef = lstsq(A,b)

  print *, 'parabola: ',coef
  ! parabola:  -0.42857142857141695        1.1428571428571503        4.2857142857142811 


end program example_lstsq1
