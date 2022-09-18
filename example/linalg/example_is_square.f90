program example_is_square
  use stdlib_linalg, only: is_square
  implicit none
  real :: A(2, 2), B(3, 2)
  logical :: res
  A = reshape([1., 2., 3., 4.], shape(A))
  B = reshape([1., 2., 3., 4., 5., 6.], shape(B))
  res = is_square(A) ! returns .true.
  res = is_square(B) ! returns .false.
end program example_is_square
