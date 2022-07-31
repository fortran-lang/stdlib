program example_is_symmetric
  use stdlib_linalg, only: is_symmetric
  implicit none
  real :: A(2, 2), B(2, 2)
  logical :: res
  A = reshape([1., 3., 3., 4.], shape(A))
  B = reshape([1., 0., 3., 4.], shape(B))
  res = is_symmetric(A) ! returns .true.
  res = is_symmetric(B) ! returns .false.
end program example_is_symmetric
