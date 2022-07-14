program example_is_skew_symmetric
  use stdlib_linalg, only: is_skew_symmetric
  implicit none
  real :: A(2, 2), B(2, 2)
  logical :: res
  A = reshape([0., -3., 3., 0.], shape(A))
  B = reshape([0., 3., 3., 0.], shape(B))
  res = is_skew_symmetric(A) ! returns .true.
  res = is_skew_symmetric(B) ! returns .false.
end program example_is_skew_symmetric
