program example_is_triangular
  use stdlib_linalg, only: is_triangular
  implicit none
  real :: A(3, 3), B(3, 3)
  logical :: res
  A = reshape([1., 0., 0., 4., 5., 0., 7., 8., 9.], shape(A))
  B = reshape([1., 0., 3., 4., 5., 0., 7., 8., 9.], shape(B))
  res = is_triangular(A, 'u') ! returns .true.
  res = is_triangular(B, 'u') ! returns .false.
end program example_is_triangular
