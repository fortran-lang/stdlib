program example_is_hessenberg
  use stdlib_linalg, only: is_hessenberg
  implicit none
  real :: A(3, 3), B(3, 3)
  logical :: res
  A = reshape([1., 2., 0., 4., 5., 6., 7., 8., 9.], shape(A))
  B = reshape([1., 2., 3., 4., 5., 6., 7., 8., 9.], shape(B))
  res = is_hessenberg(A, 'u') ! returns .true.
  res = is_hessenberg(B, 'u') ! returns .false.
end program example_is_hessenberg
