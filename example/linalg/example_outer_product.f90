program example_outer_product
  use stdlib_linalg, only: outer_product
  implicit none
  real, allocatable :: A(:, :), u(:), v(:)
  u = [1., 2., 3.]
  v = [3., 4.]
  A = outer_product(u, v)
!A = reshape([3., 6., 9., 4., 8., 12.], [3,2])
end program example_outer_product
