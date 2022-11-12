program example_diag2
  use stdlib_linalg, only: diag
  implicit none
  real, allocatable :: v(:)
  real, allocatable :: A(:, :)
  v = [1, 2, 3, 4, 5]
  A = diag(v) ! creates a 5 by 5 matrix with elements of v on the diagonal
end program example_diag2
