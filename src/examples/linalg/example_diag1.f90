program example_diag1
  use stdlib_linalg, only: diag
  implicit none
  real, allocatable :: A(:, :)
  integer :: i
  A = diag([(1, i=1, 10)]) ! creates a 10 by 10 identity matrix
end program example_diag1
