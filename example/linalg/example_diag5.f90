program example_diag5
  use stdlib_linalg, only: diag
  implicit none
  integer, parameter :: n = 3
  real :: A(n, n)
  real, allocatable :: v(:)
  A = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [n, n])
  v = diag(A, -1) ! v is [2,6]
  v = diag(A, 1)  ! v is [4,8]
end program example_diag5
