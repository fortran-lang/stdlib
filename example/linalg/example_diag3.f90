program example_diag3
  use stdlib_linalg, only: diag
  implicit none
  integer, parameter :: n = 10
  real :: c(n), ul(n - 1)
  real :: A(n, n)
  c = 2
  ul = -1
  A = diag(ul, -1) + diag(c) + diag(ul, 1) ! Gil Strang's favorite matrix
end program example_diag3
