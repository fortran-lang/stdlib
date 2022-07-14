program example_trace
  use stdlib_linalg, only: trace
  implicit none
  real :: A(3, 3)
  A = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
  print *, trace(A) ! 1 + 5 + 9
end program example_trace
