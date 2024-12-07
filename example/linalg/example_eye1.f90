program example_eye1
  use stdlib_linalg, only: eye
  implicit none
  integer :: i(2, 2)
  real :: a(3, 3)
  real :: b(2, 3)  !! Matrix is non-square.
  complex :: c(2, 2)
  I = eye(2)                !! [1,0; 0,1]
  A = eye(3)                !! [1.0,0.0,0.0; 0.0,1.0,0.0; 0.0,0.0,1.0]
  A = eye(3, 3)             !! [1.0,0.0,0.0; 0.0,1.0,0.0; 0.0,0.0,1.0]
  B = eye(2, 3)             !! [1.0,0.0,0.0; 0.0,1.0,0.0]
  C = eye(2, 2)             !! [(1.0,0.0),(0.0,0.0); (0.0,0.0),(1.0,0.0)]
  C = (1.0, 1.0)*eye(2, 2)  !! [(1.0,1.0),(0.0,0.0); (0.0,0.0),(1.0,1.0)]
end program example_eye1
