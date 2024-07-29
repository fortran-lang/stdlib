program example_solve1
  use stdlib_linalg_constants, only: sp
  use stdlib_linalg, only: solve, linalg_state_type 
  implicit none

  real(sp), allocatable :: A(:,:),b(:),x(:)

  ! Solve a system of 3 linear equations: 
  !  4x + 3y + 2z =  25
  ! -2x + 2y + 3z = -10
  !  3x - 5y + 2z =  -4

  ! Note: Fortran is column-major! -> transpose 
  A = transpose(reshape([ 4, 3, 2, &
                         -2, 2, 3, &
                          3,-5, 2], [3,3])) 
  b = [25,-10,-4]

  ! Get coefficients of y = coef(1) + x*coef(2) + x^2*coef(3)
  x = solve(A,b) 

  print *, 'solution: ',x
  ! 5.0, 3.0, -2.0

end program example_solve1

