program example_solve2
  use stdlib_linalg_constants, only: sp
  use stdlib_linalg, only: solve, linalg_state_type 
  implicit none

  complex(sp), allocatable :: A(:,:),b(:),x(:)

  ! Solve a system of 3 complex linear equations: 
  !  2x +      iy + 2z = (5-i)
  ! -ix + (4-3i)y + 6z = i
  !  4x +      3y +  z = 1

  ! Note: Fortran is column-major! -> transpose 
  A = transpose(reshape([(2.0, 0.0),(0.0, 1.0),(2.0,0.0), &
                         (0.0,-1.0),(4.0,-3.0),(6.0,0.0), &
                         (4.0, 0.0),(3.0, 0.0),(1.0,0.0)] , [3,3])) 
  b = [(5.0,-1.0),(0.0,1.0),(1.0,0.0)]

  ! Get coefficients of y = coef(1) + x*coef(2) + x^2*coef(3)
  x = solve(A,b) 

  print *, 'solution: ',x
  !   (1.0947,0.3674) (-1.519,-0.4539) (1.1784,-0.1078)

end program example_solve2

