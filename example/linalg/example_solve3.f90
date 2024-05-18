program example_solve3
  use stdlib_linalg_constants, only: sp,ilp
  use stdlib_linalg, only: solve_lu, linalg_state_type 
  implicit none

  integer(ilp) :: test
  integer(ilp), allocatable :: pivot(:)
  complex(sp), allocatable :: A(:,:),b(:),x(:)

  ! Solve a system of 3 complex linear equations: 
  !  2x +      iy + 2z = (5-i)
  ! -ix + (4-3i)y + 6z = i
  !  4x +      3y +  z = 1

  ! Note: Fortran is column-major! -> transpose 
  A = transpose(reshape([(2.0, 0.0),(0.0, 1.0),(2.0,0.0), &
                         (0.0,-1.0),(4.0,-3.0),(6.0,0.0), &
                         (4.0, 0.0),(3.0, 0.0),(1.0,0.0)] , [3,3])) 
  
  ! Pre-allocate x
  allocate(b(size(A,2)),pivot(size(A,2)))
  allocate(x,mold=b)

  ! Call system many times avoiding reallocation 
  do test=1,100
     b = test*[(5.0,-1.0),(0.0,1.0),(1.0,0.0)]
     call solve_lu(A,b,x,pivot)
     print "(i3,'-th solution: ',*(1x,f12.6))", test,x
  end do

end program example_solve3

