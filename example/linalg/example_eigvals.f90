! Eigenvalues of a general real / complex matrix 
program example_eigvals
  use stdlib_linalg, only: eigvals
  implicit none

  real, allocatable :: A(:,:),lambda(:)
  complex, allocatable :: cA(:,:),clambda(:)

  ! NB Fortran is column-major -> transpose input
  A = transpose(reshape( [ [2, 8, 4], &
                           [1, 3, 5], &
                           [9, 5,-2] ], [3,3] )) 

  ! Note: real symmetric matrix
  lambda = eigvals(A)
  print *, 'Real    matrix eigenvalues: ',lambda
  
  ! Complex general matrix
  cA = cmplx(A, -2*A)
  clambda = eigvals(cA)
  print *, 'Complex matrix eigenvalues: ',clambda
  
end program example_eigvals
