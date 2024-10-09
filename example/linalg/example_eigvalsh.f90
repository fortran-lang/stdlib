! Eigenvalues of a real symmetric / complex hermitian matrix 
program example_eigvalsh
  use stdlib_linalg, only: eigvalsh
  implicit none

  real, allocatable :: A(:,:),lambda(:)
  complex, allocatable :: cA(:,:)

  ! Decomposition of this symmetric matrix
  ! NB Fortran is column-major -> transpose input
  A = transpose(reshape( [ [2, 1, 4], &
                           [1, 3, 5], &
                           [4, 5, 4] ], [3,3] )) 

  ! Note: real symmetric matrices have real (orthogonal) eigenvalues and eigenvectors
  lambda = eigvalsh(A)
  print *, 'Symmetric matrix eigenvalues: ',lambda
  
  ! Complex hermitian matrices have real (orthogonal) eigenvalues and complex eigenvectors
  cA = A
  lambda = eigvalsh(cA)
  print *, 'Hermitian matrix eigenvalues: ',lambda
  
end program example_eigvalsh
