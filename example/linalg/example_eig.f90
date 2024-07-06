! Eigendecomposition of a real square matrix 
program example_eig
  use stdlib_linalg, only: eig
  implicit none

  integer :: i
  real, allocatable :: A(:,:)
  complex, allocatable :: lambda(:),vectors(:,:)

  ! Decomposition of this square matrix
  ! NB Fortran is column-major -> transpose input
  A = transpose(reshape( [ [2, 2, 4], &
                           [1, 3, 5], &
                           [2, 3, 4] ], [3,3] )) 

  ! Get eigenvalues and right eigenvectors
  allocate(lambda(3),vectors(3,3))
  
  call eig(A, lambda, right=vectors)
  
  do i=1,3
     print *, 'eigenvalue  ',i,': ',lambda(i)
     print *, 'eigenvector ',i,': ',vectors(:,i)
  end do
  
end program example_eig
