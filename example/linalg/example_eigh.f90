! Eigendecomposition of a real symmetric matrix 
program example_eigh
  use stdlib_linalg, only: eigh
  implicit none

  integer :: i
  real, allocatable :: A(:,:),lambda(:),vectors(:,:)
  complex, allocatable :: cA(:,:),cvectors(:,:)

  ! Decomposition of this symmetric matrix
  ! NB Fortran is column-major -> transpose input
  A = transpose(reshape( [ [2, 1, 4], &
                           [1, 3, 5], &
                           [4, 5, 4] ], [3,3] )) 

  ! Note: real symmetric matrices have real (orthogonal) eigenvalues and eigenvectors
  allocate(lambda(3),vectors(3,3))  
  call eigh(A, lambda, vectors=vectors)
  
  print *, 'Real matrix'
  do i=1,3
     print *, 'eigenvalue  ',i,': ',lambda(i)
     print *, 'eigenvector ',i,': ',vectors(:,i)
  end do
  
  ! Complex hermitian matrices have real (orthogonal) eigenvalues and complex eigenvectors
  cA = A
  
  allocate(cvectors(3,3))  
  call eigh(cA, lambda, vectors=cvectors)
  
  print *, 'Complex matrix'
  do i=1,3
     print *, 'eigenvalue  ',i,': ',lambda(i)
     print *, 'eigenvector ',i,': ',cvectors(:,i)
  end do  
  
end program example_eigh
