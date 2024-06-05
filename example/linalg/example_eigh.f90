! Eigendecomposition of a real symmetric matrix 
program example_eigh
  use stdlib_linalg, only: eigh
  implicit none

  integer :: i
  real, allocatable :: A(:,:),lambda(:),v(:,:)
  complex, allocatable :: cA(:,:),cv(:,:)

  ! Decomposition of this symmetric matrix
  ! NB Fortran is column-major -> transpose input
  A = transpose(reshape( [ [2, 1, 4], &
                           [1, 3, 5], &
                           [4, 5, 4] ], [3,3] )) 

  ! Note: real symmetric matrices have real (orthogonal) eigenvalues and eigenvectors
  allocate(lambda(3),v(3,3))  
  call eigh(A, lambda, vectors=v)
  
  print *, 'Real matrix'
  do i=1,3
     print *, 'eigenvalue  ',i,': ',lambda(i)
     print *, 'eigenvector ',i,': ',v(:,i)
  end do
  
  ! Complex hermitian matrices have real (orthogonal) eigenvalues and complex eigenvectors
  cA = A
  
  allocate(cv(3,3))  
  call eigh(cA, lambda, vectors=cv)
  
  print *, 'Complex matrix'
  do i=1,3
     print *, 'eigenvalue  ',i,': ',lambda(i)
     print *, 'eigenvector ',i,': ',cv(:,i)
  end do  
  
end program example_eigh
