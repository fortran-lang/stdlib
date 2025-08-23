! Eigendecomposition of a real square matrix for the generalized eigenproblem
program example_eig_generalized
  use stdlib_linalg, only: eig, eye
  implicit none

  integer :: i
  real, allocatable :: A(:,:), B(:,:)
  complex, allocatable :: lambda(:), vectors(:,:)

  ! Matrices for the generalized eigenproblem: A * v = lambda * B * v
  ! NB Fortran is column-major -> transpose input
  A = transpose(reshape([ [2, 2, 4], &
                          [1, 3, 5], &
                          [2, 3, 4] ], [3,3]))

  B = eye(3)

  ! Allocate eigenvalues and right eigenvectors
  allocate(lambda(3), vectors(3,3))

  ! Get eigenvalues and right eigenvectors for the generalized problem
  call eig(A, B, lambda, right=vectors)

  do i = 1, 3
     print *, 'Eigenvalue  ', i, ': ', lambda(i)
     print *, 'Eigenvector ', i, ': ', vectors(:,i)
  end do

end program example_eig_generalized

