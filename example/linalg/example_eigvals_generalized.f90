! Eigenvalues of a general real/complex matrix for the generalized eigenproblem
program example_eigvals_generalized
  use stdlib_linalg, only: eigvals, eye
  implicit none

  real, allocatable :: A(:,:), B(:,:), lambda(:)
  complex, allocatable :: cA(:,:), cB(:,:), clambda(:)

  ! NB Fortran is column-major -> transpose input
  A = transpose(reshape([ [2, 8, 4], &
                          [1, 3, 5], &
                          [9, 5,-2] ], [3,3]))

  B = eye(3)

  ! Real generalized eigenproblem
  lambda = eigvals(A, B)
  print *, 'Real generalized matrix eigenvalues: ', lambda

  ! Complex generalized eigenproblem
  cA = cmplx(A, -2*A)
  cB = cmplx(B, 0.5*B)
  clambda = eigvals(cA, cB)
  print *, 'Complex generalized matrix eigenvalues: ', clambda

end program example_eigvals_generalized
