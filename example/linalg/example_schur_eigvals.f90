! This example includes eigenvalue computation in addition to 
! the Schur decomposition for a randomly generated matrix.
program example_schur_eigenvalues
  use stdlib_linalg, only: schur
  use stdlib_linalg_constants, only: dp
  implicit none
  
  integer, parameter :: n = 5  
  real(dp), dimension(n,n) :: A, T, Z
  complex(dp), dimension(n) :: eigenvalues
  
  ! Create a random real-valued square matrix
  call random_number(A)

  ! Compute the Schur decomposition and eigenvalues
  call schur(A, T, Z, eigenvalues)

  ! Output results
  print *, "Random Matrix A:"
  print *, A
  print *, "Schur Form Matrix T:"
  print *, T
  print *, "Orthogonal Matrix Z:"
  print *, Z
  print *, "Eigenvalues:"
  print *, eigenvalues

  ! Test factorization: Z*T*Z^T = A
  print *, "Max error in reconstruction:", maxval(abs(matmul(Z, matmul(T, transpose(Z))) - A))

end program example_schur_eigenvalues

