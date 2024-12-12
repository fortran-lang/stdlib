! This example demonstrates the Schur decomposition for a complex-valued matrix.
program example_schur_complex
  use stdlib_linalg, only: schur
  use stdlib_linalg_constants, only: dp
  implicit none
  
  integer, parameter :: n = 3
  complex(dp), dimension(n,n) :: A, T, Z

  ! Initialize a complex-valued square matrix
  A = reshape([ (1, 2), (3,-1), (4, 1), &
                (0,-1), (2, 0), (1,-2), &
                (2, 3), (1, 1), (0,-1) ], shape=[n,n])

  ! Compute the Schur decomposition: A = Z T Z^H
  call schur(A, T, Z)

  ! Output results
  print *, "Original Matrix A:"
  print *, A
  print *, "Schur Form Matrix T:"
  print *, T
  print *, "Unitary Matrix Z:"
  print *, Z

  ! Test factorization: Z*T*Z^H = A
  print *, "Max error in reconstruction:", maxval(abs(matmul(Z, matmul(T, conjg(transpose(Z)))) - A))

end program example_schur_complex

