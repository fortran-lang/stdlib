! This example computes the Schur decomposition of a real-valued square matrix.
program example_schur_real
  use stdlib_linalg, only: schur
  use stdlib_linalg_constants, only: dp
  implicit none
  integer, parameter :: n = 3
  real(dp), dimension(n,n) :: A, T, Z

  ! Initialize a real-valued square matrix
  A = reshape([ 0, 2, 2, &
                0, 1, 2, &
                1, 0, 1], shape=[n,n])

  ! Compute the Schur decomposition: A = Z T Z^T
  call schur(A, T, Z)

  ! Output results
  print *, "Original Matrix A:"
  print *, A
  print *, "Schur Form Matrix T:"
  print *, T
  print *, "Orthogonal Matrix Z:"
  print *, Z

  ! Test factorization: Z*T*Z^T = A
  print *, "Max error in reconstruction:", maxval(abs(matmul(Z, matmul(T, transpose(Z))) - A))

end program example_schur_real

