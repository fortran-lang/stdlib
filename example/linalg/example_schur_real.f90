! This example computes the Schur decomposition of a real-valued square matrix.
program example_schur_real
  use stdlib_linalg, only: schur
  use stdlib_linalg_constants, only: dp
  implicit none
  real(dp), allocatable :: A(:,:), T(:,:), Z(:,:)
  integer :: n

  ! Initialize a real-valued square matrix
  n = 3
  allocate(A(n,n), T(n,n), Z(n,n))
  A = reshape([ &
       0.0_dp, 2.0_dp, 2.0_dp, &
       0.0_dp, 1.0_dp, 2.0_dp, &
       1.0_dp, 0.0_dp, 1.0_dp], shape=[n,n])

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

  deallocate(A, T, Z)
end program example_schur_real

