! This example demonstrates the Schur decomposition for a complex-valued matrix.
program example_schur_complex
  use stdlib_linalg, only: schur
  use stdlib_linalg_constants, only: dp
  implicit none
  complex(dp), allocatable :: A(:,:), T(:,:), Z(:,:)
  integer :: n

  ! Initialize a complex-valued square matrix
  n = 3
  allocate(A(n,n), T(n,n), Z(n,n))
  A = reshape([ &
       (1.0_dp, 2.0_dp), (3.0_dp, -1.0_dp), (4.0_dp, 1.0_dp), &
       (0.0_dp, -1.0_dp), (2.0_dp, 0.0_dp), (1.0_dp, -2.0_dp), &
       (2.0_dp, 3.0_dp), (1.0_dp, 1.0_dp), (0.0_dp, -1.0_dp) ], shape=[n,n])

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

  deallocate(A, T, Z)
end program example_schur_complex

