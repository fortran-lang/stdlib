program example_solve_chol
  use stdlib_linalg_constants, only: dp
  use stdlib_linalg, only: cholesky, solve_chol, linalg_state_type
  implicit none

  real(dp) :: A(3,3), L(3,3), b(3), x(3)
  type(linalg_state_type) :: state

  ! Symmetric positive definite matrix
  A(1,:) = [4.0_dp, 2.0_dp, 2.0_dp]
  A(2,:) = [2.0_dp, 5.0_dp, 1.0_dp]
  A(3,:) = [2.0_dp, 1.0_dp, 6.0_dp]

  ! Right-hand side
  b = [1.0_dp, 2.0_dp, 3.0_dp]

  ! Compute Cholesky factorization: A = L * L^T
  call cholesky(A, L, lower=.true., err=state)
  if (state%error()) error stop state%print()

  ! Solve using pre-computed Cholesky factors
  call solve_chol(L, b, x, lower=.true., err=state)
  if (state%error()) error stop state%print()

  print '("Solution: ",*(f8.4,1x))', x

end program example_solve_chol
