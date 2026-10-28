program example_solve_lower_chol
  use stdlib_linalg_constants, only: dp
  use stdlib_linalg, only: cholesky, solve_lower_chol, linalg_state_type
  implicit none

  real(dp) :: A(3,3), L(3,3), b1(3), b2(3), x(3)
  type(linalg_state_type) :: state

  ! Symmetric positive definite matrix
  A(1,:) = [4.0_dp, 2.0_dp, 2.0_dp]
  A(2,:) = [2.0_dp, 5.0_dp, 1.0_dp]
  A(3,:) = [2.0_dp, 1.0_dp, 6.0_dp]

  ! Compute lower Cholesky factorization once: A = L * L^T
  call cholesky(A, L, lower=.true., err=state)
  if (state%error()) error stop state%print()

  ! First right-hand side
  b1 = [1.0_dp, 2.0_dp, 3.0_dp]
  call solve_lower_chol(L, b1, x, err=state)
  if (state%error()) error stop state%print()
  print '("Solution 1: ",*(f8.4,1x))', x

  ! Second right-hand side (reusing the same factorization)
  b2 = [4.0_dp, 5.0_dp, 6.0_dp]
  call solve_lower_chol(L, b2, x, err=state)
  if (state%error()) error stop state%print()
  print '("Solution 2: ",*(f8.4,1x))', x

end program example_solve_lower_chol
