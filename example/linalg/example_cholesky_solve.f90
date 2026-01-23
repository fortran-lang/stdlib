program example_cholesky_solve
  use stdlib_linalg_constants, only: dp
  use stdlib_linalg, only: cholesky_solve, linalg_state_type
  implicit none

  real(dp) :: A(3,3), b(3), x(3)
  type(linalg_state_type) :: state

  ! Symmetric positive definite matrix
  A(1,:) = [4.0_dp, 2.0_dp, 2.0_dp]
  A(2,:) = [2.0_dp, 5.0_dp, 1.0_dp]
  A(3,:) = [2.0_dp, 1.0_dp, 6.0_dp]

  ! Right-hand side
  b = [1.0_dp, 2.0_dp, 3.0_dp]

  ! One-shot factorization and solve (A is preserved by default)
  call cholesky_solve(A, b, x, lower=.true., err=state)
  if (state%error()) error stop state%print()

  print '("Solution: ",*(f8.4,1x))', x

  ! For performance-critical code, use overwrite_a=.true.
  ! to avoid internal allocation (but A will be destroyed)
  ! call cholesky_solve(A, b, x, lower=.true., overwrite_a=.true., err=state)

end program example_cholesky_solve
