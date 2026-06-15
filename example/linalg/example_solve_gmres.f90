program example_solve_gmres
    use stdlib_kinds, only: dp
    use stdlib_linalg_iterative_solvers, only: stdlib_solve_gmres, pc_jacobi
    implicit none

    real(dp), parameter :: A(4,4) = reshape([5.0_dp,  1.0_dp,  2.0_dp, 0.0_dp, &
                                             0.0_dp,  4.0_dp, -1.0_dp, 1.0_dp, &
                                             1.0_dp,  0.0_dp,  3.0_dp, 2.0_dp, &
                                             2.0_dp, -1.0_dp,  0.0_dp, 6.0_dp], [4,4])
    real(dp), parameter :: x0(4) = [0.2_dp, -0.1_dp, 0.3_dp, 0.0_dp]
    real(dp), parameter :: xref(4) = [1.0_dp, 2.0_dp, -1.0_dp, 0.5_dp]
    real(dp) :: rhs(4), x(4)

    rhs = matmul(A, xref)

    ! GMRES lets the user tune the restart size and the storage mode.
    x = x0
    call stdlib_solve_gmres(A, rhs, x, restart=.false., kdim=2, maxiter=12, precond=pc_jacobi)
    print *, 'compact:', x

    x = x0
    call stdlib_solve_gmres(A, rhs, x, restart=.false., kdim=2, maxiter=12, precond=pc_jacobi, compact=.false.)
    print *, 'cached :', x
end program example_solve_gmres