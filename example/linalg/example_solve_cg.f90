program example_solve_cg
    use stdlib_kinds, only: int8, dp
    use stdlib_linalg_iterative_solvers, only: stdlib_solve_cg

    real(dp) :: matrix(2,2)
    real(dp) :: x(2), rhs(2)

    matrix = reshape( [4, 1,&
                       1, 3] , [2,2])

    x   = dble( [2,1] ) !> initial guess
    rhs = dble( [1,2] ) !> rhs vector

    call stdlib_solve_cg(matrix, rhs, x, restart=.false.)
    print *, x !> solution: [0.0909, 0.6364]

end program