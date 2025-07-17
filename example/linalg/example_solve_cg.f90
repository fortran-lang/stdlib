program example_solve_cg
    use stdlib_kinds, only: dp
    use stdlib_linalg_iterative_solvers, only: solve_cg

    real(dp) :: matrix(2,2)
    real(dp) :: x(2), load(2)

    matrix = reshape( [4, 1,&
                       1, 3] , [2,2])

    x    = dble( [2,1] ) !> initial guess
    load = dble( [1,2] ) !> load vector

    call solve_cg(matrix, load, x, restart=.false.)
    print *, x !> solution: [0.0909, 0.6364]

end program