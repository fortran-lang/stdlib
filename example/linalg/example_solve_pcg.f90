program example_solve_pcg
    use stdlib_kinds, only: int8, dp
    use stdlib_sparse
    use stdlib_linalg_iterative_solvers, only: stdlib_solve_pcg

    type(CSR_dp_type) :: laplacian_csr
    type(COO_dp_type) :: COO
    real(dp) :: laplacian(5,5)
    real(dp) :: x(5), rhs(5)
    logical(int8) :: dirichlet(5)

    laplacian = reshape( [1, -1,  0,  0,  0,&
                         -1,  2, -1,  0,  0,&
                          0, -1,  2, -1,  0,&
                          0,  0, -1,  2, -1,&
                          0,  0,  0, -1,  1] , [5,5])
    call dense2coo(laplacian,COO)
    call coo2csr(COO,laplacian_csr)

    x = 0._dp
    rhs = real( [0,0,5,0,0], kind=dp )

    dirichlet = .false._int8
    dirichlet([1,5]) = .true._int8

    call stdlib_solve_pcg(laplacian, rhs, x, rtol=1.d-6, di=dirichlet)
    print *, x !> solution: [0.0, 2.5, 5.0, 2.5, 0.0]
    x = 0._dp

    call stdlib_solve_pcg(laplacian_csr, rhs, x, rtol=1.d-6, di=dirichlet)
    print *, x !> solution: [0.0, 2.5, 5.0, 2.5, 0.0]
end program