program example_solve_bicgstab_wilkinson
    use stdlib_kinds, only: dp
    use stdlib_linalg_iterative_solvers
    use stdlib_sparse
    use stdlib_sparse_spmv
    implicit none

    integer, parameter :: n = 21
    type(COO_dp_type) :: COO
    type(CSR_dp_type) :: A
    type(stdlib_solver_workspace_dp_type) :: workspace
    real(dp) :: b(n), x(n), norm_sq0
    integer :: i

    ! Construct the Wilkinson's matrix in COO format
    ! https://en.wikipedia.org/wiki/Wilkinson_matrix
    call COO%malloc(n, n, n + 2*(n-1))
    COO%data(1:n) = [( real(abs(i), kind=dp), i=-(n-1)/2, (n-1)/2)]
    COO%data(n+1:) = 1.0_dp
    do i = 1, n
        COO%index(1:2, i) = [i,i]
    end do
    do i = 1, n-1
        COO%index(1:2, n+i) = [i,i+1]
        COO%index(1:2, n+n-1+i) = [i+1,i]
    end do
    call coo2ordered(COO,sort_data=.true.)

    ! Convert COO to CSR format
    call coo2csr(COO, A)
    
    ! Set up the right-hand side for the solution to be ones
    b = 0.0_dp
    x = 1.0_dp
    call spmv(A, x, b) ! b = A * 1
    x = 0.0_dp ! initial guess

    ! Solve the system using BiCGSTAB
    workspace%callback => my_logger
    call stdlib_solve_bicgstab(A, b, x, rtol=1e-12_dp, maxiter=100, workspace=workspace)

contains 

    subroutine my_logger(x,norm_sq,iter)
        real(dp), intent(in) :: x(:)
        real(dp), intent(in) :: norm_sq
        integer, intent(in) :: iter
        if(iter == 0) norm_sq0 = norm_sq
        print *, "Iteration: ", iter, " Residual: ", norm_sq, " Relative: ", norm_sq/norm_sq0
    end subroutine

end program example_solve_bicgstab_wilkinson