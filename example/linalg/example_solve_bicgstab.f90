program example_solve_bicgstab
    use stdlib_kinds, only: dp
    use stdlib_linalg_iterative_solvers
    implicit none
    
    integer, parameter :: n = 4
    real(dp) :: A(n,n), b(n), x(n)
    integer :: i
    
    ! Example matrix (same as SciPy documentation)
    A = reshape([4.0_dp, 2.0_dp, 0.0_dp, 1.0_dp, &
                 3.0_dp, 0.0_dp, 0.0_dp, 2.0_dp, &
                 0.0_dp, 1.0_dp, 1.0_dp, 1.0_dp, &
                 0.0_dp, 2.0_dp, 1.0_dp, 0.0_dp], [n,n])
    
    b = [-1.0_dp, -0.5_dp, -1.0_dp, 2.0_dp]
    
    x = 0.0_dp  ! Initial guess
    
    print *, 'Solving Ax = b using BiCGSTAB method:'
    print *, 'Matrix A:'
    do i = 1, n
        print '(4F8.2)', A(i,:)
    end do
    print *, 'Right-hand side b:'
    print '(4F8.2)', b
    
    ! Solve using BiCGSTAB
    call stdlib_solve_bicgstab(A, b, x, rtol=1e-10_dp, atol=1e-12_dp)
    
    print *, 'Solution x:'
    print '(4F10.6)', x
    
    ! Verify solution
    print *, 'Verification A*x:'
    print '(4F10.6)', matmul(A, x)
    
    print *, 'Residual ||b - A*x||:'
    print *, norm2(b - matmul(A, x))
    
end program 
