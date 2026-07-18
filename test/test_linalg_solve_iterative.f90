! Test linear system iterative solvers
module test_linalg_solve_iterative
    use stdlib_kinds
    use stdlib_sparse
    use stdlib_linalg_iterative_solvers
    use testdrive, only: error_type, check, new_unittest, unittest_type    

    implicit none 
    private
    
    public :: test_linear_systems

    contains

    subroutine test_linear_systems(tests)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: tests(:)

        allocate(tests(0))

        tests = [ new_unittest("stdlib_solve_cg",test_stdlib_solve_cg), &
                  new_unittest("stdlib_solve_pcg",test_stdlib_solve_pcg), &
                  new_unittest("stdlib_solve_gmres",test_stdlib_solve_gmres), &
                  new_unittest("stdlib_solve_gmres_hilbert",test_stdlib_solve_gmres_hilbert), &
                  new_unittest("stdlib_solve_bicgstab",test_stdlib_solve_bicgstab), &
                  new_unittest("stdlib_solve_bicgstab_nonsymmetric",test_stdlib_solve_bicgstab_nonsymmetric) ]

    end subroutine test_linear_systems    

    subroutine test_stdlib_solve_cg(error)
        type(error_type), allocatable, intent(out) :: error

        block
        real(sp), parameter :: tol = 1000*epsilon(0.0_sp)
        real(sp) :: A(2,2) = reshape([real(sp) ::  4,  1, &
                                             1,  3], [2,2])
        real(sp) :: x(2), load(2), xref(2)
        
        xref = [0.0909, 0.6364]
        x    = real( [2,1] , kind = sp ) ! initial guess
        load = real( [1,2] , kind = sp ) ! load vector

        call stdlib_solve_cg(A, load, x)
        
        call check(error, norm2(x-xref)<1.e-4_sp, 'error in conjugate gradient solver')
        if (allocated(error)) return
        end block

        block
        real(dp), parameter :: tol = 1000*epsilon(0.0_dp)
        real(dp) :: A(2,2) = reshape([real(dp) ::  4,  1, &
                                             1,  3], [2,2])
        real(dp) :: x(2), load(2), xref(2)
        
        xref = [0.0909, 0.6364]
        x    = real( [2,1] , kind = dp ) ! initial guess
        load = real( [1,2] , kind = dp ) ! load vector

        call stdlib_solve_cg(A, load, x)
        
        call check(error, norm2(x-xref)<1.e-4_dp, 'error in conjugate gradient solver')
        if (allocated(error)) return
        end block

    end subroutine test_stdlib_solve_cg

    subroutine test_stdlib_solve_pcg(error)
        type(error_type), allocatable, intent(out) :: error

        block
        real(sp), parameter :: tol = 1000*epsilon(0.0_sp)
        real(sp) :: A(5,5) = reshape([real(sp) :: 1, -1,  0,  0,  0,&
                                           -1,  2, -1,  0,  0,&
                                            0, -1,  2, -1,  0,&
                                            0,  0, -1,  2, -1,&
                                            0,  0,  0, -1,  1] , [5,5])
        real(sp) :: x(5), load(5), xref(5)
        logical(int8) :: dirichlet(5)
        
        xref = [0.0_sp,2.5_sp,5.0_sp,2.5_sp,0.0_sp]
        x    = 0.0_sp 
        load = real( [0,0,5,0,0] , kind = sp ) ! load vector
        dirichlet = .false._int8
        dirichlet([1,5]) = .true._int8

        call stdlib_solve_pcg(A, load, x, di=dirichlet, rtol=1.e-6_sp)

        call check(error, norm2(x-xref)<1.e-6_sp*norm2(xref), 'error in preconditionned conjugate gradient solver')
        if (allocated(error)) return
        end block

        block
        real(dp), parameter :: tol = 1000*epsilon(0.0_dp)
        real(dp) :: A(5,5) = reshape([real(dp) :: 1, -1,  0,  0,  0,&
                                           -1,  2, -1,  0,  0,&
                                            0, -1,  2, -1,  0,&
                                            0,  0, -1,  2, -1,&
                                            0,  0,  0, -1,  1] , [5,5])
        real(dp) :: x(5), load(5), xref(5)
        logical(int8) :: dirichlet(5)
        
        xref = [0.0_dp,2.5_dp,5.0_dp,2.5_dp,0.0_dp]
        x    = 0.0_dp 
        load = real( [0,0,5,0,0] , kind = dp ) ! load vector
        dirichlet = .false._int8
        dirichlet([1,5]) = .true._int8

        call stdlib_solve_pcg(A, load, x, di=dirichlet, rtol=1.e-6_dp)

        call check(error, norm2(x-xref)<1.e-6_dp*norm2(xref), 'error in preconditionned conjugate gradient solver')
        if (allocated(error)) return
        end block

    end subroutine test_stdlib_solve_pcg

    subroutine test_stdlib_solve_gmres(error)
        type(error_type), allocatable, intent(out) :: error

        block
        integer, parameter :: n = 4
        real(sp), parameter :: tol = 1000*epsilon(0.0_sp)
        real(sp), parameter :: A(n,n) = reshape([real(sp) :: 5,  1,  2,  0, &
                                           0,  4, -1,  1, &
                                           1,  0,  3,  2, &
                                           2, -1,  0,  6], [n,n])
        real(sp) :: x(n), load(n)
        real(sp), parameter :: xref(*) = [1.0_sp, 2.0_sp, -1.0_sp, 0.5_sp]
        load = matmul(A, xref)
        x = 0.0_sp

        call stdlib_solve_gmres(A, load, x, rtol=tol, maxiter=12)
        
        call check(error, norm2(x-xref)<tol*norm2(xref), 'error in GMRES solver (restarted dense test)')
        if (allocated(error)) return
        end block

        block
        integer, parameter :: n = 3
        real(sp), parameter :: tol = 1000*epsilon(0.0_sp)
        real(sp), parameter :: A(n,n) = reshape([real(sp) :: 10,  1,  2, &
                                            1, 10,  3, &
                                            2,  3, 10], [n,n])
        real(sp) :: x(n)
        real(sp), parameter :: xref(n) = [(137._sp/218._sp), -(9._sp/109._sp), (87._sp/218._sp)]
        real(sp), parameter :: load(n) = [7.0_sp, 1.0_sp, 5.0_sp]
        x = [0.2_sp, -0.1_sp, 0.3_sp]

        call stdlib_solve_gmres(A, load, x, rtol=1.e-10_sp, precond=pc_jacobi)

        call check(error, norm2(x-xref)<tol*norm2(xref), 'error in GMRES solver (Jacobi preconditioned test)')
        if (allocated(error)) return
        end block

        block
        integer, parameter :: n = 3
        real(sp), parameter :: tol = 1000*epsilon(0.0_sp)
        real(sp), parameter :: A(n,n) = reshape([real(sp) :: 10,  1,  2, &
                                            1, 10,  3, &
                                            2,  3, 10], [n,n])
        real(sp) :: x(n)
        real(sp), parameter :: xref(n) = [(137._sp/218._sp), -(9._sp/109._sp), (87._sp/218._sp)]
        real(sp), parameter :: load(n) = [7.0_sp, 1.0_sp, 5.0_sp]
        x = [0.2_sp, -0.1_sp, 0.3_sp]

        call stdlib_solve_gmres(A, load, x, rtol=1.e-10_sp, precond=pc_jacobi, compact=.false.)

        call check(error, norm2(x-xref)<tol*norm2(xref), 'error in GMRES solver (Jacobi preconditioned speed mode test)')
        if (allocated(error)) return
        end block

        block
        integer, parameter :: n = 4
        real(dp), parameter :: tol = 1000*epsilon(0.0_dp)
        real(dp), parameter :: A(n,n) = reshape([real(dp) :: 5,  1,  2,  0, &
                                           0,  4, -1,  1, &
                                           1,  0,  3,  2, &
                                           2, -1,  0,  6], [n,n])
        real(dp) :: x(n), load(n)
        real(dp), parameter :: xref(*) = [1.0_dp, 2.0_dp, -1.0_dp, 0.5_dp]
        load = matmul(A, xref)
        x = 0.0_dp

        call stdlib_solve_gmres(A, load, x, rtol=tol, maxiter=12)
        
        call check(error, norm2(x-xref)<tol*norm2(xref), 'error in GMRES solver (restarted dense test)')
        if (allocated(error)) return
        end block

        block
        integer, parameter :: n = 3
        real(dp), parameter :: tol = 1000*epsilon(0.0_dp)
        real(dp), parameter :: A(n,n) = reshape([real(dp) :: 10,  1,  2, &
                                            1, 10,  3, &
                                            2,  3, 10], [n,n])
        real(dp) :: x(n)
        real(dp), parameter :: xref(n) = [(137._dp/218._dp), -(9._dp/109._dp), (87._dp/218._dp)]
        real(dp), parameter :: load(n) = [7.0_dp, 1.0_dp, 5.0_dp]
        x = [0.2_dp, -0.1_dp, 0.3_dp]

        call stdlib_solve_gmres(A, load, x, rtol=1.e-10_dp, precond=pc_jacobi)

        call check(error, norm2(x-xref)<tol*norm2(xref), 'error in GMRES solver (Jacobi preconditioned test)')
        if (allocated(error)) return
        end block

        block
        integer, parameter :: n = 3
        real(dp), parameter :: tol = 1000*epsilon(0.0_dp)
        real(dp), parameter :: A(n,n) = reshape([real(dp) :: 10,  1,  2, &
                                            1, 10,  3, &
                                            2,  3, 10], [n,n])
        real(dp) :: x(n)
        real(dp), parameter :: xref(n) = [(137._dp/218._dp), -(9._dp/109._dp), (87._dp/218._dp)]
        real(dp), parameter :: load(n) = [7.0_dp, 1.0_dp, 5.0_dp]
        x = [0.2_dp, -0.1_dp, 0.3_dp]

        call stdlib_solve_gmres(A, load, x, rtol=1.e-10_dp, precond=pc_jacobi, compact=.false.)

        call check(error, norm2(x-xref)<tol*norm2(xref), 'error in GMRES solver (Jacobi preconditioned speed mode test)')
        if (allocated(error)) return
        end block

    end subroutine test_stdlib_solve_gmres

    subroutine test_stdlib_solve_gmres_hilbert(error)
        ! This test uses a Hilbert matrix (n=10) to validate the numerical stability 
        ! of the GMRES solver. The Hilbert matrix is notoriously ill-conditioned 
        ! (cond(A) ~ 10^13 for n=10), making it an excellent benchmark for 
        ! orthogonality maintenance. By setting rtol=0 and maxiter=n, we force 
        ! the construction of the full Krylov basis, which only converges to 
        ! the reference solution if the MGS with reorthogonalization 
        ! preserves the basis' orthogonality.
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 10
        integer :: r, c
        block
            real(dp), parameter :: tol = 1.e-3_dp
            
            real(dp) :: A(n, n), b(n), x(n)
            real(dp), parameter :: x_ref(n) = 1.0_dp
            do r = 1, n
                do c = 1, n
                    A(r, c) = 1.0_dp / real(r + c - 1, dp)
                end do
            end do

            b = matmul(A, x_ref)
            x = 0.0_dp

            call stdlib_solve_gmres(A, b, x, rtol=0.0_dp, maxiter=n)
            call check(error, norm2(x-x_ref) < tol*norm2(x_ref), & 
            'error in GMRES dp: Hilbert matrix')
            if (allocated(error)) return
        end block
    end subroutine test_stdlib_solve_gmres_hilbert

    subroutine test_stdlib_solve_bicgstab(error)
        type(error_type), allocatable, intent(out) :: error

        ! Test 1: Simple non-symmetric matrix (same as SciPy example)
        block
        real(sp), parameter :: tol = 1000*epsilon(0.0_sp)
        real(sp) :: A(4,4) = reshape([real(sp) :: 4, 2, 0, 1, &
                                            3, 0, 0, 2, &
                                            0, 1, 1, 1, &
                                            0, 2, 1, 0], [4,4])
        real(sp) :: x(4), load(4), xref(4)
        
        ! Reference solution computed with high precision
        xref = [12.5_sp, -17._sp, 23.5_sp, -24.5_sp]
        x    = 0.0_sp ! initial guess
        load = [-1.0_sp, -0.5_sp, -1.0_sp, 2.0_sp] ! load vector

        call stdlib_solve_bicgstab(A, load, x, rtol=1.e-10_sp)
        
        call check(error, norm2(x-xref)<tol*norm2(xref), 'error in BiCGSTAB solver (basic test)')
        if (allocated(error)) return
        end block

        ! Test 2: BiCGSTAB with Dirichlet boundary conditions
        block
        real(sp), parameter :: tol = 1000*epsilon(0.0_sp)
        real(sp) :: A(5,5) = reshape([real(sp) :: 1, -1,  0,  0,  0,&
                                          -1,  2, -1,  0,  0,&
                                           0, -1,  2, -1,  0,&
                                           0,  0, -1,  2, -1,&
                                           0,  0,  0, -1,  1] , [5,5])
        real(sp) :: x(5), load(5), xref(5)
        logical(int8) :: dirichlet(5)
        
        ! Expected solution with Dirichlet conditions
        xref = [0.0_sp, 2.5_sp, 5.0_sp, 2.5_sp, 0.0_sp]
        x    = 0.0_sp 
        load = [0.0_sp, 0.0_sp, 5.0_sp, 0.0_sp, 0.0_sp] ! load vector
        dirichlet = .false._int8
        dirichlet([1,5]) = .true._int8

        call stdlib_solve_bicgstab(A, load, x, di=dirichlet)
        
        call check(error, norm2(x-xref)<tol*norm2(xref), 'error in BiCGSTAB solver (Dirichlet BC test)')
        if (allocated(error)) return
        end block

        ! Test 3: BiCGSTAB with Jacobi preconditioning
        block
        real(sp), parameter :: tol = 1000*epsilon(0.0_sp)
        real(sp) :: A(3,3) = reshape([real(sp) :: 10,  1,  2, &
                                            1,  10,  3, &
                                            2,   3, 10], [3,3])
        real(sp) :: x(3), load(3), xref(3)
        
        ! Well-conditioned system with diagonal dominance
        xref = [(137._sp/218._sp), -(9._sp/109._sp), (87._sp/218._sp)]
        x    = 0.0_sp ! initial guess
        load = [7.0_sp, 1.0_sp, 5.0_sp] ! load vector

        ! Test with Jacobi preconditioning (precond=1 corresponds to pc_jacobi)
        call stdlib_solve_bicgstab(A, load, x, rtol=1.e-10_sp, precond=1)
        
        call check(error, norm2(x-xref)<tol*norm2(xref), 'error in BiCGSTAB solver (Jacobi preconditioned test)')
        if (allocated(error)) return
        end block

        ! Test 1: Simple non-symmetric matrix (same as SciPy example)
        block
        real(dp), parameter :: tol = 1000*epsilon(0.0_dp)
        real(dp) :: A(4,4) = reshape([real(dp) :: 4, 2, 0, 1, &
                                            3, 0, 0, 2, &
                                            0, 1, 1, 1, &
                                            0, 2, 1, 0], [4,4])
        real(dp) :: x(4), load(4), xref(4)
        
        ! Reference solution computed with high precision
        xref = [12.5_dp, -17._dp, 23.5_dp, -24.5_dp]
        x    = 0.0_dp ! initial guess
        load = [-1.0_dp, -0.5_dp, -1.0_dp, 2.0_dp] ! load vector

        call stdlib_solve_bicgstab(A, load, x, rtol=1.e-10_dp)
        
        call check(error, norm2(x-xref)<tol*norm2(xref), 'error in BiCGSTAB solver (basic test)')
        if (allocated(error)) return
        end block

        ! Test 2: BiCGSTAB with Dirichlet boundary conditions
        block
        real(dp), parameter :: tol = 1000*epsilon(0.0_dp)
        real(dp) :: A(5,5) = reshape([real(dp) :: 1, -1,  0,  0,  0,&
                                          -1,  2, -1,  0,  0,&
                                           0, -1,  2, -1,  0,&
                                           0,  0, -1,  2, -1,&
                                           0,  0,  0, -1,  1] , [5,5])
        real(dp) :: x(5), load(5), xref(5)
        logical(int8) :: dirichlet(5)
        
        ! Expected solution with Dirichlet conditions
        xref = [0.0_dp, 2.5_dp, 5.0_dp, 2.5_dp, 0.0_dp]
        x    = 0.0_dp 
        load = [0.0_dp, 0.0_dp, 5.0_dp, 0.0_dp, 0.0_dp] ! load vector
        dirichlet = .false._int8
        dirichlet([1,5]) = .true._int8

        call stdlib_solve_bicgstab(A, load, x, di=dirichlet)
        
        call check(error, norm2(x-xref)<tol*norm2(xref), 'error in BiCGSTAB solver (Dirichlet BC test)')
        if (allocated(error)) return
        end block

        ! Test 3: BiCGSTAB with Jacobi preconditioning
        block
        real(dp), parameter :: tol = 1000*epsilon(0.0_dp)
        real(dp) :: A(3,3) = reshape([real(dp) :: 10,  1,  2, &
                                            1,  10,  3, &
                                            2,   3, 10], [3,3])
        real(dp) :: x(3), load(3), xref(3)
        
        ! Well-conditioned system with diagonal dominance
        xref = [(137._dp/218._dp), -(9._dp/109._dp), (87._dp/218._dp)]
        x    = 0.0_dp ! initial guess
        load = [7.0_dp, 1.0_dp, 5.0_dp] ! load vector

        ! Test with Jacobi preconditioning (precond=1 corresponds to pc_jacobi)
        call stdlib_solve_bicgstab(A, load, x, rtol=1.e-10_dp, precond=1)
        
        call check(error, norm2(x-xref)<tol*norm2(xref), 'error in BiCGSTAB solver (Jacobi preconditioned test)')
        if (allocated(error)) return
        end block

    end subroutine test_stdlib_solve_bicgstab

    subroutine test_stdlib_solve_bicgstab_nonsymmetric(error)
        type(error_type), allocatable, intent(out) :: error

        ! Test BiCGSTAB on a strongly non-symmetric matrix where CG would fail
        block
        real(sp), parameter :: tol = 1000*epsilon(0.0_sp)
        real(sp) :: A(4,4) = reshape([real(sp) :: 5,  1,  2,  0, &
                                           0,  4, -1,  1, &
                                           1,  0,  3,  2, &
                                           2, -1,  0,  6], [4,4])
        real(sp) :: x(4), load(4), x_exact(4), residual
        
        ! Create a known solution and corresponding RHS
        x_exact = [1.0_sp, 2.0_sp, -1.0_sp, 0.5_sp]
        load = matmul(A, x_exact)
        x = 0.0_sp ! initial guess
        
        call stdlib_solve_bicgstab(A, load, x, rtol=1.e-10_sp, atol=1.e-12_sp)
        
        call check(error, norm2(x-x_exact)<tol*norm2(x_exact), 'error in BiCGSTAB solver (strongly non-symmetric test)')
        if (allocated(error)) return
        end block

        ! Test with a matrix that has complex eigenvalues (typical challenge for iterative solvers)
        block
        real(sp), parameter :: tol = 1000*epsilon(0.0_sp)
        real(sp) :: A(3,3) = reshape([real(sp) :: 1, -2,  1, &
                                            1,  1, -1, &
                                           -1,  1,  2], [3,3])
        real(sp) :: x(3), load(3), x_exact(3)
        
        x_exact = [2.0_sp, -1.0_sp, 1.5_sp]
        load = matmul(A, x_exact)
        x = [0.1_sp, 0.1_sp, 0.1_sp] ! non-zero initial guess
        
        call stdlib_solve_bicgstab(A, load, x, rtol=1.e-8_sp, maxiter=100)
        
        call check(error, norm2(x-x_exact)<tol*norm2(x_exact), 'error in BiCGSTAB solver (complex eigenvalues test)')
        if (allocated(error)) return
        end block

        ! Test BiCGSTAB on a strongly non-symmetric matrix where CG would fail
        block
        real(dp), parameter :: tol = 1000*epsilon(0.0_dp)
        real(dp) :: A(4,4) = reshape([real(dp) :: 5,  1,  2,  0, &
                                           0,  4, -1,  1, &
                                           1,  0,  3,  2, &
                                           2, -1,  0,  6], [4,4])
        real(dp) :: x(4), load(4), x_exact(4), residual
        
        ! Create a known solution and corresponding RHS
        x_exact = [1.0_dp, 2.0_dp, -1.0_dp, 0.5_dp]
        load = matmul(A, x_exact)
        x = 0.0_dp ! initial guess
        
        call stdlib_solve_bicgstab(A, load, x, rtol=1.e-10_dp, atol=1.e-12_dp)
        
        call check(error, norm2(x-x_exact)<tol*norm2(x_exact), 'error in BiCGSTAB solver (strongly non-symmetric test)')
        if (allocated(error)) return
        end block

        ! Test with a matrix that has complex eigenvalues (typical challenge for iterative solvers)
        block
        real(dp), parameter :: tol = 1000*epsilon(0.0_dp)
        real(dp) :: A(3,3) = reshape([real(dp) :: 1, -2,  1, &
                                            1,  1, -1, &
                                           -1,  1,  2], [3,3])
        real(dp) :: x(3), load(3), x_exact(3)
        
        x_exact = [2.0_dp, -1.0_dp, 1.5_dp]
        load = matmul(A, x_exact)
        x = [0.1_dp, 0.1_dp, 0.1_dp] ! non-zero initial guess
        
        call stdlib_solve_bicgstab(A, load, x, rtol=1.e-8_dp, maxiter=100)
        
        call check(error, norm2(x-x_exact)<tol*norm2(x_exact), 'error in BiCGSTAB solver (complex eigenvalues test)')
        if (allocated(error)) return
        end block

    end subroutine test_stdlib_solve_bicgstab_nonsymmetric

end module test_linalg_solve_iterative

program test_solve_iterative
     use, intrinsic :: iso_fortran_env, only : error_unit
     use testdrive, only : run_testsuite, new_testsuite, testsuite_type
     use test_linalg_solve_iterative, only : test_linear_systems
     implicit none
     integer :: stat, is
     type(testsuite_type), allocatable :: testsuites(:)
     character(len=*), parameter :: fmt = '("#", *(1x, a))'

     stat = 0

     testsuites = [ &
         new_testsuite("linalg_solve_iterative", test_linear_systems) &
         ]

     do is = 1, size(testsuites)
         write(error_unit, fmt) "Testing:", testsuites(is)%name
         call run_testsuite(testsuites(is)%collect, error_unit, stat)
     end do

     if (stat > 0) then
         write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
         error stop
     end if
end program test_solve_iterative