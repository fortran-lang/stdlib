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
                  new_unittest("stdlib_solve_pcg",test_stdlib_solve_pcg) ]

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
