! Test least squares solver
module test_linalg_least_squares
    use testdrive, only: error_type, check, new_unittest, unittest_type
    use stdlib_linalg_constants
    use stdlib_linalg, only: lstsq
    use stdlib_linalg_state, only: linalg_state_type

    implicit none (type,external)
    private
    
    public :: test_least_squares 

    contains

    !> Solve sample least squares problems
    subroutine test_least_squares(tests)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: tests(:)
        
        allocate(tests(0))

        tests = [tests,new_unittest("least_squares_s",test_lstsq_one_s), &
                       new_unittest("least_squares_randm_s",test_lstsq_random_s)]
        tests = [tests,new_unittest("least_squares_d",test_lstsq_one_d), &
                       new_unittest("least_squares_randm_d",test_lstsq_random_d)]

    end subroutine test_least_squares
    
    !> Simple polynomial fit
    subroutine test_lstsq_one_s(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state
        integer(ilp) :: rank

        !> Example scattered data
        real(sp), parameter :: x(*)  = real([1.0, 2.5, 3.5, 4.0, 5.0, 7.0, 8.5], sp)
        real(sp), parameter :: y(*)  = real([0.3, 1.1, 1.5, 2.0, 3.2, 6.6, 8.6], sp)
        real(sp), parameter :: ab(*) = real([0.20925829,  0.12013861], sp)

        real(sp) :: M(size(x),2),p(2)

        ! Coefficient matrix for polynomial y = a + b*x**2
        M(:,1) = x**0
        M(:,2) = x**2

        ! Find polynomial
        p = lstsq(M,y,rank=rank,err=state)

        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, all(abs(p-ab)<1.0e-6_sp), 'data converged')
        if (allocated(error)) return
        
        call check(error, rank==2, 'matrix rank == 2')
        if (allocated(error)) return

    end subroutine test_lstsq_one_s
    
    !> Fit from random array
    subroutine test_lstsq_random_s(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state
        integer(ilp), parameter :: n = 12, m = 3
        real(sp) :: xsol(m),x(m),y(n),A(n,m)

        ! Random coefficient matrix and solution
        call random_number(A)
        call random_number(xsol)
        
        ! Compute rhs
        y = matmul(A,xsol)

        ! Find polynomial
        x = lstsq(A,y,err=state)

        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, all(abs(x-xsol)<1.0e-6_sp), 'data converged')
        if (allocated(error)) return
        
    end subroutine test_lstsq_random_s    
    
    !> Simple polynomial fit
    subroutine test_lstsq_one_d(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state
        integer(ilp) :: rank

        !> Example scattered data
        real(dp), parameter :: x(*)  = real([1.0, 2.5, 3.5, 4.0, 5.0, 7.0, 8.5], dp)
        real(dp), parameter :: y(*)  = real([0.3, 1.1, 1.5, 2.0, 3.2, 6.6, 8.6], dp)
        real(dp), parameter :: ab(*) = real([0.20925829,  0.12013861], dp)

        real(dp) :: M(size(x),2),p(2)

        ! Coefficient matrix for polynomial y = a + b*x**2
        M(:,1) = x**0
        M(:,2) = x**2

        ! Find polynomial
        p = lstsq(M,y,rank=rank,err=state)

        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, all(abs(p-ab)<1.0e-6_dp), 'data converged')
        if (allocated(error)) return
        
        call check(error, rank==2, 'matrix rank == 2')
        if (allocated(error)) return

    end subroutine test_lstsq_one_d
    
    !> Fit from random array
    subroutine test_lstsq_random_d(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state
        integer(ilp), parameter :: n = 12, m = 3
        real(dp) :: xsol(m),x(m),y(n),A(n,m)

        ! Random coefficient matrix and solution
        call random_number(A)
        call random_number(xsol)
        
        ! Compute rhs
        y = matmul(A,xsol)

        ! Find polynomial
        x = lstsq(A,y,err=state)

        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, all(abs(x-xsol)<1.0e-6_dp), 'data converged')
        if (allocated(error)) return
        
    end subroutine test_lstsq_random_d    
    

end module test_linalg_least_squares

program test_lstsq
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_linalg_least_squares, only : test_least_squares
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("linalg_least_squares", test_least_squares) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program test_lstsq
