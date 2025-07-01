! Test QR factorization 
module test_linalg_qr
    use testdrive, only: error_type, check, new_unittest, unittest_type
    use stdlib_linalg_constants
    use stdlib_linalg_state, only: LINALG_VALUE_ERROR,linalg_state_type
    use stdlib_linalg, only: qr,qr_space
    use ieee_arithmetic, only: ieee_value,ieee_quiet_nan

    implicit none (type,external)
    
    public :: test_qr_factorization

    contains

    !> QR factorization tests
    subroutine test_qr_factorization(tests)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: tests(:)
        
        allocate(tests(0))
        
        call add_test(tests,new_unittest("qr_random_s",test_qr_random_s))
        call add_test(tests,new_unittest("qr_random_d",test_qr_random_d))
        call add_test(tests,new_unittest("qr_random_c",test_qr_random_c))
        call add_test(tests,new_unittest("qr_random_z",test_qr_random_z))

    end subroutine test_qr_factorization

    !> QR factorization of a random matrix
    subroutine test_qr_random_s(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: m   = 15_ilp
        integer(ilp), parameter :: n   =  4_ilp
        integer(ilp), parameter :: k   = min(m,n)
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        real(sp) :: a(m,n),aorig(m,n),q(m,m),r(m,n),qred(m,k),rred(k,n),qerr(m,6),rerr(6,n)
        real(sp) :: rea(m,n),ima(m,n)
        integer(ilp) :: lwork
        real(sp), allocatable :: work(:)
        type(linalg_state_type) :: state
        
        call random_number(rea)
        a = rea
        aorig = a
        
        ! 1) QR factorization with full matrices. Input NaNs to be sure Q and R are OK on return
        q = ieee_value(0.0_sp,ieee_quiet_nan)
        r = ieee_value(0.0_sp,ieee_quiet_nan)
        call qr(a,q,r,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(a-matmul(q,r))<tol), 'converged solution (fulle)')
        if (allocated(error)) return        
                
        ! 2) QR factorization with reduced matrices
        call qr(a,qred,rred,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(a-matmul(qred,rred))<tol), 'converged solution (reduced)')
        if (allocated(error)) return        
        
        ! 3) overwrite A
        call qr(a,qred,rred,overwrite_a=.true.,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(aorig-matmul(qred,rred))<tol), 'converged solution (overwrite A)')
        if (allocated(error)) return                
        
        ! 4) External storage option   
        a = aorig
        call qr_space(a,lwork)
        allocate(work(lwork))
        call qr(a,q,r,storage=work,err=state)
    
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(a-matmul(q,r))<tol), 'converged solution (external storage)')
        if (allocated(error)) return          
        
        ! Check that an invalid problem size returns an error
        a = aorig        
        call qr(a,qerr,rerr,err=state)
        call check(error,state%error(),'invalid matrix sizes')
        if (allocated(error)) return             
        
    end subroutine test_qr_random_s

    subroutine test_qr_random_d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: m   = 15_ilp
        integer(ilp), parameter :: n   =  4_ilp
        integer(ilp), parameter :: k   = min(m,n)
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        real(dp) :: a(m,n),aorig(m,n),q(m,m),r(m,n),qred(m,k),rred(k,n),qerr(m,6),rerr(6,n)
        real(dp) :: rea(m,n),ima(m,n)
        integer(ilp) :: lwork
        real(dp), allocatable :: work(:)
        type(linalg_state_type) :: state
        
        call random_number(rea)
        a = rea
        aorig = a
        
        ! 1) QR factorization with full matrices. Input NaNs to be sure Q and R are OK on return
        q = ieee_value(0.0_dp,ieee_quiet_nan)
        r = ieee_value(0.0_dp,ieee_quiet_nan)
        call qr(a,q,r,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(a-matmul(q,r))<tol), 'converged solution (fulle)')
        if (allocated(error)) return        
                
        ! 2) QR factorization with reduced matrices
        call qr(a,qred,rred,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(a-matmul(qred,rred))<tol), 'converged solution (reduced)')
        if (allocated(error)) return        
        
        ! 3) overwrite A
        call qr(a,qred,rred,overwrite_a=.true.,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(aorig-matmul(qred,rred))<tol), 'converged solution (overwrite A)')
        if (allocated(error)) return                
        
        ! 4) External storage option   
        a = aorig
        call qr_space(a,lwork)
        allocate(work(lwork))
        call qr(a,q,r,storage=work,err=state)
    
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(a-matmul(q,r))<tol), 'converged solution (external storage)')
        if (allocated(error)) return          
        
        ! Check that an invalid problem size returns an error
        a = aorig        
        call qr(a,qerr,rerr,err=state)
        call check(error,state%error(),'invalid matrix sizes')
        if (allocated(error)) return             
        
    end subroutine test_qr_random_d

    subroutine test_qr_random_c(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: m   = 15_ilp
        integer(ilp), parameter :: n   =  4_ilp
        integer(ilp), parameter :: k   = min(m,n)
        real(sp), parameter :: tol = 100*sqrt(epsilon(0.0_sp))
        complex(sp) :: a(m,n),aorig(m,n),q(m,m),r(m,n),qred(m,k),rred(k,n),qerr(m,6),rerr(6,n)
        real(sp) :: rea(m,n),ima(m,n)
        integer(ilp) :: lwork
        complex(sp), allocatable :: work(:)
        type(linalg_state_type) :: state
        
        call random_number(rea)
        call random_number(ima)
        a = cmplx(rea,ima,kind=sp)
        aorig = a
        
        ! 1) QR factorization with full matrices. Input NaNs to be sure Q and R are OK on return
        q = ieee_value(0.0_sp,ieee_quiet_nan)
        r = ieee_value(0.0_sp,ieee_quiet_nan)
        call qr(a,q,r,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(a-matmul(q,r))<tol), 'converged solution (fulle)')
        if (allocated(error)) return        
                
        ! 2) QR factorization with reduced matrices
        call qr(a,qred,rred,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(a-matmul(qred,rred))<tol), 'converged solution (reduced)')
        if (allocated(error)) return        
        
        ! 3) overwrite A
        call qr(a,qred,rred,overwrite_a=.true.,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(aorig-matmul(qred,rred))<tol), 'converged solution (overwrite A)')
        if (allocated(error)) return                
        
        ! 4) External storage option   
        a = aorig
        call qr_space(a,lwork)
        allocate(work(lwork))
        call qr(a,q,r,storage=work,err=state)
    
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(a-matmul(q,r))<tol), 'converged solution (external storage)')
        if (allocated(error)) return          
        
        ! Check that an invalid problem size returns an error
        a = aorig        
        call qr(a,qerr,rerr,err=state)
        call check(error,state%error(),'invalid matrix sizes')
        if (allocated(error)) return             
        
    end subroutine test_qr_random_c

    subroutine test_qr_random_z(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: m   = 15_ilp
        integer(ilp), parameter :: n   =  4_ilp
        integer(ilp), parameter :: k   = min(m,n)
        real(dp), parameter :: tol = 100*sqrt(epsilon(0.0_dp))
        complex(dp) :: a(m,n),aorig(m,n),q(m,m),r(m,n),qred(m,k),rred(k,n),qerr(m,6),rerr(6,n)
        real(dp) :: rea(m,n),ima(m,n)
        integer(ilp) :: lwork
        complex(dp), allocatable :: work(:)
        type(linalg_state_type) :: state
        
        call random_number(rea)
        call random_number(ima)
        a = cmplx(rea,ima,kind=dp)
        aorig = a
        
        ! 1) QR factorization with full matrices. Input NaNs to be sure Q and R are OK on return
        q = ieee_value(0.0_dp,ieee_quiet_nan)
        r = ieee_value(0.0_dp,ieee_quiet_nan)
        call qr(a,q,r,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(a-matmul(q,r))<tol), 'converged solution (fulle)')
        if (allocated(error)) return        
                
        ! 2) QR factorization with reduced matrices
        call qr(a,qred,rred,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(a-matmul(qred,rred))<tol), 'converged solution (reduced)')
        if (allocated(error)) return        
        
        ! 3) overwrite A
        call qr(a,qred,rred,overwrite_a=.true.,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(aorig-matmul(qred,rred))<tol), 'converged solution (overwrite A)')
        if (allocated(error)) return                
        
        ! 4) External storage option   
        a = aorig
        call qr_space(a,lwork)
        allocate(work(lwork))
        call qr(a,q,r,storage=work,err=state)
    
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return        
        
        ! Check solution
        call check(error, all(abs(a-matmul(q,r))<tol), 'converged solution (external storage)')
        if (allocated(error)) return          
        
        ! Check that an invalid problem size returns an error
        a = aorig        
        call qr(a,qerr,rerr,err=state)
        call check(error,state%error(),'invalid matrix sizes')
        if (allocated(error)) return             
        
    end subroutine test_qr_random_z


    ! gcc-15 bugfix utility
    subroutine add_test(tests,new_test)
        type(unittest_type), allocatable, intent(inout) :: tests(:)    
        type(unittest_type), intent(in) :: new_test
        
        integer :: n
        type(unittest_type), allocatable :: new_tests(:)
        
        if (allocated(tests)) then 
            n = size(tests)
        else
            n = 0
        end if
        
        allocate(new_tests(n+1))
        if (n>0) new_tests(1:n) = tests(1:n)
                 new_tests(1+n) = new_test
        call move_alloc(from=new_tests,to=tests)        
        
    end subroutine add_test

end module test_linalg_qr

program test_qr
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_linalg_qr, only : test_qr_factorization
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("linalg_qr", test_qr_factorization) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program test_qr
