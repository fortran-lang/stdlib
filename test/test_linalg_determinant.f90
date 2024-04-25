! Test matrix determinant
module test_linalg_determinant
    use testdrive, only: error_type, check, new_unittest, unittest_type
    use stdlib_linalg_constants
    use stdlib_linalg, only: eye, det, linalg_state_type

    implicit none (type,external)
    private
    
    public :: test_matrix_determinant 

    contains
    

    !> Matrix inversion tests
    subroutine test_matrix_determinant(tests)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: tests(:)
        
        allocate(tests(0))

        tests = [tests,new_unittest("$eye_det_rsp",test_rsp_eye_determinant)]
        tests = [tests,new_unittest("$eye_det_multiple_rsp",test_rsp_eye_multiple)]
        tests = [tests,new_unittest("$eye_det_rdp",test_rdp_eye_determinant)]
        tests = [tests,new_unittest("$eye_det_multiple_rdp",test_rdp_eye_multiple)]
        tests = [tests,new_unittest("$eye_det_csp",test_csp_eye_determinant)]
        tests = [tests,new_unittest("$eye_det_multiple_csp",test_csp_eye_multiple)]
        tests = [tests,new_unittest("$eye_det_cdp",test_cdp_eye_determinant)]
        tests = [tests,new_unittest("$eye_det_multiple_cdp",test_cdp_eye_multiple)]
        tests = [tests,new_unittest("$complex_det_cdp",test_csp_complex_determinant)]
        tests = [tests,new_unittest("$complex_det_cdp",test_cdp_complex_determinant)]

    end subroutine test_matrix_determinant

    !> Determinant of identity matrix
    subroutine test_rsp_eye_determinant(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: i
        integer(ilp), parameter :: n = 128_ilp

        real(sp) :: a(n,n),deta

        a = eye(n)

        !> Determinant function
        deta = det(a,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, abs(deta-1.0_sp)<epsilon(0.0_sp), 'det(eye(n))==1')
        
    end subroutine test_rsp_eye_determinant

    !> Determinant of identity matrix multiplier
    subroutine test_rsp_eye_multiple(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp), parameter :: n = 4_ilp
        real(sp), parameter :: coef = 0.01_sp
        integer(ilp) :: i,j
        real(sp) :: a(n,n),deta

        !> Multiply eye by a very small number
        a = eye(n)
        do concurrent (i=1:n)
          a(i,i) = coef
        end do

        !> Determinant: small, but a is not singular, because it is a multiple of the identity.
        deta = det(a,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, abs(deta-coef**n)<max(tiny(0.0_sp),epsilon(0.0_sp)*coef**n), &
                         'det(0.01*eye(n))==0.01^n')

    end subroutine test_rsp_eye_multiple
    !> Determinant of identity matrix
    subroutine test_rdp_eye_determinant(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: i
        integer(ilp), parameter :: n = 128_ilp

        real(dp) :: a(n,n),deta

        a = eye(n)

        !> Determinant function
        deta = det(a,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, abs(deta-1.0_dp)<epsilon(0.0_dp), 'det(eye(n))==1')
        
    end subroutine test_rdp_eye_determinant

    !> Determinant of identity matrix multiplier
    subroutine test_rdp_eye_multiple(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp), parameter :: n = 4_ilp
        real(dp), parameter :: coef = 0.01_dp
        integer(ilp) :: i,j
        real(dp) :: a(n,n),deta

        !> Multiply eye by a very small number
        a = eye(n)
        do concurrent (i=1:n)
          a(i,i) = coef
        end do

        !> Determinant: small, but a is not singular, because it is a multiple of the identity.
        deta = det(a,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, abs(deta-coef**n)<max(tiny(0.0_dp),epsilon(0.0_dp)*coef**n), &
                         'det(0.01*eye(n))==0.01^n')

    end subroutine test_rdp_eye_multiple
    !> Determinant of identity matrix
    subroutine test_csp_eye_determinant(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: i
        integer(ilp), parameter :: n = 128_ilp

        complex(sp) :: a(n,n),deta

        a = eye(n)

        !> Determinant function
        deta = det(a,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, abs(deta-1.0_sp)<epsilon(0.0_sp), 'det(eye(n))==1')
        
    end subroutine test_csp_eye_determinant

    !> Determinant of identity matrix multiplier
    subroutine test_csp_eye_multiple(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp), parameter :: n = 4_ilp
        real(sp), parameter :: coef = 0.01_sp
        integer(ilp) :: i,j
        complex(sp) :: a(n,n),deta

        !> Multiply eye by a very small number
        a = eye(n)
        do concurrent (i=1:n)
          a(i,i) = coef
        end do

        !> Determinant: small, but a is not singular, because it is a multiple of the identity.
        deta = det(a,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, abs(deta-coef**n)<max(tiny(0.0_sp),epsilon(0.0_sp)*coef**n), &
                         'det(0.01*eye(n))==0.01^n')

    end subroutine test_csp_eye_multiple
    !> Determinant of identity matrix
    subroutine test_cdp_eye_determinant(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: i
        integer(ilp), parameter :: n = 128_ilp

        complex(dp) :: a(n,n),deta

        a = eye(n)

        !> Determinant function
        deta = det(a,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, abs(deta-1.0_dp)<epsilon(0.0_dp), 'det(eye(n))==1')
        
    end subroutine test_cdp_eye_determinant

    !> Determinant of identity matrix multiplier
    subroutine test_cdp_eye_multiple(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp), parameter :: n = 4_ilp
        real(dp), parameter :: coef = 0.01_dp
        integer(ilp) :: i,j
        complex(dp) :: a(n,n),deta

        !> Multiply eye by a very small number
        a = eye(n)
        do concurrent (i=1:n)
          a(i,i) = coef
        end do

        !> Determinant: small, but a is not singular, because it is a multiple of the identity.
        deta = det(a,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, abs(deta-coef**n)<max(tiny(0.0_dp),epsilon(0.0_dp)*coef**n), &
                         'det(0.01*eye(n))==0.01^n')

    end subroutine test_cdp_eye_multiple

    !> Determinant of complex identity matrix
    subroutine test_csp_complex_determinant(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: i,j,n
        integer(ilp), parameter :: nmax = 10_ilp

        complex(sp), parameter :: res(nmax) = [complex(sp)::(1,1),(0,2),(-2,2),(-4,0),(-4,-4), &
                                                  (0,-8),(8,-8),(16,0),(16,16),(0,32)]

        complex(sp), allocatable :: a(:,:)
        complex(sp) :: deta(nmax)

        !> Test determinant for all sizes, 1:nmax
        matrix_size: do n=1,nmax

           ! Put 1+i on each diagonal element
           a = eye(n)
           do concurrent (i=1:n)
             a(i,i) = (1.0_sp,1.0_sp)
           end do

           ! Expected result
           deta(n) = det(a,err=state)

           deallocate(a)
           if (state%error()) exit matrix_size

        end do matrix_size

        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, all(abs(res-deta)<=epsilon(0.0_sp)), &
                         'det((1+i)*eye(n))  does not match result')

    end subroutine test_csp_complex_determinant

    subroutine test_cdp_complex_determinant(error)
        type(error_type), allocatable, intent(out) :: error

        type(linalg_state_type) :: state

        integer(ilp) :: i,j,n
        integer(ilp), parameter :: nmax = 10_ilp

        complex(dp), parameter :: res(nmax) = [complex(dp)::(1,1),(0,2),(-2,2),(-4,0),(-4,-4), &
                                                  (0,-8),(8,-8),(16,0),(16,16),(0,32)]

        complex(dp), allocatable :: a(:,:)
        complex(dp) :: deta(nmax)

        !> Test determinant for all sizes, 1:nmax
        matrix_size: do n=1,nmax

           ! Put 1+i on each diagonal element
           a = eye(n)
           do concurrent (i=1:n)
             a(i,i) = (1.0_dp,1.0_dp)
           end do

           ! Expected result
           deta(n) = det(a,err=state)

           deallocate(a)
           if (state%error()) exit matrix_size

        end do matrix_size

        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        call check(error, all(abs(res-deta)<=epsilon(0.0_dp)), &
                         'det((1+i)*eye(n))  does not match result')

    end subroutine test_cdp_complex_determinant


end module test_linalg_determinant

program test_det
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_linalg_determinant, only : test_matrix_determinant
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("linalg_determinant", test_matrix_determinant) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program
