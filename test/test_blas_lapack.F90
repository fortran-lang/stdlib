
module test_blas_lapack
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_kinds, only: sp, dp, xdp, qp, int8, int16, int32, int64
    use stdlib_linalg, only: eye
    use stdlib_linalg_blas
    use stdlib_linalg_lapack

    implicit none


contains

    !> Collect all exported unit tests
    subroutine collect_blas_lapack(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_gemvrsp", test_gemvrsp), &
            new_unittest("test_getrirsp", test_getrirsp), &
            new_unittest("test_gemvrdp", test_gemvrdp), &
            new_unittest("test_getrirdp", test_getrirdp), &
            new_unittest("test_idamax", test_idamax), &
            new_unittest("test_external_blas",external_blas_test), &
            new_unittest("test_external_lapack",external_lapack_test) &
            ]

    end subroutine collect_blas_lapack


    subroutine test_gemvrsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(sp) :: A(3,3),x(3),y(3),ylap(3),yintr(3),alpha,beta
        real(sp), parameter :: tol = 1000 * epsilon(1.0_sp)
        call random_number(alpha)
        call random_number(beta)
        call random_number(A)
        call random_number(x)
        call random_number(y)
        ylap = y
        call gemv('No transpose',size(A,1),size(A,2),alpha,A,size(A,1),x,1,beta,ylap,1)
        yintr = alpha*matmul(A,x)+beta*y

        call check(error, sum(abs(ylap - yintr)) < tol, &
            "blas vs. intrinsics axpy: sum() < tol failed")
        if (allocated(error)) return

    end subroutine test_gemvrsp

    ! Find matrix inverse from LU decomposition
    subroutine test_getrirsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3
        real(sp) :: A(n,n)
        real(sp),allocatable :: work(:)
        integer(ilp) :: ipiv(n),info,lwork,nb
        real(sp), parameter :: tol = 1000 * epsilon(1.0_sp)

        A = eye(n)

        ! Factorize matrix (overwrite result)
        call getrf(size(A,1),size(A,2),A,size(A,1),ipiv,info)
        call check(error, info==0, "lapack getrf returned info/=0")
        if (allocated(error)) return

        ! Get optimal worksize (returned in work(1)) (apply 2% safety parameter)
        nb = stdlib_ilaenv(1,'rgetri',' ',n,-1,-1,-1)
        lwork = nint(1.02*n*nb,kind=ilp)
        allocate (work(lwork))

        ! Invert matrix
        call getri(n,a,n,ipiv,work,lwork,info)

        call check(error, info==0, "lapack getri returned info/=0")
        if (allocated(error)) return

        call check(error, sum(abs(A - eye(3))) < tol, &
            "lapack eye inversion: tolerance check failed")
        if (allocated(error)) return

    end subroutine test_getrirsp
    subroutine test_gemvrdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(dp) :: A(3,3),x(3),y(3),ylap(3),yintr(3),alpha,beta
        real(dp), parameter :: tol = 1000 * epsilon(1.0_dp)
        call random_number(alpha)
        call random_number(beta)
        call random_number(A)
        call random_number(x)
        call random_number(y)
        ylap = y
        call gemv('No transpose',size(A,1),size(A,2),alpha,A,size(A,1),x,1,beta,ylap,1)
        yintr = alpha*matmul(A,x)+beta*y

        call check(error, sum(abs(ylap - yintr)) < tol, &
            "blas vs. intrinsics axpy: sum() < tol failed")
        if (allocated(error)) return

    end subroutine test_gemvrdp

    ! Find matrix inverse from LU decomposition
    subroutine test_getrirdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3
        real(dp) :: A(n,n)
        real(dp),allocatable :: work(:)
        integer(ilp) :: ipiv(n),info,lwork,nb
        real(dp), parameter :: tol = 1000 * epsilon(1.0_dp)

        A = eye(n)

        ! Factorize matrix (overwrite result)
        call getrf(size(A,1),size(A,2),A,size(A,1),ipiv,info)
        call check(error, info==0, "lapack getrf returned info/=0")
        if (allocated(error)) return

        ! Get optimal worksize (returned in work(1)) (apply 2% safety parameter)
        nb = stdlib_ilaenv(1,'rgetri',' ',n,-1,-1,-1)
        lwork = nint(1.02*n*nb,kind=ilp)
        allocate (work(lwork))

        ! Invert matrix
        call getri(n,a,n,ipiv,work,lwork,info)

        call check(error, info==0, "lapack getri returned info/=0")
        if (allocated(error)) return

        call check(error, sum(abs(A - eye(3))) < tol, &
            "lapack eye inversion: tolerance check failed")
        if (allocated(error)) return

    end subroutine test_getrirdp

    ! Return
    subroutine test_idamax(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 5
        integer(ilp) :: imax
        real(dp) :: x(n)

        x = [1,2,3,4,5]

        imax = stdlib_idamax(n,x,1)

        call check(error, imax==5, "blas idamax returned wrong location")

    end subroutine test_idamax

    !> Test availability of the external BLAS interface
    subroutine external_blas_test(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

#ifdef STDLIB_EXTERNAL_BLAS           
        interface 
            subroutine saxpy(n,sa,sx,incx,sy,incy)
                 import sp,ilp 
                 implicit none(type,external) 
                 real(sp), intent(in) :: sa,sx(*)
                 integer(ilp), intent(in) :: incx,incy,n
                 real(sp), intent(inout) :: sy(*)
            end subroutine saxpy        
        end interface
        
        integer(ilp), parameter :: n = 5, inc=1
        real(sp) :: a,x(n),y(n)
             
        x = 1.0_sp
        y = 2.0_sp
        a = 3.0_sp
        
        call saxpy(n,a,x,inc,y,inc)
        call check(error, all(abs(y-5.0_sp)<sqrt(epsilon(0.0_sp))), "saxpy: check result")
        if (allocated(error)) return
        
#else
        call skip_test(error, "Not using an external BLAS")
#endif        
        
    end subroutine external_blas_test        
                
    !> Test availability of the external BLAS interface
    subroutine external_lapack_test(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

#ifdef STDLIB_EXTERNAL_LAPACK       
        interface 
           subroutine dgetrf( m, n, a, lda, ipiv, info )
                import dp,ilp
                implicit none(type,external) 
                integer(ilp), intent(out) :: info,ipiv(*)
                integer(ilp), intent(in) :: lda,m,n
                real(dp), intent(inout) :: a(lda,*)
           end subroutine dgetrf       
        end interface
        
        integer(ilp), parameter :: n = 3
        real(dp) :: A(n,n)
        integer(ilp) :: ipiv(n),info


        A = eye(n)        
        info = 123

        ! Factorize matrix 
        call dgetrf(n,n,A,n,ipiv,info)

        call check(error, info==0, "dgetrf: check result")
        if (allocated(error)) return
        
#else
        call skip_test(error, "Not using an external LAPACK")
#endif        
        
    end subroutine external_lapack_test                  

end module test_blas_lapack


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_blas_lapack, only : collect_blas_lapack
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0
    testsuites = [ &
        new_testsuite("blas_lapack", collect_blas_lapack) &
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

