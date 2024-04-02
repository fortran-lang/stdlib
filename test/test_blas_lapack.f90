
module test_blas_lapack
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_kinds, only: sp, dp, xdp, qp, int8, int16, int32, int64
    use stdlib_linalg, only: eye
    use stdlib_linalg_blas
    use stdlib_linalg_lapack

    implicit none

    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1._dp)



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
            new_unittest("test_idamax", test_idamax) &
            ]

    end subroutine collect_blas_lapack


    subroutine test_gemvrsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(sp) :: A(3,3),x(3),y(3),ylap(3),yintr(3),alpha,beta
        call random_number(alpha)
        call random_number(beta)
        call random_number(A)
        call random_number(x)
        call random_number(y)
        ylap = y
        call gemv('No transpose',size(A,1),size(A,2),alpha,A,size(A,1),x,1,beta,ylap,1)
        yintr = alpha*matmul(A,x)+beta*y

        call check(error, sum(abs(ylap - yintr)) < sptol, &
            "blas vs. intrinsics axpy: sum() < sptol failed")
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

        call check(error, sum(abs(A - eye(3))) < sptol, &
            "lapack eye inversion: tolerance check failed")
        if (allocated(error)) return
    end subroutine test_getrirsp
    subroutine test_gemvrdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(dp) :: A(3,3),x(3),y(3),ylap(3),yintr(3),alpha,beta
        call random_number(alpha)
        call random_number(beta)
        call random_number(A)
        call random_number(x)
        call random_number(y)
        ylap = y
        call gemv('No transpose',size(A,1),size(A,2),alpha,A,size(A,1),x,1,beta,ylap,1)
        yintr = alpha*matmul(A,x)+beta*y

        call check(error, sum(abs(ylap - yintr)) < sptol, &
            "blas vs. intrinsics axpy: sum() < sptol failed")
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

        call check(error, sum(abs(A - eye(3))) < sptol, &
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

