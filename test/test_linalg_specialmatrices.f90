module test_specialmatrices
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_kinds
    use stdlib_linalg, only: hermitian
    use stdlib_linalg_state, only: linalg_state_type
    use stdlib_math, only: all_close
    use stdlib_specialmatrices
    implicit none

contains


    !> Collect all exported unit tests
    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('tridiagonal', test_tridiagonal), &
            new_unittest('tridiagonal error handling', test_tridiagonal_error_handling) &
        ]
    end subroutine

    subroutine test_tridiagonal(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            integer, parameter :: n = 5
            type(tridiagonal_sp_type) :: A
            real(sp), allocatable :: Amat(:,:), dl(:), dv(:), du(:)
            real(sp), allocatable :: x(:)
            real(sp), allocatable :: y1(:), y2(:)

            ! Initialize matrix.
            allocate(dl(n-1), dv(n), du(n-1))
            call random_number(dl) ; call random_number(dv) ; call random_number(du)
            A = tridiagonal(dl, dv, du) ; Amat = dense(A)

            ! Random vectors.
            allocate(x(n), source = 0.0_wp)   ; call random_number(x)
            allocate(y1(n), source = 0.0_wp)  ; allocate(y2(n), source=0.0_wp)

            ! Test y = A @ x
            y1 = matmul(Amat, x) ; call spmv(A, x, y2)
            call check(error, all_close(y1, y2), .true.)
            if (allocated(error)) return

            ! Test y = A.T @ x
            y1 = 0.0_wp ; y2 = 0.0_wp
            y1 = matmul(transpose(Amat), x) ; call spmv(A, x, y2, op="T")
            call check(error, all_close(y1, y2), .true.)
            if (allocated(error)) return

        end block
        block
            integer, parameter :: wp = dp
            integer, parameter :: n = 5
            type(tridiagonal_dp_type) :: A
            real(dp), allocatable :: Amat(:,:), dl(:), dv(:), du(:)
            real(dp), allocatable :: x(:)
            real(dp), allocatable :: y1(:), y2(:)

            ! Initialize matrix.
            allocate(dl(n-1), dv(n), du(n-1))
            call random_number(dl) ; call random_number(dv) ; call random_number(du)
            A = tridiagonal(dl, dv, du) ; Amat = dense(A)

            ! Random vectors.
            allocate(x(n), source = 0.0_wp)   ; call random_number(x)
            allocate(y1(n), source = 0.0_wp)  ; allocate(y2(n), source=0.0_wp)

            ! Test y = A @ x
            y1 = matmul(Amat, x) ; call spmv(A, x, y2)
            call check(error, all_close(y1, y2), .true.)
            if (allocated(error)) return

            ! Test y = A.T @ x
            y1 = 0.0_wp ; y2 = 0.0_wp
            y1 = matmul(transpose(Amat), x) ; call spmv(A, x, y2, op="T")
            call check(error, all_close(y1, y2), .true.)
            if (allocated(error)) return

        end block
    end subroutine

    subroutine test_tridiagonal_error_handling(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            integer, parameter :: n = 5
            type(tridiagonal_sp_type) :: A
            real(sp), allocatable :: dl(:), dv(:), du(:)
            type(linalg_state_type) :: state
            integer :: i

            !> Test constructor from arrays.
            dl = [(1.0_wp, i = 1, n-2)] ; du = dl
            dv = [(2.0_wp, i = 1, n)]
            A = tridiagonal(dl, dv, du, state)
            call check(error, state%ok(), .false.)
            if (allocated(error)) return

            !> Test contructor from constants.
            A = tridiagonal(dl(1), dv(1), du(1), -n, state)
            call check(error, state%ok(), .false.)
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            integer, parameter :: n = 5
            type(tridiagonal_dp_type) :: A
            real(dp), allocatable :: dl(:), dv(:), du(:)
            type(linalg_state_type) :: state
            integer :: i

            !> Test constructor from arrays.
            dl = [(1.0_wp, i = 1, n-2)] ; du = dl
            dv = [(2.0_wp, i = 1, n)]
            A = tridiagonal(dl, dv, du, state)
            call check(error, state%ok(), .false.)
            if (allocated(error)) return

            !> Test contructor from constants.
            A = tridiagonal(dl(1), dv(1), du(1), -n, state)
            call check(error, state%ok(), .false.)
            if (allocated(error)) return
        end block
    end subroutine

end module


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_specialmatrices, only : collect_suite
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("sparse", collect_suite) &
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
