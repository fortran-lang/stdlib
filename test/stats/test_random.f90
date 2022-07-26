module test_stats_random
    use stdlib_kinds, only: int8, int16, int32, int64
    use stdlib_random, only : random_seed, dist_rand
    use testdrive, only: new_unittest, unittest_type, error_type, check

    implicit none

    private
    public :: collect_stats_random

contains

    !> Collect all exported unit tests
    subroutine collect_stats_random(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("random_seed", test_random_seed), &
            new_unittest("random_rand_iint8", test_random_rand_iint8), &
            new_unittest("random_rand_iint16", test_random_rand_iint8), &
            new_unittest("random_rand_iint32", test_random_rand_iint8), &
            new_unittest("random_rand_iint64", test_random_rand_iint8) &
        ]

    end subroutine collect_stats_random

    subroutine test_random_seed(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: put, get, res(5)
        integer :: ans(5) = [-1859553078, -1933696596, -642834430, &
                             1711399314,  1548311463]
        integer :: i

        put = 135792468
        do i = 1, 5
            call random_seed(put, get)
            res(i) = get
            put = get
        end do
        call check(error, all(res == ans))
    end subroutine test_random_seed

    subroutine test_random_rand_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer :: put, get, i
        integer(int8) :: res(5), ans(5) = [118, -15, -72, 101, 70]

        put = 12345678
        call random_seed(put, get)
        do i = 1, 5
            res(i) = dist_rand(1_int8)
        end do
        call check(error, all(res == ans))
    end subroutine test_random_rand_iint8

    subroutine test_random_rand_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer :: put, get, i
        integer(int16) :: res(5), ans(5) = [30286, -3799, -18204, 25947, 18148]

        put = 12345678
        call random_seed(put, get)
        do i = 1, 5
            res(i) = dist_rand(1_int16)
        end do
        call check(error, all(res == ans))
    end subroutine test_random_rand_iint16

    subroutine test_random_rand_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer :: put, get, i
        integer(int32) :: res(5), ans(5)=[1984865646, -248954393, -1192993267, &
                                  1700514835, 1189401802]

        put = 12345678
        call random_seed(put, get)
        do i = 1, 5
            res(i) = dist_rand(1_int32)
        end do
        call check(error, all(res == ans))
    end subroutine test_random_rand_iint32

    subroutine test_random_rand_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer :: put, get, i
        integer(int64) :: res(5), ans(5)=[8524933037632333570_int64, &
                                  -1069250973542918798_int64,        &
                                  -5123867065024149335_int64,        &
                                  7303655603304982073_int64,         &
                                  5108441843522503546_int64]

        put = 12345678
        call random_seed(put, get)
        do i = 1, 5
            res(i) = dist_rand(1_int64)
        end do
        call check(error, all(res == ans))
    end subroutine test_random_rand_iint64

end module test_stats_random


program tester
    use iso_fortran_env, only: error_unit
    use testdrive, only: new_testsuite, run_testsuite, testsuite_type
    use test_stats_random, only: collect_stats_random
    implicit none

    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("stats_random", collect_stats_random) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program tester
