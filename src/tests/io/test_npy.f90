module test_npy
    use stdlib_kinds, only : int8, int16, int32, int64, sp, dp
    use stdlib_io_npy, only : save_npy, load_npy
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_npy

contains

    !> Collect all exported unit tests
    subroutine collect_npy(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("read-rdp-r2", test_read_rdp_rank2), &
            new_unittest("read-rdp-r3", test_read_rdp_rank3), &
            new_unittest("read-rsp-r1", test_read_rsp_rank1), &
            new_unittest("read-rsp-r2", test_read_rsp_rank2) &
            ]
    end subroutine collect_npy

    subroutine test_read_rdp_rank2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }        " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=*), parameter :: filename = ".test-rdp-r2.npy"
        real(dp), allocatable :: array(:, :)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat)
        call delete_file(filename)

        call check(error, stat, "Reading of npy file failed")
        if (allocated(error)) return

        call check(error, size(array), 40)
        if (allocated(error)) return
    end subroutine test_read_rdp_rank2

    subroutine test_read_rsp_rank2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'descr': '<f4', 'fortran_order': False, 'shape': (12, 5, ), }       " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=*), parameter :: filename = ".test-rsp-r2.npy"
        real(sp), allocatable :: array(:, :)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_sp, 1, 60)
        close(io)

        call load_npy(filename, array, stat)
        call delete_file(filename)

        call check(error, stat, "Reading of npy file failed")
        if (allocated(error)) return

        call check(error, size(array), 60)
        if (allocated(error)) return
    end subroutine test_read_rsp_rank2

    subroutine test_read_rdp_rank3(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 2, 2), }       " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=*), parameter :: filename = ".test-rdp-r3.npy"
        real(dp), allocatable :: array(:, :, :)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat)
        call delete_file(filename)

        call check(error, stat, "Reading of npy file failed")
        if (allocated(error)) return

        call check(error, size(array), 40)
        if (allocated(error)) return
    end subroutine test_read_rdp_rank3

    subroutine test_read_rsp_rank1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'descr':'<f4', 'shape' : (37 ,  ) , 'fortran_order': False}         " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=*), parameter :: filename = ".test-rsp-r1.npy"
        real(sp), allocatable :: array(:)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_sp, 1, 37)
        close(io)

        call load_npy(filename, array, stat)
        call delete_file(filename)

        call check(error, stat, "Reading of npy file failed")
        if (allocated(error)) return

        call check(error, size(array), 37)
        if (allocated(error)) return
    end subroutine test_read_rsp_rank1

    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename

        integer :: io

        open(newunit=io, file=filename)
        close(io, status="delete")
    end subroutine delete_file

end module test_npy


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_npy, only : collect_npy
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("npy", collect_npy) &
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
