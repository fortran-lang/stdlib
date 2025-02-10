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
            new_unittest("read-rsp-r2", test_read_rsp_rank2), &
            new_unittest("write-rdp-r2", test_write_rdp_rank2), &
            new_unittest("write-rsp-r2", test_write_rsp_rank2), &
            new_unittest("write-i2-r4", test_write_int16_rank4), &
            new_unittest("invalid-magic-number", test_invalid_magic_number, should_fail=.true.), &
            new_unittest("invalid-magic-string", test_invalid_magic_string, should_fail=.true.), &
            new_unittest("invalid-major-version", test_invalid_major_version, should_fail=.true.), &
            new_unittest("invalid-minor-version", test_invalid_minor_version, should_fail=.true.), &
            new_unittest("invalid-header-len", test_invalid_header_len, should_fail=.true.), &
            new_unittest("invalid-nul-byte", test_invalid_nul_byte, should_fail=.true.), &
            new_unittest("invalid-key", test_invalid_key, should_fail=.true.), &
            new_unittest("invalid-comma", test_invalid_comma, should_fail=.true.), &
            new_unittest("invalid-string", test_invalid_string, should_fail=.true.), &
            new_unittest("duplicate-descr", test_duplicate_descr, should_fail=.true.), &
            new_unittest("missing-descr", test_missing_descr, should_fail=.true.), &
            new_unittest("missing-fortran_order", test_missing_fortran_order, should_fail=.true.), &
            new_unittest("missing-shape", test_missing_shape, should_fail=.true.), &
            new_unittest("iomsg-deallocated", test_iomsg_deallocated) &
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
        real(dp), allocatable :: input(:, :), output(:, :)

        allocate(input(10, 4))
        call random_number(input)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) input
        close(io)

        call load_npy(filename, output, stat)
        call delete_file(filename)

        call check(error, stat, "Reading of npy file failed")
        if (allocated(error)) return

        call check(error, size(output), size(input))
        if (allocated(error)) return

        call check(error, any(abs(output - input) <= epsilon(1.0_dp)), &
           "Precision loss when rereading array")
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
        real(sp), allocatable :: input(:, :), output(:, :)

        allocate(input(5, 12))
        call random_number(input)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) input
        close(io)

        call load_npy(filename, output, stat)
        call delete_file(filename)

        call check(error, stat, "Reading of npy file failed")
        if (allocated(error)) return

        call check(error, size(output), size(input))
        if (allocated(error)) return

        call check(error, any(abs(output - input) <= epsilon(1.0_dp)), &
           "Precision loss when rereading array")
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
        real(dp), allocatable :: input(:, :, :), output(:, :, :)

        allocate(input(10, 2, 2))
        call random_number(input)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) input
        close(io)

        call load_npy(filename, output, stat)
        call delete_file(filename)

        call check(error, stat, "Reading of npy file failed")
        if (allocated(error)) return

        call check(error, size(output), size(input))
        if (allocated(error)) return

        call check(error, any(abs(output - input) <= epsilon(1.0_dp)), &
           "Precision loss when rereading array")
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
        real(sp), allocatable :: input(:), output(:)

        allocate(input(37))
        call random_number(input)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) input
        close(io)

        call load_npy(filename, output, stat)
        call delete_file(filename)

        call check(error, stat, "Reading of npy file failed")
        if (allocated(error)) return

        call check(error, size(output), 37)
        if (allocated(error)) return

        call check(error, any(abs(output - input) <= epsilon(1.0_dp)), &
           "Precision loss when rereading array")
    end subroutine test_read_rsp_rank1

    subroutine test_write_rdp_rank2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        character(len=*), parameter :: filename = ".test-rdp-r2-rt.npy"
        real(dp), allocatable :: input(:, :), output(:, :)

        allocate(input(10, 4))
        call random_number(input)
        call save_npy(filename, input, stat)

        call check(error, stat, "Writing of npy file failed")
        if (allocated(error)) return

        call load_npy(filename, output, stat)
        call delete_file(filename)

        call check(error, stat, "Reading of npy file failed")
        if (allocated(error)) return

        call check(error, size(output), size(input))
        if (allocated(error)) return

        call check(error, any(abs(output - input) <= epsilon(1.0_dp)), &
           "Precision loss when rereading array")
    end subroutine test_write_rdp_rank2

    subroutine test_write_rsp_rank2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        character(len=*), parameter :: filename = ".test-rsp-r2-rt.npy"
        real(sp), allocatable :: input(:, :), output(:, :)

        allocate(input(12, 5))
        call random_number(input)
        call save_npy(filename, input, stat)

        call check(error, stat, "Writing of npy file failed")
        if (allocated(error)) return

        call load_npy(filename, output, stat)
        call delete_file(filename)

        call check(error, stat, "Reading of npy file failed")
        if (allocated(error)) return

        call check(error, size(output), size(input))
        if (allocated(error)) return

        call check(error, any(abs(output - input) <= epsilon(1.0_dp)), &
           "Precision loss when rereading array")
    end subroutine test_write_rsp_rank2

    subroutine test_write_int16_rank4(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: stat, i
        character(len=*), parameter :: filename = ".test-i2-r4-rt.npy"
        integer(int16), allocatable :: input(:, :, :, :), output(:, :, :, :)

        input = reshape([(i*(i+1)/2, i = 1, 40)], [2, 5, 2, 2])
        call save_npy(filename, input, stat)

        call check(error, stat, "Writing of npy file failed")
        if (allocated(error)) return

        call load_npy(filename, output, stat)
        call delete_file(filename)

        call check(error, stat, "Reading of npy file failed")
        if (allocated(error)) return

        call check(error, size(output), size(input))
        if (allocated(error)) return

        call check(error, all(abs(output - input) == 0), &
           "Precision loss when rereading array")
    end subroutine test_write_int16_rank4

    subroutine test_invalid_magic_number(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }        " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(50) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-magic-num.npy"
        real(dp), allocatable :: array(:, :)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_magic_number

    subroutine test_invalid_magic_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }        " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "numpy" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-magic-str.npy"
        real(dp), allocatable :: array(:, :)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_magic_string

    subroutine test_invalid_major_version(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }        " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(0) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-major-version.npy"
        real(dp), allocatable :: array(:, :)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_major_version

    subroutine test_invalid_minor_version(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }        " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(9) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-minor-version.npy"
        real(dp), allocatable :: array(:, :)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_minor_version

    subroutine test_invalid_header_len(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }        " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)-1) // char(0) // dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-header-len.npy"
        real(dp), allocatable :: array(:, :)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_header_len

    subroutine test_invalid_nul_byte(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }       " //  &
            char(0) // char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-nul-byte.npy"
        real(dp), allocatable :: array(:, :)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_nul_byte

    subroutine test_invalid_key(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'fortran_order': True, 'shape': (10, 4, ), 'descr': '<f8', 'x': 1, }" //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-key.npy"
        real(dp), allocatable :: array(:, :)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_key

    subroutine test_invalid_comma(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'fortran_order': True,, 'shape': (10, 4, ), 'descr': '<f8', }       " //  &
            "   " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-comma.npy"
        real(dp), allocatable :: array(:)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_comma

    subroutine test_invalid_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'fortran_order': True, 'shape': (10, 4, ), 'descr': '<f8' '<f4', }  " //  &
            "   " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-string.npy"
        real(dp), allocatable :: array(:)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_string

    subroutine test_duplicate_descr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'descr': '<f8', 'descr': '<f8', 'fortran_order': True, 'shape': (40, ), }"//&
            "   " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-descr.npy"
        real(dp), allocatable :: array(:)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_duplicate_descr

    subroutine test_missing_descr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'fortran_order': True, 'shape': (10, 4, ), }                        " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-missing-descr.npy"
        real(dp), allocatable :: array(:, :)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_missing_descr

    subroutine test_missing_fortran_order(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'descr': '<f8', 'shape': (10, 4, ), }                               " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-missing-fortran_order.npy"
        real(dp), allocatable :: array(:, :)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_missing_fortran_order

    subroutine test_missing_shape(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
            "{'fortran_order': True, 'descr': '<f8'}                              " //  &
            char(10)
        character(len=*), parameter :: header = &
            char(int(z"93")) // "NUMPY" // char(1) // char(0) // &
            char(len(dict)) // char(0) // dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-missing-shape.npy"
        real(dp), allocatable :: array(:, :)

        open(newunit=io, file=filename, form="unformatted", access="stream")
        write(io) header
        write(io) spread(0.0_dp, 1, 40)
        close(io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_missing_shape

    subroutine test_iomsg_deallocated(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        character(len=:), allocatable :: msg

        character(len=*), parameter :: filename = ".test-iomsg-deallocated.npy"
        real(sp), allocatable :: input(:, :)

        msg = "This message should be deallocated."

        allocate(input(12, 5))
        call random_number(input)
        call save_npy(filename, input, stat, msg)
        call delete_file(filename)

        call check(error,.not. allocated(msg), "Message wrongly allocated.")

    end subroutine

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
