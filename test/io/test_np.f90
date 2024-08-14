module test_np
    use stdlib_array
    use stdlib_filesystem, only : temp_dir
    use stdlib_kinds, only : int8, int16, int32, int64, sp, dp
    use stdlib_io_np, only : save_npy, load_npy, load_npz, add_array, save_npz
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    implicit none
    private

    public :: collect_np

contains

    !> Collect all exported unit tests
    subroutine collect_np(testsuite)
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
            new_unittest("iomsg-deallocated", test_iomsg_deallocated), &
            new_unittest("npz_load_nonexistent_file", npz_load_nonexistent_file, should_fail=.true.), &
            new_unittest("npz_load_invalid_dir", npz_load_invalid_dir, should_fail=.true.), &
            new_unittest("npz_load_empty_file", npz_load_empty_file, should_fail=.true.), &
            new_unittest("npz_load_empty_zip", npz_load_empty_zip, should_fail=.true.), &
            new_unittest("npz_load_arr_empty_0", npz_load_arr_empty_0), &
            new_unittest("npz_load_arr_rand_2_3", npz_load_arr_rand_2_3), &
            new_unittest("npz_load_arr_arange_10_20", npz_load_arr_arange_10_20), &
            new_unittest("npz_load_arr_cmplx", npz_load_arr_cmplx), &
            new_unittest("npz_load_two_arr_iint64_rdp", npz_load_two_arr_iint64_rdp), &
            new_unittest("npz_load_two_arr_iint64_rdp_comp", npz_load_two_arr_iint64_rdp_comp), &
            new_unittest("npz_add_to_empty_arr", npz_add_to_empty_arr), &
            new_unittest("npz_save_empty_array_input", npz_save_empty_array_input, should_fail=.true.) &
            ]
    end subroutine collect_np

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
        real(sp), allocatable :: input(:, :), output(:, :)

        msg = "This message should be deallocated."

        allocate(input(12, 5))
        call random_number(input)
        call save_npy(filename, input, stat, msg)
        call delete_file(filename)

        call check(error,.not. allocated(msg), "Message wrongly allocated.")

    end subroutine

    subroutine npz_load_nonexistent_file(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(t_array_wrapper), allocatable :: arrays(:)

        integer :: stat
        character(*), parameter :: filename = "nonexistent.npz"
        character(*), parameter :: tmp = temp_dir//"nonexistent"

        call load_npz(filename, arrays, stat, tmp_dir=tmp)
        call check(error, stat, "Loading a non-existent npz file should fail.")
    end

    subroutine npz_load_invalid_dir(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(t_array_wrapper), allocatable :: arrays(:)

        integer :: stat
        character(*), parameter :: filename = "."
        character(*), parameter :: tmp = temp_dir//"invalid_dir"


        call load_npz(filename, arrays, stat, tmp_dir=tmp)
        call check(error, stat, "A file name that points towards a directory should fail.")
    end

    subroutine npz_load_empty_file(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(t_array_wrapper), allocatable :: arrays(:)

        integer :: io, stat
        character(*), parameter :: filename = "empty_file"
        character(*), parameter :: tmp = temp_dir//"empty_file"

        open(newunit=io, file=filename)
        close(io)

        call load_npz(filename, arrays, stat, tmp_dir=tmp)
        call check(error, stat, "Try loading an empty file as an npz file should fail.")

        call delete_file(filename)
    end

    subroutine npz_load_empty_zip(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(t_array_wrapper), allocatable :: arrays(:)
        integer :: io, stat

        character(*), parameter :: filename = "empty.zip"
        character(*), parameter :: tmp = temp_dir//"empty_zip"
        character(*), parameter:: binary_data = 'PK'//char(5)//char(6)//repeat(char(0), 18)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) binary_data
        close (io)

        call load_npz(filename, arrays, stat, tmp_dir=tmp)
        call check(error, stat, "Trying to load an npz file that is an empty zip file should fail.")

        call delete_file(filename)
    end

    subroutine npz_load_arr_empty_0(error)
        type(error_type), allocatable, intent(out) :: error

        type(t_array_wrapper), allocatable :: arrays(:)
        integer :: stat
        character(*), parameter :: filename = "empty_0.npz"
        character(*), parameter :: tmp = temp_dir//"empty_0"
        character(:), allocatable :: path


        path = get_path(filename)
        call load_npz(path, arrays, stat, tmp_dir=tmp)
        call check(error, stat, "Loading an npz that contains a single empty array shouldn't fail.")
        if (allocated(error)) return
        call check(error, size(arrays) == 1, "'"//filename//"' is supposed to contain a single array.")
        if (allocated(error)) return
        call check(error, arrays(1)%array%name == "arr_0.npy", "Wrong array name.")
        if (allocated(error)) return
        select type (typed_array => arrays(1)%array)
          class is (t_array_rdp_1)
            call check(error, size(typed_array%values) == 0, "Array in '"//filename//"' is supposed to be empty.")
          class default
            call test_failed(error, "Array in '"//filename//"' is of wrong type.")
        end select
    end

    subroutine npz_load_arr_rand_2_3(error)
        type(error_type), allocatable, intent(out) :: error

        type(t_array_wrapper), allocatable :: arrays(:)
        integer :: stat
        character(*), parameter :: filename = "rand_2_3.npz"
        character(*), parameter :: tmp = temp_dir//"rand_2_3"
        character(:), allocatable :: path

        path = get_path(filename)
        call load_npz(path, arrays, stat, tmp_dir=tmp)
        call check(error, stat, "Loading an npz file that contains a valid nd_array shouldn't fail.")
        if (allocated(error)) return
        call check(error, size(arrays) == 1, "'"//filename//"' is supposed to contain a single array.")
        if (allocated(error)) return
        call check(error, arrays(1)%array%name == "arr_0.npy", "Wrong array name.")
        if (allocated(error)) return
        select type (typed_array => arrays(1)%array)
          class is (t_array_rdp_2)
            call check(error, size(typed_array%values) == 6, "Array in '"//filename//"' is supposed to have 6 entries.")
          class default
            call test_failed(error, "Array in '"//filename//"' is of wrong type.")
        end select
    end

    subroutine npz_load_arr_arange_10_20(error)
        type(error_type), allocatable, intent(out) :: error

        type(t_array_wrapper), allocatable :: arrays(:)
        integer :: stat, i
        character(*), parameter :: filename = "arange_10_20.npz"
        character(*), parameter :: tmp = temp_dir//"arange_10_20"

        character(:), allocatable :: path

        path = get_path(filename)
        call load_npz(path, arrays, stat, tmp_dir=tmp)
        call check(error, stat, "Loading an npz file that contains a valid nd_array shouldn't fail.")
        if (allocated(error)) return
        call check(error, size(arrays) == 1, "'"//filename//"' is supposed to contain a single array.")
        if (allocated(error)) return
        call check(error, arrays(1)%array%name == "arr_0.npy", "Wrong array name.")
        if (allocated(error)) return
        select type (typed_array => arrays(1)%array)
          class is (t_array_iint64_1)
            call check(error, size(typed_array%values) == 10, "Array in '"//filename//"' is supposed to have 10 entries.")
            if (allocated(error)) return
            call check(error, typed_array%values(1) == 10, "First entry is supposed to be 10.")
            if (allocated(error)) return
            do i = 2, 10
                call check(error, typed_array%values(i) == typed_array%values(i-1) + 1, "Array is supposed to be an arange.")
                if (allocated(error)) return
            end do
          class default
            call test_failed(error, "Array in '"//filename//"' is of wrong type.")
        end select
    end

    subroutine npz_load_arr_cmplx(error)
        type(error_type), allocatable, intent(out) :: error

        type(t_array_wrapper), allocatable :: arrays(:)
        integer :: stat
        character(*), parameter :: filename = "cmplx_arr.npz"
        character(*), parameter :: tmp = temp_dir//"cmplx_arr"
        character(:), allocatable :: path

        path = get_path(filename)
        call load_npz(path, arrays, stat, tmp_dir=tmp)
        call check(error, stat, "Loading an npz file that contains a valid nd_array shouldn't fail.")
        if (allocated(error)) return
        call check(error, size(arrays) == 1, "'"//filename//"' is supposed to contain a single array.")
        if (allocated(error)) return
        call check(error, arrays(1)%array%name == "cmplx.npy", "Wrong array name.")
        if (allocated(error)) return
        select type (typed_array => arrays(1)%array)
          class is (t_array_csp_1)
            call check(error, size(typed_array%values) == 3, "Array in '"//filename//"' is supposed to have 3 entries.")
            if (allocated(error)) return
            call check(error, typed_array%values(1) == cmplx(1_dp, 2_dp), "First complex number does not match.")
            if (allocated(error)) return
            call check(error, typed_array%values(2) == cmplx(3_dp, 4_dp), "Second complex number does not match.")
            if (allocated(error)) return
            call check(error, typed_array%values(3) == cmplx(5_dp, 6_dp), "Third complex number does not match.")
            if (allocated(error)) return
          class default
            call test_failed(error, "Array in '"//filename//"' is of wrong type.")
        end select
    end

    subroutine npz_load_two_arr_iint64_rdp(error)
        type(error_type), allocatable, intent(out) :: error

        type(t_array_wrapper), allocatable :: arrays(:)
        integer :: stat
        character(*), parameter :: filename = "two_arr_iint64_rdp.npz"
        character(*), parameter :: tmp = temp_dir//"two_arr_iint64_rdp"
        character(:), allocatable :: path

        path = get_path(filename)
        call load_npz(path, arrays, stat, tmp_dir=tmp)
        call check(error, stat, "Loading an npz file that contains valid nd_arrays shouldn't fail.")
        if (allocated(error)) return
        call check(error, size(arrays) == 2, "'"//filename//"' is supposed to contain two arrays.")
        if (allocated(error)) return
        call check(error, arrays(1)%array%name == "arr_0.npy", "Wrong array name.")
        if (allocated(error)) return
        call check(error, arrays(2)%array%name == "arr_1.npy", "Wrong array name.")
        if (allocated(error)) return
        select type (typed_array => arrays(1)%array)
          class is (t_array_iint64_1)
            call check(error, size(typed_array%values) == 3, "Array in '"//filename//"' is supposed to have 3 entries.")
            if (allocated(error)) return
            call check(error, typed_array%values(1) == 1, "First integer does not match.")
            if (allocated(error)) return
            call check(error, typed_array%values(2) == 2, "Second integer does not match.")
            if (allocated(error)) return
            call check(error, typed_array%values(3) == 3, "Third integer does not match.")
            if (allocated(error)) return
          class default
            call test_failed(error, "Array in '"//filename//"' is of wrong type.")
        end select
        select type (typed_array => arrays(2)%array)
          class is (t_array_rdp_1)
            call check(error, size(typed_array%values) == 3, "Array in '"//filename//"' is supposed to have 3 entries.")
            if (allocated(error)) return
            call check(error, typed_array%values(1) == 1., "First number does not match.")
            if (allocated(error)) return
            call check(error, typed_array%values(2) == 1., "Second number does not match.")
            if (allocated(error)) return
            call check(error, typed_array%values(3) == 1., "Third number does not match.")
            if (allocated(error)) return
          class default
            call test_failed(error, "Array in '"//filename//"' is of wrong type.")
        end select
    end

    subroutine npz_load_two_arr_iint64_rdp_comp(error)
        type(error_type), allocatable, intent(out) :: error

        type(t_array_wrapper), allocatable :: arrays(:)
        integer :: stat
        character(*), parameter :: filename = "two_arr_iint64_rdp_comp.npz"
        character(*), parameter :: tmp = temp_dir//"two_arr_iint64_rdp_comp"
        character(:), allocatable :: path

        path = get_path(filename)
        call load_npz(path, arrays, stat, tmp_dir=tmp)
        call check(error, stat, "Loading a compressed npz file that contains valid nd_arrays shouldn't fail.")
        if (allocated(error)) return
        call check(error, size(arrays) == 2, "'"//filename//"' is supposed to contain two arrays.")
        if (allocated(error)) return
        call check(error, arrays(1)%array%name == "arr_0.npy", "Wrong array name.")
        if (allocated(error)) return
        call check(error, arrays(2)%array%name == "arr_1.npy", "Wrong array name.")
        if (allocated(error)) return
        select type (typed_array => arrays(1)%array)
          class is (t_array_iint64_1)
            call check(error, size(typed_array%values) == 3, "Array in '"//filename//"' is supposed to have 3 entries.")
            if (allocated(error)) return
            call check(error, typed_array%values(1) == 1, "First integer does not match.")
            if (allocated(error)) return
            call check(error, typed_array%values(2) == 2, "Second integer does not match.")
            if (allocated(error)) return
            call check(error, typed_array%values(3) == 3, "Third integer does not match.")
            if (allocated(error)) return
          class default
            call test_failed(error, "Array in '"//filename//"' is of wrong type.")
        end select
        select type (typed_array => arrays(2)%array)
          class is (t_array_rdp_1)
            call check(error, size(typed_array%values) == 3, "Array in '"//filename//"' is supposed to have 3 entries.")
            if (allocated(error)) return
            call check(error, typed_array%values(1) == 1., "First number does not match.")
            if (allocated(error)) return
            call check(error, typed_array%values(2) == 1., "Second number does not match.")
            if (allocated(error)) return
            call check(error, typed_array%values(3) == 1., "Third number does not match.")
            if (allocated(error)) return
          class default
            call test_failed(error, "Array in '"//filename//"' is of wrong type.")
        end select
    end

    subroutine npz_add_to_empty_arr(error)
        type(error_type), allocatable, intent(out) :: error

        type(t_array_wrapper), allocatable :: arrays(:)
        integer :: stat
        character(*), parameter :: filename = "npz_add_arr.npz"
        real(dp), allocatable :: input_array(:,:)

        allocate(input_array(10, 4))
        call random_number(input_array)
        call add_array(arrays, input_array, stat)
        call check(error, stat, "Error adding an array to the list of arrays.")
        if (allocated(error)) return
        call check(error, size(arrays) == 1, "Array was not added to the list of arrays.")
        if (allocated(error)) return
        call check(error, arrays(1)%array%name == "arr_0.npy", "Wrong array name.")
        if (allocated(error)) return
        select type (typed_array => arrays(1)%array)
          class is (t_array_rdp_2)
            call check(error, size(typed_array%values), size(input_array), "Array sizes to not match.")
            if (allocated(error)) return
            call check(error, any(abs(typed_array%values - input_array) <= epsilon(1.0_dp)), &
                "Precision loss when adding array.")
            if (allocated(error)) return
          class default
            call test_failed(error, "Array in '"//filename//"' is of wrong type.")
        end select
    end

    ! subroutine npz_add_arr(error)
    !     type(error_type), allocatable, intent(out) :: error

    !     type(t_array_wrapper), allocatable :: arrays(:)
    !     integer :: stat
    !     character(*), parameter :: filename = "npz_add_arr.npz"
    !     real(dp), allocatable :: input_array(:,:)

    !     allocate(input_array(10, 4))
    !     call random_number(input_array)
    !     call add_array(arrays, input_array, stat)
    !     call check(error, stat, "Error adding an array to the list of arrays.")
    !     if (allocated(error)) return
    !     call check(error, size(arrays) == 1, "Array was not added to the list of arrays.")
    !     if (allocated(error)) return
    !     call check(error, arrays(1)%array%name == "arr_0.npy", "Wrong array name.")
    !     if (allocated(error)) return
    ! end

    subroutine npz_save_empty_array_input(error)
        type(error_type), allocatable, intent(out) :: error

        type(t_array_wrapper), allocatable :: arrays(:)
        integer :: stat
        character(*), parameter :: filename = "output.npz"

        allocate(arrays(0))
        call save_npz(filename, arrays, stat)
        call check(error, stat, "Trying to save an empty array fail.")
    end

    !> Makes sure that we find the file when running both `ctest` and `fpm test`.
    function get_path(file) result(path)
        character(*), intent(in) :: file
        character(:), allocatable :: path

#ifdef TEST_ROOT_DIR
        path = TEST_ROOT_DIR//'/io/zip_files/'//file
#else
        path = 'test/io/zip_files/'//file
#endif
    end

    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename

        integer :: io

        open(newunit=io, file=filename)
        close(io, status="delete")
    end subroutine delete_file
end


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_np, only : collect_np
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("np", collect_np) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end
