module test_np
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp
    use stdlib_array
    use stdlib_strings, only: to_string
    use stdlib_io_np, only: load_npy, save_npy, load_npz
    use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
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
                    new_unittest("npz-nonexistent-file", test_npz_nonexistent_file, should_fail=.true.), &
                    new_unittest("npz-small-file", test_npz_small_file, should_fail=.true.), &
                    new_unittest("npz-empty-zip", test_npz_empty_zip, should_fail=.true.), &
                    new_unittest("npz-not-zip", test_npz_not_zip, should_fail=.true.), &
                    new_unittest("npz-empty-array", test_npz_empty_array), &
                    new_unittest("npz-exceeded-rank", test_npz_exceeded_rank, should_fail=.true.), &
                    new_unittest("npz-single-file-one-dim", test_npz_single_file_one_dim), &
                    new_unittest("npz-two-files-one-dim", test_npz_two_files) &
                    ]
    end subroutine collect_np

    subroutine test_read_rdp_rank2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
                                       "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }        "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=*), parameter :: filename = ".test-rdp-r2.npy"
        real(dp), allocatable :: input(:, :), output(:, :)

        allocate (input(10, 4))
        call random_number(input)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) input
        close (io)

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
                                       "{'descr': '<f4', 'fortran_order': False, 'shape': (12, 5, ), }       "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=*), parameter :: filename = ".test-rsp-r2.npy"
        real(sp), allocatable :: input(:, :), output(:, :)

        allocate (input(5, 12))
        call random_number(input)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) input
        close (io)

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
                                       "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 2, 2), }       "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=*), parameter :: filename = ".test-rdp-r3.npy"
        real(dp), allocatable :: input(:, :, :), output(:, :, :)

        allocate (input(10, 2, 2))
        call random_number(input)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) input
        close (io)

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
                                       "{'descr':'<f4', 'shape' : (37 ,  ) , 'fortran_order': False}         "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=*), parameter :: filename = ".test-rsp-r1.npy"
        real(sp), allocatable :: input(:), output(:)

        allocate (input(37))
        call random_number(input)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) input
        close (io)

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

        allocate (input(10, 4))
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

        allocate (input(12, 5))
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

        input = reshape([(i*(i + 1)/2, i=1, 40)], [2, 5, 2, 2])
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
                                       "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }        "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(50)//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-magic-num.npy"
        real(dp), allocatable :: array(:, :)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) spread(0.0_dp, 1, 40)
        close (io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_magic_number

    subroutine test_invalid_magic_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
                                       "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }        "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"numpy"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-magic-str.npy"
        real(dp), allocatable :: array(:, :)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) spread(0.0_dp, 1, 40)
        close (io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_magic_string

    subroutine test_invalid_major_version(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
                                       "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }        "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(0)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-major-version.npy"
        real(dp), allocatable :: array(:, :)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) spread(0.0_dp, 1, 40)
        close (io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_major_version

    subroutine test_invalid_minor_version(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
                                       "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }        "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(9)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-minor-version.npy"
        real(dp), allocatable :: array(:, :)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) spread(0.0_dp, 1, 40)
        close (io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_minor_version

    subroutine test_invalid_header_len(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
                                       "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }        "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict) - 1)//char(0)//dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-header-len.npy"
        real(dp), allocatable :: array(:, :)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) spread(0.0_dp, 1, 40)
        close (io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_header_len

    subroutine test_invalid_nul_byte(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
                                       "{'descr': '<f8', 'fortran_order': True, 'shape': (10, 4, ), }       "// &
                                       char(0)//char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-nul-byte.npy"
        real(dp), allocatable :: array(:, :)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) spread(0.0_dp, 1, 40)
        close (io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_nul_byte

    subroutine test_invalid_key(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
                                       "{'fortran_order': True, 'shape': (10, 4, ), 'descr': '<f8', 'x': 1, }"// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-key.npy"
        real(dp), allocatable :: array(:, :)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) spread(0.0_dp, 1, 40)
        close (io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_key

    subroutine test_invalid_comma(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
                                       "{'fortran_order': True,, 'shape': (10, 4, ), 'descr': '<f8', }       "// &
                                       "   "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-comma.npy"
        real(dp), allocatable :: array(:)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) spread(0.0_dp, 1, 40)
        close (io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_comma

    subroutine test_invalid_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
                                       "{'fortran_order': True, 'shape': (10, 4, ), 'descr': '<f8' '<f4', }  "// &
                                       "   "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-string.npy"
        real(dp), allocatable :: array(:)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) spread(0.0_dp, 1, 40)
        close (io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_invalid_string

    subroutine test_duplicate_descr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
                                       "{'descr': '<f8', 'descr': '<f8', 'fortran_order': True, 'shape': (40, ), }"// &
                                       "   "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-invalid-descr.npy"
        real(dp), allocatable :: array(:)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) spread(0.0_dp, 1, 40)
        close (io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_duplicate_descr

    subroutine test_missing_descr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
                                       "{'fortran_order': True, 'shape': (10, 4, ), }                        "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-missing-descr.npy"
        real(dp), allocatable :: array(:, :)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) spread(0.0_dp, 1, 40)
        close (io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_missing_descr

    subroutine test_missing_fortran_order(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
                                       "{'descr': '<f8', 'shape': (10, 4, ), }                               "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-missing-fortran_order.npy"
        real(dp), allocatable :: array(:, :)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) spread(0.0_dp, 1, 40)
        close (io)

        call load_npy(filename, array, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end subroutine test_missing_fortran_order

    subroutine test_missing_shape(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: dict = &
                                       "{'fortran_order': True, 'descr': '<f8'}                              "// &
                                       char(10)
        character(len=*), parameter :: header = &
                                       char(int(z"93"))//"NUMPY"//char(1)//char(0)// &
                                       char(len(dict))//char(0)//dict

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = ".test-missing-shape.npy"
        real(dp), allocatable :: array(:, :)

        open (newunit=io, file=filename, form="unformatted", access="stream")
        write (io) header
        write (io) spread(0.0_dp, 1, 40)
        close (io)

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

        allocate (input(12, 5))
        call random_number(input)
        call save_npy(filename, input, stat, msg)
        call delete_file(filename)

        call check(error,.not. allocated(msg), "Message wrongly allocated.")

    end subroutine

    subroutine test_npz_nonexistent_file(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: filename = 'test_nonexistent_file.npz'
        type(t_array_wrapper), allocatable :: arrays(:)
        integer :: stat
        character(len=:), allocatable :: msg

        call load_npz(filename, arrays, stat, msg)
        call check(error, stat, msg)
    end

    subroutine test_npz_small_file(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: header = 'PK'
        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = '.test-small-file.npz'
        type(t_array_wrapper), allocatable :: arrays(:)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) header
        close (io)

        call load_npz(filename, arrays, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end

    subroutine test_npz_empty_zip(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: header = 'PK'//achar(5)//achar(6)
        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = '.test-empty-zip.npz'
        type(t_array_wrapper), allocatable :: arrays(:)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) header
        close (io)

        call load_npz(filename, arrays, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end

    subroutine test_npz_not_zip(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=*), parameter :: header = 'PK'//achar(3)//achar(5)
        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = '.test-not-zip.npz'
        type(t_array_wrapper), allocatable :: arrays(:)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) header
        close (io)

        call load_npz(filename, arrays, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end

    subroutine test_npz_empty_array(error)
        type(error_type), allocatable, intent(out) :: error

        character(*), parameter :: binary_data = 'PK'//char(3)//char(4)//'-'//repeat(char(0), 7)//'!'//char(0)//'6H[s'// &
            & repeat(char(int(z'ff')), 8)//char(9)//char(0)//char(int(z'14'))//char(0)//'arr_0.npy'//char(1)//char(0)// &
            & char(int(z'10'))//char(0)//char(int(z'80'))//repeat(char(0), 7)//char(int(z'80'))//repeat(char(0), 7)// &
            & char(int(z'93'))//'NUMPY'//char(1)//char(0)//'v'//char(0)// &
            & "{'descr': '<f8', 'fortran_order': False, 'shape': (0,), }"//repeat(' ', 60)//char(int(z'0a'))//'PK'// &
            & char(1)//char(2)//'-'//char(3)//'-'//repeat(char(0), 7)//'!'//char(0)//'6H[s'//char(int(z'80'))// &
            & repeat(char(0), 3)//char(int(z'80'))//repeat(char(0), 3)//char(9)//repeat(char(0), 11)// &
            & char(int(z'80'))//char(1)//repeat(char(0), 4)//'arr_0.npyPK'//char(5)//char(6)//repeat(char(0), 4)// &
            & char(1)//char(0)//char(1)//char(0)//'7'//repeat(char(0), 3)//char(int(z'bb'))//repeat(char(0), 5)

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = '.test-empty-array.npz'
        type(t_array_wrapper), allocatable :: arrays(:)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) binary_data
        close (io)

        call load_npz(filename, arrays, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
        call check(error, size(arrays) == 1, 'Size of arrays not 1: '//trim(to_string(size(arrays))))
        call check(error, allocated(arrays(1)%array), 'Array not allocated.')

        select type (array => arrays(1)%array)
        type is (t_array_rdp_1)
            call check(error, allocated(array%values), 'Values not allocated.')
            call check(error, size(array%values) == 0, 'Values not empty: '//trim(to_string(size(array%values))))
        class default
            call test_failed(error, 'Array not allocated for correct type.')
        end select
    end

    subroutine test_npz_exceeded_rank(error)
        type(error_type), allocatable, intent(out) :: error

        character(*), parameter :: binary_data = 'PK'//char(3)//char(4)//'-'//repeat(char(0), 7)//'!'//char(0)//'8'// &
            & char(int(z'17'))//char(int(z'a4'))//'r'//repeat(char(int(z'ff')), 8)//char(9)//char(0)//char(int(z'14'))// &
            & char(0)//'arr_0.npy'//char(1)//char(0)//char(int(z'10'))//char(0)//char(int(z'80'))//repeat(char(0), 7)// &
            & char(int(z'80'))//repeat(char(0), 7)//char(int(z'93'))//'NUMPY'//char(1)//char(0)//'v'//char(0)// &
            & "{'descr': '<f8', 'fortran_order': False, 'shape': (0, 0, 0, 0, 0), }"//repeat(' ', 49)//char(int(z'0a'))//'PK'// &
            & char(1)//char(2)//'-'//char(3)//'-'//repeat(char(0), 7)//'!'//char(0)//'8'//char(int(z'17'))// &
            & char(int(z'a4'))//'r'//char(int(z'80'))//repeat(char(0), 3)//char(int(z'80'))//repeat(char(0), 3)//char(9)// &
            & repeat(char(0), 11)//char(int(z'80'))//char(1)//repeat(char(0), 4)//'arr_0.npyPK'//char(5)//char(6)// &
            & repeat(char(0), 4)//char(1)//char(0)//char(1)//char(0)//'7'//repeat(char(0), 3)//char(int(z'bb'))// &
            & repeat(char(0), 5)

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = '.test-exceeded-rank.npz'
        type(t_array_wrapper), allocatable :: arrays(:)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) binary_data
        close (io)

        call load_npz(filename, arrays, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
    end

    subroutine test_npz_single_file_one_dim(error)
        type(error_type), allocatable, intent(out) :: error

        ! arr_0.npy = [2,4,8]
        character(*), parameter :: binary_data = 'PK'//char(3)//char(4)//'-'//repeat(char(0), 7)//'!'//char(0)//'&M'// &
            & char(int(z'b0'))//char(int(z'd8'))//repeat(char(int(z'ff')), 8)//char(9)//char(0)//char(int(z'14'))// &
            & char(0)//'arr_0.npy'//char(1)//char(0)//char(int(z'10'))//char(0)//char(int(z'98'))//repeat(char(0), 7)// &
            & char(int(z'98'))//repeat(char(0), 7)//char(int(z'93'))//'NUMPY'//char(1)//char(0)//'v'//char(0)// &
            & "{'descr': '<i8', 'fortran_order': False, 'shape': (3,), }"//repeat(' ', 60)//char(int(z'0a'))// &
            & char(2)//repeat(char(0), 7)//char(4)//repeat(char(0), 7)//char(8)//repeat(char(0), 7)//'PK'//char(1)//char(2)// &
            & '-'//char(3)//'-'//repeat(char(0), 7)//'!'//char(0)//'&M'//char(int(z'b0'))//char(int(z'd8'))//char(int(z'98'))// &
            & repeat(char(0), 3)//char(int(z'98'))//repeat(char(0), 3)//char(9)//repeat(char(0), 11)//char(int(z'80'))//char(1)// &
            & repeat(char(0), 4)//'arr_0.npyPK'//char(5)//char(6)//repeat(char(0), 4)//char(1)//char(0)//char(1)// &
            & char(0)//'7'//repeat(char(0), 3)//char(int(z'd3'))//repeat(char(0), 5)

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = '.test-single-file-one-dim.npz'
        type(t_array_wrapper), allocatable :: arrays(:)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) binary_data
        close (io)

        call load_npz(filename, arrays, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
        call check(error, size(arrays) == 1, 'Size of arrays not 1: '//trim(to_string(size(arrays))))
        call check(error, allocated(arrays(1)%array), 'Array not allocated.')

        select type (array => arrays(1)%array)
        type is (t_array_iint64_1)
            call check(error, array%name == 'arr_0.npy', 'Wrong name: '//trim(array%name))
            call check(error, allocated(array%values), 'Values not allocated.')
            call check(error, size(array%values) == 3, 'Not 3 entries in values: '//trim(to_string(size(array%values))))
            call check(error, array%values(1) == 2, 'First value is not 2: '//trim(to_string(array%values(1))))
            call check(error, array%values(2) == 4, 'Second value is not 4: '//trim(to_string(array%values(2))))
            call check(error, array%values(3) == 8, 'Third value is not 8: '//trim(to_string(array%values(3))))
        class default
            call test_failed(error, 'Array not allocated for correct type.')
        end select
    end

    subroutine test_npz_two_files(error)
        type(error_type), allocatable, intent(out) :: error

        ! arr_0.npy = [[1,2],[3,4]]
        ! arr_1.npy = [1.2,3.4]
        character(*), parameter :: binary_data = 'PK'//char(3)//char(4)//'-'//repeat(char(0), 7)//'!'//char(0)//char(int(z'a0'))// &
            & 'DK['//repeat(char(int(z'ff')), 8)//char(9)//char(0)//char(int(z'14'))//char(0)//'arr_0.npy'//char(1)// &
            & char(0)//char(int(z'10'))//char(0)//char(int(z'a0'))//repeat(char(0), 7)//char(int(z'a0'))// &
            & repeat(char(0), 7)//char(int(z'93'))//'NUMPY'//char(1)//char(0)//'v'//char(0)// &
            & "{'descr': '<i8', 'fortran_order': False, 'shape': (2, 2), }"//repeat(' ', 58)//char(int(z'0a'))//char(1)// &
            & repeat(char(0), 7)//char(2)//repeat(char(0), 7)//char(3)//repeat(char(0), 7)//char(4)//repeat(char(0), 7)//'PK'// &
            & char(3)//char(4)//'-'//repeat(char(0), 7)//'!'//char(0)//char(int(z'f0'))//'zM?'//repeat(char(int(z'ff')), 8)// &
            & char(9)//char(0)//char(int(z'14'))//char(0)//'arr_1.npy'//char(1)//char(0)//char(int(z'10'))//char(0)// &
            & char(int(z'90'))//repeat(char(0), 7)//char(int(z'90'))//repeat(char(0), 7)//char(int(z'93'))//'NUMPY'//char(1)// &
            & char(0)//'v'//char(0)//"{'descr': '<f8', 'fortran_order': False, 'shape': (2,), }"//repeat(' ', 60)// &
            & char(int(z'0a'))//'333333'//char(int(z'f3'))//'?333333'//char(int(z'0b'))//'@PK'//char(1)//char(2)//'-'// &
            & char(3)//'-'//repeat(char(0), 7)//'!'//char(0)//char(int(z'a0'))//'DK['//char(int(z'a0'))//repeat(char(0), 3)// &
            & char(int(z'a0'))//repeat(char(0), 3)//char(9)//repeat(char(0), 11)//char(int(z'80'))//char(1)//repeat(char(0), 4)// &
            & 'arr_0.npyPK'//char(1)//char(2)//'-'//char(3)//'-'//repeat(char(0), 7)//'!'//char(0)//char(int(z'f0'))//'zM?'// &
            & char(int(z'90'))//repeat(char(0), 3)//char(int(z'90'))//repeat(char(0), 3)//char(9)//repeat(char(0), 11)// &
            & char(int(z'80'))//char(1)//char(int(z'db'))//repeat(char(0), 3)//'arr_1.npyPK'//char(5)//char(6)// &
            & repeat(char(0), 4)//char(2)//char(0)//char(2)//char(0)//'n'//repeat(char(0), 3)//char(int(z'a6'))//char(1)// &
            & repeat(char(0), 4)

        integer :: io, stat
        character(len=:), allocatable :: msg
        character(len=*), parameter :: filename = '.test-two-files.npz'
        type(t_array_wrapper), allocatable :: arrays(:)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) binary_data
        close (io)

        call load_npz(filename, arrays, stat, msg)
        call delete_file(filename)

        call check(error, stat, msg)
        call check(error, size(arrays) == 2, 'Size of arrays not 2: '//trim(to_string(size(arrays))))
        call check(error, allocated(arrays(1)%array), 'Array 1 not allocated.')
        call check(error, allocated(arrays(2)%array), 'Array 2 not allocated.')

        select type (array => arrays(1)%array)
        type is (t_array_iint64_2)
            call check(error, array%name == 'arr_0.npy', 'Wrong name: '//trim(array%name))
            call check(error, allocated(array%values), 'Values not allocated.')
            call check(error, size(array%values) == 4, 'Not 4 entries in values: '//trim(to_string(size(array%values))))
            call check(error, size(array%values, 1) == 2, 'Not 2 entries in dim 1: '//trim(to_string(size(array%values, 2))))
            call check(error, size(array%values, 2) == 2, 'Not 2 entries in dim 2: '//trim(to_string(size(array%values, 2))))
            call check(error, array%values(1, 1) == 1, 'First value in dim 1 not 1: '//trim(to_string(array%values(1, 1))))
            call check(error, array%values(2, 1) == 2, 'Second value in dim 1 not 2: '//trim(to_string(array%values(2, 1))))
            call check(error, array%values(1, 2) == 3, 'First value in dim 2 not 3: '//trim(to_string(array%values(1, 2))))
            call check(error, array%values(2, 2) == 4, 'Second value in dim 2 not 4: '//trim(to_string(array%values(2, 2))))
        class default
            call test_failed(error, 'Array not allocated for correct type.')
        end select

        select type (array => arrays(2)%array)
        type is (t_array_rdp_1)
            call check(error, array%name == 'arr_1.npy', 'Wrong name: '//trim(array%name))
            call check(error, allocated(array%values), 'Values not allocated.')
            call check(error, size(array%values) == 2, 'Not 2 entries in values: '//trim(to_string(size(array%values))))
            call check(error, array%values(1) == 1.2_dp, 'First value in dim 1 not 1.2: '//trim(to_string(array%values(1))))
            call check(error, array%values(2) == 3.4_dp, 'Second value in dim 1 not 3.4: '//trim(to_string(array%values(2))))
        class default
            call test_failed(error, 'Array not allocated for correct type.')
        end select
    end

    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename

        integer :: io

        open (newunit=io, file=filename)
        close (io, status="delete")
    end subroutine delete_file

end module test_np

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_np, only: collect_np
    implicit none

    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("np", collect_np) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program
