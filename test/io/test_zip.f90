module test_zip
    use stdlib_io_zip
    use stdlib_string_type, only : string_type, char
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    implicit none
    private

    public :: collect_zip

contains

    !> Collect all exported unit tests
    subroutine collect_zip(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("unzip_file_not_exists", unzip_file_not_exists, should_fail=.true.), &
            new_unittest("unzip_points_to_directory", unzip_points_to_directory, should_fail=.true.), &
            new_unittest("unzip_is_not_zip", unzip_is_not_zip, should_fail=.true.), &
            new_unittest("unzip_empty_zip", unzip_empty_zip, should_fail=.true.), &
            new_unittest("unzip_zip_has_empty_file", unzip_zip_has_empty_file), &
            new_unittest("unzip_zip_has_txt_file", unzip_zip_has_txt_file), &
            new_unittest("unzip_npz_array_empty_0_file", unzip_npz_array_empty_0_file), &
            new_unittest("unzip_two_files", unzip_two_files), &
            new_unittest("unzip_compressed_npz", unzip_compressed_npz), &
            new_unittest("zip_nonexistent_file", zip_nonexistent_file, should_fail=.true.), &
            new_unittest("zip_invalid_file", zip_invalid_file, should_fail=.true.), &
            new_unittest("zip_empty_file", zip_empty_file), &
            new_unittest("zip_invalid_output_file", zip_invalid_output_file, should_fail=.true.), &
            new_unittest("zip_two_files", zip_two_files) &
            ]
    end

    subroutine unzip_file_not_exists(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call unzip("nonexistent.npz", stat=stat)
        call check(error, stat, "Reading of a non-existent npz file should fail.")
    end

    subroutine unzip_points_to_directory(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call unzip(".", stat=stat)
        call check(error, stat, "An npz file that points towards a directory should fail.")
    end

    subroutine unzip_is_not_zip(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: io, stat
        character(*), parameter :: filename = "non_zip_file"

        open(newunit=io, file=filename)
        close(io)
        call unzip(filename, stat=stat)
        call check(error, stat, "An npz file that is not a zip file should fail.")
        call delete_file(filename)
    end

    subroutine unzip_empty_zip(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: io, stat
        character(*), parameter :: filename = "empty.zip"
        character(*), parameter:: binary_data = 'PK'//char(5)//char(6)//repeat(char(0), 18)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) binary_data
        close (io)

        call unzip(filename, stat=stat)
        call check(error, stat, "An empty zip file should fail.")

        call delete_file(filename)
    end

    subroutine unzip_zip_has_empty_file(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        character(*), parameter :: filename = "empty.zip"
        character(:), allocatable :: path

        path = get_path(filename)
        if (.not. allocated(path)) then
            call test_failed(error, "The file '"//filename//"' could not be found."); return
        end if

        call unzip(path, stat=stat)
        call check(error, stat, "Listing the contents of a zip file that contains an empty file should not fail.")
    end

    subroutine unzip_zip_has_txt_file(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        character(*), parameter :: filename = "textfile.zip"
        character(:), allocatable :: path

        path = get_path(filename)
        if (.not. allocated(path)) then
            call test_failed(error, "The file '"//filename//"' could not be found."); return
        end if

        call unzip(path, stat=stat)
        call check(error, stat, "Listing the contents of a zip file that contains an empty file should not fail.")
    end

    subroutine unzip_npz_array_empty_0_file(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        character(*), parameter :: filename = "empty_0.npz"
        character(:), allocatable :: path

        path = get_path(filename)
        if (.not. allocated(path)) then
            call test_failed(error, "The file '"//filename//"' could not be found."); return
        end if

        call unzip(path, stat=stat)
        call check(error, stat, "Listing the contents of a zip file that contains an empty file should not fail.")
    end

    subroutine unzip_two_files(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        character(*), parameter :: filename = "two_files.zip"
        character(:), allocatable :: path

        path = get_path(filename)
        if (.not. allocated(path)) then
            call test_failed(error, "The file '"//filename//"' could not be found."); return
        end if

        call unzip(path, stat=stat)
        call check(error, stat, "Listing the contents of a zip file that contains an empty file should not fail.")
    end

    subroutine unzip_compressed_npz(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        character(*), parameter :: filename = "two_arr_iint64_rdp_comp.npz"
        character(:), allocatable :: path

        path = get_path(filename)
        call unzip(path, stat=stat)
        call check(error, stat, "Listing the contents of a compressed npz file should not fail.")
    end

    subroutine zip_nonexistent_file(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        character(*), parameter :: output_file = "temp.zip"
        character(*), parameter :: input_file = "nonexistent"
        type(string_type), allocatable :: files(:)

        files = [string_type(input_file)]

        call zip(output_file, files, stat)
        call check(error, stat, "Compressing a non-existent file should fail.")
    end

    subroutine zip_invalid_file(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        character(*), parameter :: output_file = "temp.zip"
        character(*), parameter :: input_file = "."
        type(string_type), allocatable :: files(:)

        files = [string_type(input_file)]

        call zip(output_file, files, stat)
        call check(error, stat, "Compressing an invalid file should fail.")
    end

    subroutine zip_empty_file(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat, unit
        character(*), parameter :: output_file = "temp.zip"
        character(*), parameter :: input_file = "abc.txt"
        type(string_type), allocatable :: files(:)

        files = [string_type(input_file)]

        open(newunit= unit, file=input_file)
        close(unit)

        call zip(output_file, files, stat)
        call check(error, stat, "Compressing a valid empty file should not fail.")

        call delete_file(input_file)
        call delete_file(output_file)
    end

    subroutine zip_invalid_output_file(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat, unit
        character(*), parameter :: output_file = "   "
        character(*), parameter :: input_file = "abc.txt"
        type(string_type), allocatable :: files(:)

        files = [string_type(input_file)]

        open(newunit=unit, file=input_file)
        close(unit)

        call zip(output_file, files, stat)
        call check(error, stat, "Providing an empty output file should fail.")

        call delete_file(input_file)
    end

    subroutine zip_two_files(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat, unit
        character(*), parameter :: output_file = "temp.zip"
        character(*), parameter :: input_file_1 = "abc.txt"
        character(*), parameter :: input_file_2 = "def.txt"
        type(string_type), allocatable :: files(:)

        files = [string_type(input_file_1), string_type(input_file_2)]

        open(newunit=unit, file=input_file_1)
        close(unit)
        open(newunit=unit, file=input_file_2)
        close(unit)

        call zip(output_file, files, stat)
        call check(error, stat, "Compressing two valid files should not fail.")

        call delete_file(input_file_1)
        call delete_file(input_file_2)
        call delete_file(output_file)
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
    end

end

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_zip, only : collect_zip
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("zip", collect_zip) &
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
