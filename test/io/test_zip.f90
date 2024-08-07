module test_zip
    use stdlib_io_zip
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    implicit none
    private

    public :: collect_zip

    character(*), parameter :: zip_files_ctest = 'zip_files/'
    character(*), parameter :: zip_files_fpm = 'test/io/'//zip_files_ctest

contains

    !> Collect all exported unit tests
    subroutine collect_zip(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("npz_file_not_exists", npz_file_not_exists, should_fail=.true.), &
            new_unittest("npz_points_to_directory", npz_points_to_directory, should_fail=.true.), &
            new_unittest("npz_is_not_zip", npz_is_not_zip, should_fail=.true.), &
            new_unittest("npz_empty_zip", npz_empty_zip, should_fail=.true.), &
            new_unittest("npz_list_file_not_exists", npz_list_file_not_exists, should_fail=.true.), &
            new_unittest("npz_list_file_is_directory", npz_list_file_is_directory, should_fail=.true.), &
            new_unittest("npz_list_file_not_zip", npz_list_file_not_zip, should_fail=.true.), &
            new_unittest("npz_list_empty_zip", npz_list_empty_zip, should_fail=.true.), &
            new_unittest("npz_list_empty_file", npz_list_empty_file), &
            new_unittest("npz_list_txt_file", npz_list_txt_file) &
            ]
    end

    subroutine npz_file_not_exists(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call unzip("nonexistent.npz", iostat=stat)
        call check(error, stat, "Reading of a non-existent npz file should fail.")
    end

    subroutine npz_points_to_directory(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call unzip(".", iostat=stat)
        call check(error, stat, "An npz file that points towards a directory should fail.")
    end

    subroutine npz_is_not_zip(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: io, stat
        character(*), parameter :: filename = "non_zip_file"

        open(newunit=io, file=filename)
        call unzip(filename, iostat=stat)
        call check(error, stat, "An npz file that is not a zip file should fail.")
        close(io, status="delete")
    end

    subroutine npz_empty_zip(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: io, stat
        character(*), parameter :: filename = "empty.zip"
        character(*), parameter:: binary_data = 'PK'//char(5)//char(6)//repeat(char(0), 18)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) binary_data
        close (io)

        call unzip(filename, iostat=stat)
        call check(error, stat, "An empty zip file should fail.")

        call delete_file(filename)
    end

    subroutine npz_list_file_not_exists(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call list_files_in_zip("nonexistent.npz", stat)
        call check(error, stat, "Trying to list the contents of a non-existent npz file should fail.")
    end

    subroutine npz_list_file_is_directory(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call list_files_in_zip(".", stat)
        call check(error, stat, "Listing of contents of a zip file that actually points towards a directory should fail.")
    end

    subroutine npz_list_file_not_zip(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: io, stat
        character(*), parameter :: filename = "non_zip_file"

        open(newunit=io, file=filename)
        call list_files_in_zip(filename, stat)
        call check(error, stat, "Listing the contents of a non-zip file should fail.")
        close(io, status="delete")
    end

    subroutine npz_list_empty_zip(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: io, stat
        character(*), parameter :: filename = "empty.zip"
        character(*), parameter:: binary_data = 'PK'//char(5)//char(6)//repeat(char(0), 18)

        open (newunit=io, file=filename, form='unformatted', access='stream')
        write (io) binary_data
        close (io)

        call list_files_in_zip(filename, stat)
        call check(error, stat, "Listing the contents of an empty zip file should fail.")

        call delete_file(filename)
    end

    subroutine npz_list_empty_file(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        character(*), parameter :: filename = "empty.zip"
        character(:), allocatable :: path

        path = get_path(filename)
        if (.not. allocated(path)) then
            call test_failed(error, "The file '"//filename//"' could not be found."); return
        end if

        call list_files_in_zip(path, stat)
        call check(error, stat, "Listing the contents of a zip file that contains an empty file should not fail.")
    end

    subroutine npz_list_txt_file(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        character(*), parameter :: filename = "textfile.zip"
        character(:), allocatable :: path

        path = get_path(filename)
        if (.not. allocated(path)) then
            call test_failed(error, "The file '"//filename//"' could not be found."); return
        end if

        call list_files_in_zip(path, stat)
        call check(error, stat, "Listing the contents of a zip file that contains an empty file should not fail.")
    end

    !> Makes sure that we find the file when running both `ctest` and `fpm test`.
    function get_path(file) result(path)
        character(*), intent(in) :: file
        character(:), allocatable :: path

        character(:), allocatable :: path_to_check
        logical :: exists

        path_to_check = zip_files_ctest//file
        inquire(file=path_to_check, exist=exists)
        if (exists) then
            path = path_to_check
        else
            path_to_check = zip_files_fpm//file
            inquire(file=path_to_check, exist=exists)
            if (exists) path = path_to_check
        end if
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
