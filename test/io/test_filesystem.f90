module test_filesystem
    use stdlib_io_filesystem
    use stdlib_string_type, only: char, string_type
    use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
    implicit none
    private

    public :: collect_filesystem

    character(*), parameter :: temp_list_dir = 'temp_list_dir'

contains

    !> Collect all exported unit tests
    subroutine collect_filesystem(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("fs_file_is_windows", fs_is_windows), &
            new_unittest("fs_file_not_exists", fs_file_not_exists, should_fail=.true.), &
            new_unittest("fs_file_exists", fs_file_exists), &
            new_unittest("fs_current_dir_exists", fs_current_dir_exists), &
            new_unittest("fs_path_separator", fs_path_separator), &
            new_unittest("fs_run_invalid_command", fs_run_invalid_command, should_fail=.true.), &
            new_unittest("fs_run_with_invalid_option", fs_run_with_invalid_option, should_fail=.true.), &
            new_unittest("fs_run_valid_command", fs_run_valid_command), &
            new_unittest("fs_list_dir_empty", fs_list_dir_empty), &
            new_unittest("fs_list_dir_one_file", fs_list_dir_one_file), &
            new_unittest("fs_list_dir_two_files", fs_list_dir_two_files), &
            new_unittest("fs_list_dir_one_file_one_dir", fs_list_dir_one_file_one_dir), &
            new_unittest("fs_rmdir_empty", fs_rmdir_empty), &
            new_unittest("fs_rmdir_with_contents", fs_rmdir_with_contents) &
            ]
    end subroutine

    subroutine fs_is_windows(error)
        type(error_type), allocatable, intent(out) :: error

        character(len=255) :: value
        integer :: length, stat

        call get_environment_variable('HOMEDRIVE', value, length, stat)
        if (is_windows) then
            call check(error, stat == 0 .and. length > 0, "Windows should be detected.")
        else
            call check(error, stat /= 0 .and. length == 0, "Windows should not be detected.")
        end if
    end subroutine

    subroutine fs_file_not_exists(error)
        type(error_type), allocatable, intent(out) :: error

        logical :: is_existing

        is_existing = exists("nonexistent")
        call check(error, is_existing, "Non-existent file should fail.")
    end subroutine

    subroutine fs_file_exists(error)
        type(error_type), allocatable, intent(out) :: error

        logical :: is_existing
        integer :: unit
        character(*), parameter :: filename = "file.tmp"

        open(newunit=unit, file=filename)
        close(unit)

        is_existing = exists(filename)
        call check(error, is_existing, "An existing file should not fail.")
        call delete_file(filename)
    end subroutine

    subroutine fs_current_dir_exists(error)
        type(error_type), allocatable, intent(out) :: error

        logical :: is_existing

        is_existing = exists(".")
        call check(error, is_existing, "Current directory should not fail.")
    end subroutine

    subroutine fs_path_separator(error)
        type(error_type), allocatable, intent(out) :: error

        character(*), parameter :: outer_dir = "path_separator_outer"
        character(*), parameter :: inner_dir = "path_separator_inner"

        call rmdir(outer_dir)
        call check(error, .not. exists(outer_dir), "Directory should not exist.")
        call mkdir(outer_dir)
        call check(error, exists(outer_dir), "Outer directory should now exist.")
        call mkdir(outer_dir//path_separator//inner_dir)
        call check(error, exists(outer_dir//path_separator//inner_dir), "Inner directory should now exist.")
        call rmdir(outer_dir)
    end subroutine

    subroutine fs_run_invalid_command(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call run("invalid_command", iostat=stat)
        call check(error, stat, "Running an invalid command should fail.")
    end subroutine

    subroutine fs_run_with_invalid_option(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call run("whoami -X", iostat=stat)
        call check(error, stat, "Running a valid command with an invalid option should fail.")
    end subroutine

    subroutine fs_run_valid_command(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call run("whoami", iostat=stat)
        call check(error, stat, "Running a valid command should not fail.")
    end subroutine

    subroutine fs_list_dir_empty(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat
        type(string_type), allocatable :: files(:)

        call rmdir(temp_list_dir)
        call mkdir(temp_list_dir, stat)
        if (stat /= 0) then
            call test_failed(error, "Creating directory '"//temp_list_dir//"' failed."); return
        end if

        call list_dir(temp_list_dir, files, stat)
        call check(error, stat, "Listing the contents of an empty directory shouldn't fail.")
        call check(error, size(files) == 0, "The directory should be empty.")

        call rmdir(temp_list_dir)
    end subroutine

    subroutine fs_list_dir_one_file(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        type(string_type), allocatable :: files(:)
        character(*), parameter :: filename = 'abc.txt'

        call rmdir(temp_list_dir)
        call mkdir(temp_list_dir, stat)
        if (stat /= 0) then
            call test_failed(error, "Creating directory '"//temp_list_dir//"' failed."); return
        end if

        call run('touch '//temp_list_dir//'/'//filename, iostat=stat)
        if (stat /= 0) then
            call test_failed(error, "Creating file'"//filename//"' in directory '"//temp_list_dir//"' failed."); return
        end if

        call list_dir(temp_list_dir, files, stat)
        call check(error, stat, "Listing the contents of an empty directory shouldn't fail.")
        call check(error, size(files) == 1, "The directory should contain one file.")
        call check(error, char(files(1)) == filename, "The file should be '"//filename//"'.")

        call rmdir(temp_list_dir)
    end subroutine

    subroutine fs_list_dir_two_files(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        type(string_type), allocatable :: files(:)
        character(*), parameter :: filename1 = 'abc.txt'
        character(*), parameter :: filename2 = 'xyz'

        call rmdir(temp_list_dir)
        call mkdir(temp_list_dir, stat)
        if (stat /= 0) then
            call test_failed(error, "Creating directory '"//temp_list_dir//"' failed."); return
        end if

        call run('touch '//temp_list_dir//'/'//filename1, iostat=stat)
        if (stat /= 0) then
            call test_failed(error, "Creating file 1 in directory '"//temp_list_dir//"' failed."); return
        end if

        call run('touch '//temp_list_dir//'/'//filename2, iostat=stat)
        if (stat /= 0) then
            call test_failed(error, "Creating file 2 in directory '"//temp_list_dir//"' failed."); return
        end if

        call list_dir(temp_list_dir, files, stat)
        call check(error, stat, "Listing the contents of an empty directory shouldn't fail.")
        call check(error, size(files) == 2, "The directory should contain two files.")
        call check(error, char(files(1)) == filename1, "The file should be '"//filename1//"'.")
        call check(error, char(files(2)) == filename2, "The file should be '"//filename2//"'.")

        call rmdir(temp_list_dir)
    end subroutine

    subroutine fs_list_dir_one_file_one_dir(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        type(string_type), allocatable :: contents(:)
        character(*), parameter :: filename1 = 'abc.txt'
        character(*), parameter :: dir = 'xyz'

        call rmdir(temp_list_dir)
        call mkdir(temp_list_dir, stat)
        if (stat /= 0) then
            call test_failed(error, "Creating directory '"//temp_list_dir//"' failed."); return
        end if

        call run('touch '//temp_list_dir//'/'//filename1, iostat=stat)
        if (stat /= 0) then
            call test_failed(error, "Creating file 1 in directory '"//temp_list_dir//"' failed."); return
        end if

        if (is_windows) then
            call mkdir(temp_list_dir//'\'//dir, stat)
        else
            call mkdir(temp_list_dir//'/'//dir, stat)
        end if
        if (stat /= 0) then
            call test_failed(error, "Creating dir in directory '"//temp_list_dir//"' failed."); return
        end if

        call list_dir(temp_list_dir, contents, stat)
        call check(error, stat, "Listing the contents of an empty directory shouldn't fail.")
        call check(error, size(contents) == 2, "The directory should contain two files.")
        call check(error, char(contents(1)) == filename1, "The file should be '"//filename1//"'.")
        call check(error, char(contents(2)) == dir, "The file should be '"//dir//"'.")

        call rmdir(temp_list_dir)
    end subroutine

    subroutine fs_rmdir_empty(error)
        type(error_type), allocatable, intent(out) :: error

        character(*), parameter :: dir = "empty_dir_to_remove"

        call rmdir(dir)
        call check(error, .not. exists(dir), "Directory should not exist.")
        call mkdir(dir)
        call check(error, exists(dir), "Directory should exist.")
        call rmdir(dir)
        call check(error, .not. exists(dir), "Directory should not exist.")
    end subroutine

    subroutine fs_rmdir_with_contents(error)
        type(error_type), allocatable, intent(out) :: error

        character(*), parameter :: dir = "dir_with_contents_to_remove"

        call rmdir(dir)
        call check(error, .not. exists(dir), "Directory should not exist.")
        call mkdir(dir)
        call check(error, exists(dir), "Directory should exist.")
        if (is_windows) then
            call mkdir(dir//'\'//'another_dir')
        else
            call mkdir(dir//'/'//'another_dir')
        end if
        call rmdir(dir)
        call check(error, .not. exists(dir), "Directory should not exist.")
    end subroutine

    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename

        integer :: io

        open(newunit=io, file=filename)
        close(io, status="delete")
    end subroutine
end module

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_filesystem, only : collect_filesystem
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("filesystem", collect_filesystem) &
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
