module test_filesystem
    use stdlib_filesystem
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_filesystem

contains

    !> Collect all exported unit tests
    subroutine collect_filesystem(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("fs_file_not_exists", fs_file_not_exists, should_fail=.true.), &
            new_unittest("fs_file_exists", fs_file_exists), &
            new_unittest("fs_current_dir_exists", fs_current_dir_exists), &
            new_unittest("fs_run_invalid_command", fs_run_invalid_command, should_fail=.true.), &
            new_unittest("fs_run_with_invalid_option", fs_run_with_invalid_option, should_fail=.true.), &
            new_unittest("fs_run_valid_command", fs_run_valid_command) &
            ]
    end

    subroutine fs_file_not_exists(error)
        type(error_type), allocatable, intent(out) :: error

        logical :: is_existing

        is_existing = exists("nonexistent")
        call check(error, is_existing, "Non-existent file should fail.")
    end

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
    end

    subroutine fs_current_dir_exists(error)
        type(error_type), allocatable, intent(out) :: error

        logical :: is_existing

        is_existing = exists(".")
        call check(error, is_existing, "Current directory should not fail.")
    end

    subroutine fs_run_invalid_command(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call run("invalid_command", stat=stat)
        call check(error, stat, "Running an invalid command should fail.")
    end

    subroutine fs_run_with_invalid_option(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call run("whoami -X", stat=stat)
        call check(error, stat, "Running a valid command with an invalid option should fail.")
    end

    subroutine fs_run_valid_command(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: stat

        call run("whoami", stat=stat)
        call check(error, stat, "Running a valid command should not fail.")
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
end
