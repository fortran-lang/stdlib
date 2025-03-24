module test_filesystem
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_system, only: is_directory

    implicit none

contains

    !> Collect all exported unit tests
    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("fs_is_directory_dir", test_is_directory_dir), &
            new_unittest("fs_is_directory_file", test_is_directory_file) &
        ]
    end subroutine collect_suite

    ! Test `is_directory` for a directory
    subroutine test_is_directory_dir(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=256) :: dirname
        integer :: ios, iocmd
        character(len=512) :: msg

        dirname = "this_test_dir_tmp"

        ! Create a directory
        call execute_command_line("mkdir " // dirname, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        call check(error, ios == 0 .and. iocmd == 0, "Cannot create test directory: " // trim(msg))
        if (allocated(error)) return

        ! Verify `is_directory` identifies it as a directory
        call check(error, is_directory(dirname), "is_directory did not recognize a valid directory")
        if (allocated(error)) return

        ! Clean up: remove the directory
        call execute_command_line("rmdir " // dirname, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        call check(error, ios == 0 .and. iocmd == 0, "Cannot remove test directory: " // trim(msg))
    end subroutine test_is_directory_dir

    ! Test `is_directory` for a regular file
    subroutine test_is_directory_file(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=256) :: filename
        logical :: result
        integer :: ios, iunit
        character(len=512) :: msg

        filename = "test_file.txt"

        ! Create a file
        open(newunit=iunit, file=filename, status="replace", iostat=ios, iomsg=msg)
        call check(error, ios == 0, "Cannot create test file: " // trim(msg))
        if (allocated(error)) return        

        ! Verify `is_directory` identifies it as not a directory
        result = is_directory(filename)
        call check(error, .not. result, "is_directory falsely recognized a regular file as a directory")
        if (allocated(error)) return

        ! Clean up: remove the file
        close(iunit,status='delete',iostat=ios,iomsg=msg)
        call check(error, ios == 0, "Cannot delete test file: " // trim(msg))
        if (allocated(error)) return                

    end subroutine test_is_directory_file


end module test_filesystem

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_filesystem, only : collect_suite

    implicit none

    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("filesystem", collect_suite) &
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
