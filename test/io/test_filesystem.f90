module test_filesystem
    use stdlib_io_filesystem
    use stdlib_error, only: state_type    
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

        allocate(testsuite(0))

        testsuite = [ &
             new_unittest("fs_delete_non_existent", test_delete_file_non_existent), &
             new_unittest("fs_delete_existing_file", test_delete_file_existing), &
             new_unittest("fd_delete_file_being_dir", test_delete_directory) &
             ]

    end subroutine collect_filesystem

    subroutine test_delete_file_non_existent(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(state_type) :: state
        
        ! Attempt to delete a file that doesn't exist
        call delete_file('non_existent_file.txt', state)
        
        call check(error, state%error(), 'Error should be triggered for non-existent file')
        if (allocated(error)) return
        
    end subroutine test_delete_file_non_existent

    subroutine test_delete_file_existing(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        
        character(len=256) :: filename
        type(state_type) :: state
        integer :: ios,iunit
        logical :: is_present
        character(len=512) :: msg
        
        filename = 'existing_file.txt'
        
        ! Create a file to be deleted
        open(newunit=iunit, file=filename, status='replace', iostat=ios, iomsg=msg)
        call check(error, ios==0, 'Failed to create test file')
        if (allocated(error)) return
        close(iunit)

        ! Attempt to delete the existing file
        call delete_file(filename, state)
        
        ! Check deletion successful
        call check(error, state%ok(), 'delete_file returned '//state%print())
        if (allocated(error)) return
        
        ! Check if the file was successfully deleted (should no longer exist)
        inquire(file=filename, exist=is_present)
        
        call check(error, .not.is_present, 'File still present after delete')
        if (allocated(error)) return
        
    end subroutine test_delete_file_existing

    subroutine test_delete_directory(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(len=256) :: filename
        type(state_type) :: state
        integer :: ios,iocmd
        character(len=512) :: msg
        
        filename = 'test_directory'
        
        ! The directory is not nested: it should be cross-platform to just call `mkdir`
        print *, 'mkdir'
        call execute_command_line('mkdir ' // filename, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        call check(error, ios==0 .and. iocmd==0, 'Cannot init delete_directory test: '//trim(msg))
        if (allocated(error)) return
        
        ! Attempt to delete a directory (which should fail)
        print *, 'dfelete'
        call delete_file(filename, state)
        
        ! Check that an error was raised since the target is a directory
        call check(error, state%ok(), 'Error was not triggered trying to delete directory')
        if (allocated(error)) return

        ! Clean up: remove the empty directory
        print *, 'rmdir'
        call execute_command_line('rmdir ' // filename, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        call check(error, ios==0 .and. iocmd==0, 'Cannot cleanup delete_directory test: '//trim(msg))
        if (allocated(error)) return        
        
    end subroutine test_delete_directory

    ! Test `is_directory` for a directory
    subroutine test_is_directory_dir(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=256) :: dirname
        logical :: result
        integer :: ios, iocmd
        character(len=512) :: msg

        dirname = "test_dir"

        ! Create a directory
        call execute_command_line("mkdir " // dirname, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        call check(error, ios == 0 .and. iocmd == 0, "Cannot create test directory: " // trim(msg))
        if (allocated(error)) return

        ! Verify `is_directory` identifies it as a directory
        result = is_directory(dirname)
        call check(error, result, "is_directory did not recognize a valid directory")
        if (allocated(error)) return

        ! Clean up: remove the directory
        call execute_command_line("rmdir " // dirname, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        call check(error, ios == 0 .and. iocmd == 0, "Cannot remove test directory: " // trim(msg))
    end subroutine test_is_directory_dir

end module test_filesystem

program test_all_filesystem
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
end program test_all_filesystem
