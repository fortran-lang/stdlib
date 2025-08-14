module test_filesystem
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_system, only: is_directory, delete_file, FS_ERROR, FS_ERROR_CODE, &
        make_directory, remove_directory, make_directory_all, is_windows, OS_TYPE, &
        OS_WINDOWS, get_cwd, set_cwd, operator(/)
    use stdlib_error, only: state_type, STDLIB_FS_ERROR

    implicit none

contains

    !> Collect all exported unit tests
    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("fs_error", test_fs_error), &
            new_unittest("fs_is_directory_dir", test_is_directory_dir), &
            new_unittest("fs_is_directory_file", test_is_directory_file), &
            new_unittest("fs_delete_non_existent", test_delete_file_non_existent), &
            new_unittest("fs_delete_existing_file", test_delete_file_existing), &
            new_unittest("fs_delete_file_being_dir", test_delete_directory), &            
            new_unittest("fs_make_dir", test_make_directory), &            
            new_unittest("fs_make_dir_existing_dir", test_make_directory_existing), &            
            new_unittest("fs_make_dir_all", test_make_directory_all), &            
            new_unittest("fs_remove_dir", test_remove_directory), &            
            new_unittest("fs_remove_dir_non_existent", test_remove_directory_nonexistent), &            
            new_unittest("fs_cwd", test_cwd) &            
        ]
    end subroutine collect_suite

    subroutine test_fs_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(state_type) :: s1, s2
        character(:), allocatable :: msg

        msg = "code - 10, Cannot create File temp.txt - File already exists"
        s1 = FS_ERROR_CODE(10, "Cannot create File temp.txt -", "File already exists")

        call check(error, s1%state == STDLIB_FS_ERROR .and. s1%message == msg, &
            "FS_ERROR_CODE: Could not construct the state with code correctly")
        if (allocated(error)) return

        msg = "Cannot create File temp.txt - File already exists"
        s2 = FS_ERROR("Cannot create File temp.txt -", "File already exists")

        call check(error, s2%state == STDLIB_FS_ERROR .and. s2%message == msg, &
            "FS_ERROR: Could not construct state without code correctly")
        if (allocated(error)) return
    end subroutine test_fs_error

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

    subroutine test_delete_file_non_existent(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(state_type) :: state

        ! Attempt to delete a file that doesn't exist
        call delete_file('non_existent_file.txt', state)

        call check(error, state%ok(), 'Error should not be triggered for non-existent file')
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
        call execute_command_line('mkdir ' // filename, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        call check(error, ios==0 .and. iocmd==0, 'Cannot init delete_directory test: '//trim(msg))
        if (allocated(error)) return

        ! Attempt to delete a directory (which should fail)
        call delete_file(filename, state)

        ! Check that an error was raised since the target is a directory
        call check(error, state%error(), 'Error was not triggered trying to delete directory')
        if (allocated(error)) return

        ! Clean up: remove the empty directory
        call execute_command_line('rmdir ' // filename, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        call check(error, ios==0 .and. iocmd==0, 'Cannot cleanup delete_directory test: '//trim(msg))
        if (allocated(error)) return        

    end subroutine test_delete_directory
    
    subroutine test_make_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(state_type) :: err
        character(len=256) :: dir_name
        integer :: ios,iocmd
        character(len=512) :: msg

        dir_name = "test_directory"

        call make_directory(dir_name, err=err)
        call check(error, err%ok(), 'Could not make directory: '//err%print())
        if (allocated(error)) return

        ! clean up: remove the empty directory
        call execute_command_line('rmdir ' // dir_name, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        call check(error, ios==0 .and. iocmd==0, 'Cannot cleanup make_directory test: '//trim(msg))
    end subroutine test_make_directory

    subroutine test_make_directory_existing(error)
        type(error_type), allocatable, intent(out) :: error
        type(state_type) :: err
        character(len=256) :: dir_name
        integer :: ios,iocmd
        character(len=512) :: msg

        dir_name = "test_directory"

        call execute_command_line('mkdir ' // dir_name, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        call check(error, ios==0 .and. iocmd==0, 'Cannot init make_directory_existing test: '//trim(msg))
        if (allocated(error)) return

        call make_directory(dir_name, err=err)
        call check(error, err%error(), 'Made an already existing directory somehow')

        ! clean up: remove the empty directory
        call execute_command_line('rmdir ' // dir_name, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)

        if (allocated(error)) then
            ! if previous error is allocated as well
            call check(error, ios==0 .and. iocmd==0, error%message // ' and cannot cleanup make_directory test: '//trim(msg))
            return
        end if

        call check(error, ios==0 .and. iocmd==0, 'Cannot cleanup make_directory test: '//trim(msg))
    end subroutine test_make_directory_existing

    subroutine test_make_directory_all(error)
        type(error_type), allocatable, intent(out) :: error
        type(state_type) :: err
        character(len=256) :: dir_name
        integer :: ios,iocmd
        character(len=512) :: msg

        if (OS_TYPE() == OS_WINDOWS) then
            dir_name = "d1\d2\d3\d4\"
        else
            dir_name = "d1/d2/d3/d4/"
        end if

        call make_directory_all(dir_name, err=err)
        call check(error, err%ok(), 'Could not make all directories: '//err%print())
        if (allocated(error)) return

        ! clean up: remove the empty directory
        if (is_windows()) then
            call execute_command_line('rmdir /s /q d1', exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        else
            call execute_command_line('rm -rf d1', exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        end if

        call check(error, ios==0 .and. iocmd==0, 'Cannot cleanup make_directory_all test: '//trim(msg))
    end subroutine test_make_directory_all

    subroutine test_remove_directory(error)
        type(error_type), allocatable, intent(out) :: error
        type(state_type) :: err
        character(len=256) :: dir_name
        integer :: ios,iocmd
        character(len=512) :: msg

        dir_name = "test_directory"

        call execute_command_line('mkdir ' // dir_name, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        call check(error, ios==0 .and. iocmd==0, 'Cannot init remove_directory test: '//trim(msg))
        if (allocated(error)) return

        call remove_directory(dir_name, err)
        call check(error, err%ok(), 'Could not remove directory: '//err%print())

        if (allocated(error)) then 
            ! clean up: remove the empty directory
            call execute_command_line('rmdir ' // dir_name, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
            call check(error, ios==0 .and. iocmd==0, error%message // ' and cannot cleanup make_directory test: '//trim(msg))
        end if
    end subroutine test_remove_directory

    subroutine test_remove_directory_nonexistent(error)
        type(error_type), allocatable, intent(out) :: error
        type(state_type) :: err

        call remove_directory("random_name", err)
        call check(error, err%error(), 'Somehow removed a non-existent directory')
        if (allocated(error)) return
    end subroutine test_remove_directory_nonexistent

    subroutine test_cwd(error)
        type(error_type), allocatable, intent(out) :: error
        type(state_type) :: err
        character(len=256) :: dir_name
        integer :: ios,iocmd
        character(len=512) :: msg

        character(:), allocatable :: pwd1, pwd2, abs_dir_name

        ! get the initial cwd
        call get_cwd(pwd1, err)
        call check(error, err%ok(), 'Could not get current working directory: '//err%print())
        if (allocated(error)) return

        ! create a temporary directory for use by `set_cwd`
        dir_name = "test_directory"

        call execute_command_line('mkdir ' // dir_name, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        call check(error, ios==0 .and. iocmd==0, 'Cannot init cwd test: '//trim(msg))
        if (allocated(error)) return

        abs_dir_name = pwd1 / dir_name
        call set_cwd(abs_dir_name, err)
        call check(error, err%ok(), 'Could not set current working directory: '//err%print())
        if (allocated(error)) return

        ! get the new cwd -> should be same as (pwd1 / dir_name)
        call get_cwd(pwd2, err)
        call check(error, err%ok(), 'Could not get current working directory: '//err%print())
        if (allocated(error)) return

        call check(error, pwd2 == abs_dir_name, 'Working directory is wrong, & 
            & expected: '//abs_dir_name//" got: "//pwd2)
        if (allocated(error)) return

        ! cleanup: set the cwd back to the initial value
        call set_cwd(pwd1, err)
        call check(error, err%ok(), 'Could not clean up cwd test, could not set the cwd back: '//err%print())
        if (allocated(error)) then 
            ! our cwd now is `./test_directory`
            ! there is no way of removing the empty test directory
            return
        end if

        ! cleanup: remove the empty directory
        call execute_command_line('rmdir ' // dir_name, exitstat=ios, cmdstat=iocmd, cmdmsg=msg)
        call check(error, ios==0 .and. iocmd==0, 'Cannot cleanup cwd test, cannot remove empty dir: '//trim(msg))
        if (allocated(error)) return
    end subroutine test_cwd

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
