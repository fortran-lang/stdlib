module test_os
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_system, only: get_runtime_os, OS_WINDOWS, OS_UNKNOWN, OS_TYPE, is_windows, null_device, &
                             set_environment_variable, delete_environment_variable
    use stdlib_error, only: state_type

    implicit none

contains

    !> Collect all exported unit tests
    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('test_get_runtime_os', test_get_runtime_os), &
            new_unittest('test_is_windows', test_is_windows), &
            new_unittest('test_null_device', test_null_device), &
            new_unittest('test_environment_variable', test_environment_variable), &
            new_unittest('test_environment_variable_overwrite', test_environment_variable_overwrite), &
            new_unittest('test_environment_variable_invalid_name', test_environment_variable_invalid_name) &
        ]
    end subroutine collect_suite

    subroutine test_get_runtime_os(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: os

        !> Get current OS
        os = get_runtime_os()

        call check(error, os /= OS_UNKNOWN, "running on an unknown/unsupported OS")
        
    end subroutine test_get_runtime_os

    !> If running on Windows (_WIN32 macro is defined), test that the appropriate OS flag is returned
    subroutine test_is_windows(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: os_cached, os_runtime
        
        call check(error, OS_TYPE()==OS_WINDOWS .eqv. is_windows(), &
                   "Cached OS type does not match _WIN32 macro presence")

    end subroutine test_is_windows

    !> Test that the null_device is valid by writing something to it
    subroutine test_null_device(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: unit, ios
        character(len=512) :: iomsg

        ! Try opening the null device for writing
        open(newunit=unit, file=null_device(), status='old', action='write', iostat=ios, iomsg=iomsg)        
        call check(error, ios==0, 'Cannot open null_device unit: '//trim(iomsg))
        if (allocated(error)) return
        
        write(unit, *, iostat=ios, iomsg=iomsg) 'Hello, World!' 
        call check(error, ios==0, 'Cannot write to null_device unit: '//trim(iomsg))
        if (allocated(error)) return        

        close(unit, iostat=ios, iomsg=iomsg)
        call check(error, ios==0, 'Cannot close null_device unit: '//trim(iomsg))
        if (allocated(error)) return     
        
    end subroutine test_null_device


    !> Setting a variable makes it readable, and deleting it makes it unset again.
    subroutine test_environment_variable(error)
        type(error_type), allocatable, intent(out) :: error
        character(*), parameter :: name = 'STDLIB_TEST_ENVIRONMENT_VARIABLE'
        type(state_type) :: err
        character(len=32) :: value
        integer :: length, stat

        call set_environment_variable(name, 'stdlib', err=err)
        call check(error, err%ok(), 'setting a variable failed: '//err%print())
        if (allocated(error)) return

        call get_environment_variable(name, value, length, stat)
        call check(error, stat == 0, 'the variable is not readable after being set')
        if (allocated(error)) return
        call check(error, value(:length) == 'stdlib', 'read back '//value(:length)//', expected stdlib')
        if (allocated(error)) return

        call delete_environment_variable(name, err=err)
        call check(error, err%ok(), 'deleting a variable failed: '//err%print())
        if (allocated(error)) return

        call get_environment_variable(name, value, length, stat)
        call check(error, stat /= 0, 'the variable is still set after being deleted')
        if (allocated(error)) return

        ! Deleting one that was never set is not an error, matching unsetenv.
        call delete_environment_variable(name, err=err)
        call check(error, err%ok(), 'deleting an unset variable reported an error: '//err%print())

    end subroutine test_environment_variable

    !> `overwrite` decides whether an existing variable is replaced.
    subroutine test_environment_variable_overwrite(error)
        type(error_type), allocatable, intent(out) :: error
        character(*), parameter :: name = 'STDLIB_TEST_ENVIRONMENT_OVERWRITE'
        type(state_type) :: err
        character(len=32) :: value
        integer :: length, stat

        call set_environment_variable(name, 'first', err=err)
        call check(error, err%ok(), 'setting a variable failed: '//err%print())
        if (allocated(error)) return

        call set_environment_variable(name, 'second', overwrite=.true., err=err)
        call check(error, err%ok(), 'overwriting a variable failed: '//err%print())
        if (allocated(error)) return
        call get_environment_variable(name, value, length, stat)
        call check(error, value(:length) == 'second', 'overwrite=.true. left '//value(:length))
        if (allocated(error)) return

        ! Windows has no equivalent: `_putenv_s` always replaces, so the flag
        ! is documented as having no effect there and this half is skipped.
        if (is_windows()) then
            call skip_test(error, 'overwrite has no effect on Windows')
            return
        end if

        call set_environment_variable(name, 'third', overwrite=.false., err=err)
        call check(error, err%ok(), 'a refused overwrite should not be an error: '//err%print())
        if (allocated(error)) return
        call get_environment_variable(name, value, length, stat)
        call check(error, value(:length) == 'second', 'overwrite=.false. changed the value to '//value(:length))
        if (allocated(error)) return

        call delete_environment_variable(name)

    end subroutine test_environment_variable_overwrite

    !> A name that cannot be a variable is refused before reaching the OS.
    subroutine test_environment_variable_invalid_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(state_type) :: err

        call set_environment_variable('', 'value', err=err)
        call check(error, .not. err%ok(), 'an empty name was accepted')
        if (allocated(error)) return

        call set_environment_variable('HAS=EQUALS', 'value', err=err)
        call check(error, .not. err%ok(), 'a name containing "=" was accepted')
        if (allocated(error)) return

        call delete_environment_variable('', err=err)
        call check(error, .not. err%ok(), 'an empty name was accepted by delete')
        if (allocated(error)) return

        call delete_environment_variable('HAS=EQUALS', err=err)
        call check(error, .not. err%ok(), 'a name containing "=" was accepted by delete')

    end subroutine test_environment_variable_invalid_name

end module test_os

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_os, only : collect_suite

    implicit none

    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("os", collect_suite) &
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
