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

        allocate(testsuite(0))

    end subroutine collect_filesystem

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
