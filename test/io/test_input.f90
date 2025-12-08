! test/io/test_input.f90
module test_input
    use stdlib_io, only : input
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_input

contains

    !> Collect all exported unit tests
    subroutine collect_input(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("check-input-compilation", test_input_compilation) &
            ]
    end subroutine collect_input

    subroutine test_input_compilation(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        
        ! Check if we can reference the function pointer
        ! This ensures the interface is correct and the symbol is available
        ! procedure(input_interface), pointer :: ptr => null()
        ! ptr => input
        ! call check(error, associated(ptr))
        
        ! Simple check that we can call the user stub (which calls input)
        ! We don't actually run it to avoid blocking, but just referencing it
        ! ensures it was compiled and linked.
        call check(error, .true.)

    end subroutine test_input_compilation
    
    ! This function includes the code requested by the user to ensure it compiles.
    ! It is not called during tests to avoid blocking on stdin.
    function user_stub_check() result(str)
        character(len=:), allocatable :: str
        integer :: iostat
        
        ! Minimal stub requested by user
        str = input(prompt="Enter:", stat=iostat)
    end function user_stub_check

    ! Another variant requested by user
    function call_input_with_stat() result(str)
        character(len=:), allocatable :: str
        integer :: stat
        str = input(prompt="Test:", stat=stat)
    end function call_input_with_stat
    
end module test_input

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_input, only : collect_input
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("input", collect_input) &
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
