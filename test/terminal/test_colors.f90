! SPDX-Identifier: MIT

module test_colors
    use stdlib_ansi, only : fg_color_red, bg_color_yellow, style_bold, to_string
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none

contains

    !> Collect all exported unit tests
    subroutine collect_colors(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("fg_color", test_fg_color), &
            new_unittest("bg_color", test_bg_color), &
            new_unittest("style", test_style) &
            ]
    end subroutine collect_colors

    subroutine test_fg_color(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: str

        str = to_string(fg_color_red)
        call check(error, iachar(str(1:1)), 27)
        if (allocated(error)) return
        call check(error, str(2:), "[0;31m")
    end subroutine test_fg_color

    subroutine test_bg_color(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: str

        str = to_string(bg_color_yellow)
        call check(error, iachar(str(1:1)), 27)
        if (allocated(error)) return
        call check(error, str(2:), "[0;43m")
    end subroutine test_bg_color

    subroutine test_style(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: str

        str = to_string(style_bold)
        call check(error, iachar(str(1:1)), 27)
        if (allocated(error)) return
        call check(error, str(2:), "[0;1m")
    end subroutine test_style

end module test_colors


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_colors, only : collect_colors
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("colors", collect_colors) &
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
