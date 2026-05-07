! SPDX-Identifier: MIT
module test_string_to_string

    use stdlib_strings, only: to_string, to_c_char, starts_with
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_optval, only: optval
    implicit none

contains


    !> Collect all exported unit tests
    subroutine collect_string_to_string(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("to_string-complex", test_to_string_complex), &
            new_unittest("to_string-integer", test_to_string_integer), &
            new_unittest("to_string-logical", test_to_string_logical), &
            new_unittest("to_string-real", test_to_string_real), &
            new_unittest("to_string-limit-i1", test_string_i1), &
            new_unittest("to_string-limit-i2", test_string_i2), &
            new_unittest("to_string-limit-i4", test_string_i4), &
            new_unittest("to_string-limit-i8", test_string_i8), &
            new_unittest("to_c_char", test_to_c_char) &
            ]
    end subroutine collect_string_to_string

    subroutine check_formatter(error, actual, expected, description, partial)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: actual, expected, description
        logical, intent(in), optional :: partial
        logical :: stat
        character(len=:), allocatable :: msg

        if (optval(partial, .false.)) then
            stat = starts_with(actual, expected)
        else
            stat = actual == expected
        end if

        if (.not. stat) then
            msg = description // new_line("a") // &
                & "Expected: '" // expected // "' but got '" // actual // "'"
        else
            print '(" - ", a, /, "   Result: ''", a, "''")', description, actual
        end if

        call check(error, stat, msg)

    end subroutine check_formatter

    subroutine test_to_string_complex(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check_formatter(error, to_string((1, 1)), "(1.0", &
            & "Default formatter for complex number", partial=.true.)
        if (allocated(error)) return
        call check_formatter(error, to_string((1, 1), '(F6.2)'), "(  1.00,  1.00)", &
            & "Formatter for complex number")
        if (allocated(error)) return
        call check_formatter(error, to_string((-1, -1), 'F6.2'), "( -1.00, -1.00)", &
            & "Formatter for negative complex number")
        if (allocated(error)) return
        call check_formatter(error, to_string((1, 1), 'SP,F6.2'), "( +1.00, +1.00)", &
            & "Formatter with sign control descriptor for complex number")
        if (allocated(error)) return
        call check_formatter(error, to_string((1, 1), 'F6.2') // to_string((2, 2), '(F7.3)'), &
            & "(  1.00,  1.00)(  2.000,  2.000)", &
            & "Multiple formatters for complex numbers")

    end subroutine test_to_string_complex

    subroutine test_to_string_integer(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check_formatter(error, to_string(100), "100", &
            & "Default formatter for integer number")
        if (allocated(error)) return
        call check_formatter(error, to_string(100, 'I6'), "   100", &
            & "Formatter for integer number")
        if (allocated(error)) return
        call check_formatter(error, to_string(100, 'I0.6'), "000100", &
            & "Formatter with zero padding for integer number")
        if (allocated(error)) return
        call check_formatter(error, to_string(100, 'I6') // to_string(1000, '(I7)'), &
            & "   100   1000", "Multiple formatters for integers")
        if (allocated(error)) return
        call check_formatter(error, to_string(34, 'B8'), "  100010", &
            & "Binary formatter for integer number")
        if (allocated(error)) return
        call check_formatter(error, to_string(34, 'O0.3'), "042", &
            & "Octal formatter with zero padding for integer number")
        if (allocated(error)) return
        call check_formatter(error, to_string(34, 'Z3'), " 22", &
            & "Hexadecimal formatter for integer number")

    end subroutine test_to_string_integer

    subroutine test_to_string_real(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check_formatter(error, to_string(100.), "100.0", &
            & "Default formatter for real number", partial=.true.)
        if (allocated(error)) return
        call check_formatter(error, to_string(100., 'F6.2'), "100.00", &
            & "Formatter for real number")
        if (allocated(error)) return
        call check_formatter(error, to_string(289., 'E7.2'), ".29E+03", &
            & "Exponential formatter with rounding for real number")
        if (allocated(error)) return
        call check_formatter(error, to_string(128., 'ES8.2'), "1.28E+02", &
            & "Exponential formatter for real number")
        if (allocated(error)) return

        ! Wrong demonstration
        call check_formatter(error, to_string(-100., 'F6.2'), "*", &
            & "Too narrow formatter for signed real number", partial=.true.)
        if (allocated(error)) return
        call check_formatter(error, to_string(1000., 'F6.3'), "*", &
            & "Too narrow formatter for real number", partial=.true.)
        if (allocated(error)) return
        call check_formatter(error, to_string(1000., '7.3'), "[*]", &
            & "Invalid formatter for real number", partial=.true.)
        if (allocated(error)) return

    end subroutine test_to_string_real

    subroutine test_to_string_logical(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check_formatter(error, to_string(.true.), "T", &
            & "Default formatter for logcal value")
        if (allocated(error)) return
        call check_formatter(error, to_string(.true., 'L2'), " T", &
            & "Formatter for logical value")
        if (allocated(error)) return
        call check_formatter(error, to_string(.false., 'L2') // to_string(.true., '(L5)'), &
            & " F    T", "Multiple formatters for logical values")
        if (allocated(error)) return

        ! Wrong demonstration
        call check_formatter(error, to_string(.false., '1x'), "[*]", &
            & "Invalid formatter for logical value", partial=.true.)

    end subroutine test_to_string_logical

    subroutine test_to_c_char(error)
        use stdlib_kinds, only : c_char
        use stdlib_string_type, only: string_type, len, char
        use iso_c_binding, only: c_size_t
        
        !> Error handling
        type(error_type), allocatable, intent(out) :: error        
        
        !> Interface to C standard library 
        interface 
            integer(c_size_t) function c_strlen(cstr) bind(C, name="strlen") result(len)
               import :: c_char, c_size_t
               character(kind=c_char), intent(in) :: cstr(*)
            end function c_strlen
        end interface
        
        type(string_type) :: shello
        character(kind=c_char), allocatable :: cstr(:)
        character(*), parameter :: hello = "Hello, World!"
        integer :: i
          
        ! Convert character array
        cstr = to_c_char(hello)
        call check(error, len(hello)==c_strlen(cstr), 'to_c_char_from_char: invalid C length')
        if (allocated(error)) return
        
        do i=1,len(hello)
            call check(error, hello(i:i)==cstr(i), 'to_c_char_from_char: character mismatch')
            if (allocated(error)) return
        end do
        
        ! Convert string type
        shello = string_type(hello)
        cstr = to_c_char(shello)
        call check(error, len(shello)==c_strlen(cstr), 'to_c_char_from_string: invalid C length')
        if (allocated(error)) return
        
        do i=1,len(shello)
            call check(error, char(shello,pos=i)==cstr(i), 'to_c_char_from_string: character mismatch')
            if (allocated(error)) return
        end do
        
    end subroutine test_to_c_char

    subroutine test_string_i1(error)
        use stdlib_kinds, only : i1 => int8

        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, to_string(-huge(1_i1) - 1_i1), "-128")
    end subroutine test_string_i1


    subroutine test_string_i2(error)
        use stdlib_kinds, only : i2 => int16

        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, to_string(-huge(1_i2) - 1_i2), "-32768")
    end subroutine test_string_i2


    subroutine test_string_i4(error)
        use stdlib_kinds, only : i4 => int32

        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, to_string(-huge(1_i4) - 1_i4), "-2147483648")
    end subroutine test_string_i4


    subroutine test_string_i8(error)
        use stdlib_kinds, only : i8 => int64

        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, to_string(-huge(1_i8) - 1_i8), "-9223372036854775808")
    end subroutine test_string_i8


end module test_string_to_string


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_string_to_string, only : collect_string_to_string
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("string-to_string", collect_string_to_string) &
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
