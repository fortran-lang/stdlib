! SPDX-Identifier: MIT
module test_string_to_string
    
    use stdlib_strings, only: to_string, starts_with
    use stdlib_error, only: check
    use stdlib_optval, only: optval
    implicit none

contains

    subroutine check_formatter(actual, expected, description, partial)
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

        call check(stat, msg)

    end subroutine check_formatter

    subroutine test_to_string_complex
        call check_formatter(to_string((1, 1)), "(1.0", &
                & "Default formatter for complex number", partial=.true.)
        call check_formatter(to_string((1, 1), '(F6.2)'), "(  1.00,  1.00)", &
                & "Formatter for complex number")
        call check_formatter(to_string((-1, -1), 'F6.2'), "( -1.00, -1.00)", &
                & "Formatter for negative complex number")
        call check_formatter(to_string((1, 1), 'SP,F6.2'), "( +1.00, +1.00)", &
                & "Formatter with sign control descriptor for complex number")
        call check_formatter(to_string((1, 1), 'F6.2') // to_string((2, 2), '(F7.3)'), &
                & "(  1.00,  1.00)(  2.000,  2.000)", &
                & "Multiple formatters for complex numbers")

    end subroutine test_to_string_complex

    subroutine test_to_string_integer
        call check_formatter(to_string(100), "100", &
                & "Default formatter for integer number")
        call check_formatter(to_string(100, 'I6'), "   100", &
                & "Formatter for integer number")
        call check_formatter(to_string(100, 'I0.6'), "000100", &
                & "Formatter with zero padding for integer number")
        call check_formatter(to_string(100, 'I6') // to_string(1000, '(I7)'), &
                & "   100   1000", "Multiple formatters for integers")
        call check_formatter(to_string(34, 'B8'), "  100010", &
                & "Binary formatter for integer number")
        call check_formatter(to_string(34, 'O0.3'), "042", &
                & "Octal formatter with zero padding for integer number")
        call check_formatter(to_string(34, 'Z3'), " 22", &
                & "Hexadecimal formatter for integer number")

    end subroutine test_to_string_integer

    subroutine test_to_string_real
        call check_formatter(to_string(100.), "100.0", &
                & "Default formatter for real number", partial=.true.)
        call check_formatter(to_string(100., 'F6.2'), "100.00", &
                & "Formatter for real number")
        call check_formatter(to_string(289., 'E7.2'), ".29E+03", &
                & "Exponential formatter with rounding for real number")
        call check_formatter(to_string(128., 'ES8.2'), "1.28E+02", &
                & "Exponential formatter for real number")
    
      ! Wrong demonstration
        call check_formatter(to_string(-100., 'F6.2'), "*", &
                & "Too narrow formatter for signed real number", partial=.true.)
        call check_formatter(to_string(1000., 'F6.3'), "*", &
                & "Too narrow formatter for real number", partial=.true.)
        call check_formatter(to_string(1000., '7.3'), "[*]", &
                & "Invalid formatter for real number", partial=.true.)

    end subroutine test_to_string_real

    subroutine test_to_string_logical
        call check_formatter(to_string(.true.), "T", &
                & "Default formatter for logcal value")
        call check_formatter(to_string(.true., 'L2'), " T", &
                & "Formatter for logical value")
        call check_formatter(to_string(.false., 'L2') // to_string(.true., '(L5)'), &
                & " F    T", "Multiple formatters for logical values")

      ! Wrong demonstration
        call check_formatter(to_string(.false., '1x'), "[*]", &
                & "Invalid formatter for logical value", partial=.true.)

    end subroutine test_to_string_logical


end module test_string_to_string

program tester
    use test_string_to_string
    implicit none

    call test_to_string_complex
    call test_to_string_integer
    call test_to_string_logical
    call test_to_string_real

end program tester
