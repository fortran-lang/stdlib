module test_io_disp

    use stdlib_strings, only: starts_with
    use stdlib_string_type, only: string_type, assignment(=)
    use stdlib_error, only: check
    use stdlib_io, only: disp
    use stdlib_optval, only: optval
    implicit none

    integer :: unit
    character(len=200) :: string

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
            msg = description//new_line("a")// &
                & "Expected: '"//expected//"' but got '"//actual//"'"
        else
            print '(" - ", a, /, "   Result: ''", a, "''")', description, actual
        end if

        call check(stat, msg)

    end subroutine check_formatter

    subroutine test_io_disp_complex
        complex :: c(6, 6) = (1.0, 1.0)

        open (newunit=unit, status='scratch')
        call disp(c(1, 1), header='Test_io_disp_complex_scalar (brief) : ', brief=.true.)
        call disp(c(1, 1), unit=unit, header='Test_io_disp_complex_scalar (brief) : ', brief=.true.)

        call disp(c(1, :), header='Test_io_disp_complex_vector (brief) : ', brief=.true.)
        call disp(c(1, :), unit=unit, header='Test_io_disp_complex_vector (brief) : ', brief=.true.)

        call disp(c(:, 1), header='Test_io_disp_complex_vector : ', brief=.false.)
        call disp(c(:, 1), unit=unit, header='Test_io_disp_complex_vector : ', brief=.false.)

        call disp(c(1:2, 1:2), header='Test_io_disp_complex_matrix : ', brief=.false.)
        call disp(c(1:2, 1:2), unit=unit, header='Test_io_disp_complex_matrix : ', brief=.false.)

        call disp(c(:, :), header='Test_io_disp_complex_matrix (brief) : ', brief=.true.)
        call disp(c(:, :), unit=unit, header='Test_io_disp_complex_matrix (brief) : ', brief=.true.)

        !! Checks
        rewind (unit)
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_complex_scalar (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '(1.000,1.000)', 'Value')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_complex_vector (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[vector size: 6]', 'Vector Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
        '(1.000,1.000)  (1.000,1.000)  (1.000,1.000)  ..             (1.000,1.000)', "Test_io_disp_complex_vector (brief)")

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_complex_vector :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[vector size: 6]', 'Vector Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
        '(1.000,1.000)  (1.000,1.000)  (1.000,1.000)  (1.000,1.000)  (1.000,1.000)       &', 'Vector')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '(1.000,1.000)', 'Vector')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_complex_matrix :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[matrix size: 2×2]', 'Matrix Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '(1.000,1.000)  (1.000,1.000)', 'Matrix Vector 1')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '(1.000,1.000)  (1.000,1.000)', 'Matrix Vector 2')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_complex_matrix (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[matrix size: 6×6]', 'Matrix Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
        '(1.000,1.000)  (1.000,1.000)  (1.000,1.000)  ..             (1.000,1.000)', 'Matrix Vector 1')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
        '(1.000,1.000)  (1.000,1.000)  (1.000,1.000)  ..             (1.000,1.000)', 'Matrix Vector 2')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
        '(1.000,1.000)  (1.000,1.000)  (1.000,1.000)  ..             (1.000,1.000)', 'Matrix Vector 3')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
        ':              :              :              :              :', 'Matrix Vector ..')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
        '(1.000,1.000)  (1.000,1.000)  (1.000,1.000)  ..             (1.000,1.000)', 'Matrix Vector Size(Matrix, 1)')
        close (unit)

    end subroutine test_io_disp_complex

    subroutine test_io_disp_real

        real :: r(6, 6) = 1.0

        open (newunit=unit, status='scratch')
        call disp(r(1, 1), header='Test_io_disp_real_scalar (brief) : ', brief=.true.)
        call disp(r(1, 1), unit=unit, header='Test_io_disp_real_scalar (brief) : ', brief=.true.)

        call disp(r(1, :), header='Test_io_disp_real_vector (brief) : ', brief=.true.)
        call disp(r(1, :), unit=unit, header='Test_io_disp_real_vector (brief) : ', brief=.true.)

        call disp(r(:, 1), header='Test_io_disp_real_vector : ', brief=.false.)
        call disp(r(:, 1), unit=unit, header='Test_io_disp_real_vector : ', brief=.false.)

        call disp(r(1:2, 1:2), header='Test_io_disp_real_matrix : ', brief=.false.)
        call disp(r(1:2, 1:2), unit=unit, header='Test_io_disp_real_matrix : ', brief=.false.)

        call disp(r(:, :), header='Test_io_disp_real_matrix (brief) : ', brief=.true.)
        call disp(r(:, :), unit=unit, header='Test_io_disp_real_matrix (brief) : ', brief=.true.)

        !! Checks
        rewind (unit)
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_real_scalar (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '1.000', 'Value')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_real_vector (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[vector size: 6]', 'Vector Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1.000  1.000  1.000  ..     1.000', 'Brief Vector')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_real_vector :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[vector size: 6]', 'Vector Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1.000  1.000  1.000  1.000  1.000  1.000', 'Brief Vector')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_real_matrix :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[matrix size: 2×2]', 'Matrix Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1.000  1.000', 'Matrix Vector 1')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1.000  1.000', 'Matrix Vector 2')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_real_matrix (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[matrix size: 6×6]', 'Matrix Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1.000  1.000  1.000  ..     1.000', 'Matrix Vector 1')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1.000  1.000  1.000  ..     1.000', 'Matrix Vector 2')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1.000  1.000  1.000  ..     1.000', 'Matrix Vector 3')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             ':      :      :      :      :', 'Matrix Vector ..')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1.000  1.000  1.000  ..     1.000', 'Matrix Vector Size(Matrix, 1)')
        close (unit)

    end subroutine test_io_disp_real

    subroutine test_io_disp_integer

        integer :: i(6, 6) = 1

        open (newunit=unit, status='scratch')
        call disp(i(1, 1), header='Test_io_disp_integer_scalar (brief) : ', brief=.true.)
        call disp(i(1, 1), unit=unit, header='Test_io_disp_integer_scalar (brief) : ', brief=.true.)

        call disp(i(1, :), header='Test_io_disp_integer_vector (brief) : ', brief=.true.)
        call disp(i(1, :), unit=unit, header='Test_io_disp_integer_vector (brief) : ', brief=.true.)

        call disp(i(:, 1), header='Test_io_disp_integer_vector : ', brief=.false.)
        call disp(i(:, 1), unit=unit, header='Test_io_disp_integer_vector : ', brief=.false.)

        call disp(i(1:2, 1:2), header='Test_io_disp_integer_matrix : ', brief=.false.)
        call disp(i(1:2, 1:2), unit=unit, header='Test_io_disp_integer_matrix : ', brief=.false.)

        call disp(i(:, :), header='Test_io_disp_integer_matrix (brief) : ', brief=.true.)
        call disp(i(:, :), unit=unit, header='Test_io_disp_integer_matrix (brief) : ', brief=.true.)

        !! Checks
        rewind (unit)
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_integer_scalar (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '1', 'Value')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_integer_vector (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[vector size: 6]', 'Vector Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1   1   1   ..  1', 'Brief Vector')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_integer_vector :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[vector size: 6]', 'Vector Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1   1   1   1   1   1', 'Brief Vector')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_integer_matrix :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[matrix size: 2×2]', 'Matrix Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1   1', 'Matrix Vector 1')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1   1', 'Matrix Vector 2')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_integer_matrix (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[matrix size: 6×6]', 'Matrix Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1   1   1   ..  1', 'Matrix Vector 1')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1   1   1   ..  1', 'Matrix Vector 2')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1   1   1   ..  1', 'Matrix Vector 3')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             ':   :   :   :   :', 'Matrix Vector ..')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             '1   1   1   ..  1', 'Matrix Vector Size(Matrix, 1)')
        close (unit)

    end subroutine test_io_disp_integer

    subroutine test_io_disp_logical

        logical :: l(6, 6) = .true.
        ! unit = open(filenanme, 'w+t')
        open (newunit=unit, status='scratch')
        call disp(l(1, 1), header='Test_io_disp_logical_scalar (brief) : ', brief=.true.)
        call disp(l(1, 1), unit=unit, header='Test_io_disp_logical_scalar (brief) : ', brief=.true.)

        call disp(l(1, :), header='Test_io_disp_logical_vector (brief) : ', brief=.true.)
        call disp(l(1, :), unit=unit, header='Test_io_disp_logical_vector (brief) : ', brief=.true.)

        call disp(l(:, 1), header='Test_io_disp_logical_vector : ', brief=.false.)
        call disp(l(:, 1), unit=unit, header='Test_io_disp_logical_vector : ', brief=.false.)

        call disp(l(1:2, 1:2), header='Test_io_disp_logical_matrix : ', brief=.false.)
        call disp(l(1:2, 1:2), unit=unit, header='Test_io_disp_logical_matrix : ', brief=.false.)

        call disp(l(:, :), header='Test_io_disp_logical_matrix (brief) : ', brief=.true.)
        call disp(l(:, :), unit=unit, header='Test_io_disp_logical_matrix (brief) : ', brief=.true.)

        !! Checks
        rewind (unit)
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_logical_scalar (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'T', 'Value')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_logical_vector (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[vector size: 6]', 'Vector Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             'T   T   T   ..  T', 'Brief Vector')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_logical_vector :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[vector size: 6]', 'Vector Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             'T   T   T   T   T   T', 'Brief Vector')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_logical_matrix :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[matrix size: 2×2]', 'Matrix Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             'T   T', 'Matrix Vector 1')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             'T   T', 'Matrix Vector 2')

        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_logical_matrix (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), '[matrix size: 6×6]', 'Matrix Info')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             'T   T   T   ..  T', 'Matrix Vector 1')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             'T   T   T   ..  T', 'Matrix Vector 2')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             'T   T   T   ..  T', 'Matrix Vector 3')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             ':   :   :   :   :', 'Matrix Vector ..')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
                             'T   T   T   ..  T', 'Matrix Vector Size(Matrix, 1)')
        close (unit)

    end subroutine test_io_disp_logical

    subroutine test_io_disp_character

        character(*), parameter :: str = 'It is a character.'
        ! unit = open(filenanme, 'w+t')
        open (newunit=unit, status='scratch')
        call disp(str, header='Test_io_disp_character_scalar (brief) : ', brief=.true.)
        call disp(str, unit=unit, header='Test_io_disp_character_scalar (brief) : ', brief=.true.)

        !! Checks
        rewind (unit)
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_character_scalar (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'It is a character.', 'Value')
        close (unit)

    end subroutine test_io_disp_character

    subroutine test_io_disp_string_type

        type(string_type) :: str, s(6, 6)

        str = 'It is a string_type.'
        s   = 'It is a string_type.'
        open (newunit=unit, status='scratch')
        call disp(str, header='Test_io_disp_string_type_scalar (brief) : ', brief=.true.)
        call disp(str, unit=unit, header='Test_io_disp_string_type_scalar (brief) : ', brief=.true.)
        call disp(s, header='Test_io_disp_string_type_array (brief) : ', brief=.true.)
        call disp(s, unit=unit, header='Test_io_disp_string_type_array (brief) : ', brief=.true.)
        call disp(s, header='Test_io_disp_string_type_array : ')
        call disp(s, unit=unit, header='Test_io_disp_string_type_array : ')

        !! Checks
        rewind (unit)
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_string_type_scalar (brief) :', 'Header')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'It is a string_type.', 'Value')
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_string_type_array (brief) :', 'Header')
        read (unit, *)
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
            'It is a string_type.  It is a string_type.  It is a string_type.  ..                    It is a string_type.', &
            'Value')
        read (unit, *)
        read (unit, *)
        read (unit, *)
        read (unit, *)
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), 'Test_io_disp_string_type_array :', 'Header')
        read (unit, *)
        read (unit, '(A200)') string
        call check_formatter(trim(adjustl(string)), &
            'It is a string_type.  It is a string_type.  It is a string_type.                &', 'Value')
        close (unit)

    end subroutine test_io_disp_string_type
    
    subroutine larger_matrix
        real(4) :: x(51,51)
        call disp(x, header="Test_io_disp_real_matrix (51×51)(default) : [10×50]")
        call disp(x, header="Test_io_disp_real_matrix (51×51)(brief=.true.) : [5×5]", brief=.true.)
        call disp(x, header="Test_io_disp_real_matrix (51×51)(brief=.false.) : [all]", brief=.false.)
    end subroutine larger_matrix
    
end module test_io_disp

program tester

    use test_io_disp
    logical :: test_larger = .false.
    
    call test_io_disp_complex
    call test_io_disp_real
    call test_io_disp_integer
    call test_io_disp_logical
    call test_io_disp_character
    call test_io_disp_string_type

    !> Content that is difficult to test: The length of the dimension is too large
    !>  to print and check by a test program.
    
    if (test_larger) then
        call larger_matrix
    end if

end program tester
