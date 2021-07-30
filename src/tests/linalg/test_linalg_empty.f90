! SPDX-Identifier: MIT
module test_linalg_empty

    use stdlib_error, only: check
    use stdlib_linalg, only: empty
    use stdlib_ascii, only: to_string
    implicit none
    
contains

    subroutine check_shape(actual, expected, description)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: description
        logical :: stat
        character(len=:), allocatable :: msg

        if (actual /= expected) then
            msg = description//new_line("a")// &
                & "Expected: '"//to_string(expected)//"' but got '"//to_string(actual)//"'"
            stat = .false.
        else
            print '(" - ", a, /, "   Result: ''", a, "''")', description, to_string(actual)
            stat = .true.
        end if

        call check(stat, msg)

    end subroutine check_shape

    subroutine test_linalg_empty_integer
        call check_shape(size(empty(2)), 2, "test_linalg_empty_integer, vector:")
        call check_shape(size(empty(1,2)), 2, "test_linalg_empty_integer, matrix:")
    end subroutine test_linalg_empty_integer

    subroutine test_linalg_empty_real
        call check_shape(size(0.0*empty(2)), 2, "test_linalg_empty_real, vector:")
        call check_shape(size(0.0*empty(1,2)), 2, "test_linalg_empty_real, matrix:")
    end subroutine test_linalg_empty_real

    subroutine test_linalg_empty_cmplx
        call check_shape(size((0.0,0.0)*empty(2)), 2, "test_linalg_empty_cmplx, vector:")
        call check_shape(size((0.0,0.0)*empty(1,2)), 2, "test_linalg_empty_cmplx, matrix:")
    end subroutine test_linalg_empty_cmplx

end module test_linalg_empty

program tester

    use test_linalg_empty

    call test_linalg_empty_integer
    call test_linalg_empty_real
    call test_linalg_empty_cmplx

    print *, empty(2) + empty(3), size(empty(2) + empty(3))
        !!TODO: ??
        !! 44           0           3? / 2?

end program tester
