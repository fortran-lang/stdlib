module test_string_to_number
    use stdlib_kinds, only: sp, dp, xdp, qp
    use stdlib_str2num, only: to_num
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none
    
contains

    !> Collect all exported unit tests
    subroutine collect_string_to_number(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("to_sp", test_to_sp), &
            new_unittest("to_dp", test_to_dp) &
            ]
    end subroutine collect_string_to_number

    subroutine test_to_sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: wp = sp

        call check(error, ucheck("1.234"))
        if (allocated(error)) return

        call check(error, ucheck("1.E1"))
        if (allocated(error)) return

        call check(error, ucheck("1e0"))
        if (allocated(error)) return

        call check(error, ucheck("0.1234E0"))
        if (allocated(error)) return

        call check(error, ucheck("12.34E0"))
        if (allocated(error)) return

        call check(error, ucheck("0.34E2"))
        if (allocated(error)) return

        call check(error, ucheck(".34e0"))
        if (allocated(error)) return

        call check(error, ucheck("34.E1"))
        if (allocated(error)) return

        call check(error, ucheck("-34.5E1"))
        if (allocated(error)) return

        call check(error, ucheck("0.0021E10"))
        if (allocated(error)) return

        call check(error, ucheck("12.21e-1"))
        if (allocated(error)) return

        call check(error, ucheck("12.21e+001 "))
        if (allocated(error)) return

        call check(error, ucheck("-1"))
        if (allocated(error)) return

        call check(error, ucheck(" -0.23317260678539647E-01 "))
        if (allocated(error)) return

        call check(error, ucheck(" 2.5647869e-003 "//char(13)//char(10)))
        if (allocated(error)) return

        call check(error, ucheck("1.-3"))
        if (allocated(error)) return

        call check(error, ucheck("Inf"))
        if (allocated(error)) return

        call check(error, ucheck("-Inf"))
        if (allocated(error)) return

        call check(error, ucheck("NaN"))
        if (allocated(error)) return

        call check(error, ucheck("0.123456789123456789123456789123456789"))
        if (allocated(error)) return

        call check(error, ucheck("1234567890123456789012345678901234567890-9") )
        if (allocated(error)) return

        call check(error, ucheck("123456.78901234567890123456789012345678901234567890+2") )
        if (allocated(error)) return

        call check(error, ucheck("0.140129846432481707092372958328991613128026194187651577"//&
        &                        "175706828388979108268586060148663818836212158203125E-44"))
        if (allocated(error)) return

    contains
        logical function ucheck(s)
            character(*), intent(in) :: s
            real(wp) :: formatted_read_out
            real(wp) :: to_num_out
            real(wp) :: abs_err
            real(wp) :: rel_err

            ucheck = .true.
            read(s,*) formatted_read_out
            to_num_out = to_num(s, to_num_out)
            abs_err = to_num_out - formatted_read_out
            rel_err = abs_err / formatted_read_out

            if(abs(rel_err) > 0.0_wp) then
                write(*,"('formatted read : ', g0)") formatted_read_out
                write(*,"('to_num         : ', g0)") to_num_out
                write(*,"('difference abs : ', g0)") abs_err
                write(*,"('difference rel : ', g0, '%')") rel_err * 100
                ucheck = .false.
            end if
        end function
    end subroutine

    subroutine test_to_dp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: wp = dp

        call check(error, ucheck("1.234"))
        if (allocated(error)) return

        call check(error, ucheck("1.E1"))
        if (allocated(error)) return

        call check(error, ucheck("1e0"))
        if (allocated(error)) return

        call check(error, ucheck("0.1234E0"))
        if (allocated(error)) return

        call check(error, ucheck("12.34E0"))
        if (allocated(error)) return

        call check(error, ucheck("0.34E2"))
        if (allocated(error)) return

        call check(error, ucheck(".34e0"))
        if (allocated(error)) return

        call check(error, ucheck("34.E1"))
        if (allocated(error)) return

        call check(error, ucheck("-34.5E1"))
        if (allocated(error)) return

        call check(error, ucheck("0.0021E10"))
        if (allocated(error)) return

        call check(error, ucheck("12.21e-1"))
        if (allocated(error)) return

        call check(error, ucheck("12.21e+001 "))
        if (allocated(error)) return

        call check(error, ucheck("-1"))
        if (allocated(error)) return

        call check(error, ucheck(" -0.23317260678539647E-01 "))
        if (allocated(error)) return

        call check(error, ucheck(" 2.5647869e-003 "//char(13)//char(10)))
        if (allocated(error)) return

        call check(error, ucheck("1.-3"))
        if (allocated(error)) return

        call check(error, ucheck("Inf"))
        if (allocated(error)) return

        call check(error, ucheck("-Inf"))
        if (allocated(error)) return

        call check(error, ucheck("NaN"))
        if (allocated(error)) return

        call check(error, ucheck("0.123456789123456789123456789123456789"))
        if (allocated(error)) return

        call check(error, ucheck("1234567890123456789012345678901234567890-9") )
        if (allocated(error)) return

        call check(error, ucheck("123456.78901234567890123456789012345678901234567890+2") )
        if (allocated(error)) return

        call check(error, ucheck("0.140129846432481707092372958328991613128026194187651577"//&
        &                        "175706828388979108268586060148663818836212158203125E-44"))
        if (allocated(error)) return

    contains
        logical function ucheck(s)
            character(*), intent(in) :: s
            real(wp) :: formatted_read_out
            real(wp) :: to_num_out
            real(wp) :: abs_err
            real(wp) :: rel_err

            ucheck = .true.
            read(s,*) formatted_read_out
            to_num_out = to_num(s, to_num_out)
            abs_err = to_num_out - formatted_read_out
            rel_err = abs_err / formatted_read_out

            if(abs(rel_err) > epsilon(0.0_wp)) then
                write(*,"('formatted read : ', g0)") formatted_read_out
                write(*,"('to_num         : ', g0)") to_num_out
                write(*,"('difference abs : ', g0)") abs_err
                write(*,"('difference rel : ', g0, '%')") rel_err * 100
                ucheck = .false.
            end if
        end function
    end subroutine

    
end module test_string_to_number

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_string_to_number, only : collect_string_to_number
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("string_to_number", collect_string_to_number) &
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
