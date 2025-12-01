module test_print_array

    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use stdlib_linalg, only: eye
    use stdlib_io, only: print_array, get_line
    implicit none
    private

    public :: collect_print_array

contains

    !> Collect all exported unit tests
    subroutine collect_print_array(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("print-rdp", test_print_rdp), &
                    new_unittest("print-rsp", test_print_rsp), &
                    new_unittest("print-cdp", test_print_cdp), &
                    new_unittest("print-csp", test_print_csp), &
                    new_unittest("print-i1", test_print_i1), &
                    new_unittest("print-i2", test_print_i2) &
                    ]

    end subroutine collect_print_array

    subroutine test_print_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(dp) :: a(10, 10)
        integer :: fh, i
        character(256) :: line(10)
        character(:), allocatable :: buffer

        a = eye(10)
        open (newunit=fh, status='scratch')

        line(1) = " 1.0000000000000000E+000  0.0000000000000000E+000  0.0000000000000000E+000 ...  0.0000000000000000E+000"
        line(2) = " 0.0000000000000000E+000  1.0000000000000000E+000  0.0000000000000000E+000 ...  0.0000000000000000E+000"
        line(3) = " 0.0000000000000000E+000  0.0000000000000000E+000  1.0000000000000000E+000 ...  0.0000000000000000E+000"
        line(4) = "..."
        line(5) = " 0.0000000000000000E+000  0.0000000000000000E+000  0.0000000000000000E+000 ...  1.0000000000000000E+000"
        call print_array(a, fh)

        rewind (fh)
        do i = 1, 5
            call get_line(fh, buffer)
            call check(error, buffer, trim(line(i)))
            if (allocated(error)) return
        end do

        rewind (fh)
        line(1) =  "1.00|0.00|0.00|0.00|0.00"
        line(2) =  "0.00|1.00|0.00|0.00|0.00"
        line(3) =  "0.00|0.00|1.00|0.00|0.00"
        line(4) =  "0.00|0.00|0.00|1.00|0.00"
        line(5) =  "0.00|0.00|0.00|0.00|1.00"
        line(6:) = "0.00|0.00|0.00|0.00|0.00"
        
        call print_array(a(:, :5), fh, fmt="(f4.2)", brief=.false., delimiter="|")

        rewind (fh)
        do i = 1, 10
            call get_line(fh, buffer)
            call check(error, buffer, trim(line(i)))
            if (allocated(error)) return
        end do

        close (fh)

    end subroutine test_print_rdp

    subroutine test_print_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(sp) :: a(10, 10)
        integer :: fh, i
        character(256) :: line(10)
        character(:), allocatable :: buffer

        a = eye(10)
        open (newunit=fh, status='scratch')

        line(1) = " 1.00000000E+00  0.00000000E+00  0.00000000E+00 ...  0.00000000E+00"
        line(2) = " 0.00000000E+00  1.00000000E+00  0.00000000E+00 ...  0.00000000E+00"
        line(3) = " 0.00000000E+00  0.00000000E+00  1.00000000E+00 ...  0.00000000E+00"
        line(4) = "..."
        line(5) = " 0.00000000E+00  0.00000000E+00  0.00000000E+00 ...  1.00000000E+00"
        call print_array(a, fh)

        rewind (fh)
        do i = 1, 5
            call get_line(fh, buffer)
            call check(error, buffer, trim(line(i)))
            if (allocated(error)) return
        end do

        rewind (fh)
        line(1) =  "1.00|0.00|0.00|0.00|0.00"
        line(2) =  "0.00|1.00|0.00|0.00|0.00"
        line(3) =  "0.00|0.00|1.00|0.00|0.00"
        line(4) =  "0.00|0.00|0.00|1.00|0.00"
        line(5) =  "0.00|0.00|0.00|0.00|1.00"
        line(6:) = "0.00|0.00|0.00|0.00|0.00"
        call print_array(a(:, :5), fh, fmt="(f4.2)", brief=.false., delimiter="|")

        rewind (fh)
        do i = 1, 10
            call get_line(fh, buffer)
            call check(error, buffer, trim(line(i)))
            if (allocated(error)) return
        end do

        close (fh)

    end subroutine test_print_rsp

    subroutine test_print_i1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int8) :: a(10, 10)
        integer :: fh, i
        character(256) :: line(10)
        character(:), allocatable :: buffer

        a = eye(10)
        open (newunit=fh, status='scratch')

        line(1) = "1, 0, 0, ..., 0"
        line(2) = "0, 1, 0, ..., 0"
        line(3) = "0, 0, 1, ..., 0"
        line(4) = "..."
        line(5) = "0, 0, 0, ..., 1"
        call print_array(a, fh, delimiter=", ")

        rewind (fh)
        do i = 1, 5
            call get_line(fh, buffer)
            call check(error, buffer, trim(line(i)))
            if (allocated(error)) return
        end do

        rewind (fh)
        line(1)  = "01;00;00;00;00"
        line(2)  = "00;01;00;00;00"
        line(3)  = "00;00;01;00;00"
        line(4)  = "00;00;00;01;00"
        line(5)  = "00;00;00;00;01"
        line(6:) = "00;00;00;00;00"
        call print_array(a(:, :5), fh, fmt="(i0.2)", brief=.false., delimiter=";")
        rewind (fh)
        do i = 1, 10
            call get_line(fh, buffer)
            call check(error, buffer, trim(line(i)))
            if (allocated(error)) return
        end do

        close (fh)

    end subroutine test_print_i1

    subroutine test_print_i2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: a(10, 10)
        integer :: fh, i
        character(256) :: line(10)
        character(:), allocatable :: buffer

        a = eye(10)
        open (newunit=fh, status='scratch')

        line(1) = "1 0 0 ... 0"
        line(2) = "0 1 0 ... 0"
        line(3) = "0 0 1 ... 0"
        line(4) = "..."
        line(5) = "0 0 0 ... 1"

        call print_array(a, fh)
        rewind (fh)
        do i = 1, 5
            call get_line(fh, buffer)
            call check(error, buffer, trim(line(i)))
            if (allocated(error)) return
        end do

        rewind (fh)
        line(1)  = "01; 00; 00; 00; 00"
        line(2)  = "00; 01; 00; 00; 00"
        line(3)  = "00; 00; 01; 00; 00"
        line(4)  = "00; 00; 00; 01; 00"
        line(5)  = "00; 00; 00; 00; 01"
        line(6:) = "00; 00; 00; 00; 00"
        call print_array(a(:, :5), fh, fmt="(i0.2)", brief=.false., delimiter="; ")
        rewind (fh)
        do i = 1, 10
            call get_line(fh, buffer)
            call check(error, buffer, trim(line(i)))
            if (allocated(error)) return
        end do

        close (fh)

    end subroutine test_print_i2

    subroutine test_print_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        complex(dp) :: a(10, 10)
        integer :: fh, i
        character(256) :: line(10)
        character(:), allocatable :: buffer

        a = eye(10)
        open (newunit=fh, status='scratch')

        line(1) = " 1.0000000000000000E+000  0.0000000000000000E+000  0.0000000000000000E+000  0.0000000000000000E+000  &
            &0.0000000000000000E+000  0.0000000000000000E+000 ...  0.0000000000000000E+000  0.0000000000000000E+000"
        line(2) = " 0.0000000000000000E+000  0.0000000000000000E+000  1.0000000000000000E+000  0.0000000000000000E+000  &
            &0.0000000000000000E+000  0.0000000000000000E+000 ...  0.0000000000000000E+000  0.0000000000000000E+000"
        line(3) = " 0.0000000000000000E+000  0.0000000000000000E+000  0.0000000000000000E+000  0.0000000000000000E+000  &
            &1.0000000000000000E+000  0.0000000000000000E+000 ...  0.0000000000000000E+000  0.0000000000000000E+000"
        line(4) = "..."
        line(5) = " 0.0000000000000000E+000  0.0000000000000000E+000  0.0000000000000000E+000  0.0000000000000000E+000  &
            &0.0000000000000000E+000  0.0000000000000000E+000 ...  1.0000000000000000E+000  0.0000000000000000E+000"

        call print_array(a, fh)
        rewind (fh)
        do i = 1, 5
            call get_line(fh, buffer)
            call check(error, buffer, trim(line(i)))
            if (allocated(error)) return
        end do

        rewind (fh)
        line(1)  = "1.00,0.00|0.00,0.00|0.00,0.00|0.00,0.00|0.00,0.00"
        line(2)  = "0.00,0.00|1.00,0.00|0.00,0.00|0.00,0.00|0.00,0.00"
        line(3)  = "0.00,0.00|0.00,0.00|1.00,0.00|0.00,0.00|0.00,0.00"
        line(4)  = "0.00,0.00|0.00,0.00|0.00,0.00|1.00,0.00|0.00,0.00"
        line(5)  = "0.00,0.00|0.00,0.00|0.00,0.00|0.00,0.00|1.00,0.00"
        line(6:) = "0.00,0.00|0.00,0.00|0.00,0.00|0.00,0.00|0.00,0.00"
        call print_array(a(:, :5), fh, fmt="(f4.2,"","",f4.2)", brief=.false., delimiter="|")
        rewind (fh)
        do i = 1, 10
            call get_line(fh, buffer)
            call check(error, buffer, trim(line(i)))
            if (allocated(error)) return
        end do

        close (fh)

    end subroutine test_print_cdp

    subroutine test_print_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        complex(sp) :: a(10, 10)
        integer :: fh, i
        character(256) :: line(10)
        character(:), allocatable :: buffer

        a = eye(10)
        open (newunit=fh, status='scratch')

        line(1) = " 1.00000000E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00 ...  &
            &0.00000000E+00  0.00000000E+00"
        line(2) = " 0.00000000E+00  0.00000000E+00  1.00000000E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00 ...  &
            &0.00000000E+00  0.00000000E+00"
        line(3) = " 0.00000000E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00  1.00000000E+00  0.00000000E+00 ...  &
            &0.00000000E+00  0.00000000E+00"
        line(4) = "..."
        line(5) = " 0.00000000E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00  0.00000000E+00 ...  &
            &1.00000000E+00  0.00000000E+00"

        call print_array(a, fh)
        rewind (fh)
        do i = 1, 5
            call get_line(fh, buffer)
            call check(error, buffer, trim(line(i)))
            if (allocated(error)) return
        end do

        rewind (fh)
        line(1) =  " 1.00, 0.00; 0.00, 0.00; 0.00, 0.00; 0.00, 0.00; 0.00, 0.00"
        line(2) =  " 0.00, 0.00; 1.00, 0.00; 0.00, 0.00; 0.00, 0.00; 0.00, 0.00"
        line(3) =  " 0.00, 0.00; 0.00, 0.00; 1.00, 0.00; 0.00, 0.00; 0.00, 0.00"
        line(4) =  " 0.00, 0.00; 0.00, 0.00; 0.00, 0.00; 1.00, 0.00; 0.00, 0.00"
        line(5) =  " 0.00, 0.00; 0.00, 0.00; 0.00, 0.00; 0.00, 0.00; 1.00, 0.00"
        line(6:) = " 0.00, 0.00; 0.00, 0.00; 0.00, 0.00; 0.00, 0.00; 0.00, 0.00"
        call print_array(a(:, :5), fh, fmt="(1x,f4.2,"","",1x,f4.2)", brief=.false., delimiter=";")
        rewind (fh)
        do i = 1, 10
            call get_line(fh, buffer)
            call check(error, buffer, trim(line(i)))
            if (allocated(error)) return
        end do

        close (fh)

    end subroutine test_print_csp

end module test_print_array

program tester

    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_print_array, only: collect_print_array
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("print-array", collect_print_array) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program tester

