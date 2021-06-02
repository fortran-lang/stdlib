program test_linspace
    use stdlib_error, only: check
    use stdlib_kinds, only: sp, dp
    use stdlib_math, only: linspace 

    implicit none
    logical :: warn = .false.

    ! Testing linspace.
    !
    ! For single and double precision, check if the beginning and end values are properly calculated
    ! and make sure that the size of the result is as expected.
    !
    !
    open(unit=9, file="test_linspace_log.txt", status="unknown") ! Log the results of the functio

    call test_linspace_sp
    call test_linspace_dp
    call test_linspace_neg_index ! Make sure that when passed a negative index the result is an empty array
    call test_linspace_cmplx
    call test_linspace_cmplx_2
    call test_linspace_cmplx_3
    call test_linspace_cmplx_sp
    call test_linspace_cmplx_sp_2

    close(unit=9)    


contains

    subroutine test_linspace_sp

        integer :: n = 20
        real(sp) :: start = 1.0_sp
        real(sp) :: end = 10.0_sp

        real(sp), dimension(:), allocatable :: x

        x = linspace(start, end, n)

        call check(x(1) == start, msg="Initial value of array is not equal to the passed start parameter", warn=warn)
        call check(x(20) == end, msg="Final array value is not equal to end parameter", warn=warn)
        call check(size(x) == n, msg="Array not allocated to appropriate size", warn=warn)

    end subroutine

    subroutine test_linspace_dp

        real(dp) :: start = 1.0_dp
        real(dp) :: end = 10.0_dp

        real(dp), dimension(:), allocatable :: x

        x = linspace(start, end)

        call check(x(1) == start, msg="Initial value of array is not equal to the passed start parameter", warn=warn)
        call check(x(100) == end, msg="Final array value is not equal to end parameter", warn=warn)
        call check(size(x) == 100, msg="Array not allocated to appropriate size", warn=warn)

    end subroutine

    subroutine test_linspace_neg_index

        real(dp) :: start = 1.0_dp
        real(dp) :: end = 10.0_dp

        real(dp), dimension(:), allocatable :: x

        x = linspace(start, end, -15)

        call check(size(x) == 0, msg="Allocated array is not empty", warn=warn)

    end subroutine

    subroutine test_linspace_cmplx

        complex(dp) :: start = (0.0_dp, 10.0_dp)
        complex(dp) :: end = (1.0_dp, 0.0_dp)

        complex(dp) :: z(10)

        integer :: i

        z = linspace(start, end, 10)

        write(unit=9, fmt=*) "linspace((0.0_dp, 10.0_dp), (1.0_dp, 0.0_dp), 10): "
        write(unit=9,fmt=99)
        do i = 1, 10
            write(unit=9,fmt=*) z(i)
        end do
        write(9,*)
        write(9,*)
              
        99 format(70("="))      

    end subroutine

    subroutine test_linspace_cmplx_2

        complex(dp) :: start = (10.0_dp, 10.0_dp)
        complex(dp) :: end = (1.0_dp, 1.0_dp)

        complex(dp) :: z(5)

        integer :: i

        z = linspace(start, end, 5)

        write(unit=9, fmt=*) "linspace((10.0_dp, 10.0_dp), (1.0_dp, 1.0_dp), 5): "
        write(unit=9,fmt=99)
        do i = 1, 5
            write(unit=9,fmt=*) z(i)
        end do
        write(9,*)
        write(9,*)
              
        99 format(70("="))      

    end subroutine

    subroutine test_linspace_cmplx_3

        complex(dp) :: start = (-5.0_dp, 100.0_dp)
        complex(dp) :: end = (20.0_dp, 13.0_dp)

        complex(dp) :: z(20)

        integer :: i

        z = linspace(start, end, 20)

        write(unit=9, fmt=*) "linspace((-5.0_dp, 100.0_dp), (20.0_dp, 13.0_dp), 20): "
        write(unit=9,fmt=99)
        do i = 1, 20
            write(unit=9,fmt=*) z(i)
        end do
        write(9,*)
        write(9,*)
              
        99 format(70("="))      

    end subroutine

    subroutine test_linspace_cmplx_sp

        complex(sp) :: start = (0.5_sp, 5.0_sp)
        complex(sp) :: end = (1.0_sp, -30.0_sp)

        complex(sp) :: z(10)

        integer :: i

        z = linspace(start, end, 10)

        write(unit=9, fmt=*) "linspace((0.5_sp, 5.0_sp), (1.0_sp, -30.0_sp), 10): "
        write(unit=9,fmt=99)
        do i = 1, 10
            write(unit=9,fmt=*) z(i)
        end do
        write(9,*)
        write(9,*)
              
        99 format(70("="))      

    end subroutine

    subroutine test_linspace_cmplx_sp_2

        complex(sp) :: start = (50.0_sp, 500.0_sp)
        complex(sp) :: end = (-100.0_sp, 2000.0_sp)

        complex(sp) :: z(100)

        integer :: i

        z = linspace(start, end, 100)

        write(unit=9, fmt=*) "linspace((50.0_sp, 500.0_sp), (-100.0_sp, 2000.0_sp)): "
        write(unit=9,fmt=99)
        do i = 1, 100
            write(unit=9,fmt=*) z(i)
        end do
        write(9,*)
        write(9,*)
              
        99 format(70("="))      

    end subroutine


end program