program test_lnspace

use stdlib_error, only: check
use stdlib_kinds, only: sp, dp, int8, int16, int32, int64
use stdlib_math, only: lnspace, EULERS_NUMBER_DP, EULERS_NUMBER_SP

implicit none

    logical :: warn = .false. 

    ! Testing lnspace
    !
    ! lnspace should return a rank 1 array of values equally logarithmically spaced 
    ! from the base**start to base**end, using Euler's number e as the base. If no length
    ! is specified, return a rank 1 array with 50 elements.
    integer :: iunit

    open(newunit=iunit, file="test_lnspace_log.txt", status="unknown") ! Log the results of the function

    write(iunit,*) "Writing to unit #: ", iunit

    call test_linspace_sp
    call test_linspace_dp
    call test_linspace_default

    close(unit=iunit)    

contains

    subroutine test_linspace_sp

        integer :: n = 20
        real(sp) :: start = 0.0_sp
        real(sp) :: end = 2.0_sp
        
        real(sp), allocatable :: x(:)

        logical :: cond_1, cond_2

        x = lnspace(start, end, n)

        cond_1 = (x(1) == (EULERS_NUMBER_SP ** start))
        cond_2 = (x(n) == (EULERS_NUMBER_SP ** end))

        ! call check(cond_1, msg="Initial value of array is not equal to e^start", warn=warn)
        ! call check(cond_2, msg="Final value of array is not equal to e^end", warn=warn)
        call check(size(x) == n, msg="Array not allocated to appropriate size", warn=warn)

        ! call global_logger%add_log_file("test_lnspace_log_file.txt", unit=unit)
        ! call global_logger%log_message("x = lnspace(0.0_sp, 2.0_sp, 20)")
        ! 99 format(G11.5, X)

        write(unit=iunit, fmt=*) "lnspace(0.0_sp, 2.0_sp, 20): "
        write(unit=iunit,fmt=99)
        write(unit=iunit,fmt="(20(F7.3, 2X))") x
        write(iunit,*)
        write(iunit,*)
              
        99 format(70("="))

    end subroutine 

    subroutine test_linspace_dp

        integer :: n = 10
        real(dp) :: start = 1.0_dp
        real(dp) :: end = 0.0_dp
        
        real(dp), allocatable :: x(:)

        logical :: cond_1, cond_2

        x = lnspace(start, end, n)

        cond_1 = (x(1) == (EULERS_NUMBER_DP ** start))
        cond_2 = (x(n) == (EULERS_NUMBER_DP ** end))

        ! call check(cond_1, msg="Initial value of array is not equal to e^start", warn=warn)
        ! call check(cond_2, msg="Final value of array is not equal to e^end", warn=warn)
        call check(size(x) == n, msg="Array not allocated to appropriate size", warn=warn)

        ! call global_logger%add_log_file("test_lnspace_log_file.txt", unit=unit)
        ! call global_logger%log_message("x = lnspace(0.0_sp, 2.0_sp, 20)")
        ! 99 format(G11.5, X)

        write(unit=iunit, fmt=*) "lnspace(1.0_dp, 0.0_dp, 10): "
        write(unit=iunit,fmt=99)
        write(unit=iunit,fmt="(10(F7.3, 2X))") x
        write(iunit,*)
        write(iunit,*)
              
        99 format(70("="))

    end subroutine 

    subroutine test_linspace_default

        real(dp) :: start = 0.0_dp
        real(dp) :: end = 1.0_dp
        
        real(dp), allocatable :: x(:)

        logical :: cond_1, cond_2

        x = lnspace(start, end)

        cond_1 = (x(1) == (EULERS_NUMBER_DP ** start))
        cond_2 = (x(50) == (EULERS_NUMBER_DP ** end))

        ! call check(cond_1, msg="Initial value of array is not equal to e^start", warn=warn)
        ! call check(cond_2, msg="Final value of array is not equal to e^end", warn=warn)
        call check(size(x) == 50, msg="Array not allocated to appropriate size", warn=warn)

        ! call global_logger%add_log_file("test_lnspace_log_file.txt", unit=unit)
        ! call global_logger%log_message("x = lnspace(0.0_sp, 2.0_sp, 20)")
        ! 99 format(G11.5, X)

        write(unit=iunit, fmt=*) "lnspace(0.0_dp, 1.0_dp): "
        write(unit=iunit,fmt=99)
        write(unit=iunit,fmt="(50(F7.3, 2X))") x
        write(iunit,*)
        write(iunit,*)
              
        99 format(70("="))

    end subroutine 


end program