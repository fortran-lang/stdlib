program test_linspace
    use stdlib_error, only: check
    use stdlib_kinds, only: sp, dp
    use stdlib_stats, only: linspace 

    implicit none
    logical :: warn = .true.

    call test_linspace_sp
    call test_linspace_dp

contains

    subroutine test_linspace_sp

        integer :: n = 20
        real(sp) :: start = 1.0_sp
        real(sp) :: end = 10.0_sp

        real(sp), dimension(:), allocatable :: x

        x = linspace(start, end, n)

        call check(x(1) == start, msg="Initial value of array is not equal to the passed start parameter", warn=warn)


    end subroutine

    subroutine test_linspace_dp

        real(dp) :: start = 1.0_dp
        real(dp) :: end = 10.0_dp

        real(dp), dimension(:), allocatable :: x

        x = linspace(start, end)

        call check(x(1) == start, msg="Initial value of array is not equal to the passed start parameter", warn=warn)

    end subroutine


end program