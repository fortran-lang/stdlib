program test_linspace
    use stdlib_error, only: check
    use stdlib_kinds, only: sp, dp
    use stdlib_stats, only: linspace 

    implicit none
    logical :: warn = .false.

    call test_linspace_sp
    call test_linspace_dp
    call test_linspace_neg_index ! Make sure that when passed a negative index the result is an empty array

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


end program