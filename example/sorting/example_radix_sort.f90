program example_radix_sort
    use iso_fortran_env
    use iso_c_binding
    use ieee_arithmetic
    use stdlib_sorting, only: radix_sort, ord_sort
    implicit none
    integer(int8), allocatable :: arri8(:)
    integer(int16), allocatable :: arri16(:)
    real(real64), allocatable, target :: arrf64(:), x

    real(real32), dimension(:, :), allocatable :: arr1, arr2
    real(real32), dimension(:), allocatable :: work
    integer :: i, test_size, repeat
    real :: start, stop

    arri8 = [-128, 127, 0, -1, 1]
    call radix_sort(arri8)
    print *, arri8

    arri16 = [-32767, 32767, 0, 0, -3, 2, -3]
    call radix_sort(arri16, reverse=.true.)
    print *, arri16

    allocate (arrf64(10))
    x = 0.0 ! divide zero will arise compile error
    arrf64(1) = 1.0/x
    arrf64(2) = 0.0
    arrf64(3) = 0.0/x
    arrf64(4) = -1.0/x
    arrf64(5) = -0.0
    arrf64(6) = 1.0
    arrf64(7) = -1.0
    arrf64(8) = 3.45
    arrf64(9) = -3.14
    arrf64(10) = 3.44
    call radix_sort(arrf64)
    print *, arrf64
    ! In my computer, it gives
    ! nan, -inf, -3.14, -1.0, -0.0, 0.0, 1.0, 3.44, 3.45, inf
    ! but the position of nan is undefined, the position of `±inf`, `±0.0` is as expected

    test_size = 100000
    repeat = 100
    allocate (arr1(test_size, repeat))
    allocate (arr2(test_size, repeat))
    call random_number(arr1)
    arr1 = arr1 - 0.5
    arr2(:, :) = arr1(:, :)
    allocate (work(test_size))
    call cpu_time(start)
    do i = 1, repeat
        call ord_sort(arr1(:, i), work)
    end do
    call cpu_time(stop)
    print *, "ord_sort time = ", (stop - start)
    call cpu_time(start)
    do i = 1, repeat
        call radix_sort(arr2(:, i), work)
    end do
    call cpu_time(stop)
    print *, "radix_sort time = ", (stop - start)
    print *, all(arr1 == arr2)
end program example_radix_sort
