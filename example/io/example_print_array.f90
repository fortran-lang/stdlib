program example_io_print_array

    use stdlib_io, only: print_array
    implicit none

    integer, dimension(6, 3) :: array = reshape([1, 2, 3, 4, 5, 6, &
                                                 7, 8, 9, 10, 11, 12, &
                                                 13, 14, 15, 16, 17, 18], [6, 3])

    print "(a)", "=== print_array 1 ==="
    call print_array(array, unit=6, fmt='(i3)', delimiter='|', brief=.true.)

    print "(a)", "=== print_array 2 ==="
    call print_array(array(:1, :))

end program example_io_print_array
