program example_io_print_array_brief

    use stdlib_io, only: print_array
    implicit none

    real, dimension(123, 456) :: array

    call random_number(array)

    print "(a)", "=== print_array ==="
    call print_array(array, delimiter=', ', brief=.true.)

end program example_io_print_array_brief

! === print_array ===
!  5.44562101E-01,  5.97862303E-01,  1.64548337E-01, ...,  1.18231595E-01
!  6.42154396E-01,  8.24955523E-01,  2.37900555E-01, ...,  6.27207816E-01
!  6.64815307E-02,  8.11999142E-01,  2.75685191E-01, ...,  4.20989931E-01
! ...
!  8.61099184E-01,  6.91828251E-01,  2.91268706E-01, ...,  3.90086174E-01
