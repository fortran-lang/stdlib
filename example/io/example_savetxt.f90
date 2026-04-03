program example_savetxt
    use stdlib_io, only: savetxt
    use, intrinsic :: iso_fortran_env, only: output_unit

    implicit none
    real :: x(3, 2)
    integer :: unit
    x = reshape([1, 2, 3, 4, 5, 6], [3, 2])

    ! Output to file, default options
    call savetxt('example1.dat', x)
    ! Result in example1.dat:
    !  1.00000000E+00  4.00000000E+00
    !  2.00000000E+00  5.00000000E+00
    !  3.00000000E+00  6.00000000E+00
    !
    ! Output to file, with custom delimiter
    call savetxt('example1.csv', x, delimiter=',')
    ! Result in example1.csv:
    ! 1.00000000E+00, 4.00000000E+00
    ! 2.00000000E+00, 5.00000000E+00
    ! 3.00000000E+00, 6.00000000E+00
    !
    ! Output to file, with header and footer and custom comment string
    call savetxt('example2.dat', x, header='x  (x-units)   y  (y-units)', comments='!#', footer='This is all data')
    ! Result in example2.dat:
  !!# x  (x-units)   y  (y-units)
    ! 1.00000000E+00  4.00000000E+00
    ! 2.00000000E+00  5.00000000E+00
    ! 3.00000000E+00  6.00000000E+00
  !!# This is all data
    !
    ! Output to file. Custom format
    call savetxt('example3.dat', x, fmt='f4.2')
    ! Result in example3.dat:
    ! 1.00 4.00
    ! 2.00 5.00
    ! 3.00 6.00
    !
    ! Output to standard output. Diferent formats for the columns
    call savetxt(output_unit, x, header='x   y', fmt='(f4.2,1x,f3.1)')
    ! Output:
    ! # x   y
    ! 1.00 4.0
    ! 2.00 5.0
    ! 3.00 6.0
    !
    ! Save data and then overwrite it:
    call savetxt('example4.dat', x)
    call savetxt('example4.dat', 2 * x)
    ! Result in example4.dat:
    ! 2.00000000E+00  8.00000000E+00
    ! 4.00000000E+00  1.00000000E+01
    ! 6.00000000E+00  1.20000000E+01

    ! Save data and then append some more:
    open (newunit=unit, file='example5.dat')
    call savetxt(unit, x, header='x  (x-units)   y  (y-units)')
    call savetxt(unit, 2 * x)
    close (unit)
    ! Result in example5.dat:
    ! # x  (x-units)   y  (y-units)
    !  1.00000000E+00  4.00000000E+00
    !  2.00000000E+00  5.00000000E+00
    !  3.00000000E+00  6.00000000E+00
    !  2.00000000E+00  8.00000000E+00
    !  4.00000000E+00  1.00000000E+01
    !  6.00000000E+00  1.20000000E+01

end program example_savetxt
