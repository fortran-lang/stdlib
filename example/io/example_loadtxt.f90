program example_loadtxt
    use stdlib_kinds, only: dp
    use stdlib_io, only: loadtxt
    implicit none
    real(dp), allocatable :: x(:, :)

    call loadtxt('example.dat', x)

    call loadtxt('example.csv', x, delimiter=',')

end program example_loadtxt
