program example_solve
    use stdlib_linalg, only: solve
    implicit none
    real(4) :: a(2, 2), b(2, 1), x(2, 1)
    a = reshape([1, 3, 2, 5], [2, 2])
    b = reshape([1, 2], [2, 1])
    x = solve(a, b)
    print *, x(:, 1) ! [-1., 1.]
end program example_solve
