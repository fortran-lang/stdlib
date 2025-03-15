program example_matmul
    use stdlib_intrinsics, only: stdlib_matmul
    complex :: x(2, 2), y(2, 2)
    real :: r1(50, 100), r2(100, 40), r3(40, 50)
    real, allocatable :: res(:, :)
    x = reshape([(0, 0), (1, 0), (1, 0), (0, 0)], [2, 2])
    y = reshape([(0, 0), (0, -1), (0, 1), (0, 0)], [2, 2]) ! pauli y-matrix

    print *, stdlib_matmul(y, y, y, y, y) ! should be y
    print *, stdlib_matmul(x, x, y, x) ! should be -i x sigma_z

    call random_seed()
    call random_number(r1)
    call random_number(r2)
    call random_number(r3)

    res = stdlib_matmul(r1, r2, r3) ! 50x50 matrix
    print *, shape(res)
end program example_matmul
