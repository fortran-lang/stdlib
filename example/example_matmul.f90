program example_matmul
    use stdlib_intrinsics, only: stdlib_matmul
    complex :: x(2, 2), y(2, 2), z(2, 2)

    x = reshape([(0, 0), (1, 0), (1, 0), (0, 0)], [2, 2])  ! pauli x-matrix
    y = reshape([(0, 0), (0, 1), (0, -1), (0, 0)], [2, 2]) ! pauli y-matrix
    z = reshape([(1, 0), (0, 0), (0, 0), (-1, 0)], [2, 2]) ! pauli z-matrix

    print *, stdlib_matmul(x, y) ! should be iota*z
    print *, stdlib_matmul(y, z, x) ! should be iota*identity
    print *, stdlib_matmul(x, x, z, y) ! should be -iota*x
    print *, stdlib_matmul(x, x, z, y, y) ! should be z
end program example_matmul
