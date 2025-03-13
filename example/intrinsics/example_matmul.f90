program example_matmul
    use stdlib_intrinsics, only: stdlib_matmul
    complex :: a(2,2)
    a = reshape([(0, 0), (0, -1), (0, 1), (0, 0)], [2, 2]) ! pauli y-matrix

    print *, stdlib_matmul(a, a, a, a, a) ! should be sigma_y
end program example_matmul
