program example_tridiagonal_matrix_cdp
    use stdlib_linalg_constants, only: dp
    use stdlib_specialmatrices, only: tridiagonal_cdp_type, tridiagonal, dense, spmv
    implicit none

    integer, parameter :: n = 5
    type(tridiagonal_cdp_type) :: A
    complex(dp) :: dl(n-1), dv(n), du(n-1)
    complex(dp) :: x(n), y(n), y_dense(n)
    integer :: i
    complex(dp) :: alpha, beta

    dl = [(cmplx(i,i, dp), i=1, n - 1)]
    dv = [(cmplx(2*i,2*i, dp), i=1, n)]
    du = [(cmplx(3*i,3*i, dp), i=1, n - 1)]

    A = tridiagonal(dl, dv, du)

    x = (1.0_dp, 0.0_dp)
    y = (3.0_dp, -7.0_dp)
    y_dense = (0.0_dp, 0.0_dp)
    alpha = cmplx(2.0_dp, 3.0_dp)
    beta = cmplx(-1.0_dp, 5.0_dp)

    y_dense = alpha * matmul(dense(A), x) + beta * y
    call spmv(A, x, y, alpha, beta)

    print *, 'dense       :', y_dense
    print *, 'Tridiagonal :', y
end program example_tridiagonal_matrix_cdp
