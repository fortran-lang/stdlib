program example_sparse_spmv
    use stdlib_linalg_constants, only: dp
    use stdlib_sparse
    implicit none

    integer, parameter :: m = 4, n = 2
    real(dp) :: A(m,n), x(n)
    real(dp) :: y_dense(m), y_coo(m), y_csr(m)
    real(dp) :: alpha, beta
    type(COO_dp_type) :: COO
    type(CSR_dp_type) :: CSR

    call random_number(A)
    ! Convert from dense to COO and CSR matrices
    call dense2coo( A , COO )
    call coo2csr( COO , CSR )
    
    ! Initialize vectors
    x       = 1._dp
    y_dense = 2._dp
    y_coo   = y_dense
    y_csr   = y_dense

    ! Perform matrix-vector product
    alpha = 3._dp; beta = 2._dp
    y_dense = alpha * matmul(A,x) + beta * y_dense
    call spmv( COO , x , y_coo , alpha = alpha, beta = beta )
    call spmv( CSR , x , y_csr , alpha = alpha, beta = beta )
    
    print *, 'dense :', y_dense
    print *, 'coo   :', y_coo
    print *, 'csr   :', y_csr
  
end program example_sparse_spmv