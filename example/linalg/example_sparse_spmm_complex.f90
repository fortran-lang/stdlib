program example_sparse_spmm_complex
    use stdlib_linalg_constants, only: dp
    use stdlib_sparse
    implicit none

    complex(dp), parameter :: alpha = (2._dp,-1._dp)
    complex(dp), parameter :: beta = (-0.5_dp,0.25_dp)
    real(dp), parameter :: tol = 100 * epsilon(1._dp)
    complex(dp) :: a(2,3), b(3,2), left(2,3)
    complex(dp) :: c(3,3), expected(3,3), right(2,2)
    complex(dp) :: product_dense(2,2), eye2(2,2), gram_dense(3,3), eye3(3,3)
    type(COO_cdp_type) :: coo_a, coo_b, coo_product
    type(CSR_cdp_type) :: csr_a, gram
    integer :: work(3), i

    a = reshape([cmplx(1._dp,1._dp,dp), (0._dp,0._dp), &
                 cmplx(2._dp,-1._dp,dp), cmplx(3._dp,2._dp,dp), &
                 (0._dp,0._dp), cmplx(4._dp,-1._dp,dp)],shape(a))
    b = reshape([cmplx(1._dp,-1._dp,dp), (0._dp,0._dp), &
                 cmplx(2._dp,1._dp,dp), cmplx(3._dp,-2._dp,dp), &
                 (0._dp,0._dp), cmplx(4._dp,1._dp,dp)],shape(b))
    left = cmplx(1._dp,2._dp,dp)
    call dense2coo(a,coo_a)
    call coo2csr(coo_a,csr_a)
    call dense2coo(b,coo_b)

    ! Conjugate transpose of a rectangular sparse matrix.
    c = (1._dp,1._dp)
    expected = alpha * matmul(conjg(transpose(a)),left) + beta * c
    call spmm(csr_a,left,c,alpha=alpha,beta=beta,op=sparse_op_hermitian)
    call report('CSR^H x dense',maxval(abs(c-expected)))

    ! The sparse operand can also appear on the right.
    right = (0._dp,0._dp)
    call spmm(left,coo_a,right,op=sparse_op_hermitian)
    call report('dense x COO^H',maxval(abs(right-matmul(left,conjg(transpose(a))))))

    ! Prepare and reuse a same-format complex COO product.
    call spmm_prepare(coo_a,coo_b,coo_product)
    call spmm_kernel_coo('N','N',(1._dp,0._dp), &
        [coo_a%nrows,coo_a%ncols],[coo_b%nrows,coo_b%ncols], &
        coo_a%data,coo_a%index,coo_b%data,coo_b%index,coo_product%data,coo_product%index,work)
    eye2 = (0._dp,0._dp)
    eye2(1,1) = (1._dp,0._dp)
    eye2(2,2) = (1._dp,0._dp)
    product_dense = (0._dp,0._dp)
    call spmm(coo_product,eye2,product_dense)
    call report('COO x COO -> COO',maxval(abs(product_dense-matmul(a,b))))

    ! Build A^H*A directly from sparse factors, then reuse the result buffers.
    call spmm_prepare(csr_a,csr_a,gram,op_a='H')
    call spmm(csr_a,csr_a,gram,op_a='H',allow_resize=.false.,work=work)
    eye3 = (0._dp,0._dp)
    do i = 1, 3
        eye3(i,i) = (1._dp,0._dp)
    end do
    gram_dense = (0._dp,0._dp)
    call spmm(gram,eye3,gram_dense)
    call report('CSR^H x CSR -> CSR',maxval(abs(gram_dense-matmul(conjg(transpose(a)),a))))

contains
    subroutine report(label, difference)
        character(*), intent(in) :: label
        real(dp), intent(in) :: difference
        print '(a,": max error = ",es12.4)', label, difference
        if (difference > tol) error stop 'SpMM example result differs from MATMUL'
    end subroutine
end program example_sparse_spmm_complex
