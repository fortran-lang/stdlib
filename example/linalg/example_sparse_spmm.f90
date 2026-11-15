program example_sparse_spmm
    use stdlib_linalg_constants, only: dp
    use stdlib_sparse
    implicit none

    real(dp) :: a(3,3), b(3,2), c(3,2), reference(3,2)
    type(COO_dp_type) :: coo, coo_product
    type(CSR_dp_type) :: csr, product
    type(CSC_dp_type) :: csc, csc_product
    type(ELL_dp_type) :: ell, ell_product
    type(SELLC_dp_type) :: sellc, sellc_product
    integer :: work(3)

    a = reshape([1._dp,0._dp,2._dp, &
                 0._dp,3._dp,0._dp, &
                 4._dp,0._dp,5._dp],shape(a))
    b = reshape([1._dp,2._dp,3._dp, &
                 4._dp,5._dp,6._dp],shape(b))
    call dense2coo(a,coo)
    call coo2csr(coo,csr)
    call coo2csc(coo,csc)
    call csr2ell(csr,ell)
    call csr2sellc(csr,sellc)

    reference = 2._dp * matmul(a,b) + 1._dp
    c = 1._dp
    call spmm(coo,b,c,alpha=2._dp,beta=1._dp)
    print *, 'dense reference:', reference
    print *, 'COO x dense:    ', c
    c = 1._dp
    call spmm(csr,b,c,alpha=2._dp,beta=1._dp)
    print *, 'CSR x dense:    ', c
    c = 1._dp
    call spmm(csc,b,c,alpha=2._dp,beta=1._dp)
    print *, 'CSC x dense:    ', c
    c = 1._dp
    call spmm(ell,b,c,alpha=2._dp,beta=1._dp)
    print *, 'ELL x dense:    ', c
    c = 1._dp
    call spmm(sellc,b,c,alpha=2._dp,beta=1._dp)
    print *, 'SELLC x dense:  ', c

    ! Sparse factors and result have the same format and numeric kind.
    call spmm(coo,coo,coo_product)
    call spmm(csc,csc,csc_product)
    call spmm(ell,ell,ell_product)
    call spmm(sellc,sellc,sellc_product)

    ! Construct the CSR product pattern once, then update only values.
    call spmm_prepare(csr,csr,product)
    call spmm_kernel_csr('N','N',1._dp,[csr%nrows,csr%ncols],[csr%nrows,csr%ncols], &
        csr%data,csr%rowptr,csr%col,csr%data,csr%rowptr,csr%col, &
        product%data,product%rowptr,product%col,work)
    print *, 'CSR product values:', product%data
    csr%data = 2._dp*csr%data
    call spmm(csr,csr,product,allow_resize=.false.,work=work)
    print *, 'Repeated CSR product values:', product%data
end program example_sparse_spmm
