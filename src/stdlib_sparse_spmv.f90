!! The `stdlib_sparse_spmv` submodule provides matrix-vector product kernels.
!!
! This code was modified from https://github.com/jalvesz/FSPARSE by its author: Alves Jose
module stdlib_sparse_spmv
    use stdlib_sparse_constants
    use stdlib_sparse_kinds
    implicit none
    private

    !! Version experimental
    !!
    !! Apply the sparse matrix-vector product $$y = \alpha * op(M) * x + \beta * y $$
    !! [Specifications](../page/specs/stdlib_sparse.html#spmv)
    interface spmv
        module subroutine spmv_coo_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:,:)
            real(sp), intent(inout) :: vec_y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:,:)
            real(sp), intent(inout) :: vec_y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:,:)
            real(sp), intent(inout) :: vec_y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:,:)
            real(sp), intent(inout) :: vec_y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_sellc_sp(matrix,vec_x,vec_y,alpha,beta,op)
            type(SELLC_sp_type), intent(in) :: matrix
            real(sp), intent(in)    :: vec_x(:)
            real(sp), intent(inout) :: vec_y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:,:)
            real(dp), intent(inout) :: vec_y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:,:)
            real(dp), intent(inout) :: vec_y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:,:)
            real(dp), intent(inout) :: vec_y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:,:)
            real(dp), intent(inout) :: vec_y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_sellc_dp(matrix,vec_x,vec_y,alpha,beta,op)
            type(SELLC_dp_type), intent(in) :: matrix
            real(dp), intent(in)    :: vec_x(:)
            real(dp), intent(inout) :: vec_y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:,:)
            complex(sp), intent(inout) :: vec_y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:,:)
            complex(sp), intent(inout) :: vec_y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:,:)
            complex(sp), intent(inout) :: vec_y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:,:)
            complex(sp), intent(inout) :: vec_y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_sellc_csp(matrix,vec_x,vec_y,alpha,beta,op)
            type(SELLC_csp_type), intent(in) :: matrix
            complex(sp), intent(in)    :: vec_x(:)
            complex(sp), intent(inout) :: vec_y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_coo_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(COO_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:,:)
            complex(dp), intent(inout) :: vec_y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csr_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSR_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:,:)
            complex(dp), intent(inout) :: vec_y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_csc_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(CSC_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:,:)
            complex(dp), intent(inout) :: vec_y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_ell_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(ELL_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:,:)
            complex(dp), intent(inout) :: vec_y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_sellc_cdp(matrix,vec_x,vec_y,alpha,beta,op)
            type(SELLC_cdp_type), intent(in) :: matrix
            complex(dp), intent(in)    :: vec_x(:)
            complex(dp), intent(inout) :: vec_y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
    end interface

    !! Version experimental
    !!
    !! Apply the COO sparse matrix-vector product $$y = \alpha * op(M) * x + \beta * y $$
    !! [Specifications](../page/specs/stdlib_sparse.html#spmv)
    interface spmv_kernel_coo
        module subroutine spmv_kernel_coo_1d_sp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            real(sp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            real(sp), intent(in), contiguous    :: vec_x(:)
            real(sp), intent(inout), contiguous :: vec_y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_coo_2d_sp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            real(sp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            real(sp), intent(in), contiguous    :: vec_x(:,:)
            real(sp), intent(inout), contiguous :: vec_y(:,:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_coo_1d_dp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            real(dp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            real(dp), intent(in), contiguous    :: vec_x(:)
            real(dp), intent(inout), contiguous :: vec_y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_coo_2d_dp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            real(dp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            real(dp), intent(in), contiguous    :: vec_x(:,:)
            real(dp), intent(inout), contiguous :: vec_y(:,:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_coo_1d_csp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            complex(sp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            complex(sp), intent(in), contiguous    :: vec_x(:)
            complex(sp), intent(inout), contiguous :: vec_y(:)
            complex(sp), intent(in) :: alpha
            complex(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_coo_2d_csp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            complex(sp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            complex(sp), intent(in), contiguous    :: vec_x(:,:)
            complex(sp), intent(inout), contiguous :: vec_y(:,:)
            complex(sp), intent(in) :: alpha
            complex(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_coo_1d_cdp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            complex(dp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            complex(dp), intent(in), contiguous    :: vec_x(:)
            complex(dp), intent(inout), contiguous :: vec_y(:)
            complex(dp), intent(in) :: alpha
            complex(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_coo_2d_cdp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            complex(dp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            complex(dp), intent(in), contiguous    :: vec_x(:,:)
            complex(dp), intent(inout), contiguous :: vec_y(:,:)
            complex(dp), intent(in) :: alpha
            complex(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
    end interface

    !! Version experimental
    !!
    !! Apply the CSC sparse matrix-vector product $$y = \alpha * op(M) * x + \beta * y $$
    !! [Specifications](../page/specs/stdlib_sparse.html#spmv)
    interface spmv_kernel_csc
        module subroutine spmv_kernel_csc_1d_sp(op,alpha,data,colptr,row,storage,vec_x,beta,vec_y)
            real(sp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: row(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            real(sp), intent(in), contiguous    :: vec_x(:)
            real(sp), intent(inout), contiguous :: vec_y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csc_2d_sp(op,alpha,data,colptr,row,storage,vec_x,beta,vec_y)
            real(sp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: row(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            real(sp), intent(in), contiguous    :: vec_x(:,:)
            real(sp), intent(inout), contiguous :: vec_y(:,:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csc_1d_dp(op,alpha,data,colptr,row,storage,vec_x,beta,vec_y)
            real(dp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: row(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            real(dp), intent(in), contiguous    :: vec_x(:)
            real(dp), intent(inout), contiguous :: vec_y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csc_2d_dp(op,alpha,data,colptr,row,storage,vec_x,beta,vec_y)
            real(dp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: row(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            real(dp), intent(in), contiguous    :: vec_x(:,:)
            real(dp), intent(inout), contiguous :: vec_y(:,:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csc_1d_csp(op,alpha,data,colptr,row,storage,vec_x,beta,vec_y)
            complex(sp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: row(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            complex(sp), intent(in), contiguous    :: vec_x(:)
            complex(sp), intent(inout), contiguous :: vec_y(:)
            complex(sp), intent(in) :: alpha
            complex(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csc_2d_csp(op,alpha,data,colptr,row,storage,vec_x,beta,vec_y)
            complex(sp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: row(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            complex(sp), intent(in), contiguous    :: vec_x(:,:)
            complex(sp), intent(inout), contiguous :: vec_y(:,:)
            complex(sp), intent(in) :: alpha
            complex(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csc_1d_cdp(op,alpha,data,colptr,row,storage,vec_x,beta,vec_y)
            complex(dp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: row(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            complex(dp), intent(in), contiguous    :: vec_x(:)
            complex(dp), intent(inout), contiguous :: vec_y(:)
            complex(dp), intent(in) :: alpha
            complex(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csc_2d_cdp(op,alpha,data,colptr,row,storage,vec_x,beta,vec_y)
            complex(dp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: colptr(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: row(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            complex(dp), intent(in), contiguous    :: vec_x(:,:)
            complex(dp), intent(inout), contiguous :: vec_y(:,:)
            complex(dp), intent(in) :: alpha
            complex(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
    end interface

    !! Version experimental
    !!
    !! Apply the CSR sparse matrix-vector product $$y = \alpha * op(M) * x + \beta * y $$
    !! [Specifications](../page/specs/stdlib_sparse.html#spmv)
    interface spmv_kernel_csr
        module subroutine spmv_kernel_csr_1d_sp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
            real(sp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: col(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: rowptr(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            real(sp), intent(in), contiguous    :: vec_x(:)
            real(sp), intent(inout), contiguous :: vec_y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csr_2d_sp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
            real(sp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: col(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: rowptr(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            real(sp), intent(in), contiguous    :: vec_x(:,:)
            real(sp), intent(inout), contiguous :: vec_y(:,:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csr_1d_dp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
            real(dp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: col(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: rowptr(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            real(dp), intent(in), contiguous    :: vec_x(:)
            real(dp), intent(inout), contiguous :: vec_y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csr_2d_dp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
            real(dp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: col(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: rowptr(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            real(dp), intent(in), contiguous    :: vec_x(:,:)
            real(dp), intent(inout), contiguous :: vec_y(:,:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csr_1d_csp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
            complex(sp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: col(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: rowptr(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            complex(sp), intent(in), contiguous    :: vec_x(:)
            complex(sp), intent(inout), contiguous :: vec_y(:)
            complex(sp), intent(in) :: alpha
            complex(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csr_2d_csp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
            complex(sp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: col(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: rowptr(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            complex(sp), intent(in), contiguous    :: vec_x(:,:)
            complex(sp), intent(inout), contiguous :: vec_y(:,:)
            complex(sp), intent(in) :: alpha
            complex(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csr_1d_cdp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
            complex(dp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: col(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: rowptr(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            complex(dp), intent(in), contiguous    :: vec_x(:)
            complex(dp), intent(inout), contiguous :: vec_y(:)
            complex(dp), intent(in) :: alpha
            complex(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_csr_2d_cdp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
            complex(dp), intent(in), contiguous :: data(:)
            integer(ilp), intent(in), contiguous :: col(:) !! matrix column pointer
            integer(ilp), intent(in), contiguous :: rowptr(:)  !! matrix row pointer
            integer, intent(in) :: storage !! storage
            complex(dp), intent(in), contiguous    :: vec_x(:,:)
            complex(dp), intent(inout), contiguous :: vec_y(:,:)
            complex(dp), intent(in) :: alpha
            complex(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
    end interface

    !! Version experimental
    !!
    !! Apply the ELL sparse matrix-vector product $$y = \alpha * op(M) * x + \beta * y $$
    !! [Specifications](../page/specs/stdlib_sparse.html#spmv)
    interface spmv_kernel_ell
        module subroutine spmv_kernel_ell_1d_sp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            real(sp), intent(in), contiguous :: data(:,:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            real(sp), intent(in), contiguous    :: vec_x(:)
            real(sp), intent(inout), contiguous :: vec_y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_ell_2d_sp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            real(sp), intent(in), contiguous :: data(:,:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            real(sp), intent(in), contiguous    :: vec_x(:,:)
            real(sp), intent(inout), contiguous :: vec_y(:,:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_ell_1d_dp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            real(dp), intent(in), contiguous :: data(:,:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            real(dp), intent(in), contiguous    :: vec_x(:)
            real(dp), intent(inout), contiguous :: vec_y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_ell_2d_dp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            real(dp), intent(in), contiguous :: data(:,:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            real(dp), intent(in), contiguous    :: vec_x(:,:)
            real(dp), intent(inout), contiguous :: vec_y(:,:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_ell_1d_csp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            complex(sp), intent(in), contiguous :: data(:,:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            complex(sp), intent(in), contiguous    :: vec_x(:)
            complex(sp), intent(inout), contiguous :: vec_y(:)
            complex(sp), intent(in) :: alpha
            complex(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_ell_2d_csp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            complex(sp), intent(in), contiguous :: data(:,:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            complex(sp), intent(in), contiguous    :: vec_x(:,:)
            complex(sp), intent(inout), contiguous :: vec_y(:,:)
            complex(sp), intent(in) :: alpha
            complex(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_ell_1d_cdp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            complex(dp), intent(in), contiguous :: data(:,:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            complex(dp), intent(in), contiguous    :: vec_x(:)
            complex(dp), intent(inout), contiguous :: vec_y(:)
            complex(dp), intent(in) :: alpha
            complex(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_ell_2d_cdp(op,alpha,data,index,storage,vec_x,beta,vec_y)
            complex(dp), intent(in), contiguous :: data(:,:)
            integer(ilp), intent(in), contiguous :: index(:,:)
            integer, intent(in) :: storage
            complex(dp), intent(in), contiguous    :: vec_x(:,:)
            complex(dp), intent(inout), contiguous :: vec_y(:,:)
            complex(dp), intent(in) :: alpha
            complex(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
    end interface

    !! Version experimental
    !!
    !! Apply the SELLC sparse matrix-vector product $$y = \alpha * op(M) * x + \beta * y $$
    !! [Specifications](../page/specs/stdlib_sparse.html#spmv)
    interface spmv_kernel_sellc
        module subroutine spmv_kernel_sellc_sp(op,alpha,data,ia,ja,storage,vec_x,beta,vec_y)
            !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
            real(sp), intent(in), contiguous :: data(:,:)
            integer(ilp), intent(in), contiguous :: ia(:)
            integer(ilp), intent(in), contiguous :: ja(:,:)
            integer, intent(in) :: storage
            real(sp), intent(in), contiguous    :: vec_x(:)
            real(sp), intent(inout), contiguous :: vec_y(:)
            real(sp), intent(in) :: alpha
            real(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_sellc_dp(op,alpha,data,ia,ja,storage,vec_x,beta,vec_y)
            !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
            real(dp), intent(in), contiguous :: data(:,:)
            integer(ilp), intent(in), contiguous :: ia(:)
            integer(ilp), intent(in), contiguous :: ja(:,:)
            integer, intent(in) :: storage
            real(dp), intent(in), contiguous    :: vec_x(:)
            real(dp), intent(inout), contiguous :: vec_y(:)
            real(dp), intent(in) :: alpha
            real(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_sellc_csp(op,alpha,data,ia,ja,storage,vec_x,beta,vec_y)
            !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
            complex(sp), intent(in), contiguous :: data(:,:)
            integer(ilp), intent(in), contiguous :: ia(:)
            integer(ilp), intent(in), contiguous :: ja(:,:)
            integer, intent(in) :: storage
            complex(sp), intent(in), contiguous    :: vec_x(:)
            complex(sp), intent(inout), contiguous :: vec_y(:)
            complex(sp), intent(in) :: alpha
            complex(sp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
        module subroutine spmv_kernel_sellc_cdp(op,alpha,data,ia,ja,storage,vec_x,beta,vec_y)
            !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
            complex(dp), intent(in), contiguous :: data(:,:)
            integer(ilp), intent(in), contiguous :: ia(:)
            integer(ilp), intent(in), contiguous :: ja(:,:)
            integer, intent(in) :: storage
            complex(dp), intent(in), contiguous    :: vec_x(:)
            complex(dp), intent(inout), contiguous :: vec_y(:)
            complex(dp), intent(in) :: alpha
            complex(dp), intent(in) :: beta
            character(1), intent(in) :: op
        end subroutine
    end interface

    public :: spmv
    public :: spmv_kernel_coo
    public :: spmv_kernel_csc
    public :: spmv_kernel_csr
    public :: spmv_kernel_ell
    public :: spmv_kernel_sellc

end module
