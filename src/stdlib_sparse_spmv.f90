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
    public :: spmv

end module
