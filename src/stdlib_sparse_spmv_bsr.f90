submodule (stdlib_sparse_spmv) stdlib_sparse_spmv_bsr
contains

    !! spmv_bsr
    module subroutine spmv_bsr_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(BSR_sp_type), intent(in) :: matrix
        real(sp), intent(in), contiguous    :: vec_x(:)
        real(sp), intent(inout), contiguous :: vec_y(:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(sp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp;  if(present(alpha)) alpha_ = alpha
        beta_ = zero_sp;  if(present(beta)) beta_ = beta

        call spmv_kernel_bsr_1d_sp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_bsr_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(BSR_sp_type), intent(in) :: matrix
        real(sp), intent(in), contiguous    :: vec_x(:,:)
        real(sp), intent(inout), contiguous :: vec_y(:,:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(sp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp;  if(present(alpha)) alpha_ = alpha
        beta_ = zero_sp;  if(present(beta)) beta_ = beta

        call spmv_kernel_bsr_2d_sp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_kernel_bsr_1d_sp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
        real(sp), intent(in), contiguous :: data(:,:,:)
        integer(ilp), intent(in), contiguous :: col(:) !! block column index
        integer(ilp), intent(in), contiguous :: rowptr(:)  !! block row pointer
        integer, intent(in) :: storage !! storage
        real(sp), intent(in), contiguous    :: vec_x(:)
        real(sp), intent(inout), contiguous :: vec_y(:)
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: br, bc, nbrow

        br = size(data,dim=1)
        bc = size(data,dim=2)
        nbrow = size(rowptr) - 1

        if( storage == sparse_full .and. op==sparse_op_none ) then
            ! beta is applied while writing each block row of vec_y
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_gather_1d_b2_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (3)
                call bsr_gather_1d_b3_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (4)
                call bsr_gather_1d_b4_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (5)
                call bsr_gather_1d_b5_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (6)
                call bsr_gather_1d_b6_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (7)
                call bsr_gather_1d_b7_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (8)
                call bsr_gather_1d_b8_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case default
                call bsr_gather_1d_gen_sp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            end select
            return
        end if

        if( storage /= sparse_full .and. br /= bc ) then
            print *, "error: bsr symmetric storage requires square blocks."
            return
        end if

        if( beta == zero_sp ) then
            vec_y = zero_sp
        else
            vec_y = beta * vec_y
        end if

        if( storage == sparse_full ) then
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_scatter_1d_b2_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (3)
                call bsr_scatter_1d_b3_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (4)
                call bsr_scatter_1d_b4_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (5)
                call bsr_scatter_1d_b5_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (6)
                call bsr_scatter_1d_b6_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (7)
                call bsr_scatter_1d_b7_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (8)
                call bsr_scatter_1d_b8_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case default
                call bsr_scatter_1d_gen_sp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            end select
        else
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_symm_1d_b2_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (3)
                call bsr_symm_1d_b3_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (4)
                call bsr_symm_1d_b4_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (5)
                call bsr_symm_1d_b5_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (6)
                call bsr_symm_1d_b6_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (7)
                call bsr_symm_1d_b7_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (8)
                call bsr_symm_1d_b8_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case default
                call bsr_symm_1d_gen_sp(br, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            end select
        end if
    end subroutine

    module subroutine spmv_kernel_bsr_2d_sp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
        real(sp), intent(in), contiguous :: data(:,:,:)
        integer(ilp), intent(in), contiguous :: col(:) !! block column index
        integer(ilp), intent(in), contiguous :: rowptr(:)  !! block row pointer
        integer, intent(in) :: storage !! storage
        real(sp), intent(in), contiguous    :: vec_x(:,:)
        real(sp), intent(inout), contiguous :: vec_y(:,:)
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: br, bc, nbrow, k

        br = size(data,dim=1)
        bc = size(data,dim=2)
        nbrow = size(rowptr) - 1
        k = size(vec_x,dim=1)

        if( storage == sparse_full .and. op==sparse_op_none ) then
            ! beta is applied while writing each block row of vec_y
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_gather_2d_b2_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (3)
                call bsr_gather_2d_b3_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (4)
                call bsr_gather_2d_b4_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (5)
                call bsr_gather_2d_b5_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (6)
                call bsr_gather_2d_b6_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (7)
                call bsr_gather_2d_b7_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (8)
                call bsr_gather_2d_b8_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case default
                call bsr_gather_2d_gen_sp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            end select
            return
        end if

        if( storage /= sparse_full .and. br /= bc ) then
            print *, "error: bsr symmetric storage requires square blocks."
            return
        end if

        if( beta == zero_sp ) then
            vec_y = zero_sp
        else
            vec_y = beta * vec_y
        end if

        if( storage == sparse_full ) then
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_scatter_2d_b2_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (3)
                call bsr_scatter_2d_b3_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (4)
                call bsr_scatter_2d_b4_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (5)
                call bsr_scatter_2d_b5_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (6)
                call bsr_scatter_2d_b6_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (7)
                call bsr_scatter_2d_b7_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (8)
                call bsr_scatter_2d_b8_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case default
                call bsr_scatter_2d_gen_sp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            end select
        else
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_symm_2d_b2_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (3)
                call bsr_symm_2d_b3_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (4)
                call bsr_symm_2d_b4_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (5)
                call bsr_symm_2d_b5_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (6)
                call bsr_symm_2d_b6_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (7)
                call bsr_symm_2d_b7_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (8)
                call bsr_symm_2d_b8_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case default
                call bsr_symm_2d_gen_sp(br, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            end select
        end if
    end subroutine

    module subroutine spmv_bsr_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(BSR_dp_type), intent(in) :: matrix
        real(dp), intent(in), contiguous    :: vec_x(:)
        real(dp), intent(inout), contiguous :: vec_y(:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(dp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp;  if(present(alpha)) alpha_ = alpha
        beta_ = zero_dp;  if(present(beta)) beta_ = beta

        call spmv_kernel_bsr_1d_dp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_bsr_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(BSR_dp_type), intent(in) :: matrix
        real(dp), intent(in), contiguous    :: vec_x(:,:)
        real(dp), intent(inout), contiguous :: vec_y(:,:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(dp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp;  if(present(alpha)) alpha_ = alpha
        beta_ = zero_dp;  if(present(beta)) beta_ = beta

        call spmv_kernel_bsr_2d_dp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_kernel_bsr_1d_dp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
        real(dp), intent(in), contiguous :: data(:,:,:)
        integer(ilp), intent(in), contiguous :: col(:) !! block column index
        integer(ilp), intent(in), contiguous :: rowptr(:)  !! block row pointer
        integer, intent(in) :: storage !! storage
        real(dp), intent(in), contiguous    :: vec_x(:)
        real(dp), intent(inout), contiguous :: vec_y(:)
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: br, bc, nbrow

        br = size(data,dim=1)
        bc = size(data,dim=2)
        nbrow = size(rowptr) - 1

        if( storage == sparse_full .and. op==sparse_op_none ) then
            ! beta is applied while writing each block row of vec_y
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_gather_1d_b2_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (3)
                call bsr_gather_1d_b3_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (4)
                call bsr_gather_1d_b4_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (5)
                call bsr_gather_1d_b5_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (6)
                call bsr_gather_1d_b6_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (7)
                call bsr_gather_1d_b7_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (8)
                call bsr_gather_1d_b8_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case default
                call bsr_gather_1d_gen_dp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            end select
            return
        end if

        if( storage /= sparse_full .and. br /= bc ) then
            print *, "error: bsr symmetric storage requires square blocks."
            return
        end if

        if( beta == zero_dp ) then
            vec_y = zero_dp
        else
            vec_y = beta * vec_y
        end if

        if( storage == sparse_full ) then
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_scatter_1d_b2_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (3)
                call bsr_scatter_1d_b3_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (4)
                call bsr_scatter_1d_b4_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (5)
                call bsr_scatter_1d_b5_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (6)
                call bsr_scatter_1d_b6_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (7)
                call bsr_scatter_1d_b7_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (8)
                call bsr_scatter_1d_b8_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case default
                call bsr_scatter_1d_gen_dp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            end select
        else
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_symm_1d_b2_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (3)
                call bsr_symm_1d_b3_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (4)
                call bsr_symm_1d_b4_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (5)
                call bsr_symm_1d_b5_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (6)
                call bsr_symm_1d_b6_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (7)
                call bsr_symm_1d_b7_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (8)
                call bsr_symm_1d_b8_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case default
                call bsr_symm_1d_gen_dp(br, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            end select
        end if
    end subroutine

    module subroutine spmv_kernel_bsr_2d_dp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
        real(dp), intent(in), contiguous :: data(:,:,:)
        integer(ilp), intent(in), contiguous :: col(:) !! block column index
        integer(ilp), intent(in), contiguous :: rowptr(:)  !! block row pointer
        integer, intent(in) :: storage !! storage
        real(dp), intent(in), contiguous    :: vec_x(:,:)
        real(dp), intent(inout), contiguous :: vec_y(:,:)
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: br, bc, nbrow, k

        br = size(data,dim=1)
        bc = size(data,dim=2)
        nbrow = size(rowptr) - 1
        k = size(vec_x,dim=1)

        if( storage == sparse_full .and. op==sparse_op_none ) then
            ! beta is applied while writing each block row of vec_y
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_gather_2d_b2_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (3)
                call bsr_gather_2d_b3_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (4)
                call bsr_gather_2d_b4_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (5)
                call bsr_gather_2d_b5_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (6)
                call bsr_gather_2d_b6_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (7)
                call bsr_gather_2d_b7_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (8)
                call bsr_gather_2d_b8_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case default
                call bsr_gather_2d_gen_dp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            end select
            return
        end if

        if( storage /= sparse_full .and. br /= bc ) then
            print *, "error: bsr symmetric storage requires square blocks."
            return
        end if

        if( beta == zero_dp ) then
            vec_y = zero_dp
        else
            vec_y = beta * vec_y
        end if

        if( storage == sparse_full ) then
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_scatter_2d_b2_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (3)
                call bsr_scatter_2d_b3_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (4)
                call bsr_scatter_2d_b4_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (5)
                call bsr_scatter_2d_b5_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (6)
                call bsr_scatter_2d_b6_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (7)
                call bsr_scatter_2d_b7_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (8)
                call bsr_scatter_2d_b8_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case default
                call bsr_scatter_2d_gen_dp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            end select
        else
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_symm_2d_b2_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (3)
                call bsr_symm_2d_b3_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (4)
                call bsr_symm_2d_b4_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (5)
                call bsr_symm_2d_b5_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (6)
                call bsr_symm_2d_b6_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (7)
                call bsr_symm_2d_b7_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case (8)
                call bsr_symm_2d_b8_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            case default
                call bsr_symm_2d_gen_dp(br, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
            end select
        end if
    end subroutine

    module subroutine spmv_bsr_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(BSR_csp_type), intent(in) :: matrix
        complex(sp), intent(in), contiguous    :: vec_x(:)
        complex(sp), intent(inout), contiguous :: vec_y(:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(sp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp;  if(present(alpha)) alpha_ = alpha
        beta_ = zero_sp;  if(present(beta)) beta_ = beta

        call spmv_kernel_bsr_1d_csp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_bsr_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(BSR_csp_type), intent(in) :: matrix
        complex(sp), intent(in), contiguous    :: vec_x(:,:)
        complex(sp), intent(inout), contiguous :: vec_y(:,:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(sp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp;  if(present(alpha)) alpha_ = alpha
        beta_ = zero_sp;  if(present(beta)) beta_ = beta

        call spmv_kernel_bsr_2d_csp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_kernel_bsr_1d_csp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
        complex(sp), intent(in), contiguous :: data(:,:,:)
        integer(ilp), intent(in), contiguous :: col(:) !! block column index
        integer(ilp), intent(in), contiguous :: rowptr(:)  !! block row pointer
        integer, intent(in) :: storage !! storage
        complex(sp), intent(in), contiguous    :: vec_x(:)
        complex(sp), intent(inout), contiguous :: vec_y(:)
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: br, bc, nbrow

        br = size(data,dim=1)
        bc = size(data,dim=2)
        nbrow = size(rowptr) - 1

        if( storage == sparse_full .and. op==sparse_op_none ) then
            ! beta is applied while writing each block row of vec_y
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_gather_1d_b2_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (3)
                call bsr_gather_1d_b3_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (4)
                call bsr_gather_1d_b4_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (5)
                call bsr_gather_1d_b5_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (6)
                call bsr_gather_1d_b6_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (7)
                call bsr_gather_1d_b7_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (8)
                call bsr_gather_1d_b8_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case default
                call bsr_gather_1d_gen_csp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            end select
            return
        end if

        if( storage /= sparse_full .and. br /= bc ) then
            print *, "error: bsr symmetric storage requires square blocks."
            return
        end if

        if( beta == zero_csp ) then
            vec_y = zero_csp
        else
            vec_y = beta * vec_y
        end if

        if( storage == sparse_full ) then
            if( op==sparse_op_hermitian ) then
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_scatter_h_1d_b2_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_scatter_h_1d_b3_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_scatter_h_1d_b4_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_scatter_h_1d_b5_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_scatter_h_1d_b6_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_scatter_h_1d_b7_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_scatter_h_1d_b8_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_scatter_h_1d_gen_csp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            else
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_scatter_1d_b2_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_scatter_1d_b3_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_scatter_1d_b4_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_scatter_1d_b5_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_scatter_1d_b6_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_scatter_1d_b7_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_scatter_1d_b8_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_scatter_1d_gen_csp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            end if
        else
            if( op==sparse_op_hermitian ) then
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_symm_h_1d_b2_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_symm_h_1d_b3_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_symm_h_1d_b4_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_symm_h_1d_b5_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_symm_h_1d_b6_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_symm_h_1d_b7_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_symm_h_1d_b8_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_symm_h_1d_gen_csp(br, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            else
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_symm_1d_b2_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_symm_1d_b3_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_symm_1d_b4_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_symm_1d_b5_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_symm_1d_b6_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_symm_1d_b7_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_symm_1d_b8_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_symm_1d_gen_csp(br, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            end if
        end if
    end subroutine

    module subroutine spmv_kernel_bsr_2d_csp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
        complex(sp), intent(in), contiguous :: data(:,:,:)
        integer(ilp), intent(in), contiguous :: col(:) !! block column index
        integer(ilp), intent(in), contiguous :: rowptr(:)  !! block row pointer
        integer, intent(in) :: storage !! storage
        complex(sp), intent(in), contiguous    :: vec_x(:,:)
        complex(sp), intent(inout), contiguous :: vec_y(:,:)
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: br, bc, nbrow, k

        br = size(data,dim=1)
        bc = size(data,dim=2)
        nbrow = size(rowptr) - 1
        k = size(vec_x,dim=1)

        if( storage == sparse_full .and. op==sparse_op_none ) then
            ! beta is applied while writing each block row of vec_y
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_gather_2d_b2_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (3)
                call bsr_gather_2d_b3_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (4)
                call bsr_gather_2d_b4_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (5)
                call bsr_gather_2d_b5_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (6)
                call bsr_gather_2d_b6_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (7)
                call bsr_gather_2d_b7_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (8)
                call bsr_gather_2d_b8_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case default
                call bsr_gather_2d_gen_csp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            end select
            return
        end if

        if( storage /= sparse_full .and. br /= bc ) then
            print *, "error: bsr symmetric storage requires square blocks."
            return
        end if

        if( beta == zero_csp ) then
            vec_y = zero_csp
        else
            vec_y = beta * vec_y
        end if

        if( storage == sparse_full ) then
            if( op==sparse_op_hermitian ) then
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_scatter_h_2d_b2_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_scatter_h_2d_b3_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_scatter_h_2d_b4_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_scatter_h_2d_b5_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_scatter_h_2d_b6_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_scatter_h_2d_b7_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_scatter_h_2d_b8_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_scatter_h_2d_gen_csp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            else
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_scatter_2d_b2_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_scatter_2d_b3_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_scatter_2d_b4_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_scatter_2d_b5_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_scatter_2d_b6_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_scatter_2d_b7_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_scatter_2d_b8_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_scatter_2d_gen_csp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            end if
        else
            if( op==sparse_op_hermitian ) then
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_symm_h_2d_b2_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_symm_h_2d_b3_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_symm_h_2d_b4_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_symm_h_2d_b5_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_symm_h_2d_b6_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_symm_h_2d_b7_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_symm_h_2d_b8_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_symm_h_2d_gen_csp(br, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            else
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_symm_2d_b2_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_symm_2d_b3_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_symm_2d_b4_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_symm_2d_b5_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_symm_2d_b6_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_symm_2d_b7_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_symm_2d_b8_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_symm_2d_gen_csp(br, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            end if
        end if
    end subroutine

    module subroutine spmv_bsr_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(BSR_cdp_type), intent(in) :: matrix
        complex(dp), intent(in), contiguous    :: vec_x(:)
        complex(dp), intent(inout), contiguous :: vec_y(:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(dp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp;  if(present(alpha)) alpha_ = alpha
        beta_ = zero_dp;  if(present(beta)) beta_ = beta

        call spmv_kernel_bsr_1d_cdp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_bsr_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(BSR_cdp_type), intent(in) :: matrix
        complex(dp), intent(in), contiguous    :: vec_x(:,:)
        complex(dp), intent(inout), contiguous :: vec_y(:,:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(dp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp;  if(present(alpha)) alpha_ = alpha
        beta_ = zero_dp;  if(present(beta)) beta_ = beta

        call spmv_kernel_bsr_2d_cdp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_kernel_bsr_1d_cdp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
        complex(dp), intent(in), contiguous :: data(:,:,:)
        integer(ilp), intent(in), contiguous :: col(:) !! block column index
        integer(ilp), intent(in), contiguous :: rowptr(:)  !! block row pointer
        integer, intent(in) :: storage !! storage
        complex(dp), intent(in), contiguous    :: vec_x(:)
        complex(dp), intent(inout), contiguous :: vec_y(:)
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: br, bc, nbrow

        br = size(data,dim=1)
        bc = size(data,dim=2)
        nbrow = size(rowptr) - 1

        if( storage == sparse_full .and. op==sparse_op_none ) then
            ! beta is applied while writing each block row of vec_y
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_gather_1d_b2_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (3)
                call bsr_gather_1d_b3_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (4)
                call bsr_gather_1d_b4_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (5)
                call bsr_gather_1d_b5_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (6)
                call bsr_gather_1d_b6_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (7)
                call bsr_gather_1d_b7_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (8)
                call bsr_gather_1d_b8_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case default
                call bsr_gather_1d_gen_cdp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            end select
            return
        end if

        if( storage /= sparse_full .and. br /= bc ) then
            print *, "error: bsr symmetric storage requires square blocks."
            return
        end if

        if( beta == zero_cdp ) then
            vec_y = zero_cdp
        else
            vec_y = beta * vec_y
        end if

        if( storage == sparse_full ) then
            if( op==sparse_op_hermitian ) then
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_scatter_h_1d_b2_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_scatter_h_1d_b3_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_scatter_h_1d_b4_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_scatter_h_1d_b5_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_scatter_h_1d_b6_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_scatter_h_1d_b7_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_scatter_h_1d_b8_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_scatter_h_1d_gen_cdp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            else
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_scatter_1d_b2_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_scatter_1d_b3_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_scatter_1d_b4_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_scatter_1d_b5_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_scatter_1d_b6_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_scatter_1d_b7_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_scatter_1d_b8_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_scatter_1d_gen_cdp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            end if
        else
            if( op==sparse_op_hermitian ) then
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_symm_h_1d_b2_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_symm_h_1d_b3_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_symm_h_1d_b4_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_symm_h_1d_b5_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_symm_h_1d_b6_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_symm_h_1d_b7_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_symm_h_1d_b8_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_symm_h_1d_gen_cdp(br, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            else
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_symm_1d_b2_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_symm_1d_b3_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_symm_1d_b4_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_symm_1d_b5_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_symm_1d_b6_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_symm_1d_b7_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_symm_1d_b8_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_symm_1d_gen_cdp(br, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            end if
        end if
    end subroutine

    module subroutine spmv_kernel_bsr_2d_cdp(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)
        complex(dp), intent(in), contiguous :: data(:,:,:)
        integer(ilp), intent(in), contiguous :: col(:) !! block column index
        integer(ilp), intent(in), contiguous :: rowptr(:)  !! block row pointer
        integer, intent(in) :: storage !! storage
        complex(dp), intent(in), contiguous    :: vec_x(:,:)
        complex(dp), intent(inout), contiguous :: vec_y(:,:)
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: br, bc, nbrow, k

        br = size(data,dim=1)
        bc = size(data,dim=2)
        nbrow = size(rowptr) - 1
        k = size(vec_x,dim=1)

        if( storage == sparse_full .and. op==sparse_op_none ) then
            ! beta is applied while writing each block row of vec_y
            select case (merge(br, 0_ilp, br == bc))
            case (2)
                call bsr_gather_2d_b2_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (3)
                call bsr_gather_2d_b3_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (4)
                call bsr_gather_2d_b4_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (5)
                call bsr_gather_2d_b5_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (6)
                call bsr_gather_2d_b6_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (7)
                call bsr_gather_2d_b7_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case (8)
                call bsr_gather_2d_b8_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            case default
                call bsr_gather_2d_gen_cdp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
            end select
            return
        end if

        if( storage /= sparse_full .and. br /= bc ) then
            print *, "error: bsr symmetric storage requires square blocks."
            return
        end if

        if( beta == zero_cdp ) then
            vec_y = zero_cdp
        else
            vec_y = beta * vec_y
        end if

        if( storage == sparse_full ) then
            if( op==sparse_op_hermitian ) then
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_scatter_h_2d_b2_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_scatter_h_2d_b3_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_scatter_h_2d_b4_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_scatter_h_2d_b5_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_scatter_h_2d_b6_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_scatter_h_2d_b7_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_scatter_h_2d_b8_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_scatter_h_2d_gen_cdp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            else
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_scatter_2d_b2_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_scatter_2d_b3_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_scatter_2d_b4_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_scatter_2d_b5_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_scatter_2d_b6_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_scatter_2d_b7_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_scatter_2d_b8_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_scatter_2d_gen_cdp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            end if
        else
            if( op==sparse_op_hermitian ) then
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_symm_h_2d_b2_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_symm_h_2d_b3_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_symm_h_2d_b4_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_symm_h_2d_b5_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_symm_h_2d_b6_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_symm_h_2d_b7_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_symm_h_2d_b8_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_symm_h_2d_gen_cdp(br, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            else
                select case (merge(br, 0_ilp, br == bc))
                case (2)
                    call bsr_symm_2d_b2_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (3)
                    call bsr_symm_2d_b3_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (4)
                    call bsr_symm_2d_b4_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (5)
                    call bsr_symm_2d_b5_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (6)
                    call bsr_symm_2d_b6_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (7)
                    call bsr_symm_2d_b7_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case (8)
                    call bsr_symm_2d_b8_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                case default
                    call bsr_symm_2d_gen_cdp(br, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
                end select
            end if
        end if
    end subroutine


    !! Block kernels. Vectors are viewed through explicit-shape dummies as
    !! vec(block_size, num_blocks) for rank-1 and vec(k, block_size, num_blocks) for rank-2,
    !! which is the same memory as the caller's vec(n) and vec(k, n).

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b2_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(2,*)
        real(sp), intent(inout) :: vec_y(2,*)
        real(sp) :: aux(2)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_sp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b2_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,2,*)
        real(sp), intent(inout) :: vec_y(k,2,*)
        real(sp) :: aux(2,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 2
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_sp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b2_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(2,*)
        real(sp), intent(inout) :: vec_y(2,*)
        real(sp) :: aux(2)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b2_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,2,*)
        real(sp), intent(inout) :: vec_y(k,2,*)
        real(sp) :: aux(2,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b2_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(2,*)
        real(sp), intent(inout) :: vec_y(2,*)
        real(sp) :: aux(2), aux2(2)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_sp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 2
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 2
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b2_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,2,*)
        real(sp), intent(inout) :: vec_y(k,2,*)
        real(sp) :: aux(2,k), aux2(2,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_sp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 2
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 2
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b3_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(3,*)
        real(sp), intent(inout) :: vec_y(3,*)
        real(sp) :: aux(3)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_sp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b3_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,3,*)
        real(sp), intent(inout) :: vec_y(k,3,*)
        real(sp) :: aux(3,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 3
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_sp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b3_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(3,*)
        real(sp), intent(inout) :: vec_y(3,*)
        real(sp) :: aux(3)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b3_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,3,*)
        real(sp), intent(inout) :: vec_y(k,3,*)
        real(sp) :: aux(3,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b3_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(3,*)
        real(sp), intent(inout) :: vec_y(3,*)
        real(sp) :: aux(3), aux2(3)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_sp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 3
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 3
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b3_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,3,*)
        real(sp), intent(inout) :: vec_y(k,3,*)
        real(sp) :: aux(3,k), aux2(3,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_sp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 3
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 3
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b4_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(4,*)
        real(sp), intent(inout) :: vec_y(4,*)
        real(sp) :: aux(4)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_sp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b4_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,4,*)
        real(sp), intent(inout) :: vec_y(k,4,*)
        real(sp) :: aux(4,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 4
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_sp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b4_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(4,*)
        real(sp), intent(inout) :: vec_y(4,*)
        real(sp) :: aux(4)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b4_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,4,*)
        real(sp), intent(inout) :: vec_y(k,4,*)
        real(sp) :: aux(4,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b4_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(4,*)
        real(sp), intent(inout) :: vec_y(4,*)
        real(sp) :: aux(4), aux2(4)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_sp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 4
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 4
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b4_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,4,*)
        real(sp), intent(inout) :: vec_y(k,4,*)
        real(sp) :: aux(4,k), aux2(4,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_sp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 4
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 4
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b5_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(5,*)
        real(sp), intent(inout) :: vec_y(5,*)
        real(sp) :: aux(5)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_sp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b5_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,5,*)
        real(sp), intent(inout) :: vec_y(k,5,*)
        real(sp) :: aux(5,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 5
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_sp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b5_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(5,*)
        real(sp), intent(inout) :: vec_y(5,*)
        real(sp) :: aux(5)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b5_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,5,*)
        real(sp), intent(inout) :: vec_y(k,5,*)
        real(sp) :: aux(5,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b5_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(5,*)
        real(sp), intent(inout) :: vec_y(5,*)
        real(sp) :: aux(5), aux2(5)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_sp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 5
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 5
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b5_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,5,*)
        real(sp), intent(inout) :: vec_y(k,5,*)
        real(sp) :: aux(5,k), aux2(5,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_sp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 5
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 5
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b6_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(6,*)
        real(sp), intent(inout) :: vec_y(6,*)
        real(sp) :: aux(6)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_sp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b6_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,6,*)
        real(sp), intent(inout) :: vec_y(k,6,*)
        real(sp) :: aux(6,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 6
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_sp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b6_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(6,*)
        real(sp), intent(inout) :: vec_y(6,*)
        real(sp) :: aux(6)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b6_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,6,*)
        real(sp), intent(inout) :: vec_y(k,6,*)
        real(sp) :: aux(6,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b6_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(6,*)
        real(sp), intent(inout) :: vec_y(6,*)
        real(sp) :: aux(6), aux2(6)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_sp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 6
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 6
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b6_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,6,*)
        real(sp), intent(inout) :: vec_y(k,6,*)
        real(sp) :: aux(6,k), aux2(6,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_sp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 6
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 6
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b7_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(7,*)
        real(sp), intent(inout) :: vec_y(7,*)
        real(sp) :: aux(7)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_sp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b7_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,7,*)
        real(sp), intent(inout) :: vec_y(k,7,*)
        real(sp) :: aux(7,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 7
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_sp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b7_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(7,*)
        real(sp), intent(inout) :: vec_y(7,*)
        real(sp) :: aux(7)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b7_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,7,*)
        real(sp), intent(inout) :: vec_y(k,7,*)
        real(sp) :: aux(7,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b7_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(7,*)
        real(sp), intent(inout) :: vec_y(7,*)
        real(sp) :: aux(7), aux2(7)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_sp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 7
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 7
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b7_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,7,*)
        real(sp), intent(inout) :: vec_y(k,7,*)
        real(sp) :: aux(7,k), aux2(7,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_sp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 7
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 7
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b8_sp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(8,*)
        real(sp), intent(inout) :: vec_y(8,*)
        real(sp) :: aux(8)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_sp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b8_sp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,8,*)
        real(sp), intent(inout) :: vec_y(k,8,*)
        real(sp) :: aux(8,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 8
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_sp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b8_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(8,*)
        real(sp), intent(inout) :: vec_y(8,*)
        real(sp) :: aux(8)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b8_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,8,*)
        real(sp), intent(inout) :: vec_y(k,8,*)
        real(sp) :: aux(8,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b8_sp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(8,*)
        real(sp), intent(inout) :: vec_y(8,*)
        real(sp) :: aux(8), aux2(8)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_sp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 8
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 8
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b8_sp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,8,*)
        real(sp), intent(inout) :: vec_y(k,8,*)
        real(sp) :: aux(8,k), aux2(8,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_sp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 8
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 8
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_gen_sp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(bc,*)
        real(sp), intent(inout) :: vec_y(br,*)
        real(sp) :: aux(br)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_sp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_gen_sp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,bc,*)
        real(sp), intent(inout) :: vec_y(k,br,*)
        real(sp) :: aux(br,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_sp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, bc
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_sp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_gen_sp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(br,*)
        real(sp), intent(inout) :: vec_y(bc,*)
        real(sp) :: aux(br)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_gen_sp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,br,*)
        real(sp), intent(inout) :: vec_y(k,bc,*)
        real(sp) :: aux(br,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_gen_sp(br, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(br,br,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(br,*)
        real(sp), intent(inout) :: vec_y(br,*)
        real(sp) :: aux(br), aux2(br)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_sp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, br
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, br
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_gen_sp(br, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: data(br,br,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(sp), intent(in) :: vec_x(k,br,*)
        real(sp), intent(inout) :: vec_y(k,br,*)
        real(sp) :: aux(br,k), aux2(br,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_sp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, br
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, br
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b2_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(2,*)
        real(dp), intent(inout) :: vec_y(2,*)
        real(dp) :: aux(2)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_dp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b2_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,2,*)
        real(dp), intent(inout) :: vec_y(k,2,*)
        real(dp) :: aux(2,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 2
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_dp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b2_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(2,*)
        real(dp), intent(inout) :: vec_y(2,*)
        real(dp) :: aux(2)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b2_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,2,*)
        real(dp), intent(inout) :: vec_y(k,2,*)
        real(dp) :: aux(2,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b2_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(2,*)
        real(dp), intent(inout) :: vec_y(2,*)
        real(dp) :: aux(2), aux2(2)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_dp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 2
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 2
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b2_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,2,*)
        real(dp), intent(inout) :: vec_y(k,2,*)
        real(dp) :: aux(2,k), aux2(2,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_dp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 2
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 2
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b3_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(3,*)
        real(dp), intent(inout) :: vec_y(3,*)
        real(dp) :: aux(3)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_dp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b3_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,3,*)
        real(dp), intent(inout) :: vec_y(k,3,*)
        real(dp) :: aux(3,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 3
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_dp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b3_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(3,*)
        real(dp), intent(inout) :: vec_y(3,*)
        real(dp) :: aux(3)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b3_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,3,*)
        real(dp), intent(inout) :: vec_y(k,3,*)
        real(dp) :: aux(3,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b3_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(3,*)
        real(dp), intent(inout) :: vec_y(3,*)
        real(dp) :: aux(3), aux2(3)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_dp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 3
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 3
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b3_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,3,*)
        real(dp), intent(inout) :: vec_y(k,3,*)
        real(dp) :: aux(3,k), aux2(3,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_dp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 3
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 3
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b4_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(4,*)
        real(dp), intent(inout) :: vec_y(4,*)
        real(dp) :: aux(4)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_dp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b4_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,4,*)
        real(dp), intent(inout) :: vec_y(k,4,*)
        real(dp) :: aux(4,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 4
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_dp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b4_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(4,*)
        real(dp), intent(inout) :: vec_y(4,*)
        real(dp) :: aux(4)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b4_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,4,*)
        real(dp), intent(inout) :: vec_y(k,4,*)
        real(dp) :: aux(4,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b4_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(4,*)
        real(dp), intent(inout) :: vec_y(4,*)
        real(dp) :: aux(4), aux2(4)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_dp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 4
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 4
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b4_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,4,*)
        real(dp), intent(inout) :: vec_y(k,4,*)
        real(dp) :: aux(4,k), aux2(4,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_dp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 4
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 4
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b5_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(5,*)
        real(dp), intent(inout) :: vec_y(5,*)
        real(dp) :: aux(5)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_dp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b5_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,5,*)
        real(dp), intent(inout) :: vec_y(k,5,*)
        real(dp) :: aux(5,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 5
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_dp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b5_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(5,*)
        real(dp), intent(inout) :: vec_y(5,*)
        real(dp) :: aux(5)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b5_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,5,*)
        real(dp), intent(inout) :: vec_y(k,5,*)
        real(dp) :: aux(5,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b5_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(5,*)
        real(dp), intent(inout) :: vec_y(5,*)
        real(dp) :: aux(5), aux2(5)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_dp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 5
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 5
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b5_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,5,*)
        real(dp), intent(inout) :: vec_y(k,5,*)
        real(dp) :: aux(5,k), aux2(5,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_dp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 5
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 5
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b6_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(6,*)
        real(dp), intent(inout) :: vec_y(6,*)
        real(dp) :: aux(6)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_dp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b6_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,6,*)
        real(dp), intent(inout) :: vec_y(k,6,*)
        real(dp) :: aux(6,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 6
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_dp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b6_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(6,*)
        real(dp), intent(inout) :: vec_y(6,*)
        real(dp) :: aux(6)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b6_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,6,*)
        real(dp), intent(inout) :: vec_y(k,6,*)
        real(dp) :: aux(6,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b6_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(6,*)
        real(dp), intent(inout) :: vec_y(6,*)
        real(dp) :: aux(6), aux2(6)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_dp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 6
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 6
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b6_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,6,*)
        real(dp), intent(inout) :: vec_y(k,6,*)
        real(dp) :: aux(6,k), aux2(6,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_dp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 6
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 6
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b7_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(7,*)
        real(dp), intent(inout) :: vec_y(7,*)
        real(dp) :: aux(7)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_dp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b7_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,7,*)
        real(dp), intent(inout) :: vec_y(k,7,*)
        real(dp) :: aux(7,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 7
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_dp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b7_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(7,*)
        real(dp), intent(inout) :: vec_y(7,*)
        real(dp) :: aux(7)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b7_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,7,*)
        real(dp), intent(inout) :: vec_y(k,7,*)
        real(dp) :: aux(7,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b7_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(7,*)
        real(dp), intent(inout) :: vec_y(7,*)
        real(dp) :: aux(7), aux2(7)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_dp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 7
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 7
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b7_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,7,*)
        real(dp), intent(inout) :: vec_y(k,7,*)
        real(dp) :: aux(7,k), aux2(7,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_dp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 7
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 7
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b8_dp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(8,*)
        real(dp), intent(inout) :: vec_y(8,*)
        real(dp) :: aux(8)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_dp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b8_dp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,8,*)
        real(dp), intent(inout) :: vec_y(k,8,*)
        real(dp) :: aux(8,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 8
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_dp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b8_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(8,*)
        real(dp), intent(inout) :: vec_y(8,*)
        real(dp) :: aux(8)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b8_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,8,*)
        real(dp), intent(inout) :: vec_y(k,8,*)
        real(dp) :: aux(8,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b8_dp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(8,*)
        real(dp), intent(inout) :: vec_y(8,*)
        real(dp) :: aux(8), aux2(8)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_dp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 8
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 8
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b8_dp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,8,*)
        real(dp), intent(inout) :: vec_y(k,8,*)
        real(dp) :: aux(8,k), aux2(8,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_dp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 8
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 8
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_gen_dp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(bc,*)
        real(dp), intent(inout) :: vec_y(br,*)
        real(dp) :: aux(br)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_dp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_gen_dp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,bc,*)
        real(dp), intent(inout) :: vec_y(k,br,*)
        real(dp) :: aux(br,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_dp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, bc
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_dp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_gen_dp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(br,*)
        real(dp), intent(inout) :: vec_y(bc,*)
        real(dp) :: aux(br)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_gen_dp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,br,*)
        real(dp), intent(inout) :: vec_y(k,bc,*)
        real(dp) :: aux(br,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_gen_dp(br, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(br,br,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(br,*)
        real(dp), intent(inout) :: vec_y(br,*)
        real(dp) :: aux(br), aux2(br)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_dp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, br
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, br
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_gen_dp(br, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: data(br,br,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        real(dp), intent(in) :: vec_x(k,br,*)
        real(dp), intent(inout) :: vec_y(k,br,*)
        real(dp) :: aux(br,k), aux2(br,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_dp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, br
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, br
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b2_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(2,*)
        complex(sp), intent(inout) :: vec_y(2,*)
        complex(sp) :: aux(2)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_csp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b2_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,2,*)
        complex(sp), intent(inout) :: vec_y(k,2,*)
        complex(sp) :: aux(2,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 2
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_csp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b2_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(2,*)
        complex(sp), intent(inout) :: vec_y(2,*)
        complex(sp) :: aux(2)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b2_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,2,*)
        complex(sp), intent(inout) :: vec_y(k,2,*)
        complex(sp) :: aux(2,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b2_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(2,*)
        complex(sp), intent(inout) :: vec_y(2,*)
        complex(sp) :: aux(2), aux2(2)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 2
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 2
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b2_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,2,*)
        complex(sp), intent(inout) :: vec_y(k,2,*)
        complex(sp) :: aux(2,k), aux2(2,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 2
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 2
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b2_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(2,*)
        complex(sp), intent(inout) :: vec_y(2,*)
        complex(sp) :: aux(2)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b2_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,2,*)
        complex(sp), intent(inout) :: vec_y(k,2,*)
        complex(sp) :: aux(2,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b2_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(2,*)
        complex(sp), intent(inout) :: vec_y(2,*)
        complex(sp) :: aux(2), aux2(2)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 2
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 2
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b2_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,2,*)
        complex(sp), intent(inout) :: vec_y(k,2,*)
        complex(sp) :: aux(2,k), aux2(2,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 2
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 2
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b3_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(3,*)
        complex(sp), intent(inout) :: vec_y(3,*)
        complex(sp) :: aux(3)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_csp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b3_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,3,*)
        complex(sp), intent(inout) :: vec_y(k,3,*)
        complex(sp) :: aux(3,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 3
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_csp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b3_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(3,*)
        complex(sp), intent(inout) :: vec_y(3,*)
        complex(sp) :: aux(3)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b3_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,3,*)
        complex(sp), intent(inout) :: vec_y(k,3,*)
        complex(sp) :: aux(3,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b3_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(3,*)
        complex(sp), intent(inout) :: vec_y(3,*)
        complex(sp) :: aux(3), aux2(3)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 3
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 3
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b3_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,3,*)
        complex(sp), intent(inout) :: vec_y(k,3,*)
        complex(sp) :: aux(3,k), aux2(3,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 3
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 3
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b3_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(3,*)
        complex(sp), intent(inout) :: vec_y(3,*)
        complex(sp) :: aux(3)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b3_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,3,*)
        complex(sp), intent(inout) :: vec_y(k,3,*)
        complex(sp) :: aux(3,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b3_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(3,*)
        complex(sp), intent(inout) :: vec_y(3,*)
        complex(sp) :: aux(3), aux2(3)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 3
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 3
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b3_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,3,*)
        complex(sp), intent(inout) :: vec_y(k,3,*)
        complex(sp) :: aux(3,k), aux2(3,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 3
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 3
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b4_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(4,*)
        complex(sp), intent(inout) :: vec_y(4,*)
        complex(sp) :: aux(4)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_csp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b4_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,4,*)
        complex(sp), intent(inout) :: vec_y(k,4,*)
        complex(sp) :: aux(4,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 4
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_csp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b4_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(4,*)
        complex(sp), intent(inout) :: vec_y(4,*)
        complex(sp) :: aux(4)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b4_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,4,*)
        complex(sp), intent(inout) :: vec_y(k,4,*)
        complex(sp) :: aux(4,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b4_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(4,*)
        complex(sp), intent(inout) :: vec_y(4,*)
        complex(sp) :: aux(4), aux2(4)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 4
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 4
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b4_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,4,*)
        complex(sp), intent(inout) :: vec_y(k,4,*)
        complex(sp) :: aux(4,k), aux2(4,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 4
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 4
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b4_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(4,*)
        complex(sp), intent(inout) :: vec_y(4,*)
        complex(sp) :: aux(4)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b4_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,4,*)
        complex(sp), intent(inout) :: vec_y(k,4,*)
        complex(sp) :: aux(4,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b4_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(4,*)
        complex(sp), intent(inout) :: vec_y(4,*)
        complex(sp) :: aux(4), aux2(4)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 4
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 4
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b4_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,4,*)
        complex(sp), intent(inout) :: vec_y(k,4,*)
        complex(sp) :: aux(4,k), aux2(4,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 4
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 4
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b5_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(5,*)
        complex(sp), intent(inout) :: vec_y(5,*)
        complex(sp) :: aux(5)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_csp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b5_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,5,*)
        complex(sp), intent(inout) :: vec_y(k,5,*)
        complex(sp) :: aux(5,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 5
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_csp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b5_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(5,*)
        complex(sp), intent(inout) :: vec_y(5,*)
        complex(sp) :: aux(5)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b5_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,5,*)
        complex(sp), intent(inout) :: vec_y(k,5,*)
        complex(sp) :: aux(5,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b5_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(5,*)
        complex(sp), intent(inout) :: vec_y(5,*)
        complex(sp) :: aux(5), aux2(5)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 5
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 5
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b5_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,5,*)
        complex(sp), intent(inout) :: vec_y(k,5,*)
        complex(sp) :: aux(5,k), aux2(5,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 5
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 5
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b5_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(5,*)
        complex(sp), intent(inout) :: vec_y(5,*)
        complex(sp) :: aux(5)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b5_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,5,*)
        complex(sp), intent(inout) :: vec_y(k,5,*)
        complex(sp) :: aux(5,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b5_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(5,*)
        complex(sp), intent(inout) :: vec_y(5,*)
        complex(sp) :: aux(5), aux2(5)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 5
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 5
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b5_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,5,*)
        complex(sp), intent(inout) :: vec_y(k,5,*)
        complex(sp) :: aux(5,k), aux2(5,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 5
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 5
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b6_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(6,*)
        complex(sp), intent(inout) :: vec_y(6,*)
        complex(sp) :: aux(6)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_csp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b6_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,6,*)
        complex(sp), intent(inout) :: vec_y(k,6,*)
        complex(sp) :: aux(6,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 6
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_csp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b6_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(6,*)
        complex(sp), intent(inout) :: vec_y(6,*)
        complex(sp) :: aux(6)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b6_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,6,*)
        complex(sp), intent(inout) :: vec_y(k,6,*)
        complex(sp) :: aux(6,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b6_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(6,*)
        complex(sp), intent(inout) :: vec_y(6,*)
        complex(sp) :: aux(6), aux2(6)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 6
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 6
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b6_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,6,*)
        complex(sp), intent(inout) :: vec_y(k,6,*)
        complex(sp) :: aux(6,k), aux2(6,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 6
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 6
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b6_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(6,*)
        complex(sp), intent(inout) :: vec_y(6,*)
        complex(sp) :: aux(6)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b6_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,6,*)
        complex(sp), intent(inout) :: vec_y(k,6,*)
        complex(sp) :: aux(6,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b6_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(6,*)
        complex(sp), intent(inout) :: vec_y(6,*)
        complex(sp) :: aux(6), aux2(6)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 6
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 6
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b6_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,6,*)
        complex(sp), intent(inout) :: vec_y(k,6,*)
        complex(sp) :: aux(6,k), aux2(6,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 6
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 6
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b7_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(7,*)
        complex(sp), intent(inout) :: vec_y(7,*)
        complex(sp) :: aux(7)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_csp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b7_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,7,*)
        complex(sp), intent(inout) :: vec_y(k,7,*)
        complex(sp) :: aux(7,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 7
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_csp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b7_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(7,*)
        complex(sp), intent(inout) :: vec_y(7,*)
        complex(sp) :: aux(7)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b7_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,7,*)
        complex(sp), intent(inout) :: vec_y(k,7,*)
        complex(sp) :: aux(7,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b7_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(7,*)
        complex(sp), intent(inout) :: vec_y(7,*)
        complex(sp) :: aux(7), aux2(7)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 7
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 7
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b7_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,7,*)
        complex(sp), intent(inout) :: vec_y(k,7,*)
        complex(sp) :: aux(7,k), aux2(7,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 7
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 7
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b7_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(7,*)
        complex(sp), intent(inout) :: vec_y(7,*)
        complex(sp) :: aux(7)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b7_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,7,*)
        complex(sp), intent(inout) :: vec_y(k,7,*)
        complex(sp) :: aux(7,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b7_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(7,*)
        complex(sp), intent(inout) :: vec_y(7,*)
        complex(sp) :: aux(7), aux2(7)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 7
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 7
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b7_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,7,*)
        complex(sp), intent(inout) :: vec_y(k,7,*)
        complex(sp) :: aux(7,k), aux2(7,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 7
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 7
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b8_csp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(8,*)
        complex(sp), intent(inout) :: vec_y(8,*)
        complex(sp) :: aux(8)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_csp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b8_csp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,8,*)
        complex(sp), intent(inout) :: vec_y(k,8,*)
        complex(sp) :: aux(8,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 8
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_csp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b8_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(8,*)
        complex(sp), intent(inout) :: vec_y(8,*)
        complex(sp) :: aux(8)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b8_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,8,*)
        complex(sp), intent(inout) :: vec_y(k,8,*)
        complex(sp) :: aux(8,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b8_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(8,*)
        complex(sp), intent(inout) :: vec_y(8,*)
        complex(sp) :: aux(8), aux2(8)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 8
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 8
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b8_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,8,*)
        complex(sp), intent(inout) :: vec_y(k,8,*)
        complex(sp) :: aux(8,k), aux2(8,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 8
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 8
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b8_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(8,*)
        complex(sp), intent(inout) :: vec_y(8,*)
        complex(sp) :: aux(8)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b8_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,8,*)
        complex(sp), intent(inout) :: vec_y(k,8,*)
        complex(sp) :: aux(8,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b8_csp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(8,*)
        complex(sp), intent(inout) :: vec_y(8,*)
        complex(sp) :: aux(8), aux2(8)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 8
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 8
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b8_csp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,8,*)
        complex(sp), intent(inout) :: vec_y(k,8,*)
        complex(sp) :: aux(8,k), aux2(8,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 8
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 8
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_gen_csp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(bc,*)
        complex(sp), intent(inout) :: vec_y(br,*)
        complex(sp) :: aux(br)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_csp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_gen_csp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,bc,*)
        complex(sp), intent(inout) :: vec_y(k,br,*)
        complex(sp) :: aux(br,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_csp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, bc
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_csp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_gen_csp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(br,*)
        complex(sp), intent(inout) :: vec_y(bc,*)
        complex(sp) :: aux(br)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_gen_csp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,br,*)
        complex(sp), intent(inout) :: vec_y(k,bc,*)
        complex(sp) :: aux(br,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_gen_csp(br, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(br,br,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(br,*)
        complex(sp), intent(inout) :: vec_y(br,*)
        complex(sp) :: aux(br), aux2(br)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, br
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, br
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_gen_csp(br, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(br,br,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,br,*)
        complex(sp), intent(inout) :: vec_y(k,br,*)
        complex(sp) :: aux(br,k), aux2(br,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, br
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, br
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_gen_csp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(br,*)
        complex(sp), intent(inout) :: vec_y(bc,*)
        complex(sp) :: aux(br)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_gen_csp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,br,*)
        complex(sp), intent(inout) :: vec_y(k,bc,*)
        complex(sp) :: aux(br,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_gen_csp(br, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(br,br,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(br,*)
        complex(sp), intent(inout) :: vec_y(br,*)
        complex(sp) :: aux(br), aux2(br)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_csp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, br
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, br
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_gen_csp(br, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: data(br,br,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(sp), intent(in) :: vec_x(k,br,*)
        complex(sp), intent(inout) :: vec_y(k,br,*)
        complex(sp) :: aux(br,k), aux2(br,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_csp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, br
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, br
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b2_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(2,*)
        complex(dp), intent(inout) :: vec_y(2,*)
        complex(dp) :: aux(2)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_cdp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b2_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,2,*)
        complex(dp), intent(inout) :: vec_y(k,2,*)
        complex(dp) :: aux(2,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 2
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_cdp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b2_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(2,*)
        complex(dp), intent(inout) :: vec_y(2,*)
        complex(dp) :: aux(2)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b2_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,2,*)
        complex(dp), intent(inout) :: vec_y(k,2,*)
        complex(dp) :: aux(2,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b2_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(2,*)
        complex(dp), intent(inout) :: vec_y(2,*)
        complex(dp) :: aux(2), aux2(2)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 2
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 2
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b2_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,2,*)
        complex(dp), intent(inout) :: vec_y(k,2,*)
        complex(dp) :: aux(2,k), aux2(2,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 2
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 2
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b2_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(2,*)
        complex(dp), intent(inout) :: vec_y(2,*)
        complex(dp) :: aux(2)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b2_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,2,*)
        complex(dp), intent(inout) :: vec_y(k,2,*)
        complex(dp) :: aux(2,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 2
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b2_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(2,*)
        complex(dp), intent(inout) :: vec_y(2,*)
        complex(dp) :: aux(2), aux2(2)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 2
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 2
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b2_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(2,2,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,2,*)
        complex(dp), intent(inout) :: vec_y(k,2,*)
        complex(dp) :: aux(2,k), aux2(2,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 2
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 2
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b3_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(3,*)
        complex(dp), intent(inout) :: vec_y(3,*)
        complex(dp) :: aux(3)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_cdp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b3_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,3,*)
        complex(dp), intent(inout) :: vec_y(k,3,*)
        complex(dp) :: aux(3,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 3
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_cdp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b3_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(3,*)
        complex(dp), intent(inout) :: vec_y(3,*)
        complex(dp) :: aux(3)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b3_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,3,*)
        complex(dp), intent(inout) :: vec_y(k,3,*)
        complex(dp) :: aux(3,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b3_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(3,*)
        complex(dp), intent(inout) :: vec_y(3,*)
        complex(dp) :: aux(3), aux2(3)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 3
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 3
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b3_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,3,*)
        complex(dp), intent(inout) :: vec_y(k,3,*)
        complex(dp) :: aux(3,k), aux2(3,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 3
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 3
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b3_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(3,*)
        complex(dp), intent(inout) :: vec_y(3,*)
        complex(dp) :: aux(3)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b3_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,3,*)
        complex(dp), intent(inout) :: vec_y(k,3,*)
        complex(dp) :: aux(3,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 3
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b3_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(3,*)
        complex(dp), intent(inout) :: vec_y(3,*)
        complex(dp) :: aux(3), aux2(3)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 3
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 3
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b3_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(3,3,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,3,*)
        complex(dp), intent(inout) :: vec_y(k,3,*)
        complex(dp) :: aux(3,k), aux2(3,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 3
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 3
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b4_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(4,*)
        complex(dp), intent(inout) :: vec_y(4,*)
        complex(dp) :: aux(4)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_cdp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b4_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,4,*)
        complex(dp), intent(inout) :: vec_y(k,4,*)
        complex(dp) :: aux(4,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 4
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_cdp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b4_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(4,*)
        complex(dp), intent(inout) :: vec_y(4,*)
        complex(dp) :: aux(4)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b4_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,4,*)
        complex(dp), intent(inout) :: vec_y(k,4,*)
        complex(dp) :: aux(4,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b4_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(4,*)
        complex(dp), intent(inout) :: vec_y(4,*)
        complex(dp) :: aux(4), aux2(4)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 4
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 4
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b4_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,4,*)
        complex(dp), intent(inout) :: vec_y(k,4,*)
        complex(dp) :: aux(4,k), aux2(4,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 4
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 4
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b4_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(4,*)
        complex(dp), intent(inout) :: vec_y(4,*)
        complex(dp) :: aux(4)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b4_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,4,*)
        complex(dp), intent(inout) :: vec_y(k,4,*)
        complex(dp) :: aux(4,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 4
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b4_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(4,*)
        complex(dp), intent(inout) :: vec_y(4,*)
        complex(dp) :: aux(4), aux2(4)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 4
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 4
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b4_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(4,4,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,4,*)
        complex(dp), intent(inout) :: vec_y(k,4,*)
        complex(dp) :: aux(4,k), aux2(4,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 4
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 4
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b5_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(5,*)
        complex(dp), intent(inout) :: vec_y(5,*)
        complex(dp) :: aux(5)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_cdp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b5_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,5,*)
        complex(dp), intent(inout) :: vec_y(k,5,*)
        complex(dp) :: aux(5,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 5
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_cdp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b5_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(5,*)
        complex(dp), intent(inout) :: vec_y(5,*)
        complex(dp) :: aux(5)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b5_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,5,*)
        complex(dp), intent(inout) :: vec_y(k,5,*)
        complex(dp) :: aux(5,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b5_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(5,*)
        complex(dp), intent(inout) :: vec_y(5,*)
        complex(dp) :: aux(5), aux2(5)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 5
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 5
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b5_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,5,*)
        complex(dp), intent(inout) :: vec_y(k,5,*)
        complex(dp) :: aux(5,k), aux2(5,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 5
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 5
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b5_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(5,*)
        complex(dp), intent(inout) :: vec_y(5,*)
        complex(dp) :: aux(5)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b5_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,5,*)
        complex(dp), intent(inout) :: vec_y(k,5,*)
        complex(dp) :: aux(5,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 5
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b5_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(5,*)
        complex(dp), intent(inout) :: vec_y(5,*)
        complex(dp) :: aux(5), aux2(5)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 5
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 5
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b5_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(5,5,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,5,*)
        complex(dp), intent(inout) :: vec_y(k,5,*)
        complex(dp) :: aux(5,k), aux2(5,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 5
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 5
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b6_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(6,*)
        complex(dp), intent(inout) :: vec_y(6,*)
        complex(dp) :: aux(6)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_cdp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b6_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,6,*)
        complex(dp), intent(inout) :: vec_y(k,6,*)
        complex(dp) :: aux(6,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 6
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_cdp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b6_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(6,*)
        complex(dp), intent(inout) :: vec_y(6,*)
        complex(dp) :: aux(6)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b6_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,6,*)
        complex(dp), intent(inout) :: vec_y(k,6,*)
        complex(dp) :: aux(6,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b6_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(6,*)
        complex(dp), intent(inout) :: vec_y(6,*)
        complex(dp) :: aux(6), aux2(6)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 6
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 6
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b6_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,6,*)
        complex(dp), intent(inout) :: vec_y(k,6,*)
        complex(dp) :: aux(6,k), aux2(6,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 6
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 6
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b6_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(6,*)
        complex(dp), intent(inout) :: vec_y(6,*)
        complex(dp) :: aux(6)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b6_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,6,*)
        complex(dp), intent(inout) :: vec_y(k,6,*)
        complex(dp) :: aux(6,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 6
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b6_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(6,*)
        complex(dp), intent(inout) :: vec_y(6,*)
        complex(dp) :: aux(6), aux2(6)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 6
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 6
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b6_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(6,6,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,6,*)
        complex(dp), intent(inout) :: vec_y(k,6,*)
        complex(dp) :: aux(6,k), aux2(6,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 6
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 6
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b7_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(7,*)
        complex(dp), intent(inout) :: vec_y(7,*)
        complex(dp) :: aux(7)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_cdp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b7_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,7,*)
        complex(dp), intent(inout) :: vec_y(k,7,*)
        complex(dp) :: aux(7,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 7
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_cdp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b7_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(7,*)
        complex(dp), intent(inout) :: vec_y(7,*)
        complex(dp) :: aux(7)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b7_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,7,*)
        complex(dp), intent(inout) :: vec_y(k,7,*)
        complex(dp) :: aux(7,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b7_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(7,*)
        complex(dp), intent(inout) :: vec_y(7,*)
        complex(dp) :: aux(7), aux2(7)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 7
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 7
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b7_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,7,*)
        complex(dp), intent(inout) :: vec_y(k,7,*)
        complex(dp) :: aux(7,k), aux2(7,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 7
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 7
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b7_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(7,*)
        complex(dp), intent(inout) :: vec_y(7,*)
        complex(dp) :: aux(7)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b7_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,7,*)
        complex(dp), intent(inout) :: vec_y(k,7,*)
        complex(dp) :: aux(7,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 7
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b7_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(7,*)
        complex(dp), intent(inout) :: vec_y(7,*)
        complex(dp) :: aux(7), aux2(7)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 7
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 7
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b7_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(7,7,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,7,*)
        complex(dp), intent(inout) :: vec_y(k,7,*)
        complex(dp) :: aux(7,k), aux2(7,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 7
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 7
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_b8_cdp(nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(8,*)
        complex(dp), intent(inout) :: vec_y(8,*)
        complex(dp) :: aux(8)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_cdp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_b8_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,8,*)
        complex(dp), intent(inout) :: vec_y(k,8,*)
        complex(dp) :: aux(8,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, 8
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_cdp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_b8_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(8,*)
        complex(dp), intent(inout) :: vec_y(8,*)
        complex(dp) :: aux(8)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_b8_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,8,*)
        complex(dp), intent(inout) :: vec_y(k,8,*)
        complex(dp) :: aux(8,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_b8_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(8,*)
        complex(dp), intent(inout) :: vec_y(8,*)
        complex(dp) :: aux(8), aux2(8)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 8
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 8
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_b8_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,8,*)
        complex(dp), intent(inout) :: vec_y(k,8,*)
        complex(dp) :: aux(8,k), aux2(8,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 8
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 8
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_b8_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(8,*)
        complex(dp), intent(inout) :: vec_y(8,*)
        complex(dp) :: aux(8)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_b8_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,8,*)
        complex(dp), intent(inout) :: vec_y(k,8,*)
        complex(dp) :: aux(8,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, 8
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_b8_cdp(nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(8,*)
        complex(dp), intent(inout) :: vec_y(8,*)
        complex(dp) :: aux(8), aux2(8)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, 8
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, 8
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_b8_cdp(k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(8,8,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,8,*)
        complex(dp), intent(inout) :: vec_y(k,8,*)
        complex(dp) :: aux(8,k), aux2(8,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, 8
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, 8
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_1d_gen_cdp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(bc,*)
        complex(dp), intent(inout) :: vec_y(br,*)
        complex(dp) :: aux(br)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    aux = aux + data(:,jj,j) * vec_x(jj,col(j))
                end do
            end do
            if( beta == zero_cdp ) then
                vec_y(:,i) = alpha * aux
            else
                vec_y(:,i) = beta * vec_y(:,i) + alpha * aux
            end if
        end do
    end subroutine

    !> y = beta * y + alpha * A * x
    pure subroutine bsr_gather_2d_gen_cdp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, beta, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,bc,*)
        complex(dp), intent(inout) :: vec_y(k,br,*)
        complex(dp) :: aux(br,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            aux = zero_cdp
            do j = rowptr(i), rowptr(i+1)-1
                do r = 1, k
                    do jj = 1, bc
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,col(j))
                    end do
                end do
            end do
            if( beta == zero_cdp ) then
                do r = 1, k
                    vec_y(r,:,i) = alpha * aux(:,r)
                end do
            else
                do r = 1, k
                    vec_y(r,:,i) = beta * vec_y(r,:,i) + alpha * aux(:,r)
                end do
            end if
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_1d_gen_cdp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(br,*)
        complex(dp), intent(inout) :: vec_y(bc,*)
        complex(dp) :: aux(br)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( data(:,jj,j) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^T * x
    pure subroutine bsr_scatter_2d_gen_cdp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,br,*)
        complex(dp), intent(inout) :: vec_y(k,bc,*)
        complex(dp) :: aux(br,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( data(:,jj,j) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_1d_gen_cdp(br, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(br,br,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(br,*)
        complex(dp), intent(inout) :: vec_y(br,*)
        complex(dp) :: aux(br), aux2(br)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, br
                    aux = aux + data(:,jj,j) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, br
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( data(:,jj,j) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * A * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_2d_gen_cdp(br, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(br,br,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,br,*)
        complex(dp), intent(inout) :: vec_y(k,br,*)
        complex(dp) :: aux(br,k), aux2(br,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, br
                        aux(:,r) = aux(:,r) + data(:,jj,j) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, br
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( data(:,jj,j) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine
    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_1d_gen_cdp(br, bc, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(br,*)
        complex(dp), intent(inout) :: vec_y(bc,*)
        complex(dp) :: aux(br)
        integer(ilp) :: i, j, jj

        do i = 1, nbrow
            aux = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    vec_y(jj,col(j)) = vec_y(jj,col(j)) + sum( conjg(data(:,jj,j)) * aux )
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * A^H * x
    pure subroutine bsr_scatter_h_2d_gen_cdp(br, bc, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br, bc
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(br,bc,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,br,*)
        complex(dp), intent(inout) :: vec_y(k,bc,*)
        complex(dp) :: aux(br,k)
        integer(ilp) :: i, j, jj, r

        do i = 1, nbrow
            do r = 1, k
                aux(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                do jj = 1, bc
                    do r = 1, k
                        vec_y(r,jj,col(j)) = vec_y(r,jj,col(j)) + sum( conjg(data(:,jj,j)) * aux(:,r) )
                    end do
                end do
            end do
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_1d_gen_cdp(br, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(br,br,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(br,*)
        complex(dp), intent(inout) :: vec_y(br,*)
        complex(dp) :: aux(br), aux2(br)
        integer(ilp) :: i, j, jj, bj

        do i = 1, nbrow
            aux  = zero_cdp
            aux2 = alpha * vec_x(:,i)
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do jj = 1, br
                    aux = aux + conjg(data(:,jj,j)) * vec_x(jj,bj)
                end do
                if( bj == i ) cycle
                do jj = 1, br
                    vec_y(jj,bj) = vec_y(jj,bj) + sum( conjg(data(:,jj,j)) * aux2 )
                end do
            end do
            vec_y(:,i) = vec_y(:,i) + alpha * aux
        end do
    end subroutine

    !> y = y + alpha * conjg(A) * x, A symmetric with a single triangle of blocks
    !> stored (lower or upper, the operation is the same) and full diagonal blocks
    pure subroutine bsr_symm_h_2d_gen_cdp(br, k, nbrow, alpha, data, col, rowptr, vec_x, vec_y)
        integer(ilp), intent(in) :: br
        integer(ilp), intent(in) :: k
        integer(ilp), intent(in) :: nbrow
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: data(br,br,*)
        integer(ilp), intent(in) :: col(*), rowptr(*)
        complex(dp), intent(in) :: vec_x(k,br,*)
        complex(dp), intent(inout) :: vec_y(k,br,*)
        complex(dp) :: aux(br,k), aux2(br,k)
        integer(ilp) :: i, j, jj, bj, r

        do i = 1, nbrow
            aux = zero_cdp
            do r = 1, k
                aux2(:,r) = alpha * vec_x(r,:,i)
            end do
            do j = rowptr(i), rowptr(i+1)-1
                bj = col(j)
                do r = 1, k
                    do jj = 1, br
                        aux(:,r) = aux(:,r) + conjg(data(:,jj,j)) * vec_x(r,jj,bj)
                    end do
                end do
                if( bj == i ) cycle
                do jj = 1, br
                    do r = 1, k
                        vec_y(r,jj,bj) = vec_y(r,jj,bj) + sum( conjg(data(:,jj,j)) * aux2(:,r) )
                    end do
                end do
            end do
            do r = 1, k
                vec_y(r,:,i) = vec_y(r,:,i) + alpha * aux(:,r)
            end do
        end do
    end subroutine

end submodule stdlib_sparse_spmv_bsr
