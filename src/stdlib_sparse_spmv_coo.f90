submodule (stdlib_sparse_spmv) stdlib_sparse_spmv_coo
contains

    !! spmv_coo
    module subroutine spmv_coo_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_sp_type), intent(in) :: matrix
        real(sp), intent(in)    :: vec_x(:)
        real(sp), intent(inout) :: vec_y(:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(sp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha

        beta_ = zero_sp
        if(present(beta)) beta_ = beta

        call spmv_kernel_coo_1d_sp(op_, alpha_, &
            matrix%data, matrix%index, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_kernel_coo_1d_sp(op,alpha,data,index,storage,vec_x,beta,vec_y)
        real(sp), intent(in), contiguous :: data(:)
        integer(ilp), intent(in), contiguous :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        real(sp), intent(in), contiguous    :: vec_x(:)
        real(sp), intent(inout), contiguous :: vec_y(:)
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: col_index, k, row_index
        integer(ilp) :: nnz !! number of non-zero values

        nnz = size(index, 2)

        vec_y = beta * vec_y

        select case(op)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha*data(k) * vec_x(row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha*data(k) * vec_x(row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_sp_type), intent(in) :: matrix
        real(sp), intent(in)    :: vec_x(:,:)
        real(sp), intent(inout) :: vec_y(:,:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(sp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha

        beta_ = zero_sp
        if(present(beta)) beta_ = beta

        call spmv_kernel_coo_2d_sp(op_, alpha_, &
            matrix%data, matrix%index, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_kernel_coo_2d_sp(op,alpha,data,index,storage,vec_x,beta,vec_y)
        real(sp), intent(in), contiguous :: data(:)
        integer(ilp), intent(in), contiguous :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        real(sp), intent(in), contiguous    :: vec_x(:,:)
        real(sp), intent(inout), contiguous :: vec_y(:,:)
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: col_index, k, row_index
        integer(ilp) :: nnz !! number of non-zero values

        nnz = size(index, 2)

        vec_y = beta * vec_y

        select case(op)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha*data(k) * vec_x(:,row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha*data(k) * vec_x(:,row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_dp_type), intent(in) :: matrix
        real(dp), intent(in)    :: vec_x(:)
        real(dp), intent(inout) :: vec_y(:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(dp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha

        beta_ = zero_dp
        if(present(beta)) beta_ = beta

        call spmv_kernel_coo_1d_dp(op_, alpha_, &
            matrix%data, matrix%index, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_kernel_coo_1d_dp(op,alpha,data,index,storage,vec_x,beta,vec_y)
        real(dp), intent(in), contiguous :: data(:)
        integer(ilp), intent(in), contiguous :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        real(dp), intent(in), contiguous    :: vec_x(:)
        real(dp), intent(inout), contiguous :: vec_y(:)
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: col_index, k, row_index
        integer(ilp) :: nnz !! number of non-zero values

        nnz = size(index, 2)

        vec_y = beta * vec_y

        select case(op)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha*data(k) * vec_x(row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha*data(k) * vec_x(row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_dp_type), intent(in) :: matrix
        real(dp), intent(in)    :: vec_x(:,:)
        real(dp), intent(inout) :: vec_y(:,:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(dp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha

        beta_ = zero_dp
        if(present(beta)) beta_ = beta

        call spmv_kernel_coo_2d_dp(op_, alpha_, &
            matrix%data, matrix%index, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_kernel_coo_2d_dp(op,alpha,data,index,storage,vec_x,beta,vec_y)
        real(dp), intent(in), contiguous :: data(:)
        integer(ilp), intent(in), contiguous :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        real(dp), intent(in), contiguous    :: vec_x(:,:)
        real(dp), intent(inout), contiguous :: vec_y(:,:)
        real(dp), intent(in) :: alpha
        real(dp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: col_index, k, row_index
        integer(ilp) :: nnz !! number of non-zero values

        nnz = size(index, 2)

        vec_y = beta * vec_y

        select case(op)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha*data(k) * vec_x(:,row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha*data(k) * vec_x(:,row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_csp_type), intent(in) :: matrix
        complex(sp), intent(in)    :: vec_x(:)
        complex(sp), intent(inout) :: vec_y(:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(sp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha

        beta_ = zero_csp
        if(present(beta)) beta_ = beta

        call spmv_kernel_coo_1d_csp(op_, alpha_, &
            matrix%data, matrix%index, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_kernel_coo_1d_csp(op,alpha,data,index,storage,vec_x,beta,vec_y)
        complex(sp), intent(in), contiguous :: data(:)
        integer(ilp), intent(in), contiguous :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        complex(sp), intent(in), contiguous    :: vec_x(:)
        complex(sp), intent(inout), contiguous :: vec_y(:)
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: col_index, k, row_index
        integer(ilp) :: nnz !! number of non-zero values

        nnz = size(index, 2)

        vec_y = beta * vec_y

        select case(op)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha*data(k) * vec_x(row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha*data(k) * vec_x(row_index)
                end do

            end if
        case(sparse_op_hermitian)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*conjg(data(k)) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*conjg(data(k)) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha*conjg(data(k)) * vec_x(row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_csp_type), intent(in) :: matrix
        complex(sp), intent(in)    :: vec_x(:,:)
        complex(sp), intent(inout) :: vec_y(:,:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(sp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha

        beta_ = zero_csp
        if(present(beta)) beta_ = beta

        call spmv_kernel_coo_2d_csp(op_, alpha_, &
            matrix%data, matrix%index, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_kernel_coo_2d_csp(op,alpha,data,index,storage,vec_x,beta,vec_y)
        complex(sp), intent(in), contiguous :: data(:)
        integer(ilp), intent(in), contiguous :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        complex(sp), intent(in), contiguous    :: vec_x(:,:)
        complex(sp), intent(inout), contiguous :: vec_y(:,:)
        complex(sp), intent(in) :: alpha
        complex(sp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: col_index, k, row_index
        integer(ilp) :: nnz !! number of non-zero values

        nnz = size(index, 2)

        vec_y = beta * vec_y

        select case(op)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha*data(k) * vec_x(:,row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha*data(k) * vec_x(:,row_index)
                end do

            end if
        case(sparse_op_hermitian)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*conjg(data(k)) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*conjg(data(k)) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha*conjg(data(k)) * vec_x(:,row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_cdp_type), intent(in) :: matrix
        complex(dp), intent(in)    :: vec_x(:)
        complex(dp), intent(inout) :: vec_y(:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(dp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha

        beta_ = zero_cdp
        if(present(beta)) beta_ = beta

        call spmv_kernel_coo_1d_cdp(op_, alpha_, &
            matrix%data, matrix%index, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_kernel_coo_1d_cdp(op,alpha,data,index,storage,vec_x,beta,vec_y)
        complex(dp), intent(in), contiguous :: data(:)
        integer(ilp), intent(in), contiguous :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        complex(dp), intent(in), contiguous    :: vec_x(:)
        complex(dp), intent(inout), contiguous :: vec_y(:)
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: col_index, k, row_index
        integer(ilp) :: nnz !! number of non-zero values

        nnz = size(index, 2)

        vec_y = beta * vec_y

        select case(op)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha*data(k) * vec_x(row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*data(k) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha*data(k) * vec_x(row_index)
                end do

            end if
        case(sparse_op_hermitian)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*conjg(data(k)) * vec_x(col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(row_index) = vec_y(row_index) + alpha*conjg(data(k)) * vec_x(col_index)
                    if( row_index==col_index ) cycle
                    vec_y(col_index) = vec_y(col_index) + alpha*conjg(data(k)) * vec_x(row_index)
                end do

            end if
        end select
    end subroutine

    module subroutine spmv_coo_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(COO_cdp_type), intent(in) :: matrix
        complex(dp), intent(in)    :: vec_x(:,:)
        complex(dp), intent(inout) :: vec_y(:,:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(dp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha

        beta_ = zero_cdp
        if(present(beta)) beta_ = beta

        call spmv_kernel_coo_2d_cdp(op_, alpha_, &
            matrix%data, matrix%index, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

    module subroutine spmv_kernel_coo_2d_cdp(op,alpha,data,index,storage,vec_x,beta,vec_y)
        complex(dp), intent(in), contiguous :: data(:)
        integer(ilp), intent(in), contiguous :: index(:,:) !! Matrix coordinates index(2,nnz)
        integer, intent(in) :: storage !! storage
        complex(dp), intent(in), contiguous    :: vec_x(:,:)
        complex(dp), intent(inout), contiguous :: vec_y(:,:)
        complex(dp), intent(in) :: alpha
        complex(dp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: col_index, k, row_index
        integer(ilp) :: nnz !! number of non-zero values

        nnz = size(index, 2)

        vec_y = beta * vec_y

        select case(op)
        case(sparse_op_none)
            if(storage == sparse_full) then
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    row_index = index(1,k)
                    col_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha*data(k) * vec_x(:,row_index)
                end do

            end if
        case(sparse_op_transpose)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*data(k) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha*data(k) * vec_x(:,row_index)
                end do

            end if
        case(sparse_op_hermitian)
            if(storage == sparse_full) then
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*conjg(data(k)) * vec_x(:,col_index)
                end do

            else 
                do k = 1, nnz
                    col_index = index(1,k)
                    row_index = index(2,k)
                    vec_y(:,row_index) = vec_y(:,row_index) + alpha*conjg(data(k)) * vec_x(:,col_index)
                    if( row_index==col_index ) cycle
                    vec_y(:,col_index) = vec_y(:,col_index) + alpha*conjg(data(k)) * vec_x(:,row_index)
                end do

            end if
        end select
    end subroutine


end submodule stdlib_sparse_spmv_coo
