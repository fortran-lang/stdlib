submodule (stdlib_sparse_spmv) stdlib_sparse_spmv_ell
contains

    !! spmv_ell
    module subroutine spmv_ell_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_sp_type), intent(in) :: matrix
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

        call spmv_kernel_ell_1d_sp(op_,alpha_, &
            matrix%data,matrix%index,matrix%storage, &
            vec_x,beta_,vec_y)

    end subroutine
    
    module subroutine spmv_kernel_ell_1d_sp(op,alpha,data,index,storage,vec_x,beta,vec_y)
        real(sp), intent(in), contiguous :: data(:,:)
        integer(ilp), intent(in), contiguous :: index(:,:)
        integer, intent(in) :: storage
        real(sp), intent(in), contiguous    :: vec_x(:)
        real(sp), intent(inout), contiguous :: vec_y(:)
        real(sp), intent(in) :: alpha
        real(sp), intent(in) :: beta
        character(1), intent(in) :: op
        integer(ilp) :: i, j, k
        integer(ilp) :: nrows
        integer :: mnz_p_row
        
        nrows = size(index, 1)
        mnz_p_row = size(index, 2)

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(i) = vec_y(i) + alpha*data(i,k) * vec_x(j)
                    end do
                end do
            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(j) = vec_y(j) + alpha*data(i,k) * vec_x(i)
                    end do
                end do
            else if( storage /= sparse_full .and. op/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(i) = vec_y(i) + alpha*data(i,k) * vec_x(j)
                        if(i==j) cycle 
                        vec_y(j) = vec_y(j) + alpha*data(i,k) * vec_x(i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_sp_type), intent(in) :: matrix
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

        call spmv_kernel_ell_2d_sp(op_,alpha_, &
            matrix%data,matrix%index,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j, k
        integer(ilp) :: nrows
        integer :: mnz_p_row
        
        nrows = size(index, 1)
        mnz_p_row = size(index, 2)

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,i) = vec_y(:,i) + alpha*data(i,k) * vec_x(:,j)
                    end do
                end do
            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,j) = vec_y(:,j) + alpha*data(i,k) * vec_x(:,i)
                    end do
                end do
            else if( storage /= sparse_full .and. op/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(:,i) = vec_y(:,i) + alpha*data(i,k) * vec_x(:,j)
                        if(i==j) cycle 
                        vec_y(:,j) = vec_y(:,j) + alpha*data(i,k) * vec_x(:,i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_dp_type), intent(in) :: matrix
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

        call spmv_kernel_ell_1d_dp(op_,alpha_, &
            matrix%data,matrix%index,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j, k
        integer(ilp) :: nrows
        integer :: mnz_p_row
        
        nrows = size(index, 1)
        mnz_p_row = size(index, 2)

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(i) = vec_y(i) + alpha*data(i,k) * vec_x(j)
                    end do
                end do
            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(j) = vec_y(j) + alpha*data(i,k) * vec_x(i)
                    end do
                end do
            else if( storage /= sparse_full .and. op/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(i) = vec_y(i) + alpha*data(i,k) * vec_x(j)
                        if(i==j) cycle 
                        vec_y(j) = vec_y(j) + alpha*data(i,k) * vec_x(i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_dp_type), intent(in) :: matrix
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

        call spmv_kernel_ell_2d_dp(op_,alpha_, &
            matrix%data,matrix%index,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j, k
        integer(ilp) :: nrows
        integer :: mnz_p_row
        
        nrows = size(index, 1)
        mnz_p_row = size(index, 2)

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,i) = vec_y(:,i) + alpha*data(i,k) * vec_x(:,j)
                    end do
                end do
            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,j) = vec_y(:,j) + alpha*data(i,k) * vec_x(:,i)
                    end do
                end do
            else if( storage /= sparse_full .and. op/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(:,i) = vec_y(:,i) + alpha*data(i,k) * vec_x(:,j)
                        if(i==j) cycle 
                        vec_y(:,j) = vec_y(:,j) + alpha*data(i,k) * vec_x(:,i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_csp_type), intent(in) :: matrix
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

        beta_ = zero_sp
        if(present(beta)) beta_ = beta

        call spmv_kernel_ell_1d_csp(op_,alpha_, &
            matrix%data,matrix%index,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j, k
        integer(ilp) :: nrows
        integer :: mnz_p_row
        
        nrows = size(index, 1)
        mnz_p_row = size(index, 2)

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(i) = vec_y(i) + alpha*data(i,k) * vec_x(j)
                    end do
                end do
            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(j) = vec_y(j) + alpha*data(i,k) * vec_x(i)
                    end do
                end do
            else if( storage /= sparse_full .and. op/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(i) = vec_y(i) + alpha*data(i,k) * vec_x(j)
                        if(i==j) cycle 
                        vec_y(j) = vec_y(j) + alpha*data(i,k) * vec_x(i)
                    end do
                end do
            else if( storage == sparse_full .and. op==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(j) = vec_y(j) + alpha*conjg(data(i,k)) * vec_x(i)
                    end do
                end do
            else if( storage /= sparse_full .and. op==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(i) = vec_y(i) + alpha*conjg(data(i,k)) * vec_x(j)
                        if(i==j) cycle 
                        vec_y(j) = vec_y(j) + alpha*conjg(data(i,k)) * vec_x(i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_csp_type), intent(in) :: matrix
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

        beta_ = zero_sp
        if(present(beta)) beta_ = beta

        call spmv_kernel_ell_2d_csp(op_,alpha_, &
            matrix%data,matrix%index,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j, k
        integer(ilp) :: nrows
        integer :: mnz_p_row
        
        nrows = size(index, 1)
        mnz_p_row = size(index, 2)

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,i) = vec_y(:,i) + alpha*data(i,k) * vec_x(:,j)
                    end do
                end do
            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,j) = vec_y(:,j) + alpha*data(i,k) * vec_x(:,i)
                    end do
                end do
            else if( storage /= sparse_full .and. op/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(:,i) = vec_y(:,i) + alpha*data(i,k) * vec_x(:,j)
                        if(i==j) cycle 
                        vec_y(:,j) = vec_y(:,j) + alpha*data(i,k) * vec_x(:,i)
                    end do
                end do
            else if( storage == sparse_full .and. op==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,j) = vec_y(:,j) + alpha*conjg(data(i,k)) * vec_x(:,i)
                    end do
                end do
            else if( storage /= sparse_full .and. op==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(:,i) = vec_y(:,i) + alpha*conjg(data(i,k)) * vec_x(:,j)
                        if(i==j) cycle 
                        vec_y(:,j) = vec_y(:,j) + alpha*conjg(data(i,k)) * vec_x(:,i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_cdp_type), intent(in) :: matrix
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

        beta_ = zero_dp
        if(present(beta)) beta_ = beta

        call spmv_kernel_ell_1d_cdp(op_,alpha_, &
            matrix%data,matrix%index,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j, k
        integer(ilp) :: nrows
        integer :: mnz_p_row
        
        nrows = size(index, 1)
        mnz_p_row = size(index, 2)

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(i) = vec_y(i) + alpha*data(i,k) * vec_x(j)
                    end do
                end do
            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(j) = vec_y(j) + alpha*data(i,k) * vec_x(i)
                    end do
                end do
            else if( storage /= sparse_full .and. op/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(i) = vec_y(i) + alpha*data(i,k) * vec_x(j)
                        if(i==j) cycle 
                        vec_y(j) = vec_y(j) + alpha*data(i,k) * vec_x(i)
                    end do
                end do
            else if( storage == sparse_full .and. op==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(j) = vec_y(j) + alpha*conjg(data(i,k)) * vec_x(i)
                    end do
                end do
            else if( storage /= sparse_full .and. op==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(i) = vec_y(i) + alpha*conjg(data(i,k)) * vec_x(j)
                        if(i==j) cycle 
                        vec_y(j) = vec_y(j) + alpha*conjg(data(i,k)) * vec_x(i)
                    end do
                end do
            end if
    end subroutine

    module subroutine spmv_ell_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(ELL_cdp_type), intent(in) :: matrix
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

        beta_ = zero_dp
        if(present(beta)) beta_ = beta

        call spmv_kernel_ell_2d_cdp(op_,alpha_, &
            matrix%data,matrix%index,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j, k
        integer(ilp) :: nrows
        integer :: mnz_p_row
        
        nrows = size(index, 1)
        mnz_p_row = size(index, 2)

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,i) = vec_y(:,i) + alpha*data(i,k) * vec_x(:,j)
                    end do
                end do
            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,j) = vec_y(:,j) + alpha*data(i,k) * vec_x(:,i)
                    end do
                end do
            else if( storage /= sparse_full .and. op/=sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(:,i) = vec_y(:,i) + alpha*data(i,k) * vec_x(:,j)
                        if(i==j) cycle 
                        vec_y(:,j) = vec_y(:,j) + alpha*data(i,k) * vec_x(:,i)
                    end do
                end do
            else if( storage == sparse_full .and. op==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j>0) vec_y(:,j) = vec_y(:,j) + alpha*conjg(data(i,k)) * vec_x(:,i)
                    end do
                end do
            else if( storage /= sparse_full .and. op==sparse_op_hermitian ) then
                do i = 1, nrows
                    do k = 1, MNZ_P_ROW
                        j = index(i,k)
                        if(j<=0) cycle
                        vec_y(:,i) = vec_y(:,i) + alpha*conjg(data(i,k)) * vec_x(:,j)
                        if(i==j) cycle 
                        vec_y(:,j) = vec_y(:,j) + alpha*conjg(data(i,k)) * vec_x(:,i)
                    end do
                end do
            end if
    end subroutine


end submodule stdlib_sparse_spmv_ell
