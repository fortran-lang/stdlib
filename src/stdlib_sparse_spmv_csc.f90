submodule (stdlib_sparse_spmv) stdlib_sparse_spmv_csc
contains

    !! spmv_csc
    module subroutine spmv_csc_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_sp_type), intent(in) :: matrix
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

        call spmv_kernel_csc_1d_sp(op_,alpha_, &
            matrix%data,matrix%colptr,matrix%row,matrix%storage, &
            vec_x,beta_,vec_y)

    end subroutine
    
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
        integer(ilp) :: i, j
        integer(ilp) :: ncols
        real(sp) :: aux

        ncols = size(colptr)-1

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha * vec_x(j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(row(i)) = vec_y(row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_sp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                    end do
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha * data(i) * vec_x(j)
                    end do
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_sp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha * data(i) * vec_x(j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(j)
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            end if
    end subroutine
    module subroutine spmv_csc_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_sp_type), intent(in) :: matrix
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

        call spmv_kernel_csc_2d_sp(op_,alpha_, &
            matrix%data,matrix%colptr,matrix%row,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: ncols
        real(sp) :: aux(size(vec_x,dim=1))

        ncols = size(colptr)-1

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha * vec_x(:,j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(:,row(i)) = vec_y(:,row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_sp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(:,j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha * data(i) * vec_x(:,j)
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_sp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha * data(i) * vec_x(:,j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(:,j)
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            end if
    end subroutine
    module subroutine spmv_csc_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_dp_type), intent(in) :: matrix
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

        call spmv_kernel_csc_1d_dp(op_,alpha_, &
            matrix%data,matrix%colptr,matrix%row,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: ncols
        real(dp) :: aux

        ncols = size(colptr)-1

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha * vec_x(j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(row(i)) = vec_y(row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_dp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                    end do
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha * data(i) * vec_x(j)
                    end do
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_dp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha * data(i) * vec_x(j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(j)
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            end if
    end subroutine
    module subroutine spmv_csc_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_dp_type), intent(in) :: matrix
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

        call spmv_kernel_csc_2d_dp(op_,alpha_, &
            matrix%data,matrix%colptr,matrix%row,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: ncols
        real(dp) :: aux(size(vec_x,dim=1))

        ncols = size(colptr)-1

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha * vec_x(:,j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(:,row(i)) = vec_y(:,row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_dp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(:,j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha * data(i) * vec_x(:,j)
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_dp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha * data(i) * vec_x(:,j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(:,j)
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            end if
    end subroutine
    module subroutine spmv_csc_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_csp_type), intent(in) :: matrix
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

        call spmv_kernel_csc_1d_csp(op_,alpha_, &
            matrix%data,matrix%colptr,matrix%row,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: ncols
        complex(sp) :: aux

        ncols = size(colptr)-1

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha * vec_x(j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(row(i)) = vec_y(row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_sp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                    end do
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha * data(i) * vec_x(j)
                    end do
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_csp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha * data(i) * vec_x(j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(j)
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_full .and. op==sparse_op_hermitian ) then
                do concurrent(j=1:ncols)
                    aux = zero_sp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(row(i))
                    end do
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_lower .and. op==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(j) * conjg(data(colptr(j)))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha * conjg(data(i)) * vec_x(j)
                    end do
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_upper .and. op==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_csp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + conjg(data(i)) * vec_x(j)
                        vec_y(row(i)) = vec_y(row(i)) + alpha * conjg(data(i)) * vec_x(j)
                    end do
                    aux = aux + conjg(data(colptr(j+1)-1)) * vec_x(j)
                    vec_y(j) = vec_y(j) + alpha * aux
                end do
            end if
    end subroutine
    module subroutine spmv_csc_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_csp_type), intent(in) :: matrix
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

        call spmv_kernel_csc_2d_csp(op_,alpha_, &
            matrix%data,matrix%colptr,matrix%row,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: ncols
        complex(sp) :: aux(size(vec_x,dim=1))

        ncols = size(colptr)-1

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha * vec_x(:,j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(:,row(i)) = vec_y(:,row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_sp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(:,j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha * data(i) * vec_x(:,j)
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_csp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha * data(i) * vec_x(:,j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(:,j)
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_full .and. op==sparse_op_hermitian ) then
                do concurrent(j=1:ncols)
                    aux = zero_sp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(:,row(i))
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_lower .and. op==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(:,j) * conjg(data(colptr(j)))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha * conjg(data(i)) * vec_x(:,j)
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_upper .and. op==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_csp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + conjg(data(i)) * vec_x(:,j)
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha * conjg(data(i)) * vec_x(:,j)
                    end do
                    aux = aux + conjg(data(colptr(j+1)-1)) * vec_x(:,j)
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do
            end if
    end subroutine
    module subroutine spmv_csc_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_cdp_type), intent(in) :: matrix
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

        call spmv_kernel_csc_1d_cdp(op_,alpha_, &
            matrix%data,matrix%colptr,matrix%row,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: ncols
        complex(dp) :: aux

        ncols = size(colptr)-1

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha * vec_x(j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(row(i)) = vec_y(row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_dp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                    end do
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha * data(i) * vec_x(j)
                    end do
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_cdp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha * data(i) * vec_x(j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(j)
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_full .and. op==sparse_op_hermitian ) then
                do concurrent(j=1:ncols)
                    aux = zero_dp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(row(i))
                    end do
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_lower .and. op==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(j) * conjg(data(colptr(j)))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha * conjg(data(i)) * vec_x(j)
                    end do
                    vec_y(j) = vec_y(j) + alpha * aux
                end do

            else if( storage == sparse_upper .and. op==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_cdp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + conjg(data(i)) * vec_x(j)
                        vec_y(row(i)) = vec_y(row(i)) + alpha * conjg(data(i)) * vec_x(j)
                    end do
                    aux = aux + conjg(data(colptr(j+1)-1)) * vec_x(j)
                    vec_y(j) = vec_y(j) + alpha * aux
                end do
            end if
    end subroutine
    module subroutine spmv_csc_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_cdp_type), intent(in) :: matrix
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

        call spmv_kernel_csc_2d_cdp(op_,alpha_, &
            matrix%data,matrix%colptr,matrix%row,matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: ncols
        complex(dp) :: aux(size(vec_x,dim=1))

        ncols = size(colptr)-1

        vec_y = beta * vec_y

            if( storage == sparse_full .and. op==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha * vec_x(:,j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(:,row(i)) = vec_y(:,row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_dp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(:,j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha * data(i) * vec_x(:,j)
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_cdp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha * data(i) * vec_x(:,j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(:,j)
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_full .and. op==sparse_op_hermitian ) then
                do concurrent(j=1:ncols)
                    aux = zero_dp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(:,row(i))
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_lower .and. op==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(:,j) * conjg(data(colptr(j)))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha * conjg(data(i)) * vec_x(:,j)
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do

            else if( storage == sparse_upper .and. op==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_cdp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + conjg(data(i)) * vec_x(:,j)
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha * conjg(data(i)) * vec_x(:,j)
                    end do
                    aux = aux + conjg(data(colptr(j+1)-1)) * vec_x(:,j)
                    vec_y(:,j) = vec_y(:,j) + alpha * aux
                end do
            end if
    end subroutine

end submodule stdlib_sparse_spmv_csc
