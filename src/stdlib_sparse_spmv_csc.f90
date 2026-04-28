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
        real(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        real(sp) :: aux

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_sp
        endif

        associate( data => matrix%data, colptr => matrix%colptr, row => matrix%row, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha_ * vec_x(j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(row(i)) = vec_y(row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_sp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                    end do
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha_ * data(i) * vec_x(j)
                    end do
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_sp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha_ * data(i) * vec_x(j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(j)
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            end if
        end associate
    end subroutine
    
    module subroutine spmv_csc_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_sp_type), intent(in) :: matrix
        real(sp), intent(in)    :: vec_x(:,:)
        real(sp), intent(inout) :: vec_y(:,:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        real(sp) :: aux(size(vec_x,dim=1))

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_sp
        endif

        associate( data => matrix%data, colptr => matrix%colptr, row => matrix%row, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha_ * vec_x(:,j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(:,row(i)) = vec_y(:,row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_sp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(:,j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha_ * data(i) * vec_x(:,j)
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_sp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha_ * data(i) * vec_x(:,j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(:,j)
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            end if
        end associate
    end subroutine
    
    module subroutine spmv_csc_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_dp_type), intent(in) :: matrix
        real(dp), intent(in)    :: vec_x(:)
        real(dp), intent(inout) :: vec_y(:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        real(dp) :: aux

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_dp
        endif

        associate( data => matrix%data, colptr => matrix%colptr, row => matrix%row, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha_ * vec_x(j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(row(i)) = vec_y(row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_dp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                    end do
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha_ * data(i) * vec_x(j)
                    end do
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_dp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha_ * data(i) * vec_x(j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(j)
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            end if
        end associate
    end subroutine
    
    module subroutine spmv_csc_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_dp_type), intent(in) :: matrix
        real(dp), intent(in)    :: vec_x(:,:)
        real(dp), intent(inout) :: vec_y(:,:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        real(dp) :: aux(size(vec_x,dim=1))

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_dp
        endif

        associate( data => matrix%data, colptr => matrix%colptr, row => matrix%row, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha_ * vec_x(:,j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(:,row(i)) = vec_y(:,row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_dp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(:,j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha_ * data(i) * vec_x(:,j)
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_dp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha_ * data(i) * vec_x(:,j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(:,j)
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            end if
        end associate
    end subroutine
    
    module subroutine spmv_csc_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_csp_type), intent(in) :: matrix
        complex(sp), intent(in)    :: vec_x(:)
        complex(sp), intent(inout) :: vec_y(:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        complex(sp) :: aux

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_csp
        endif

        associate( data => matrix%data, colptr => matrix%colptr, row => matrix%row, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha_ * vec_x(j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(row(i)) = vec_y(row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_sp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                    end do
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha_ * data(i) * vec_x(j)
                    end do
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_csp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha_ * data(i) * vec_x(j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(j)
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_full .and. op_==sparse_op_hermitian ) then
                do concurrent(j=1:ncols)
                    aux = zero_sp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(row(i))
                    end do
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_lower .and. op_==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(j) * conjg(data(colptr(j)))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha_ * conjg(data(i)) * vec_x(j)
                    end do
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_upper .and. op_==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_csp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + conjg(data(i)) * vec_x(j)
                        vec_y(row(i)) = vec_y(row(i)) + alpha_ * conjg(data(i)) * vec_x(j)
                    end do
                    aux = aux + conjg(data(colptr(j+1)-1)) * vec_x(j)
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do
            end if
        end associate
    end subroutine
    
    module subroutine spmv_csc_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_csp_type), intent(in) :: matrix
        complex(sp), intent(in)    :: vec_x(:,:)
        complex(sp), intent(inout) :: vec_y(:,:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        complex(sp) :: aux(size(vec_x,dim=1))

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_csp
        endif

        associate( data => matrix%data, colptr => matrix%colptr, row => matrix%row, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha_ * vec_x(:,j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(:,row(i)) = vec_y(:,row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_sp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(:,j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha_ * data(i) * vec_x(:,j)
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_csp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha_ * data(i) * vec_x(:,j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(:,j)
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_full .and. op_==sparse_op_hermitian ) then
                do concurrent(j=1:ncols)
                    aux = zero_sp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(:,row(i))
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_lower .and. op_==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(:,j) * conjg(data(colptr(j)))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha_ * conjg(data(i)) * vec_x(:,j)
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_upper .and. op_==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_csp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + conjg(data(i)) * vec_x(:,j)
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha_ * conjg(data(i)) * vec_x(:,j)
                    end do
                    aux = aux + conjg(data(colptr(j+1)-1)) * vec_x(:,j)
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do
            end if
        end associate
    end subroutine
    
    module subroutine spmv_csc_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_cdp_type), intent(in) :: matrix
        complex(dp), intent(in)    :: vec_x(:)
        complex(dp), intent(inout) :: vec_y(:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        complex(dp) :: aux

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_cdp
        endif

        associate( data => matrix%data, colptr => matrix%colptr, row => matrix%row, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha_ * vec_x(j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(row(i)) = vec_y(row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_dp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                    end do
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha_ * data(i) * vec_x(j)
                    end do
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_cdp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha_ * data(i) * vec_x(j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(j)
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_full .and. op_==sparse_op_hermitian ) then
                do concurrent(j=1:ncols)
                    aux = zero_dp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(row(i))
                    end do
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_lower .and. op_==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(j) * conjg(data(colptr(j)))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(row(i))
                        vec_y(row(i)) = vec_y(row(i)) + alpha_ * conjg(data(i)) * vec_x(j)
                    end do
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do

            else if( storage == sparse_upper .and. op_==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_cdp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + conjg(data(i)) * vec_x(j)
                        vec_y(row(i)) = vec_y(row(i)) + alpha_ * conjg(data(i)) * vec_x(j)
                    end do
                    aux = aux + conjg(data(colptr(j+1)-1)) * vec_x(j)
                    vec_y(j) = vec_y(j) + alpha_ * aux
                end do
            end if
        end associate
    end subroutine
    
    module subroutine spmv_csc_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSC_cdp_type), intent(in) :: matrix
        complex(dp), intent(in)    :: vec_x(:,:)
        complex(dp), intent(inout) :: vec_y(:,:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        complex(dp) :: aux(size(vec_x,dim=1))

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_cdp
        endif

        associate( data => matrix%data, colptr => matrix%colptr, row => matrix%row, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do concurrent(j=1:ncols)
                    aux = alpha_ * vec_x(:,j)
                    do i = colptr(j), colptr(j+1)-1
                        vec_y(:,row(i)) = vec_y(:,row(i)) + data(i) * aux
                    end do
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do concurrent(j=1:ncols)
                    aux = zero_dp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(:,j) * data(colptr(j))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha_ * data(i) * vec_x(:,j)
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_cdp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + data(i) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha_ * data(i) * vec_x(:,j)
                    end do
                    aux = aux + data(colptr(j+1)-1) * vec_x(:,j)
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_full .and. op_==sparse_op_hermitian ) then
                do concurrent(j=1:ncols)
                    aux = zero_dp
                    do i = colptr(j), colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(:,row(i))
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_lower .and. op_==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = vec_x(:,j) * conjg(data(colptr(j)))
                    do i = colptr(j)+1, colptr(j+1)-1
                        aux = aux + conjg(data(i)) * vec_x(:,row(i))
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha_ * conjg(data(i)) * vec_x(:,j)
                    end do
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do

            else if( storage == sparse_upper .and. op_==sparse_op_hermitian )then
                do j = 1 , ncols
                    aux  = zero_cdp
                    do i = colptr(j), colptr(j+1)-2
                        aux = aux + conjg(data(i)) * vec_x(:,j)
                        vec_y(:,row(i)) = vec_y(:,row(i)) + alpha_ * conjg(data(i)) * vec_x(:,j)
                    end do
                    aux = aux + conjg(data(colptr(j+1)-1)) * vec_x(:,j)
                    vec_y(:,j) = vec_y(:,j) + alpha_ * aux
                end do
            end if
        end associate
    end subroutine
    

end submodule stdlib_sparse_spmv_csc