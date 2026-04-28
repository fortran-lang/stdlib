submodule (stdlib_sparse_spmv) stdlib_sparse_spmv_csr
contains

    !! spmv_csr
    module subroutine spmv_csr_1d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_sp_type), intent(in) :: matrix
        real(sp), intent(in)    :: vec_x(:)
        real(sp), intent(inout) :: vec_y(:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        real(sp) :: aux, aux2
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_sp
        endif

        associate( data => matrix%data, col => matrix%col, rowptr => matrix%rowptr, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
    
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_sp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                    end do
                    vec_y(i) = vec_y(i) + alpha_ * aux
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha_ * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_sp
                    aux2 = alpha_ * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    aux = alpha_ * aux + data(j) * aux2
                    vec_y(i) = vec_y(i) + aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(i) * data(rowptr(i))
                    aux2 = alpha_ * vec_x(i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    vec_y(i) = vec_y(i) + alpha_ * aux
                end do
                
            end if
        end associate
    end subroutine
    
    module subroutine spmv_csr_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_sp_type), intent(in) :: matrix
        real(sp), intent(in)    :: vec_x(:,:)
        real(sp), intent(inout) :: vec_y(:,:)
        real(sp), intent(in), optional :: alpha
        real(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        real(sp) :: aux(size(vec_x,dim=1)), aux2(size(vec_x,dim=1))
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_sp
        endif

        associate( data => matrix%data, col => matrix%col, rowptr => matrix%rowptr, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
    
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_sp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha_ * aux
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha_ * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_sp
                    aux2 = alpha_ * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    aux = alpha_ * aux + data(j) * aux2
                    vec_y(:,i) = vec_y(:,i) + aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(:,i) * data(rowptr(i))
                    aux2 = alpha_ * vec_x(:,i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha_ * aux
                end do
                
            end if
        end associate
    end subroutine
    
    module subroutine spmv_csr_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_dp_type), intent(in) :: matrix
        real(dp), intent(in)    :: vec_x(:)
        real(dp), intent(inout) :: vec_y(:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        real(dp) :: aux, aux2
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_dp
        endif

        associate( data => matrix%data, col => matrix%col, rowptr => matrix%rowptr, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
    
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_dp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                    end do
                    vec_y(i) = vec_y(i) + alpha_ * aux
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha_ * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_dp
                    aux2 = alpha_ * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    aux = alpha_ * aux + data(j) * aux2
                    vec_y(i) = vec_y(i) + aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(i) * data(rowptr(i))
                    aux2 = alpha_ * vec_x(i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    vec_y(i) = vec_y(i) + alpha_ * aux
                end do
                
            end if
        end associate
    end subroutine
    
    module subroutine spmv_csr_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_dp_type), intent(in) :: matrix
        real(dp), intent(in)    :: vec_x(:,:)
        real(dp), intent(inout) :: vec_y(:,:)
        real(dp), intent(in), optional :: alpha
        real(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        real(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        real(dp) :: aux(size(vec_x,dim=1)), aux2(size(vec_x,dim=1))
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_dp
        endif

        associate( data => matrix%data, col => matrix%col, rowptr => matrix%rowptr, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
    
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_dp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha_ * aux
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha_ * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_dp
                    aux2 = alpha_ * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    aux = alpha_ * aux + data(j) * aux2
                    vec_y(:,i) = vec_y(:,i) + aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(:,i) * data(rowptr(i))
                    aux2 = alpha_ * vec_x(:,i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha_ * aux
                end do
                
            end if
        end associate
    end subroutine
    
    module subroutine spmv_csr_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_csp_type), intent(in) :: matrix
        complex(sp), intent(in)    :: vec_x(:)
        complex(sp), intent(inout) :: vec_y(:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        complex(sp) :: aux, aux2
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_csp
        endif

        associate( data => matrix%data, col => matrix%col, rowptr => matrix%rowptr, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
    
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_sp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                    end do
                    vec_y(i) = vec_y(i) + alpha_ * aux
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha_ * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_csp
                    aux2 = alpha_ * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    aux = alpha_ * aux + data(j) * aux2
                    vec_y(i) = vec_y(i) + aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(i) * data(rowptr(i))
                    aux2 = alpha_ * vec_x(i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    vec_y(i) = vec_y(i) + alpha_ * aux
                end do
                
            else if( storage == sparse_full .and. op_==sparse_op_hermitian) then
                do i = 1, nrows
                    aux = alpha_ * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(col(j)) = vec_y(col(j)) + conjg(data(j)) * aux
                    end do
                end do

            else if( storage == sparse_lower .and. op_==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_csp
                    aux2 = alpha_ * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + conjg(data(j)) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + conjg(data(j)) * aux2
                    end do
                    aux = alpha_ * aux + conjg(data(j)) * aux2
                    vec_y(i) = vec_y(i) + aux
                end do

            else if( storage == sparse_upper .and. op_==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(i) * conjg(data(rowptr(i)))
                    aux2 = alpha_ * vec_x(i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + conjg(data(j)) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + conjg(data(j)) * aux2
                    end do
                    vec_y(i) = vec_y(i) + alpha_ * aux
                end do
            end if
        end associate
    end subroutine
    
    module subroutine spmv_csr_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_csp_type), intent(in) :: matrix
        complex(sp), intent(in)    :: vec_x(:,:)
        complex(sp), intent(inout) :: vec_y(:,:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(sp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        complex(sp) :: aux(size(vec_x,dim=1)), aux2(size(vec_x,dim=1))
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_csp
        endif

        associate( data => matrix%data, col => matrix%col, rowptr => matrix%rowptr, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
    
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_sp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha_ * aux
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha_ * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_csp
                    aux2 = alpha_ * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    aux = alpha_ * aux + data(j) * aux2
                    vec_y(:,i) = vec_y(:,i) + aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(:,i) * data(rowptr(i))
                    aux2 = alpha_ * vec_x(:,i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha_ * aux
                end do
                
            else if( storage == sparse_full .and. op_==sparse_op_hermitian) then
                do i = 1, nrows
                    aux = alpha_ * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(:,col(j)) = vec_y(:,col(j)) + conjg(data(j)) * aux
                    end do
                end do

            else if( storage == sparse_lower .and. op_==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_csp
                    aux2 = alpha_ * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + conjg(data(j)) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + conjg(data(j)) * aux2
                    end do
                    aux = alpha_ * aux + conjg(data(j)) * aux2
                    vec_y(:,i) = vec_y(:,i) + aux
                end do

            else if( storage == sparse_upper .and. op_==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(:,i) * conjg(data(rowptr(i)))
                    aux2 = alpha_ * vec_x(:,i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + conjg(data(j)) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + conjg(data(j)) * aux2
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha_ * aux
                end do
            end if
        end associate
    end subroutine
    
    module subroutine spmv_csr_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_cdp_type), intent(in) :: matrix
        complex(dp), intent(in)    :: vec_x(:)
        complex(dp), intent(inout) :: vec_y(:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        complex(dp) :: aux, aux2
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_cdp
        endif

        associate( data => matrix%data, col => matrix%col, rowptr => matrix%rowptr, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
    
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_dp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                    end do
                    vec_y(i) = vec_y(i) + alpha_ * aux
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha_ * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_cdp
                    aux2 = alpha_ * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    aux = alpha_ * aux + data(j) * aux2
                    vec_y(i) = vec_y(i) + aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(i) * data(rowptr(i))
                    aux2 = alpha_ * vec_x(i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    vec_y(i) = vec_y(i) + alpha_ * aux
                end do
                
            else if( storage == sparse_full .and. op_==sparse_op_hermitian) then
                do i = 1, nrows
                    aux = alpha_ * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(col(j)) = vec_y(col(j)) + conjg(data(j)) * aux
                    end do
                end do

            else if( storage == sparse_lower .and. op_==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_cdp
                    aux2 = alpha_ * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + conjg(data(j)) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + conjg(data(j)) * aux2
                    end do
                    aux = alpha_ * aux + conjg(data(j)) * aux2
                    vec_y(i) = vec_y(i) + aux
                end do

            else if( storage == sparse_upper .and. op_==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(i) * conjg(data(rowptr(i)))
                    aux2 = alpha_ * vec_x(i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + conjg(data(j)) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + conjg(data(j)) * aux2
                    end do
                    vec_y(i) = vec_y(i) + alpha_ * aux
                end do
            end if
        end associate
    end subroutine
    
    module subroutine spmv_csr_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_cdp_type), intent(in) :: matrix
        complex(dp), intent(in)    :: vec_x(:,:)
        complex(dp), intent(inout) :: vec_y(:,:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(dp) :: alpha_
        character(1) :: op_
        integer(ilp) :: i, j
        complex(dp) :: aux(size(vec_x,dim=1)), aux2(size(vec_x,dim=1))
        
        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_dp
        if(present(alpha)) alpha_ = alpha
        if(present(beta)) then
            vec_y = beta * vec_y
        else 
            vec_y = zero_cdp
        endif

        associate( data => matrix%data, col => matrix%col, rowptr => matrix%rowptr, &
            & nnz => matrix%nnz, nrows => matrix%nrows, ncols => matrix%ncols, storage => matrix%storage )
    
            if( storage == sparse_full .and. op_==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_dp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha_ * aux
                end do

            else if( storage == sparse_full .and. op_==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha_ * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_cdp
                    aux2 = alpha_ * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    aux = alpha_ * aux + data(j) * aux2
                    vec_y(:,i) = vec_y(:,i) + aux
                end do

            else if( storage == sparse_upper .and. op_/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(:,i) * data(rowptr(i))
                    aux2 = alpha_ * vec_x(:,i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha_ * aux
                end do
                
            else if( storage == sparse_full .and. op_==sparse_op_hermitian) then
                do i = 1, nrows
                    aux = alpha_ * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(:,col(j)) = vec_y(:,col(j)) + conjg(data(j)) * aux
                    end do
                end do

            else if( storage == sparse_lower .and. op_==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_cdp
                    aux2 = alpha_ * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + conjg(data(j)) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + conjg(data(j)) * aux2
                    end do
                    aux = alpha_ * aux + conjg(data(j)) * aux2
                    vec_y(:,i) = vec_y(:,i) + aux
                end do

            else if( storage == sparse_upper .and. op_==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(:,i) * conjg(data(rowptr(i)))
                    aux2 = alpha_ * vec_x(:,i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + conjg(data(j)) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + conjg(data(j)) * aux2
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha_ * aux
                end do
            end if
        end associate
    end subroutine
    

end submodule stdlib_sparse_spmv_csr