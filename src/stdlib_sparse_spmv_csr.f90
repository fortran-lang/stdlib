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
        real(sp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_sp
        if(present(alpha)) alpha_ = alpha

        beta_ = zero_sp
        if(present(beta)) beta_ = beta

        call spmv_kernel_csr_1d_sp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

    end subroutine

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
        integer(ilp) :: i, j
        integer(ilp) :: nrows
        real(sp) :: aux, aux2
        
        nrows = size(rowptr)-1

        vec_y = beta * vec_y
   
            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_sp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                    end do
                    vec_y(i) = vec_y(i) + alpha * aux
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_sp
                    aux2 = alpha * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    aux = alpha * aux + data(j) * aux2
                    vec_y(i) = vec_y(i) + aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(i) * data(rowptr(i))
                    aux2 = alpha * vec_x(i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    vec_y(i) = vec_y(i) + alpha * aux
                end do
                
            end if
    end subroutine

    module subroutine spmv_csr_2d_sp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_sp_type), intent(in) :: matrix
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

        call spmv_kernel_csr_2d_sp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: nrows
        real(sp) :: aux(size(vec_x,dim=1)), aux2(size(vec_x,dim=1))
        
        nrows = size(rowptr)-1

        vec_y = beta * vec_y
   
            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_sp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha * aux
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_sp
                    aux2 = alpha * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    aux = alpha * aux + data(j) * aux2
                    vec_y(:,i) = vec_y(:,i) + aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(:,i) * data(rowptr(i))
                    aux2 = alpha * vec_x(:,i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha * aux
                end do
                
            end if
    end subroutine

    module subroutine spmv_csr_1d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_dp_type), intent(in) :: matrix
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

        call spmv_kernel_csr_1d_dp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: nrows
        real(dp) :: aux, aux2
        
        nrows = size(rowptr)-1

        vec_y = beta * vec_y
   
            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_dp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                    end do
                    vec_y(i) = vec_y(i) + alpha * aux
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_dp
                    aux2 = alpha * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    aux = alpha * aux + data(j) * aux2
                    vec_y(i) = vec_y(i) + aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(i) * data(rowptr(i))
                    aux2 = alpha * vec_x(i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    vec_y(i) = vec_y(i) + alpha * aux
                end do
                
            end if
    end subroutine

    module subroutine spmv_csr_2d_dp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_dp_type), intent(in) :: matrix
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

        call spmv_kernel_csr_2d_dp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: nrows
        real(dp) :: aux(size(vec_x,dim=1)), aux2(size(vec_x,dim=1))
        
        nrows = size(rowptr)-1

        vec_y = beta * vec_y
   
            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_dp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha * aux
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_dp
                    aux2 = alpha * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    aux = alpha * aux + data(j) * aux2
                    vec_y(:,i) = vec_y(:,i) + aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(:,i) * data(rowptr(i))
                    aux2 = alpha * vec_x(:,i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha * aux
                end do
                
            end if
    end subroutine

    module subroutine spmv_csr_1d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_csp_type), intent(in) :: matrix
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

        call spmv_kernel_csr_1d_csp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: nrows
        complex(sp) :: aux, aux2
        
        nrows = size(rowptr)-1

        vec_y = beta * vec_y
   
            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_sp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                    end do
                    vec_y(i) = vec_y(i) + alpha * aux
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_csp
                    aux2 = alpha * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    aux = alpha * aux + data(j) * aux2
                    vec_y(i) = vec_y(i) + aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(i) * data(rowptr(i))
                    aux2 = alpha * vec_x(i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    vec_y(i) = vec_y(i) + alpha * aux
                end do
                
            else if( storage == sparse_full .and. op==sparse_op_hermitian) then
                do i = 1, nrows
                    aux = alpha * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(col(j)) = vec_y(col(j)) + conjg(data(j)) * aux
                    end do
                end do

            else if( storage == sparse_lower .and. op==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_csp
                    aux2 = alpha * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + conjg(data(j)) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + conjg(data(j)) * aux2
                    end do
                    aux = alpha * aux + conjg(data(j)) * aux2
                    vec_y(i) = vec_y(i) + aux
                end do

            else if( storage == sparse_upper .and. op==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(i) * conjg(data(rowptr(i)))
                    aux2 = alpha * vec_x(i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + conjg(data(j)) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + conjg(data(j)) * aux2
                    end do
                    vec_y(i) = vec_y(i) + alpha * aux
                end do
            end if
    end subroutine

    module subroutine spmv_csr_2d_csp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_csp_type), intent(in) :: matrix
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

        call spmv_kernel_csr_2d_csp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: nrows
        complex(sp) :: aux(size(vec_x,dim=1)), aux2(size(vec_x,dim=1))
        
        nrows = size(rowptr)-1

        vec_y = beta * vec_y
   
            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_sp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha * aux
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_csp
                    aux2 = alpha * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    aux = alpha * aux + data(j) * aux2
                    vec_y(:,i) = vec_y(:,i) + aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(:,i) * data(rowptr(i))
                    aux2 = alpha * vec_x(:,i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha * aux
                end do
                
            else if( storage == sparse_full .and. op==sparse_op_hermitian) then
                do i = 1, nrows
                    aux = alpha * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(:,col(j)) = vec_y(:,col(j)) + conjg(data(j)) * aux
                    end do
                end do

            else if( storage == sparse_lower .and. op==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_csp
                    aux2 = alpha * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + conjg(data(j)) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + conjg(data(j)) * aux2
                    end do
                    aux = alpha * aux + conjg(data(j)) * aux2
                    vec_y(:,i) = vec_y(:,i) + aux
                end do

            else if( storage == sparse_upper .and. op==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(:,i) * conjg(data(rowptr(i)))
                    aux2 = alpha * vec_x(:,i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + conjg(data(j)) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + conjg(data(j)) * aux2
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha * aux
                end do
            end if
    end subroutine

    module subroutine spmv_csr_1d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_cdp_type), intent(in) :: matrix
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

        call spmv_kernel_csr_1d_cdp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: nrows
        complex(dp) :: aux, aux2
        
        nrows = size(rowptr)-1

        vec_y = beta * vec_y
   
            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_dp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                    end do
                    vec_y(i) = vec_y(i) + alpha * aux
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_cdp
                    aux2 = alpha * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    aux = alpha * aux + data(j) * aux2
                    vec_y(i) = vec_y(i) + aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(i) * data(rowptr(i))
                    aux2 = alpha * vec_x(i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + data(j) * aux2
                    end do
                    vec_y(i) = vec_y(i) + alpha * aux
                end do
                
            else if( storage == sparse_full .and. op==sparse_op_hermitian) then
                do i = 1, nrows
                    aux = alpha * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(col(j)) = vec_y(col(j)) + conjg(data(j)) * aux
                    end do
                end do

            else if( storage == sparse_lower .and. op==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_cdp
                    aux2 = alpha * vec_x(i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + conjg(data(j)) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + conjg(data(j)) * aux2
                    end do
                    aux = alpha * aux + conjg(data(j)) * aux2
                    vec_y(i) = vec_y(i) + aux
                end do

            else if( storage == sparse_upper .and. op==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(i) * conjg(data(rowptr(i)))
                    aux2 = alpha * vec_x(i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + conjg(data(j)) * vec_x(col(j))
                        vec_y(col(j)) = vec_y(col(j)) + conjg(data(j)) * aux2
                    end do
                    vec_y(i) = vec_y(i) + alpha * aux
                end do
            end if
    end subroutine

    module subroutine spmv_csr_2d_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        type(CSR_cdp_type), intent(in) :: matrix
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

        call spmv_kernel_csr_2d_cdp(op_, alpha_, &
            matrix%data, matrix%col, matrix%rowptr, matrix%storage, &
            vec_x, beta_, vec_y)

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
        integer(ilp) :: i, j
        integer(ilp) :: nrows
        complex(dp) :: aux(size(vec_x,dim=1)), aux2(size(vec_x,dim=1))
        
        nrows = size(rowptr)-1

        vec_y = beta * vec_y
   
            if( storage == sparse_full .and. op==sparse_op_none ) then
                do i = 1, nrows
                    aux = zero_dp
                    do j = rowptr(i), rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha * aux
                end do

            else if( storage == sparse_full .and. op==sparse_op_transpose ) then
                do i = 1, nrows
                    aux = alpha * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux
                    end do
                end do
                
            else if( storage == sparse_lower .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_cdp
                    aux2 = alpha * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    aux = alpha * aux + data(j) * aux2
                    vec_y(:,i) = vec_y(:,i) + aux
                end do

            else if( storage == sparse_upper .and. op/=sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(:,i) * data(rowptr(i))
                    aux2 = alpha * vec_x(:,i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + data(j) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + data(j) * aux2
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha * aux
                end do
                
            else if( storage == sparse_full .and. op==sparse_op_hermitian) then
                do i = 1, nrows
                    aux = alpha * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-1
                        vec_y(:,col(j)) = vec_y(:,col(j)) + conjg(data(j)) * aux
                    end do
                end do

            else if( storage == sparse_lower .and. op==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = zero_cdp
                    aux2 = alpha * vec_x(:,i)
                    do j = rowptr(i), rowptr(i+1)-2
                        aux = aux + conjg(data(j)) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + conjg(data(j)) * aux2
                    end do
                    aux = alpha * aux + conjg(data(j)) * aux2
                    vec_y(:,i) = vec_y(:,i) + aux
                end do

            else if( storage == sparse_upper .and. op==sparse_op_hermitian )then
                do i = 1 , nrows
                    aux  = vec_x(:,i) * conjg(data(rowptr(i)))
                    aux2 = alpha * vec_x(:,i)
                    do j = rowptr(i)+1, rowptr(i+1)-1
                        aux = aux + conjg(data(j)) * vec_x(:,col(j))
                        vec_y(:,col(j)) = vec_y(:,col(j)) + conjg(data(j)) * aux2
                    end do
                    vec_y(:,i) = vec_y(:,i) + alpha * aux
                end do
            end if
    end subroutine


end submodule stdlib_sparse_spmv_csr
