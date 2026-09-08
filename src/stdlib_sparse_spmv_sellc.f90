submodule (stdlib_sparse_spmv) stdlib_sparse_spmv_sellc
contains

    !! spmv_sellc
    module subroutine spmv_sellc_sp(matrix,vec_x,vec_y,alpha,beta,op)
        !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
        type(SELLC_sp_type), intent(in) :: matrix
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

        call spmv_kernel_sellc_sp(op_,alpha_, &
            matrix%data, matrix%rowptr, matrix%col, &
            matrix%storage, &
            vec_x,beta_,vec_y)

    end subroutine

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
        integer(ilp) :: i, nz, rowidx, num_chunks, rm
        integer(ilp) :: nrows
        integer :: chunk_size

        chunk_size = size(data, 1)

        if( .not.any( [4, 8, 16] == chunk_size ) ) then
            print *, "error: sellc chunk size not supported."
            return
        end if

        vec_y = beta * vec_y

        nrows = merge(size(vec_y), size(vec_x), op==sparse_op_none)

        num_chunks = nrows / chunk_size
        rm = nrows - num_chunks * chunk_size

        if( storage == sparse_full .and. op==sparse_op_none ) then

            select case(chunk_size)
            case(4)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*4 + 1 
                    call chunk_kernel_4(nz,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
                end do
            case(8)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*8 + 1 
                    call chunk_kernel_8(nz,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
                end do
            case(16)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*16 + 1 
                    call chunk_kernel_16(nz,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
                end do
            end select
            
            ! remainder
            if(rm>0)then 
                i = num_chunks + 1 
                nz = ia(i+1) - ia(i)
                rowidx = (i - 1)*chunk_size + 1
                call chunk_kernel_rm(nz,chunk_size,rm,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
            end if
            
        else if( storage == sparse_full .and. op==sparse_op_transpose ) then

            select case(chunk_size)
            case(4)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*4 + 1 
                    call chunk_kernel_trans_4(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            case(8)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*8 + 1 
                    call chunk_kernel_trans_8(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            case(16)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*16 + 1 
                    call chunk_kernel_trans_16(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            end select
            
            ! remainder
            if(rm>0)then 
                i = num_chunks + 1 
                nz = ia(i+1) - ia(i)
                rowidx = (i - 1)*chunk_size + 1
                call chunk_kernel_rm_trans(nz,chunk_size,rm,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
            end if
        
        else
            print *, "error: sellc format for spmv operation not yet supported."
            return
        end if

    contains
        pure subroutine chunk_kernel_4(n,a,col,x,y)
            integer, value      :: n
            real(sp), intent(in)  :: a(4,n), x(:)
            integer(ilp), intent(in) :: col(4,n)
            real(sp), intent(inout) :: y(4)
            integer :: j
            do j = 1, n
                y(:) = y(:) + alpha * a(:,j) * x(col(:,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_trans_4(n,a,col,x,y)
            integer, value      :: n
            real(sp), intent(in)  :: a(4,n), x(4)
            integer(ilp), intent(in) :: col(4,n)
            real(sp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 4
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_8(n,a,col,x,y)
            integer, value      :: n
            real(sp), intent(in)  :: a(8,n), x(:)
            integer(ilp), intent(in) :: col(8,n)
            real(sp), intent(inout) :: y(8)
            integer :: j
            do j = 1, n
                y(:) = y(:) + alpha * a(:,j) * x(col(:,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_trans_8(n,a,col,x,y)
            integer, value      :: n
            real(sp), intent(in)  :: a(8,n), x(8)
            integer(ilp), intent(in) :: col(8,n)
            real(sp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 8
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_16(n,a,col,x,y)
            integer, value      :: n
            real(sp), intent(in)  :: a(16,n), x(:)
            integer(ilp), intent(in) :: col(16,n)
            real(sp), intent(inout) :: y(16)
            integer :: j
            do j = 1, n
                y(:) = y(:) + alpha * a(:,j) * x(col(:,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_trans_16(n,a,col,x,y)
            integer, value      :: n
            real(sp), intent(in)  :: a(16,n), x(16)
            integer(ilp), intent(in) :: col(16,n)
            real(sp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 16
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
    
        pure subroutine chunk_kernel_rm(n,cs,r,a,col,x,y)
            integer, value      :: n, cs, r
            real(sp), intent(in)  :: a(cs,n), x(:)
            integer(ilp), intent(in) :: col(cs,n)
            real(sp), intent(inout) :: y(r)
            integer :: j
            do j = 1, n
                y(1:r) = y(1:r) + alpha * a(1:r,j) * x(col(1:r,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_rm_trans(n,cs,r,a,col,x,y)
            integer, value      :: n, cs, r
            real(sp), intent(in)  :: a(cs,n), x(r)
            integer(ilp), intent(in) :: col(cs,n)
            real(sp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, r
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine

    end subroutine
    module subroutine spmv_sellc_dp(matrix,vec_x,vec_y,alpha,beta,op)
        !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
        type(SELLC_dp_type), intent(in) :: matrix
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

        call spmv_kernel_sellc_dp(op_,alpha_, &
            matrix%data, matrix%rowptr, matrix%col, &
            matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, nz, rowidx, num_chunks, rm
        integer(ilp) :: nrows
        integer :: chunk_size

        chunk_size = size(data, 1)

        if( .not.any( [4, 8, 16] == chunk_size ) ) then
            print *, "error: sellc chunk size not supported."
            return
        end if

        vec_y = beta * vec_y

        nrows = merge(size(vec_y), size(vec_x), op==sparse_op_none)

        num_chunks = nrows / chunk_size
        rm = nrows - num_chunks * chunk_size

        if( storage == sparse_full .and. op==sparse_op_none ) then

            select case(chunk_size)
            case(4)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*4 + 1 
                    call chunk_kernel_4(nz,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
                end do
            case(8)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*8 + 1 
                    call chunk_kernel_8(nz,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
                end do
            case(16)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*16 + 1 
                    call chunk_kernel_16(nz,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
                end do
            end select
            
            ! remainder
            if(rm>0)then 
                i = num_chunks + 1 
                nz = ia(i+1) - ia(i)
                rowidx = (i - 1)*chunk_size + 1
                call chunk_kernel_rm(nz,chunk_size,rm,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
            end if
            
        else if( storage == sparse_full .and. op==sparse_op_transpose ) then

            select case(chunk_size)
            case(4)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*4 + 1 
                    call chunk_kernel_trans_4(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            case(8)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*8 + 1 
                    call chunk_kernel_trans_8(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            case(16)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*16 + 1 
                    call chunk_kernel_trans_16(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            end select
            
            ! remainder
            if(rm>0)then 
                i = num_chunks + 1 
                nz = ia(i+1) - ia(i)
                rowidx = (i - 1)*chunk_size + 1
                call chunk_kernel_rm_trans(nz,chunk_size,rm,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
            end if
        
        else
            print *, "error: sellc format for spmv operation not yet supported."
            return
        end if

    contains
        pure subroutine chunk_kernel_4(n,a,col,x,y)
            integer, value      :: n
            real(dp), intent(in)  :: a(4,n), x(:)
            integer(ilp), intent(in) :: col(4,n)
            real(dp), intent(inout) :: y(4)
            integer :: j
            do j = 1, n
                y(:) = y(:) + alpha * a(:,j) * x(col(:,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_trans_4(n,a,col,x,y)
            integer, value      :: n
            real(dp), intent(in)  :: a(4,n), x(4)
            integer(ilp), intent(in) :: col(4,n)
            real(dp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 4
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_8(n,a,col,x,y)
            integer, value      :: n
            real(dp), intent(in)  :: a(8,n), x(:)
            integer(ilp), intent(in) :: col(8,n)
            real(dp), intent(inout) :: y(8)
            integer :: j
            do j = 1, n
                y(:) = y(:) + alpha * a(:,j) * x(col(:,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_trans_8(n,a,col,x,y)
            integer, value      :: n
            real(dp), intent(in)  :: a(8,n), x(8)
            integer(ilp), intent(in) :: col(8,n)
            real(dp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 8
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_16(n,a,col,x,y)
            integer, value      :: n
            real(dp), intent(in)  :: a(16,n), x(:)
            integer(ilp), intent(in) :: col(16,n)
            real(dp), intent(inout) :: y(16)
            integer :: j
            do j = 1, n
                y(:) = y(:) + alpha * a(:,j) * x(col(:,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_trans_16(n,a,col,x,y)
            integer, value      :: n
            real(dp), intent(in)  :: a(16,n), x(16)
            integer(ilp), intent(in) :: col(16,n)
            real(dp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 16
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
    
        pure subroutine chunk_kernel_rm(n,cs,r,a,col,x,y)
            integer, value      :: n, cs, r
            real(dp), intent(in)  :: a(cs,n), x(:)
            integer(ilp), intent(in) :: col(cs,n)
            real(dp), intent(inout) :: y(r)
            integer :: j
            do j = 1, n
                y(1:r) = y(1:r) + alpha * a(1:r,j) * x(col(1:r,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_rm_trans(n,cs,r,a,col,x,y)
            integer, value      :: n, cs, r
            real(dp), intent(in)  :: a(cs,n), x(r)
            integer(ilp), intent(in) :: col(cs,n)
            real(dp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, r
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine

    end subroutine
    module subroutine spmv_sellc_csp(matrix,vec_x,vec_y,alpha,beta,op)
        !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
        type(SELLC_csp_type), intent(in) :: matrix
        complex(sp), intent(in)    :: vec_x(:)
        complex(sp), intent(inout) :: vec_y(:)
        complex(sp), intent(in), optional :: alpha
        complex(sp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(sp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_csp
        if(present(alpha)) alpha_ = alpha

        beta_ = zero_csp
        if(present(beta)) beta_ = beta

        call spmv_kernel_sellc_csp(op_,alpha_, &
            matrix%data, matrix%rowptr, matrix%col, &
            matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, nz, rowidx, num_chunks, rm
        integer(ilp) :: nrows
        integer :: chunk_size

        chunk_size = size(data, 1)

        if( .not.any( [4, 8, 16] == chunk_size ) ) then
            print *, "error: sellc chunk size not supported."
            return
        end if

        vec_y = beta * vec_y

        nrows = merge(size(vec_y), size(vec_x), op==sparse_op_none)

        num_chunks = nrows / chunk_size
        rm = nrows - num_chunks * chunk_size

        if( storage == sparse_full .and. op==sparse_op_none ) then

            select case(chunk_size)
            case(4)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*4 + 1 
                    call chunk_kernel_4(nz,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
                end do
            case(8)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*8 + 1 
                    call chunk_kernel_8(nz,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
                end do
            case(16)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*16 + 1 
                    call chunk_kernel_16(nz,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
                end do
            end select
            
            ! remainder
            if(rm>0)then 
                i = num_chunks + 1 
                nz = ia(i+1) - ia(i)
                rowidx = (i - 1)*chunk_size + 1
                call chunk_kernel_rm(nz,chunk_size,rm,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
            end if
            
        else if( storage == sparse_full .and. op==sparse_op_transpose ) then

            select case(chunk_size)
            case(4)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*4 + 1 
                    call chunk_kernel_trans_4(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            case(8)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*8 + 1 
                    call chunk_kernel_trans_8(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            case(16)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*16 + 1 
                    call chunk_kernel_trans_16(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            end select
            
            ! remainder
            if(rm>0)then 
                i = num_chunks + 1 
                nz = ia(i+1) - ia(i)
                rowidx = (i - 1)*chunk_size + 1
                call chunk_kernel_rm_trans(nz,chunk_size,rm,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
            end if
        
        else if( storage == sparse_full .and. op==sparse_op_hermitian ) then

            select case(chunk_size)
            case(4)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*4 + 1 
                    call chunk_kernel_herm_4(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            case(8)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*8 + 1 
                    call chunk_kernel_herm_8(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            case(16)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*16 + 1 
                    call chunk_kernel_herm_16(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            end select
            
            ! remainder
            if(rm>0)then 
                i = num_chunks + 1 
                nz = ia(i+1) - ia(i)
                rowidx = (i - 1)*chunk_size + 1
                call chunk_kernel_rm_herm(nz,chunk_size,rm,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
            end if
        else
            print *, "error: sellc format for spmv operation not yet supported."
            return
        end if

    contains
        pure subroutine chunk_kernel_4(n,a,col,x,y)
            integer, value      :: n
            complex(sp), intent(in)  :: a(4,n), x(:)
            integer(ilp), intent(in) :: col(4,n)
            complex(sp), intent(inout) :: y(4)
            integer :: j
            do j = 1, n
                y(:) = y(:) + alpha * a(:,j) * x(col(:,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_trans_4(n,a,col,x,y)
            integer, value      :: n
            complex(sp), intent(in)  :: a(4,n), x(4)
            integer(ilp), intent(in) :: col(4,n)
            complex(sp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 4
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_herm_4(n,a,col,x,y)
            integer, value      :: n
            complex(sp), intent(in)  :: a(4,n), x(4)
            integer(ilp), intent(in) :: col(4,n)
            complex(sp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 4
                    y(col(k,j)) = y(col(k,j)) + alpha * conjg(a(k,j)) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_8(n,a,col,x,y)
            integer, value      :: n
            complex(sp), intent(in)  :: a(8,n), x(:)
            integer(ilp), intent(in) :: col(8,n)
            complex(sp), intent(inout) :: y(8)
            integer :: j
            do j = 1, n
                y(:) = y(:) + alpha * a(:,j) * x(col(:,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_trans_8(n,a,col,x,y)
            integer, value      :: n
            complex(sp), intent(in)  :: a(8,n), x(8)
            integer(ilp), intent(in) :: col(8,n)
            complex(sp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 8
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_herm_8(n,a,col,x,y)
            integer, value      :: n
            complex(sp), intent(in)  :: a(8,n), x(8)
            integer(ilp), intent(in) :: col(8,n)
            complex(sp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 8
                    y(col(k,j)) = y(col(k,j)) + alpha * conjg(a(k,j)) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_16(n,a,col,x,y)
            integer, value      :: n
            complex(sp), intent(in)  :: a(16,n), x(:)
            integer(ilp), intent(in) :: col(16,n)
            complex(sp), intent(inout) :: y(16)
            integer :: j
            do j = 1, n
                y(:) = y(:) + alpha * a(:,j) * x(col(:,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_trans_16(n,a,col,x,y)
            integer, value      :: n
            complex(sp), intent(in)  :: a(16,n), x(16)
            integer(ilp), intent(in) :: col(16,n)
            complex(sp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 16
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_herm_16(n,a,col,x,y)
            integer, value      :: n
            complex(sp), intent(in)  :: a(16,n), x(16)
            integer(ilp), intent(in) :: col(16,n)
            complex(sp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 16
                    y(col(k,j)) = y(col(k,j)) + alpha * conjg(a(k,j)) * x(k)
                end do
            end do
        end subroutine
    
        pure subroutine chunk_kernel_rm(n,cs,r,a,col,x,y)
            integer, value      :: n, cs, r
            complex(sp), intent(in)  :: a(cs,n), x(:)
            integer(ilp), intent(in) :: col(cs,n)
            complex(sp), intent(inout) :: y(r)
            integer :: j
            do j = 1, n
                y(1:r) = y(1:r) + alpha * a(1:r,j) * x(col(1:r,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_rm_trans(n,cs,r,a,col,x,y)
            integer, value      :: n, cs, r
            complex(sp), intent(in)  :: a(cs,n), x(r)
            integer(ilp), intent(in) :: col(cs,n)
            complex(sp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, r
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_rm_herm(n,cs,r,a,col,x,y)
            integer, value      :: n, cs, r
            complex(sp), intent(in)  :: a(cs,n), x(r)
            integer(ilp), intent(in) :: col(cs,n)
            complex(sp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, r
                    y(col(k,j)) = y(col(k,j)) + alpha * conjg(a(k,j)) * x(k)
                end do
            end do
        end subroutine

    end subroutine
    module subroutine spmv_sellc_cdp(matrix,vec_x,vec_y,alpha,beta,op)
        !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
        type(SELLC_cdp_type), intent(in) :: matrix
        complex(dp), intent(in)    :: vec_x(:)
        complex(dp), intent(inout) :: vec_y(:)
        complex(dp), intent(in), optional :: alpha
        complex(dp), intent(in), optional :: beta
        character(1), intent(in), optional :: op
        complex(dp) :: alpha_, beta_
        character(1) :: op_

        op_ = sparse_op_none; if(present(op)) op_ = op
        alpha_ = one_cdp
        if(present(alpha)) alpha_ = alpha

        beta_ = zero_cdp
        if(present(beta)) beta_ = beta

        call spmv_kernel_sellc_cdp(op_,alpha_, &
            matrix%data, matrix%rowptr, matrix%col, &
            matrix%storage, &
            vec_x,beta_,vec_y)

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
        integer(ilp) :: i, nz, rowidx, num_chunks, rm
        integer(ilp) :: nrows
        integer :: chunk_size

        chunk_size = size(data, 1)

        if( .not.any( [4, 8, 16] == chunk_size ) ) then
            print *, "error: sellc chunk size not supported."
            return
        end if

        vec_y = beta * vec_y

        nrows = merge(size(vec_y), size(vec_x), op==sparse_op_none)

        num_chunks = nrows / chunk_size
        rm = nrows - num_chunks * chunk_size

        if( storage == sparse_full .and. op==sparse_op_none ) then

            select case(chunk_size)
            case(4)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*4 + 1 
                    call chunk_kernel_4(nz,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
                end do
            case(8)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*8 + 1 
                    call chunk_kernel_8(nz,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
                end do
            case(16)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*16 + 1 
                    call chunk_kernel_16(nz,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
                end do
            end select
            
            ! remainder
            if(rm>0)then 
                i = num_chunks + 1 
                nz = ia(i+1) - ia(i)
                rowidx = (i - 1)*chunk_size + 1
                call chunk_kernel_rm(nz,chunk_size,rm,data(:,ia(i)),ja(:,ia(i)),vec_x,vec_y(rowidx:))
            end if
            
        else if( storage == sparse_full .and. op==sparse_op_transpose ) then

            select case(chunk_size)
            case(4)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*4 + 1 
                    call chunk_kernel_trans_4(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            case(8)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*8 + 1 
                    call chunk_kernel_trans_8(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            case(16)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*16 + 1 
                    call chunk_kernel_trans_16(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            end select
            
            ! remainder
            if(rm>0)then 
                i = num_chunks + 1 
                nz = ia(i+1) - ia(i)
                rowidx = (i - 1)*chunk_size + 1
                call chunk_kernel_rm_trans(nz,chunk_size,rm,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
            end if
        
        else if( storage == sparse_full .and. op==sparse_op_hermitian ) then

            select case(chunk_size)
            case(4)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*4 + 1 
                    call chunk_kernel_herm_4(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            case(8)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*8 + 1 
                    call chunk_kernel_herm_8(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            case(16)
                do i = 1, num_chunks
                    nz = ia(i+1) - ia(i)
                    rowidx = (i - 1)*16 + 1 
                    call chunk_kernel_herm_16(nz,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
                end do
            end select
            
            ! remainder
            if(rm>0)then 
                i = num_chunks + 1 
                nz = ia(i+1) - ia(i)
                rowidx = (i - 1)*chunk_size + 1
                call chunk_kernel_rm_herm(nz,chunk_size,rm,data(:,ia(i)),ja(:,ia(i)),vec_x(rowidx:),vec_y)
            end if
        else
            print *, "error: sellc format for spmv operation not yet supported."
            return
        end if

    contains
        pure subroutine chunk_kernel_4(n,a,col,x,y)
            integer, value      :: n
            complex(dp), intent(in)  :: a(4,n), x(:)
            integer(ilp), intent(in) :: col(4,n)
            complex(dp), intent(inout) :: y(4)
            integer :: j
            do j = 1, n
                y(:) = y(:) + alpha * a(:,j) * x(col(:,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_trans_4(n,a,col,x,y)
            integer, value      :: n
            complex(dp), intent(in)  :: a(4,n), x(4)
            integer(ilp), intent(in) :: col(4,n)
            complex(dp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 4
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_herm_4(n,a,col,x,y)
            integer, value      :: n
            complex(dp), intent(in)  :: a(4,n), x(4)
            integer(ilp), intent(in) :: col(4,n)
            complex(dp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 4
                    y(col(k,j)) = y(col(k,j)) + alpha * conjg(a(k,j)) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_8(n,a,col,x,y)
            integer, value      :: n
            complex(dp), intent(in)  :: a(8,n), x(:)
            integer(ilp), intent(in) :: col(8,n)
            complex(dp), intent(inout) :: y(8)
            integer :: j
            do j = 1, n
                y(:) = y(:) + alpha * a(:,j) * x(col(:,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_trans_8(n,a,col,x,y)
            integer, value      :: n
            complex(dp), intent(in)  :: a(8,n), x(8)
            integer(ilp), intent(in) :: col(8,n)
            complex(dp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 8
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_herm_8(n,a,col,x,y)
            integer, value      :: n
            complex(dp), intent(in)  :: a(8,n), x(8)
            integer(ilp), intent(in) :: col(8,n)
            complex(dp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 8
                    y(col(k,j)) = y(col(k,j)) + alpha * conjg(a(k,j)) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_16(n,a,col,x,y)
            integer, value      :: n
            complex(dp), intent(in)  :: a(16,n), x(:)
            integer(ilp), intent(in) :: col(16,n)
            complex(dp), intent(inout) :: y(16)
            integer :: j
            do j = 1, n
                y(:) = y(:) + alpha * a(:,j) * x(col(:,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_trans_16(n,a,col,x,y)
            integer, value      :: n
            complex(dp), intent(in)  :: a(16,n), x(16)
            integer(ilp), intent(in) :: col(16,n)
            complex(dp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 16
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_herm_16(n,a,col,x,y)
            integer, value      :: n
            complex(dp), intent(in)  :: a(16,n), x(16)
            integer(ilp), intent(in) :: col(16,n)
            complex(dp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, 16
                    y(col(k,j)) = y(col(k,j)) + alpha * conjg(a(k,j)) * x(k)
                end do
            end do
        end subroutine
    
        pure subroutine chunk_kernel_rm(n,cs,r,a,col,x,y)
            integer, value      :: n, cs, r
            complex(dp), intent(in)  :: a(cs,n), x(:)
            integer(ilp), intent(in) :: col(cs,n)
            complex(dp), intent(inout) :: y(r)
            integer :: j
            do j = 1, n
                y(1:r) = y(1:r) + alpha * a(1:r,j) * x(col(1:r,j))
            end do
        end subroutine
        pure subroutine chunk_kernel_rm_trans(n,cs,r,a,col,x,y)
            integer, value      :: n, cs, r
            complex(dp), intent(in)  :: a(cs,n), x(r)
            integer(ilp), intent(in) :: col(cs,n)
            complex(dp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, r
                    y(col(k,j)) = y(col(k,j)) + alpha * a(k,j) * x(k)
                end do
            end do
        end subroutine
        pure subroutine chunk_kernel_rm_herm(n,cs,r,a,col,x,y)
            integer, value      :: n, cs, r
            complex(dp), intent(in)  :: a(cs,n), x(r)
            integer(ilp), intent(in) :: col(cs,n)
            complex(dp), intent(inout) :: y(:)
            integer :: j, k
            do j = 1, n
                do k = 1, r
                    y(col(k,j)) = y(col(k,j)) + alpha * conjg(a(k,j)) * x(k)
                end do
            end do
        end subroutine

    end subroutine

end submodule stdlib_sparse_spmv_sellc
