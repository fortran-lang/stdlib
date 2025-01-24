
! Vector norms
submodule(stdlib_linalg) stdlib_linalg_norms
     use stdlib_linalg_constants
     use stdlib_linalg_blas
     use stdlib_linalg_lapack
     use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
         LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR     
     use iso_c_binding, only: c_intptr_t,c_char,c_loc
     implicit none
     
     character(*), parameter :: this = 'norm'
     
     !> List of internal norm flags
     integer(ilp), parameter :: NORM_ONE       = 1_ilp 
     integer(ilp), parameter :: NORM_TWO       = 2_ilp
     integer(ilp), parameter :: NORM_POW_FIRST = 3_ilp       
     integer(ilp), parameter :: NORM_INF       = +huge(0_ilp) ! infinity norm 
     integer(ilp), parameter :: NORM_POW_LAST  = NORM_INF - 1_ilp
     integer(ilp), parameter :: NORM_MINUSINF  = -huge(0_ilp)
          
     !> Wrappers to LAPACK *LANGE matrix norm flags
     character, parameter :: MAT_NORM_MAT = 'M' ! maxval(sum(abs(a)))   ! over whole matrix: unused
     character, parameter :: MAT_NORM_ONE = '1' ! maxval(sum(abs(a),1)) ! over columns
     character, parameter :: MAT_NORM_INF = 'I' ! maxval(sum(abs(a),2)) ! over rows
     character, parameter :: MAT_NORM_FRO = 'E' ! sqrt(sum(a**2))       ! "Euclidean" or "Frobenius"     
     !> Other wrappers to matrix norms
     character, parameter :: MAT_NORM_SVD = '2' ! maxval(svdvals(a))    ! Maximum singular value
     
     interface parse_norm_type
        module procedure parse_norm_type_integer
        module procedure parse_norm_type_character
     end interface parse_norm_type
     
     interface mat_task_request
        module procedure mat_task_request_integer
        module procedure mat_task_request_character
     end interface mat_task_request
     
     
     interface stride_1d
        module procedure stride_1d_s
        module procedure stride_1d_d
        module procedure stride_1d_c
        module procedure stride_1d_z
     end interface stride_1d
     
     contains
     
     !> Parse norm type from an integer user input
     pure subroutine parse_norm_type_integer(order,norm_type,err)
        !> User input value
        integer(ilp), intent(in) :: order
        !> Return value: norm type
        integer(ilp), intent(out) :: norm_type
        !> State return flag
        type(linalg_state_type), intent(out) :: err
        
        select case (order)
           case (1_ilp)
               norm_type = NORM_ONE
           case (2_ilp)
               norm_type = NORM_TWO
           case (3_ilp:NORM_POW_LAST)
               norm_type = order
           case (NORM_INF:)
               norm_type = NORM_INF
           case (:NORM_MINUSINF)
               norm_type = NORM_MINUSINF
           
           case default
               norm_type = NORM_ONE
               err = linalg_state_type(this,LINALG_ERROR,'Input norm type ',order,' is not recognized.')
        end select    
        
     end subroutine parse_norm_type_integer

     pure subroutine parse_norm_type_character(order,norm_type,err)
        !> User input value
        character(len=*), intent(in) :: order
        !> Return value: norm type
        integer(ilp), intent(out) :: norm_type
        !> State return flag
        type(linalg_state_type), intent(out) :: err
        
        integer(ilp) :: int_order,read_err
        
        select case (order)
           case ('inf','Inf','INF')
              norm_type = NORM_INF
           case ('-inf','-Inf','-INF')
              norm_type = NORM_MINUSINF
           case ('Euclidean','euclidean','EUCLIDEAN')
              norm_type = NORM_TWO
           case default
            
              ! Check if this input can be read as an integer
              read(order,*,iostat=read_err) int_order
              if (read_err/=0) then 
                 ! Cannot read as an integer
                 norm_type = NORM_ONE
                 err = linalg_state_type(this,LINALG_ERROR,'Input norm type ',order,' is not recognized.')                 
              else
                 call parse_norm_type_integer(int_order,norm_type,err)
              endif  

        end select    
        
     end subroutine parse_norm_type_character

     !> From a user norm request, generate a *LANGE task command
     pure subroutine mat_task_request_integer(order,mat_task,err)
        !> Parsed matrix norm type
        integer(ilp), optional, intent(in) :: order
        !> LANGE task
        character, intent(out) :: mat_task
        !> Error flag
        type(linalg_state_type), intent(inout) :: err
        
        if (present(order)) then 
        
            select case (order)
               case (NORM_INF)
                  mat_task = MAT_NORM_INF 
               case (NORM_TWO)
                  mat_task = MAT_NORM_SVD 
               case (NORM_ONE)
                  mat_task = MAT_NORM_ONE
               case default 
                  err = linalg_state_type(this,LINALG_VALUE_ERROR,'Integer order ',order,' is not a valid matrix norm input.')
            end select
            
        else
            
            ! No user input: Frobenius norm
            mat_task = MAT_NORM_FRO
            
        endif
     end subroutine mat_task_request_integer

     pure subroutine mat_task_request_character(order,mat_task,err)
        !> User input value
        character(len=*), intent(in) :: order
        !> Return value: norm type
        character, intent(out) :: mat_task
        !> State return flag
        type(linalg_state_type), intent(out) :: err
        
        integer(ilp) :: int_order,read_err
        
        select case (order)
           case ('inf','Inf','INF')
              mat_task = MAT_NORM_INF
           case ('Euclidean','euclidean','EUCLIDEAN','Frobenius','frobenius','FROBENIUS','Fro','fro','frob')
              mat_task = MAT_NORM_FRO
           case default
            
              ! Check if this input can be read as an integer
              read(order,*,iostat=read_err) int_order
              if (read_err/=0 .or. all(int_order/=[1,2])) then 
                 ! Cannot read as an integer                 
                 err = linalg_state_type(this,LINALG_ERROR,'Matrix norm input',order,' is not recognized.')                 
              endif
              
              select case (int_order)
                 case (1);     mat_task = MAT_NORM_ONE
                 case (2);     mat_task = MAT_NORM_SVD
                 case default; mat_task = MAT_NORM_ONE   
              end select
              
        end select    
        
     end subroutine mat_task_request_character

    
    ! Compute stride of a 1d array
    pure integer(ilp) function stride_1d_s(a) result(stride)
        !> Input 1-d array 
        real(sp), intent(in), target :: a(:)
        
        integer(c_intptr_t) :: a1,a2
        
        if (size(a,kind=ilp)<=1_ilp) then 
           stride = 1_ilp
        else
           a1 = transfer(c_loc(a(1)),a1)
           a2 = transfer(c_loc(a(2)),a2)
           stride = bit_size(0_c_char)*int(a2-a1, ilp)/storage_size(a, kind=ilp)
        endif
        
    end function stride_1d_s
    
    ! Private internal 1D implementation. This has to be used only internally, 
    ! when all inputs are already checked for correctness.
    pure subroutine internal_norm_1D_s(sze, a, nrm, norm_request)
        !> Input matrix length
        integer(ilp), intent(in) :: sze
        !> Input contiguous 1-d matrix a(*)
        real(sp), intent(in) :: a(sze)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Internal matrix request 
        integer(ilp), intent(in) :: norm_request
        
        integer(ilp) :: i
        real(sp) :: rorder
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        select case(norm_request)
            case(NORM_ONE)
                nrm = asum(sze,a,incx=1_ilp)
            case(NORM_TWO)            
                nrm = nrm2(sze,a,incx=1_ilp)
            case(NORM_INF)
                i = stdlib_isamax(sze,a,incx=1_ilp)
                nrm = abs(a(i))
            case(NORM_MINUSINF)
                nrm = minval( abs(a) )
            case (NORM_POW_FIRST:NORM_POW_LAST)
                rorder = 1.0_sp / norm_request
                nrm = sum( abs(a) ** norm_request ) ** rorder
        end select
        
    end subroutine internal_norm_1D_s     
    

    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_char_s(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        real(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_1D_char_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_char_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_char_s(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        real(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_1D_char_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_char_s

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_char_s(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        real(sp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_char_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_char_s(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_2D_char_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_char_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_char_s(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_2D_char_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_char_s

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_char_s(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_char_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_char_s(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_3D_char_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_char_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_char_s(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_3D_char_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_char_s

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_char_s(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_char_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_char_s(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_4D_char_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_char_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_char_s(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_4D_char_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_char_s

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_char_s(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_char_s


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and char input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_char_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_char_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_char_s
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_char_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        real(sp), allocatable :: apack(:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_char_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_char_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_char_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_char_s
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_char_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        real(sp), allocatable :: apack(:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_char_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_char_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_char_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_char_s
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_char_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        real(sp), allocatable :: apack(:,:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_char_s

    
    !====================================================================
    ! Matrix norms
    !====================================================================    
    
    ! Internal implementation 
    module function matrix_norm_char_s(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        real(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(sp) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n
        character :: mat_task
        real(sp), target :: work1(1)
        real(sp), pointer :: work(:)        
        
        m = size(a,dim=1,kind=ilp)
        n = size(a,dim=2,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif       
        
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        else
            work => work1
        end if
                
        if (mat_task==MAT_NORM_SVD) then 
           nrm = maxval(svdvals(a,err_),1) 
           call linalg_error_handling(err_,err)
        else
           ! LAPACK interface 
           nrm = lange(mat_task,m,n,a,m,work)
        end if
        
        if (mat_task==MAT_NORM_INF) deallocate(work)
        
    end function matrix_norm_char_s
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_3D_to_1D_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        real(sp), intent(in), contiguous, target :: a(:,:,:)
        !> Norm of the matrix.        
        real(sp), allocatable :: nrm(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(3) :: s,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(m,m=1_ilp,3_ilp)]
        integer(ilp) :: j3
        logical :: contiguous_data
        character :: mat_task
        real(sp), target :: work1(1)
        real(sp), pointer :: work(:)        
        real(sp), pointer :: apack(:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=3)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',3,' matrix norm has invalid dim=',dims)
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3),err_)
            nrm(j3) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3) = &
            lange(mat_task,m,n,apack(:,:,j3),lda,work)            
        end if
        enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_3D_to_1D_char_s    
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_4D_to_2D_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        real(sp), intent(in), contiguous, target :: a(:,:,:,:)
        !> Norm of the matrix.        
        real(sp), allocatable :: nrm(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(4) :: s,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(m,m=1_ilp,4_ilp)]
        integer(ilp) :: j3, j4
        logical :: contiguous_data
        character :: mat_task
        real(sp), target :: work1(1)
        real(sp), pointer :: work(:)        
        real(sp), pointer :: apack(:,:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=4)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',4,' matrix norm has invalid dim=',dims)
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3), size(apack,4)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j4 = lbound(apack, 4), ubound(apack, 4)
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3, j4),err_)
            nrm(j3, j4) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3, j4) = &
            lange(mat_task,m,n,apack(:,:,j3, j4),lda,work)            
        end if
        enddo; enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_4D_to_2D_char_s    
    
    

    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_int_s(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        real(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_1D_int_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_int_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_int_s(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        real(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_1D_int_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_int_s

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_int_s(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        real(sp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_int_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_int_s(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_2D_int_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_int_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_int_s(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_2D_int_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_int_s

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_int_s(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_int_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_int_s(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_3D_int_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_int_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_int_s(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_3D_int_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_int_s

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_int_s(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_int_s


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_int_s(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_4D_int_s(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_int_s
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_int_s(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_4D_int_s(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_int_s

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_int_s(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_s(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_int_s


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and int input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_int_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_int_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_int_s
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_int_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        real(sp), allocatable :: apack(:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_int_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_int_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_int_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_int_s
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_int_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        real(sp), allocatable :: apack(:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_int_s

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_int_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_s(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_int_s

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_s(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_int_s
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_int_s(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(sp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        real(sp), allocatable :: apack(:,:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_s(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_s(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_int_s

    
    !====================================================================
    ! Matrix norms
    !====================================================================    
    
    ! Internal implementation 
    module function matrix_norm_int_s(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        real(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(sp) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n
        character :: mat_task
        real(sp), target :: work1(1)
        real(sp), pointer :: work(:)        
        
        m = size(a,dim=1,kind=ilp)
        n = size(a,dim=2,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif       
        
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        else
            work => work1
        end if
                
        if (mat_task==MAT_NORM_SVD) then 
           nrm = maxval(svdvals(a,err_),1) 
           call linalg_error_handling(err_,err)
        else
           ! LAPACK interface 
           nrm = lange(mat_task,m,n,a,m,work)
        end if
        
        if (mat_task==MAT_NORM_INF) deallocate(work)
        
    end function matrix_norm_int_s
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_3D_to_1D_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        real(sp), intent(in), contiguous, target :: a(:,:,:)
        !> Norm of the matrix.        
        real(sp), allocatable :: nrm(:)
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(3) :: s,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(m,m=1_ilp,3_ilp)]
        integer(ilp) :: j3
        logical :: contiguous_data
        character :: mat_task
        real(sp), target :: work1(1)
        real(sp), pointer :: work(:)        
        real(sp), pointer :: apack(:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=3)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',3,' matrix norm has invalid dim=',dims)
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3),err_)
            nrm(j3) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3) = &
            lange(mat_task,m,n,apack(:,:,j3),lda,work)            
        end if
        enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_3D_to_1D_int_s    
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_4D_to_2D_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        real(sp), intent(in), contiguous, target :: a(:,:,:,:)
        !> Norm of the matrix.        
        real(sp), allocatable :: nrm(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(4) :: s,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(m,m=1_ilp,4_ilp)]
        integer(ilp) :: j3, j4
        logical :: contiguous_data
        character :: mat_task
        real(sp), target :: work1(1)
        real(sp), pointer :: work(:)        
        real(sp), pointer :: apack(:,:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=4)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',4,' matrix norm has invalid dim=',dims)
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3), size(apack,4)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j4 = lbound(apack, 4), ubound(apack, 4)
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3, j4),err_)
            nrm(j3, j4) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3, j4) = &
            lange(mat_task,m,n,apack(:,:,j3, j4),lda,work)            
        end if
        enddo; enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_4D_to_2D_int_s    
    
    
    
    ! Compute stride of a 1d array
    pure integer(ilp) function stride_1d_d(a) result(stride)
        !> Input 1-d array 
        real(dp), intent(in), target :: a(:)
        
        integer(c_intptr_t) :: a1,a2
        
        if (size(a,kind=ilp)<=1_ilp) then 
           stride = 1_ilp
        else
           a1 = transfer(c_loc(a(1)),a1)
           a2 = transfer(c_loc(a(2)),a2)
           stride = bit_size(0_c_char)*int(a2-a1, ilp)/storage_size(a, kind=ilp)
        endif
        
    end function stride_1d_d
    
    ! Private internal 1D implementation. This has to be used only internally, 
    ! when all inputs are already checked for correctness.
    pure subroutine internal_norm_1D_d(sze, a, nrm, norm_request)
        !> Input matrix length
        integer(ilp), intent(in) :: sze
        !> Input contiguous 1-d matrix a(*)
        real(dp), intent(in) :: a(sze)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Internal matrix request 
        integer(ilp), intent(in) :: norm_request
        
        integer(ilp) :: i
        real(dp) :: rorder
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        select case(norm_request)
            case(NORM_ONE)
                nrm = asum(sze,a,incx=1_ilp)
            case(NORM_TWO)            
                nrm = nrm2(sze,a,incx=1_ilp)
            case(NORM_INF)
                i = stdlib_idamax(sze,a,incx=1_ilp)
                nrm = abs(a(i))
            case(NORM_MINUSINF)
                nrm = minval( abs(a) )
            case (NORM_POW_FIRST:NORM_POW_LAST)
                rorder = 1.0_dp / norm_request
                nrm = sum( abs(a) ** norm_request ) ** rorder
        end select
        
    end subroutine internal_norm_1D_d     
    

    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_char_d(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        real(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_1D_char_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_char_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_char_d(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        real(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_1D_char_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_char_d

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_char_d(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        real(dp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_char_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_char_d(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_2D_char_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_char_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_char_d(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_2D_char_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_char_d

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_char_d(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_char_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_char_d(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_3D_char_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_char_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_char_d(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_3D_char_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_char_d

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_char_d(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_char_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_char_d(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_4D_char_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_char_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_char_d(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_4D_char_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_char_d

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_char_d(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_char_d


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and char input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_char_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_char_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_char_d
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_char_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        real(dp), allocatable :: apack(:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_char_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_char_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_char_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_char_d
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_char_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        real(dp), allocatable :: apack(:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_char_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_char_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_char_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_char_d
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_char_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        real(dp), allocatable :: apack(:,:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_char_d

    
    !====================================================================
    ! Matrix norms
    !====================================================================    
    
    ! Internal implementation 
    module function matrix_norm_char_d(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        real(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(dp) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n
        character :: mat_task
        real(dp), target :: work1(1)
        real(dp), pointer :: work(:)        
        
        m = size(a,dim=1,kind=ilp)
        n = size(a,dim=2,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif       
        
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        else
            work => work1
        end if
                
        if (mat_task==MAT_NORM_SVD) then 
           nrm = maxval(svdvals(a,err_),1) 
           call linalg_error_handling(err_,err)
        else
           ! LAPACK interface 
           nrm = lange(mat_task,m,n,a,m,work)
        end if
        
        if (mat_task==MAT_NORM_INF) deallocate(work)
        
    end function matrix_norm_char_d
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_3D_to_1D_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        real(dp), intent(in), contiguous, target :: a(:,:,:)
        !> Norm of the matrix.        
        real(dp), allocatable :: nrm(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(3) :: s,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(m,m=1_ilp,3_ilp)]
        integer(ilp) :: j3
        logical :: contiguous_data
        character :: mat_task
        real(dp), target :: work1(1)
        real(dp), pointer :: work(:)        
        real(dp), pointer :: apack(:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=3)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',3,' matrix norm has invalid dim=',dims)
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3),err_)
            nrm(j3) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3) = &
            lange(mat_task,m,n,apack(:,:,j3),lda,work)            
        end if
        enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_3D_to_1D_char_d    
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_4D_to_2D_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        real(dp), intent(in), contiguous, target :: a(:,:,:,:)
        !> Norm of the matrix.        
        real(dp), allocatable :: nrm(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(4) :: s,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(m,m=1_ilp,4_ilp)]
        integer(ilp) :: j3, j4
        logical :: contiguous_data
        character :: mat_task
        real(dp), target :: work1(1)
        real(dp), pointer :: work(:)        
        real(dp), pointer :: apack(:,:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=4)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',4,' matrix norm has invalid dim=',dims)
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3), size(apack,4)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j4 = lbound(apack, 4), ubound(apack, 4)
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3, j4),err_)
            nrm(j3, j4) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3, j4) = &
            lange(mat_task,m,n,apack(:,:,j3, j4),lda,work)            
        end if
        enddo; enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_4D_to_2D_char_d    
    
    

    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_int_d(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        real(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_1D_int_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_int_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_int_d(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        real(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_1D_int_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_int_d

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_int_d(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        real(dp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_int_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_int_d(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_2D_int_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_int_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_int_d(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_2D_int_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_int_d

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_int_d(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_int_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_int_d(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_3D_int_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_int_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_int_d(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_3D_int_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_int_d

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_int_d(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_int_d


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_int_d(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_4D_int_d(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_int_d
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_int_d(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_4D_int_d(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_int_d

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_int_d(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_d(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_int_d


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and int input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_int_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_int_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_int_d
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_int_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        real(dp), allocatable :: apack(:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_int_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_int_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_int_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_int_d
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_int_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        real(dp), allocatable :: apack(:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_int_d

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_int_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_d(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_int_d

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_d(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_int_d
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_int_d(a, nrm, order, dim, err)
        !> Input matrix a[..]
        real(dp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        real(dp), allocatable :: apack(:,:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_d(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_d(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_int_d

    
    !====================================================================
    ! Matrix norms
    !====================================================================    
    
    ! Internal implementation 
    module function matrix_norm_int_d(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        real(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(dp) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n
        character :: mat_task
        real(dp), target :: work1(1)
        real(dp), pointer :: work(:)        
        
        m = size(a,dim=1,kind=ilp)
        n = size(a,dim=2,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif       
        
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        else
            work => work1
        end if
                
        if (mat_task==MAT_NORM_SVD) then 
           nrm = maxval(svdvals(a,err_),1) 
           call linalg_error_handling(err_,err)
        else
           ! LAPACK interface 
           nrm = lange(mat_task,m,n,a,m,work)
        end if
        
        if (mat_task==MAT_NORM_INF) deallocate(work)
        
    end function matrix_norm_int_d
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_3D_to_1D_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        real(dp), intent(in), contiguous, target :: a(:,:,:)
        !> Norm of the matrix.        
        real(dp), allocatable :: nrm(:)
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(3) :: s,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(m,m=1_ilp,3_ilp)]
        integer(ilp) :: j3
        logical :: contiguous_data
        character :: mat_task
        real(dp), target :: work1(1)
        real(dp), pointer :: work(:)        
        real(dp), pointer :: apack(:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=3)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',3,' matrix norm has invalid dim=',dims)
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3),err_)
            nrm(j3) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3) = &
            lange(mat_task,m,n,apack(:,:,j3),lda,work)            
        end if
        enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_3D_to_1D_int_d    
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_4D_to_2D_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        real(dp), intent(in), contiguous, target :: a(:,:,:,:)
        !> Norm of the matrix.        
        real(dp), allocatable :: nrm(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(4) :: s,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(m,m=1_ilp,4_ilp)]
        integer(ilp) :: j3, j4
        logical :: contiguous_data
        character :: mat_task
        real(dp), target :: work1(1)
        real(dp), pointer :: work(:)        
        real(dp), pointer :: apack(:,:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=4)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',4,' matrix norm has invalid dim=',dims)
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3), size(apack,4)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j4 = lbound(apack, 4), ubound(apack, 4)
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3, j4),err_)
            nrm(j3, j4) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3, j4) = &
            lange(mat_task,m,n,apack(:,:,j3, j4),lda,work)            
        end if
        enddo; enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_4D_to_2D_int_d    
    
    
    
    ! Compute stride of a 1d array
    pure integer(ilp) function stride_1d_c(a) result(stride)
        !> Input 1-d array 
        complex(sp), intent(in), target :: a(:)
        
        integer(c_intptr_t) :: a1,a2
        
        if (size(a,kind=ilp)<=1_ilp) then 
           stride = 1_ilp
        else
           a1 = transfer(c_loc(a(1)),a1)
           a2 = transfer(c_loc(a(2)),a2)
           stride = bit_size(0_c_char)*int(a2-a1, ilp)/storage_size(a, kind=ilp)
        endif
        
    end function stride_1d_c
    
    ! Private internal 1D implementation. This has to be used only internally, 
    ! when all inputs are already checked for correctness.
    pure subroutine internal_norm_1D_c(sze, a, nrm, norm_request)
        !> Input matrix length
        integer(ilp), intent(in) :: sze
        !> Input contiguous 1-d matrix a(*)
        complex(sp), intent(in) :: a(sze)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Internal matrix request 
        integer(ilp), intent(in) :: norm_request
        
        integer(ilp) :: i
        real(sp) :: rorder
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        select case(norm_request)
            case(NORM_ONE)
                nrm = asum(sze,a,incx=1_ilp)
            case(NORM_TWO)            
                nrm = nrm2(sze,a,incx=1_ilp)
            case(NORM_INF)
                ! The default BLAS stdlib_icamax uses |Re(.)|+|Im(.)|. Do not use it.
                i = stdlib_icmax1(sze,a,incx=1_ilp)
                nrm = abs(a(i))
            case(NORM_MINUSINF)
                nrm = minval( abs(a) )
            case (NORM_POW_FIRST:NORM_POW_LAST)
                rorder = 1.0_sp / norm_request
                nrm = sum( abs(a) ** norm_request ) ** rorder
        end select
        
    end subroutine internal_norm_1D_c     
    

    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_char_c(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_1D_char_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_char_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_char_c(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_1D_char_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_char_c

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_char_c(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_char_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_char_c(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_2D_char_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_char_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_char_c(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_2D_char_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_char_c

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_char_c(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_char_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_char_c(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_3D_char_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_char_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_char_c(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_3D_char_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_char_c

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_char_c(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_char_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_char_c(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_4D_char_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_char_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_char_c(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_4D_char_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_char_c

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_char_c(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_char_c


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and char input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_char_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_char_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_char_c
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_char_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        complex(sp), allocatable :: apack(:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_char_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_char_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_char_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_char_c
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_char_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        complex(sp), allocatable :: apack(:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_char_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_char_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_char_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_char_c
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_char_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        complex(sp), allocatable :: apack(:,:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_char_c

    
    !====================================================================
    ! Matrix norms
    !====================================================================    
    
    ! Internal implementation 
    module function matrix_norm_char_c(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        complex(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(sp) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n
        character :: mat_task
        real(sp), target :: work1(1)
        real(sp), pointer :: work(:)        
        
        m = size(a,dim=1,kind=ilp)
        n = size(a,dim=2,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif       
        
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        else
            work => work1
        end if
                
        if (mat_task==MAT_NORM_SVD) then 
           nrm = maxval(svdvals(a,err_),1) 
           call linalg_error_handling(err_,err)
        else
           ! LAPACK interface 
           nrm = lange(mat_task,m,n,a,m,work)
        end if
        
        if (mat_task==MAT_NORM_INF) deallocate(work)
        
    end function matrix_norm_char_c
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_3D_to_1D_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        complex(sp), intent(in), contiguous, target :: a(:,:,:)
        !> Norm of the matrix.        
        real(sp), allocatable :: nrm(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(3) :: s,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(m,m=1_ilp,3_ilp)]
        integer(ilp) :: j3
        logical :: contiguous_data
        character :: mat_task
        real(sp), target :: work1(1)
        real(sp), pointer :: work(:)        
        complex(sp), pointer :: apack(:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=3)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',3,' matrix norm has invalid dim=',dims)
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3),err_)
            nrm(j3) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3) = &
            lange(mat_task,m,n,apack(:,:,j3),lda,work)            
        end if
        enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_3D_to_1D_char_c    
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_4D_to_2D_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        complex(sp), intent(in), contiguous, target :: a(:,:,:,:)
        !> Norm of the matrix.        
        real(sp), allocatable :: nrm(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(4) :: s,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(m,m=1_ilp,4_ilp)]
        integer(ilp) :: j3, j4
        logical :: contiguous_data
        character :: mat_task
        real(sp), target :: work1(1)
        real(sp), pointer :: work(:)        
        complex(sp), pointer :: apack(:,:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=4)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',4,' matrix norm has invalid dim=',dims)
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3), size(apack,4)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j4 = lbound(apack, 4), ubound(apack, 4)
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3, j4),err_)
            nrm(j3, j4) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3, j4) = &
            lange(mat_task,m,n,apack(:,:,j3, j4),lda,work)            
        end if
        enddo; enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_4D_to_2D_char_c    
    
    

    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_int_c(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_1D_int_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_int_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_int_c(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_1D_int_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_int_c

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_int_c(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_int_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_int_c(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_2D_int_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_int_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_int_c(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_2D_int_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_int_c

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_int_c(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_int_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_int_c(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_3D_int_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_int_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_int_c(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_3D_int_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_int_c

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_int_c(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_int_c


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_int_c(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm
                                    
        call norm_4D_int_c(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_int_c
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_int_c(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm                 
                
        call norm_4D_int_c(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_int_c

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_int_c(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(sp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_c(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_int_c


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and int input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_int_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_int_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_int_c
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_int_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        complex(sp), allocatable :: apack(:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_int_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_int_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_int_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_int_c
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_int_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        complex(sp), allocatable :: apack(:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_int_c

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_int_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_c(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_int_c

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_c(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_int_c
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_int_c(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        complex(sp), allocatable :: apack(:,:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_c(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_c(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_int_c

    
    !====================================================================
    ! Matrix norms
    !====================================================================    
    
    ! Internal implementation 
    module function matrix_norm_int_c(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        complex(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(sp) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n
        character :: mat_task
        real(sp), target :: work1(1)
        real(sp), pointer :: work(:)        
        
        m = size(a,dim=1,kind=ilp)
        n = size(a,dim=2,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_sp
        
        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif       
        
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        else
            work => work1
        end if
                
        if (mat_task==MAT_NORM_SVD) then 
           nrm = maxval(svdvals(a,err_),1) 
           call linalg_error_handling(err_,err)
        else
           ! LAPACK interface 
           nrm = lange(mat_task,m,n,a,m,work)
        end if
        
        if (mat_task==MAT_NORM_INF) deallocate(work)
        
    end function matrix_norm_int_c
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_3D_to_1D_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        complex(sp), intent(in), contiguous, target :: a(:,:,:)
        !> Norm of the matrix.        
        real(sp), allocatable :: nrm(:)
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(3) :: s,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(m,m=1_ilp,3_ilp)]
        integer(ilp) :: j3
        logical :: contiguous_data
        character :: mat_task
        real(sp), target :: work1(1)
        real(sp), pointer :: work(:)        
        complex(sp), pointer :: apack(:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=3)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',3,' matrix norm has invalid dim=',dims)
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3),err_)
            nrm(j3) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3) = &
            lange(mat_task,m,n,apack(:,:,j3),lda,work)            
        end if
        enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_3D_to_1D_int_c    
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_4D_to_2D_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        complex(sp), intent(in), contiguous, target :: a(:,:,:,:)
        !> Norm of the matrix.        
        real(sp), allocatable :: nrm(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(4) :: s,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(m,m=1_ilp,4_ilp)]
        integer(ilp) :: j3, j4
        logical :: contiguous_data
        character :: mat_task
        real(sp), target :: work1(1)
        real(sp), pointer :: work(:)        
        complex(sp), pointer :: apack(:,:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=4)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',4,' matrix norm has invalid dim=',dims)
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3), size(apack,4)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j4 = lbound(apack, 4), ubound(apack, 4)
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3, j4),err_)
            nrm(j3, j4) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3, j4) = &
            lange(mat_task,m,n,apack(:,:,j3, j4),lda,work)            
        end if
        enddo; enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_4D_to_2D_int_c    
    
    
    
    ! Compute stride of a 1d array
    pure integer(ilp) function stride_1d_z(a) result(stride)
        !> Input 1-d array 
        complex(dp), intent(in), target :: a(:)
        
        integer(c_intptr_t) :: a1,a2
        
        if (size(a,kind=ilp)<=1_ilp) then 
           stride = 1_ilp
        else
           a1 = transfer(c_loc(a(1)),a1)
           a2 = transfer(c_loc(a(2)),a2)
           stride = bit_size(0_c_char)*int(a2-a1, ilp)/storage_size(a, kind=ilp)
        endif
        
    end function stride_1d_z
    
    ! Private internal 1D implementation. This has to be used only internally, 
    ! when all inputs are already checked for correctness.
    pure subroutine internal_norm_1D_z(sze, a, nrm, norm_request)
        !> Input matrix length
        integer(ilp), intent(in) :: sze
        !> Input contiguous 1-d matrix a(*)
        complex(dp), intent(in) :: a(sze)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Internal matrix request 
        integer(ilp), intent(in) :: norm_request
        
        integer(ilp) :: i
        real(dp) :: rorder
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        select case(norm_request)
            case(NORM_ONE)
                nrm = asum(sze,a,incx=1_ilp)
            case(NORM_TWO)            
                nrm = nrm2(sze,a,incx=1_ilp)
            case(NORM_INF)
                ! The default BLAS stdlib_izamax uses |Re(.)|+|Im(.)|. Do not use it.
                i = stdlib_izmax1(sze,a,incx=1_ilp)
                nrm = abs(a(i))
            case(NORM_MINUSINF)
                nrm = minval( abs(a) )
            case (NORM_POW_FIRST:NORM_POW_LAST)
                rorder = 1.0_dp / norm_request
                nrm = sum( abs(a) ** norm_request ) ** rorder
        end select
        
    end subroutine internal_norm_1D_z     
    

    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_char_z(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_1D_char_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_char_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_char_z(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_1D_char_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_char_z

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_char_z(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_char_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_char_z(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_2D_char_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_char_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_char_z(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_2D_char_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_char_z

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_char_z(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_char_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_char_z(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_3D_char_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_char_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_char_z(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_3D_char_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_char_z

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_char_z(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_char_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_char_z(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_4D_char_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_char_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_char_z(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_4D_char_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_char_z

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_char_z(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_char_z


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and char input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_char_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_char_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_char_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_char_z
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_char_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        complex(dp), allocatable :: apack(:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_char_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_char_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_char_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_char_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_char_z
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_char_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        complex(dp), allocatable :: apack(:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_char_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_char_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_char_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_char_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_char_z
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_char_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        complex(dp), allocatable :: apack(:,:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_char_z

    
    !====================================================================
    ! Matrix norms
    !====================================================================    
    
    ! Internal implementation 
    module function matrix_norm_char_z(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        complex(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(dp) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n
        character :: mat_task
        real(dp), target :: work1(1)
        real(dp), pointer :: work(:)        
        
        m = size(a,dim=1,kind=ilp)
        n = size(a,dim=2,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif       
        
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        else
            work => work1
        end if
                
        if (mat_task==MAT_NORM_SVD) then 
           nrm = maxval(svdvals(a,err_),1) 
           call linalg_error_handling(err_,err)
        else
           ! LAPACK interface 
           nrm = lange(mat_task,m,n,a,m,work)
        end if
        
        if (mat_task==MAT_NORM_INF) deallocate(work)
        
    end function matrix_norm_char_z
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_3D_to_1D_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        complex(dp), intent(in), contiguous, target :: a(:,:,:)
        !> Norm of the matrix.        
        real(dp), allocatable :: nrm(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(3) :: s,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(m,m=1_ilp,3_ilp)]
        integer(ilp) :: j3
        logical :: contiguous_data
        character :: mat_task
        real(dp), target :: work1(1)
        real(dp), pointer :: work(:)        
        complex(dp), pointer :: apack(:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=3)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',3,' matrix norm has invalid dim=',dims)
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3),err_)
            nrm(j3) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3) = &
            lange(mat_task,m,n,apack(:,:,j3),lda,work)            
        end if
        enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_3D_to_1D_char_z    
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_4D_to_2D_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        complex(dp), intent(in), contiguous, target :: a(:,:,:,:)
        !> Norm of the matrix.        
        real(dp), allocatable :: nrm(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(4) :: s,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(m,m=1_ilp,4_ilp)]
        integer(ilp) :: j3, j4
        logical :: contiguous_data
        character :: mat_task
        real(dp), target :: work1(1)
        real(dp), pointer :: work(:)        
        complex(dp), pointer :: apack(:,:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=4)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',4,' matrix norm has invalid dim=',dims)
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3), size(apack,4)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j4 = lbound(apack, 4), ubound(apack, 4)
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3, j4),err_)
            nrm(j3, j4) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3, j4) = &
            lange(mat_task,m,n,apack(:,:,j3, j4),lda,work)            
        end if
        enddo; enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_4D_to_2D_char_z    
    
    

    !==============================================
    ! Norms : any rank to scalar
    !==============================================


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_1D_order_int_z(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_1D_int_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_1D_order_int_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_1D_order_err_int_z(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_1D_int_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_1D_order_err_int_z

    ! Internal implementation: 1-d    
    pure module subroutine norm_1D_int_z(a, nrm, order, err)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in), target :: a(:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_1D_int_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_order_int_z(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_2D_int_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_2D_order_int_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_2D_order_err_int_z(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_2D_int_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_2D_order_err_int_z

    ! Internal implementation: 2-d    
    pure module subroutine norm_2D_int_z(a, nrm, order, err)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_2D_int_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_order_int_z(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_3D_int_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_3D_order_int_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_3D_order_err_int_z(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_3D_int_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_3D_order_err_int_z

    ! Internal implementation: 3-d    
    pure module subroutine norm_3D_int_z(a, nrm, order, err)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in), target :: a(:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_3D_int_z


    ! Pure function interface, with order specification. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_order_int_z(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm
                                    
        call norm_4D_int_z(a, nrm=nrm, order=order)
        
    end function stdlib_linalg_norm_4D_order_int_z
    
    ! Function interface with output error
    module function stdlib_linalg_norm_4D_order_err_int_z(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm                 
                
        call norm_4D_int_z(a, nrm=nrm, order=order, err=err)
        
    end function stdlib_linalg_norm_4D_order_err_int_z

    ! Internal implementation: 4-d    
    pure module subroutine norm_4D_int_z(a, nrm, order, err)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Norm of the matrix.
        real(dp), intent(out) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err         
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,norm_request
        
        sze = size(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        ! Check matrix size
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif         
        
        ! Get norm
        call internal_norm_1D_z(sze, a, nrm, norm_request)
        call linalg_error_handling(err_,err)
        
    end subroutine norm_4D_int_z


    !====================================================================
    ! Norms : any rank to rank-1, with DIM specifier and int input
    !====================================================================

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_2D_to_1D_int_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_2D_to_1D_int_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_2D_to_1D_err_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        
        call norm_2D_to_1D_int_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_2D_to_1D_err_int_z
    
    ! Internal implementation 
    pure module subroutine norm_2D_to_1D_int_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2
        logical :: contiguous_data
        integer(ilp), dimension(2) :: spe,spack,perm,iperm
        integer(ilp), dimension(2), parameter :: dim_range = [(lda,lda=1_ilp,2_ilp)]
        complex(dp), allocatable :: apack(:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>2) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2), &
                    nrm(j2), norm_request)
            enddo            
            
        else
            
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2), &
                    nrm(j2), norm_request)
            enddo               
            
        endif        
        
    end subroutine norm_2D_to_1D_int_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_3D_to_2D_int_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_3D_to_2D_int_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_3D_to_2D_err_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        
        call norm_3D_to_2D_int_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_3D_to_2D_err_int_z
    
    ! Internal implementation 
    pure module subroutine norm_3D_to_2D_int_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3
        logical :: contiguous_data
        integer(ilp), dimension(3) :: spe,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(lda,lda=1_ilp,3_ilp)]
        complex(dp), allocatable :: apack(:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>3) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo            
            
        else
            
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3), &
                    nrm(j2, j3), norm_request)
            enddo; enddo               
            
        endif        
        
    end subroutine norm_3D_to_2D_int_z

    
    ! Pure function interface with DIM specifier. On error, the code will stop
    pure module function stdlib_linalg_norm_4D_to_3D_int_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_z(a, nrm, order, dim)
            
    end function stdlib_linalg_norm_4D_to_3D_int_z

    ! Function interface with DIM specifier and output error state.
    module function stdlib_linalg_norm_4D_to_3D_err_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix.
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
        
        call norm_4D_to_3D_int_z(a, nrm, order, dim, err)
            
    end function stdlib_linalg_norm_4D_to_3D_err_int_z
    
    ! Internal implementation 
    pure module subroutine norm_4D_to_3D_int_z(a, nrm, order, dim, err)
        !> Input matrix a[..]
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Dimension to collapse by computing the norm w.r.t other dimensions
        !  (dim must be defined before it is used for `nrm`)
        integer(ilp), intent(in) :: dim        
        !> Norm of the matrix.        
        real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
            & merge(size(a, 3), size(a, 4), mask=3<dim))     
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: sze,lda,norm_request,j2, j3, j4
        logical :: contiguous_data
        integer(ilp), dimension(4) :: spe,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(lda,lda=1_ilp,4_ilp)]
        complex(dp), allocatable :: apack(:,:,:,:)
        
        ! Input matrix properties
        sze = size (a,kind=ilp)
        spe = shape(a,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (sze<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check dimension choice
        if (dim<1 .or. dim>4) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'dimension ',dim, &
                                'is out of rank for shape(a)=',shape(a,kind=ilp))
            call linalg_error_handling(err_,err)
            return
        end if
        
        ! Check norm request
        call parse_norm_type(order,norm_request,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif     
        
        ! The norm's leading dimension
        lda = spe(dim)    
        
        ! Check if input column data is contiguous
        contiguous_data = dim==1
        
        ! Get packed data with the norm dimension as the first dimension
        if (.not.contiguous_data) then 
            
            ! Permute array to map dim to 1
            perm = [dim,pack(dim_range,dim_range/=dim)]
            iperm(perm) = dim_range            
            spack = spe(perm)            
            apack = reshape(a, shape=spack, order=iperm)                 
            
            do j4 = lbound(apack, 4), ubound(apack, 4)
            do j3 = lbound(apack, 3), ubound(apack, 3)
            do j2 = lbound(apack, 2), ubound(apack, 2)
                call internal_norm_1D_z(lda, apack(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo            
            
        else
            
            do j4 = lbound(a, 4), ubound(a, 4)
            do j3 = lbound(a, 3), ubound(a, 3)
            do j2 = lbound(a, 2), ubound(a, 2)
                call internal_norm_1D_z(lda, a(:, j2, j3, j4), &
                    nrm(j2, j3, j4), norm_request)
            enddo; enddo; enddo               
            
        endif        
        
    end subroutine norm_4D_to_3D_int_z

    
    !====================================================================
    ! Matrix norms
    !====================================================================    
    
    ! Internal implementation 
    module function matrix_norm_int_z(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        complex(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(dp) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err           
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n
        character :: mat_task
        real(dp), target :: work1(1)
        real(dp), pointer :: work(:)        
        
        m = size(a,dim=1,kind=ilp)
        n = size(a,dim=2,kind=ilp)
        
        ! Initialize norm to zero
        nrm = 0.0_dp
        
        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            call linalg_error_handling(err_,err)
            return
        end if

        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            call linalg_error_handling(err_,err)
            return
        endif       
        
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        else
            work => work1
        end if
                
        if (mat_task==MAT_NORM_SVD) then 
           nrm = maxval(svdvals(a,err_),1) 
           call linalg_error_handling(err_,err)
        else
           ! LAPACK interface 
           nrm = lange(mat_task,m,n,a,m,work)
        end if
        
        if (mat_task==MAT_NORM_INF) deallocate(work)
        
    end function matrix_norm_int_z
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_3D_to_1D_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        complex(dp), intent(in), contiguous, target :: a(:,:,:)
        !> Norm of the matrix.        
        real(dp), allocatable :: nrm(:)
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(3) :: s,spack,perm,iperm
        integer(ilp), dimension(3), parameter :: dim_range = [(m,m=1_ilp,3_ilp)]
        integer(ilp) :: j3
        logical :: contiguous_data
        character :: mat_task
        real(dp), target :: work1(1)
        real(dp), pointer :: work(:)        
        complex(dp), pointer :: apack(:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=3)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',3,' matrix norm has invalid dim=',dims)
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3),err_)
            nrm(j3) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3) = &
            lange(mat_task,m,n,apack(:,:,j3),lda,work)            
        end if
        enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_3D_to_1D_int_z    
    
    
    ! Pure function interface with DIM specifier. On error, the code will stop
    module function matrix_norm_4D_to_2D_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a(m,n)
        complex(dp), intent(in), contiguous, target :: a(:,:,:,:)
        !> Norm of the matrix.        
        real(dp), allocatable :: nrm(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
        integer(ilp), optional, intent(in) :: dim(2)
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err  
        
        type(linalg_state_type) :: err_
        integer(ilp) :: m,n,lda,dims(2),svd_errors
        integer(ilp), dimension(4) :: s,spack,perm,iperm
        integer(ilp), dimension(4), parameter :: dim_range = [(m,m=1_ilp,4_ilp)]
        integer(ilp) :: j3, j4
        logical :: contiguous_data
        character :: mat_task
        real(dp), target :: work1(1)
        real(dp), pointer :: work(:)        
        complex(dp), pointer :: apack(:,:,:,:)
        
        ! Get dimensions
        if (present(dim)) then 
           dims = dim 
        else
           dims = [1,2]
        endif
        
        nullify(apack)
        svd_errors = 0

        if (dims(1)==dims(2) .or. .not.all(dims>0 .and. dims<=4)) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'Rank-',4,' matrix norm has invalid dim=',dims)
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif
        
        ! Check norm request: user + *LANGE support
        call mat_task_request(order,mat_task,err_)
        if (err_%error()) then 
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        endif             
        
        ! Input matrix properties
        s = shape(a,kind=ilp)
        
        ! Check if input column data is contiguous
        contiguous_data = all(dims==[1,2])
        
        ! Matrix norm size
        m = s(dims(1))
        n = s(dims(2))

        if (m<=0 .or. n<=0) then
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix shape: a=',[m,n])
            allocate(nrm(0,0))
            call linalg_error_handling(err_,err)
            return
        end if

        ! Get packed data with norm dimensions as 1:2
        if (contiguous_data) then 
            
            ! Reshape without moving data
            spack =  s
            apack => a
            
        else
            
            ! Dimension permutations to map dims(1),dims(2) => 1:2
            perm = [dims,pack(dim_range, dim_range/=dims(1) .and. dim_range/=dims(2))]            
            iperm(perm) = dim_range            
            spack = s(perm)            
            allocate(apack,source=reshape(a, shape=spack, order=iperm))
            
        endif
            
        if (mat_task==MAT_NORM_INF) then 
            allocate(work(m))
        elseif (mat_task==MAT_NORM_SVD) then 
            allocate(work(min(m,n)))
        else
            work => work1
        endif
        
        ! Allocate norm        
        allocate(nrm(size(apack,3), size(apack,4)))
        
        lda = size(apack,dim=1,kind=ilp)
        
        ! LAPACK interface
        do j4 = lbound(apack, 4), ubound(apack, 4)
        do j3 = lbound(apack, 3), ubound(apack, 3)
        if (mat_task==MAT_NORM_SVD) then
            work(:) = svdvals(apack(:,:,j3, j4),err_)
            nrm(j3, j4) = maxval(work,1)
            if (err_%error()) svd_errors = svd_errors+1
        else
            nrm(j3, j4) = &
            lange(mat_task,m,n,apack(:,:,j3, j4),lda,work)            
        end if
        enddo; enddo
        
        if (any(mat_task==[MAT_NORM_INF,MAT_NORM_SVD])) deallocate(work)
        if (.not.contiguous_data) deallocate(apack)
        
        if (svd_errors>0) then 
            err_ = linalg_state_type(this,LINALG_VALUE_ERROR,svd_errors,'failed SVDs')
            call linalg_error_handling(err_,err)
        endif             
                
    end function matrix_norm_4D_to_2D_int_z    
    
    

end submodule stdlib_linalg_norms
