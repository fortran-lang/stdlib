submodule (stdlib_linalg) stdlib_linalg_least_squares
!! Least-squares solution to Ax=b
     use stdlib_linalg_constants
     use stdlib_linalg_lapack, only: gelsd, gglse, stdlib_ilaenv
     use stdlib_linalg_lapack_aux, only: handle_gelsd_info, handle_gglse_info
     use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
         LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR
     implicit none
     
     character(*), parameter :: this = 'lstsq'

     contains

     ! Workspace needed by gelsd
     elemental subroutine sgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
         integer(ilp), intent(in) :: m,n,nrhs
         integer(ilp), intent(out) :: lrwork,liwork,lcwork

         integer(ilp) :: smlsiz,mnmin,nlvl

         mnmin = min(m,n)

         ! Maximum size of the subproblems at the bottom of the computation (~25)
         smlsiz = stdlib_ilaenv(9,'sgelsd',' ',0,0,0,0)

         ! The exact minimum amount of workspace needed depends on M, N and NRHS. 
         nlvl = max(0, ilog2(mnmin/(smlsiz+1))+1)

         ! Real space
         lrwork = 12*mnmin+2*mnmin*smlsiz+8*mnmin*nlvl+mnmin*nrhs+(smlsiz+1)**2
         lrwork = max(1,lrwork)

         ! Complex space
         lcwork = 2*mnmin + nrhs*mnmin

         ! Integer space
         liwork = max(1, 3*mnmin*nlvl+11*mnmin)

         ! For good performance, the workspace should generally be larger. 
         ! Allocate 25% more space than strictly needed.
         lrwork = ceiling(1.25*lrwork,kind=ilp)
         lcwork = ceiling(1.25*lcwork,kind=ilp)
         liwork = ceiling(1.25*liwork,kind=ilp)

     end subroutine sgelsd_space

     ! Workspace needed by gelsd
     elemental subroutine dgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
         integer(ilp), intent(in) :: m,n,nrhs
         integer(ilp), intent(out) :: lrwork,liwork,lcwork

         integer(ilp) :: smlsiz,mnmin,nlvl

         mnmin = min(m,n)

         ! Maximum size of the subproblems at the bottom of the computation (~25)
         smlsiz = stdlib_ilaenv(9,'dgelsd',' ',0,0,0,0)

         ! The exact minimum amount of workspace needed depends on M, N and NRHS. 
         nlvl = max(0, ilog2(mnmin/(smlsiz+1))+1)

         ! Real space
         lrwork = 12*mnmin+2*mnmin*smlsiz+8*mnmin*nlvl+mnmin*nrhs+(smlsiz+1)**2
         lrwork = max(1,lrwork)

         ! Complex space
         lcwork = 2*mnmin + nrhs*mnmin

         ! Integer space
         liwork = max(1, 3*mnmin*nlvl+11*mnmin)

         ! For good performance, the workspace should generally be larger. 
         ! Allocate 25% more space than strictly needed.
         lrwork = ceiling(1.25*lrwork,kind=ilp)
         lcwork = ceiling(1.25*lcwork,kind=ilp)
         liwork = ceiling(1.25*liwork,kind=ilp)

     end subroutine dgelsd_space

     ! Workspace needed by gelsd
     elemental subroutine cgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
         integer(ilp), intent(in) :: m,n,nrhs
         integer(ilp), intent(out) :: lrwork,liwork,lcwork

         integer(ilp) :: smlsiz,mnmin,nlvl

         mnmin = min(m,n)

         ! Maximum size of the subproblems at the bottom of the computation (~25)
         smlsiz = stdlib_ilaenv(9,'cgelsd',' ',0,0,0,0)

         ! The exact minimum amount of workspace needed depends on M, N and NRHS. 
         nlvl = max(0, ilog2(mnmin/(smlsiz+1))+1)

         ! Real space
         lrwork = 10*mnmin+2*mnmin*smlsiz+8*mnmin*nlvl+3*smlsiz*nrhs+max((smlsiz+1)**2,n*(1+nrhs)+2*nrhs)
         lrwork = max(1,lrwork)

         ! Complex space
         lcwork = 2*mnmin + nrhs*mnmin

         ! Integer space
         liwork = max(1, 3*mnmin*nlvl+11*mnmin)

         ! For good performance, the workspace should generally be larger. 
         ! Allocate 25% more space than strictly needed.
         lrwork = ceiling(1.25*lrwork,kind=ilp)
         lcwork = ceiling(1.25*lcwork,kind=ilp)
         liwork = ceiling(1.25*liwork,kind=ilp)

     end subroutine cgelsd_space

     ! Workspace needed by gelsd
     elemental subroutine zgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
         integer(ilp), intent(in) :: m,n,nrhs
         integer(ilp), intent(out) :: lrwork,liwork,lcwork

         integer(ilp) :: smlsiz,mnmin,nlvl

         mnmin = min(m,n)

         ! Maximum size of the subproblems at the bottom of the computation (~25)
         smlsiz = stdlib_ilaenv(9,'zgelsd',' ',0,0,0,0)

         ! The exact minimum amount of workspace needed depends on M, N and NRHS. 
         nlvl = max(0, ilog2(mnmin/(smlsiz+1))+1)

         ! Real space
         lrwork = 10*mnmin+2*mnmin*smlsiz+8*mnmin*nlvl+3*smlsiz*nrhs+max((smlsiz+1)**2,n*(1+nrhs)+2*nrhs)
         lrwork = max(1,lrwork)

         ! Complex space
         lcwork = 2*mnmin + nrhs*mnmin

         ! Integer space
         liwork = max(1, 3*mnmin*nlvl+11*mnmin)

         ! For good performance, the workspace should generally be larger. 
         ! Allocate 25% more space than strictly needed.
         lrwork = ceiling(1.25*lrwork,kind=ilp)
         lcwork = ceiling(1.25*lcwork,kind=ilp)
         liwork = ceiling(1.25*liwork,kind=ilp)

     end subroutine zgelsd_space



     ! Compute the integer, real, [complex] working space requested by the least squares procedure
     pure module subroutine stdlib_linalg_s_lstsq_space_one(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         real(sp), intent(in) :: b(:)
         !> Size of the working space arrays       
         integer(ilp), intent(out) :: lrwork,liwork
         integer(ilp)  :: lcwork
         
         integer(ilp) :: m,n,nrhs
         
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         nrhs = size(b,kind=ilp)/size(b,1,kind=ilp)
                  
         call sgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
             
     end subroutine stdlib_linalg_s_lstsq_space_one

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module function stdlib_linalg_s_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [m] or matrix of size [m,nrhs].
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag. 
     !! return: x Solution vector of size [n] or solution matrix of size [n,nrhs].
     !!
         !> Input matrix a[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         real(sp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:)         
         
         integer(ilp) :: n,nrhs,ldb

         n    = size(a,2,kind=ilp)
         ldb  = size(b,1,kind=ilp)         
         nrhs = size(b,kind=ilp)/ldb
         
         ! Initialize solution with the shape of the rhs
         allocate(x(n))

         call stdlib_linalg_s_solve_lstsq_one(a,b,x,&
              cond=cond,overwrite_a=overwrite_a,rank=rank,err=err)

     end function stdlib_linalg_s_lstsq_one

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module subroutine stdlib_linalg_s_solve_lstsq_one(a,b,x,real_storage,int_storage, &
              cond,singvals,overwrite_a,rank,err) 
             
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [n] or matrix of size [n,nrhs].
     !! param: x Solution vector of size at [>=n] or solution matrix of size [>=n,nrhs].
     !! param: real_storage [optional] Real working space
     !! param: int_storage [optional] Integer working space
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: singvals [optional] Real array of size [min(m,n)] returning a list of singular values.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag.      
     !!
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(sp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(sp), optional, intent(out), target :: singvals(:)         
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldb,nrhs,ldx,nrhsx,info,mnmin,mnmax,arank,lrwork,liwork,lcwork
         integer(ilp) :: nrs,nis,nsvd
         integer(ilp), pointer :: iwork(:)
         logical(lk) :: copy_a,large_enough_x
         real(sp) :: acond,rcond
         real(sp), pointer :: rwork(:),singular(:)
         real(sp), pointer :: xmat(:,:),amat(:,:)

         ! Problem sizes
         m     = size(a,1,kind=ilp)
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx
         mnmin = min(m,n)
         mnmax = max(m,n)

         if (lda<1 .or. n<1 .or. ldb<1 .or. ldb/=m .or. ldx<n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx])
            call linalg_error_handling(err0,err)
            if (present(rank)) rank = 0
            return
         end if

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! If x is large enough to store b, use it as temporary rhs storage. 
         large_enough_x = ldx>=m
         if (large_enough_x) then          
            xmat(1:ldx,1:nrhs) => x
         else
            allocate(xmat(m,nrhs))
         endif

         xmat(1:m,1) = b

         ! Singular values array (in decreasing order)
         if (present(singvals)) then 
            singular => singvals            
            nsvd = size(singular,kind=ilp)
         else
            allocate(singular(mnmin))
            nsvd = mnmin
         endif
         
         ! rcond is used to determine the effective rank of A.
         ! Singular values S(i) <= RCOND*maxval(S) are treated as zero.
         ! Use same default value as NumPy
         if (present(cond)) then
            rcond = cond
         else
            rcond = epsilon(0.0_sp)*mnmax
         endif
         if (rcond<0) rcond = epsilon(0.0_sp)*mnmax

         ! Get working space size
         call sgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
         
         ! Real working space
         if (present(real_storage)) then 
            rwork => real_storage            
         else
            allocate(rwork(lrwork))
         endif
         nrs = size(rwork,kind=ilp)

         ! Integer working space
         if (present(int_storage)) then 
            iwork => int_storage            
         else
            allocate(iwork(liwork))
         endif
         nis = size(iwork,kind=ilp)
         
                    
         if (nrs<lrwork .or. nis<liwork &
             .or. nsvd<mnmin) then 
            ! Halt on insufficient space
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient working space: ',&
                         'real=',nrs,' should be >=',lrwork, &
                        ', int=',nis,' should be >=',liwork, &
                         
                      ', singv=',nsvd,' should be >=',mnmin)
         
         else

            ! Solve system using singular value decomposition
            call gelsd(m,n,nrhs,amat,lda,xmat,ldb,singular,rcond,arank, &
                       rwork,nrs,iwork,info)

            ! The condition number of A in the 2-norm = S(1)/S(min(m,n)).
            acond = singular(1)/singular(mnmin)

            ! Process output
            call handle_gelsd_info(this,info,lda,n,ldb,nrhs,err0)
         
         endif           
                  
         ! Process output and return
         if (.not.large_enough_x) then 
            x(1:n) = xmat(1:n,1)
            deallocate(xmat)
         endif
         if (copy_a) deallocate(amat)         

         if (present(rank)) rank = arank
         if (.not.present(real_storage)) deallocate(rwork)
         if (.not.present(int_storage))  deallocate(iwork)
         if (.not.present(singvals)) deallocate(singular)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_s_solve_lstsq_one


     ! Compute the integer, real, [complex] working space requested by the least squares procedure
     pure module subroutine stdlib_linalg_d_lstsq_space_one(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         real(dp), intent(in) :: b(:)
         !> Size of the working space arrays       
         integer(ilp), intent(out) :: lrwork,liwork
         integer(ilp)  :: lcwork
         
         integer(ilp) :: m,n,nrhs
         
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         nrhs = size(b,kind=ilp)/size(b,1,kind=ilp)
                  
         call dgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
             
     end subroutine stdlib_linalg_d_lstsq_space_one

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module function stdlib_linalg_d_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [m] or matrix of size [m,nrhs].
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag. 
     !! return: x Solution vector of size [n] or solution matrix of size [n,nrhs].
     !!
         !> Input matrix a[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         real(dp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:)         
         
         integer(ilp) :: n,nrhs,ldb

         n    = size(a,2,kind=ilp)
         ldb  = size(b,1,kind=ilp)         
         nrhs = size(b,kind=ilp)/ldb
         
         ! Initialize solution with the shape of the rhs
         allocate(x(n))

         call stdlib_linalg_d_solve_lstsq_one(a,b,x,&
              cond=cond,overwrite_a=overwrite_a,rank=rank,err=err)

     end function stdlib_linalg_d_lstsq_one

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module subroutine stdlib_linalg_d_solve_lstsq_one(a,b,x,real_storage,int_storage, &
              cond,singvals,overwrite_a,rank,err) 
             
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [n] or matrix of size [n,nrhs].
     !! param: x Solution vector of size at [>=n] or solution matrix of size [>=n,nrhs].
     !! param: real_storage [optional] Real working space
     !! param: int_storage [optional] Integer working space
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: singvals [optional] Real array of size [min(m,n)] returning a list of singular values.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag.      
     !!
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(dp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(dp), optional, intent(out), target :: singvals(:)         
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldb,nrhs,ldx,nrhsx,info,mnmin,mnmax,arank,lrwork,liwork,lcwork
         integer(ilp) :: nrs,nis,nsvd
         integer(ilp), pointer :: iwork(:)
         logical(lk) :: copy_a,large_enough_x
         real(dp) :: acond,rcond
         real(dp), pointer :: rwork(:),singular(:)
         real(dp), pointer :: xmat(:,:),amat(:,:)

         ! Problem sizes
         m     = size(a,1,kind=ilp)
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx
         mnmin = min(m,n)
         mnmax = max(m,n)

         if (lda<1 .or. n<1 .or. ldb<1 .or. ldb/=m .or. ldx<n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx])
            call linalg_error_handling(err0,err)
            if (present(rank)) rank = 0
            return
         end if

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! If x is large enough to store b, use it as temporary rhs storage. 
         large_enough_x = ldx>=m
         if (large_enough_x) then          
            xmat(1:ldx,1:nrhs) => x
         else
            allocate(xmat(m,nrhs))
         endif

         xmat(1:m,1) = b

         ! Singular values array (in decreasing order)
         if (present(singvals)) then 
            singular => singvals            
            nsvd = size(singular,kind=ilp)
         else
            allocate(singular(mnmin))
            nsvd = mnmin
         endif
         
         ! rcond is used to determine the effective rank of A.
         ! Singular values S(i) <= RCOND*maxval(S) are treated as zero.
         ! Use same default value as NumPy
         if (present(cond)) then
            rcond = cond
         else
            rcond = epsilon(0.0_dp)*mnmax
         endif
         if (rcond<0) rcond = epsilon(0.0_dp)*mnmax

         ! Get working space size
         call dgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
         
         ! Real working space
         if (present(real_storage)) then 
            rwork => real_storage            
         else
            allocate(rwork(lrwork))
         endif
         nrs = size(rwork,kind=ilp)

         ! Integer working space
         if (present(int_storage)) then 
            iwork => int_storage            
         else
            allocate(iwork(liwork))
         endif
         nis = size(iwork,kind=ilp)
         
                    
         if (nrs<lrwork .or. nis<liwork &
             .or. nsvd<mnmin) then 
            ! Halt on insufficient space
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient working space: ',&
                         'real=',nrs,' should be >=',lrwork, &
                        ', int=',nis,' should be >=',liwork, &
                         
                      ', singv=',nsvd,' should be >=',mnmin)
         
         else

            ! Solve system using singular value decomposition
            call gelsd(m,n,nrhs,amat,lda,xmat,ldb,singular,rcond,arank, &
                       rwork,nrs,iwork,info)

            ! The condition number of A in the 2-norm = S(1)/S(min(m,n)).
            acond = singular(1)/singular(mnmin)

            ! Process output
            call handle_gelsd_info(this,info,lda,n,ldb,nrhs,err0)
         
         endif           
                  
         ! Process output and return
         if (.not.large_enough_x) then 
            x(1:n) = xmat(1:n,1)
            deallocate(xmat)
         endif
         if (copy_a) deallocate(amat)         

         if (present(rank)) rank = arank
         if (.not.present(real_storage)) deallocate(rwork)
         if (.not.present(int_storage))  deallocate(iwork)
         if (.not.present(singvals)) deallocate(singular)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_d_solve_lstsq_one


     ! Compute the integer, real, [complex] working space requested by the least squares procedure
     pure module subroutine stdlib_linalg_c_lstsq_space_one(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         complex(sp), intent(in) :: b(:)
         !> Size of the working space arrays       
         integer(ilp), intent(out) :: lrwork,liwork
         integer(ilp) , intent(out) :: lcwork
         
         integer(ilp) :: m,n,nrhs
         
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         nrhs = size(b,kind=ilp)/size(b,1,kind=ilp)
                  
         call cgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
             
     end subroutine stdlib_linalg_c_lstsq_space_one

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module function stdlib_linalg_c_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [m] or matrix of size [m,nrhs].
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag. 
     !! return: x Solution vector of size [n] or solution matrix of size [n,nrhs].
     !!
         !> Input matrix a[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         complex(sp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:)         
         
         integer(ilp) :: n,nrhs,ldb

         n    = size(a,2,kind=ilp)
         ldb  = size(b,1,kind=ilp)         
         nrhs = size(b,kind=ilp)/ldb
         
         ! Initialize solution with the shape of the rhs
         allocate(x(n))

         call stdlib_linalg_c_solve_lstsq_one(a,b,x,&
              cond=cond,overwrite_a=overwrite_a,rank=rank,err=err)

     end function stdlib_linalg_c_lstsq_one

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module subroutine stdlib_linalg_c_solve_lstsq_one(a,b,x,real_storage,int_storage, &
             cmpl_storage, cond,singvals,overwrite_a,rank,err) 
             
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [n] or matrix of size [n,nrhs].
     !! param: x Solution vector of size at [>=n] or solution matrix of size [>=n,nrhs].
     !! param: real_storage [optional] Real working space
     !! param: int_storage [optional] Integer working space
     !! param: cmpl_storage [optional] Complex working space
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: singvals [optional] Real array of size [min(m,n)] returning a list of singular values.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag.      
     !!
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(sp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(sp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(sp), optional, intent(out), target :: singvals(:)         
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldb,nrhs,ldx,nrhsx,info,mnmin,mnmax,arank,lrwork,liwork,lcwork
         integer(ilp) :: nrs,nis,nsvd
         integer(ilp) :: ncs
         integer(ilp), pointer :: iwork(:)
         logical(lk) :: copy_a,large_enough_x
         real(sp) :: acond,rcond
         real(sp), pointer :: rwork(:),singular(:)
         complex(sp), pointer :: xmat(:,:),amat(:,:)
         complex(sp), pointer :: cwork(:)

         ! Problem sizes
         m     = size(a,1,kind=ilp)
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx
         mnmin = min(m,n)
         mnmax = max(m,n)

         if (lda<1 .or. n<1 .or. ldb<1 .or. ldb/=m .or. ldx<n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx])
            call linalg_error_handling(err0,err)
            if (present(rank)) rank = 0
            return
         end if

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! If x is large enough to store b, use it as temporary rhs storage. 
         large_enough_x = ldx>=m
         if (large_enough_x) then          
            xmat(1:ldx,1:nrhs) => x
         else
            allocate(xmat(m,nrhs))
         endif

         xmat(1:m,1) = b

         ! Singular values array (in decreasing order)
         if (present(singvals)) then 
            singular => singvals            
            nsvd = size(singular,kind=ilp)
         else
            allocate(singular(mnmin))
            nsvd = mnmin
         endif
         
         ! rcond is used to determine the effective rank of A.
         ! Singular values S(i) <= RCOND*maxval(S) are treated as zero.
         ! Use same default value as NumPy
         if (present(cond)) then
            rcond = cond
         else
            rcond = epsilon(0.0_sp)*mnmax
         endif
         if (rcond<0) rcond = epsilon(0.0_sp)*mnmax

         ! Get working space size
         call cgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
         
         ! Real working space
         if (present(real_storage)) then 
            rwork => real_storage            
         else
            allocate(rwork(lrwork))
         endif
         nrs = size(rwork,kind=ilp)

         ! Integer working space
         if (present(int_storage)) then 
            iwork => int_storage            
         else
            allocate(iwork(liwork))
         endif
         nis = size(iwork,kind=ilp)
         
         ! Complex working space
         if (present(cmpl_storage)) then 
            cwork => cmpl_storage            
         else
            allocate(cwork(lcwork))
         endif
         ncs = size(cwork,kind=ilp)
                    
         if (nrs<lrwork .or. nis<liwork .or. ncs<lcwork &
             .or. nsvd<mnmin) then 
            ! Halt on insufficient space
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient working space: ',&
                         'real=',nrs,' should be >=',lrwork, &
                        ', int=',nis,' should be >=',liwork, &
                        ', cmplx=',ncs,' should be >=',lcwork, & 
                      ', singv=',nsvd,' should be >=',mnmin)
         
         else

            ! Solve system using singular value decomposition
            call gelsd(m,n,nrhs,amat,lda,xmat,ldb,singular,rcond,arank, &
                       cwork,ncs,rwork,iwork,info)

            ! The condition number of A in the 2-norm = S(1)/S(min(m,n)).
            acond = singular(1)/singular(mnmin)

            ! Process output
            call handle_gelsd_info(this,info,lda,n,ldb,nrhs,err0)
         
         endif           
                  
         ! Process output and return
         if (.not.large_enough_x) then 
            x(1:n) = xmat(1:n,1)
            deallocate(xmat)
         endif
         if (copy_a) deallocate(amat)         

         if (present(rank)) rank = arank
         if (.not.present(real_storage)) deallocate(rwork)
         if (.not.present(int_storage))  deallocate(iwork)
         if (.not.present(cmpl_storage)) deallocate(cwork)
         if (.not.present(singvals)) deallocate(singular)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_c_solve_lstsq_one


     ! Compute the integer, real, [complex] working space requested by the least squares procedure
     pure module subroutine stdlib_linalg_z_lstsq_space_one(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         complex(dp), intent(in) :: b(:)
         !> Size of the working space arrays       
         integer(ilp), intent(out) :: lrwork,liwork
         integer(ilp) , intent(out) :: lcwork
         
         integer(ilp) :: m,n,nrhs
         
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         nrhs = size(b,kind=ilp)/size(b,1,kind=ilp)
                  
         call zgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
             
     end subroutine stdlib_linalg_z_lstsq_space_one

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module function stdlib_linalg_z_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [m] or matrix of size [m,nrhs].
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag. 
     !! return: x Solution vector of size [n] or solution matrix of size [n,nrhs].
     !!
         !> Input matrix a[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         complex(dp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:)         
         
         integer(ilp) :: n,nrhs,ldb

         n    = size(a,2,kind=ilp)
         ldb  = size(b,1,kind=ilp)         
         nrhs = size(b,kind=ilp)/ldb
         
         ! Initialize solution with the shape of the rhs
         allocate(x(n))

         call stdlib_linalg_z_solve_lstsq_one(a,b,x,&
              cond=cond,overwrite_a=overwrite_a,rank=rank,err=err)

     end function stdlib_linalg_z_lstsq_one

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module subroutine stdlib_linalg_z_solve_lstsq_one(a,b,x,real_storage,int_storage, &
             cmpl_storage, cond,singvals,overwrite_a,rank,err) 
             
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [n] or matrix of size [n,nrhs].
     !! param: x Solution vector of size at [>=n] or solution matrix of size [>=n,nrhs].
     !! param: real_storage [optional] Real working space
     !! param: int_storage [optional] Integer working space
     !! param: cmpl_storage [optional] Complex working space
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: singvals [optional] Real array of size [min(m,n)] returning a list of singular values.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag.      
     !!
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(dp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(dp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(dp), optional, intent(out), target :: singvals(:)         
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldb,nrhs,ldx,nrhsx,info,mnmin,mnmax,arank,lrwork,liwork,lcwork
         integer(ilp) :: nrs,nis,nsvd
         integer(ilp) :: ncs
         integer(ilp), pointer :: iwork(:)
         logical(lk) :: copy_a,large_enough_x
         real(dp) :: acond,rcond
         real(dp), pointer :: rwork(:),singular(:)
         complex(dp), pointer :: xmat(:,:),amat(:,:)
         complex(dp), pointer :: cwork(:)

         ! Problem sizes
         m     = size(a,1,kind=ilp)
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx
         mnmin = min(m,n)
         mnmax = max(m,n)

         if (lda<1 .or. n<1 .or. ldb<1 .or. ldb/=m .or. ldx<n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx])
            call linalg_error_handling(err0,err)
            if (present(rank)) rank = 0
            return
         end if

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! If x is large enough to store b, use it as temporary rhs storage. 
         large_enough_x = ldx>=m
         if (large_enough_x) then          
            xmat(1:ldx,1:nrhs) => x
         else
            allocate(xmat(m,nrhs))
         endif

         xmat(1:m,1) = b

         ! Singular values array (in decreasing order)
         if (present(singvals)) then 
            singular => singvals            
            nsvd = size(singular,kind=ilp)
         else
            allocate(singular(mnmin))
            nsvd = mnmin
         endif
         
         ! rcond is used to determine the effective rank of A.
         ! Singular values S(i) <= RCOND*maxval(S) are treated as zero.
         ! Use same default value as NumPy
         if (present(cond)) then
            rcond = cond
         else
            rcond = epsilon(0.0_dp)*mnmax
         endif
         if (rcond<0) rcond = epsilon(0.0_dp)*mnmax

         ! Get working space size
         call zgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
         
         ! Real working space
         if (present(real_storage)) then 
            rwork => real_storage            
         else
            allocate(rwork(lrwork))
         endif
         nrs = size(rwork,kind=ilp)

         ! Integer working space
         if (present(int_storage)) then 
            iwork => int_storage            
         else
            allocate(iwork(liwork))
         endif
         nis = size(iwork,kind=ilp)
         
         ! Complex working space
         if (present(cmpl_storage)) then 
            cwork => cmpl_storage            
         else
            allocate(cwork(lcwork))
         endif
         ncs = size(cwork,kind=ilp)
                    
         if (nrs<lrwork .or. nis<liwork .or. ncs<lcwork &
             .or. nsvd<mnmin) then 
            ! Halt on insufficient space
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient working space: ',&
                         'real=',nrs,' should be >=',lrwork, &
                        ', int=',nis,' should be >=',liwork, &
                        ', cmplx=',ncs,' should be >=',lcwork, & 
                      ', singv=',nsvd,' should be >=',mnmin)
         
         else

            ! Solve system using singular value decomposition
            call gelsd(m,n,nrhs,amat,lda,xmat,ldb,singular,rcond,arank, &
                       cwork,ncs,rwork,iwork,info)

            ! The condition number of A in the 2-norm = S(1)/S(min(m,n)).
            acond = singular(1)/singular(mnmin)

            ! Process output
            call handle_gelsd_info(this,info,lda,n,ldb,nrhs,err0)
         
         endif           
                  
         ! Process output and return
         if (.not.large_enough_x) then 
            x(1:n) = xmat(1:n,1)
            deallocate(xmat)
         endif
         if (copy_a) deallocate(amat)         

         if (present(rank)) rank = arank
         if (.not.present(real_storage)) deallocate(rwork)
         if (.not.present(int_storage))  deallocate(iwork)
         if (.not.present(cmpl_storage)) deallocate(cwork)
         if (.not.present(singvals)) deallocate(singular)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_z_solve_lstsq_one


     ! Compute the integer, real, [complex] working space requested by the least squares procedure
     pure module subroutine stdlib_linalg_s_lstsq_space_many(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> Size of the working space arrays       
         integer(ilp), intent(out) :: lrwork,liwork
         integer(ilp)  :: lcwork
         
         integer(ilp) :: m,n,nrhs
         
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         nrhs = size(b,kind=ilp)/size(b,1,kind=ilp)
                  
         call sgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
             
     end subroutine stdlib_linalg_s_lstsq_space_many

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module function stdlib_linalg_s_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [m] or matrix of size [m,nrhs].
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag. 
     !! return: x Solution vector of size [n] or solution matrix of size [n,nrhs].
     !!
         !> Input matrix a[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:,:)         
         
         integer(ilp) :: n,nrhs,ldb

         n    = size(a,2,kind=ilp)
         ldb  = size(b,1,kind=ilp)         
         nrhs = size(b,kind=ilp)/ldb
         
         ! Initialize solution with the shape of the rhs
         allocate(x(n,nrhs))

         call stdlib_linalg_s_solve_lstsq_many(a,b,x,&
              cond=cond,overwrite_a=overwrite_a,rank=rank,err=err)

     end function stdlib_linalg_s_lstsq_many

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module subroutine stdlib_linalg_s_solve_lstsq_many(a,b,x,real_storage,int_storage, &
              cond,singvals,overwrite_a,rank,err) 
             
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [n] or matrix of size [n,nrhs].
     !! param: x Solution vector of size at [>=n] or solution matrix of size [>=n,nrhs].
     !! param: real_storage [optional] Real working space
     !! param: int_storage [optional] Integer working space
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: singvals [optional] Real array of size [min(m,n)] returning a list of singular values.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag.      
     !!
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(sp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(sp), optional, intent(out), target :: singvals(:)         
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldb,nrhs,ldx,nrhsx,info,mnmin,mnmax,arank,lrwork,liwork,lcwork
         integer(ilp) :: nrs,nis,nsvd
         integer(ilp), pointer :: iwork(:)
         logical(lk) :: copy_a,large_enough_x
         real(sp) :: acond,rcond
         real(sp), pointer :: rwork(:),singular(:)
         real(sp), pointer :: xmat(:,:),amat(:,:)

         ! Problem sizes
         m     = size(a,1,kind=ilp)
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx
         mnmin = min(m,n)
         mnmax = max(m,n)

         if (lda<1 .or. n<1 .or. ldb<1 .or. ldb/=m .or. ldx<n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx])
            call linalg_error_handling(err0,err)
            if (present(rank)) rank = 0
            return
         end if

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! If x is large enough to store b, use it as temporary rhs storage. 
         large_enough_x = ldx>=m
         if (large_enough_x) then          
            xmat(1:ldx,1:nrhs) => x
         else
            allocate(xmat(m,nrhs))
         endif

         xmat(1:m,1:nrhs) = b

         ! Singular values array (in decreasing order)
         if (present(singvals)) then 
            singular => singvals            
            nsvd = size(singular,kind=ilp)
         else
            allocate(singular(mnmin))
            nsvd = mnmin
         endif
         
         ! rcond is used to determine the effective rank of A.
         ! Singular values S(i) <= RCOND*maxval(S) are treated as zero.
         ! Use same default value as NumPy
         if (present(cond)) then
            rcond = cond
         else
            rcond = epsilon(0.0_sp)*mnmax
         endif
         if (rcond<0) rcond = epsilon(0.0_sp)*mnmax

         ! Get working space size
         call sgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
         
         ! Real working space
         if (present(real_storage)) then 
            rwork => real_storage            
         else
            allocate(rwork(lrwork))
         endif
         nrs = size(rwork,kind=ilp)

         ! Integer working space
         if (present(int_storage)) then 
            iwork => int_storage            
         else
            allocate(iwork(liwork))
         endif
         nis = size(iwork,kind=ilp)
         
                    
         if (nrs<lrwork .or. nis<liwork &
             .or. nsvd<mnmin) then 
            ! Halt on insufficient space
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient working space: ',&
                         'real=',nrs,' should be >=',lrwork, &
                        ', int=',nis,' should be >=',liwork, &
                         
                      ', singv=',nsvd,' should be >=',mnmin)
         
         else

            ! Solve system using singular value decomposition
            call gelsd(m,n,nrhs,amat,lda,xmat,ldb,singular,rcond,arank, &
                       rwork,nrs,iwork,info)

            ! The condition number of A in the 2-norm = S(1)/S(min(m,n)).
            acond = singular(1)/singular(mnmin)

            ! Process output
            call handle_gelsd_info(this,info,lda,n,ldb,nrhs,err0)
         
         endif           
                  
         ! Process output and return
         if (.not.large_enough_x) then 
            x(1:n,1:nrhs) = xmat(1:n,1:nrhs)
            deallocate(xmat)
         endif
         if (copy_a) deallocate(amat)         

         if (present(rank)) rank = arank
         if (.not.present(real_storage)) deallocate(rwork)
         if (.not.present(int_storage))  deallocate(iwork)
         if (.not.present(singvals)) deallocate(singular)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_s_solve_lstsq_many


     ! Compute the integer, real, [complex] working space requested by the least squares procedure
     pure module subroutine stdlib_linalg_d_lstsq_space_many(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> Size of the working space arrays       
         integer(ilp), intent(out) :: lrwork,liwork
         integer(ilp)  :: lcwork
         
         integer(ilp) :: m,n,nrhs
         
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         nrhs = size(b,kind=ilp)/size(b,1,kind=ilp)
                  
         call dgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
             
     end subroutine stdlib_linalg_d_lstsq_space_many

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module function stdlib_linalg_d_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [m] or matrix of size [m,nrhs].
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag. 
     !! return: x Solution vector of size [n] or solution matrix of size [n,nrhs].
     !!
         !> Input matrix a[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:,:)         
         
         integer(ilp) :: n,nrhs,ldb

         n    = size(a,2,kind=ilp)
         ldb  = size(b,1,kind=ilp)         
         nrhs = size(b,kind=ilp)/ldb
         
         ! Initialize solution with the shape of the rhs
         allocate(x(n,nrhs))

         call stdlib_linalg_d_solve_lstsq_many(a,b,x,&
              cond=cond,overwrite_a=overwrite_a,rank=rank,err=err)

     end function stdlib_linalg_d_lstsq_many

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module subroutine stdlib_linalg_d_solve_lstsq_many(a,b,x,real_storage,int_storage, &
              cond,singvals,overwrite_a,rank,err) 
             
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [n] or matrix of size [n,nrhs].
     !! param: x Solution vector of size at [>=n] or solution matrix of size [>=n,nrhs].
     !! param: real_storage [optional] Real working space
     !! param: int_storage [optional] Integer working space
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: singvals [optional] Real array of size [min(m,n)] returning a list of singular values.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag.      
     !!
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(dp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(dp), optional, intent(out), target :: singvals(:)         
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldb,nrhs,ldx,nrhsx,info,mnmin,mnmax,arank,lrwork,liwork,lcwork
         integer(ilp) :: nrs,nis,nsvd
         integer(ilp), pointer :: iwork(:)
         logical(lk) :: copy_a,large_enough_x
         real(dp) :: acond,rcond
         real(dp), pointer :: rwork(:),singular(:)
         real(dp), pointer :: xmat(:,:),amat(:,:)

         ! Problem sizes
         m     = size(a,1,kind=ilp)
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx
         mnmin = min(m,n)
         mnmax = max(m,n)

         if (lda<1 .or. n<1 .or. ldb<1 .or. ldb/=m .or. ldx<n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx])
            call linalg_error_handling(err0,err)
            if (present(rank)) rank = 0
            return
         end if

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! If x is large enough to store b, use it as temporary rhs storage. 
         large_enough_x = ldx>=m
         if (large_enough_x) then          
            xmat(1:ldx,1:nrhs) => x
         else
            allocate(xmat(m,nrhs))
         endif

         xmat(1:m,1:nrhs) = b

         ! Singular values array (in decreasing order)
         if (present(singvals)) then 
            singular => singvals            
            nsvd = size(singular,kind=ilp)
         else
            allocate(singular(mnmin))
            nsvd = mnmin
         endif
         
         ! rcond is used to determine the effective rank of A.
         ! Singular values S(i) <= RCOND*maxval(S) are treated as zero.
         ! Use same default value as NumPy
         if (present(cond)) then
            rcond = cond
         else
            rcond = epsilon(0.0_dp)*mnmax
         endif
         if (rcond<0) rcond = epsilon(0.0_dp)*mnmax

         ! Get working space size
         call dgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
         
         ! Real working space
         if (present(real_storage)) then 
            rwork => real_storage            
         else
            allocate(rwork(lrwork))
         endif
         nrs = size(rwork,kind=ilp)

         ! Integer working space
         if (present(int_storage)) then 
            iwork => int_storage            
         else
            allocate(iwork(liwork))
         endif
         nis = size(iwork,kind=ilp)
         
                    
         if (nrs<lrwork .or. nis<liwork &
             .or. nsvd<mnmin) then 
            ! Halt on insufficient space
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient working space: ',&
                         'real=',nrs,' should be >=',lrwork, &
                        ', int=',nis,' should be >=',liwork, &
                         
                      ', singv=',nsvd,' should be >=',mnmin)
         
         else

            ! Solve system using singular value decomposition
            call gelsd(m,n,nrhs,amat,lda,xmat,ldb,singular,rcond,arank, &
                       rwork,nrs,iwork,info)

            ! The condition number of A in the 2-norm = S(1)/S(min(m,n)).
            acond = singular(1)/singular(mnmin)

            ! Process output
            call handle_gelsd_info(this,info,lda,n,ldb,nrhs,err0)
         
         endif           
                  
         ! Process output and return
         if (.not.large_enough_x) then 
            x(1:n,1:nrhs) = xmat(1:n,1:nrhs)
            deallocate(xmat)
         endif
         if (copy_a) deallocate(amat)         

         if (present(rank)) rank = arank
         if (.not.present(real_storage)) deallocate(rwork)
         if (.not.present(int_storage))  deallocate(iwork)
         if (.not.present(singvals)) deallocate(singular)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_d_solve_lstsq_many


     ! Compute the integer, real, [complex] working space requested by the least squares procedure
     pure module subroutine stdlib_linalg_c_lstsq_space_many(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> Size of the working space arrays       
         integer(ilp), intent(out) :: lrwork,liwork
         integer(ilp) , intent(out) :: lcwork
         
         integer(ilp) :: m,n,nrhs
         
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         nrhs = size(b,kind=ilp)/size(b,1,kind=ilp)
                  
         call cgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
             
     end subroutine stdlib_linalg_c_lstsq_space_many

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module function stdlib_linalg_c_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [m] or matrix of size [m,nrhs].
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag. 
     !! return: x Solution vector of size [n] or solution matrix of size [n,nrhs].
     !!
         !> Input matrix a[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:,:)         
         
         integer(ilp) :: n,nrhs,ldb

         n    = size(a,2,kind=ilp)
         ldb  = size(b,1,kind=ilp)         
         nrhs = size(b,kind=ilp)/ldb
         
         ! Initialize solution with the shape of the rhs
         allocate(x(n,nrhs))

         call stdlib_linalg_c_solve_lstsq_many(a,b,x,&
              cond=cond,overwrite_a=overwrite_a,rank=rank,err=err)

     end function stdlib_linalg_c_lstsq_many

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module subroutine stdlib_linalg_c_solve_lstsq_many(a,b,x,real_storage,int_storage, &
             cmpl_storage, cond,singvals,overwrite_a,rank,err) 
             
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [n] or matrix of size [n,nrhs].
     !! param: x Solution vector of size at [>=n] or solution matrix of size [>=n,nrhs].
     !! param: real_storage [optional] Real working space
     !! param: int_storage [optional] Integer working space
     !! param: cmpl_storage [optional] Complex working space
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: singvals [optional] Real array of size [min(m,n)] returning a list of singular values.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag.      
     !!
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(sp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(sp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(sp), optional, intent(out), target :: singvals(:)         
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldb,nrhs,ldx,nrhsx,info,mnmin,mnmax,arank,lrwork,liwork,lcwork
         integer(ilp) :: nrs,nis,nsvd
         integer(ilp) :: ncs
         integer(ilp), pointer :: iwork(:)
         logical(lk) :: copy_a,large_enough_x
         real(sp) :: acond,rcond
         real(sp), pointer :: rwork(:),singular(:)
         complex(sp), pointer :: xmat(:,:),amat(:,:)
         complex(sp), pointer :: cwork(:)

         ! Problem sizes
         m     = size(a,1,kind=ilp)
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx
         mnmin = min(m,n)
         mnmax = max(m,n)

         if (lda<1 .or. n<1 .or. ldb<1 .or. ldb/=m .or. ldx<n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx])
            call linalg_error_handling(err0,err)
            if (present(rank)) rank = 0
            return
         end if

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! If x is large enough to store b, use it as temporary rhs storage. 
         large_enough_x = ldx>=m
         if (large_enough_x) then          
            xmat(1:ldx,1:nrhs) => x
         else
            allocate(xmat(m,nrhs))
         endif

         xmat(1:m,1:nrhs) = b

         ! Singular values array (in decreasing order)
         if (present(singvals)) then 
            singular => singvals            
            nsvd = size(singular,kind=ilp)
         else
            allocate(singular(mnmin))
            nsvd = mnmin
         endif
         
         ! rcond is used to determine the effective rank of A.
         ! Singular values S(i) <= RCOND*maxval(S) are treated as zero.
         ! Use same default value as NumPy
         if (present(cond)) then
            rcond = cond
         else
            rcond = epsilon(0.0_sp)*mnmax
         endif
         if (rcond<0) rcond = epsilon(0.0_sp)*mnmax

         ! Get working space size
         call cgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
         
         ! Real working space
         if (present(real_storage)) then 
            rwork => real_storage            
         else
            allocate(rwork(lrwork))
         endif
         nrs = size(rwork,kind=ilp)

         ! Integer working space
         if (present(int_storage)) then 
            iwork => int_storage            
         else
            allocate(iwork(liwork))
         endif
         nis = size(iwork,kind=ilp)
         
         ! Complex working space
         if (present(cmpl_storage)) then 
            cwork => cmpl_storage            
         else
            allocate(cwork(lcwork))
         endif
         ncs = size(cwork,kind=ilp)
                    
         if (nrs<lrwork .or. nis<liwork .or. ncs<lcwork &
             .or. nsvd<mnmin) then 
            ! Halt on insufficient space
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient working space: ',&
                         'real=',nrs,' should be >=',lrwork, &
                        ', int=',nis,' should be >=',liwork, &
                        ', cmplx=',ncs,' should be >=',lcwork, & 
                      ', singv=',nsvd,' should be >=',mnmin)
         
         else

            ! Solve system using singular value decomposition
            call gelsd(m,n,nrhs,amat,lda,xmat,ldb,singular,rcond,arank, &
                       cwork,ncs,rwork,iwork,info)

            ! The condition number of A in the 2-norm = S(1)/S(min(m,n)).
            acond = singular(1)/singular(mnmin)

            ! Process output
            call handle_gelsd_info(this,info,lda,n,ldb,nrhs,err0)
         
         endif           
                  
         ! Process output and return
         if (.not.large_enough_x) then 
            x(1:n,1:nrhs) = xmat(1:n,1:nrhs)
            deallocate(xmat)
         endif
         if (copy_a) deallocate(amat)         

         if (present(rank)) rank = arank
         if (.not.present(real_storage)) deallocate(rwork)
         if (.not.present(int_storage))  deallocate(iwork)
         if (.not.present(cmpl_storage)) deallocate(cwork)
         if (.not.present(singvals)) deallocate(singular)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_c_solve_lstsq_many


     ! Compute the integer, real, [complex] working space requested by the least squares procedure
     pure module subroutine stdlib_linalg_z_lstsq_space_many(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> Size of the working space arrays       
         integer(ilp), intent(out) :: lrwork,liwork
         integer(ilp) , intent(out) :: lcwork
         
         integer(ilp) :: m,n,nrhs
         
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         nrhs = size(b,kind=ilp)/size(b,1,kind=ilp)
                  
         call zgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
             
     end subroutine stdlib_linalg_z_lstsq_space_many

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module function stdlib_linalg_z_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [m] or matrix of size [m,nrhs].
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag. 
     !! return: x Solution vector of size [n] or solution matrix of size [n,nrhs].
     !!
         !> Input matrix a[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[m] or b[m,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:,:)         
         
         integer(ilp) :: n,nrhs,ldb

         n    = size(a,2,kind=ilp)
         ldb  = size(b,1,kind=ilp)         
         nrhs = size(b,kind=ilp)/ldb
         
         ! Initialize solution with the shape of the rhs
         allocate(x(n,nrhs))

         call stdlib_linalg_z_solve_lstsq_many(a,b,x,&
              cond=cond,overwrite_a=overwrite_a,rank=rank,err=err)

     end function stdlib_linalg_z_lstsq_many

     ! Compute the least-squares solution to a real system of linear equations Ax = b
     module subroutine stdlib_linalg_z_solve_lstsq_many(a,b,x,real_storage,int_storage, &
             cmpl_storage, cond,singvals,overwrite_a,rank,err) 
             
     !!### Summary
     !! Compute least-squares solution to a real system of linear equations \( Ax = b \) 
     !!
     !!### Description
     !!
     !! This function computes the least-squares solution of a linear matrix problem.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: b Right-hand-side vector of size [n] or matrix of size [n,nrhs].
     !! param: x Solution vector of size at [>=n] or solution matrix of size [>=n,nrhs].
     !! param: real_storage [optional] Real working space
     !! param: int_storage [optional] Integer working space
     !! param: cmpl_storage [optional] Complex working space
     !! param: cond [optional] Real input threshold indicating that singular values `s_i <= cond*maxval(s)` 
     !!        do not contribute to the matrix rank.
     !! param: singvals [optional] Real array of size [min(m,n)] returning a list of singular values.
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: rank [optional] integer flag returning matrix rank. 
     !! param: err [optional] State return flag.      
     !!
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(dp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(dp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(dp), optional, intent(out), target :: singvals(:)         
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldb,nrhs,ldx,nrhsx,info,mnmin,mnmax,arank,lrwork,liwork,lcwork
         integer(ilp) :: nrs,nis,nsvd
         integer(ilp) :: ncs
         integer(ilp), pointer :: iwork(:)
         logical(lk) :: copy_a,large_enough_x
         real(dp) :: acond,rcond
         real(dp), pointer :: rwork(:),singular(:)
         complex(dp), pointer :: xmat(:,:),amat(:,:)
         complex(dp), pointer :: cwork(:)

         ! Problem sizes
         m     = size(a,1,kind=ilp)
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx
         mnmin = min(m,n)
         mnmax = max(m,n)

         if (lda<1 .or. n<1 .or. ldb<1 .or. ldb/=m .or. ldx<n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx])
            call linalg_error_handling(err0,err)
            if (present(rank)) rank = 0
            return
         end if

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! If x is large enough to store b, use it as temporary rhs storage. 
         large_enough_x = ldx>=m
         if (large_enough_x) then          
            xmat(1:ldx,1:nrhs) => x
         else
            allocate(xmat(m,nrhs))
         endif

         xmat(1:m,1:nrhs) = b

         ! Singular values array (in decreasing order)
         if (present(singvals)) then 
            singular => singvals            
            nsvd = size(singular,kind=ilp)
         else
            allocate(singular(mnmin))
            nsvd = mnmin
         endif
         
         ! rcond is used to determine the effective rank of A.
         ! Singular values S(i) <= RCOND*maxval(S) are treated as zero.
         ! Use same default value as NumPy
         if (present(cond)) then
            rcond = cond
         else
            rcond = epsilon(0.0_dp)*mnmax
         endif
         if (rcond<0) rcond = epsilon(0.0_dp)*mnmax

         ! Get working space size
         call zgelsd_space(m,n,nrhs,lrwork,liwork,lcwork)
         
         ! Real working space
         if (present(real_storage)) then 
            rwork => real_storage            
         else
            allocate(rwork(lrwork))
         endif
         nrs = size(rwork,kind=ilp)

         ! Integer working space
         if (present(int_storage)) then 
            iwork => int_storage            
         else
            allocate(iwork(liwork))
         endif
         nis = size(iwork,kind=ilp)
         
         ! Complex working space
         if (present(cmpl_storage)) then 
            cwork => cmpl_storage            
         else
            allocate(cwork(lcwork))
         endif
         ncs = size(cwork,kind=ilp)
                    
         if (nrs<lrwork .or. nis<liwork .or. ncs<lcwork &
             .or. nsvd<mnmin) then 
            ! Halt on insufficient space
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient working space: ',&
                         'real=',nrs,' should be >=',lrwork, &
                        ', int=',nis,' should be >=',liwork, &
                        ', cmplx=',ncs,' should be >=',lcwork, & 
                      ', singv=',nsvd,' should be >=',mnmin)
         
         else

            ! Solve system using singular value decomposition
            call gelsd(m,n,nrhs,amat,lda,xmat,ldb,singular,rcond,arank, &
                       cwork,ncs,rwork,iwork,info)

            ! The condition number of A in the 2-norm = S(1)/S(min(m,n)).
            acond = singular(1)/singular(mnmin)

            ! Process output
            call handle_gelsd_info(this,info,lda,n,ldb,nrhs,err0)
         
         endif           
                  
         ! Process output and return
         if (.not.large_enough_x) then 
            x(1:n,1:nrhs) = xmat(1:n,1:nrhs)
            deallocate(xmat)
         endif
         if (copy_a) deallocate(amat)         

         if (present(rank)) rank = arank
         if (.not.present(real_storage)) deallocate(rwork)
         if (.not.present(int_storage))  deallocate(iwork)
         if (.not.present(cmpl_storage)) deallocate(cwork)
         if (.not.present(singvals)) deallocate(singular)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_z_solve_lstsq_many


     ! Simple integer log2 implementation
     elemental integer(ilp) function ilog2(x)
        integer(ilp), intent(in) :: x

        integer(ilp) :: remndr

        if (x>0) then
           remndr = x
           ilog2 = -1_ilp
           do while (remndr>0)
               ilog2  = ilog2 + 1_ilp
               remndr = shiftr(remndr,1)
           end do
        else
           ilog2 = -huge(0_ilp)
        endif
     end function ilog2

    !-------------------------------------------------------------
    !-----     Equality-constrained Least-Squares solver     -----
    !-------------------------------------------------------------

    pure subroutine check_problem_size(ma, na, mb, mc, nc, md, mx, err)
        integer(ilp), intent(in) :: ma, na, mb, mc, nc, md, mx
        type(linalg_state_type), intent(out) :: err

        ! Check sizes.
        if (ma < 1 .or. na < 1) then
            err = linalg_state_type(this, LINALG_VALUE_ERROR, 'Invalid matrix size a(m, n) =', [ma, na])
            return
        else if (mc < 1 .or. nc < 1) then
            err = linalg_state_type(this, LINALG_VALUE_ERROR, 'Invalid matrix size c(p, n) =', [mc, nc])
        else if (na /= nc) then
            err = linalg_state_type(this, LINALG_VALUE_ERROR, 'Matrix A and matrix C have inconsistent number of columns.')
        else if (mb /= ma) then
            err = linalg_state_type(this, LINALG_VALUE_ERROR, 'Size(b) inconsistent with number of rows in a, size(b) =', mb)
        else if (md /= mc) then
            err = linalg_state_type(this, LINALG_VALUE_ERROR, 'Size(d) inconsistent with number of rows in c, size(d) =', md)
        else if (na /= mx) then
            err = linalg_state_type(this, LINALG_VALUE_ERROR, 'Size(x) inconsistent with number of columns of a, size(x) =', mx)
        endif
    end subroutine check_problem_size

    ! Compute the size of the workspace requested by the constrained least-squares procedure.
    module subroutine stdlib_linalg_s_constrained_lstsq_space(A, C, lwork, err)
        !> Least-squares cost.
        real(sp), intent(in) :: A(:, :)
        !> Equality constraints.
        real(sp), intent(in) :: C(:, :)
        !> Size of the workspace array.
        integer(ilp), intent(out)  :: lwork
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err
        !> Local variables.
        integer(ilp) :: m, n, p, info
        real(sp) :: a_dummy(1, 1), b_dummy(1)
        real(sp) :: c_dummy(1, 1), d_dummy(1)
        real(sp) :: work(1), x(1)
        type(linalg_state_type) :: err0
        !> Problem dimensions.
        m = size(A, 1) ; n = size(A, 2) ; p = size(C, 1)
        lwork = -1_ilp
        !> Workspace query.
        call gglse(m, n, p, a_dummy, m, c_dummy, p, b_dummy, d_dummy, x, work, lwork, info)
        call handle_gglse_info(this, info, m, n, p, err0)
        !> Optimal workspace size.
        lwork = ceiling(real(work(1), kind=sp), kind=ilp)

        call linalg_error_handling(err0, err)
    end subroutine stdlib_linalg_s_constrained_lstsq_space

    ! Constrained least-squares solver.
    module subroutine stdlib_linalg_s_solve_constrained_lstsq(A, b, C, d, x, storage, overwrite_matrices, err)
        !!  ### Summary
        !!  Compute the solution of the equality constrained least-squares problem
        !!
        !!      minimize     || Ax - b ||²
        !!      subject to      Cx = d
        !!
        !!  ### Description
        !!
        !!  This function computes the solution of an equality constrained linear least-squares
        !!  problem.
        !!
        !!  param: a Input matrix of size [m, n] (with m > n).
        !!  param: b Right-hand side vector of size [m] in the least-squares cost.
        !!  param: c Input matrix of size [p, n] (with p < n) defining the equality constraints.
        !!  param: d Right-hand side vector of size [p] in the equality constraints.
        !!  param: x Vector of size [n] solution to the problem.
        !!  param: storage [optional] Working array.
        !!  param: overwrite_matrices [optional] Boolean flag indicating whether the matrices
        !!                                       and vectors can be overwritten.
        !!  param: err [optional] State return flag.
        !!
        !> Least-squares cost.
        real(sp), intent(inout), target :: A(:, :), b(:)
        !> Equality constraints.
        real(sp), intent(inout), target :: C(:, :), d(:)
        !> Solution vector.
        real(sp), intent(out) :: x(:)
        !> [optional] Storage.
        real(sp), optional, intent(out), target :: storage(:)
        !> [optional] Can A, b, C, and d be overwritten?
        logical(lk), optional, intent(in) :: overwrite_matrices
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err

        ! Local variables.
        type(linalg_state_type) :: err0
        integer(ilp) :: ma, na, mb
        integer(ilp) :: mc, nc, md
        integer(ilp) :: mx
        logical(lk) :: overwrite_matrices_
        real(sp), pointer :: amat(:, :), bvec(:)
        real(sp), pointer :: cmat(:, :), dvec(:)
        ! LAPACK related.
        integer(ilp) :: lwork, info
        real(sp), pointer :: work(:)

        !> Check dimensions.
        ma = size(A, 1, kind=ilp) ; na = size(A, 2, kind=ilp)
        mc = size(C, 1, kind=ilp) ; nc = size(C, 2, kind=ilp)
        mb = size(b, kind=ilp) ; md = size(d, kind=ilp) ; mx = size(x, kind=ilp)
        call check_problem_size(ma, na, mb, mc, nc, md, mx, err0)
        if (err0%error()) then
            call linalg_error_handling(err0, err)
            return
        endif

        !> Check if matrices can be overwritten.
        overwrite_matrices_ = optval(overwrite_matrices, .false._lk)

        !> Allocate matrices.
        if (overwrite_matrices_) then
            amat => a
            bvec => b
            cmat => c
            dvec => d
        else
            allocate(amat(ma, na), source=a)
            allocate(bvec(mb), source=b)
            allocate(cmat(mc, nc), source=c)
            allocate(dvec(md), source=d)
        endif

        !> Retrieve workspace size.
        call stdlib_linalg_s_constrained_lstsq_space(A, C, lwork, err0)

        if (err0%ok()) then
            !> Workspace.
            if (present(storage)) then
                work => storage
            else
                allocate(work(lwork))
            endif
            if (size(work, kind=ilp) < lwork) then
                err0 = linalg_state_type(this, LINALG_ERROR, 'Insufficient workspace. Should be at least ', lwork)
                call linalg_error_handling(err0, err)
                return
            endif

            !> Compute constrained lstsq solution.
            call gglse(ma, na, mc, amat, ma, cmat, mc, bvec, dvec, x, work, lwork, info)
            call handle_gglse_info(this, info, ma, na, mc, err0)

            !> Deallocate.
            if (.not. present(storage)) deallocate(work)
        endif

        if (.not. overwrite_matrices_) then
            deallocate(amat, bvec, cmat, dvec)
        endif

        call linalg_error_handling(err0, err)

    end subroutine stdlib_linalg_s_solve_constrained_lstsq

    module function stdlib_linalg_s_constrained_lstsq(A, b, C, d, overwrite_matrices, err) result(x)
        !!  ### Summary
        !!  Compute the solution of the equality constrained least-squares problem
        !!
        !!      minimize     || Ax - b ||²
        !!      subject to      Cx = d
        !!
        !!  ### Description
        !!
        !!  This function computes the solution of an equality constrained linear least-squares
        !!  problem.
        !!
        !!  param: a Input matrix of size [m, n] (with m > n).
        !!  param: b Right-hand side vector of size [m] in the least-squares cost.
        !!  param: c Input matrix of size [p, n] (with p < n) defining the equality constraints.
        !!  param: d Right-hand side vector of size [p] in the equality constraints.
        !!  param: x Vector of size [n] solution to the problem.
        !!  param: overwrite_matrices [optional] Boolean flag indicating whether the matrices
        !!                                       and vectors can be overwritten.
        !!  param: err [optional] State return flag.
        !!
        !> Least-squares cost.
        real(sp), intent(inout), target :: A(:, :), b(:)
        !> Equality constraints.
        real(sp), intent(inout), target :: C(:, :), d(:)
        !> [optional] Can A, b, C, d be overwritten?
        logical(lk), optional, intent(in) :: overwrite_matrices
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err
        !> Solution of the constrained least-squares problem.
        real(sp), allocatable, target :: x(:)

        ! Local variables.
        integer(ilp) :: n

        n = size(A, 2, kind=ilp)
        allocate(x(n))
        call stdlib_linalg_s_solve_constrained_lstsq(A, b, C, d, x, overwrite_matrices=overwrite_matrices, err=err)
    end function stdlib_linalg_s_constrained_lstsq
    ! Compute the size of the workspace requested by the constrained least-squares procedure.
    module subroutine stdlib_linalg_d_constrained_lstsq_space(A, C, lwork, err)
        !> Least-squares cost.
        real(dp), intent(in) :: A(:, :)
        !> Equality constraints.
        real(dp), intent(in) :: C(:, :)
        !> Size of the workspace array.
        integer(ilp), intent(out)  :: lwork
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err
        !> Local variables.
        integer(ilp) :: m, n, p, info
        real(dp) :: a_dummy(1, 1), b_dummy(1)
        real(dp) :: c_dummy(1, 1), d_dummy(1)
        real(dp) :: work(1), x(1)
        type(linalg_state_type) :: err0
        !> Problem dimensions.
        m = size(A, 1) ; n = size(A, 2) ; p = size(C, 1)
        lwork = -1_ilp
        !> Workspace query.
        call gglse(m, n, p, a_dummy, m, c_dummy, p, b_dummy, d_dummy, x, work, lwork, info)
        call handle_gglse_info(this, info, m, n, p, err0)
        !> Optimal workspace size.
        lwork = ceiling(real(work(1), kind=dp), kind=ilp)

        call linalg_error_handling(err0, err)
    end subroutine stdlib_linalg_d_constrained_lstsq_space

    ! Constrained least-squares solver.
    module subroutine stdlib_linalg_d_solve_constrained_lstsq(A, b, C, d, x, storage, overwrite_matrices, err)
        !!  ### Summary
        !!  Compute the solution of the equality constrained least-squares problem
        !!
        !!      minimize     || Ax - b ||²
        !!      subject to      Cx = d
        !!
        !!  ### Description
        !!
        !!  This function computes the solution of an equality constrained linear least-squares
        !!  problem.
        !!
        !!  param: a Input matrix of size [m, n] (with m > n).
        !!  param: b Right-hand side vector of size [m] in the least-squares cost.
        !!  param: c Input matrix of size [p, n] (with p < n) defining the equality constraints.
        !!  param: d Right-hand side vector of size [p] in the equality constraints.
        !!  param: x Vector of size [n] solution to the problem.
        !!  param: storage [optional] Working array.
        !!  param: overwrite_matrices [optional] Boolean flag indicating whether the matrices
        !!                                       and vectors can be overwritten.
        !!  param: err [optional] State return flag.
        !!
        !> Least-squares cost.
        real(dp), intent(inout), target :: A(:, :), b(:)
        !> Equality constraints.
        real(dp), intent(inout), target :: C(:, :), d(:)
        !> Solution vector.
        real(dp), intent(out) :: x(:)
        !> [optional] Storage.
        real(dp), optional, intent(out), target :: storage(:)
        !> [optional] Can A, b, C, and d be overwritten?
        logical(lk), optional, intent(in) :: overwrite_matrices
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err

        ! Local variables.
        type(linalg_state_type) :: err0
        integer(ilp) :: ma, na, mb
        integer(ilp) :: mc, nc, md
        integer(ilp) :: mx
        logical(lk) :: overwrite_matrices_
        real(dp), pointer :: amat(:, :), bvec(:)
        real(dp), pointer :: cmat(:, :), dvec(:)
        ! LAPACK related.
        integer(ilp) :: lwork, info
        real(dp), pointer :: work(:)

        !> Check dimensions.
        ma = size(A, 1, kind=ilp) ; na = size(A, 2, kind=ilp)
        mc = size(C, 1, kind=ilp) ; nc = size(C, 2, kind=ilp)
        mb = size(b, kind=ilp) ; md = size(d, kind=ilp) ; mx = size(x, kind=ilp)
        call check_problem_size(ma, na, mb, mc, nc, md, mx, err0)
        if (err0%error()) then
            call linalg_error_handling(err0, err)
            return
        endif

        !> Check if matrices can be overwritten.
        overwrite_matrices_ = optval(overwrite_matrices, .false._lk)

        !> Allocate matrices.
        if (overwrite_matrices_) then
            amat => a
            bvec => b
            cmat => c
            dvec => d
        else
            allocate(amat(ma, na), source=a)
            allocate(bvec(mb), source=b)
            allocate(cmat(mc, nc), source=c)
            allocate(dvec(md), source=d)
        endif

        !> Retrieve workspace size.
        call stdlib_linalg_d_constrained_lstsq_space(A, C, lwork, err0)

        if (err0%ok()) then
            !> Workspace.
            if (present(storage)) then
                work => storage
            else
                allocate(work(lwork))
            endif
            if (size(work, kind=ilp) < lwork) then
                err0 = linalg_state_type(this, LINALG_ERROR, 'Insufficient workspace. Should be at least ', lwork)
                call linalg_error_handling(err0, err)
                return
            endif

            !> Compute constrained lstsq solution.
            call gglse(ma, na, mc, amat, ma, cmat, mc, bvec, dvec, x, work, lwork, info)
            call handle_gglse_info(this, info, ma, na, mc, err0)

            !> Deallocate.
            if (.not. present(storage)) deallocate(work)
        endif

        if (.not. overwrite_matrices_) then
            deallocate(amat, bvec, cmat, dvec)
        endif

        call linalg_error_handling(err0, err)

    end subroutine stdlib_linalg_d_solve_constrained_lstsq

    module function stdlib_linalg_d_constrained_lstsq(A, b, C, d, overwrite_matrices, err) result(x)
        !!  ### Summary
        !!  Compute the solution of the equality constrained least-squares problem
        !!
        !!      minimize     || Ax - b ||²
        !!      subject to      Cx = d
        !!
        !!  ### Description
        !!
        !!  This function computes the solution of an equality constrained linear least-squares
        !!  problem.
        !!
        !!  param: a Input matrix of size [m, n] (with m > n).
        !!  param: b Right-hand side vector of size [m] in the least-squares cost.
        !!  param: c Input matrix of size [p, n] (with p < n) defining the equality constraints.
        !!  param: d Right-hand side vector of size [p] in the equality constraints.
        !!  param: x Vector of size [n] solution to the problem.
        !!  param: overwrite_matrices [optional] Boolean flag indicating whether the matrices
        !!                                       and vectors can be overwritten.
        !!  param: err [optional] State return flag.
        !!
        !> Least-squares cost.
        real(dp), intent(inout), target :: A(:, :), b(:)
        !> Equality constraints.
        real(dp), intent(inout), target :: C(:, :), d(:)
        !> [optional] Can A, b, C, d be overwritten?
        logical(lk), optional, intent(in) :: overwrite_matrices
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err
        !> Solution of the constrained least-squares problem.
        real(dp), allocatable, target :: x(:)

        ! Local variables.
        integer(ilp) :: n

        n = size(A, 2, kind=ilp)
        allocate(x(n))
        call stdlib_linalg_d_solve_constrained_lstsq(A, b, C, d, x, overwrite_matrices=overwrite_matrices, err=err)
    end function stdlib_linalg_d_constrained_lstsq
    ! Compute the size of the workspace requested by the constrained least-squares procedure.
    module subroutine stdlib_linalg_c_constrained_lstsq_space(A, C, lwork, err)
        !> Least-squares cost.
        complex(sp), intent(in) :: A(:, :)
        !> Equality constraints.
        complex(sp), intent(in) :: C(:, :)
        !> Size of the workspace array.
        integer(ilp), intent(out)  :: lwork
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err
        !> Local variables.
        integer(ilp) :: m, n, p, info
        complex(sp) :: a_dummy(1, 1), b_dummy(1)
        complex(sp) :: c_dummy(1, 1), d_dummy(1)
        complex(sp) :: work(1), x(1)
        type(linalg_state_type) :: err0
        !> Problem dimensions.
        m = size(A, 1) ; n = size(A, 2) ; p = size(C, 1)
        lwork = -1_ilp
        !> Workspace query.
        call gglse(m, n, p, a_dummy, m, c_dummy, p, b_dummy, d_dummy, x, work, lwork, info)
        call handle_gglse_info(this, info, m, n, p, err0)
        !> Optimal workspace size.
        lwork = ceiling(real(work(1), kind=sp), kind=ilp)

        call linalg_error_handling(err0, err)
    end subroutine stdlib_linalg_c_constrained_lstsq_space

    ! Constrained least-squares solver.
    module subroutine stdlib_linalg_c_solve_constrained_lstsq(A, b, C, d, x, storage, overwrite_matrices, err)
        !!  ### Summary
        !!  Compute the solution of the equality constrained least-squares problem
        !!
        !!      minimize     || Ax - b ||²
        !!      subject to      Cx = d
        !!
        !!  ### Description
        !!
        !!  This function computes the solution of an equality constrained linear least-squares
        !!  problem.
        !!
        !!  param: a Input matrix of size [m, n] (with m > n).
        !!  param: b Right-hand side vector of size [m] in the least-squares cost.
        !!  param: c Input matrix of size [p, n] (with p < n) defining the equality constraints.
        !!  param: d Right-hand side vector of size [p] in the equality constraints.
        !!  param: x Vector of size [n] solution to the problem.
        !!  param: storage [optional] Working array.
        !!  param: overwrite_matrices [optional] Boolean flag indicating whether the matrices
        !!                                       and vectors can be overwritten.
        !!  param: err [optional] State return flag.
        !!
        !> Least-squares cost.
        complex(sp), intent(inout), target :: A(:, :), b(:)
        !> Equality constraints.
        complex(sp), intent(inout), target :: C(:, :), d(:)
        !> Solution vector.
        complex(sp), intent(out) :: x(:)
        !> [optional] Storage.
        complex(sp), optional, intent(out), target :: storage(:)
        !> [optional] Can A, b, C, and d be overwritten?
        logical(lk), optional, intent(in) :: overwrite_matrices
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err

        ! Local variables.
        type(linalg_state_type) :: err0
        integer(ilp) :: ma, na, mb
        integer(ilp) :: mc, nc, md
        integer(ilp) :: mx
        logical(lk) :: overwrite_matrices_
        complex(sp), pointer :: amat(:, :), bvec(:)
        complex(sp), pointer :: cmat(:, :), dvec(:)
        ! LAPACK related.
        integer(ilp) :: lwork, info
        complex(sp), pointer :: work(:)

        !> Check dimensions.
        ma = size(A, 1, kind=ilp) ; na = size(A, 2, kind=ilp)
        mc = size(C, 1, kind=ilp) ; nc = size(C, 2, kind=ilp)
        mb = size(b, kind=ilp) ; md = size(d, kind=ilp) ; mx = size(x, kind=ilp)
        call check_problem_size(ma, na, mb, mc, nc, md, mx, err0)
        if (err0%error()) then
            call linalg_error_handling(err0, err)
            return
        endif

        !> Check if matrices can be overwritten.
        overwrite_matrices_ = optval(overwrite_matrices, .false._lk)

        !> Allocate matrices.
        if (overwrite_matrices_) then
            amat => a
            bvec => b
            cmat => c
            dvec => d
        else
            allocate(amat(ma, na), source=a)
            allocate(bvec(mb), source=b)
            allocate(cmat(mc, nc), source=c)
            allocate(dvec(md), source=d)
        endif

        !> Retrieve workspace size.
        call stdlib_linalg_c_constrained_lstsq_space(A, C, lwork, err0)

        if (err0%ok()) then
            !> Workspace.
            if (present(storage)) then
                work => storage
            else
                allocate(work(lwork))
            endif
            if (size(work, kind=ilp) < lwork) then
                err0 = linalg_state_type(this, LINALG_ERROR, 'Insufficient workspace. Should be at least ', lwork)
                call linalg_error_handling(err0, err)
                return
            endif

            !> Compute constrained lstsq solution.
            call gglse(ma, na, mc, amat, ma, cmat, mc, bvec, dvec, x, work, lwork, info)
            call handle_gglse_info(this, info, ma, na, mc, err0)

            !> Deallocate.
            if (.not. present(storage)) deallocate(work)
        endif

        if (.not. overwrite_matrices_) then
            deallocate(amat, bvec, cmat, dvec)
        endif

        call linalg_error_handling(err0, err)

    end subroutine stdlib_linalg_c_solve_constrained_lstsq

    module function stdlib_linalg_c_constrained_lstsq(A, b, C, d, overwrite_matrices, err) result(x)
        !!  ### Summary
        !!  Compute the solution of the equality constrained least-squares problem
        !!
        !!      minimize     || Ax - b ||²
        !!      subject to      Cx = d
        !!
        !!  ### Description
        !!
        !!  This function computes the solution of an equality constrained linear least-squares
        !!  problem.
        !!
        !!  param: a Input matrix of size [m, n] (with m > n).
        !!  param: b Right-hand side vector of size [m] in the least-squares cost.
        !!  param: c Input matrix of size [p, n] (with p < n) defining the equality constraints.
        !!  param: d Right-hand side vector of size [p] in the equality constraints.
        !!  param: x Vector of size [n] solution to the problem.
        !!  param: overwrite_matrices [optional] Boolean flag indicating whether the matrices
        !!                                       and vectors can be overwritten.
        !!  param: err [optional] State return flag.
        !!
        !> Least-squares cost.
        complex(sp), intent(inout), target :: A(:, :), b(:)
        !> Equality constraints.
        complex(sp), intent(inout), target :: C(:, :), d(:)
        !> [optional] Can A, b, C, d be overwritten?
        logical(lk), optional, intent(in) :: overwrite_matrices
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err
        !> Solution of the constrained least-squares problem.
        complex(sp), allocatable, target :: x(:)

        ! Local variables.
        integer(ilp) :: n

        n = size(A, 2, kind=ilp)
        allocate(x(n))
        call stdlib_linalg_c_solve_constrained_lstsq(A, b, C, d, x, overwrite_matrices=overwrite_matrices, err=err)
    end function stdlib_linalg_c_constrained_lstsq
    ! Compute the size of the workspace requested by the constrained least-squares procedure.
    module subroutine stdlib_linalg_z_constrained_lstsq_space(A, C, lwork, err)
        !> Least-squares cost.
        complex(dp), intent(in) :: A(:, :)
        !> Equality constraints.
        complex(dp), intent(in) :: C(:, :)
        !> Size of the workspace array.
        integer(ilp), intent(out)  :: lwork
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err
        !> Local variables.
        integer(ilp) :: m, n, p, info
        complex(dp) :: a_dummy(1, 1), b_dummy(1)
        complex(dp) :: c_dummy(1, 1), d_dummy(1)
        complex(dp) :: work(1), x(1)
        type(linalg_state_type) :: err0
        !> Problem dimensions.
        m = size(A, 1) ; n = size(A, 2) ; p = size(C, 1)
        lwork = -1_ilp
        !> Workspace query.
        call gglse(m, n, p, a_dummy, m, c_dummy, p, b_dummy, d_dummy, x, work, lwork, info)
        call handle_gglse_info(this, info, m, n, p, err0)
        !> Optimal workspace size.
        lwork = ceiling(real(work(1), kind=dp), kind=ilp)

        call linalg_error_handling(err0, err)
    end subroutine stdlib_linalg_z_constrained_lstsq_space

    ! Constrained least-squares solver.
    module subroutine stdlib_linalg_z_solve_constrained_lstsq(A, b, C, d, x, storage, overwrite_matrices, err)
        !!  ### Summary
        !!  Compute the solution of the equality constrained least-squares problem
        !!
        !!      minimize     || Ax - b ||²
        !!      subject to      Cx = d
        !!
        !!  ### Description
        !!
        !!  This function computes the solution of an equality constrained linear least-squares
        !!  problem.
        !!
        !!  param: a Input matrix of size [m, n] (with m > n).
        !!  param: b Right-hand side vector of size [m] in the least-squares cost.
        !!  param: c Input matrix of size [p, n] (with p < n) defining the equality constraints.
        !!  param: d Right-hand side vector of size [p] in the equality constraints.
        !!  param: x Vector of size [n] solution to the problem.
        !!  param: storage [optional] Working array.
        !!  param: overwrite_matrices [optional] Boolean flag indicating whether the matrices
        !!                                       and vectors can be overwritten.
        !!  param: err [optional] State return flag.
        !!
        !> Least-squares cost.
        complex(dp), intent(inout), target :: A(:, :), b(:)
        !> Equality constraints.
        complex(dp), intent(inout), target :: C(:, :), d(:)
        !> Solution vector.
        complex(dp), intent(out) :: x(:)
        !> [optional] Storage.
        complex(dp), optional, intent(out), target :: storage(:)
        !> [optional] Can A, b, C, and d be overwritten?
        logical(lk), optional, intent(in) :: overwrite_matrices
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err

        ! Local variables.
        type(linalg_state_type) :: err0
        integer(ilp) :: ma, na, mb
        integer(ilp) :: mc, nc, md
        integer(ilp) :: mx
        logical(lk) :: overwrite_matrices_
        complex(dp), pointer :: amat(:, :), bvec(:)
        complex(dp), pointer :: cmat(:, :), dvec(:)
        ! LAPACK related.
        integer(ilp) :: lwork, info
        complex(dp), pointer :: work(:)

        !> Check dimensions.
        ma = size(A, 1, kind=ilp) ; na = size(A, 2, kind=ilp)
        mc = size(C, 1, kind=ilp) ; nc = size(C, 2, kind=ilp)
        mb = size(b, kind=ilp) ; md = size(d, kind=ilp) ; mx = size(x, kind=ilp)
        call check_problem_size(ma, na, mb, mc, nc, md, mx, err0)
        if (err0%error()) then
            call linalg_error_handling(err0, err)
            return
        endif

        !> Check if matrices can be overwritten.
        overwrite_matrices_ = optval(overwrite_matrices, .false._lk)

        !> Allocate matrices.
        if (overwrite_matrices_) then
            amat => a
            bvec => b
            cmat => c
            dvec => d
        else
            allocate(amat(ma, na), source=a)
            allocate(bvec(mb), source=b)
            allocate(cmat(mc, nc), source=c)
            allocate(dvec(md), source=d)
        endif

        !> Retrieve workspace size.
        call stdlib_linalg_z_constrained_lstsq_space(A, C, lwork, err0)

        if (err0%ok()) then
            !> Workspace.
            if (present(storage)) then
                work => storage
            else
                allocate(work(lwork))
            endif
            if (size(work, kind=ilp) < lwork) then
                err0 = linalg_state_type(this, LINALG_ERROR, 'Insufficient workspace. Should be at least ', lwork)
                call linalg_error_handling(err0, err)
                return
            endif

            !> Compute constrained lstsq solution.
            call gglse(ma, na, mc, amat, ma, cmat, mc, bvec, dvec, x, work, lwork, info)
            call handle_gglse_info(this, info, ma, na, mc, err0)

            !> Deallocate.
            if (.not. present(storage)) deallocate(work)
        endif

        if (.not. overwrite_matrices_) then
            deallocate(amat, bvec, cmat, dvec)
        endif

        call linalg_error_handling(err0, err)

    end subroutine stdlib_linalg_z_solve_constrained_lstsq

    module function stdlib_linalg_z_constrained_lstsq(A, b, C, d, overwrite_matrices, err) result(x)
        !!  ### Summary
        !!  Compute the solution of the equality constrained least-squares problem
        !!
        !!      minimize     || Ax - b ||²
        !!      subject to      Cx = d
        !!
        !!  ### Description
        !!
        !!  This function computes the solution of an equality constrained linear least-squares
        !!  problem.
        !!
        !!  param: a Input matrix of size [m, n] (with m > n).
        !!  param: b Right-hand side vector of size [m] in the least-squares cost.
        !!  param: c Input matrix of size [p, n] (with p < n) defining the equality constraints.
        !!  param: d Right-hand side vector of size [p] in the equality constraints.
        !!  param: x Vector of size [n] solution to the problem.
        !!  param: overwrite_matrices [optional] Boolean flag indicating whether the matrices
        !!                                       and vectors can be overwritten.
        !!  param: err [optional] State return flag.
        !!
        !> Least-squares cost.
        complex(dp), intent(inout), target :: A(:, :), b(:)
        !> Equality constraints.
        complex(dp), intent(inout), target :: C(:, :), d(:)
        !> [optional] Can A, b, C, d be overwritten?
        logical(lk), optional, intent(in) :: overwrite_matrices
        !> [optional] State return flag.
        type(linalg_state_type), optional, intent(out) :: err
        !> Solution of the constrained least-squares problem.
        complex(dp), allocatable, target :: x(:)

        ! Local variables.
        integer(ilp) :: n

        n = size(A, 2, kind=ilp)
        allocate(x(n))
        call stdlib_linalg_z_solve_constrained_lstsq(A, b, C, d, x, overwrite_matrices=overwrite_matrices, err=err)
    end function stdlib_linalg_z_constrained_lstsq

end submodule stdlib_linalg_least_squares
