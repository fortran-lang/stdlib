submodule(stdlib_linalg) stdlib_linalg_svd
!! Singular-Value Decomposition    
     use stdlib_linalg_constants
     use stdlib_linalg_lapack, only: gesdd
     use stdlib_linalg_lapack_aux, only: handle_gesdd_info
     use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
         LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR, LINALG_SUCCESS
     implicit none
     
     character(*), parameter :: this = 'svd'
     
     !> List of internal GESDD tasks: 

     !> Return full matrices U, V^T to separate storage
     character, parameter :: GESDD_FULL_MATRICES   = 'A'

     !> Return shrunk matrices U, V^T to k = min(m,n)
     character, parameter :: GESDD_SHRINK_MATRICES = 'S'

     !> Overwrite A storage with U (if M>=N) or VT (if M<N); separate storage for the other matrix
     character, parameter :: GESDD_OVERWRITE_A     = 'O'

     !> Do not return either U or VT (singular values array only)
     character, parameter :: GESDD_SINGVAL_ONLY    = 'N'

     contains


     !> Singular values of matrix A
     module function stdlib_linalg_svdvals_s(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!                
         !> Input matrix A[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(sp), allocatable :: s(:)

         !> Create
         real(sp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(s(k))

         !> Compute singular values
         call stdlib_linalg_svd_s(amat,s,overwrite_a=.false.,err=err)

     end function stdlib_linalg_svdvals_s

     !> SVD of matrix A = U S V^T, returning S and optionally U and V^T
     module subroutine stdlib_linalg_svd_s(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!        
         !> Input matrix A[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(sp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors
         real(sp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         real(sp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldu,ldvt,info,k,lwork,liwork,lrwork
         integer(ilp), allocatable :: iwork(:)
         logical(lk) :: overwrite_a_,full_storage,compute_uv,temp_u,temp_vt,can_overwrite_amat
         character :: task
         real(sp), target :: work_dummy(1),u_dummy(1,1),vt_dummy(1,1)
         real(sp), allocatable :: work(:)
         real(sp), pointer :: amat(:,:),umat(:,:),vtmat(:,:)

         !> Matrix determinant size
         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)
         lda = m

         if (.not.k>0) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or matrix size: a=',[m,n])
            call linalg_error_handling(err0,err)
            return
         elseif (.not.size(s,kind=ilp)>=k) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'singular value array has insufficient size:',&
                                                        ' s=',shape(s,kind=ilp),', k=',k)
            call linalg_error_handling(err0,err)
            return
         endif

         ! Integer storage
         liwork = 8*k
         allocate(iwork(liwork))

         ! Can A be overwritten? By default, do not overwrite
         overwrite_a_ = .false.
         if (present(overwrite_a)) overwrite_a_ = overwrite_a

         ! Initialize a matrix temporary?
         if (overwrite_a_) then
            amat => a
         else
            allocate(amat(m,n),source=a)
         endif
         
         ! Check if we can overwrite amat with data that will be lost 
         can_overwrite_amat = (.not.overwrite_a_) .and. merge(.not.present(u),.not.present(vt),m>=n)         
            
         ! Full-size matrices
         if (present(full_matrices)) then
            full_storage = full_matrices
         else
            full_storage = .true.
         endif

         ! Decide if U, VT matrices should be computed
         compute_uv = present(u) .or. present(vt)

         ! U, VT storage
         if (present(u)) then
            ! User input
            umat   => u
            temp_u = .false.
         elseif ((m>=n .and. .not.overwrite_a_) .or. .not.compute_uv) then
            ! U not wanted, and A can be overwritten: do not allocate
            umat   => u_dummy
            temp_u = .false.
         elseif (.not.full_storage) then
            ! Allocate with minimum size
            allocate(umat(m,k))
            temp_u = .true.
         else
            ! Allocate with regular size
            allocate(umat(m,m))
            temp_u = .true.
         end if

         if (present(vt)) then
            ! User input
            vtmat   => vt
            temp_vt = .false.
         elseif ((m<n .and. .not.overwrite_a_) .or. .not.compute_uv) then
            ! amat can be overwritten, VT not wanted: VT is returned upon A
            vtmat   => vt_dummy
            temp_vt = .false.
         elseif (.not.full_storage) then
            ! Allocate with minimum size
            allocate(vtmat(k,n))
            temp_vt = .true.
         else
            ! Allocate with regular size
            allocate(vtmat(n,n))
            temp_vt = .true.
         end if

         ldu  = size(umat ,1,kind=ilp)
         ldvt = size(vtmat,1,kind=ilp)

         ! Decide SVD task
         if (.not.compute_uv) then
            task = GESDD_SINGVAL_ONLY
         elseif (can_overwrite_amat) then
            ! A is a copy: we can overwrite its storage
            task = GESDD_OVERWRITE_A
         elseif (.not.full_storage) then
            task = GESDD_SHRINK_MATRICES
         else
            task = GESDD_FULL_MATRICES
         end if

         ! Compute workspace
         lrwork = -1_ilp ! not needed

         ! First call: request working storage space
         lwork = -1_ilp
         call gesdd(task,m,n,amat,lda,s,umat,ldu,vtmat,ldvt,&
                    work_dummy,lwork,iwork,info)
         call handle_gesdd_info(this,err0,info,m,n)

         ! Compute SVD
         if (info==0) then

            !> Prepare working storage
            ! Check if the returned working storage space is smaller than the largest value
            ! allowed by lwork
            lwork = merge(nint(real(work_dummy(1),kind=sp), kind=ilp) &
                          , huge(lwork) &
                          , real(work_dummy(1),kind=sp) < real(huge(lwork),kind=sp) )
            allocate(work(lwork))

            !> Compute SVD
            call gesdd(task,m,n,amat,lda,s,umat,ldu,vtmat,ldvt,&
                       work,lwork,iwork,info)
            call handle_gesdd_info(this,err0,info,m,n)

         endif

         ! Finalize storage and process output flag
         if (.not.overwrite_a_) deallocate(amat)
         if (temp_u)            deallocate(umat)
         if (temp_vt)           deallocate(vtmat)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_svd_s

     !> Singular values of matrix A
     module function stdlib_linalg_svdvals_d(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!                
         !> Input matrix A[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(dp), allocatable :: s(:)

         !> Create
         real(dp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(s(k))

         !> Compute singular values
         call stdlib_linalg_svd_d(amat,s,overwrite_a=.false.,err=err)

     end function stdlib_linalg_svdvals_d

     !> SVD of matrix A = U S V^T, returning S and optionally U and V^T
     module subroutine stdlib_linalg_svd_d(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!        
         !> Input matrix A[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(dp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors
         real(dp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         real(dp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldu,ldvt,info,k,lwork,liwork,lrwork
         integer(ilp), allocatable :: iwork(:)
         logical(lk) :: overwrite_a_,full_storage,compute_uv,temp_u,temp_vt,can_overwrite_amat
         character :: task
         real(dp), target :: work_dummy(1),u_dummy(1,1),vt_dummy(1,1)
         real(dp), allocatable :: work(:)
         real(dp), pointer :: amat(:,:),umat(:,:),vtmat(:,:)

         !> Matrix determinant size
         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)
         lda = m

         if (.not.k>0) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or matrix size: a=',[m,n])
            call linalg_error_handling(err0,err)
            return
         elseif (.not.size(s,kind=ilp)>=k) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'singular value array has insufficient size:',&
                                                        ' s=',shape(s,kind=ilp),', k=',k)
            call linalg_error_handling(err0,err)
            return
         endif

         ! Integer storage
         liwork = 8*k
         allocate(iwork(liwork))

         ! Can A be overwritten? By default, do not overwrite
         overwrite_a_ = .false.
         if (present(overwrite_a)) overwrite_a_ = overwrite_a

         ! Initialize a matrix temporary?
         if (overwrite_a_) then
            amat => a
         else
            allocate(amat(m,n),source=a)
         endif
         
         ! Check if we can overwrite amat with data that will be lost 
         can_overwrite_amat = (.not.overwrite_a_) .and. merge(.not.present(u),.not.present(vt),m>=n)         
            
         ! Full-size matrices
         if (present(full_matrices)) then
            full_storage = full_matrices
         else
            full_storage = .true.
         endif

         ! Decide if U, VT matrices should be computed
         compute_uv = present(u) .or. present(vt)

         ! U, VT storage
         if (present(u)) then
            ! User input
            umat   => u
            temp_u = .false.
         elseif ((m>=n .and. .not.overwrite_a_) .or. .not.compute_uv) then
            ! U not wanted, and A can be overwritten: do not allocate
            umat   => u_dummy
            temp_u = .false.
         elseif (.not.full_storage) then
            ! Allocate with minimum size
            allocate(umat(m,k))
            temp_u = .true.
         else
            ! Allocate with regular size
            allocate(umat(m,m))
            temp_u = .true.
         end if

         if (present(vt)) then
            ! User input
            vtmat   => vt
            temp_vt = .false.
         elseif ((m<n .and. .not.overwrite_a_) .or. .not.compute_uv) then
            ! amat can be overwritten, VT not wanted: VT is returned upon A
            vtmat   => vt_dummy
            temp_vt = .false.
         elseif (.not.full_storage) then
            ! Allocate with minimum size
            allocate(vtmat(k,n))
            temp_vt = .true.
         else
            ! Allocate with regular size
            allocate(vtmat(n,n))
            temp_vt = .true.
         end if

         ldu  = size(umat ,1,kind=ilp)
         ldvt = size(vtmat,1,kind=ilp)

         ! Decide SVD task
         if (.not.compute_uv) then
            task = GESDD_SINGVAL_ONLY
         elseif (can_overwrite_amat) then
            ! A is a copy: we can overwrite its storage
            task = GESDD_OVERWRITE_A
         elseif (.not.full_storage) then
            task = GESDD_SHRINK_MATRICES
         else
            task = GESDD_FULL_MATRICES
         end if

         ! Compute workspace
         lrwork = -1_ilp ! not needed

         ! First call: request working storage space
         lwork = -1_ilp
         call gesdd(task,m,n,amat,lda,s,umat,ldu,vtmat,ldvt,&
                    work_dummy,lwork,iwork,info)
         call handle_gesdd_info(this,err0,info,m,n)

         ! Compute SVD
         if (info==0) then

            !> Prepare working storage
            ! Check if the returned working storage space is smaller than the largest value
            ! allowed by lwork
            lwork = merge(nint(real(work_dummy(1),kind=dp), kind=ilp) &
                          , huge(lwork) &
                          , real(work_dummy(1),kind=dp) < real(huge(lwork),kind=dp) )
            allocate(work(lwork))

            !> Compute SVD
            call gesdd(task,m,n,amat,lda,s,umat,ldu,vtmat,ldvt,&
                       work,lwork,iwork,info)
            call handle_gesdd_info(this,err0,info,m,n)

         endif

         ! Finalize storage and process output flag
         if (.not.overwrite_a_) deallocate(amat)
         if (temp_u)            deallocate(umat)
         if (temp_vt)           deallocate(vtmat)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_svd_d

     !> Singular values of matrix A
     module function stdlib_linalg_svdvals_c(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!                
         !> Input matrix A[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(sp), allocatable :: s(:)

         !> Create
         complex(sp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(s(k))

         !> Compute singular values
         call stdlib_linalg_svd_c(amat,s,overwrite_a=.false.,err=err)

     end function stdlib_linalg_svdvals_c

     !> SVD of matrix A = U S V^T, returning S and optionally U and V^T
     module subroutine stdlib_linalg_svd_c(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!        
         !> Input matrix A[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(sp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors
         complex(sp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         complex(sp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldu,ldvt,info,k,lwork,liwork,lrwork
         integer(ilp), allocatable :: iwork(:)
         logical(lk) :: overwrite_a_,full_storage,compute_uv,temp_u,temp_vt,can_overwrite_amat
         character :: task
         complex(sp), target :: work_dummy(1),u_dummy(1,1),vt_dummy(1,1)
         complex(sp), allocatable :: work(:)
         real(sp), allocatable :: rwork(:)
         complex(sp), pointer :: amat(:,:),umat(:,:),vtmat(:,:)

         !> Matrix determinant size
         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)
         lda = m

         if (.not.k>0) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or matrix size: a=',[m,n])
            call linalg_error_handling(err0,err)
            return
         elseif (.not.size(s,kind=ilp)>=k) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'singular value array has insufficient size:',&
                                                        ' s=',shape(s,kind=ilp),', k=',k)
            call linalg_error_handling(err0,err)
            return
         endif

         ! Integer storage
         liwork = 8*k
         allocate(iwork(liwork))

         ! Can A be overwritten? By default, do not overwrite
         overwrite_a_ = .false.
         if (present(overwrite_a)) overwrite_a_ = overwrite_a

         ! Initialize a matrix temporary?
         if (overwrite_a_) then
            amat => a
         else
            allocate(amat(m,n),source=a)
         endif
         
         ! Check if we can overwrite amat with data that will be lost 
         can_overwrite_amat = (.not.overwrite_a_) .and. merge(.not.present(u),.not.present(vt),m>=n)         
            
         ! Full-size matrices
         if (present(full_matrices)) then
            full_storage = full_matrices
         else
            full_storage = .true.
         endif

         ! Decide if U, VT matrices should be computed
         compute_uv = present(u) .or. present(vt)

         ! U, VT storage
         if (present(u)) then
            ! User input
            umat   => u
            temp_u = .false.
         elseif ((m>=n .and. .not.overwrite_a_) .or. .not.compute_uv) then
            ! U not wanted, and A can be overwritten: do not allocate
            umat   => u_dummy
            temp_u = .false.
         elseif (.not.full_storage) then
            ! Allocate with minimum size
            allocate(umat(m,k))
            temp_u = .true.
         else
            ! Allocate with regular size
            allocate(umat(m,m))
            temp_u = .true.
         end if

         if (present(vt)) then
            ! User input
            vtmat   => vt
            temp_vt = .false.
         elseif ((m<n .and. .not.overwrite_a_) .or. .not.compute_uv) then
            ! amat can be overwritten, VT not wanted: VT is returned upon A
            vtmat   => vt_dummy
            temp_vt = .false.
         elseif (.not.full_storage) then
            ! Allocate with minimum size
            allocate(vtmat(k,n))
            temp_vt = .true.
         else
            ! Allocate with regular size
            allocate(vtmat(n,n))
            temp_vt = .true.
         end if

         ldu  = size(umat ,1,kind=ilp)
         ldvt = size(vtmat,1,kind=ilp)

         ! Decide SVD task
         if (.not.compute_uv) then
            task = GESDD_SINGVAL_ONLY
         elseif (can_overwrite_amat) then
            ! A is a copy: we can overwrite its storage
            task = GESDD_OVERWRITE_A
         elseif (.not.full_storage) then
            task = GESDD_SHRINK_MATRICES
         else
            task = GESDD_FULL_MATRICES
         end if

         ! Compute workspace
         if (task==GESDD_SINGVAL_ONLY) then
            lrwork = max(1,7*k)
         else
            lrwork = max(1,5*k*(k+1),2*k*(k+max(m,n))+k)
         endif
         allocate(rwork(lrwork))

         ! First call: request working storage space
         lwork = -1_ilp
         call gesdd(task,m,n,amat,lda,s,umat,ldu,vtmat,ldvt,&
                    work_dummy,lwork,rwork,iwork,info)
         call handle_gesdd_info(this,err0,info,m,n)

         ! Compute SVD
         if (info==0) then

            !> Prepare working storage
            ! Check if the returned working storage space is smaller than the largest value
            ! allowed by lwork
            lwork = merge(nint(real(work_dummy(1),kind=sp), kind=ilp) &
                          , huge(lwork) &
                          , real(work_dummy(1),kind=sp) < real(huge(lwork),kind=sp) )
            allocate(work(lwork))

            !> Compute SVD
            call gesdd(task,m,n,amat,lda,s,umat,ldu,vtmat,ldvt,&
                       work,lwork,rwork,iwork,info)
            call handle_gesdd_info(this,err0,info,m,n)

         endif

         ! Finalize storage and process output flag
         if (.not.overwrite_a_) deallocate(amat)
         if (temp_u)            deallocate(umat)
         if (temp_vt)           deallocate(vtmat)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_svd_c

     !> Singular values of matrix A
     module function stdlib_linalg_svdvals_z(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!                
         !> Input matrix A[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(dp), allocatable :: s(:)

         !> Create
         complex(dp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(s(k))

         !> Compute singular values
         call stdlib_linalg_svd_z(amat,s,overwrite_a=.false.,err=err)

     end function stdlib_linalg_svdvals_z

     !> SVD of matrix A = U S V^T, returning S and optionally U and V^T
     module subroutine stdlib_linalg_svd_z(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!        
         !> Input matrix A[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(dp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors
         complex(dp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         complex(dp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldu,ldvt,info,k,lwork,liwork,lrwork
         integer(ilp), allocatable :: iwork(:)
         logical(lk) :: overwrite_a_,full_storage,compute_uv,temp_u,temp_vt,can_overwrite_amat
         character :: task
         complex(dp), target :: work_dummy(1),u_dummy(1,1),vt_dummy(1,1)
         complex(dp), allocatable :: work(:)
         real(dp), allocatable :: rwork(:)
         complex(dp), pointer :: amat(:,:),umat(:,:),vtmat(:,:)

         !> Matrix determinant size
         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)
         lda = m

         if (.not.k>0) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or matrix size: a=',[m,n])
            call linalg_error_handling(err0,err)
            return
         elseif (.not.size(s,kind=ilp)>=k) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'singular value array has insufficient size:',&
                                                        ' s=',shape(s,kind=ilp),', k=',k)
            call linalg_error_handling(err0,err)
            return
         endif

         ! Integer storage
         liwork = 8*k
         allocate(iwork(liwork))

         ! Can A be overwritten? By default, do not overwrite
         overwrite_a_ = .false.
         if (present(overwrite_a)) overwrite_a_ = overwrite_a

         ! Initialize a matrix temporary?
         if (overwrite_a_) then
            amat => a
         else
            allocate(amat(m,n),source=a)
         endif
         
         ! Check if we can overwrite amat with data that will be lost 
         can_overwrite_amat = (.not.overwrite_a_) .and. merge(.not.present(u),.not.present(vt),m>=n)         
            
         ! Full-size matrices
         if (present(full_matrices)) then
            full_storage = full_matrices
         else
            full_storage = .true.
         endif

         ! Decide if U, VT matrices should be computed
         compute_uv = present(u) .or. present(vt)

         ! U, VT storage
         if (present(u)) then
            ! User input
            umat   => u
            temp_u = .false.
         elseif ((m>=n .and. .not.overwrite_a_) .or. .not.compute_uv) then
            ! U not wanted, and A can be overwritten: do not allocate
            umat   => u_dummy
            temp_u = .false.
         elseif (.not.full_storage) then
            ! Allocate with minimum size
            allocate(umat(m,k))
            temp_u = .true.
         else
            ! Allocate with regular size
            allocate(umat(m,m))
            temp_u = .true.
         end if

         if (present(vt)) then
            ! User input
            vtmat   => vt
            temp_vt = .false.
         elseif ((m<n .and. .not.overwrite_a_) .or. .not.compute_uv) then
            ! amat can be overwritten, VT not wanted: VT is returned upon A
            vtmat   => vt_dummy
            temp_vt = .false.
         elseif (.not.full_storage) then
            ! Allocate with minimum size
            allocate(vtmat(k,n))
            temp_vt = .true.
         else
            ! Allocate with regular size
            allocate(vtmat(n,n))
            temp_vt = .true.
         end if

         ldu  = size(umat ,1,kind=ilp)
         ldvt = size(vtmat,1,kind=ilp)

         ! Decide SVD task
         if (.not.compute_uv) then
            task = GESDD_SINGVAL_ONLY
         elseif (can_overwrite_amat) then
            ! A is a copy: we can overwrite its storage
            task = GESDD_OVERWRITE_A
         elseif (.not.full_storage) then
            task = GESDD_SHRINK_MATRICES
         else
            task = GESDD_FULL_MATRICES
         end if

         ! Compute workspace
         if (task==GESDD_SINGVAL_ONLY) then
            lrwork = max(1,7*k)
         else
            lrwork = max(1,5*k*(k+1),2*k*(k+max(m,n))+k)
         endif
         allocate(rwork(lrwork))

         ! First call: request working storage space
         lwork = -1_ilp
         call gesdd(task,m,n,amat,lda,s,umat,ldu,vtmat,ldvt,&
                    work_dummy,lwork,rwork,iwork,info)
         call handle_gesdd_info(this,err0,info,m,n)

         ! Compute SVD
         if (info==0) then

            !> Prepare working storage
            ! Check if the returned working storage space is smaller than the largest value
            ! allowed by lwork
            lwork = merge(nint(real(work_dummy(1),kind=dp), kind=ilp) &
                          , huge(lwork) &
                          , real(work_dummy(1),kind=dp) < real(huge(lwork),kind=dp) )
            allocate(work(lwork))

            !> Compute SVD
            call gesdd(task,m,n,amat,lda,s,umat,ldu,vtmat,ldvt,&
                       work,lwork,rwork,iwork,info)
            call handle_gesdd_info(this,err0,info,m,n)

         endif

         ! Finalize storage and process output flag
         if (.not.overwrite_a_) deallocate(amat)
         if (temp_u)            deallocate(umat)
         if (temp_vt)           deallocate(vtmat)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_svd_z

end submodule stdlib_linalg_svd
