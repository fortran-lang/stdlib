submodule (stdlib_linalg) stdlib_linalg_eigenvalues
!! Compute eigenvalues and eigenvectors    
     use stdlib_linalg_constants
     use stdlib_linalg_lapack, only: geev, heev, syev
     use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
          LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR, LINALG_SUCCESS     
     implicit none(type,external)
     
     character(*), parameter :: this = 'eigenvalues'

     contains
     
     !> Request for eigenvector calculation
     elemental character function eigenvectors_task(required)
        logical(lk), intent(in) :: required
        eigenvectors_task = merge('V','N',required)
     end function eigenvectors_task
     
     !> Request for symmetry side (default: lower)
     elemental character function symmetric_triangle_task(upper)
        logical(lk), optional, intent(in) :: upper
        symmetric_triangle_task = 'L'
        if (present(upper)) symmetric_triangle_task = merge('U','L',upper)
     end function symmetric_triangle_task

     !> Process GEEV output flags
     elemental subroutine handle_geev_info(err,info,m,n)
        !> Error handler
        type(linalg_state_type), intent(inout) :: err
        !> GEEV return flag
        integer(ilp), intent(in) :: info
        !> Input matrix size
        integer(ilp), intent(in) :: m,n

        select case (info)
           case (0)
               ! Success!
               err%state = LINALG_SUCCESS
           case (-1)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Invalid task ID: left eigenvectors.')
           case (-2)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Invalid task ID: right eigenvectors.')
           case (-5,-3)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',[m,n])
           case (-9)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient left vector matrix size.')
           case (-11)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient right vector matrix size.')
           case (-13)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Insufficient work array size.')
           case (1:)
               err = linalg_state_type(this,LINALG_ERROR,'Eigenvalue computation did not converge.')
           case default
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Unknown error returned by geev.')
        end select

     end subroutine handle_geev_info

     !> Process SYEV/HEEV output flags
     elemental subroutine handle_heev_info(err,info,m,n)
        !> Error handler
        type(linalg_state_type), intent(inout) :: err
        !> SYEV/HEEV return flag
        integer(ilp), intent(in) :: info
        !> Input matrix size
        integer(ilp), intent(in) :: m,n

        select case (info)
           case (0)
               ! Success!
               err%state = LINALG_SUCCESS
           case (-1)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Invalid eigenvector request.')
           case (-2)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Invalid triangular section request.')
           case (-5,-3)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',[m,n])
           case (-8)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'insufficient workspace size.')
           case (1:)
               err = linalg_state_type(this,LINALG_ERROR,'Eigenvalue computation did not converge.')
           case default
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Unknown error returned by syev/heev.')
        end select

     end subroutine handle_heev_info


     module function stdlib_linalg_eigvals_s(a,err) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of eigenvalues
         complex(sp), allocatable :: lambda(:)

         !> Create
         real(sp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eig_s(amat,lambda,overwrite_a=.false.,err=err)

     end function stdlib_linalg_eigvals_s

     module function stdlib_linalg_eigvals_noerr_s(a) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> Array of eigenvalues
         complex(sp), allocatable :: lambda(:)

         !> Create
         real(sp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eig_s(amat,lambda,overwrite_a=.false.)

     end function stdlib_linalg_eigvals_noerr_s

     module subroutine stdlib_linalg_eig_s(a,lambda,right,left,overwrite_a,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of eigenvalues, 
     !! and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         complex(sp), intent(out) :: lambda(:)
         !> [optional] RIGHT eigenvectors of A (as columns)
         complex(sp), optional, intent(out), target :: right(:,:)
         !> [optional] LEFT eigenvectors of A (as columns)
         complex(sp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldu,ldv,info,k,lwork,neig
         logical(lk) :: copy_a
         character :: task_u,task_v
         real(sp), target :: work_dummy(1),u_dummy(1,1),v_dummy(1,1)
         real(sp), allocatable :: work(:)
         real(sp), pointer :: amat(:,:),umat(:,:),vmat(:,:)
         real(sp), pointer :: lreal(:),limag(:)
         !> Matrix size
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         k    = min(m,n)
         neig = size(lambda,kind=ilp) 
         lda  = m

         if (k<=0 .or. m/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                          'invalid or matrix size a=',[m,n],', must be nonempty square.')
            call linalg_error_handling(err0,err)
            return
         elseif (neig<k) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                          'eigenvalue array has insufficient size:',&
                                          ' lambda=',neig,', n=',n)
            call linalg_error_handling(err0,err)
            return
         endif

         ! Can A be overwritten? By default, do not overwrite
         copy_a = .true._lk
         if (present(overwrite_a)) copy_a = .not.overwrite_a

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(m,n),source=a)
         else
            amat => a
         endif

         ! Decide if U, V eigenvectors
         task_u = eigenvectors_task(present(left))
         task_v = eigenvectors_task(present(right))         
         
         if (present(right)) then 
                        
            ! For a real matrix, GEEV returns real arrays. 
            ! Allocate temporary reals, will be converted to complex vectors at the end.
            allocate(vmat(n,n))
            
            if (size(vmat,2,kind=ilp)<n) then 
               err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                        'right eigenvector matrix has insufficient size: ',&
                                        shape(vmat),', with n=',n)
            endif  
            
         else
            vmat => v_dummy
         endif
            
         if (present(left)) then
            
            ! For a real matrix, GEEV returns real arrays. 
            ! Allocate temporary reals, will be converted to complex vectors at the end.
            allocate(umat(n,n))

            if (size(umat,2,kind=ilp)<n) then 
               err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                        'left eigenvector matrix has insufficient size: ',&
                                        shape(umat),', with n=',n)
            endif                
            
         else
            umat => u_dummy
         endif
         
         get_geev: if (err0%ok()) then 

             ldu = size(umat,1,kind=ilp)
             ldv = size(vmat,1,kind=ilp)

             ! Compute workspace size
             allocate(lreal(n),limag(n))

             lwork = -1_ilp
            
             call geev(task_u,task_v,n,amat,lda,&
                       lreal,limag,  &
                       umat,ldu,vmat,ldv,&
                       work_dummy,lwork,info)
             call handle_geev_info(err0,info,m,n)

             ! Compute eigenvalues
             if (info==0) then

                !> Prepare working storage
                lwork = nint(real(work_dummy(1),kind=sp), kind=ilp)
                allocate(work(lwork))

                !> Compute eigensystem
                call geev(task_u,task_v,n,amat,lda,&
                          lreal,limag,  &
                          umat,ldu,vmat,ldv,&            
                          work,lwork,info)
                call handle_geev_info(err0,info,m,n)

             endif
             
             ! Finalize storage and process output flag
             lambda(:n) = cmplx(lreal(:n),limag(:n),kind=sp) 
             
             ! If the j-th and (j+1)-st eigenvalues form a complex conjugate pair, 
             ! geev returns reals as: 
             ! u(j)   = VL(:,j) + i*VL(:,j+1) and
             ! u(j+1) = VL(:,j) - i*VL(:,j+1). 
             ! Convert these to complex numbers here.            
             if (present(right)) call assign_real_eigenvectors_sp(n,lambda,vmat,right)
             if (present(left))  call assign_real_eigenvectors_sp(n,lambda,umat,left)
         
         endif get_geev
         
         if (copy_a) deallocate(amat)
         if (present(right)) deallocate(vmat)
         if (present(left)) deallocate(umat)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_eig_s
     
     module function stdlib_linalg_eigvalsh_s(a,upper_a,err) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A
         !> Input matrix A[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of eigenvalues
         real(sp), allocatable :: lambda(:)
         
         real(sp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eigh_s(amat,lambda,upper_a=upper_a,overwrite_a=.false.,err=err)
         
     end function stdlib_linalg_eigvalsh_s
          
     module function stdlib_linalg_eigvalsh_noerr_s(a,upper_a) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A        
         !> Input matrix A[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: use lower
         logical(lk), optional, intent(in) :: upper_a         
         !> Array of eigenvalues
         real(sp), allocatable :: lambda(:)

         real(sp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eigh_s(amat,lambda,upper_a=upper_a,overwrite_a=.false.)

     end function stdlib_linalg_eigvalsh_noerr_s     

     module subroutine stdlib_linalg_eigh_s(a,lambda,vectors,upper_a,overwrite_a,err)
     !! Eigendecomposition of a real symmetric or complex Hermitian matrix A returning an array `lambda` 
     !! of eigenvalues, and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         real(sp), intent(out) :: lambda(:)
         !> The columns of vectors contain the orthonormal eigenvectors of A
         real(sp), optional, intent(out), target :: vectors(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Should the upper/lower half of A be used? Default: use lower
         logical(lk), optional, intent(in) :: upper_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,info,k,lwork,neig
         logical(lk) :: copy_a
         character :: triangle,task
         real(sp), target :: work_dummy(1)
         real(sp), allocatable :: work(:)
         real(sp), pointer :: amat(:,:)

         !> Matrix size
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         k    = min(m,n)
         neig = size(lambda,kind=ilp) 

         if (k<=0 .or. m/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or matrix size a=',[m,n], &
                                                        ', must be non-empty square.')
            call linalg_error_handling(err0,err)
            return
         elseif (neig<k) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'eigenvalue array has insufficient size:',&
                                                        ' lambda=',neig,' must be >= n=',n)
            call linalg_error_handling(err0,err)
            return
         endif
        
         ! Check if input A can be overwritten
         copy_a = .true._lk
         if (present(vectors)) then 
            ! No need to copy A anyways
            copy_a = .false.
         elseif (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         endif        
         
         ! Should we use the upper or lower half of the matrix?
         triangle = symmetric_triangle_task(upper_a)
         
         ! Request for eigenvectors
         task = eigenvectors_task(present(vectors))
         
         if (present(vectors)) then 
            
            ! Check size
            if (any(shape(vectors,kind=ilp)<n)) then 
               err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                        'eigenvector matrix has insufficient size: ',&
                                        shape(vectors),', with n=',n)
               call linalg_error_handling(err0,err)
               return
            endif            
            
            ! The input matrix will be overwritten by the vectors. 
            ! So, use this one as storage for syev/heev
            amat => vectors
            
            ! Copy data in
            amat(:n,:n) = a(:n,:n)
                        
         elseif (copy_a) then
            ! Initialize a matrix temporary
            allocate(amat(m,n),source=a)
         else
            ! Overwrite A
            amat => a
         endif


         lda = size(amat,1,kind=ilp)

         ! Request workspace size
         lwork = -1_ilp
         call syev(task,triangle,n,amat,lda,lambda,work_dummy,lwork,info)
         call handle_heev_info(err0,info,m,n)

         ! Compute eigenvalues
         if (info==0) then

            !> Prepare working storage
            lwork = nint(real(work_dummy(1),kind=sp), kind=ilp)
            allocate(work(lwork))

            !> Compute eigensystem
            call syev(task,triangle,n,amat,lda,lambda,work,lwork,info)
            call handle_heev_info(err0,info,m,n)

         endif
         
         ! Finalize storage and process output flag         
         if (copy_a) deallocate(amat)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_eigh_s
     

     module function stdlib_linalg_eigvals_d(a,err) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of eigenvalues
         complex(dp), allocatable :: lambda(:)

         !> Create
         real(dp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eig_d(amat,lambda,overwrite_a=.false.,err=err)

     end function stdlib_linalg_eigvals_d

     module function stdlib_linalg_eigvals_noerr_d(a) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> Array of eigenvalues
         complex(dp), allocatable :: lambda(:)

         !> Create
         real(dp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eig_d(amat,lambda,overwrite_a=.false.)

     end function stdlib_linalg_eigvals_noerr_d

     module subroutine stdlib_linalg_eig_d(a,lambda,right,left,overwrite_a,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of eigenvalues, 
     !! and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         complex(dp), intent(out) :: lambda(:)
         !> [optional] RIGHT eigenvectors of A (as columns)
         complex(dp), optional, intent(out), target :: right(:,:)
         !> [optional] LEFT eigenvectors of A (as columns)
         complex(dp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldu,ldv,info,k,lwork,neig
         logical(lk) :: copy_a
         character :: task_u,task_v
         real(dp), target :: work_dummy(1),u_dummy(1,1),v_dummy(1,1)
         real(dp), allocatable :: work(:)
         real(dp), pointer :: amat(:,:),umat(:,:),vmat(:,:)
         real(dp), pointer :: lreal(:),limag(:)
         !> Matrix size
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         k    = min(m,n)
         neig = size(lambda,kind=ilp) 
         lda  = m

         if (k<=0 .or. m/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                          'invalid or matrix size a=',[m,n],', must be nonempty square.')
            call linalg_error_handling(err0,err)
            return
         elseif (neig<k) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                          'eigenvalue array has insufficient size:',&
                                          ' lambda=',neig,', n=',n)
            call linalg_error_handling(err0,err)
            return
         endif

         ! Can A be overwritten? By default, do not overwrite
         copy_a = .true._lk
         if (present(overwrite_a)) copy_a = .not.overwrite_a

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(m,n),source=a)
         else
            amat => a
         endif

         ! Decide if U, V eigenvectors
         task_u = eigenvectors_task(present(left))
         task_v = eigenvectors_task(present(right))         
         
         if (present(right)) then 
                        
            ! For a real matrix, GEEV returns real arrays. 
            ! Allocate temporary reals, will be converted to complex vectors at the end.
            allocate(vmat(n,n))
            
            if (size(vmat,2,kind=ilp)<n) then 
               err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                        'right eigenvector matrix has insufficient size: ',&
                                        shape(vmat),', with n=',n)
            endif  
            
         else
            vmat => v_dummy
         endif
            
         if (present(left)) then
            
            ! For a real matrix, GEEV returns real arrays. 
            ! Allocate temporary reals, will be converted to complex vectors at the end.
            allocate(umat(n,n))

            if (size(umat,2,kind=ilp)<n) then 
               err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                        'left eigenvector matrix has insufficient size: ',&
                                        shape(umat),', with n=',n)
            endif                
            
         else
            umat => u_dummy
         endif
         
         get_geev: if (err0%ok()) then 

             ldu = size(umat,1,kind=ilp)
             ldv = size(vmat,1,kind=ilp)

             ! Compute workspace size
             allocate(lreal(n),limag(n))

             lwork = -1_ilp
            
             call geev(task_u,task_v,n,amat,lda,&
                       lreal,limag,  &
                       umat,ldu,vmat,ldv,&
                       work_dummy,lwork,info)
             call handle_geev_info(err0,info,m,n)

             ! Compute eigenvalues
             if (info==0) then

                !> Prepare working storage
                lwork = nint(real(work_dummy(1),kind=dp), kind=ilp)
                allocate(work(lwork))

                !> Compute eigensystem
                call geev(task_u,task_v,n,amat,lda,&
                          lreal,limag,  &
                          umat,ldu,vmat,ldv,&            
                          work,lwork,info)
                call handle_geev_info(err0,info,m,n)

             endif
             
             ! Finalize storage and process output flag
             lambda(:n) = cmplx(lreal(:n),limag(:n),kind=dp) 
             
             ! If the j-th and (j+1)-st eigenvalues form a complex conjugate pair, 
             ! geev returns reals as: 
             ! u(j)   = VL(:,j) + i*VL(:,j+1) and
             ! u(j+1) = VL(:,j) - i*VL(:,j+1). 
             ! Convert these to complex numbers here.            
             if (present(right)) call assign_real_eigenvectors_dp(n,lambda,vmat,right)
             if (present(left))  call assign_real_eigenvectors_dp(n,lambda,umat,left)
         
         endif get_geev
         
         if (copy_a) deallocate(amat)
         if (present(right)) deallocate(vmat)
         if (present(left)) deallocate(umat)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_eig_d
     
     module function stdlib_linalg_eigvalsh_d(a,upper_a,err) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A
         !> Input matrix A[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of eigenvalues
         real(dp), allocatable :: lambda(:)
         
         real(dp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eigh_d(amat,lambda,upper_a=upper_a,overwrite_a=.false.,err=err)
         
     end function stdlib_linalg_eigvalsh_d
          
     module function stdlib_linalg_eigvalsh_noerr_d(a,upper_a) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A        
         !> Input matrix A[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: use lower
         logical(lk), optional, intent(in) :: upper_a         
         !> Array of eigenvalues
         real(dp), allocatable :: lambda(:)

         real(dp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eigh_d(amat,lambda,upper_a=upper_a,overwrite_a=.false.)

     end function stdlib_linalg_eigvalsh_noerr_d     

     module subroutine stdlib_linalg_eigh_d(a,lambda,vectors,upper_a,overwrite_a,err)
     !! Eigendecomposition of a real symmetric or complex Hermitian matrix A returning an array `lambda` 
     !! of eigenvalues, and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         real(dp), intent(out) :: lambda(:)
         !> The columns of vectors contain the orthonormal eigenvectors of A
         real(dp), optional, intent(out), target :: vectors(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Should the upper/lower half of A be used? Default: use lower
         logical(lk), optional, intent(in) :: upper_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,info,k,lwork,neig
         logical(lk) :: copy_a
         character :: triangle,task
         real(dp), target :: work_dummy(1)
         real(dp), allocatable :: work(:)
         real(dp), pointer :: amat(:,:)

         !> Matrix size
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         k    = min(m,n)
         neig = size(lambda,kind=ilp) 

         if (k<=0 .or. m/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or matrix size a=',[m,n], &
                                                        ', must be non-empty square.')
            call linalg_error_handling(err0,err)
            return
         elseif (neig<k) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'eigenvalue array has insufficient size:',&
                                                        ' lambda=',neig,' must be >= n=',n)
            call linalg_error_handling(err0,err)
            return
         endif
        
         ! Check if input A can be overwritten
         copy_a = .true._lk
         if (present(vectors)) then 
            ! No need to copy A anyways
            copy_a = .false.
         elseif (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         endif        
         
         ! Should we use the upper or lower half of the matrix?
         triangle = symmetric_triangle_task(upper_a)
         
         ! Request for eigenvectors
         task = eigenvectors_task(present(vectors))
         
         if (present(vectors)) then 
            
            ! Check size
            if (any(shape(vectors,kind=ilp)<n)) then 
               err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                        'eigenvector matrix has insufficient size: ',&
                                        shape(vectors),', with n=',n)
               call linalg_error_handling(err0,err)
               return
            endif            
            
            ! The input matrix will be overwritten by the vectors. 
            ! So, use this one as storage for syev/heev
            amat => vectors
            
            ! Copy data in
            amat(:n,:n) = a(:n,:n)
                        
         elseif (copy_a) then
            ! Initialize a matrix temporary
            allocate(amat(m,n),source=a)
         else
            ! Overwrite A
            amat => a
         endif


         lda = size(amat,1,kind=ilp)

         ! Request workspace size
         lwork = -1_ilp
         call syev(task,triangle,n,amat,lda,lambda,work_dummy,lwork,info)
         call handle_heev_info(err0,info,m,n)

         ! Compute eigenvalues
         if (info==0) then

            !> Prepare working storage
            lwork = nint(real(work_dummy(1),kind=dp), kind=ilp)
            allocate(work(lwork))

            !> Compute eigensystem
            call syev(task,triangle,n,amat,lda,lambda,work,lwork,info)
            call handle_heev_info(err0,info,m,n)

         endif
         
         ! Finalize storage and process output flag         
         if (copy_a) deallocate(amat)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_eigh_d
     

     module function stdlib_linalg_eigvals_c(a,err) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of eigenvalues
         complex(sp), allocatable :: lambda(:)

         !> Create
         complex(sp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eig_c(amat,lambda,overwrite_a=.false.,err=err)

     end function stdlib_linalg_eigvals_c

     module function stdlib_linalg_eigvals_noerr_c(a) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> Array of eigenvalues
         complex(sp), allocatable :: lambda(:)

         !> Create
         complex(sp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eig_c(amat,lambda,overwrite_a=.false.)

     end function stdlib_linalg_eigvals_noerr_c

     module subroutine stdlib_linalg_eig_c(a,lambda,right,left,overwrite_a,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of eigenvalues, 
     !! and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         complex(sp), intent(out) :: lambda(:)
         !> [optional] RIGHT eigenvectors of A (as columns)
         complex(sp), optional, intent(out), target :: right(:,:)
         !> [optional] LEFT eigenvectors of A (as columns)
         complex(sp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldu,ldv,info,k,lwork,neig
         logical(lk) :: copy_a
         character :: task_u,task_v
         complex(sp), target :: work_dummy(1),u_dummy(1,1),v_dummy(1,1)
         complex(sp), allocatable :: work(:)
         complex(sp), pointer :: amat(:,:),umat(:,:),vmat(:,:)
         real(sp), allocatable :: rwork(:)
         !> Matrix size
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         k    = min(m,n)
         neig = size(lambda,kind=ilp) 
         lda  = m

         if (k<=0 .or. m/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                          'invalid or matrix size a=',[m,n],', must be nonempty square.')
            call linalg_error_handling(err0,err)
            return
         elseif (neig<k) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                          'eigenvalue array has insufficient size:',&
                                          ' lambda=',neig,', n=',n)
            call linalg_error_handling(err0,err)
            return
         endif

         ! Can A be overwritten? By default, do not overwrite
         copy_a = .true._lk
         if (present(overwrite_a)) copy_a = .not.overwrite_a

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(m,n),source=a)
         else
            amat => a
         endif

         ! Decide if U, V eigenvectors
         task_u = eigenvectors_task(present(left))
         task_v = eigenvectors_task(present(right))         
         
         if (present(right)) then 
                        
            ! For a complex matrix, GEEV returns complex arrays. 
            ! Point directly to output.
            vmat => right
            
            if (size(vmat,2,kind=ilp)<n) then 
               err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                        'right eigenvector matrix has insufficient size: ',&
                                        shape(vmat),', with n=',n)
            endif  
            
         else
            vmat => v_dummy
         endif
            
         if (present(left)) then
            
            ! For a complex matrix, GEEV returns complex arrays. 
            ! Point directly to output.
            umat => left

            if (size(umat,2,kind=ilp)<n) then 
               err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                        'left eigenvector matrix has insufficient size: ',&
                                        shape(umat),', with n=',n)
            endif                
            
         else
            umat => u_dummy
         endif
         
         get_geev: if (err0%ok()) then 

             ldu = size(umat,1,kind=ilp)
             ldv = size(vmat,1,kind=ilp)

             ! Compute workspace size
             allocate(rwork(2*n))

             lwork = -1_ilp
            
             call geev(task_u,task_v,n,amat,lda,&
                       lambda,  &
                       umat,ldu,vmat,ldv,&
                       work_dummy,lwork,rwork,info)
             call handle_geev_info(err0,info,m,n)

             ! Compute eigenvalues
             if (info==0) then

                !> Prepare working storage
                lwork = nint(real(work_dummy(1),kind=sp), kind=ilp)
                allocate(work(lwork))

                !> Compute eigensystem
                call geev(task_u,task_v,n,amat,lda,&
                          lambda,  &
                          umat,ldu,vmat,ldv,&            
                          work,lwork,rwork,info)
                call handle_geev_info(err0,info,m,n)

             endif
             
             ! Finalize storage and process output flag
         
         endif get_geev
         
         if (copy_a) deallocate(amat)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_eig_c
     
     module function stdlib_linalg_eigvalsh_c(a,upper_a,err) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A
         !> Input matrix A[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of eigenvalues
         real(sp), allocatable :: lambda(:)
         
         complex(sp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eigh_c(amat,lambda,upper_a=upper_a,overwrite_a=.false.,err=err)
         
     end function stdlib_linalg_eigvalsh_c
          
     module function stdlib_linalg_eigvalsh_noerr_c(a,upper_a) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A        
         !> Input matrix A[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: use lower
         logical(lk), optional, intent(in) :: upper_a         
         !> Array of eigenvalues
         real(sp), allocatable :: lambda(:)

         complex(sp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eigh_c(amat,lambda,upper_a=upper_a,overwrite_a=.false.)

     end function stdlib_linalg_eigvalsh_noerr_c     

     module subroutine stdlib_linalg_eigh_c(a,lambda,vectors,upper_a,overwrite_a,err)
     !! Eigendecomposition of a real symmetric or complex Hermitian matrix A returning an array `lambda` 
     !! of eigenvalues, and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         real(sp), intent(out) :: lambda(:)
         !> The columns of vectors contain the orthonormal eigenvectors of A
         complex(sp), optional, intent(out), target :: vectors(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Should the upper/lower half of A be used? Default: use lower
         logical(lk), optional, intent(in) :: upper_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,info,k,lwork,neig
         logical(lk) :: copy_a
         character :: triangle,task
         complex(sp), target :: work_dummy(1)
         complex(sp), allocatable :: work(:)
         real(sp), allocatable :: rwork(:)
         complex(sp), pointer :: amat(:,:)

         !> Matrix size
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         k    = min(m,n)
         neig = size(lambda,kind=ilp) 

         if (k<=0 .or. m/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or matrix size a=',[m,n], &
                                                        ', must be non-empty square.')
            call linalg_error_handling(err0,err)
            return
         elseif (neig<k) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'eigenvalue array has insufficient size:',&
                                                        ' lambda=',neig,' must be >= n=',n)
            call linalg_error_handling(err0,err)
            return
         endif
        
         ! Check if input A can be overwritten
         copy_a = .true._lk
         if (present(vectors)) then 
            ! No need to copy A anyways
            copy_a = .false.
         elseif (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         endif        
         
         ! Should we use the upper or lower half of the matrix?
         triangle = symmetric_triangle_task(upper_a)
         
         ! Request for eigenvectors
         task = eigenvectors_task(present(vectors))
         
         if (present(vectors)) then 
            
            ! Check size
            if (any(shape(vectors,kind=ilp)<n)) then 
               err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                        'eigenvector matrix has insufficient size: ',&
                                        shape(vectors),', with n=',n)
               call linalg_error_handling(err0,err)
               return
            endif            
            
            ! The input matrix will be overwritten by the vectors. 
            ! So, use this one as storage for syev/heev
            amat => vectors
            
            ! Copy data in
            amat(:n,:n) = a(:n,:n)
                        
         elseif (copy_a) then
            ! Initialize a matrix temporary
            allocate(amat(m,n),source=a)
         else
            ! Overwrite A
            amat => a
         endif


         lda = size(amat,1,kind=ilp)

         ! Request workspace size
         lwork = -1_ilp
         allocate(rwork(max(1,3*n-2)))
         call heev(task,triangle,n,amat,lda,lambda,work_dummy,lwork,rwork,info)
         call handle_heev_info(err0,info,m,n)

         ! Compute eigenvalues
         if (info==0) then

            !> Prepare working storage
            lwork = nint(real(work_dummy(1),kind=sp), kind=ilp)
            allocate(work(lwork))

            !> Compute eigensystem
            call heev(task,triangle,n,amat,lda,lambda,work,lwork,rwork,info)
            call handle_heev_info(err0,info,m,n)

         endif
         
         ! Finalize storage and process output flag         
         if (copy_a) deallocate(amat)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_eigh_c
     

     module function stdlib_linalg_eigvals_z(a,err) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of eigenvalues
         complex(dp), allocatable :: lambda(:)

         !> Create
         complex(dp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eig_z(amat,lambda,overwrite_a=.false.,err=err)

     end function stdlib_linalg_eigvals_z

     module function stdlib_linalg_eigvals_noerr_z(a) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> Array of eigenvalues
         complex(dp), allocatable :: lambda(:)

         !> Create
         complex(dp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eig_z(amat,lambda,overwrite_a=.false.)

     end function stdlib_linalg_eigvals_noerr_z

     module subroutine stdlib_linalg_eig_z(a,lambda,right,left,overwrite_a,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of eigenvalues, 
     !! and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         complex(dp), intent(out) :: lambda(:)
         !> [optional] RIGHT eigenvectors of A (as columns)
         complex(dp), optional, intent(out), target :: right(:,:)
         !> [optional] LEFT eigenvectors of A (as columns)
         complex(dp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,ldu,ldv,info,k,lwork,neig
         logical(lk) :: copy_a
         character :: task_u,task_v
         complex(dp), target :: work_dummy(1),u_dummy(1,1),v_dummy(1,1)
         complex(dp), allocatable :: work(:)
         complex(dp), pointer :: amat(:,:),umat(:,:),vmat(:,:)
         real(dp), allocatable :: rwork(:)
         !> Matrix size
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         k    = min(m,n)
         neig = size(lambda,kind=ilp) 
         lda  = m

         if (k<=0 .or. m/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                          'invalid or matrix size a=',[m,n],', must be nonempty square.')
            call linalg_error_handling(err0,err)
            return
         elseif (neig<k) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                          'eigenvalue array has insufficient size:',&
                                          ' lambda=',neig,', n=',n)
            call linalg_error_handling(err0,err)
            return
         endif

         ! Can A be overwritten? By default, do not overwrite
         copy_a = .true._lk
         if (present(overwrite_a)) copy_a = .not.overwrite_a

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(m,n),source=a)
         else
            amat => a
         endif

         ! Decide if U, V eigenvectors
         task_u = eigenvectors_task(present(left))
         task_v = eigenvectors_task(present(right))         
         
         if (present(right)) then 
                        
            ! For a complex matrix, GEEV returns complex arrays. 
            ! Point directly to output.
            vmat => right
            
            if (size(vmat,2,kind=ilp)<n) then 
               err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                        'right eigenvector matrix has insufficient size: ',&
                                        shape(vmat),', with n=',n)
            endif  
            
         else
            vmat => v_dummy
         endif
            
         if (present(left)) then
            
            ! For a complex matrix, GEEV returns complex arrays. 
            ! Point directly to output.
            umat => left

            if (size(umat,2,kind=ilp)<n) then 
               err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                        'left eigenvector matrix has insufficient size: ',&
                                        shape(umat),', with n=',n)
            endif                
            
         else
            umat => u_dummy
         endif
         
         get_geev: if (err0%ok()) then 

             ldu = size(umat,1,kind=ilp)
             ldv = size(vmat,1,kind=ilp)

             ! Compute workspace size
             allocate(rwork(2*n))

             lwork = -1_ilp
            
             call geev(task_u,task_v,n,amat,lda,&
                       lambda,  &
                       umat,ldu,vmat,ldv,&
                       work_dummy,lwork,rwork,info)
             call handle_geev_info(err0,info,m,n)

             ! Compute eigenvalues
             if (info==0) then

                !> Prepare working storage
                lwork = nint(real(work_dummy(1),kind=dp), kind=ilp)
                allocate(work(lwork))

                !> Compute eigensystem
                call geev(task_u,task_v,n,amat,lda,&
                          lambda,  &
                          umat,ldu,vmat,ldv,&            
                          work,lwork,rwork,info)
                call handle_geev_info(err0,info,m,n)

             endif
             
             ! Finalize storage and process output flag
         
         endif get_geev
         
         if (copy_a) deallocate(amat)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_eig_z
     
     module function stdlib_linalg_eigvalsh_z(a,upper_a,err) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A
         !> Input matrix A[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of eigenvalues
         real(dp), allocatable :: lambda(:)
         
         complex(dp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eigh_z(amat,lambda,upper_a=upper_a,overwrite_a=.false.,err=err)
         
     end function stdlib_linalg_eigvalsh_z
          
     module function stdlib_linalg_eigvalsh_noerr_z(a,upper_a) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A        
         !> Input matrix A[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: use lower
         logical(lk), optional, intent(in) :: upper_a         
         !> Array of eigenvalues
         real(dp), allocatable :: lambda(:)

         complex(dp), pointer :: amat(:,:)
         integer(ilp) :: m,n,k

         !> Create an internal pointer so the intent of A won't affect the next call
         amat => a

         m   = size(a,1,kind=ilp)
         n   = size(a,2,kind=ilp)
         k   = min(m,n)

         !> Allocate return storage
         allocate(lambda(k))

         !> Compute eigenvalues only
         call stdlib_linalg_eigh_z(amat,lambda,upper_a=upper_a,overwrite_a=.false.)

     end function stdlib_linalg_eigvalsh_noerr_z     

     module subroutine stdlib_linalg_eigh_z(a,lambda,vectors,upper_a,overwrite_a,err)
     !! Eigendecomposition of a real symmetric or complex Hermitian matrix A returning an array `lambda` 
     !! of eigenvalues, and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         real(dp), intent(out) :: lambda(:)
         !> The columns of vectors contain the orthonormal eigenvectors of A
         complex(dp), optional, intent(out), target :: vectors(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Should the upper/lower half of A be used? Default: use lower
         logical(lk), optional, intent(in) :: upper_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,lda,info,k,lwork,neig
         logical(lk) :: copy_a
         character :: triangle,task
         complex(dp), target :: work_dummy(1)
         complex(dp), allocatable :: work(:)
         real(dp), allocatable :: rwork(:)
         complex(dp), pointer :: amat(:,:)

         !> Matrix size
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         k    = min(m,n)
         neig = size(lambda,kind=ilp) 

         if (k<=0 .or. m/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or matrix size a=',[m,n], &
                                                        ', must be non-empty square.')
            call linalg_error_handling(err0,err)
            return
         elseif (neig<k) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'eigenvalue array has insufficient size:',&
                                                        ' lambda=',neig,' must be >= n=',n)
            call linalg_error_handling(err0,err)
            return
         endif
        
         ! Check if input A can be overwritten
         copy_a = .true._lk
         if (present(vectors)) then 
            ! No need to copy A anyways
            copy_a = .false.
         elseif (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         endif        
         
         ! Should we use the upper or lower half of the matrix?
         triangle = symmetric_triangle_task(upper_a)
         
         ! Request for eigenvectors
         task = eigenvectors_task(present(vectors))
         
         if (present(vectors)) then 
            
            ! Check size
            if (any(shape(vectors,kind=ilp)<n)) then 
               err0 = linalg_state_type(this,LINALG_VALUE_ERROR,&
                                        'eigenvector matrix has insufficient size: ',&
                                        shape(vectors),', with n=',n)
               call linalg_error_handling(err0,err)
               return
            endif            
            
            ! The input matrix will be overwritten by the vectors. 
            ! So, use this one as storage for syev/heev
            amat => vectors
            
            ! Copy data in
            amat(:n,:n) = a(:n,:n)
                        
         elseif (copy_a) then
            ! Initialize a matrix temporary
            allocate(amat(m,n),source=a)
         else
            ! Overwrite A
            amat => a
         endif


         lda = size(amat,1,kind=ilp)

         ! Request workspace size
         lwork = -1_ilp
         allocate(rwork(max(1,3*n-2)))
         call heev(task,triangle,n,amat,lda,lambda,work_dummy,lwork,rwork,info)
         call handle_heev_info(err0,info,m,n)

         ! Compute eigenvalues
         if (info==0) then

            !> Prepare working storage
            lwork = nint(real(work_dummy(1),kind=dp), kind=ilp)
            allocate(work(lwork))

            !> Compute eigensystem
            call heev(task,triangle,n,amat,lda,lambda,work,lwork,rwork,info)
            call handle_heev_info(err0,info,m,n)

         endif
         
         ! Finalize storage and process output flag         
         if (copy_a) deallocate(amat)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_eigh_z
     
     
     pure subroutine assign_real_eigenvectors_sp(n,lambda,lmat,out_mat)
     !! GEEV for real matrices returns complex eigenvalues in real arrays, where two consecutive
     !! reals at [j,j+1] locations represent the real and imaginary parts of two complex conjugate 
     !! eigenvalues. Convert them to complex here, following the GEEV logic.     
        !> Problem size
        integer(ilp), intent(in) :: n
        !> Array of eigenvalues
        complex(sp), intent(in) :: lambda(:)
        !> Real matrix as returned by geev 
        real(sp), intent(in) :: lmat(:,:)
        !> Complex matrix as returned by eig
        complex(sp), intent(out) :: out_mat(:,:)
        
        integer(ilp) :: i,j
        
        ! Copy matrix
        do concurrent(i=1:n,j=1:n)
           out_mat(i,j) = lmat(i,j)
        end do
        
        ! If the j-th and (j+1)-st eigenvalues form a complex conjugate pair, 
        ! geev returns them as reals as: 
        ! u(j)   = VL(:,j) + i*VL(:,j+1) and
        ! u(j+1) = VL(:,j) - i*VL(:,j+1). 
        ! Convert these to complex numbers here.   
        do j=1,n-1
           if (lambda(j)==conjg(lambda(j+1))) then  
              out_mat(:,  j) = cmplx(lmat(:,j), lmat(:,j+1),kind=sp)
              out_mat(:,j+1) = cmplx(lmat(:,j),-lmat(:,j+1),kind=sp)
           endif
        end do           
        
     end subroutine assign_real_eigenvectors_sp
     
     module subroutine stdlib_linalg_real_eig_s(a,lambda,right,left,overwrite_a,err)
      !! Eigendecomposition of matrix A returning an array `lambda` of real eigenvalues, 
      !! and optionally right or left eigenvectors. Returns an error if the eigenvalues had
      !! non-trivial imaginary parts.
          !> Input matrix A[m,n]
          real(sp), intent(inout), target :: a(:,:)
          !> Array of real eigenvalues
          real(sp), intent(out) :: lambda(:)
          !> The columns of RIGHT contain the right eigenvectors of A
          complex(sp), optional, intent(out), target :: right(:,:)
          !> The columns of LEFT contain the left eigenvectors of A
          complex(sp), optional, intent(out), target :: left(:,:)
          !> [optional] Can A data be overwritten and destroyed?
          logical(lk), optional, intent(in) :: overwrite_a
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), optional, intent(out) :: err
          
          type(linalg_state_type) :: err0
          integer(ilp) :: n
          complex(sp), allocatable :: clambda(:)
          real(sp), parameter :: rtol = epsilon(0.0_sp)
          real(sp), parameter :: atol = tiny(0.0_sp)
          
          n = size(lambda,dim=1,kind=ilp)
          allocate(clambda(n))
          
          call stdlib_linalg_eig_s(a,clambda,right,left,overwrite_a,err0)
          
          ! Check that no eigenvalues have meaningful imaginary part
          if (err0%ok() .and. any(aimag(clambda)>atol+rtol*abs(abs(clambda)))) then 
             err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                'complex eigenvalues detected: max(imag(lambda))=',maxval(aimag(clambda)))
          endif
          
          ! Return real components only
          lambda(:n) = real(clambda,kind=sp)
          
          call linalg_error_handling(err0,err)
          
     end subroutine stdlib_linalg_real_eig_s       
     
     pure subroutine assign_real_eigenvectors_dp(n,lambda,lmat,out_mat)
     !! GEEV for real matrices returns complex eigenvalues in real arrays, where two consecutive
     !! reals at [j,j+1] locations represent the real and imaginary parts of two complex conjugate 
     !! eigenvalues. Convert them to complex here, following the GEEV logic.     
        !> Problem size
        integer(ilp), intent(in) :: n
        !> Array of eigenvalues
        complex(dp), intent(in) :: lambda(:)
        !> Real matrix as returned by geev 
        real(dp), intent(in) :: lmat(:,:)
        !> Complex matrix as returned by eig
        complex(dp), intent(out) :: out_mat(:,:)
        
        integer(ilp) :: i,j
        
        ! Copy matrix
        do concurrent(i=1:n,j=1:n)
           out_mat(i,j) = lmat(i,j)
        end do
        
        ! If the j-th and (j+1)-st eigenvalues form a complex conjugate pair, 
        ! geev returns them as reals as: 
        ! u(j)   = VL(:,j) + i*VL(:,j+1) and
        ! u(j+1) = VL(:,j) - i*VL(:,j+1). 
        ! Convert these to complex numbers here.   
        do j=1,n-1
           if (lambda(j)==conjg(lambda(j+1))) then  
              out_mat(:,  j) = cmplx(lmat(:,j), lmat(:,j+1),kind=dp)
              out_mat(:,j+1) = cmplx(lmat(:,j),-lmat(:,j+1),kind=dp)
           endif
        end do           
        
     end subroutine assign_real_eigenvectors_dp
     
     module subroutine stdlib_linalg_real_eig_d(a,lambda,right,left,overwrite_a,err)
      !! Eigendecomposition of matrix A returning an array `lambda` of real eigenvalues, 
      !! and optionally right or left eigenvectors. Returns an error if the eigenvalues had
      !! non-trivial imaginary parts.
          !> Input matrix A[m,n]
          real(dp), intent(inout), target :: a(:,:)
          !> Array of real eigenvalues
          real(dp), intent(out) :: lambda(:)
          !> The columns of RIGHT contain the right eigenvectors of A
          complex(dp), optional, intent(out), target :: right(:,:)
          !> The columns of LEFT contain the left eigenvectors of A
          complex(dp), optional, intent(out), target :: left(:,:)
          !> [optional] Can A data be overwritten and destroyed?
          logical(lk), optional, intent(in) :: overwrite_a
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), optional, intent(out) :: err
          
          type(linalg_state_type) :: err0
          integer(ilp) :: n
          complex(dp), allocatable :: clambda(:)
          real(dp), parameter :: rtol = epsilon(0.0_dp)
          real(dp), parameter :: atol = tiny(0.0_dp)
          
          n = size(lambda,dim=1,kind=ilp)
          allocate(clambda(n))
          
          call stdlib_linalg_eig_d(a,clambda,right,left,overwrite_a,err0)
          
          ! Check that no eigenvalues have meaningful imaginary part
          if (err0%ok() .and. any(aimag(clambda)>atol+rtol*abs(abs(clambda)))) then 
             err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                'complex eigenvalues detected: max(imag(lambda))=',maxval(aimag(clambda)))
          endif
          
          ! Return real components only
          lambda(:n) = real(clambda,kind=dp)
          
          call linalg_error_handling(err0,err)
          
     end subroutine stdlib_linalg_real_eig_d       
     

end submodule stdlib_linalg_eigenvalues
