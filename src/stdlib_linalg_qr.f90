submodule (stdlib_linalg) stdlib_linalg_qr
     use stdlib_linalg_constants
     use stdlib_linalg_lapack, only: geqrf, orgqr, ungqr
     use stdlib_linalg_lapack_aux, only: handle_geqrf_info, handle_orgqr_info
     use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
         LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR     
     implicit none

     character(*), parameter :: this = 'qr'

     contains
     
     ! Check problem size and evaluate whether full/reduced problem is requested
     pure subroutine check_problem_size(m,n,q1,q2,r1,r2,err,reduced)
         integer(ilp), intent(in) :: m,n,q1,q2,r1,r2         
         type(linalg_state_type), intent(out) :: err
         logical, intent(out) :: reduced
         
         integer(ilp) :: k
         
         k = min(m,n)
         reduced = .false.
         
         ! Check sizes
         if (m<1 .or. n<1) then
            err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a(m,n)=',[m,n])
         else
            
            ! Check if we should operate on reduced full QR
            ! - Reduced: shape(Q)==[m,k], shape(R)==[k,n]
            ! - Full   : shape(Q)==[m,m], shape(R)==[m,n]
            if (all([q1,q2]==[m,k] .and. [r1,r2]==[k,n])) then 
                reduced = .true.
            elseif (all([q1,q2]==[m,m] .and. [r1,r2]==[m,n])) then 
                reduced = .false.
            else
                err = linalg_state_type(this,LINALG_VALUE_ERROR,'with a=',[m,n],'q=',[q1,q2],'r=',[r1,r2], &
                      'problem is neither full (q=',[m,m],'r=',[m,n],') nor reduced (q=',[m,m],'r=',[m,n],')')              
            endif 
         end if
         
     end subroutine check_problem_size
     

     
     ! Get workspace size for QR operations
     pure module subroutine get_qr_s_workspace(a,lwork,err)
         !> Input matrix a[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> Minimum workspace size for both operations
         integer(ilp), intent(out) :: lwork
         !> State return flag. Returns an error if the query failed
         type(linalg_state_type), optional, intent(out) :: err
         
         integer(ilp) :: m,n,k,info,lwork_qr,lwork_ord
         real(sp) :: work_dummy(1),tau_dummy(1),a_dummy(1,1)
         type(linalg_state_type) :: err0
         
         lwork = -1_ilp
         
         !> Problem sizes
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)  
         k = min(m,n)      
         
         ! QR space
         lwork_qr = -1_ilp
         call geqrf(m,n,a_dummy,m,tau_dummy,work_dummy,lwork_qr,info)    
         call handle_geqrf_info(this,info,m,n,lwork_qr,err0)
         if (err0%error()) then 
            call linalg_error_handling(err0,err)
            return
         endif
         lwork_qr = ceiling(real(work_dummy(1),kind=sp),kind=ilp)
         
         ! Ordering space (for full factorization)
         lwork_ord = -1_ilp
         call  orgqr   &
              (m,m,k,a_dummy,m,tau_dummy,work_dummy,lwork_ord,info)
         call handle_orgqr_info(this,info,m,n,k,lwork_ord,err0)   
         if (err0%error()) then 
            call linalg_error_handling(err0,err)
            return
         endif
         lwork_ord = ceiling(real(work_dummy(1),kind=sp),kind=ilp)
         
         ! Pick the largest size, so two operations can be performed with the same allocation
         lwork = max(lwork_qr, lwork_ord)             
                  
     end subroutine get_qr_s_workspace 
     
     ! Compute the solution to a real system of linear equations A * X = B
     pure module subroutine stdlib_linalg_s_qr(a,q,r,overwrite_a,storage,err) 
         !> Input matrix a[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Orthogonal matrix Q ([m,m], or [m,k] if reduced)
         real(sp), intent(out), contiguous, target :: q(:,:)
         !> Upper triangular matrix R ([m,n], or [k,n] if reduced)
         real(sp), intent(out), contiguous, target :: r(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Provide pre-allocated workspace, size to be checked with qr_space
         real(sp), intent(out), optional, target :: storage(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: i,j,m,n,k,q1,q2,r1,r2,lda,lwork,info
         logical(lk) :: overwrite_a_,use_q_matrix,reduced
         real(sp) :: r11
         real(sp), parameter :: zero = 0.0_sp
         
         real(sp), pointer :: amat(:,:),tau(:),work(:)         

         !> Problem sizes
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         k    = min(m,n)
         q1   = size(q,1,kind=ilp)
         q2   = size(q,2,kind=ilp)
         r1   = size(r,1,kind=ilp)
         r2   = size(r,2,kind=ilp)

         ! Check if we should operate on reduced full QR
         call check_problem_size(m,n,q1,q2,r1,r2,err0,reduced)
         if (err0%error()) then 
            call linalg_error_handling(err0,err)
            return
         end if
                  
         ! Check if Q can be used as temporary storage for A, 
         ! to be destroyed by *GEQRF
         use_q_matrix = q1>=m .and. q2>=n

         ! Can A be overwritten? By default, do not overwrite
         overwrite_a_ = .false._lk
         if (present(overwrite_a) .and. .not.use_q_matrix) overwrite_a_ = overwrite_a
         
         ! Initialize a matrix temporary, or reuse available
         ! storage if possible
         if (use_q_matrix) then 
            amat => q 
            q(:m,:n) = a
         elseif (overwrite_a_) then
            amat => a
         else
            allocate(amat(m,n),source=a)
         endif
         lda = size(amat,1,kind=ilp)
         
         ! To store the elementary reflectors, we need a [1:k] column. 
         if (.not.use_q_matrix) then 
            ! Q is not being used as the storage matrix
            tau(1:k) => q(1:k,1)
         else
            ! R has unused contiguous storage in the 1st column, except for the 
            ! diagonal element. So, use the full column and store it in a dummy variable
            tau(1:k) => r(1:k,1)
         endif

         ! Retrieve workspace size
         call get_qr_s_workspace(a,lwork,err0)

         if (err0%ok()) then 
                     
             if (present(storage)) then 
                work => storage
             else
                allocate(work(lwork))
             endif
             if (.not.size(work,kind=ilp)>=lwork) then 
                 err0 = linalg_state_type(this,LINALG_ERROR,'insufficient workspace: should be at least ',lwork)
                 call linalg_error_handling(err0,err)
                 return
             endif
             
             ! Compute factorization. 
             call geqrf(m,n,amat,m,tau,work,lwork,info) 
             call handle_geqrf_info(this,info,m,n,lwork,err0)
             
             if (err0%ok()) then      
                
                 ! Get R matrix out before overwritten. 
                 ! Do not copy the first column at this stage: it may be being used by `tau`
                 r11 = amat(1,1)
                 forall(i=1:min(r1,m),j=2:n) r(i,j) = merge(amat(i,j),zero,i<=j)
             
                 ! Convert K elementary reflectors tau(1:k) -> orthogonal matrix Q
                 call  orgqr   &
                      (q1,q2,k,amat,lda,tau,work,lwork,info)
                 call handle_orgqr_info(this,info,m,n,k,lwork,err0)      
                      
                 ! Copy result back to Q
                 if (.not.use_q_matrix) q = amat(:q1,:q2) 
                 
                 ! Copy first column of R
                 r(1,1)  = r11
                 r(2:r1,1) = zero
                 
                 ! Ensure last m-n rows of R are zeros, 
                 ! if full matrices were provided
                 if (.not.reduced) r(k+1:m,1:n) = zero
                    
             endif
             
             if (.not.present(storage)) deallocate(work)
          
         endif            

         if (.not.(use_q_matrix.or.overwrite_a_)) deallocate(amat)

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_s_qr

     
     ! Get workspace size for QR operations
     pure module subroutine get_qr_d_workspace(a,lwork,err)
         !> Input matrix a[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> Minimum workspace size for both operations
         integer(ilp), intent(out) :: lwork
         !> State return flag. Returns an error if the query failed
         type(linalg_state_type), optional, intent(out) :: err
         
         integer(ilp) :: m,n,k,info,lwork_qr,lwork_ord
         real(dp) :: work_dummy(1),tau_dummy(1),a_dummy(1,1)
         type(linalg_state_type) :: err0
         
         lwork = -1_ilp
         
         !> Problem sizes
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)  
         k = min(m,n)      
         
         ! QR space
         lwork_qr = -1_ilp
         call geqrf(m,n,a_dummy,m,tau_dummy,work_dummy,lwork_qr,info)    
         call handle_geqrf_info(this,info,m,n,lwork_qr,err0)
         if (err0%error()) then 
            call linalg_error_handling(err0,err)
            return
         endif
         lwork_qr = ceiling(real(work_dummy(1),kind=dp),kind=ilp)
         
         ! Ordering space (for full factorization)
         lwork_ord = -1_ilp
         call  orgqr   &
              (m,m,k,a_dummy,m,tau_dummy,work_dummy,lwork_ord,info)
         call handle_orgqr_info(this,info,m,n,k,lwork_ord,err0)   
         if (err0%error()) then 
            call linalg_error_handling(err0,err)
            return
         endif
         lwork_ord = ceiling(real(work_dummy(1),kind=dp),kind=ilp)
         
         ! Pick the largest size, so two operations can be performed with the same allocation
         lwork = max(lwork_qr, lwork_ord)             
                  
     end subroutine get_qr_d_workspace 
     
     ! Compute the solution to a real system of linear equations A * X = B
     pure module subroutine stdlib_linalg_d_qr(a,q,r,overwrite_a,storage,err) 
         !> Input matrix a[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Orthogonal matrix Q ([m,m], or [m,k] if reduced)
         real(dp), intent(out), contiguous, target :: q(:,:)
         !> Upper triangular matrix R ([m,n], or [k,n] if reduced)
         real(dp), intent(out), contiguous, target :: r(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Provide pre-allocated workspace, size to be checked with qr_space
         real(dp), intent(out), optional, target :: storage(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: i,j,m,n,k,q1,q2,r1,r2,lda,lwork,info
         logical(lk) :: overwrite_a_,use_q_matrix,reduced
         real(dp) :: r11
         real(dp), parameter :: zero = 0.0_dp
         
         real(dp), pointer :: amat(:,:),tau(:),work(:)         

         !> Problem sizes
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         k    = min(m,n)
         q1   = size(q,1,kind=ilp)
         q2   = size(q,2,kind=ilp)
         r1   = size(r,1,kind=ilp)
         r2   = size(r,2,kind=ilp)

         ! Check if we should operate on reduced full QR
         call check_problem_size(m,n,q1,q2,r1,r2,err0,reduced)
         if (err0%error()) then 
            call linalg_error_handling(err0,err)
            return
         end if
                  
         ! Check if Q can be used as temporary storage for A, 
         ! to be destroyed by *GEQRF
         use_q_matrix = q1>=m .and. q2>=n

         ! Can A be overwritten? By default, do not overwrite
         overwrite_a_ = .false._lk
         if (present(overwrite_a) .and. .not.use_q_matrix) overwrite_a_ = overwrite_a
         
         ! Initialize a matrix temporary, or reuse available
         ! storage if possible
         if (use_q_matrix) then 
            amat => q 
            q(:m,:n) = a
         elseif (overwrite_a_) then
            amat => a
         else
            allocate(amat(m,n),source=a)
         endif
         lda = size(amat,1,kind=ilp)
         
         ! To store the elementary reflectors, we need a [1:k] column. 
         if (.not.use_q_matrix) then 
            ! Q is not being used as the storage matrix
            tau(1:k) => q(1:k,1)
         else
            ! R has unused contiguous storage in the 1st column, except for the 
            ! diagonal element. So, use the full column and store it in a dummy variable
            tau(1:k) => r(1:k,1)
         endif

         ! Retrieve workspace size
         call get_qr_d_workspace(a,lwork,err0)

         if (err0%ok()) then 
                     
             if (present(storage)) then 
                work => storage
             else
                allocate(work(lwork))
             endif
             if (.not.size(work,kind=ilp)>=lwork) then 
                 err0 = linalg_state_type(this,LINALG_ERROR,'insufficient workspace: should be at least ',lwork)
                 call linalg_error_handling(err0,err)
                 return
             endif
             
             ! Compute factorization. 
             call geqrf(m,n,amat,m,tau,work,lwork,info) 
             call handle_geqrf_info(this,info,m,n,lwork,err0)
             
             if (err0%ok()) then      
                
                 ! Get R matrix out before overwritten. 
                 ! Do not copy the first column at this stage: it may be being used by `tau`
                 r11 = amat(1,1)
                 forall(i=1:min(r1,m),j=2:n) r(i,j) = merge(amat(i,j),zero,i<=j)
             
                 ! Convert K elementary reflectors tau(1:k) -> orthogonal matrix Q
                 call  orgqr   &
                      (q1,q2,k,amat,lda,tau,work,lwork,info)
                 call handle_orgqr_info(this,info,m,n,k,lwork,err0)      
                      
                 ! Copy result back to Q
                 if (.not.use_q_matrix) q = amat(:q1,:q2) 
                 
                 ! Copy first column of R
                 r(1,1)  = r11
                 r(2:r1,1) = zero
                 
                 ! Ensure last m-n rows of R are zeros, 
                 ! if full matrices were provided
                 if (.not.reduced) r(k+1:m,1:n) = zero
                    
             endif
             
             if (.not.present(storage)) deallocate(work)
          
         endif            

         if (.not.(use_q_matrix.or.overwrite_a_)) deallocate(amat)

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_d_qr

     
     ! Get workspace size for QR operations
     pure module subroutine get_qr_c_workspace(a,lwork,err)
         !> Input matrix a[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> Minimum workspace size for both operations
         integer(ilp), intent(out) :: lwork
         !> State return flag. Returns an error if the query failed
         type(linalg_state_type), optional, intent(out) :: err
         
         integer(ilp) :: m,n,k,info,lwork_qr,lwork_ord
         complex(sp) :: work_dummy(1),tau_dummy(1),a_dummy(1,1)
         type(linalg_state_type) :: err0
         
         lwork = -1_ilp
         
         !> Problem sizes
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)  
         k = min(m,n)      
         
         ! QR space
         lwork_qr = -1_ilp
         call geqrf(m,n,a_dummy,m,tau_dummy,work_dummy,lwork_qr,info)    
         call handle_geqrf_info(this,info,m,n,lwork_qr,err0)
         if (err0%error()) then 
            call linalg_error_handling(err0,err)
            return
         endif
         lwork_qr = ceiling(real(work_dummy(1),kind=sp),kind=ilp)
         
         ! Ordering space (for full factorization)
         lwork_ord = -1_ilp
         call  ungqr   &
              (m,m,k,a_dummy,m,tau_dummy,work_dummy,lwork_ord,info)
         call handle_orgqr_info(this,info,m,n,k,lwork_ord,err0)   
         if (err0%error()) then 
            call linalg_error_handling(err0,err)
            return
         endif
         lwork_ord = ceiling(real(work_dummy(1),kind=sp),kind=ilp)
         
         ! Pick the largest size, so two operations can be performed with the same allocation
         lwork = max(lwork_qr, lwork_ord)             
                  
     end subroutine get_qr_c_workspace 
     
     ! Compute the solution to a real system of linear equations A * X = B
     pure module subroutine stdlib_linalg_c_qr(a,q,r,overwrite_a,storage,err) 
         !> Input matrix a[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Orthogonal matrix Q ([m,m], or [m,k] if reduced)
         complex(sp), intent(out), contiguous, target :: q(:,:)
         !> Upper triangular matrix R ([m,n], or [k,n] if reduced)
         complex(sp), intent(out), contiguous, target :: r(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Provide pre-allocated workspace, size to be checked with qr_space
         complex(sp), intent(out), optional, target :: storage(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: i,j,m,n,k,q1,q2,r1,r2,lda,lwork,info
         logical(lk) :: overwrite_a_,use_q_matrix,reduced
         complex(sp) :: r11
         complex(sp), parameter :: zero = 0.0_sp
         
         complex(sp), pointer :: amat(:,:),tau(:),work(:)         

         !> Problem sizes
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         k    = min(m,n)
         q1   = size(q,1,kind=ilp)
         q2   = size(q,2,kind=ilp)
         r1   = size(r,1,kind=ilp)
         r2   = size(r,2,kind=ilp)

         ! Check if we should operate on reduced full QR
         call check_problem_size(m,n,q1,q2,r1,r2,err0,reduced)
         if (err0%error()) then 
            call linalg_error_handling(err0,err)
            return
         end if
                  
         ! Check if Q can be used as temporary storage for A, 
         ! to be destroyed by *GEQRF
         use_q_matrix = q1>=m .and. q2>=n

         ! Can A be overwritten? By default, do not overwrite
         overwrite_a_ = .false._lk
         if (present(overwrite_a) .and. .not.use_q_matrix) overwrite_a_ = overwrite_a
         
         ! Initialize a matrix temporary, or reuse available
         ! storage if possible
         if (use_q_matrix) then 
            amat => q 
            q(:m,:n) = a
         elseif (overwrite_a_) then
            amat => a
         else
            allocate(amat(m,n),source=a)
         endif
         lda = size(amat,1,kind=ilp)
         
         ! To store the elementary reflectors, we need a [1:k] column. 
         if (.not.use_q_matrix) then 
            ! Q is not being used as the storage matrix
            tau(1:k) => q(1:k,1)
         else
            ! R has unused contiguous storage in the 1st column, except for the 
            ! diagonal element. So, use the full column and store it in a dummy variable
            tau(1:k) => r(1:k,1)
         endif

         ! Retrieve workspace size
         call get_qr_c_workspace(a,lwork,err0)

         if (err0%ok()) then 
                     
             if (present(storage)) then 
                work => storage
             else
                allocate(work(lwork))
             endif
             if (.not.size(work,kind=ilp)>=lwork) then 
                 err0 = linalg_state_type(this,LINALG_ERROR,'insufficient workspace: should be at least ',lwork)
                 call linalg_error_handling(err0,err)
                 return
             endif
             
             ! Compute factorization. 
             call geqrf(m,n,amat,m,tau,work,lwork,info) 
             call handle_geqrf_info(this,info,m,n,lwork,err0)
             
             if (err0%ok()) then      
                
                 ! Get R matrix out before overwritten. 
                 ! Do not copy the first column at this stage: it may be being used by `tau`
                 r11 = amat(1,1)
                 forall(i=1:min(r1,m),j=2:n) r(i,j) = merge(amat(i,j),zero,i<=j)
             
                 ! Convert K elementary reflectors tau(1:k) -> orthogonal matrix Q
                 call  ungqr   &
                      (q1,q2,k,amat,lda,tau,work,lwork,info)
                 call handle_orgqr_info(this,info,m,n,k,lwork,err0)      
                      
                 ! Copy result back to Q
                 if (.not.use_q_matrix) q = amat(:q1,:q2) 
                 
                 ! Copy first column of R
                 r(1,1)  = r11
                 r(2:r1,1) = zero
                 
                 ! Ensure last m-n rows of R are zeros, 
                 ! if full matrices were provided
                 if (.not.reduced) r(k+1:m,1:n) = zero
                    
             endif
             
             if (.not.present(storage)) deallocate(work)
          
         endif            

         if (.not.(use_q_matrix.or.overwrite_a_)) deallocate(amat)

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_c_qr

     
     ! Get workspace size for QR operations
     pure module subroutine get_qr_z_workspace(a,lwork,err)
         !> Input matrix a[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> Minimum workspace size for both operations
         integer(ilp), intent(out) :: lwork
         !> State return flag. Returns an error if the query failed
         type(linalg_state_type), optional, intent(out) :: err
         
         integer(ilp) :: m,n,k,info,lwork_qr,lwork_ord
         complex(dp) :: work_dummy(1),tau_dummy(1),a_dummy(1,1)
         type(linalg_state_type) :: err0
         
         lwork = -1_ilp
         
         !> Problem sizes
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)  
         k = min(m,n)      
         
         ! QR space
         lwork_qr = -1_ilp
         call geqrf(m,n,a_dummy,m,tau_dummy,work_dummy,lwork_qr,info)    
         call handle_geqrf_info(this,info,m,n,lwork_qr,err0)
         if (err0%error()) then 
            call linalg_error_handling(err0,err)
            return
         endif
         lwork_qr = ceiling(real(work_dummy(1),kind=dp),kind=ilp)
         
         ! Ordering space (for full factorization)
         lwork_ord = -1_ilp
         call  ungqr   &
              (m,m,k,a_dummy,m,tau_dummy,work_dummy,lwork_ord,info)
         call handle_orgqr_info(this,info,m,n,k,lwork_ord,err0)   
         if (err0%error()) then 
            call linalg_error_handling(err0,err)
            return
         endif
         lwork_ord = ceiling(real(work_dummy(1),kind=dp),kind=ilp)
         
         ! Pick the largest size, so two operations can be performed with the same allocation
         lwork = max(lwork_qr, lwork_ord)             
                  
     end subroutine get_qr_z_workspace 
     
     ! Compute the solution to a real system of linear equations A * X = B
     pure module subroutine stdlib_linalg_z_qr(a,q,r,overwrite_a,storage,err) 
         !> Input matrix a[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Orthogonal matrix Q ([m,m], or [m,k] if reduced)
         complex(dp), intent(out), contiguous, target :: q(:,:)
         !> Upper triangular matrix R ([m,n], or [k,n] if reduced)
         complex(dp), intent(out), contiguous, target :: r(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Provide pre-allocated workspace, size to be checked with qr_space
         complex(dp), intent(out), optional, target :: storage(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: i,j,m,n,k,q1,q2,r1,r2,lda,lwork,info
         logical(lk) :: overwrite_a_,use_q_matrix,reduced
         complex(dp) :: r11
         complex(dp), parameter :: zero = 0.0_dp
         
         complex(dp), pointer :: amat(:,:),tau(:),work(:)         

         !> Problem sizes
         m    = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)
         k    = min(m,n)
         q1   = size(q,1,kind=ilp)
         q2   = size(q,2,kind=ilp)
         r1   = size(r,1,kind=ilp)
         r2   = size(r,2,kind=ilp)

         ! Check if we should operate on reduced full QR
         call check_problem_size(m,n,q1,q2,r1,r2,err0,reduced)
         if (err0%error()) then 
            call linalg_error_handling(err0,err)
            return
         end if
                  
         ! Check if Q can be used as temporary storage for A, 
         ! to be destroyed by *GEQRF
         use_q_matrix = q1>=m .and. q2>=n

         ! Can A be overwritten? By default, do not overwrite
         overwrite_a_ = .false._lk
         if (present(overwrite_a) .and. .not.use_q_matrix) overwrite_a_ = overwrite_a
         
         ! Initialize a matrix temporary, or reuse available
         ! storage if possible
         if (use_q_matrix) then 
            amat => q 
            q(:m,:n) = a
         elseif (overwrite_a_) then
            amat => a
         else
            allocate(amat(m,n),source=a)
         endif
         lda = size(amat,1,kind=ilp)
         
         ! To store the elementary reflectors, we need a [1:k] column. 
         if (.not.use_q_matrix) then 
            ! Q is not being used as the storage matrix
            tau(1:k) => q(1:k,1)
         else
            ! R has unused contiguous storage in the 1st column, except for the 
            ! diagonal element. So, use the full column and store it in a dummy variable
            tau(1:k) => r(1:k,1)
         endif

         ! Retrieve workspace size
         call get_qr_z_workspace(a,lwork,err0)

         if (err0%ok()) then 
                     
             if (present(storage)) then 
                work => storage
             else
                allocate(work(lwork))
             endif
             if (.not.size(work,kind=ilp)>=lwork) then 
                 err0 = linalg_state_type(this,LINALG_ERROR,'insufficient workspace: should be at least ',lwork)
                 call linalg_error_handling(err0,err)
                 return
             endif
             
             ! Compute factorization. 
             call geqrf(m,n,amat,m,tau,work,lwork,info) 
             call handle_geqrf_info(this,info,m,n,lwork,err0)
             
             if (err0%ok()) then      
                
                 ! Get R matrix out before overwritten. 
                 ! Do not copy the first column at this stage: it may be being used by `tau`
                 r11 = amat(1,1)
                 forall(i=1:min(r1,m),j=2:n) r(i,j) = merge(amat(i,j),zero,i<=j)
             
                 ! Convert K elementary reflectors tau(1:k) -> orthogonal matrix Q
                 call  ungqr   &
                      (q1,q2,k,amat,lda,tau,work,lwork,info)
                 call handle_orgqr_info(this,info,m,n,k,lwork,err0)      
                      
                 ! Copy result back to Q
                 if (.not.use_q_matrix) q = amat(:q1,:q2) 
                 
                 ! Copy first column of R
                 r(1,1)  = r11
                 r(2:r1,1) = zero
                 
                 ! Ensure last m-n rows of R are zeros, 
                 ! if full matrices were provided
                 if (.not.reduced) r(k+1:m,1:n) = zero
                    
             endif
             
             if (.not.present(storage)) deallocate(work)
          
         endif            

         if (.not.(use_q_matrix.or.overwrite_a_)) deallocate(amat)

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_z_qr


end submodule stdlib_linalg_qr
