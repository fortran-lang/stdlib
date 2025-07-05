! Cholesky factorization of a matrix, based on LAPACK *POTRF functions
submodule (stdlib_linalg) stdlib_linalg_cholesky
     use stdlib_linalg_constants
     use stdlib_linalg_lapack, only: potrf
     use stdlib_linalg_lapack_aux, only: handle_potrf_info
     use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
         LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR
     implicit none

     
     character(*), parameter :: this = 'cholesky'

     contains

     
     ! Compute the Cholesky factorization of a symmetric / Hermitian matrix, A = L*L^T = U^T*U. 
     ! The factorization is returned in-place, overwriting matrix A
     pure module subroutine stdlib_linalg_s_cholesky_inplace(a,lower,other_zeroed,err) 
         !> Input matrix a[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,info,j
         logical(lk) :: lower_,other_zeroed_
         character :: triangle
         real(sp), parameter :: zero = 0.0_sp
         
         !> Check if the lower or upper factor is required. 
         !> Default: use lower factor
         lower_ = .true.
         if (present(lower)) lower_ = lower
         triangle = merge('L','U',lower_)
         
         !> Check if the unused half of the return matrix should be zeroed out (default).
         !> Otherwise it is unused and will contain garbage. 
         other_zeroed_ = .true.
         if (present(other_zeroed)) other_zeroed_ = other_zeroed

         !> Problem size
         lda  = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)

         ! Check sizes
         if (n<1 .or. lda<1 .or. lda<n) then
             
             err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                      'invalid matrix size: a(m,n)=',[lda,n])
            
         else
         
             ! Compute factorization
             call potrf(triangle,n,a,lda,info)
             call handle_potrf_info(this, info,triangle,lda,n,err0)
                      
             ! Zero-out the unused part of matrix A
             clean_unused: if (other_zeroed_ .and. err0%ok()) then    
                 if (lower_) then 
                    forall (j=2:n) a(:j-1,j) = zero
                 else
                    forall (j=1:n-1) a(j+1:,j) = zero
                 endif
             endif clean_unused  
         
         endif 

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_s_cholesky_inplace

     ! Compute the Cholesky factorization of a symmetric / Hermitian matrix, A = L*L^T = U^T*U. 
     ! The factorization is returned as a separate matrix
     pure module subroutine stdlib_linalg_s_cholesky(a,c,lower,other_zeroed,err) 
         !> Input matrix a[n,n]
         real(sp), intent(in) :: a(:,:)
         !> Output matrix with Cholesky factors c[n,n]
         real(sp), intent(out) :: c(:,:)         
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,ldc,nc
         
         ! Check C sizes
         lda = size(a,1,kind=ilp)
           n = size(a,2,kind=ilp)
         ldc = size(c,1,kind=ilp)
          nc = size(c,2,kind=ilp)
          
         if (lda<1 .or. n<1 .or. lda<n .or. ldc<n .or. nc<n) then 
            
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                     'invalid matrix sizes: a=',[lda,n], &
                                                          ' c=',[ldc,nc])
        
         else
            
            ! Copy data in
            c(:n,:n) = a(:n,:n)
            
            ! Get cholesky factors
            call stdlib_linalg_s_cholesky_inplace(c,lower,other_zeroed,err0)            
            
         end if            
         
         ! Process output and return
         call linalg_error_handling(err0,err)
         
     end subroutine stdlib_linalg_s_cholesky

     ! Compute the Cholesky factorization of a symmetric / Hermitian matrix, A = L*L^T = U^T*U. 
     ! Function interface
     pure module function stdlib_linalg_s_cholesky_fun(a,lower,other_zeroed) result(c)
         !> Input matrix a[n,n]
         real(sp), intent(in) :: a(:,:)
         !> Output matrix with Cholesky factors c[n,n]
         real(sp) :: c(size(a, 1),size(a, 2))         
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         
         c = a
         
         call stdlib_linalg_s_cholesky_inplace(c,lower,other_zeroed)

     end function stdlib_linalg_s_cholesky_fun

     
     ! Compute the Cholesky factorization of a symmetric / Hermitian matrix, A = L*L^T = U^T*U. 
     ! The factorization is returned in-place, overwriting matrix A
     pure module subroutine stdlib_linalg_d_cholesky_inplace(a,lower,other_zeroed,err) 
         !> Input matrix a[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,info,j
         logical(lk) :: lower_,other_zeroed_
         character :: triangle
         real(dp), parameter :: zero = 0.0_dp
         
         !> Check if the lower or upper factor is required. 
         !> Default: use lower factor
         lower_ = .true.
         if (present(lower)) lower_ = lower
         triangle = merge('L','U',lower_)
         
         !> Check if the unused half of the return matrix should be zeroed out (default).
         !> Otherwise it is unused and will contain garbage. 
         other_zeroed_ = .true.
         if (present(other_zeroed)) other_zeroed_ = other_zeroed

         !> Problem size
         lda  = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)

         ! Check sizes
         if (n<1 .or. lda<1 .or. lda<n) then
             
             err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                      'invalid matrix size: a(m,n)=',[lda,n])
            
         else
         
             ! Compute factorization
             call potrf(triangle,n,a,lda,info)
             call handle_potrf_info(this, info,triangle,lda,n,err0)
                      
             ! Zero-out the unused part of matrix A
             clean_unused: if (other_zeroed_ .and. err0%ok()) then    
                 if (lower_) then 
                    forall (j=2:n) a(:j-1,j) = zero
                 else
                    forall (j=1:n-1) a(j+1:,j) = zero
                 endif
             endif clean_unused  
         
         endif 

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_d_cholesky_inplace

     ! Compute the Cholesky factorization of a symmetric / Hermitian matrix, A = L*L^T = U^T*U. 
     ! The factorization is returned as a separate matrix
     pure module subroutine stdlib_linalg_d_cholesky(a,c,lower,other_zeroed,err) 
         !> Input matrix a[n,n]
         real(dp), intent(in) :: a(:,:)
         !> Output matrix with Cholesky factors c[n,n]
         real(dp), intent(out) :: c(:,:)         
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,ldc,nc
         
         ! Check C sizes
         lda = size(a,1,kind=ilp)
           n = size(a,2,kind=ilp)
         ldc = size(c,1,kind=ilp)
          nc = size(c,2,kind=ilp)
          
         if (lda<1 .or. n<1 .or. lda<n .or. ldc<n .or. nc<n) then 
            
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                     'invalid matrix sizes: a=',[lda,n], &
                                                          ' c=',[ldc,nc])
        
         else
            
            ! Copy data in
            c(:n,:n) = a(:n,:n)
            
            ! Get cholesky factors
            call stdlib_linalg_d_cholesky_inplace(c,lower,other_zeroed,err0)            
            
         end if            
         
         ! Process output and return
         call linalg_error_handling(err0,err)
         
     end subroutine stdlib_linalg_d_cholesky

     ! Compute the Cholesky factorization of a symmetric / Hermitian matrix, A = L*L^T = U^T*U. 
     ! Function interface
     pure module function stdlib_linalg_d_cholesky_fun(a,lower,other_zeroed) result(c)
         !> Input matrix a[n,n]
         real(dp), intent(in) :: a(:,:)
         !> Output matrix with Cholesky factors c[n,n]
         real(dp) :: c(size(a, 1),size(a, 2))         
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         
         c = a
         
         call stdlib_linalg_d_cholesky_inplace(c,lower,other_zeroed)

     end function stdlib_linalg_d_cholesky_fun

     
     ! Compute the Cholesky factorization of a symmetric / Hermitian matrix, A = L*L^T = U^T*U. 
     ! The factorization is returned in-place, overwriting matrix A
     pure module subroutine stdlib_linalg_c_cholesky_inplace(a,lower,other_zeroed,err) 
         !> Input matrix a[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,info,j
         logical(lk) :: lower_,other_zeroed_
         character :: triangle
         complex(sp), parameter :: zero = 0.0_sp
         
         !> Check if the lower or upper factor is required. 
         !> Default: use lower factor
         lower_ = .true.
         if (present(lower)) lower_ = lower
         triangle = merge('L','U',lower_)
         
         !> Check if the unused half of the return matrix should be zeroed out (default).
         !> Otherwise it is unused and will contain garbage. 
         other_zeroed_ = .true.
         if (present(other_zeroed)) other_zeroed_ = other_zeroed

         !> Problem size
         lda  = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)

         ! Check sizes
         if (n<1 .or. lda<1 .or. lda<n) then
             
             err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                      'invalid matrix size: a(m,n)=',[lda,n])
            
         else
         
             ! Compute factorization
             call potrf(triangle,n,a,lda,info)
             call handle_potrf_info(this, info,triangle,lda,n,err0)
                      
             ! Zero-out the unused part of matrix A
             clean_unused: if (other_zeroed_ .and. err0%ok()) then    
                 if (lower_) then 
                    forall (j=2:n) a(:j-1,j) = zero
                 else
                    forall (j=1:n-1) a(j+1:,j) = zero
                 endif
             endif clean_unused  
         
         endif 

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_c_cholesky_inplace

     ! Compute the Cholesky factorization of a symmetric / Hermitian matrix, A = L*L^T = U^T*U. 
     ! The factorization is returned as a separate matrix
     pure module subroutine stdlib_linalg_c_cholesky(a,c,lower,other_zeroed,err) 
         !> Input matrix a[n,n]
         complex(sp), intent(in) :: a(:,:)
         !> Output matrix with Cholesky factors c[n,n]
         complex(sp), intent(out) :: c(:,:)         
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,ldc,nc
         
         ! Check C sizes
         lda = size(a,1,kind=ilp)
           n = size(a,2,kind=ilp)
         ldc = size(c,1,kind=ilp)
          nc = size(c,2,kind=ilp)
          
         if (lda<1 .or. n<1 .or. lda<n .or. ldc<n .or. nc<n) then 
            
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                     'invalid matrix sizes: a=',[lda,n], &
                                                          ' c=',[ldc,nc])
        
         else
            
            ! Copy data in
            c(:n,:n) = a(:n,:n)
            
            ! Get cholesky factors
            call stdlib_linalg_c_cholesky_inplace(c,lower,other_zeroed,err0)            
            
         end if            
         
         ! Process output and return
         call linalg_error_handling(err0,err)
         
     end subroutine stdlib_linalg_c_cholesky

     ! Compute the Cholesky factorization of a symmetric / Hermitian matrix, A = L*L^T = U^T*U. 
     ! Function interface
     pure module function stdlib_linalg_c_cholesky_fun(a,lower,other_zeroed) result(c)
         !> Input matrix a[n,n]
         complex(sp), intent(in) :: a(:,:)
         !> Output matrix with Cholesky factors c[n,n]
         complex(sp) :: c(size(a, 1),size(a, 2))         
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         
         c = a
         
         call stdlib_linalg_c_cholesky_inplace(c,lower,other_zeroed)

     end function stdlib_linalg_c_cholesky_fun

     
     ! Compute the Cholesky factorization of a symmetric / Hermitian matrix, A = L*L^T = U^T*U. 
     ! The factorization is returned in-place, overwriting matrix A
     pure module subroutine stdlib_linalg_z_cholesky_inplace(a,lower,other_zeroed,err) 
         !> Input matrix a[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,info,j
         logical(lk) :: lower_,other_zeroed_
         character :: triangle
         complex(dp), parameter :: zero = 0.0_dp
         
         !> Check if the lower or upper factor is required. 
         !> Default: use lower factor
         lower_ = .true.
         if (present(lower)) lower_ = lower
         triangle = merge('L','U',lower_)
         
         !> Check if the unused half of the return matrix should be zeroed out (default).
         !> Otherwise it is unused and will contain garbage. 
         other_zeroed_ = .true.
         if (present(other_zeroed)) other_zeroed_ = other_zeroed

         !> Problem size
         lda  = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)

         ! Check sizes
         if (n<1 .or. lda<1 .or. lda<n) then
             
             err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                      'invalid matrix size: a(m,n)=',[lda,n])
            
         else
         
             ! Compute factorization
             call potrf(triangle,n,a,lda,info)
             call handle_potrf_info(this, info,triangle,lda,n,err0)
                      
             ! Zero-out the unused part of matrix A
             clean_unused: if (other_zeroed_ .and. err0%ok()) then    
                 if (lower_) then 
                    forall (j=2:n) a(:j-1,j) = zero
                 else
                    forall (j=1:n-1) a(j+1:,j) = zero
                 endif
             endif clean_unused  
         
         endif 

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_z_cholesky_inplace

     ! Compute the Cholesky factorization of a symmetric / Hermitian matrix, A = L*L^T = U^T*U. 
     ! The factorization is returned as a separate matrix
     pure module subroutine stdlib_linalg_z_cholesky(a,c,lower,other_zeroed,err) 
         !> Input matrix a[n,n]
         complex(dp), intent(in) :: a(:,:)
         !> Output matrix with Cholesky factors c[n,n]
         complex(dp), intent(out) :: c(:,:)         
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,ldc,nc
         
         ! Check C sizes
         lda = size(a,1,kind=ilp)
           n = size(a,2,kind=ilp)
         ldc = size(c,1,kind=ilp)
          nc = size(c,2,kind=ilp)
          
         if (lda<1 .or. n<1 .or. lda<n .or. ldc<n .or. nc<n) then 
            
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR, &
                                     'invalid matrix sizes: a=',[lda,n], &
                                                          ' c=',[ldc,nc])
        
         else
            
            ! Copy data in
            c(:n,:n) = a(:n,:n)
            
            ! Get cholesky factors
            call stdlib_linalg_z_cholesky_inplace(c,lower,other_zeroed,err0)            
            
         end if            
         
         ! Process output and return
         call linalg_error_handling(err0,err)
         
     end subroutine stdlib_linalg_z_cholesky

     ! Compute the Cholesky factorization of a symmetric / Hermitian matrix, A = L*L^T = U^T*U. 
     ! Function interface
     pure module function stdlib_linalg_z_cholesky_fun(a,lower,other_zeroed) result(c)
         !> Input matrix a[n,n]
         complex(dp), intent(in) :: a(:,:)
         !> Output matrix with Cholesky factors c[n,n]
         complex(dp) :: c(size(a, 1),size(a, 2))         
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         
         c = a
         
         call stdlib_linalg_z_cholesky_inplace(c,lower,other_zeroed)

     end function stdlib_linalg_z_cholesky_fun


end submodule stdlib_linalg_cholesky
