submodule(stdlib_linalg) stdlib_linalg_pseudoinverse
!! Compute the (Moore-Penrose) pseudo-inverse of a matrix.    
     use stdlib_linalg_constants
     use stdlib_linalg_blas
     use stdlib_linalg_lapack
     use stdlib_linalg_state
     use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
     implicit none
     
     character(*), parameter :: this = 'pseudo-inverse'

     contains


     ! Compute the in-place pseudo-inverse of matrix a
     module subroutine stdlib_linalg_pseudoinvert_s(a,pinva,rtol,err)
         !> Input matrix a[m,n]
         real(sp), intent(inout) :: a(:,:)
         !> Output pseudo-inverse matrix
         real(sp), intent(out) :: pinva(:,:)
         !> [optional] Relative tolerance for singular value cutoff
         real(sp), optional, intent(in) :: rtol
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         ! Local variables
         real(sp) :: tolerance,cutoff
         real(sp), allocatable :: s(:)
         real(sp), allocatable :: u(:,:),vt(:,:)
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,k,i,j
         real(sp), parameter :: alpha = 1.0_sp, beta = 0.0_sp         
         character, parameter :: H =  'T' 
         
         ! Problem size
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)
         k = min(m,n)
         if (m<1 .or. n<1) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',[m,n])
            call linalg_error_handling(err0,err)
            return
         end if         
         
         if (any(shape(pinva,kind=ilp)/=[n,m])) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid pinv size:',shape(pinva),'should be',[n,m])
            call linalg_error_handling(err0,err)
            return
         end if                  
         
         ! Singular value threshold
         tolerance = max(m,n)*epsilon(0.0_sp)
         
         ! User threshold: fallback to default if <=0
         if (present(rtol)) then 
            if (rtol>0.0_sp) tolerance = rtol                
         end if
         
         allocate(s(k),u(m,k),vt(k,n))
         call svd(a,s,u,vt,overwrite_a=.false.,full_matrices=.false.,err=err0)
         if (err0%error()) then 
            err0 = linalg_state_type(this,LINALG_ERROR,'svd failure -',err0%message)
            call linalg_error_handling(err0,err)
            return
         endif
         
         !> Discard singular values
         cutoff = tolerance*maxval(s)
         s = merge(1.0_sp/s,0.0_sp,s>cutoff)

         ! Get pseudo-inverse: A_pinv = V * (diag(1/s) * U^H) = V * (U * diag(1/s))^H
         
         ! 1) compute (U * diag(1/s)) in-place
         do concurrent (i=1:m,j=1:k)
            u(i,j) = s(j)*u(i,j)
         end do
            
         ! 2) commutate matmul: A_pinv = V * (U * diag(1/s))^H = ((U * diag(1/s)) * V^H)^H. 
         !    This avoids one matrix transpose
         call gemm(H, H, n, m, k, alpha, vt, k, u, m, beta, pinva, size(pinva,1,kind=ilp))

     end subroutine stdlib_linalg_pseudoinvert_s

     ! Function interface
     module function stdlib_linalg_pseudoinverse_s(a,rtol,err) result(pinva)
         !> Input matrix a[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> [optional] Relative tolerance for singular value cutoff
         real(sp), optional, intent(in) :: rtol         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Matrix pseudo-inverse
         real(sp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))         
         
         ! Use pointer to circumvent svd intent(inout) restriction
         real(sp), pointer :: ap(:,:)
         ap => a
         
         call stdlib_linalg_pseudoinvert_s(ap,pinva,rtol,err)

     end function stdlib_linalg_pseudoinverse_s

     ! Inverse matrix operator
     module function stdlib_linalg_pinv_s_operator(a) result(pinva)
         !> Input matrix a[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> Result pseudo-inverse matrix
         real(sp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))
         
         type(linalg_state_type) :: err

         ! Use pointer to circumvent svd intent(inout) restriction
         real(sp), pointer :: ap(:,:)
         ap => a

         call stdlib_linalg_pseudoinvert_s(ap,pinva,err=err)

         if (err%error()) then 
         pinva = ieee_value(1.0_sp,ieee_quiet_nan)
         endif

     end function stdlib_linalg_pinv_s_operator


     ! Compute the in-place pseudo-inverse of matrix a
     module subroutine stdlib_linalg_pseudoinvert_d(a,pinva,rtol,err)
         !> Input matrix a[m,n]
         real(dp), intent(inout) :: a(:,:)
         !> Output pseudo-inverse matrix
         real(dp), intent(out) :: pinva(:,:)
         !> [optional] Relative tolerance for singular value cutoff
         real(dp), optional, intent(in) :: rtol
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         ! Local variables
         real(dp) :: tolerance,cutoff
         real(dp), allocatable :: s(:)
         real(dp), allocatable :: u(:,:),vt(:,:)
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,k,i,j
         real(dp), parameter :: alpha = 1.0_dp, beta = 0.0_dp         
         character, parameter :: H =  'T' 
         
         ! Problem size
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)
         k = min(m,n)
         if (m<1 .or. n<1) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',[m,n])
            call linalg_error_handling(err0,err)
            return
         end if         
         
         if (any(shape(pinva,kind=ilp)/=[n,m])) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid pinv size:',shape(pinva),'should be',[n,m])
            call linalg_error_handling(err0,err)
            return
         end if                  
         
         ! Singular value threshold
         tolerance = max(m,n)*epsilon(0.0_dp)
         
         ! User threshold: fallback to default if <=0
         if (present(rtol)) then 
            if (rtol>0.0_dp) tolerance = rtol                
         end if
         
         allocate(s(k),u(m,k),vt(k,n))
         call svd(a,s,u,vt,overwrite_a=.false.,full_matrices=.false.,err=err0)
         if (err0%error()) then 
            err0 = linalg_state_type(this,LINALG_ERROR,'svd failure -',err0%message)
            call linalg_error_handling(err0,err)
            return
         endif
         
         !> Discard singular values
         cutoff = tolerance*maxval(s)
         s = merge(1.0_dp/s,0.0_dp,s>cutoff)

         ! Get pseudo-inverse: A_pinv = V * (diag(1/s) * U^H) = V * (U * diag(1/s))^H
         
         ! 1) compute (U * diag(1/s)) in-place
         do concurrent (i=1:m,j=1:k)
            u(i,j) = s(j)*u(i,j)
         end do
            
         ! 2) commutate matmul: A_pinv = V * (U * diag(1/s))^H = ((U * diag(1/s)) * V^H)^H. 
         !    This avoids one matrix transpose
         call gemm(H, H, n, m, k, alpha, vt, k, u, m, beta, pinva, size(pinva,1,kind=ilp))

     end subroutine stdlib_linalg_pseudoinvert_d

     ! Function interface
     module function stdlib_linalg_pseudoinverse_d(a,rtol,err) result(pinva)
         !> Input matrix a[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> [optional] Relative tolerance for singular value cutoff
         real(dp), optional, intent(in) :: rtol         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Matrix pseudo-inverse
         real(dp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))         
         
         ! Use pointer to circumvent svd intent(inout) restriction
         real(dp), pointer :: ap(:,:)
         ap => a
         
         call stdlib_linalg_pseudoinvert_d(ap,pinva,rtol,err)

     end function stdlib_linalg_pseudoinverse_d

     ! Inverse matrix operator
     module function stdlib_linalg_pinv_d_operator(a) result(pinva)
         !> Input matrix a[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> Result pseudo-inverse matrix
         real(dp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))
         
         type(linalg_state_type) :: err

         ! Use pointer to circumvent svd intent(inout) restriction
         real(dp), pointer :: ap(:,:)
         ap => a

         call stdlib_linalg_pseudoinvert_d(ap,pinva,err=err)

         if (err%error()) then 
         pinva = ieee_value(1.0_dp,ieee_quiet_nan)
         endif

     end function stdlib_linalg_pinv_d_operator


     ! Compute the in-place pseudo-inverse of matrix a
     module subroutine stdlib_linalg_pseudoinvert_c(a,pinva,rtol,err)
         !> Input matrix a[m,n]
         complex(sp), intent(inout) :: a(:,:)
         !> Output pseudo-inverse matrix
         complex(sp), intent(out) :: pinva(:,:)
         !> [optional] Relative tolerance for singular value cutoff
         real(sp), optional, intent(in) :: rtol
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         ! Local variables
         real(sp) :: tolerance,cutoff
         real(sp), allocatable :: s(:)
         complex(sp), allocatable :: u(:,:),vt(:,:)
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,k,i,j
         complex(sp), parameter :: alpha = 1.0_sp, beta = 0.0_sp         
         character, parameter :: H =  'C' 
         
         ! Problem size
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)
         k = min(m,n)
         if (m<1 .or. n<1) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',[m,n])
            call linalg_error_handling(err0,err)
            return
         end if         
         
         if (any(shape(pinva,kind=ilp)/=[n,m])) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid pinv size:',shape(pinva),'should be',[n,m])
            call linalg_error_handling(err0,err)
            return
         end if                  
         
         ! Singular value threshold
         tolerance = max(m,n)*epsilon(0.0_sp)
         
         ! User threshold: fallback to default if <=0
         if (present(rtol)) then 
            if (rtol>0.0_sp) tolerance = rtol                
         end if
         
         allocate(s(k),u(m,k),vt(k,n))
         call svd(a,s,u,vt,overwrite_a=.false.,full_matrices=.false.,err=err0)
         if (err0%error()) then 
            err0 = linalg_state_type(this,LINALG_ERROR,'svd failure -',err0%message)
            call linalg_error_handling(err0,err)
            return
         endif
         
         !> Discard singular values
         cutoff = tolerance*maxval(s)
         s = merge(1.0_sp/s,0.0_sp,s>cutoff)

         ! Get pseudo-inverse: A_pinv = V * (diag(1/s) * U^H) = V * (U * diag(1/s))^H
         
         ! 1) compute (U * diag(1/s)) in-place
         do concurrent (i=1:m,j=1:k)
            u(i,j) = s(j)*u(i,j)
         end do
            
         ! 2) commutate matmul: A_pinv = V * (U * diag(1/s))^H = ((U * diag(1/s)) * V^H)^H. 
         !    This avoids one matrix transpose
         call gemm(H, H, n, m, k, alpha, vt, k, u, m, beta, pinva, size(pinva,1,kind=ilp))

     end subroutine stdlib_linalg_pseudoinvert_c

     ! Function interface
     module function stdlib_linalg_pseudoinverse_c(a,rtol,err) result(pinva)
         !> Input matrix a[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> [optional] Relative tolerance for singular value cutoff
         real(sp), optional, intent(in) :: rtol         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Matrix pseudo-inverse
         complex(sp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))         
         
         ! Use pointer to circumvent svd intent(inout) restriction
         complex(sp), pointer :: ap(:,:)
         ap => a
         
         call stdlib_linalg_pseudoinvert_c(ap,pinva,rtol,err)

     end function stdlib_linalg_pseudoinverse_c

     ! Inverse matrix operator
     module function stdlib_linalg_pinv_c_operator(a) result(pinva)
         !> Input matrix a[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> Result pseudo-inverse matrix
         complex(sp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))
         
         type(linalg_state_type) :: err

         ! Use pointer to circumvent svd intent(inout) restriction
         complex(sp), pointer :: ap(:,:)
         ap => a

         call stdlib_linalg_pseudoinvert_c(ap,pinva,err=err)

         if (err%error()) then 
         pinva = cmplx(ieee_value(1.0_sp,ieee_quiet_nan), &
                       ieee_value(1.0_sp,ieee_quiet_nan), kind=sp)
         endif

     end function stdlib_linalg_pinv_c_operator


     ! Compute the in-place pseudo-inverse of matrix a
     module subroutine stdlib_linalg_pseudoinvert_z(a,pinva,rtol,err)
         !> Input matrix a[m,n]
         complex(dp), intent(inout) :: a(:,:)
         !> Output pseudo-inverse matrix
         complex(dp), intent(out) :: pinva(:,:)
         !> [optional] Relative tolerance for singular value cutoff
         real(dp), optional, intent(in) :: rtol
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         ! Local variables
         real(dp) :: tolerance,cutoff
         real(dp), allocatable :: s(:)
         complex(dp), allocatable :: u(:,:),vt(:,:)
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,k,i,j
         complex(dp), parameter :: alpha = 1.0_dp, beta = 0.0_dp         
         character, parameter :: H =  'C' 
         
         ! Problem size
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)
         k = min(m,n)
         if (m<1 .or. n<1) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',[m,n])
            call linalg_error_handling(err0,err)
            return
         end if         
         
         if (any(shape(pinva,kind=ilp)/=[n,m])) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid pinv size:',shape(pinva),'should be',[n,m])
            call linalg_error_handling(err0,err)
            return
         end if                  
         
         ! Singular value threshold
         tolerance = max(m,n)*epsilon(0.0_dp)
         
         ! User threshold: fallback to default if <=0
         if (present(rtol)) then 
            if (rtol>0.0_dp) tolerance = rtol                
         end if
         
         allocate(s(k),u(m,k),vt(k,n))
         call svd(a,s,u,vt,overwrite_a=.false.,full_matrices=.false.,err=err0)
         if (err0%error()) then 
            err0 = linalg_state_type(this,LINALG_ERROR,'svd failure -',err0%message)
            call linalg_error_handling(err0,err)
            return
         endif
         
         !> Discard singular values
         cutoff = tolerance*maxval(s)
         s = merge(1.0_dp/s,0.0_dp,s>cutoff)

         ! Get pseudo-inverse: A_pinv = V * (diag(1/s) * U^H) = V * (U * diag(1/s))^H
         
         ! 1) compute (U * diag(1/s)) in-place
         do concurrent (i=1:m,j=1:k)
            u(i,j) = s(j)*u(i,j)
         end do
            
         ! 2) commutate matmul: A_pinv = V * (U * diag(1/s))^H = ((U * diag(1/s)) * V^H)^H. 
         !    This avoids one matrix transpose
         call gemm(H, H, n, m, k, alpha, vt, k, u, m, beta, pinva, size(pinva,1,kind=ilp))

     end subroutine stdlib_linalg_pseudoinvert_z

     ! Function interface
     module function stdlib_linalg_pseudoinverse_z(a,rtol,err) result(pinva)
         !> Input matrix a[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> [optional] Relative tolerance for singular value cutoff
         real(dp), optional, intent(in) :: rtol         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Matrix pseudo-inverse
         complex(dp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))         
         
         ! Use pointer to circumvent svd intent(inout) restriction
         complex(dp), pointer :: ap(:,:)
         ap => a
         
         call stdlib_linalg_pseudoinvert_z(ap,pinva,rtol,err)

     end function stdlib_linalg_pseudoinverse_z

     ! Inverse matrix operator
     module function stdlib_linalg_pinv_z_operator(a) result(pinva)
         !> Input matrix a[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> Result pseudo-inverse matrix
         complex(dp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))
         
         type(linalg_state_type) :: err

         ! Use pointer to circumvent svd intent(inout) restriction
         complex(dp), pointer :: ap(:,:)
         ap => a

         call stdlib_linalg_pseudoinvert_z(ap,pinva,err=err)

         if (err%error()) then 
         pinva = cmplx(ieee_value(1.0_dp,ieee_quiet_nan), &
                       ieee_value(1.0_dp,ieee_quiet_nan), kind=dp)
         endif

     end function stdlib_linalg_pinv_z_operator


end submodule stdlib_linalg_pseudoinverse
