submodule (stdlib_linalg) stdlib_linalg_inverse
!! Compute inverse of a square matrix    
     use stdlib_linalg_constants
     use stdlib_linalg_lapack, only: getri,getrf,stdlib_ilaenv
     use stdlib_linalg_lapack_aux, only: handle_getri_info
     use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
         LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR
     use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
     implicit none

     character(*), parameter :: this = 'inverse'  

     contains

     ! Compute the in-place square matrix inverse of a
     module subroutine stdlib_linalg_invert_inplace_s(a,pivot,err)
         !> Input matrix a[n,n]. On return, A is destroyed and replaced by the inverse
         real(sp), intent(inout) :: a(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,info,nb,lwork,npiv
         integer(ilp), pointer :: ipiv(:)
         real(sp), allocatable :: work(:)

         !> Problem sizes
         lda  = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)

         ! Has a pre-allocated pivots storage array been provided? 
         if (present(pivot)) then 
            ipiv => pivot
         else
            allocate(ipiv(n))
         endif
         npiv = size(ipiv,kind=ilp)

         if (lda<1 .or. n<1 .or. lda/=n .or. npiv<n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',[lda,n], &
                                                             ', pivot=',npiv)
            if (.not.present(pivot)) deallocate(ipiv)
            call linalg_error_handling(err0,err)
            return
         end if

         ! Factorize matrix (overwrite result)
         call getrf(lda,n,a,lda,ipiv,info)

         ! Return codes from getrf and getri are identical
         if (info==0) then

            ! Get optimal worksize (returned in work(1)) (inflate by a 5% safety margin)
            nb = stdlib_ilaenv(1,'sgetri',' ',n,-1,-1,-1)
            lwork = max(1,min(huge(0_ilp),ceiling(1.05_sp*real(n,sp)*nb,kind=ilp)))

            allocate(work(lwork))

            ! Invert matrix
            call getri(n,a,lda,ipiv,work,lwork,info)

         endif

         ! Process output
         call handle_getri_info(this,info,lda,n,err0)

         ! Process output and return
         if (.not.present(pivot)) deallocate(ipiv)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_invert_inplace_s

     ! Compute the square matrix inverse of a
     module subroutine stdlib_linalg_invert_split_s(a,inva,pivot,err)
         !> Input matrix a[n,n].
         real(sp), intent(in) :: a(:,:)
         !> Inverse matrix a[n,n]. 
         real(sp), intent(out) :: inva(:,:)         
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         type(linalg_state_type) :: err0
         integer(ilp) :: sa(2),sinva(2)
         
         sa    = shape(a,kind=ilp)
         sinva = shape(inva,kind=ilp)
         
         if (any(sa/=sinva)) then
 
             err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',sa,' inva=',sinva)
            
         else
            
             !> Copy data in 
             inva = a

             !> Compute matrix inverse
             call stdlib_linalg_invert_inplace_s(inva,pivot=pivot,err=err0)      
            
         end if         
         
         ! Process output and return
         call linalg_error_handling(err0,err)   
         
     end subroutine stdlib_linalg_invert_split_s

     ! Invert matrix in place
     module function stdlib_linalg_inverse_s(a,err) result(inva)
         !> Input matrix a[n,n]
         real(sp), intent(in) :: a(:,:)
         !> Output matrix inverse
         real(sp), allocatable :: inva(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Allocate with copy
         allocate(inva,source=a)

         !> Compute matrix inverse
         call stdlib_linalg_invert_inplace_s(inva,err=err)

     end function stdlib_linalg_inverse_s

     ! Inverse matrix operator
     module function stdlib_linalg_inverse_s_operator(a) result(inva)
         !> Input matrix a[n,n]
         real(sp), intent(in) :: a(:,:)
         !> Result matrix
         real(sp), allocatable :: inva(:,:)
         
         type(linalg_state_type) :: err

         ! Provide an error handler to return NaNs on issues
         inva = stdlib_linalg_inverse_s(a,err=err)
         
         ! Return NaN on issues
         if (err%error()) then 
            if (allocated(inva)) deallocate(inva)
            allocate(inva(size(a,1,kind=ilp),size(a,2,kind=ilp))) 
            
            inva = ieee_value(1.0_sp,ieee_quiet_nan)
         endif

     end function stdlib_linalg_inverse_s_operator

     ! Compute the in-place square matrix inverse of a
     module subroutine stdlib_linalg_invert_inplace_d(a,pivot,err)
         !> Input matrix a[n,n]. On return, A is destroyed and replaced by the inverse
         real(dp), intent(inout) :: a(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,info,nb,lwork,npiv
         integer(ilp), pointer :: ipiv(:)
         real(dp), allocatable :: work(:)

         !> Problem sizes
         lda  = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)

         ! Has a pre-allocated pivots storage array been provided? 
         if (present(pivot)) then 
            ipiv => pivot
         else
            allocate(ipiv(n))
         endif
         npiv = size(ipiv,kind=ilp)

         if (lda<1 .or. n<1 .or. lda/=n .or. npiv<n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',[lda,n], &
                                                             ', pivot=',npiv)
            if (.not.present(pivot)) deallocate(ipiv)
            call linalg_error_handling(err0,err)
            return
         end if

         ! Factorize matrix (overwrite result)
         call getrf(lda,n,a,lda,ipiv,info)

         ! Return codes from getrf and getri are identical
         if (info==0) then

            ! Get optimal worksize (returned in work(1)) (inflate by a 5% safety margin)
            nb = stdlib_ilaenv(1,'dgetri',' ',n,-1,-1,-1)
            lwork = max(1,min(huge(0_ilp),ceiling(1.05_dp*real(n,dp)*nb,kind=ilp)))

            allocate(work(lwork))

            ! Invert matrix
            call getri(n,a,lda,ipiv,work,lwork,info)

         endif

         ! Process output
         call handle_getri_info(this,info,lda,n,err0)

         ! Process output and return
         if (.not.present(pivot)) deallocate(ipiv)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_invert_inplace_d

     ! Compute the square matrix inverse of a
     module subroutine stdlib_linalg_invert_split_d(a,inva,pivot,err)
         !> Input matrix a[n,n].
         real(dp), intent(in) :: a(:,:)
         !> Inverse matrix a[n,n]. 
         real(dp), intent(out) :: inva(:,:)         
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         type(linalg_state_type) :: err0
         integer(ilp) :: sa(2),sinva(2)
         
         sa    = shape(a,kind=ilp)
         sinva = shape(inva,kind=ilp)
         
         if (any(sa/=sinva)) then
 
             err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',sa,' inva=',sinva)
            
         else
            
             !> Copy data in 
             inva = a

             !> Compute matrix inverse
             call stdlib_linalg_invert_inplace_d(inva,pivot=pivot,err=err0)      
            
         end if         
         
         ! Process output and return
         call linalg_error_handling(err0,err)   
         
     end subroutine stdlib_linalg_invert_split_d

     ! Invert matrix in place
     module function stdlib_linalg_inverse_d(a,err) result(inva)
         !> Input matrix a[n,n]
         real(dp), intent(in) :: a(:,:)
         !> Output matrix inverse
         real(dp), allocatable :: inva(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Allocate with copy
         allocate(inva,source=a)

         !> Compute matrix inverse
         call stdlib_linalg_invert_inplace_d(inva,err=err)

     end function stdlib_linalg_inverse_d

     ! Inverse matrix operator
     module function stdlib_linalg_inverse_d_operator(a) result(inva)
         !> Input matrix a[n,n]
         real(dp), intent(in) :: a(:,:)
         !> Result matrix
         real(dp), allocatable :: inva(:,:)
         
         type(linalg_state_type) :: err

         ! Provide an error handler to return NaNs on issues
         inva = stdlib_linalg_inverse_d(a,err=err)
         
         ! Return NaN on issues
         if (err%error()) then 
            if (allocated(inva)) deallocate(inva)
            allocate(inva(size(a,1,kind=ilp),size(a,2,kind=ilp))) 
            
            inva = ieee_value(1.0_dp,ieee_quiet_nan)
         endif

     end function stdlib_linalg_inverse_d_operator

     ! Compute the in-place square matrix inverse of a
     module subroutine stdlib_linalg_invert_inplace_c(a,pivot,err)
         !> Input matrix a[n,n]. On return, A is destroyed and replaced by the inverse
         complex(sp), intent(inout) :: a(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,info,nb,lwork,npiv
         integer(ilp), pointer :: ipiv(:)
         complex(sp), allocatable :: work(:)

         !> Problem sizes
         lda  = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)

         ! Has a pre-allocated pivots storage array been provided? 
         if (present(pivot)) then 
            ipiv => pivot
         else
            allocate(ipiv(n))
         endif
         npiv = size(ipiv,kind=ilp)

         if (lda<1 .or. n<1 .or. lda/=n .or. npiv<n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',[lda,n], &
                                                             ', pivot=',npiv)
            if (.not.present(pivot)) deallocate(ipiv)
            call linalg_error_handling(err0,err)
            return
         end if

         ! Factorize matrix (overwrite result)
         call getrf(lda,n,a,lda,ipiv,info)

         ! Return codes from getrf and getri are identical
         if (info==0) then

            ! Get optimal worksize (returned in work(1)) (inflate by a 5% safety margin)
            nb = stdlib_ilaenv(1,'cgetri',' ',n,-1,-1,-1)
            lwork = max(1,min(huge(0_ilp),ceiling(1.05_sp*real(n,sp)*nb,kind=ilp)))

            allocate(work(lwork))

            ! Invert matrix
            call getri(n,a,lda,ipiv,work,lwork,info)

         endif

         ! Process output
         call handle_getri_info(this,info,lda,n,err0)

         ! Process output and return
         if (.not.present(pivot)) deallocate(ipiv)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_invert_inplace_c

     ! Compute the square matrix inverse of a
     module subroutine stdlib_linalg_invert_split_c(a,inva,pivot,err)
         !> Input matrix a[n,n].
         complex(sp), intent(in) :: a(:,:)
         !> Inverse matrix a[n,n]. 
         complex(sp), intent(out) :: inva(:,:)         
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         type(linalg_state_type) :: err0
         integer(ilp) :: sa(2),sinva(2)
         
         sa    = shape(a,kind=ilp)
         sinva = shape(inva,kind=ilp)
         
         if (any(sa/=sinva)) then
 
             err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',sa,' inva=',sinva)
            
         else
            
             !> Copy data in 
             inva = a

             !> Compute matrix inverse
             call stdlib_linalg_invert_inplace_c(inva,pivot=pivot,err=err0)      
            
         end if         
         
         ! Process output and return
         call linalg_error_handling(err0,err)   
         
     end subroutine stdlib_linalg_invert_split_c

     ! Invert matrix in place
     module function stdlib_linalg_inverse_c(a,err) result(inva)
         !> Input matrix a[n,n]
         complex(sp), intent(in) :: a(:,:)
         !> Output matrix inverse
         complex(sp), allocatable :: inva(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Allocate with copy
         allocate(inva,source=a)

         !> Compute matrix inverse
         call stdlib_linalg_invert_inplace_c(inva,err=err)

     end function stdlib_linalg_inverse_c

     ! Inverse matrix operator
     module function stdlib_linalg_inverse_c_operator(a) result(inva)
         !> Input matrix a[n,n]
         complex(sp), intent(in) :: a(:,:)
         !> Result matrix
         complex(sp), allocatable :: inva(:,:)
         
         type(linalg_state_type) :: err

         ! Provide an error handler to return NaNs on issues
         inva = stdlib_linalg_inverse_c(a,err=err)
         
         ! Return NaN on issues
         if (err%error()) then 
            if (allocated(inva)) deallocate(inva)
            allocate(inva(size(a,1,kind=ilp),size(a,2,kind=ilp))) 
            
            inva = cmplx(ieee_value(1.0_sp,ieee_quiet_nan), &
                         ieee_value(1.0_sp,ieee_quiet_nan), kind=sp)
         endif

     end function stdlib_linalg_inverse_c_operator

     ! Compute the in-place square matrix inverse of a
     module subroutine stdlib_linalg_invert_inplace_z(a,pivot,err)
         !> Input matrix a[n,n]. On return, A is destroyed and replaced by the inverse
         complex(dp), intent(inout) :: a(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,info,nb,lwork,npiv
         integer(ilp), pointer :: ipiv(:)
         complex(dp), allocatable :: work(:)

         !> Problem sizes
         lda  = size(a,1,kind=ilp)
         n    = size(a,2,kind=ilp)

         ! Has a pre-allocated pivots storage array been provided? 
         if (present(pivot)) then 
            ipiv => pivot
         else
            allocate(ipiv(n))
         endif
         npiv = size(ipiv,kind=ilp)

         if (lda<1 .or. n<1 .or. lda/=n .or. npiv<n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',[lda,n], &
                                                             ', pivot=',npiv)
            if (.not.present(pivot)) deallocate(ipiv)
            call linalg_error_handling(err0,err)
            return
         end if

         ! Factorize matrix (overwrite result)
         call getrf(lda,n,a,lda,ipiv,info)

         ! Return codes from getrf and getri are identical
         if (info==0) then

            ! Get optimal worksize (returned in work(1)) (inflate by a 5% safety margin)
            nb = stdlib_ilaenv(1,'zgetri',' ',n,-1,-1,-1)
            lwork = max(1,min(huge(0_ilp),ceiling(1.05_dp*real(n,dp)*nb,kind=ilp)))

            allocate(work(lwork))

            ! Invert matrix
            call getri(n,a,lda,ipiv,work,lwork,info)

         endif

         ! Process output
         call handle_getri_info(this,info,lda,n,err0)

         ! Process output and return
         if (.not.present(pivot)) deallocate(ipiv)
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_invert_inplace_z

     ! Compute the square matrix inverse of a
     module subroutine stdlib_linalg_invert_split_z(a,inva,pivot,err)
         !> Input matrix a[n,n].
         complex(dp), intent(in) :: a(:,:)
         !> Inverse matrix a[n,n]. 
         complex(dp), intent(out) :: inva(:,:)         
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         type(linalg_state_type) :: err0
         integer(ilp) :: sa(2),sinva(2)
         
         sa    = shape(a,kind=ilp)
         sinva = shape(inva,kind=ilp)
         
         if (any(sa/=sinva)) then
 
             err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',sa,' inva=',sinva)
            
         else
            
             !> Copy data in 
             inva = a

             !> Compute matrix inverse
             call stdlib_linalg_invert_inplace_z(inva,pivot=pivot,err=err0)      
            
         end if         
         
         ! Process output and return
         call linalg_error_handling(err0,err)   
         
     end subroutine stdlib_linalg_invert_split_z

     ! Invert matrix in place
     module function stdlib_linalg_inverse_z(a,err) result(inva)
         !> Input matrix a[n,n]
         complex(dp), intent(in) :: a(:,:)
         !> Output matrix inverse
         complex(dp), allocatable :: inva(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err

         !> Allocate with copy
         allocate(inva,source=a)

         !> Compute matrix inverse
         call stdlib_linalg_invert_inplace_z(inva,err=err)

     end function stdlib_linalg_inverse_z

     ! Inverse matrix operator
     module function stdlib_linalg_inverse_z_operator(a) result(inva)
         !> Input matrix a[n,n]
         complex(dp), intent(in) :: a(:,:)
         !> Result matrix
         complex(dp), allocatable :: inva(:,:)
         
         type(linalg_state_type) :: err

         ! Provide an error handler to return NaNs on issues
         inva = stdlib_linalg_inverse_z(a,err=err)
         
         ! Return NaN on issues
         if (err%error()) then 
            if (allocated(inva)) deallocate(inva)
            allocate(inva(size(a,1,kind=ilp),size(a,2,kind=ilp))) 
            
            inva = cmplx(ieee_value(1.0_dp,ieee_quiet_nan), &
                         ieee_value(1.0_dp,ieee_quiet_nan), kind=dp)
         endif

     end function stdlib_linalg_inverse_z_operator


end submodule stdlib_linalg_inverse
