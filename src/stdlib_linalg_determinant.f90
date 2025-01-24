submodule (stdlib_linalg) stdlib_linalg_determinant
!! Determinant of a rectangular matrix
     use stdlib_linalg_constants
     use stdlib_linalg_lapack, only: getrf
     use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
         LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR
     implicit none

     ! Function interface
     character(*), parameter :: this = 'determinant'

     contains

     ! BLAS/LAPACK backends do not currently support xdp
     pure module function stdlib_linalg_pure_rspdeterminant(a) result(det)     
     !!### Summary
     !! Compute determinant of a real square matrix (pure interface).
     !!
     !!### Description
     !!
     !! This function computes the determinant of a real square matrix.
     !!
     !! param: a Input matrix of size [m,n].
     !! return: det Matrix determinant.
     !!
     !!### Example
     !!
     !!```fortran
     !!
     !! real(sp) :: matrix(3,3)
     !! real(sp) :: determinant
     !! matrix = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
     !! determinant = det(matrix)         
     !!
     !!```
         !> Input matrix a[m,n]
         real(sp), intent(in) :: a(:,:)
         !> Matrix determinant
         real(sp) :: det

         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,info,perm,k
         integer(ilp), allocatable :: ipiv(:)
         real(sp), allocatable :: amat(:,:)

         ! Matrix determinant size
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)

         if (m/=n .or. .not.min(m,n)>=0) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or non-square matrix: a=[',m,',',n,']')
            det  = 0.0_sp
            ! Process output and return
            call linalg_error_handling(err0)
         return
         end if

         select case (m)
            case (0)
                
                ! Empty array has determinant 1 because math
                det = 1.0_sp

            case (1)
                
                ! Scalar input
                det = a(1,1)

            case default

                ! Find determinant from LU decomposition

                ! Initialize a matrix temporary
                allocate(amat(m,n),source=a)

                ! Pivot indices
                allocate(ipiv(n))

                ! Compute determinant from LU factorization, then calculate the 
                ! product of all diagonal entries of the U factor.
                call getrf(m,n,amat,m,ipiv,info)

                select case (info)
                   case (0)
                       ! Success: compute determinant

                       ! Start with real 1.0
                       det = 1.0_sp
                       perm = 0
                       do k=1,n
                          if (ipiv(k)/=k) perm = perm+1
                          det = det*amat(k,k)
                       end do
                       if (mod(perm,2)/=0) det = -det

                   case (:-1)
                       err0 = linalg_state_type(this,LINALG_ERROR,'invalid matrix size a=[',m,',',n,']')
                   case (1:)
                       err0 = linalg_state_type(this,LINALG_ERROR,'singular matrix')
                   case default
                       err0 = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
                end select

                deallocate(amat)

         end select

         ! Process output and return
         call linalg_error_handling(err0)

     end function stdlib_linalg_pure_rspdeterminant
     
     module function stdlib_linalg_rspdeterminant(a,overwrite_a,err) result(det)
     !!### Summary
     !! Compute determinant of a square matrix (with error control).
     !!
     !!### Description
     !!
     !! This function computes the determinant of a square matrix with error control.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: err State return flag. 
     !! return: det Matrix determinant.
     !!
     !!### Example
     !!
     !!```fortran
     !!
     !! real(sp) :: matrix(3,3)
     !! real(sp) :: determinant
     !! matrix = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
     !! determinant = det(matrix, err=err)         
     !!
     !!```        
     !     
         !> Input matrix a[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> State return flag. 
         type(linalg_state_type), intent(out) :: err
         !> Matrix determinant
         real(sp) :: det

         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,info,perm,k
         integer(ilp), allocatable :: ipiv(:)
         logical(lk) :: copy_a
         real(sp), pointer :: amat(:,:)

         ! Matrix determinant size
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)

         if (m/=n .or. .not.min(m,n)>=0) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or non-square matrix: a=[',m,',',n,']')
            det  = 0.0_sp
            ! Process output and return
            call linalg_error_handling(err0,err)
            return
         end if

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         select case (m)
            case (0)
                
                ! Empty array has determinant 1 because math
                det = 1.0_sp

            case (1)
                
                ! Scalar input
                det = a(1,1)

            case default

                ! Find determinant from LU decomposition

                ! Initialize a matrix temporary
                if (copy_a) then
                   allocate(amat, source=a)
                else
                   amat => a
                endif

                ! Pivot indices
                allocate(ipiv(n))

                ! Compute determinant from LU factorization, then calculate the 
                ! product of all diagonal entries of the U factor.
                call getrf(m,n,amat,m,ipiv,info)

                select case (info)
                   case (0)
                       ! Success: compute determinant

                       ! Start with real 1.0
                       det = 1.0_sp
                       perm = 0
                       do k=1,n
                          if (ipiv(k)/=k) perm = perm+1
                          det = det*amat(k,k)
                       end do
                       if (mod(perm,2)/=0) det = -det

                   case (:-1)
                       err0 = linalg_state_type(this,LINALG_ERROR,'invalid matrix size a=[',m,',',n,']')
                   case (1:)
                       err0 = linalg_state_type(this,LINALG_ERROR,'singular matrix')
                   case default
                       err0 = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
                end select

                if (copy_a) deallocate(amat)

         end select

         ! Process output and return
         call linalg_error_handling(err0,err)

     end function stdlib_linalg_rspdeterminant

     pure module function stdlib_linalg_pure_rdpdeterminant(a) result(det)     
     !!### Summary
     !! Compute determinant of a real square matrix (pure interface).
     !!
     !!### Description
     !!
     !! This function computes the determinant of a real square matrix.
     !!
     !! param: a Input matrix of size [m,n].
     !! return: det Matrix determinant.
     !!
     !!### Example
     !!
     !!```fortran
     !!
     !! real(dp) :: matrix(3,3)
     !! real(dp) :: determinant
     !! matrix = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
     !! determinant = det(matrix)         
     !!
     !!```
         !> Input matrix a[m,n]
         real(dp), intent(in) :: a(:,:)
         !> Matrix determinant
         real(dp) :: det

         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,info,perm,k
         integer(ilp), allocatable :: ipiv(:)
         real(dp), allocatable :: amat(:,:)

         ! Matrix determinant size
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)

         if (m/=n .or. .not.min(m,n)>=0) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or non-square matrix: a=[',m,',',n,']')
            det  = 0.0_dp
            ! Process output and return
            call linalg_error_handling(err0)
         return
         end if

         select case (m)
            case (0)
                
                ! Empty array has determinant 1 because math
                det = 1.0_dp

            case (1)
                
                ! Scalar input
                det = a(1,1)

            case default

                ! Find determinant from LU decomposition

                ! Initialize a matrix temporary
                allocate(amat(m,n),source=a)

                ! Pivot indices
                allocate(ipiv(n))

                ! Compute determinant from LU factorization, then calculate the 
                ! product of all diagonal entries of the U factor.
                call getrf(m,n,amat,m,ipiv,info)

                select case (info)
                   case (0)
                       ! Success: compute determinant

                       ! Start with real 1.0
                       det = 1.0_dp
                       perm = 0
                       do k=1,n
                          if (ipiv(k)/=k) perm = perm+1
                          det = det*amat(k,k)
                       end do
                       if (mod(perm,2)/=0) det = -det

                   case (:-1)
                       err0 = linalg_state_type(this,LINALG_ERROR,'invalid matrix size a=[',m,',',n,']')
                   case (1:)
                       err0 = linalg_state_type(this,LINALG_ERROR,'singular matrix')
                   case default
                       err0 = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
                end select

                deallocate(amat)

         end select

         ! Process output and return
         call linalg_error_handling(err0)

     end function stdlib_linalg_pure_rdpdeterminant
     
     module function stdlib_linalg_rdpdeterminant(a,overwrite_a,err) result(det)
     !!### Summary
     !! Compute determinant of a square matrix (with error control).
     !!
     !!### Description
     !!
     !! This function computes the determinant of a square matrix with error control.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: err State return flag. 
     !! return: det Matrix determinant.
     !!
     !!### Example
     !!
     !!```fortran
     !!
     !! real(dp) :: matrix(3,3)
     !! real(dp) :: determinant
     !! matrix = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
     !! determinant = det(matrix, err=err)         
     !!
     !!```        
     !     
         !> Input matrix a[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> State return flag. 
         type(linalg_state_type), intent(out) :: err
         !> Matrix determinant
         real(dp) :: det

         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,info,perm,k
         integer(ilp), allocatable :: ipiv(:)
         logical(lk) :: copy_a
         real(dp), pointer :: amat(:,:)

         ! Matrix determinant size
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)

         if (m/=n .or. .not.min(m,n)>=0) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or non-square matrix: a=[',m,',',n,']')
            det  = 0.0_dp
            ! Process output and return
            call linalg_error_handling(err0,err)
            return
         end if

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         select case (m)
            case (0)
                
                ! Empty array has determinant 1 because math
                det = 1.0_dp

            case (1)
                
                ! Scalar input
                det = a(1,1)

            case default

                ! Find determinant from LU decomposition

                ! Initialize a matrix temporary
                if (copy_a) then
                   allocate(amat, source=a)
                else
                   amat => a
                endif

                ! Pivot indices
                allocate(ipiv(n))

                ! Compute determinant from LU factorization, then calculate the 
                ! product of all diagonal entries of the U factor.
                call getrf(m,n,amat,m,ipiv,info)

                select case (info)
                   case (0)
                       ! Success: compute determinant

                       ! Start with real 1.0
                       det = 1.0_dp
                       perm = 0
                       do k=1,n
                          if (ipiv(k)/=k) perm = perm+1
                          det = det*amat(k,k)
                       end do
                       if (mod(perm,2)/=0) det = -det

                   case (:-1)
                       err0 = linalg_state_type(this,LINALG_ERROR,'invalid matrix size a=[',m,',',n,']')
                   case (1:)
                       err0 = linalg_state_type(this,LINALG_ERROR,'singular matrix')
                   case default
                       err0 = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
                end select

                if (copy_a) deallocate(amat)

         end select

         ! Process output and return
         call linalg_error_handling(err0,err)

     end function stdlib_linalg_rdpdeterminant

     pure module function stdlib_linalg_pure_cspdeterminant(a) result(det)     
     !!### Summary
     !! Compute determinant of a real square matrix (pure interface).
     !!
     !!### Description
     !!
     !! This function computes the determinant of a real square matrix.
     !!
     !! param: a Input matrix of size [m,n].
     !! return: det Matrix determinant.
     !!
     !!### Example
     !!
     !!```fortran
     !!
     !! complex(sp) :: matrix(3,3)
     !! complex(sp) :: determinant
     !! matrix = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
     !! determinant = det(matrix)         
     !!
     !!```
         !> Input matrix a[m,n]
         complex(sp), intent(in) :: a(:,:)
         !> Matrix determinant
         complex(sp) :: det

         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,info,perm,k
         integer(ilp), allocatable :: ipiv(:)
         complex(sp), allocatable :: amat(:,:)

         ! Matrix determinant size
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)

         if (m/=n .or. .not.min(m,n)>=0) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or non-square matrix: a=[',m,',',n,']')
            det  = 0.0_sp
            ! Process output and return
            call linalg_error_handling(err0)
         return
         end if

         select case (m)
            case (0)
                
                ! Empty array has determinant 1 because math
                det = 1.0_sp

            case (1)
                
                ! Scalar input
                det = a(1,1)

            case default

                ! Find determinant from LU decomposition

                ! Initialize a matrix temporary
                allocate(amat(m,n),source=a)

                ! Pivot indices
                allocate(ipiv(n))

                ! Compute determinant from LU factorization, then calculate the 
                ! product of all diagonal entries of the U factor.
                call getrf(m,n,amat,m,ipiv,info)

                select case (info)
                   case (0)
                       ! Success: compute determinant

                       ! Start with real 1.0
                       det = 1.0_sp
                       perm = 0
                       do k=1,n
                          if (ipiv(k)/=k) perm = perm+1
                          det = det*amat(k,k)
                       end do
                       if (mod(perm,2)/=0) det = -det

                   case (:-1)
                       err0 = linalg_state_type(this,LINALG_ERROR,'invalid matrix size a=[',m,',',n,']')
                   case (1:)
                       err0 = linalg_state_type(this,LINALG_ERROR,'singular matrix')
                   case default
                       err0 = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
                end select

                deallocate(amat)

         end select

         ! Process output and return
         call linalg_error_handling(err0)

     end function stdlib_linalg_pure_cspdeterminant
     
     module function stdlib_linalg_cspdeterminant(a,overwrite_a,err) result(det)
     !!### Summary
     !! Compute determinant of a square matrix (with error control).
     !!
     !!### Description
     !!
     !! This function computes the determinant of a square matrix with error control.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: err State return flag. 
     !! return: det Matrix determinant.
     !!
     !!### Example
     !!
     !!```fortran
     !!
     !! complex(sp) :: matrix(3,3)
     !! complex(sp) :: determinant
     !! matrix = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
     !! determinant = det(matrix, err=err)         
     !!
     !!```        
     !     
         !> Input matrix a[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> State return flag. 
         type(linalg_state_type), intent(out) :: err
         !> Matrix determinant
         complex(sp) :: det

         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,info,perm,k
         integer(ilp), allocatable :: ipiv(:)
         logical(lk) :: copy_a
         complex(sp), pointer :: amat(:,:)

         ! Matrix determinant size
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)

         if (m/=n .or. .not.min(m,n)>=0) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or non-square matrix: a=[',m,',',n,']')
            det  = 0.0_sp
            ! Process output and return
            call linalg_error_handling(err0,err)
            return
         end if

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         select case (m)
            case (0)
                
                ! Empty array has determinant 1 because math
                det = 1.0_sp

            case (1)
                
                ! Scalar input
                det = a(1,1)

            case default

                ! Find determinant from LU decomposition

                ! Initialize a matrix temporary
                if (copy_a) then
                   allocate(amat, source=a)
                else
                   amat => a
                endif

                ! Pivot indices
                allocate(ipiv(n))

                ! Compute determinant from LU factorization, then calculate the 
                ! product of all diagonal entries of the U factor.
                call getrf(m,n,amat,m,ipiv,info)

                select case (info)
                   case (0)
                       ! Success: compute determinant

                       ! Start with real 1.0
                       det = 1.0_sp
                       perm = 0
                       do k=1,n
                          if (ipiv(k)/=k) perm = perm+1
                          det = det*amat(k,k)
                       end do
                       if (mod(perm,2)/=0) det = -det

                   case (:-1)
                       err0 = linalg_state_type(this,LINALG_ERROR,'invalid matrix size a=[',m,',',n,']')
                   case (1:)
                       err0 = linalg_state_type(this,LINALG_ERROR,'singular matrix')
                   case default
                       err0 = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
                end select

                if (copy_a) deallocate(amat)

         end select

         ! Process output and return
         call linalg_error_handling(err0,err)

     end function stdlib_linalg_cspdeterminant

     pure module function stdlib_linalg_pure_cdpdeterminant(a) result(det)     
     !!### Summary
     !! Compute determinant of a real square matrix (pure interface).
     !!
     !!### Description
     !!
     !! This function computes the determinant of a real square matrix.
     !!
     !! param: a Input matrix of size [m,n].
     !! return: det Matrix determinant.
     !!
     !!### Example
     !!
     !!```fortran
     !!
     !! complex(dp) :: matrix(3,3)
     !! complex(dp) :: determinant
     !! matrix = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
     !! determinant = det(matrix)         
     !!
     !!```
         !> Input matrix a[m,n]
         complex(dp), intent(in) :: a(:,:)
         !> Matrix determinant
         complex(dp) :: det

         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,info,perm,k
         integer(ilp), allocatable :: ipiv(:)
         complex(dp), allocatable :: amat(:,:)

         ! Matrix determinant size
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)

         if (m/=n .or. .not.min(m,n)>=0) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or non-square matrix: a=[',m,',',n,']')
            det  = 0.0_dp
            ! Process output and return
            call linalg_error_handling(err0)
         return
         end if

         select case (m)
            case (0)
                
                ! Empty array has determinant 1 because math
                det = 1.0_dp

            case (1)
                
                ! Scalar input
                det = a(1,1)

            case default

                ! Find determinant from LU decomposition

                ! Initialize a matrix temporary
                allocate(amat(m,n),source=a)

                ! Pivot indices
                allocate(ipiv(n))

                ! Compute determinant from LU factorization, then calculate the 
                ! product of all diagonal entries of the U factor.
                call getrf(m,n,amat,m,ipiv,info)

                select case (info)
                   case (0)
                       ! Success: compute determinant

                       ! Start with real 1.0
                       det = 1.0_dp
                       perm = 0
                       do k=1,n
                          if (ipiv(k)/=k) perm = perm+1
                          det = det*amat(k,k)
                       end do
                       if (mod(perm,2)/=0) det = -det

                   case (:-1)
                       err0 = linalg_state_type(this,LINALG_ERROR,'invalid matrix size a=[',m,',',n,']')
                   case (1:)
                       err0 = linalg_state_type(this,LINALG_ERROR,'singular matrix')
                   case default
                       err0 = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
                end select

                deallocate(amat)

         end select

         ! Process output and return
         call linalg_error_handling(err0)

     end function stdlib_linalg_pure_cdpdeterminant
     
     module function stdlib_linalg_cdpdeterminant(a,overwrite_a,err) result(det)
     !!### Summary
     !! Compute determinant of a square matrix (with error control).
     !!
     !!### Description
     !!
     !! This function computes the determinant of a square matrix with error control.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: err State return flag. 
     !! return: det Matrix determinant.
     !!
     !!### Example
     !!
     !!```fortran
     !!
     !! complex(dp) :: matrix(3,3)
     !! complex(dp) :: determinant
     !! matrix = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
     !! determinant = det(matrix, err=err)         
     !!
     !!```        
     !     
         !> Input matrix a[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> State return flag. 
         type(linalg_state_type), intent(out) :: err
         !> Matrix determinant
         complex(dp) :: det

         !! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: m,n,info,perm,k
         integer(ilp), allocatable :: ipiv(:)
         logical(lk) :: copy_a
         complex(dp), pointer :: amat(:,:)

         ! Matrix determinant size
         m = size(a,1,kind=ilp)
         n = size(a,2,kind=ilp)

         if (m/=n .or. .not.min(m,n)>=0) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid or non-square matrix: a=[',m,',',n,']')
            det  = 0.0_dp
            ! Process output and return
            call linalg_error_handling(err0,err)
            return
         end if

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         select case (m)
            case (0)
                
                ! Empty array has determinant 1 because math
                det = 1.0_dp

            case (1)
                
                ! Scalar input
                det = a(1,1)

            case default

                ! Find determinant from LU decomposition

                ! Initialize a matrix temporary
                if (copy_a) then
                   allocate(amat, source=a)
                else
                   amat => a
                endif

                ! Pivot indices
                allocate(ipiv(n))

                ! Compute determinant from LU factorization, then calculate the 
                ! product of all diagonal entries of the U factor.
                call getrf(m,n,amat,m,ipiv,info)

                select case (info)
                   case (0)
                       ! Success: compute determinant

                       ! Start with real 1.0
                       det = 1.0_dp
                       perm = 0
                       do k=1,n
                          if (ipiv(k)/=k) perm = perm+1
                          det = det*amat(k,k)
                       end do
                       if (mod(perm,2)/=0) det = -det

                   case (:-1)
                       err0 = linalg_state_type(this,LINALG_ERROR,'invalid matrix size a=[',m,',',n,']')
                   case (1:)
                       err0 = linalg_state_type(this,LINALG_ERROR,'singular matrix')
                   case default
                       err0 = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
                end select

                if (copy_a) deallocate(amat)

         end select

         ! Process output and return
         call linalg_error_handling(err0,err)

     end function stdlib_linalg_cdpdeterminant


end submodule stdlib_linalg_determinant
