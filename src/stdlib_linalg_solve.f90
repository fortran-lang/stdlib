submodule (stdlib_linalg) stdlib_linalg_solve
!! Solve linear system Ax=b
     use stdlib_linalg_constants
     use stdlib_linalg_lapack, only: gesv
     use stdlib_linalg_lapack_aux, only: handle_gesv_info
     use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
          LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR     
     implicit none
     
     character(*), parameter :: this = 'solve'

     contains
     

     ! Compute the solution to a real system of linear equations A * X = B
     module function stdlib_linalg_s_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:)
                  
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_s_solve_lu_one(a,b,x,overwrite_a=overwrite_a,err=err)
            
     end function stdlib_linalg_s_solve_one

     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module function stdlib_linalg_s_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:)
         
         ! Local variables
         real(sp), allocatable :: amat(:,:)
         
         ! Copy `a` so it can be intent(in)
         allocate(amat,source=a)
         
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_s_solve_lu_one(amat,b,x,overwrite_a=.true.)

     end function stdlib_linalg_s_pure_solve_one
     
     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module subroutine stdlib_linalg_s_solve_lu_one(a,b,x,pivot,overwrite_a,err)
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         ! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,ldb,ldx,nrhsx,nrhs,info,npiv
         integer(ilp), pointer :: ipiv(:)
         logical(lk) :: copy_a
         real(sp), pointer :: xmat(:,:),amat(:,:)         

         ! Problem sizes
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx

         ! Has a pre-allocated pivots storage array been provided? 
         if (present(pivot)) then 
            ipiv => pivot
         else
            allocate(ipiv(n))
         endif
         npiv  = size(ipiv,kind=ilp)

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         if (any([lda,n,ldb]<1) .or. any([lda,ldb,ldx]/=n) .or. nrhsx/=nrhs .or. npiv/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx], &
                                                             'pivot=',n)
            call linalg_error_handling(err0,err)
            return
         end if

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! Initialize solution with the rhs
         x = b
         xmat(1:n,1:nrhs) => x

         ! Solve system
         call gesv(n,nrhs,amat,lda,ipiv,xmat,ldb,info)

         ! Process output
         call handle_gesv_info(this,info,lda,n,nrhs,err0)

         if (copy_a) deallocate(amat)
         if (.not.present(pivot)) deallocate(ipiv)

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_s_solve_lu_one     
     
     ! Compute the solution to a real system of linear equations A * X = B
     module function stdlib_linalg_d_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:)
                  
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_d_solve_lu_one(a,b,x,overwrite_a=overwrite_a,err=err)
            
     end function stdlib_linalg_d_solve_one

     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module function stdlib_linalg_d_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:)
         
         ! Local variables
         real(dp), allocatable :: amat(:,:)
         
         ! Copy `a` so it can be intent(in)
         allocate(amat,source=a)
         
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_d_solve_lu_one(amat,b,x,overwrite_a=.true.)

     end function stdlib_linalg_d_pure_solve_one
     
     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module subroutine stdlib_linalg_d_solve_lu_one(a,b,x,pivot,overwrite_a,err)
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         ! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,ldb,ldx,nrhsx,nrhs,info,npiv
         integer(ilp), pointer :: ipiv(:)
         logical(lk) :: copy_a
         real(dp), pointer :: xmat(:,:),amat(:,:)         

         ! Problem sizes
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx

         ! Has a pre-allocated pivots storage array been provided? 
         if (present(pivot)) then 
            ipiv => pivot
         else
            allocate(ipiv(n))
         endif
         npiv  = size(ipiv,kind=ilp)

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         if (any([lda,n,ldb]<1) .or. any([lda,ldb,ldx]/=n) .or. nrhsx/=nrhs .or. npiv/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx], &
                                                             'pivot=',n)
            call linalg_error_handling(err0,err)
            return
         end if

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! Initialize solution with the rhs
         x = b
         xmat(1:n,1:nrhs) => x

         ! Solve system
         call gesv(n,nrhs,amat,lda,ipiv,xmat,ldb,info)

         ! Process output
         call handle_gesv_info(this,info,lda,n,nrhs,err0)

         if (copy_a) deallocate(amat)
         if (.not.present(pivot)) deallocate(ipiv)

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_d_solve_lu_one     
     
     ! Compute the solution to a real system of linear equations A * X = B
     module function stdlib_linalg_c_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:)
                  
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_c_solve_lu_one(a,b,x,overwrite_a=overwrite_a,err=err)
            
     end function stdlib_linalg_c_solve_one

     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module function stdlib_linalg_c_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:)
         
         ! Local variables
         complex(sp), allocatable :: amat(:,:)
         
         ! Copy `a` so it can be intent(in)
         allocate(amat,source=a)
         
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_c_solve_lu_one(amat,b,x,overwrite_a=.true.)

     end function stdlib_linalg_c_pure_solve_one
     
     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module subroutine stdlib_linalg_c_solve_lu_one(a,b,x,pivot,overwrite_a,err)
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         ! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,ldb,ldx,nrhsx,nrhs,info,npiv
         integer(ilp), pointer :: ipiv(:)
         logical(lk) :: copy_a
         complex(sp), pointer :: xmat(:,:),amat(:,:)         

         ! Problem sizes
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx

         ! Has a pre-allocated pivots storage array been provided? 
         if (present(pivot)) then 
            ipiv => pivot
         else
            allocate(ipiv(n))
         endif
         npiv  = size(ipiv,kind=ilp)

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         if (any([lda,n,ldb]<1) .or. any([lda,ldb,ldx]/=n) .or. nrhsx/=nrhs .or. npiv/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx], &
                                                             'pivot=',n)
            call linalg_error_handling(err0,err)
            return
         end if

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! Initialize solution with the rhs
         x = b
         xmat(1:n,1:nrhs) => x

         ! Solve system
         call gesv(n,nrhs,amat,lda,ipiv,xmat,ldb,info)

         ! Process output
         call handle_gesv_info(this,info,lda,n,nrhs,err0)

         if (copy_a) deallocate(amat)
         if (.not.present(pivot)) deallocate(ipiv)

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_c_solve_lu_one     
     
     ! Compute the solution to a real system of linear equations A * X = B
     module function stdlib_linalg_z_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:)
                  
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_z_solve_lu_one(a,b,x,overwrite_a=overwrite_a,err=err)
            
     end function stdlib_linalg_z_solve_one

     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module function stdlib_linalg_z_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:)
         
         ! Local variables
         complex(dp), allocatable :: amat(:,:)
         
         ! Copy `a` so it can be intent(in)
         allocate(amat,source=a)
         
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_z_solve_lu_one(amat,b,x,overwrite_a=.true.)

     end function stdlib_linalg_z_pure_solve_one
     
     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module subroutine stdlib_linalg_z_solve_lu_one(a,b,x,pivot,overwrite_a,err)
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         ! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,ldb,ldx,nrhsx,nrhs,info,npiv
         integer(ilp), pointer :: ipiv(:)
         logical(lk) :: copy_a
         complex(dp), pointer :: xmat(:,:),amat(:,:)         

         ! Problem sizes
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx

         ! Has a pre-allocated pivots storage array been provided? 
         if (present(pivot)) then 
            ipiv => pivot
         else
            allocate(ipiv(n))
         endif
         npiv  = size(ipiv,kind=ilp)

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         if (any([lda,n,ldb]<1) .or. any([lda,ldb,ldx]/=n) .or. nrhsx/=nrhs .or. npiv/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx], &
                                                             'pivot=',n)
            call linalg_error_handling(err0,err)
            return
         end if

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! Initialize solution with the rhs
         x = b
         xmat(1:n,1:nrhs) => x

         ! Solve system
         call gesv(n,nrhs,amat,lda,ipiv,xmat,ldb,info)

         ! Process output
         call handle_gesv_info(this,info,lda,n,nrhs,err0)

         if (copy_a) deallocate(amat)
         if (.not.present(pivot)) deallocate(ipiv)

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_z_solve_lu_one     
     
     ! Compute the solution to a real system of linear equations A * X = B
     module function stdlib_linalg_s_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:,:)
                  
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_s_solve_lu_many(a,b,x,overwrite_a=overwrite_a,err=err)
            
     end function stdlib_linalg_s_solve_many

     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module function stdlib_linalg_s_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:,:)
         
         ! Local variables
         real(sp), allocatable :: amat(:,:)
         
         ! Copy `a` so it can be intent(in)
         allocate(amat,source=a)
         
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_s_solve_lu_many(amat,b,x,overwrite_a=.true.)

     end function stdlib_linalg_s_pure_solve_many
     
     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module subroutine stdlib_linalg_s_solve_lu_many(a,b,x,pivot,overwrite_a,err)
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         ! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,ldb,ldx,nrhsx,nrhs,info,npiv
         integer(ilp), pointer :: ipiv(:)
         logical(lk) :: copy_a
         real(sp), pointer :: xmat(:,:),amat(:,:)         

         ! Problem sizes
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx

         ! Has a pre-allocated pivots storage array been provided? 
         if (present(pivot)) then 
            ipiv => pivot
         else
            allocate(ipiv(n))
         endif
         npiv  = size(ipiv,kind=ilp)

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         if (any([lda,n,ldb]<1) .or. any([lda,ldb,ldx]/=n) .or. nrhsx/=nrhs .or. npiv/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx], &
                                                             'pivot=',n)
            call linalg_error_handling(err0,err)
            return
         end if

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! Initialize solution with the rhs
         x = b
         xmat(1:n,1:nrhs) => x

         ! Solve system
         call gesv(n,nrhs,amat,lda,ipiv,xmat,ldb,info)

         ! Process output
         call handle_gesv_info(this,info,lda,n,nrhs,err0)

         if (copy_a) deallocate(amat)
         if (.not.present(pivot)) deallocate(ipiv)

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_s_solve_lu_many     
     
     ! Compute the solution to a real system of linear equations A * X = B
     module function stdlib_linalg_d_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:,:)
                  
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_d_solve_lu_many(a,b,x,overwrite_a=overwrite_a,err=err)
            
     end function stdlib_linalg_d_solve_many

     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module function stdlib_linalg_d_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:,:)
         
         ! Local variables
         real(dp), allocatable :: amat(:,:)
         
         ! Copy `a` so it can be intent(in)
         allocate(amat,source=a)
         
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_d_solve_lu_many(amat,b,x,overwrite_a=.true.)

     end function stdlib_linalg_d_pure_solve_many
     
     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module subroutine stdlib_linalg_d_solve_lu_many(a,b,x,pivot,overwrite_a,err)
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         ! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,ldb,ldx,nrhsx,nrhs,info,npiv
         integer(ilp), pointer :: ipiv(:)
         logical(lk) :: copy_a
         real(dp), pointer :: xmat(:,:),amat(:,:)         

         ! Problem sizes
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx

         ! Has a pre-allocated pivots storage array been provided? 
         if (present(pivot)) then 
            ipiv => pivot
         else
            allocate(ipiv(n))
         endif
         npiv  = size(ipiv,kind=ilp)

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         if (any([lda,n,ldb]<1) .or. any([lda,ldb,ldx]/=n) .or. nrhsx/=nrhs .or. npiv/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx], &
                                                             'pivot=',n)
            call linalg_error_handling(err0,err)
            return
         end if

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! Initialize solution with the rhs
         x = b
         xmat(1:n,1:nrhs) => x

         ! Solve system
         call gesv(n,nrhs,amat,lda,ipiv,xmat,ldb,info)

         ! Process output
         call handle_gesv_info(this,info,lda,n,nrhs,err0)

         if (copy_a) deallocate(amat)
         if (.not.present(pivot)) deallocate(ipiv)

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_d_solve_lu_many     
     
     ! Compute the solution to a real system of linear equations A * X = B
     module function stdlib_linalg_c_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:,:)
                  
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_c_solve_lu_many(a,b,x,overwrite_a=overwrite_a,err=err)
            
     end function stdlib_linalg_c_solve_many

     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module function stdlib_linalg_c_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:,:)
         
         ! Local variables
         complex(sp), allocatable :: amat(:,:)
         
         ! Copy `a` so it can be intent(in)
         allocate(amat,source=a)
         
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_c_solve_lu_many(amat,b,x,overwrite_a=.true.)

     end function stdlib_linalg_c_pure_solve_many
     
     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module subroutine stdlib_linalg_c_solve_lu_many(a,b,x,pivot,overwrite_a,err)
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         ! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,ldb,ldx,nrhsx,nrhs,info,npiv
         integer(ilp), pointer :: ipiv(:)
         logical(lk) :: copy_a
         complex(sp), pointer :: xmat(:,:),amat(:,:)         

         ! Problem sizes
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx

         ! Has a pre-allocated pivots storage array been provided? 
         if (present(pivot)) then 
            ipiv => pivot
         else
            allocate(ipiv(n))
         endif
         npiv  = size(ipiv,kind=ilp)

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         if (any([lda,n,ldb]<1) .or. any([lda,ldb,ldx]/=n) .or. nrhsx/=nrhs .or. npiv/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx], &
                                                             'pivot=',n)
            call linalg_error_handling(err0,err)
            return
         end if

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! Initialize solution with the rhs
         x = b
         xmat(1:n,1:nrhs) => x

         ! Solve system
         call gesv(n,nrhs,amat,lda,ipiv,xmat,ldb,info)

         ! Process output
         call handle_gesv_info(this,info,lda,n,nrhs,err0)

         if (copy_a) deallocate(amat)
         if (.not.present(pivot)) deallocate(ipiv)

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_c_solve_lu_many     
     
     ! Compute the solution to a real system of linear equations A * X = B
     module function stdlib_linalg_z_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:,:)
                  
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_z_solve_lu_many(a,b,x,overwrite_a=overwrite_a,err=err)
            
     end function stdlib_linalg_z_solve_many

     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module function stdlib_linalg_z_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:,:)
         
         ! Local variables
         complex(dp), allocatable :: amat(:,:)
         
         ! Copy `a` so it can be intent(in)
         allocate(amat,source=a)
         
         ! Initialize solution shape from the rhs array
         allocate(x,mold=b)         
         
         call stdlib_linalg_z_solve_lu_many(amat,b,x,overwrite_a=.true.)

     end function stdlib_linalg_z_pure_solve_many
     
     !> Compute the solution to a real system of linear equations A * X = B (pure interface)
     pure module subroutine stdlib_linalg_z_solve_lu_many(a,b,x,pivot,overwrite_a,err)
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         
         ! Local variables
         type(linalg_state_type) :: err0
         integer(ilp) :: lda,n,ldb,ldx,nrhsx,nrhs,info,npiv
         integer(ilp), pointer :: ipiv(:)
         logical(lk) :: copy_a
         complex(dp), pointer :: xmat(:,:),amat(:,:)         

         ! Problem sizes
         lda   = size(a,1,kind=ilp)
         n     = size(a,2,kind=ilp)
         ldb   = size(b,1,kind=ilp)
         nrhs  = size(b  ,kind=ilp)/ldb
         ldx   = size(x,1,kind=ilp)
         nrhsx = size(x  ,kind=ilp)/ldx

         ! Has a pre-allocated pivots storage array been provided? 
         if (present(pivot)) then 
            ipiv => pivot
         else
            allocate(ipiv(n))
         endif
         npiv  = size(ipiv,kind=ilp)

         ! Can A be overwritten? By default, do not overwrite
         if (present(overwrite_a)) then
            copy_a = .not.overwrite_a
         else
            copy_a = .true._lk
         endif

         if (any([lda,n,ldb]<1) .or. any([lda,ldb,ldx]/=n) .or. nrhsx/=nrhs .or. npiv/=n) then
            err0 = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid sizes: a=',[lda,n], &
                                                             'b=',[ldb,nrhs],' x=',[ldx,nrhsx], &
                                                             'pivot=',n)
            call linalg_error_handling(err0,err)
            return
         end if

         ! Initialize a matrix temporary
         if (copy_a) then
            allocate(amat(lda,n),source=a)
         else
            amat => a
         endif

         ! Initialize solution with the rhs
         x = b
         xmat(1:n,1:nrhs) => x

         ! Solve system
         call gesv(n,nrhs,amat,lda,ipiv,xmat,ldb,info)

         ! Process output
         call handle_gesv_info(this,info,lda,n,nrhs,err0)

         if (copy_a) deallocate(amat)
         if (.not.present(pivot)) deallocate(ipiv)

         ! Process output and return
         call linalg_error_handling(err0,err)

     end subroutine stdlib_linalg_z_solve_lu_many     
     

end submodule stdlib_linalg_solve
