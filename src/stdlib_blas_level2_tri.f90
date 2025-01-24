submodule(stdlib_blas) stdlib_blas_level2_tri
  implicit none


  contains

     pure module subroutine stdlib_strmv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! STRMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kx
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_xerbla('STRMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               temp = x(j)
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do i = 1,j - 1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               temp = x(j)
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do i = n,j + 1,-1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (nounit) temp = temp*a(j,j)
                           do i = j - 1,1,-1
                               temp = temp + a(i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*a(j,j)
                           do i = j - 1,1,-1
                               ix = ix - incx
                               temp = temp + a(i,j)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (nounit) temp = temp*a(j,j)
                           do i = j + 1,n
                               temp = temp + a(i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*a(j,j)
                           do i = j + 1,n
                               ix = ix + incx
                               temp = temp + a(i,j)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_strmv

     pure module subroutine stdlib_dtrmv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! DTRMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kx
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_xerbla('DTRMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               temp = x(j)
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do i = 1,j - 1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               temp = x(j)
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do i = n,j + 1,-1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (nounit) temp = temp*a(j,j)
                           do i = j - 1,1,-1
                               temp = temp + a(i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*a(j,j)
                           do i = j - 1,1,-1
                               ix = ix - incx
                               temp = temp + a(i,j)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (nounit) temp = temp*a(j,j)
                           do i = j + 1,n
                               temp = temp + a(i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*a(j,j)
                           do i = j + 1,n
                               ix = ix + incx
                               temp = temp + a(i,j)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_dtrmv


     pure module subroutine stdlib_ctrmv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! CTRMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_xerbla('CTRMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               temp = x(j)
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do i = 1,j - 1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               temp = x(j)
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do i = n,j + 1,-1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x  or  x := a**h*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j - 1,1,-1
                                   temp = temp + a(i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j - 1,1,-1
                                   temp = temp + conjg(a(i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   temp = temp + a(i,j)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   temp = temp + conjg(a(i,j))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j + 1,n
                                   temp = temp + a(i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j + 1,n
                                   temp = temp + conjg(a(i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j + 1,n
                                   ix = ix + incx
                                   temp = temp + a(i,j)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j + 1,n
                                   ix = ix + incx
                                   temp = temp + conjg(a(i,j))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_ctrmv

     pure module subroutine stdlib_ztrmv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! ZTRMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_xerbla('ZTRMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               temp = x(j)
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do i = 1,j - 1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               temp = x(j)
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do i = n,j + 1,-1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x  or  x := a**h*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j - 1,1,-1
                                   temp = temp + a(i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j - 1,1,-1
                                   temp = temp + conjg(a(i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   temp = temp + a(i,j)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   temp = temp + conjg(a(i,j))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j + 1,n
                                   temp = temp + a(i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j + 1,n
                                   temp = temp + conjg(a(i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j + 1,n
                                   ix = ix + incx
                                   temp = temp + a(i,j)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j + 1,n
                                   ix = ix + incx
                                   temp = temp + conjg(a(i,j))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_ztrmv




     pure module subroutine stdlib_stbmv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! STBMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular band matrix, with ( k + 1 ) diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('STBMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx   too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
               ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               temp = x(j)
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(kplus1,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(kplus1,j)
                           end if
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               temp = x(j)
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(1,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(1,j)
                           end if
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = kplus1 - j
                           if (nounit) temp = temp*a(kplus1,j)
                           do i = j - 1,max(1,j-k),-1
                               temp = temp + a(l+i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           kx = kx - incx
                           ix = kx
                           l = kplus1 - j
                           if (nounit) temp = temp*a(kplus1,j)
                           do i = j - 1,max(1,j-k),-1
                               temp = temp + a(l+i,j)*x(ix)
                               ix = ix - incx
                           end do
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = 1 - j
                           if (nounit) temp = temp*a(1,j)
                           do i = j + 1,min(n,j+k)
                               temp = temp + a(l+i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           kx = kx + incx
                           ix = kx
                           l = 1 - j
                           if (nounit) temp = temp*a(1,j)
                           do i = j + 1,min(n,j+k)
                               temp = temp + a(l+i,j)*x(ix)
                               ix = ix + incx
                           end do
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_stbmv

     pure module subroutine stdlib_dtbmv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! DTBMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular band matrix, with ( k + 1 ) diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('DTBMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx   too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
               ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               temp = x(j)
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(kplus1,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(kplus1,j)
                           end if
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               temp = x(j)
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(1,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(1,j)
                           end if
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = kplus1 - j
                           if (nounit) temp = temp*a(kplus1,j)
                           do i = j - 1,max(1,j-k),-1
                               temp = temp + a(l+i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           kx = kx - incx
                           ix = kx
                           l = kplus1 - j
                           if (nounit) temp = temp*a(kplus1,j)
                           do i = j - 1,max(1,j-k),-1
                               temp = temp + a(l+i,j)*x(ix)
                               ix = ix - incx
                           end do
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = 1 - j
                           if (nounit) temp = temp*a(1,j)
                           do i = j + 1,min(n,j+k)
                               temp = temp + a(l+i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           kx = kx + incx
                           ix = kx
                           l = 1 - j
                           if (nounit) temp = temp*a(1,j)
                           do i = j + 1,min(n,j+k)
                               temp = temp + a(l+i,j)*x(ix)
                               ix = ix + incx
                           end do
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_dtbmv


     pure module subroutine stdlib_ctbmv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! CTBMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular band matrix, with ( k + 1 ) diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('CTBMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx   too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
               ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               temp = x(j)
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(kplus1,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(kplus1,j)
                           end if
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               temp = x(j)
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(1,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(1,j)
                           end if
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x  or  x := a**h*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = kplus1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(kplus1,j)
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + a(l+i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(kplus1,j))
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + conjg(a(l+i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           kx = kx - incx
                           ix = kx
                           l = kplus1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(kplus1,j)
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + a(l+i,j)*x(ix)
                                   ix = ix - incx
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(kplus1,j))
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + conjg(a(l+i,j))*x(ix)
                                   ix = ix - incx
                               end do
                           end if
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = 1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(1,j)
                               do i = j + 1,min(n,j+k)
                                   temp = temp + a(l+i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(1,j))
                               do i = j + 1,min(n,j+k)
                                   temp = temp + conjg(a(l+i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           kx = kx + incx
                           ix = kx
                           l = 1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(1,j)
                               do i = j + 1,min(n,j+k)
                                   temp = temp + a(l+i,j)*x(ix)
                                   ix = ix + incx
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(1,j))
                               do i = j + 1,min(n,j+k)
                                   temp = temp + conjg(a(l+i,j))*x(ix)
                                   ix = ix + incx
                               end do
                           end if
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_ctbmv

     pure module subroutine stdlib_ztbmv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! ZTBMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular band matrix, with ( k + 1 ) diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('ZTBMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx   too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
               ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               temp = x(j)
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(kplus1,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(kplus1,j)
                           end if
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               temp = x(j)
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(1,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(1,j)
                           end if
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x  or  x := a**h*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = kplus1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(kplus1,j)
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + a(l+i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(kplus1,j))
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + conjg(a(l+i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           kx = kx - incx
                           ix = kx
                           l = kplus1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(kplus1,j)
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + a(l+i,j)*x(ix)
                                   ix = ix - incx
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(kplus1,j))
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + conjg(a(l+i,j))*x(ix)
                                   ix = ix - incx
                               end do
                           end if
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = 1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(1,j)
                               do i = j + 1,min(n,j+k)
                                   temp = temp + a(l+i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(1,j))
                               do i = j + 1,min(n,j+k)
                                   temp = temp + conjg(a(l+i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           kx = kx + incx
                           ix = kx
                           l = 1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(1,j)
                               do i = j + 1,min(n,j+k)
                                   temp = temp + a(l+i,j)*x(ix)
                                   ix = ix + incx
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(1,j))
                               do i = j + 1,min(n,j+k)
                                   temp = temp + conjg(a(l+i,j))*x(ix)
                                   ix = ix + incx
                               end do
                           end if
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_ztbmv




     pure module subroutine stdlib_stpmv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_sp
     !! STPMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: ap(*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: nounit
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_xerbla('STPMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with one pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x:= a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               temp = x(j)
                               k = kk
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k + 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk+j-1)
                           end if
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk + j - 2
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk+j-1)
                           end if
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               temp = x(j)
                               k = kk
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k - 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk-n+j)
                           end if
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk - (n- (j+1)),-1
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk-n+j)
                           end if
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (nounit) temp = temp*ap(kk)
                           k = kk - 1
                           do i = j - 1,1,-1
                               temp = temp + ap(k)*x(i)
                               k = k - 1
                           end do
                           x(j) = temp
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*ap(kk)
                           do k = kk - 1,kk - j + 1,-1
                               ix = ix - incx
                               temp = temp + ap(k)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (nounit) temp = temp*ap(kk)
                           k = kk + 1
                           do i = j + 1,n
                               temp = temp + ap(k)*x(i)
                               k = k + 1
                           end do
                           x(j) = temp
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*ap(kk)
                           do k = kk + 1,kk + n - j
                               ix = ix + incx
                               temp = temp + ap(k)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_stpmv

     pure module subroutine stdlib_dtpmv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_dp
     !! DTPMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: ap(*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: nounit
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_xerbla('DTPMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with one pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x:= a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               temp = x(j)
                               k = kk
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k + 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk+j-1)
                           end if
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk + j - 2
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk+j-1)
                           end if
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               temp = x(j)
                               k = kk
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k - 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk-n+j)
                           end if
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk - (n- (j+1)),-1
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk-n+j)
                           end if
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (nounit) temp = temp*ap(kk)
                           k = kk - 1
                           do i = j - 1,1,-1
                               temp = temp + ap(k)*x(i)
                               k = k - 1
                           end do
                           x(j) = temp
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*ap(kk)
                           do k = kk - 1,kk - j + 1,-1
                               ix = ix - incx
                               temp = temp + ap(k)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (nounit) temp = temp*ap(kk)
                           k = kk + 1
                           do i = j + 1,n
                               temp = temp + ap(k)*x(i)
                               k = k + 1
                           end do
                           x(j) = temp
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*ap(kk)
                           do k = kk + 1,kk + n - j
                               ix = ix + incx
                               temp = temp + ap(k)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_dtpmv


     pure module subroutine stdlib_ctpmv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_sp
     !! CTPMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_xerbla('CTPMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with cone pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x:= a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               temp = x(j)
                               k = kk
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k + 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk+j-1)
                           end if
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk + j - 2
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk+j-1)
                           end if
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               temp = x(j)
                               k = kk
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k - 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk-n+j)
                           end if
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk - (n- (j+1)),-1
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk-n+j)
                           end if
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x  or  x := a**h*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           k = kk - 1
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do i = j - 1,1,-1
                                   temp = temp + ap(k)*x(i)
                                   k = k - 1
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do i = j - 1,1,-1
                                   temp = temp + conjg(ap(k))*x(i)
                                   k = k - 1
                               end do
                           end if
                           x(j) = temp
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   temp = temp + ap(k)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   temp = temp + conjg(ap(k))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           k = kk + 1
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do i = j + 1,n
                                   temp = temp + ap(k)*x(i)
                                   k = k + 1
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do i = j + 1,n
                                   temp = temp + conjg(ap(k))*x(i)
                                   k = k + 1
                               end do
                           end if
                           x(j) = temp
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   temp = temp + ap(k)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   temp = temp + conjg(ap(k))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_ctpmv

     pure module subroutine stdlib_ztpmv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_dp
     !! ZTPMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_xerbla('ZTPMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with cone pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x:= a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               temp = x(j)
                               k = kk
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k + 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk+j-1)
                           end if
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk + j - 2
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk+j-1)
                           end if
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               temp = x(j)
                               k = kk
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k - 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk-n+j)
                           end if
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk - (n- (j+1)),-1
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk-n+j)
                           end if
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x  or  x := a**h*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           k = kk - 1
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do i = j - 1,1,-1
                                   temp = temp + ap(k)*x(i)
                                   k = k - 1
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do i = j - 1,1,-1
                                   temp = temp + conjg(ap(k))*x(i)
                                   k = k - 1
                               end do
                           end if
                           x(j) = temp
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   temp = temp + ap(k)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   temp = temp + conjg(ap(k))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           k = kk + 1
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do i = j + 1,n
                                   temp = temp + ap(k)*x(i)
                                   k = k + 1
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do i = j + 1,n
                                   temp = temp + conjg(ap(k))*x(i)
                                   k = k + 1
                               end do
                           end if
                           x(j) = temp
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   temp = temp + ap(k)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   temp = temp + conjg(ap(k))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_ztpmv




     pure module subroutine stdlib_strsv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! STRSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kx
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_xerbla('STRSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j + 1,n
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j + 1,n
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           do i = 1,j - 1
                               temp = temp - a(i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           do i = 1,j - 1
                               temp = temp - a(i,j)*x(ix)
                               ix = ix + incx
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           do i = n,j + 1,-1
                               temp = temp - a(i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           do i = n,j + 1,-1
                               temp = temp - a(i,j)*x(ix)
                               ix = ix - incx
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_strsv

     pure module subroutine stdlib_dtrsv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! DTRSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kx
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_xerbla('DTRSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j + 1,n
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j + 1,n
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           do i = 1,j - 1
                               temp = temp - a(i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           do i = 1,j - 1
                               temp = temp - a(i,j)*x(ix)
                               ix = ix + incx
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           do i = n,j + 1,-1
                               temp = temp - a(i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           do i = n,j + 1,-1
                               temp = temp - a(i,j)*x(ix)
                               ix = ix - incx
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_dtrsv


     pure module subroutine stdlib_ctrsv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! CTRSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_xerbla('CTRSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j + 1,n
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j + 1,n
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x  or  x := inv( a**h )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (noconj) then
                               do i = 1,j - 1
                                   temp = temp - a(i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = 1,j - 1
                                   temp = temp - conjg(a(i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           ix = kx
                           temp = x(jx)
                           if (noconj) then
                               do i = 1,j - 1
                                   temp = temp - a(i,j)*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = 1,j - 1
                                   temp = temp - conjg(a(i,j))*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (noconj) then
                               do i = n,j + 1,-1
                                   temp = temp - a(i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = n,j + 1,-1
                                   temp = temp - conjg(a(i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           ix = kx
                           temp = x(jx)
                           if (noconj) then
                               do i = n,j + 1,-1
                                   temp = temp - a(i,j)*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = n,j + 1,-1
                                   temp = temp - conjg(a(i,j))*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_ctrsv

     pure module subroutine stdlib_ztrsv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! ZTRSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_xerbla('ZTRSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j + 1,n
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j + 1,n
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x  or  x := inv( a**h )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (noconj) then
                               do i = 1,j - 1
                                   temp = temp - a(i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = 1,j - 1
                                   temp = temp - conjg(a(i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           ix = kx
                           temp = x(jx)
                           if (noconj) then
                               do i = 1,j - 1
                                   temp = temp - a(i,j)*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = 1,j - 1
                                   temp = temp - conjg(a(i,j))*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (noconj) then
                               do i = n,j + 1,-1
                                   temp = temp - a(i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = n,j + 1,-1
                                   temp = temp - conjg(a(i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           ix = kx
                           temp = x(jx)
                           if (noconj) then
                               do i = n,j + 1,-1
                                   temp = temp - a(i,j)*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = n,j + 1,-1
                                   temp = temp - conjg(a(i,j))*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_ztrsv




     pure module subroutine stdlib_stbsv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! STBSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular band matrix, with ( k + 1 )
     !! diagonals.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('STBSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed by sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               l = kplus1 - j
                               if (nounit) x(j) = x(j)/a(kplus1,j)
                               temp = x(j)
                               do i = j - 1,max(1,j-k),-1
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           kx = kx - incx
                           if (x(jx)/=zero) then
                               ix = kx
                               l = kplus1 - j
                               if (nounit) x(jx) = x(jx)/a(kplus1,j)
                               temp = x(jx)
                               do i = j - 1,max(1,j-k),-1
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               l = 1 - j
                               if (nounit) x(j) = x(j)/a(1,j)
                               temp = x(j)
                               do i = j + 1,min(n,j+k)
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           kx = kx + incx
                           if (x(jx)/=zero) then
                               ix = kx
                               l = 1 - j
                               if (nounit) x(jx) = x(jx)/a(1,j)
                               temp = x(jx)
                               do i = j + 1,min(n,j+k)
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t)*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = kplus1 - j
                           do i = max(1,j-k),j - 1
                               temp = temp - a(l+i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(kplus1,j)
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           l = kplus1 - j
                           do i = max(1,j-k),j - 1
                               temp = temp - a(l+i,j)*x(ix)
                               ix = ix + incx
                           end do
                           if (nounit) temp = temp/a(kplus1,j)
                           x(jx) = temp
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = 1 - j
                           do i = min(n,j+k),j + 1,-1
                               temp = temp - a(l+i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(1,j)
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           l = 1 - j
                           do i = min(n,j+k),j + 1,-1
                               temp = temp - a(l+i,j)*x(ix)
                               ix = ix - incx
                           end do
                           if (nounit) temp = temp/a(1,j)
                           x(jx) = temp
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_stbsv

     pure module subroutine stdlib_dtbsv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! DTBSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular band matrix, with ( k + 1 )
     !! diagonals.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('DTBSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed by sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               l = kplus1 - j
                               if (nounit) x(j) = x(j)/a(kplus1,j)
                               temp = x(j)
                               do i = j - 1,max(1,j-k),-1
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           kx = kx - incx
                           if (x(jx)/=zero) then
                               ix = kx
                               l = kplus1 - j
                               if (nounit) x(jx) = x(jx)/a(kplus1,j)
                               temp = x(jx)
                               do i = j - 1,max(1,j-k),-1
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               l = 1 - j
                               if (nounit) x(j) = x(j)/a(1,j)
                               temp = x(j)
                               do i = j + 1,min(n,j+k)
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           kx = kx + incx
                           if (x(jx)/=zero) then
                               ix = kx
                               l = 1 - j
                               if (nounit) x(jx) = x(jx)/a(1,j)
                               temp = x(jx)
                               do i = j + 1,min(n,j+k)
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t)*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = kplus1 - j
                           do i = max(1,j-k),j - 1
                               temp = temp - a(l+i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(kplus1,j)
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           l = kplus1 - j
                           do i = max(1,j-k),j - 1
                               temp = temp - a(l+i,j)*x(ix)
                               ix = ix + incx
                           end do
                           if (nounit) temp = temp/a(kplus1,j)
                           x(jx) = temp
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = 1 - j
                           do i = min(n,j+k),j + 1,-1
                               temp = temp - a(l+i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(1,j)
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           l = 1 - j
                           do i = min(n,j+k),j + 1,-1
                               temp = temp - a(l+i,j)*x(ix)
                               ix = ix - incx
                           end do
                           if (nounit) temp = temp/a(1,j)
                           x(jx) = temp
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_dtbsv


     pure module subroutine stdlib_ctbsv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! CTBSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular band matrix, with ( k + 1 )
     !! diagonals.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('CTBSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed by sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               l = kplus1 - j
                               if (nounit) x(j) = x(j)/a(kplus1,j)
                               temp = x(j)
                               do i = j - 1,max(1,j-k),-1
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           kx = kx - incx
                           if (x(jx)/=czero) then
                               ix = kx
                               l = kplus1 - j
                               if (nounit) x(jx) = x(jx)/a(kplus1,j)
                               temp = x(jx)
                               do i = j - 1,max(1,j-k),-1
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               l = 1 - j
                               if (nounit) x(j) = x(j)/a(1,j)
                               temp = x(j)
                               do i = j + 1,min(n,j+k)
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           kx = kx + incx
                           if (x(jx)/=czero) then
                               ix = kx
                               l = 1 - j
                               if (nounit) x(jx) = x(jx)/a(1,j)
                               temp = x(jx)
                               do i = j + 1,min(n,j+k)
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x  or  x := inv( a**h )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = kplus1 - j
                           if (noconj) then
                               do i = max(1,j-k),j - 1
                                   temp = temp - a(l+i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(kplus1,j)
                           else
                               do i = max(1,j-k),j - 1
                                   temp = temp - conjg(a(l+i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(kplus1,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           l = kplus1 - j
                           if (noconj) then
                               do i = max(1,j-k),j - 1
                                   temp = temp - a(l+i,j)*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/a(kplus1,j)
                           else
                               do i = max(1,j-k),j - 1
                                   temp = temp - conjg(a(l+i,j))*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/conjg(a(kplus1,j))
                           end if
                           x(jx) = temp
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = 1 - j
                           if (noconj) then
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - a(l+i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(1,j)
                           else
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - conjg(a(l+i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(1,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           l = 1 - j
                           if (noconj) then
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - a(l+i,j)*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/a(1,j)
                           else
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - conjg(a(l+i,j))*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/conjg(a(1,j))
                           end if
                           x(jx) = temp
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_ctbsv

     pure module subroutine stdlib_ztbsv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! ZTBSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular band matrix, with ( k + 1 )
     !! diagonals.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('ZTBSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed by sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               l = kplus1 - j
                               if (nounit) x(j) = x(j)/a(kplus1,j)
                               temp = x(j)
                               do i = j - 1,max(1,j-k),-1
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           kx = kx - incx
                           if (x(jx)/=czero) then
                               ix = kx
                               l = kplus1 - j
                               if (nounit) x(jx) = x(jx)/a(kplus1,j)
                               temp = x(jx)
                               do i = j - 1,max(1,j-k),-1
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               l = 1 - j
                               if (nounit) x(j) = x(j)/a(1,j)
                               temp = x(j)
                               do i = j + 1,min(n,j+k)
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           kx = kx + incx
                           if (x(jx)/=czero) then
                               ix = kx
                               l = 1 - j
                               if (nounit) x(jx) = x(jx)/a(1,j)
                               temp = x(jx)
                               do i = j + 1,min(n,j+k)
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x  or  x := inv( a**h )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = kplus1 - j
                           if (noconj) then
                               do i = max(1,j-k),j - 1
                                   temp = temp - a(l+i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(kplus1,j)
                           else
                               do i = max(1,j-k),j - 1
                                   temp = temp - conjg(a(l+i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(kplus1,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           l = kplus1 - j
                           if (noconj) then
                               do i = max(1,j-k),j - 1
                                   temp = temp - a(l+i,j)*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/a(kplus1,j)
                           else
                               do i = max(1,j-k),j - 1
                                   temp = temp - conjg(a(l+i,j))*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/conjg(a(kplus1,j))
                           end if
                           x(jx) = temp
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = 1 - j
                           if (noconj) then
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - a(l+i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(1,j)
                           else
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - conjg(a(l+i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(1,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           l = 1 - j
                           if (noconj) then
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - a(l+i,j)*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/a(1,j)
                           else
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - conjg(a(l+i,j))*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/conjg(a(1,j))
                           end if
                           x(jx) = temp
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_ztbsv




     pure module subroutine stdlib_stpsv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_sp
     !! STPSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix, supplied in packed form.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: ap(*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: nounit
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_xerbla('STPSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with one pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk - 1
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*ap(k)
                                   k = k - 1
                               end do
                           end if
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk + 1
                               do i = j + 1,n
                                   x(i) = x(i) - temp*ap(k)
                                   k = k + 1
                               end do
                           end if
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           k = kk
                           do i = 1,j - 1
                               temp = temp - ap(k)*x(i)
                               k = k + 1
                           end do
                           if (nounit) temp = temp/ap(kk+j-1)
                           x(j) = temp
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           do k = kk,kk + j - 2
                               temp = temp - ap(k)*x(ix)
                               ix = ix + incx
                           end do
                           if (nounit) temp = temp/ap(kk+j-1)
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           k = kk
                           do i = n,j + 1,-1
                               temp = temp - ap(k)*x(i)
                               k = k - 1
                           end do
                           if (nounit) temp = temp/ap(kk-n+j)
                           x(j) = temp
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           do k = kk,kk - (n- (j+1)),-1
                               temp = temp - ap(k)*x(ix)
                               ix = ix - incx
                           end do
                           if (nounit) temp = temp/ap(kk-n+j)
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_stpsv

     pure module subroutine stdlib_dtpsv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_dp
     !! DTPSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix, supplied in packed form.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: ap(*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: nounit
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_xerbla('DTPSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with one pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk - 1
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*ap(k)
                                   k = k - 1
                               end do
                           end if
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk + 1
                               do i = j + 1,n
                                   x(i) = x(i) - temp*ap(k)
                                   k = k + 1
                               end do
                           end if
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           k = kk
                           do i = 1,j - 1
                               temp = temp - ap(k)*x(i)
                               k = k + 1
                           end do
                           if (nounit) temp = temp/ap(kk+j-1)
                           x(j) = temp
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           do k = kk,kk + j - 2
                               temp = temp - ap(k)*x(ix)
                               ix = ix + incx
                           end do
                           if (nounit) temp = temp/ap(kk+j-1)
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           k = kk
                           do i = n,j + 1,-1
                               temp = temp - ap(k)*x(i)
                               k = k - 1
                           end do
                           if (nounit) temp = temp/ap(kk-n+j)
                           x(j) = temp
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           do k = kk,kk - (n- (j+1)),-1
                               temp = temp - ap(k)*x(ix)
                               ix = ix - incx
                           end do
                           if (nounit) temp = temp/ap(kk-n+j)
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_dtpsv


     pure module subroutine stdlib_ctpsv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_sp
     !! CTPSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix, supplied in packed form.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_xerbla('CTPSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with cone pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk - 1
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*ap(k)
                                   k = k - 1
                               end do
                           end if
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk + 1
                               do i = j + 1,n
                                   x(i) = x(i) - temp*ap(k)
                                   k = k + 1
                               end do
                           end if
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x  or  x := inv( a**h )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           k = kk
                           if (noconj) then
                               do i = 1,j - 1
                                   temp = temp - ap(k)*x(i)
                                   k = k + 1
                               end do
                               if (nounit) temp = temp/ap(kk+j-1)
                           else
                               do i = 1,j - 1
                                   temp = temp - conjg(ap(k))*x(i)
                                   k = k + 1
                               end do
                               if (nounit) temp = temp/conjg(ap(kk+j-1))
                           end if
                           x(j) = temp
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           if (noconj) then
                               do k = kk,kk + j - 2
                                   temp = temp - ap(k)*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/ap(kk+j-1)
                           else
                               do k = kk,kk + j - 2
                                   temp = temp - conjg(ap(k))*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/conjg(ap(kk+j-1))
                           end if
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           k = kk
                           if (noconj) then
                               do i = n,j + 1,-1
                                   temp = temp - ap(k)*x(i)
                                   k = k - 1
                               end do
                               if (nounit) temp = temp/ap(kk-n+j)
                           else
                               do i = n,j + 1,-1
                                   temp = temp - conjg(ap(k))*x(i)
                                   k = k - 1
                               end do
                               if (nounit) temp = temp/conjg(ap(kk-n+j))
                           end if
                           x(j) = temp
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           if (noconj) then
                               do k = kk,kk - (n- (j+1)),-1
                                   temp = temp - ap(k)*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/ap(kk-n+j)
                           else
                               do k = kk,kk - (n- (j+1)),-1
                                   temp = temp - conjg(ap(k))*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/conjg(ap(kk-n+j))
                           end if
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_ctpsv

     pure module subroutine stdlib_ztpsv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_dp
     !! ZTPSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix, supplied in packed form.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_xerbla('ZTPSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with cone pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk - 1
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*ap(k)
                                   k = k - 1
                               end do
                           end if
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk + 1
                               do i = j + 1,n
                                   x(i) = x(i) - temp*ap(k)
                                   k = k + 1
                               end do
                           end if
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x  or  x := inv( a**h )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           k = kk
                           if (noconj) then
                               do i = 1,j - 1
                                   temp = temp - ap(k)*x(i)
                                   k = k + 1
                               end do
                               if (nounit) temp = temp/ap(kk+j-1)
                           else
                               do i = 1,j - 1
                                   temp = temp - conjg(ap(k))*x(i)
                                   k = k + 1
                               end do
                               if (nounit) temp = temp/conjg(ap(kk+j-1))
                           end if
                           x(j) = temp
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           if (noconj) then
                               do k = kk,kk + j - 2
                                   temp = temp - ap(k)*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/ap(kk+j-1)
                           else
                               do k = kk,kk + j - 2
                                   temp = temp - conjg(ap(k))*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/conjg(ap(kk+j-1))
                           end if
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           k = kk
                           if (noconj) then
                               do i = n,j + 1,-1
                                   temp = temp - ap(k)*x(i)
                                   k = k - 1
                               end do
                               if (nounit) temp = temp/ap(kk-n+j)
                           else
                               do i = n,j + 1,-1
                                   temp = temp - conjg(ap(k))*x(i)
                                   k = k - 1
                               end do
                               if (nounit) temp = temp/conjg(ap(kk-n+j))
                           end if
                           x(j) = temp
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           if (noconj) then
                               do k = kk,kk - (n- (j+1)),-1
                                   temp = temp - ap(k)*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/ap(kk-n+j)
                           else
                               do k = kk,kk - (n- (j+1)),-1
                                   temp = temp - conjg(ap(k))*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/conjg(ap(kk-n+j))
                           end if
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_ztpsv




     pure module subroutine stdlib_I64_strmv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! STRMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kx
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('STRMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               temp = x(j)
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do i = 1,j - 1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               temp = x(j)
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do i = n,j + 1,-1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (nounit) temp = temp*a(j,j)
                           do i = j - 1,1,-1
                               temp = temp + a(i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*a(j,j)
                           do i = j - 1,1,-1
                               ix = ix - incx
                               temp = temp + a(i,j)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (nounit) temp = temp*a(j,j)
                           do i = j + 1,n
                               temp = temp + a(i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*a(j,j)
                           do i = j + 1,n
                               ix = ix + incx
                               temp = temp + a(i,j)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_strmv

     pure module subroutine stdlib_I64_dtrmv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! DTRMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kx
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('DTRMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               temp = x(j)
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do i = 1,j - 1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               temp = x(j)
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do i = n,j + 1,-1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (nounit) temp = temp*a(j,j)
                           do i = j - 1,1,-1
                               temp = temp + a(i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*a(j,j)
                           do i = j - 1,1,-1
                               ix = ix - incx
                               temp = temp + a(i,j)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (nounit) temp = temp*a(j,j)
                           do i = j + 1,n
                               temp = temp + a(i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*a(j,j)
                           do i = j + 1,n
                               ix = ix + incx
                               temp = temp + a(i,j)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_dtrmv


     pure module subroutine stdlib_I64_ctrmv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! CTRMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CTRMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               temp = x(j)
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do i = 1,j - 1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               temp = x(j)
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do i = n,j + 1,-1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x  or  x := a**h*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j - 1,1,-1
                                   temp = temp + a(i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j - 1,1,-1
                                   temp = temp + conjg(a(i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   temp = temp + a(i,j)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   temp = temp + conjg(a(i,j))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j + 1,n
                                   temp = temp + a(i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j + 1,n
                                   temp = temp + conjg(a(i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j + 1,n
                                   ix = ix + incx
                                   temp = temp + a(i,j)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j + 1,n
                                   ix = ix + incx
                                   temp = temp + conjg(a(i,j))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_ctrmv

     pure module subroutine stdlib_I64_ztrmv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! ZTRMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZTRMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               temp = x(j)
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do i = 1,j - 1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               temp = x(j)
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*a(i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(j,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do i = n,j + 1,-1
                                   x(ix) = x(ix) + temp*a(i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(j,j)
                           end if
                           jx = jx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x  or  x := a**h*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j - 1,1,-1
                                   temp = temp + a(i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j - 1,1,-1
                                   temp = temp + conjg(a(i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   temp = temp + a(i,j)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   temp = temp + conjg(a(i,j))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j + 1,n
                                   temp = temp + a(i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j + 1,n
                                   temp = temp + conjg(a(i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*a(j,j)
                               do i = j + 1,n
                                   ix = ix + incx
                                   temp = temp + a(i,j)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(j,j))
                               do i = j + 1,n
                                   ix = ix + incx
                                   temp = temp + conjg(a(i,j))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_ztrmv




     pure module subroutine stdlib_I64_stbmv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! STBMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular band matrix, with ( k + 1 ) diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('STBMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx   too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
               ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               temp = x(j)
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(kplus1,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(kplus1,j)
                           end if
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               temp = x(j)
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(1,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(1,j)
                           end if
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = kplus1 - j
                           if (nounit) temp = temp*a(kplus1,j)
                           do i = j - 1,max(1,j-k),-1
                               temp = temp + a(l+i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           kx = kx - incx
                           ix = kx
                           l = kplus1 - j
                           if (nounit) temp = temp*a(kplus1,j)
                           do i = j - 1,max(1,j-k),-1
                               temp = temp + a(l+i,j)*x(ix)
                               ix = ix - incx
                           end do
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = 1 - j
                           if (nounit) temp = temp*a(1,j)
                           do i = j + 1,min(n,j+k)
                               temp = temp + a(l+i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           kx = kx + incx
                           ix = kx
                           l = 1 - j
                           if (nounit) temp = temp*a(1,j)
                           do i = j + 1,min(n,j+k)
                               temp = temp + a(l+i,j)*x(ix)
                               ix = ix + incx
                           end do
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_stbmv

     pure module subroutine stdlib_I64_dtbmv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! DTBMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular band matrix, with ( k + 1 ) diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('DTBMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx   too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
               ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               temp = x(j)
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(kplus1,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(kplus1,j)
                           end if
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               temp = x(j)
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(1,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(1,j)
                           end if
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = kplus1 - j
                           if (nounit) temp = temp*a(kplus1,j)
                           do i = j - 1,max(1,j-k),-1
                               temp = temp + a(l+i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           kx = kx - incx
                           ix = kx
                           l = kplus1 - j
                           if (nounit) temp = temp*a(kplus1,j)
                           do i = j - 1,max(1,j-k),-1
                               temp = temp + a(l+i,j)*x(ix)
                               ix = ix - incx
                           end do
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = 1 - j
                           if (nounit) temp = temp*a(1,j)
                           do i = j + 1,min(n,j+k)
                               temp = temp + a(l+i,j)*x(i)
                           end do
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           kx = kx + incx
                           ix = kx
                           l = 1 - j
                           if (nounit) temp = temp*a(1,j)
                           do i = j + 1,min(n,j+k)
                               temp = temp + a(l+i,j)*x(ix)
                               ix = ix + incx
                           end do
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_dtbmv


     pure module subroutine stdlib_I64_ctbmv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! CTBMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular band matrix, with ( k + 1 ) diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CTBMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx   too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
               ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               temp = x(j)
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(kplus1,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(kplus1,j)
                           end if
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               temp = x(j)
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(1,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(1,j)
                           end if
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x  or  x := a**h*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = kplus1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(kplus1,j)
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + a(l+i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(kplus1,j))
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + conjg(a(l+i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           kx = kx - incx
                           ix = kx
                           l = kplus1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(kplus1,j)
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + a(l+i,j)*x(ix)
                                   ix = ix - incx
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(kplus1,j))
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + conjg(a(l+i,j))*x(ix)
                                   ix = ix - incx
                               end do
                           end if
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = 1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(1,j)
                               do i = j + 1,min(n,j+k)
                                   temp = temp + a(l+i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(1,j))
                               do i = j + 1,min(n,j+k)
                                   temp = temp + conjg(a(l+i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           kx = kx + incx
                           ix = kx
                           l = 1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(1,j)
                               do i = j + 1,min(n,j+k)
                                   temp = temp + a(l+i,j)*x(ix)
                                   ix = ix + incx
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(1,j))
                               do i = j + 1,min(n,j+k)
                                   temp = temp + conjg(a(l+i,j))*x(ix)
                                   ix = ix + incx
                               end do
                           end if
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_ctbmv

     pure module subroutine stdlib_I64_ztbmv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! ZTBMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular band matrix, with ( k + 1 ) diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZTBMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx   too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
               ! form  x := a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               temp = x(j)
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(kplus1,j)
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               l = kplus1 - j
                               do i = max(1,j-k),j - 1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(kplus1,j)
                           end if
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               temp = x(j)
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(i) = x(i) + temp*a(l+i,j)
                               end do
                               if (nounit) x(j) = x(j)*a(1,j)
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               l = 1 - j
                               do i = min(n,j+k),j + 1,-1
                                   x(ix) = x(ix) + temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*a(1,j)
                           end if
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x  or  x := a**h*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = kplus1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(kplus1,j)
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + a(l+i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(kplus1,j))
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + conjg(a(l+i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           kx = kx - incx
                           ix = kx
                           l = kplus1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(kplus1,j)
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + a(l+i,j)*x(ix)
                                   ix = ix - incx
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(kplus1,j))
                               do i = j - 1,max(1,j-k),-1
                                   temp = temp + conjg(a(l+i,j))*x(ix)
                                   ix = ix - incx
                               end do
                           end if
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = 1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(1,j)
                               do i = j + 1,min(n,j+k)
                                   temp = temp + a(l+i,j)*x(i)
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(1,j))
                               do i = j + 1,min(n,j+k)
                                   temp = temp + conjg(a(l+i,j))*x(i)
                               end do
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           kx = kx + incx
                           ix = kx
                           l = 1 - j
                           if (noconj) then
                               if (nounit) temp = temp*a(1,j)
                               do i = j + 1,min(n,j+k)
                                   temp = temp + a(l+i,j)*x(ix)
                                   ix = ix + incx
                               end do
                           else
                               if (nounit) temp = temp*conjg(a(1,j))
                               do i = j + 1,min(n,j+k)
                                   temp = temp + conjg(a(l+i,j))*x(ix)
                                   ix = ix + incx
                               end do
                           end if
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_ztbmv




     pure module subroutine stdlib_I64_stpmv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_sp
     !! STPMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: ap(*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: nounit
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('STPMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with one pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x:= a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               temp = x(j)
                               k = kk
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k + 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk+j-1)
                           end if
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk + j - 2
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk+j-1)
                           end if
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               temp = x(j)
                               k = kk
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k - 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk-n+j)
                           end if
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk - (n- (j+1)),-1
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk-n+j)
                           end if
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (nounit) temp = temp*ap(kk)
                           k = kk - 1
                           do i = j - 1,1,-1
                               temp = temp + ap(k)*x(i)
                               k = k - 1
                           end do
                           x(j) = temp
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*ap(kk)
                           do k = kk - 1,kk - j + 1,-1
                               ix = ix - incx
                               temp = temp + ap(k)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (nounit) temp = temp*ap(kk)
                           k = kk + 1
                           do i = j + 1,n
                               temp = temp + ap(k)*x(i)
                               k = k + 1
                           end do
                           x(j) = temp
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*ap(kk)
                           do k = kk + 1,kk + n - j
                               ix = ix + incx
                               temp = temp + ap(k)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_stpmv

     pure module subroutine stdlib_I64_dtpmv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_dp
     !! DTPMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: ap(*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: nounit
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('DTPMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with one pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x:= a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               temp = x(j)
                               k = kk
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k + 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk+j-1)
                           end if
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk + j - 2
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk+j-1)
                           end if
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               temp = x(j)
                               k = kk
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k - 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk-n+j)
                           end if
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk - (n- (j+1)),-1
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk-n+j)
                           end if
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (nounit) temp = temp*ap(kk)
                           k = kk - 1
                           do i = j - 1,1,-1
                               temp = temp + ap(k)*x(i)
                               k = k - 1
                           end do
                           x(j) = temp
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*ap(kk)
                           do k = kk - 1,kk - j + 1,-1
                               ix = ix - incx
                               temp = temp + ap(k)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (nounit) temp = temp*ap(kk)
                           k = kk + 1
                           do i = j + 1,n
                               temp = temp + ap(k)*x(i)
                               k = k + 1
                           end do
                           x(j) = temp
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (nounit) temp = temp*ap(kk)
                           do k = kk + 1,kk + n - j
                               ix = ix + incx
                               temp = temp + ap(k)*x(ix)
                           end do
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_dtpmv


     pure module subroutine stdlib_I64_ctpmv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_sp
     !! CTPMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CTPMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with cone pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x:= a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               temp = x(j)
                               k = kk
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k + 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk+j-1)
                           end if
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk + j - 2
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk+j-1)
                           end if
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               temp = x(j)
                               k = kk
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k - 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk-n+j)
                           end if
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk - (n- (j+1)),-1
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk-n+j)
                           end if
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x  or  x := a**h*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           k = kk - 1
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do i = j - 1,1,-1
                                   temp = temp + ap(k)*x(i)
                                   k = k - 1
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do i = j - 1,1,-1
                                   temp = temp + conjg(ap(k))*x(i)
                                   k = k - 1
                               end do
                           end if
                           x(j) = temp
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   temp = temp + ap(k)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   temp = temp + conjg(ap(k))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           k = kk + 1
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do i = j + 1,n
                                   temp = temp + ap(k)*x(i)
                                   k = k + 1
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do i = j + 1,n
                                   temp = temp + conjg(ap(k))*x(i)
                                   k = k + 1
                               end do
                           end if
                           x(j) = temp
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   temp = temp + ap(k)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   temp = temp + conjg(ap(k))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_ctpmv

     pure module subroutine stdlib_I64_ztpmv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_dp
     !! ZTPMV performs one of the matrix-vector operations
     !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
     !! where x is an n element vector and  A is an n by n unit, or non-unit,
     !! upper or lower triangular matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZTPMV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with cone pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x:= a*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               temp = x(j)
                               k = kk
                               do i = 1,j - 1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k + 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk+j-1)
                           end if
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk + j - 2
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix + incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk+j-1)
                           end if
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               temp = x(j)
                               k = kk
                               do i = n,j + 1,-1
                                   x(i) = x(i) + temp*ap(k)
                                   k = k - 1
                               end do
                               if (nounit) x(j) = x(j)*ap(kk-n+j)
                           end if
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               temp = x(jx)
                               ix = kx
                               do k = kk,kk - (n- (j+1)),-1
                                   x(ix) = x(ix) + temp*ap(k)
                                   ix = ix - incx
                               end do
                               if (nounit) x(jx) = x(jx)*ap(kk-n+j)
                           end if
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := a**t*x  or  x := a**h*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           k = kk - 1
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do i = j - 1,1,-1
                                   temp = temp + ap(k)*x(i)
                                   k = k - 1
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do i = j - 1,1,-1
                                   temp = temp + conjg(ap(k))*x(i)
                                   k = k - 1
                               end do
                           end if
                           x(j) = temp
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   temp = temp + ap(k)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   temp = temp + conjg(ap(k))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           k = kk + 1
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do i = j + 1,n
                                   temp = temp + ap(k)*x(i)
                                   k = k + 1
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do i = j + 1,n
                                   temp = temp + conjg(ap(k))*x(i)
                                   k = k + 1
                               end do
                           end if
                           x(j) = temp
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = jx
                           if (noconj) then
                               if (nounit) temp = temp*ap(kk)
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   temp = temp + ap(k)*x(ix)
                               end do
                           else
                               if (nounit) temp = temp*conjg(ap(kk))
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   temp = temp + conjg(ap(k))*x(ix)
                               end do
                           end if
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_ztpmv




     pure module subroutine stdlib_I64_strsv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! STRSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kx
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('STRSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j + 1,n
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j + 1,n
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           do i = 1,j - 1
                               temp = temp - a(i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           do i = 1,j - 1
                               temp = temp - a(i,j)*x(ix)
                               ix = ix + incx
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           do i = n,j + 1,-1
                               temp = temp - a(i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           do i = n,j + 1,-1
                               temp = temp - a(i,j)*x(ix)
                               ix = ix - incx
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_strsv

     pure module subroutine stdlib_I64_dtrsv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! DTRSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kx
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('DTRSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j + 1,n
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j + 1,n
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           do i = 1,j - 1
                               temp = temp - a(i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           do i = 1,j - 1
                               temp = temp - a(i,j)*x(ix)
                               ix = ix + incx
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           do i = n,j + 1,-1
                               temp = temp - a(i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           do i = n,j + 1,-1
                               temp = temp - a(i,j)*x(ix)
                               ix = ix - incx
                           end do
                           if (nounit) temp = temp/a(j,j)
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_dtrsv


     pure module subroutine stdlib_I64_ctrsv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! CTRSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CTRSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j + 1,n
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j + 1,n
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x  or  x := inv( a**h )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (noconj) then
                               do i = 1,j - 1
                                   temp = temp - a(i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = 1,j - 1
                                   temp = temp - conjg(a(i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           ix = kx
                           temp = x(jx)
                           if (noconj) then
                               do i = 1,j - 1
                                   temp = temp - a(i,j)*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = 1,j - 1
                                   temp = temp - conjg(a(i,j))*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (noconj) then
                               do i = n,j + 1,-1
                                   temp = temp - a(i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = n,j + 1,-1
                                   temp = temp - conjg(a(i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           ix = kx
                           temp = x(jx)
                           if (noconj) then
                               do i = n,j + 1,-1
                                   temp = temp - a(i,j)*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = n,j + 1,-1
                                   temp = temp - conjg(a(i,j))*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_ctrsv

     pure module subroutine stdlib_I64_ztrsv(uplo,trans,diag,n,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! ZTRSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,n)) then
               info = 6
           else if (incx==0) then
               info = 8
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZTRSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j - 1,1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/a(j,j)
                               temp = x(j)
                               do i = j + 1,n
                                   x(i) = x(i) - temp*a(i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/a(j,j)
                               temp = x(jx)
                               ix = jx
                               do i = j + 1,n
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*a(i,j)
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x  or  x := inv( a**h )*x.
               if (stdlib_lsame(uplo,'U')) then
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           if (noconj) then
                               do i = 1,j - 1
                                   temp = temp - a(i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = 1,j - 1
                                   temp = temp - conjg(a(i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           ix = kx
                           temp = x(jx)
                           if (noconj) then
                               do i = 1,j - 1
                                   temp = temp - a(i,j)*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = 1,j - 1
                                   temp = temp - conjg(a(i,j))*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(jx) = temp
                           jx = jx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           if (noconj) then
                               do i = n,j + 1,-1
                                   temp = temp - a(i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = n,j + 1,-1
                                   temp = temp - conjg(a(i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           ix = kx
                           temp = x(jx)
                           if (noconj) then
                               do i = n,j + 1,-1
                                   temp = temp - a(i,j)*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/a(j,j)
                           else
                               do i = n,j + 1,-1
                                   temp = temp - conjg(a(i,j))*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/conjg(a(j,j))
                           end if
                           x(jx) = temp
                           jx = jx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_ztrsv




     pure module subroutine stdlib_I64_stbsv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! STBSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular band matrix, with ( k + 1 )
     !! diagonals.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('STBSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed by sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               l = kplus1 - j
                               if (nounit) x(j) = x(j)/a(kplus1,j)
                               temp = x(j)
                               do i = j - 1,max(1,j-k),-1
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           kx = kx - incx
                           if (x(jx)/=zero) then
                               ix = kx
                               l = kplus1 - j
                               if (nounit) x(jx) = x(jx)/a(kplus1,j)
                               temp = x(jx)
                               do i = j - 1,max(1,j-k),-1
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               l = 1 - j
                               if (nounit) x(j) = x(j)/a(1,j)
                               temp = x(j)
                               do i = j + 1,min(n,j+k)
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           kx = kx + incx
                           if (x(jx)/=zero) then
                               ix = kx
                               l = 1 - j
                               if (nounit) x(jx) = x(jx)/a(1,j)
                               temp = x(jx)
                               do i = j + 1,min(n,j+k)
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t)*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = kplus1 - j
                           do i = max(1,j-k),j - 1
                               temp = temp - a(l+i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(kplus1,j)
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           l = kplus1 - j
                           do i = max(1,j-k),j - 1
                               temp = temp - a(l+i,j)*x(ix)
                               ix = ix + incx
                           end do
                           if (nounit) temp = temp/a(kplus1,j)
                           x(jx) = temp
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = 1 - j
                           do i = min(n,j+k),j + 1,-1
                               temp = temp - a(l+i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(1,j)
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           l = 1 - j
                           do i = min(n,j+k),j + 1,-1
                               temp = temp - a(l+i,j)*x(ix)
                               ix = ix - incx
                           end do
                           if (nounit) temp = temp/a(1,j)
                           x(jx) = temp
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_stbsv

     pure module subroutine stdlib_I64_dtbsv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! DTBSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular band matrix, with ( k + 1 )
     !! diagonals.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: nounit
           ! Intrinsic Functions 
           intrinsic :: max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('DTBSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed by sequentially with one pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               l = kplus1 - j
                               if (nounit) x(j) = x(j)/a(kplus1,j)
                               temp = x(j)
                               do i = j - 1,max(1,j-k),-1
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           kx = kx - incx
                           if (x(jx)/=zero) then
                               ix = kx
                               l = kplus1 - j
                               if (nounit) x(jx) = x(jx)/a(kplus1,j)
                               temp = x(jx)
                               do i = j - 1,max(1,j-k),-1
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               l = 1 - j
                               if (nounit) x(j) = x(j)/a(1,j)
                               temp = x(j)
                               do i = j + 1,min(n,j+k)
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           kx = kx + incx
                           if (x(jx)/=zero) then
                               ix = kx
                               l = 1 - j
                               if (nounit) x(jx) = x(jx)/a(1,j)
                               temp = x(jx)
                               do i = j + 1,min(n,j+k)
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t)*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = kplus1 - j
                           do i = max(1,j-k),j - 1
                               temp = temp - a(l+i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(kplus1,j)
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           l = kplus1 - j
                           do i = max(1,j-k),j - 1
                               temp = temp - a(l+i,j)*x(ix)
                               ix = ix + incx
                           end do
                           if (nounit) temp = temp/a(kplus1,j)
                           x(jx) = temp
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = 1 - j
                           do i = min(n,j+k),j + 1,-1
                               temp = temp - a(l+i,j)*x(i)
                           end do
                           if (nounit) temp = temp/a(1,j)
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           l = 1 - j
                           do i = min(n,j+k),j + 1,-1
                               temp = temp - a(l+i,j)*x(ix)
                               ix = ix - incx
                           end do
                           if (nounit) temp = temp/a(1,j)
                           x(jx) = temp
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_dtbsv


     pure module subroutine stdlib_I64_ctbsv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_sp
     !! CTBSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular band matrix, with ( k + 1 )
     !! diagonals.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CTBSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed by sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               l = kplus1 - j
                               if (nounit) x(j) = x(j)/a(kplus1,j)
                               temp = x(j)
                               do i = j - 1,max(1,j-k),-1
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           kx = kx - incx
                           if (x(jx)/=czero) then
                               ix = kx
                               l = kplus1 - j
                               if (nounit) x(jx) = x(jx)/a(kplus1,j)
                               temp = x(jx)
                               do i = j - 1,max(1,j-k),-1
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               l = 1 - j
                               if (nounit) x(j) = x(j)/a(1,j)
                               temp = x(j)
                               do i = j + 1,min(n,j+k)
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           kx = kx + incx
                           if (x(jx)/=czero) then
                               ix = kx
                               l = 1 - j
                               if (nounit) x(jx) = x(jx)/a(1,j)
                               temp = x(jx)
                               do i = j + 1,min(n,j+k)
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x  or  x := inv( a**h )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = kplus1 - j
                           if (noconj) then
                               do i = max(1,j-k),j - 1
                                   temp = temp - a(l+i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(kplus1,j)
                           else
                               do i = max(1,j-k),j - 1
                                   temp = temp - conjg(a(l+i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(kplus1,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           l = kplus1 - j
                           if (noconj) then
                               do i = max(1,j-k),j - 1
                                   temp = temp - a(l+i,j)*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/a(kplus1,j)
                           else
                               do i = max(1,j-k),j - 1
                                   temp = temp - conjg(a(l+i,j))*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/conjg(a(kplus1,j))
                           end if
                           x(jx) = temp
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = 1 - j
                           if (noconj) then
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - a(l+i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(1,j)
                           else
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - conjg(a(l+i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(1,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           l = 1 - j
                           if (noconj) then
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - a(l+i,j)*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/a(1,j)
                           else
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - conjg(a(l+i,j))*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/conjg(a(1,j))
                           end if
                           x(jx) = temp
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_ctbsv

     pure module subroutine stdlib_I64_ztbsv(uplo,trans,diag,n,k,a,lda,x,incx)
     use stdlib_blas_constants_dp
     !! ZTBSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular band matrix, with ( k + 1 )
     !! diagonals.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kplus1, kx, l
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda< (k+1)) then
               info = 7
           else if (incx==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZTBSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed by sequentially with cone pass through a.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               l = kplus1 - j
                               if (nounit) x(j) = x(j)/a(kplus1,j)
                               temp = x(j)
                               do i = j - 1,max(1,j-k),-1
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           kx = kx - incx
                           if (x(jx)/=czero) then
                               ix = kx
                               l = kplus1 - j
                               if (nounit) x(jx) = x(jx)/a(kplus1,j)
                               temp = x(jx)
                               do i = j - 1,max(1,j-k),-1
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix - incx
                               end do
                           end if
                           jx = jx - incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               l = 1 - j
                               if (nounit) x(j) = x(j)/a(1,j)
                               temp = x(j)
                               do i = j + 1,min(n,j+k)
                                   x(i) = x(i) - temp*a(l+i,j)
                               end do
                           end if
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           kx = kx + incx
                           if (x(jx)/=czero) then
                               ix = kx
                               l = 1 - j
                               if (nounit) x(jx) = x(jx)/a(1,j)
                               temp = x(jx)
                               do i = j + 1,min(n,j+k)
                                   x(ix) = x(ix) - temp*a(l+i,j)
                                   ix = ix + incx
                               end do
                           end if
                           jx = jx + incx
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x  or  x := inv( a**h )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kplus1 = k + 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           l = kplus1 - j
                           if (noconj) then
                               do i = max(1,j-k),j - 1
                                   temp = temp - a(l+i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(kplus1,j)
                           else
                               do i = max(1,j-k),j - 1
                                   temp = temp - conjg(a(l+i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(kplus1,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           l = kplus1 - j
                           if (noconj) then
                               do i = max(1,j-k),j - 1
                                   temp = temp - a(l+i,j)*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/a(kplus1,j)
                           else
                               do i = max(1,j-k),j - 1
                                   temp = temp - conjg(a(l+i,j))*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/conjg(a(kplus1,j))
                           end if
                           x(jx) = temp
                           jx = jx + incx
                           if (j>k) kx = kx + incx
                       end do
                   end if
               else
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           l = 1 - j
                           if (noconj) then
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - a(l+i,j)*x(i)
                               end do
                               if (nounit) temp = temp/a(1,j)
                           else
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - conjg(a(l+i,j))*x(i)
                               end do
                               if (nounit) temp = temp/conjg(a(1,j))
                           end if
                           x(j) = temp
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           l = 1 - j
                           if (noconj) then
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - a(l+i,j)*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/a(1,j)
                           else
                               do i = min(n,j+k),j + 1,-1
                                   temp = temp - conjg(a(l+i,j))*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/conjg(a(1,j))
                           end if
                           x(jx) = temp
                           jx = jx - incx
                           if ((n-j)>=k) kx = kx - incx
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_ztbsv




     pure module subroutine stdlib_I64_stpsv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_sp
     !! STPSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix, supplied in packed form.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: ap(*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: nounit
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('STPSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with one pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk - 1
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*ap(k)
                                   k = k - 1
                               end do
                           end if
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk + 1
                               do i = j + 1,n
                                   x(i) = x(i) - temp*ap(k)
                                   k = k + 1
                               end do
                           end if
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           k = kk
                           do i = 1,j - 1
                               temp = temp - ap(k)*x(i)
                               k = k + 1
                           end do
                           if (nounit) temp = temp/ap(kk+j-1)
                           x(j) = temp
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           do k = kk,kk + j - 2
                               temp = temp - ap(k)*x(ix)
                               ix = ix + incx
                           end do
                           if (nounit) temp = temp/ap(kk+j-1)
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           k = kk
                           do i = n,j + 1,-1
                               temp = temp - ap(k)*x(i)
                               k = k - 1
                           end do
                           if (nounit) temp = temp/ap(kk-n+j)
                           x(j) = temp
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           do k = kk,kk - (n- (j+1)),-1
                               temp = temp - ap(k)*x(ix)
                               ix = ix - incx
                           end do
                           if (nounit) temp = temp/ap(kk-n+j)
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_stpsv

     pure module subroutine stdlib_I64_dtpsv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_dp
     !! DTPSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix, supplied in packed form.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: ap(*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: nounit
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('DTPSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with one pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk - 1
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*ap(k)
                                   k = k - 1
                               end do
                           end if
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=zero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk + 1
                               do i = j + 1,n
                                   x(i) = x(i) - temp*ap(k)
                                   k = k + 1
                               end do
                           end if
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=zero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           k = kk
                           do i = 1,j - 1
                               temp = temp - ap(k)*x(i)
                               k = k + 1
                           end do
                           if (nounit) temp = temp/ap(kk+j-1)
                           x(j) = temp
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           do k = kk,kk + j - 2
                               temp = temp - ap(k)*x(ix)
                               ix = ix + incx
                           end do
                           if (nounit) temp = temp/ap(kk+j-1)
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           k = kk
                           do i = n,j + 1,-1
                               temp = temp - ap(k)*x(i)
                               k = k - 1
                           end do
                           if (nounit) temp = temp/ap(kk-n+j)
                           x(j) = temp
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           do k = kk,kk - (n- (j+1)),-1
                               temp = temp - ap(k)*x(ix)
                               ix = ix - incx
                           end do
                           if (nounit) temp = temp/ap(kk-n+j)
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_dtpsv


     pure module subroutine stdlib_I64_ctpsv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_sp
     !! CTPSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix, supplied in packed form.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CTPSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with cone pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk - 1
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*ap(k)
                                   k = k - 1
                               end do
                           end if
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk + 1
                               do i = j + 1,n
                                   x(i) = x(i) - temp*ap(k)
                                   k = k + 1
                               end do
                           end if
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x  or  x := inv( a**h )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           k = kk
                           if (noconj) then
                               do i = 1,j - 1
                                   temp = temp - ap(k)*x(i)
                                   k = k + 1
                               end do
                               if (nounit) temp = temp/ap(kk+j-1)
                           else
                               do i = 1,j - 1
                                   temp = temp - conjg(ap(k))*x(i)
                                   k = k + 1
                               end do
                               if (nounit) temp = temp/conjg(ap(kk+j-1))
                           end if
                           x(j) = temp
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           if (noconj) then
                               do k = kk,kk + j - 2
                                   temp = temp - ap(k)*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/ap(kk+j-1)
                           else
                               do k = kk,kk + j - 2
                                   temp = temp - conjg(ap(k))*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/conjg(ap(kk+j-1))
                           end if
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           k = kk
                           if (noconj) then
                               do i = n,j + 1,-1
                                   temp = temp - ap(k)*x(i)
                                   k = k - 1
                               end do
                               if (nounit) temp = temp/ap(kk-n+j)
                           else
                               do i = n,j + 1,-1
                                   temp = temp - conjg(ap(k))*x(i)
                                   k = k - 1
                               end do
                               if (nounit) temp = temp/conjg(ap(kk-n+j))
                           end if
                           x(j) = temp
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           if (noconj) then
                               do k = kk,kk - (n- (j+1)),-1
                                   temp = temp - ap(k)*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/ap(kk-n+j)
                           else
                               do k = kk,kk - (n- (j+1)),-1
                                   temp = temp - conjg(ap(k))*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/conjg(ap(kk-n+j))
                           end if
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_ctpsv

     pure module subroutine stdlib_I64_ztpsv(uplo,trans,diag,n,ap,x,incx)
     use stdlib_blas_constants_dp
     !! ZTPSV solves one of the systems of equations
     !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
     !! where b and x are n element vectors and A is an n by n unit, or
     !! non-unit, upper or lower triangular matrix, supplied in packed form.
     !! No test for singularity or near-singularity is included in this
     !! routine. Such tests must be performed before calling this routine.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           logical(lk) :: noconj, nounit
           ! Intrinsic Functions 
           intrinsic :: conjg
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 2
           else if (.not.stdlib_lsame(diag,'U') .and. .not.stdlib_lsame(diag,'N')) then
               info = 3
           else if (n<0) then
               info = 4
           else if (incx==0) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZTPSV ',info)
               return
           end if
           ! quick return if possible.
           if (n==0) return
           noconj = stdlib_lsame(trans,'T')
           nounit = stdlib_lsame(diag,'N')
           ! set up the start point in x if the increment is not unity. this
           ! will be  ( n - 1 )*incx  too small for descending loops.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of ap are
           ! accessed sequentially with cone pass through ap.
           if (stdlib_lsame(trans,'N')) then
              ! form  x := inv( a )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk - 1
                               do i = j - 1,1,-1
                                   x(i) = x(i) - temp*ap(k)
                                   k = k - 1
                               end do
                           end if
                           kk = kk - j
                       end do
                   else
                       jx = kx + (n-1)*incx
                       do j = n,1,-1
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk - 1,kk - j + 1,-1
                                   ix = ix - incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx - incx
                           kk = kk - j
                       end do
                   end if
               else
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           if (x(j)/=czero) then
                               if (nounit) x(j) = x(j)/ap(kk)
                               temp = x(j)
                               k = kk + 1
                               do i = j + 1,n
                                   x(i) = x(i) - temp*ap(k)
                                   k = k + 1
                               end do
                           end if
                           kk = kk + (n-j+1)
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           if (x(jx)/=czero) then
                               if (nounit) x(jx) = x(jx)/ap(kk)
                               temp = x(jx)
                               ix = jx
                               do k = kk + 1,kk + n - j
                                   ix = ix + incx
                                   x(ix) = x(ix) - temp*ap(k)
                               end do
                           end if
                           jx = jx + incx
                           kk = kk + (n-j+1)
                       end do
                   end if
               end if
           else
              ! form  x := inv( a**t )*x  or  x := inv( a**h )*x.
               if (stdlib_lsame(uplo,'U')) then
                   kk = 1
                   if (incx==1) then
                       do j = 1,n
                           temp = x(j)
                           k = kk
                           if (noconj) then
                               do i = 1,j - 1
                                   temp = temp - ap(k)*x(i)
                                   k = k + 1
                               end do
                               if (nounit) temp = temp/ap(kk+j-1)
                           else
                               do i = 1,j - 1
                                   temp = temp - conjg(ap(k))*x(i)
                                   k = k + 1
                               end do
                               if (nounit) temp = temp/conjg(ap(kk+j-1))
                           end if
                           x(j) = temp
                           kk = kk + j
                       end do
                   else
                       jx = kx
                       do j = 1,n
                           temp = x(jx)
                           ix = kx
                           if (noconj) then
                               do k = kk,kk + j - 2
                                   temp = temp - ap(k)*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/ap(kk+j-1)
                           else
                               do k = kk,kk + j - 2
                                   temp = temp - conjg(ap(k))*x(ix)
                                   ix = ix + incx
                               end do
                               if (nounit) temp = temp/conjg(ap(kk+j-1))
                           end if
                           x(jx) = temp
                           jx = jx + incx
                           kk = kk + j
                       end do
                   end if
               else
                   kk = (n* (n+1))/2
                   if (incx==1) then
                       do j = n,1,-1
                           temp = x(j)
                           k = kk
                           if (noconj) then
                               do i = n,j + 1,-1
                                   temp = temp - ap(k)*x(i)
                                   k = k - 1
                               end do
                               if (nounit) temp = temp/ap(kk-n+j)
                           else
                               do i = n,j + 1,-1
                                   temp = temp - conjg(ap(k))*x(i)
                                   k = k - 1
                               end do
                               if (nounit) temp = temp/conjg(ap(kk-n+j))
                           end if
                           x(j) = temp
                           kk = kk - (n-j+1)
                       end do
                   else
                       kx = kx + (n-1)*incx
                       jx = kx
                       do j = n,1,-1
                           temp = x(jx)
                           ix = kx
                           if (noconj) then
                               do k = kk,kk - (n- (j+1)),-1
                                   temp = temp - ap(k)*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/ap(kk-n+j)
                           else
                               do k = kk,kk - (n- (j+1)),-1
                                   temp = temp - conjg(ap(k))*x(ix)
                                   ix = ix - incx
                               end do
                               if (nounit) temp = temp/conjg(ap(kk-n+j))
                           end if
                           x(jx) = temp
                           jx = jx - incx
                           kk = kk - (n-j+1)
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_I64_ztpsv



end submodule stdlib_blas_level2_tri
