submodule(stdlib_blas) stdlib_blas_level2_gen
  implicit none


  contains

     pure module subroutine stdlib_sgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! SGEMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (lda<max(1,m)) then
               info = 6
           else if (incx==0) then
               info = 8
           else if (incy==0) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('SGEMV ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==zero).and. (beta==one))) return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if (stdlib_lsame(trans,'N')) then
               lenx = n
               leny = m
           else
               lenx = m
               leny = n
           end if
           if (incx>0) then
               kx = 1
           else
               kx = 1 - (lenx-1)*incx
           end if
           if (incy>0) then
               ky = 1
           else
               ky = 1 - (leny-1)*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           ! first form  y := beta*y.
           if (beta/=one) then
               if (incy==1) then
                   if (beta==zero) then
                       do i = 1,leny
                           y(i) = zero
                       end do
                   else
                       do i = 1,leny
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==zero) then
                       do i = 1,leny
                           y(iy) = zero
                           iy = iy + incy
                       end do
                   else
                       do i = 1,leny
                           y(iy) = beta*y(iy)
                           iy = iy + incy
                       end do
                   end if
               end if
           end if
           if (alpha==zero) return
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       do i = 1,m
                           y(i) = y(i) + temp*a(i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       do i = 1,m
                           y(iy) = y(iy) + temp*a(i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = zero
                       do i = 1,m
                           temp = temp + a(i,j)*x(i)
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = zero
                       ix = kx
                       do i = 1,m
                           temp = temp + a(i,j)*x(ix)
                           ix = ix + incx
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_sgemv

     pure module subroutine stdlib_dgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! DGEMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (lda<max(1,m)) then
               info = 6
           else if (incx==0) then
               info = 8
           else if (incy==0) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('DGEMV ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==zero).and. (beta==one))) return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if (stdlib_lsame(trans,'N')) then
               lenx = n
               leny = m
           else
               lenx = m
               leny = n
           end if
           if (incx>0) then
               kx = 1
           else
               kx = 1 - (lenx-1)*incx
           end if
           if (incy>0) then
               ky = 1
           else
               ky = 1 - (leny-1)*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           ! first form  y := beta*y.
           if (beta/=one) then
               if (incy==1) then
                   if (beta==zero) then
                       do i = 1,leny
                           y(i) = zero
                       end do
                   else
                       do i = 1,leny
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==zero) then
                       do i = 1,leny
                           y(iy) = zero
                           iy = iy + incy
                       end do
                   else
                       do i = 1,leny
                           y(iy) = beta*y(iy)
                           iy = iy + incy
                       end do
                   end if
               end if
           end if
           if (alpha==zero) return
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       do i = 1,m
                           y(i) = y(i) + temp*a(i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       do i = 1,m
                           y(iy) = y(iy) + temp*a(i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = zero
                       do i = 1,m
                           temp = temp + a(i,j)*x(i)
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = zero
                       ix = kx
                       do i = 1,m
                           temp = temp + a(i,j)*x(ix)
                           ix = ix + incx
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_dgemv


     pure module subroutine stdlib_cgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! CGEMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
     !! y := alpha*A**H*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
           logical(lk) :: noconj
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (lda<max(1,m)) then
               info = 6
           else if (incx==0) then
               info = 8
           else if (incy==0) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('CGEMV ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==czero).and. (beta==cone))) return
           noconj = stdlib_lsame(trans,'T')
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if (stdlib_lsame(trans,'N')) then
               lenx = n
               leny = m
           else
               lenx = m
               leny = n
           end if
           if (incx>0) then
               kx = 1
           else
               kx = 1 - (lenx-1)*incx
           end if
           if (incy>0) then
               ky = 1
           else
               ky = 1 - (leny-1)*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           ! first form  y := beta*y.
           if (beta/=cone) then
               if (incy==1) then
                   if (beta==czero) then
                       do i = 1,leny
                           y(i) = czero
                       end do
                   else
                       do i = 1,leny
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==czero) then
                       do i = 1,leny
                           y(iy) = czero
                           iy = iy + incy
                       end do
                   else
                       do i = 1,leny
                           y(iy) = beta*y(iy)
                           iy = iy + incy
                       end do
                   end if
               end if
           end if
           if (alpha==czero) return
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       do i = 1,m
                           y(i) = y(i) + temp*a(i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       do i = 1,m
                           y(iy) = y(iy) + temp*a(i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y  or  y := alpha*a**h*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = czero
                       if (noconj) then
                           do i = 1,m
                               temp = temp + a(i,j)*x(i)
                           end do
                       else
                           do i = 1,m
                               temp = temp + conjg(a(i,j))*x(i)
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = czero
                       ix = kx
                       if (noconj) then
                           do i = 1,m
                               temp = temp + a(i,j)*x(ix)
                               ix = ix + incx
                           end do
                       else
                           do i = 1,m
                               temp = temp + conjg(a(i,j))*x(ix)
                               ix = ix + incx
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_cgemv

     pure module subroutine stdlib_zgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! ZGEMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
     !! y := alpha*A**H*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
           logical(lk) :: noconj
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (lda<max(1,m)) then
               info = 6
           else if (incx==0) then
               info = 8
           else if (incy==0) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('ZGEMV ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==czero).and. (beta==cone))) return
           noconj = stdlib_lsame(trans,'T')
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if (stdlib_lsame(trans,'N')) then
               lenx = n
               leny = m
           else
               lenx = m
               leny = n
           end if
           if (incx>0) then
               kx = 1
           else
               kx = 1 - (lenx-1)*incx
           end if
           if (incy>0) then
               ky = 1
           else
               ky = 1 - (leny-1)*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           ! first form  y := beta*y.
           if (beta/=cone) then
               if (incy==1) then
                   if (beta==czero) then
                       do i = 1,leny
                           y(i) = czero
                       end do
                   else
                       do i = 1,leny
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==czero) then
                       do i = 1,leny
                           y(iy) = czero
                           iy = iy + incy
                       end do
                   else
                       do i = 1,leny
                           y(iy) = beta*y(iy)
                           iy = iy + incy
                       end do
                   end if
               end if
           end if
           if (alpha==czero) return
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       do i = 1,m
                           y(i) = y(i) + temp*a(i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       do i = 1,m
                           y(iy) = y(iy) + temp*a(i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y  or  y := alpha*a**h*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = czero
                       if (noconj) then
                           do i = 1,m
                               temp = temp + a(i,j)*x(i)
                           end do
                       else
                           do i = 1,m
                               temp = temp + conjg(a(i,j))*x(i)
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = czero
                       ix = kx
                       if (noconj) then
                           do i = 1,m
                               temp = temp + a(i,j)*x(ix)
                               ix = ix + incx
                           end do
                       else
                           do i = 1,m
                               temp = temp + conjg(a(i,j))*x(ix)
                               ix = ix + incx
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_zgemv




     pure module subroutine stdlib_sger(m,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_sp
     !! SGER performs the rank 1 operation
     !! A := alpha*x*y**T + A,
     !! where alpha is a scalar, x is an m element vector, y is an n element
     !! vector and A is an m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, ix, j, jy, kx
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (m<0) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,m)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('SGER  ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or. (alpha==zero)) return
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (incy>0) then
               jy = 1
           else
               jy = 1 - (n-1)*incy
           end if
           if (incx==1) then
               do j = 1,n
                   if (y(jy)/=zero) then
                       temp = alpha*y(jy)
                       do i = 1,m
                           a(i,j) = a(i,j) + x(i)*temp
                       end do
                   end if
                   jy = jy + incy
               end do
           else
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (m-1)*incx
               end if
               do j = 1,n
                   if (y(jy)/=zero) then
                       temp = alpha*y(jy)
                       ix = kx
                       do i = 1,m
                           a(i,j) = a(i,j) + x(ix)*temp
                           ix = ix + incx
                       end do
                   end if
                   jy = jy + incy
               end do
           end if
           return
     end subroutine stdlib_sger

     pure module subroutine stdlib_dger(m,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_dp
     !! DGER performs the rank 1 operation
     !! A := alpha*x*y**T + A,
     !! where alpha is a scalar, x is an m element vector, y is an n element
     !! vector and A is an m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, ix, j, jy, kx
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (m<0) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,m)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('DGER  ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or. (alpha==zero)) return
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (incy>0) then
               jy = 1
           else
               jy = 1 - (n-1)*incy
           end if
           if (incx==1) then
               do j = 1,n
                   if (y(jy)/=zero) then
                       temp = alpha*y(jy)
                       do i = 1,m
                           a(i,j) = a(i,j) + x(i)*temp
                       end do
                   end if
                   jy = jy + incy
               end do
           else
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (m-1)*incx
               end if
               do j = 1,n
                   if (y(jy)/=zero) then
                       temp = alpha*y(jy)
                       ix = kx
                       do i = 1,m
                           a(i,j) = a(i,j) + x(ix)*temp
                           ix = ix + incx
                       end do
                   end if
                   jy = jy + incy
               end do
           end if
           return
     end subroutine stdlib_dger




     pure module subroutine stdlib_cgerc(m,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_sp
     !! CGERC performs the rank 1 operation
     !! A := alpha*x*y**H + A,
     !! where alpha is a scalar, x is an m element vector, y is an n element
     !! vector and A is an m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, ix, j, jy, kx
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (m<0) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,m)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('CGERC ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or. (alpha==czero)) return
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (incy>0) then
               jy = 1
           else
               jy = 1 - (n-1)*incy
           end if
           if (incx==1) then
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*conjg(y(jy))
                       do i = 1,m
                           a(i,j) = a(i,j) + x(i)*temp
                       end do
                   end if
                   jy = jy + incy
               end do
           else
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (m-1)*incx
               end if
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*conjg(y(jy))
                       ix = kx
                       do i = 1,m
                           a(i,j) = a(i,j) + x(ix)*temp
                           ix = ix + incx
                       end do
                   end if
                   jy = jy + incy
               end do
           end if
           return
     end subroutine stdlib_cgerc

     pure module subroutine stdlib_zgerc(m,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_dp
     !! ZGERC performs the rank 1 operation
     !! A := alpha*x*y**H + A,
     !! where alpha is a scalar, x is an m element vector, y is an n element
     !! vector and A is an m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, ix, j, jy, kx
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (m<0) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,m)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('ZGERC ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or. (alpha==czero)) return
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (incy>0) then
               jy = 1
           else
               jy = 1 - (n-1)*incy
           end if
           if (incx==1) then
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*conjg(y(jy))
                       do i = 1,m
                           a(i,j) = a(i,j) + x(i)*temp
                       end do
                   end if
                   jy = jy + incy
               end do
           else
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (m-1)*incx
               end if
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*conjg(y(jy))
                       ix = kx
                       do i = 1,m
                           a(i,j) = a(i,j) + x(ix)*temp
                           ix = ix + incx
                       end do
                   end if
                   jy = jy + incy
               end do
           end if
           return
     end subroutine stdlib_zgerc




     pure module subroutine stdlib_cgeru(m,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_sp
     !! CGERU performs the rank 1 operation
     !! A := alpha*x*y**T + A,
     !! where alpha is a scalar, x is an m element vector, y is an n element
     !! vector and A is an m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, ix, j, jy, kx
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (m<0) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,m)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('CGERU ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or. (alpha==czero)) return
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (incy>0) then
               jy = 1
           else
               jy = 1 - (n-1)*incy
           end if
           if (incx==1) then
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*y(jy)
                       do i = 1,m
                           a(i,j) = a(i,j) + x(i)*temp
                       end do
                   end if
                   jy = jy + incy
               end do
           else
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (m-1)*incx
               end if
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*y(jy)
                       ix = kx
                       do i = 1,m
                           a(i,j) = a(i,j) + x(ix)*temp
                           ix = ix + incx
                       end do
                   end if
                   jy = jy + incy
               end do
           end if
           return
     end subroutine stdlib_cgeru

     pure module subroutine stdlib_zgeru(m,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_dp
     !! ZGERU performs the rank 1 operation
     !! A := alpha*x*y**T + A,
     !! where alpha is a scalar, x is an m element vector, y is an n element
     !! vector and A is an m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, ix, j, jy, kx
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (m<0) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,m)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('ZGERU ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or. (alpha==czero)) return
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (incy>0) then
               jy = 1
           else
               jy = 1 - (n-1)*incy
           end if
           if (incx==1) then
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*y(jy)
                       do i = 1,m
                           a(i,j) = a(i,j) + x(i)*temp
                       end do
                   end if
                   jy = jy + incy
               end do
           else
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (m-1)*incx
               end if
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*y(jy)
                       ix = kx
                       do i = 1,m
                           a(i,j) = a(i,j) + x(ix)*temp
                           ix = ix + incx
                       end do
                   end if
                   jy = jy + incy
               end do
           end if
           return
     end subroutine stdlib_zgeru




     pure module subroutine stdlib_cher(uplo,n,alpha,x,incx,a,lda)
     use stdlib_blas_constants_sp
     !! CHER performs the hermitian rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n hermitian matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kx
           ! Intrinsic Functions 
           intrinsic :: conjg,max,real
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (lda<max(1,n)) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_xerbla('CHER  ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==real(czero,KIND=sp))) return
           ! set the start point in x if the increment is not unity.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in upper triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                           a(j,j) = real(a(j,j),KIND=sp) + real(x(j)*temp,KIND=sp)
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           ix = kx
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(ix)*temp
                               ix = ix + incx
                           end do
                           a(j,j) = real(a(j,j),KIND=sp) + real(x(jx)*temp,KIND=sp)
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                       jx = jx + incx
                   end do
               end if
           else
              ! form  a  when a is stored in lower triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           a(j,j) = real(a(j,j),KIND=sp) + real(temp*x(j),KIND=sp)
                           do i = j + 1,n
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           a(j,j) = real(a(j,j),KIND=sp) + real(temp*x(jx),KIND=sp)
                           ix = jx
                           do i = j + 1,n
                               ix = ix + incx
                               a(i,j) = a(i,j) + x(ix)*temp
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                       jx = jx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_cher

     pure module subroutine stdlib_zher(uplo,n,alpha,x,incx,a,lda)
     use stdlib_blas_constants_dp
     !! ZHER performs the hermitian rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n hermitian matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kx
           ! Intrinsic Functions 
           intrinsic :: real,conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (lda<max(1,n)) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_xerbla('ZHER  ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==real(czero,KIND=dp))) return
           ! set the start point in x if the increment is not unity.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in upper triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                           a(j,j) = real(a(j,j),KIND=dp) + real(x(j)*temp,KIND=dp)
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           ix = kx
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(ix)*temp
                               ix = ix + incx
                           end do
                           a(j,j) = real(a(j,j),KIND=dp) + real(x(jx)*temp,KIND=dp)
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                       jx = jx + incx
                   end do
               end if
           else
              ! form  a  when a is stored in lower triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           a(j,j) = real(a(j,j),KIND=dp) + real(temp*x(j),KIND=dp)
                           do i = j + 1,n
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           a(j,j) = real(a(j,j),KIND=dp) + real(temp*x(jx),KIND=dp)
                           ix = jx
                           do i = j + 1,n
                               ix = ix + incx
                               a(i,j) = a(i,j) + x(ix)*temp
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                       jx = jx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_zher




     pure module subroutine stdlib_cher2(uplo,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_sp
     !! CHER2 performs the hermitian rank 2 operation
     !! A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an n
     !! by n hermitian matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: conjg,max,real
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,n)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('CHER2 ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==czero)) return
           ! set up the start points in x and y if the increments are not both
           ! unity.
           if ((incx/=1) .or. (incy/=1)) then
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (n-1)*incx
               end if
               if (incy>0) then
                   ky = 1
               else
                   ky = 1 - (n-1)*incy
               end if
               jx = kx
               jy = ky
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                           a(j,j) = real(a(j,j),KIND=sp) +real(x(j)*temp1+y(j)*temp2,KIND=sp)
                                     
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           ix = kx
                           iy = ky
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                           a(j,j) = real(a(j,j),KIND=sp) +real(x(jx)*temp1+y(jy)*temp2,KIND=sp)
                                     
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  a  when a is stored in the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           a(j,j) = real(a(j,j),KIND=sp) +real(x(j)*temp1+y(j)*temp2,KIND=sp)
                                     
                           do i = j + 1,n
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           a(j,j) = real(a(j,j),KIND=sp) +real(x(jx)*temp1+y(jy)*temp2,KIND=sp)
                                     
                           ix = jx
                           iy = jy
                           do i = j + 1,n
                               ix = ix + incx
                               iy = iy + incy
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_cher2

     pure module subroutine stdlib_zher2(uplo,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_dp
     !! ZHER2 performs the hermitian rank 2 operation
     !! A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an n
     !! by n hermitian matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: real,conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,n)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('ZHER2 ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==czero)) return
           ! set up the start points in x and y if the increments are not both
           ! unity.
           if ((incx/=1) .or. (incy/=1)) then
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (n-1)*incx
               end if
               if (incy>0) then
                   ky = 1
               else
                   ky = 1 - (n-1)*incy
               end if
               jx = kx
               jy = ky
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                           a(j,j) = real(a(j,j),KIND=dp) +real(x(j)*temp1+y(j)*temp2,KIND=dp)
                                     
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           ix = kx
                           iy = ky
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                           a(j,j) = real(a(j,j),KIND=dp) +real(x(jx)*temp1+y(jy)*temp2,KIND=dp)
                                     
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  a  when a is stored in the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           a(j,j) = real(a(j,j),KIND=dp) +real(x(j)*temp1+y(j)*temp2,KIND=dp)
                                     
                           do i = j + 1,n
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           a(j,j) = real(a(j,j),KIND=dp) +real(x(jx)*temp1+y(jy)*temp2,KIND=dp)
                                     
                           ix = jx
                           iy = jy
                           do i = j + 1,n
                               ix = ix + incx
                               iy = iy + incy
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_zher2




     pure module subroutine stdlib_chemv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! CHEMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n hermitian matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: conjg,max,real
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (lda<max(1,n)) then
               info = 5
           else if (incx==0) then
               info = 7
           else if (incy==0) then
               info = 10
           end if
           if (info/=0) then
               call stdlib_xerbla('CHEMV ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. ((alpha==czero).and. (beta==cone))) return
           ! set up the start points in  x  and  y.
           if (incx>0) then
               kx = 1
           else
               kx = 1 - (n-1)*incx
           end if
           if (incy>0) then
               ky = 1
           else
               ky = 1 - (n-1)*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           ! first form  y := beta*y.
           if (beta/=cone) then
               if (incy==1) then
                   if (beta==czero) then
                       do i = 1,n
                           y(i) = czero
                       end do
                   else
                       do i = 1,n
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==czero) then
                       do i = 1,n
                           y(iy) = czero
                           iy = iy + incy
                       end do
                   else
                       do i = 1,n
                           y(iy) = beta*y(iy)
                           iy = iy + incy
                       end do
                   end if
               end if
           end if
           if (alpha==czero) return
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when a is stored in upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(i)
                       end do
                       y(j) = y(j) + temp1*real(a(j,j),KIND=sp) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       ix = kx
                       iy = ky
                       do i = 1,j - 1
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*real(a(j,j),KIND=sp) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  y  when a is stored in lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       y(j) = y(j) + temp1*real(a(j,j),KIND=sp)
                       do i = j + 1,n
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       y(jy) = y(jy) + temp1*real(a(j,j),KIND=sp)
                       ix = jx
                       iy = jy
                       do i = j + 1,n
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_chemv

     pure module subroutine stdlib_zhemv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! ZHEMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n hermitian matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: real,conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (lda<max(1,n)) then
               info = 5
           else if (incx==0) then
               info = 7
           else if (incy==0) then
               info = 10
           end if
           if (info/=0) then
               call stdlib_xerbla('ZHEMV ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. ((alpha==czero).and. (beta==cone))) return
           ! set up the start points in  x  and  y.
           if (incx>0) then
               kx = 1
           else
               kx = 1 - (n-1)*incx
           end if
           if (incy>0) then
               ky = 1
           else
               ky = 1 - (n-1)*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           ! first form  y := beta*y.
           if (beta/=cone) then
               if (incy==1) then
                   if (beta==czero) then
                       do i = 1,n
                           y(i) = czero
                       end do
                   else
                       do i = 1,n
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==czero) then
                       do i = 1,n
                           y(iy) = czero
                           iy = iy + incy
                       end do
                   else
                       do i = 1,n
                           y(iy) = beta*y(iy)
                           iy = iy + incy
                       end do
                   end if
               end if
           end if
           if (alpha==czero) return
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when a is stored in upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(i)
                       end do
                       y(j) = y(j) + temp1*real(a(j,j),KIND=dp) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       ix = kx
                       iy = ky
                       do i = 1,j - 1
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*real(a(j,j),KIND=dp) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  y  when a is stored in lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       y(j) = y(j) + temp1*real(a(j,j),KIND=dp)
                       do i = j + 1,n
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       y(jy) = y(jy) + temp1*real(a(j,j),KIND=dp)
                       ix = jx
                       iy = jy
                       do i = j + 1,n
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_zhemv




     pure module subroutine stdlib_I64_sgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! SGEMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (lda<max(1,m)) then
               info = 6
           else if (incx==0) then
               info = 8
           else if (incy==0) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('SGEMV ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==zero).and. (beta==one))) return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if (stdlib_lsame(trans,'N')) then
               lenx = n
               leny = m
           else
               lenx = m
               leny = n
           end if
           if (incx>0) then
               kx = 1
           else
               kx = 1 - (lenx-1)*incx
           end if
           if (incy>0) then
               ky = 1
           else
               ky = 1 - (leny-1)*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           ! first form  y := beta*y.
           if (beta/=one) then
               if (incy==1) then
                   if (beta==zero) then
                       do i = 1,leny
                           y(i) = zero
                       end do
                   else
                       do i = 1,leny
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==zero) then
                       do i = 1,leny
                           y(iy) = zero
                           iy = iy + incy
                       end do
                   else
                       do i = 1,leny
                           y(iy) = beta*y(iy)
                           iy = iy + incy
                       end do
                   end if
               end if
           end if
           if (alpha==zero) return
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       do i = 1,m
                           y(i) = y(i) + temp*a(i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       do i = 1,m
                           y(iy) = y(iy) + temp*a(i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = zero
                       do i = 1,m
                           temp = temp + a(i,j)*x(i)
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = zero
                       ix = kx
                       do i = 1,m
                           temp = temp + a(i,j)*x(ix)
                           ix = ix + incx
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_sgemv

     pure module subroutine stdlib_I64_dgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! DGEMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (lda<max(1,m)) then
               info = 6
           else if (incx==0) then
               info = 8
           else if (incy==0) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('DGEMV ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==zero).and. (beta==one))) return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if (stdlib_lsame(trans,'N')) then
               lenx = n
               leny = m
           else
               lenx = m
               leny = n
           end if
           if (incx>0) then
               kx = 1
           else
               kx = 1 - (lenx-1)*incx
           end if
           if (incy>0) then
               ky = 1
           else
               ky = 1 - (leny-1)*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           ! first form  y := beta*y.
           if (beta/=one) then
               if (incy==1) then
                   if (beta==zero) then
                       do i = 1,leny
                           y(i) = zero
                       end do
                   else
                       do i = 1,leny
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==zero) then
                       do i = 1,leny
                           y(iy) = zero
                           iy = iy + incy
                       end do
                   else
                       do i = 1,leny
                           y(iy) = beta*y(iy)
                           iy = iy + incy
                       end do
                   end if
               end if
           end if
           if (alpha==zero) return
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       do i = 1,m
                           y(i) = y(i) + temp*a(i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       do i = 1,m
                           y(iy) = y(iy) + temp*a(i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = zero
                       do i = 1,m
                           temp = temp + a(i,j)*x(i)
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = zero
                       ix = kx
                       do i = 1,m
                           temp = temp + a(i,j)*x(ix)
                           ix = ix + incx
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_dgemv


     pure module subroutine stdlib_I64_cgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! CGEMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
     !! y := alpha*A**H*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
           logical(lk) :: noconj
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (lda<max(1,m)) then
               info = 6
           else if (incx==0) then
               info = 8
           else if (incy==0) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CGEMV ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==czero).and. (beta==cone))) return
           noconj = stdlib_lsame(trans,'T')
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if (stdlib_lsame(trans,'N')) then
               lenx = n
               leny = m
           else
               lenx = m
               leny = n
           end if
           if (incx>0) then
               kx = 1
           else
               kx = 1 - (lenx-1)*incx
           end if
           if (incy>0) then
               ky = 1
           else
               ky = 1 - (leny-1)*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           ! first form  y := beta*y.
           if (beta/=cone) then
               if (incy==1) then
                   if (beta==czero) then
                       do i = 1,leny
                           y(i) = czero
                       end do
                   else
                       do i = 1,leny
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==czero) then
                       do i = 1,leny
                           y(iy) = czero
                           iy = iy + incy
                       end do
                   else
                       do i = 1,leny
                           y(iy) = beta*y(iy)
                           iy = iy + incy
                       end do
                   end if
               end if
           end if
           if (alpha==czero) return
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       do i = 1,m
                           y(i) = y(i) + temp*a(i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       do i = 1,m
                           y(iy) = y(iy) + temp*a(i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y  or  y := alpha*a**h*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = czero
                       if (noconj) then
                           do i = 1,m
                               temp = temp + a(i,j)*x(i)
                           end do
                       else
                           do i = 1,m
                               temp = temp + conjg(a(i,j))*x(i)
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = czero
                       ix = kx
                       if (noconj) then
                           do i = 1,m
                               temp = temp + a(i,j)*x(ix)
                               ix = ix + incx
                           end do
                       else
                           do i = 1,m
                               temp = temp + conjg(a(i,j))*x(ix)
                               ix = ix + incx
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_cgemv

     pure module subroutine stdlib_I64_zgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! ZGEMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
     !! y := alpha*A**H*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
           logical(lk) :: noconj
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (lda<max(1,m)) then
               info = 6
           else if (incx==0) then
               info = 8
           else if (incy==0) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZGEMV ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==czero).and. (beta==cone))) return
           noconj = stdlib_lsame(trans,'T')
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if (stdlib_lsame(trans,'N')) then
               lenx = n
               leny = m
           else
               lenx = m
               leny = n
           end if
           if (incx>0) then
               kx = 1
           else
               kx = 1 - (lenx-1)*incx
           end if
           if (incy>0) then
               ky = 1
           else
               ky = 1 - (leny-1)*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           ! first form  y := beta*y.
           if (beta/=cone) then
               if (incy==1) then
                   if (beta==czero) then
                       do i = 1,leny
                           y(i) = czero
                       end do
                   else
                       do i = 1,leny
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==czero) then
                       do i = 1,leny
                           y(iy) = czero
                           iy = iy + incy
                       end do
                   else
                       do i = 1,leny
                           y(iy) = beta*y(iy)
                           iy = iy + incy
                       end do
                   end if
               end if
           end if
           if (alpha==czero) return
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       do i = 1,m
                           y(i) = y(i) + temp*a(i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       do i = 1,m
                           y(iy) = y(iy) + temp*a(i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y  or  y := alpha*a**h*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = czero
                       if (noconj) then
                           do i = 1,m
                               temp = temp + a(i,j)*x(i)
                           end do
                       else
                           do i = 1,m
                               temp = temp + conjg(a(i,j))*x(i)
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = czero
                       ix = kx
                       if (noconj) then
                           do i = 1,m
                               temp = temp + a(i,j)*x(ix)
                               ix = ix + incx
                           end do
                       else
                           do i = 1,m
                               temp = temp + conjg(a(i,j))*x(ix)
                               ix = ix + incx
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_zgemv




     pure module subroutine stdlib_I64_sger(m,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_sp
     !! SGER performs the rank 1 operation
     !! A := alpha*x*y**T + A,
     !! where alpha is a scalar, x is an m element vector, y is an n element
     !! vector and A is an m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jy, kx
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (m<0) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,m)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('SGER  ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or. (alpha==zero)) return
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (incy>0) then
               jy = 1
           else
               jy = 1 - (n-1)*incy
           end if
           if (incx==1) then
               do j = 1,n
                   if (y(jy)/=zero) then
                       temp = alpha*y(jy)
                       do i = 1,m
                           a(i,j) = a(i,j) + x(i)*temp
                       end do
                   end if
                   jy = jy + incy
               end do
           else
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (m-1)*incx
               end if
               do j = 1,n
                   if (y(jy)/=zero) then
                       temp = alpha*y(jy)
                       ix = kx
                       do i = 1,m
                           a(i,j) = a(i,j) + x(ix)*temp
                           ix = ix + incx
                       end do
                   end if
                   jy = jy + incy
               end do
           end if
           return
     end subroutine stdlib_I64_sger

     pure module subroutine stdlib_I64_dger(m,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_dp
     !! DGER performs the rank 1 operation
     !! A := alpha*x*y**T + A,
     !! where alpha is a scalar, x is an m element vector, y is an n element
     !! vector and A is an m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jy, kx
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (m<0) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,m)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('DGER  ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or. (alpha==zero)) return
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through a.
           if (incy>0) then
               jy = 1
           else
               jy = 1 - (n-1)*incy
           end if
           if (incx==1) then
               do j = 1,n
                   if (y(jy)/=zero) then
                       temp = alpha*y(jy)
                       do i = 1,m
                           a(i,j) = a(i,j) + x(i)*temp
                       end do
                   end if
                   jy = jy + incy
               end do
           else
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (m-1)*incx
               end if
               do j = 1,n
                   if (y(jy)/=zero) then
                       temp = alpha*y(jy)
                       ix = kx
                       do i = 1,m
                           a(i,j) = a(i,j) + x(ix)*temp
                           ix = ix + incx
                       end do
                   end if
                   jy = jy + incy
               end do
           end if
           return
     end subroutine stdlib_I64_dger




     pure module subroutine stdlib_I64_cgerc(m,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_sp
     !! CGERC performs the rank 1 operation
     !! A := alpha*x*y**H + A,
     !! where alpha is a scalar, x is an m element vector, y is an n element
     !! vector and A is an m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, lda, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jy, kx
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (m<0) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,m)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CGERC ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or. (alpha==czero)) return
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (incy>0) then
               jy = 1
           else
               jy = 1 - (n-1)*incy
           end if
           if (incx==1) then
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*conjg(y(jy))
                       do i = 1,m
                           a(i,j) = a(i,j) + x(i)*temp
                       end do
                   end if
                   jy = jy + incy
               end do
           else
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (m-1)*incx
               end if
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*conjg(y(jy))
                       ix = kx
                       do i = 1,m
                           a(i,j) = a(i,j) + x(ix)*temp
                           ix = ix + incx
                       end do
                   end if
                   jy = jy + incy
               end do
           end if
           return
     end subroutine stdlib_I64_cgerc

     pure module subroutine stdlib_I64_zgerc(m,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_dp
     !! ZGERC performs the rank 1 operation
     !! A := alpha*x*y**H + A,
     !! where alpha is a scalar, x is an m element vector, y is an n element
     !! vector and A is an m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, lda, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jy, kx
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! test the input parameters.
           info = 0
           if (m<0) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,m)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZGERC ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or. (alpha==czero)) return
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (incy>0) then
               jy = 1
           else
               jy = 1 - (n-1)*incy
           end if
           if (incx==1) then
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*conjg(y(jy))
                       do i = 1,m
                           a(i,j) = a(i,j) + x(i)*temp
                       end do
                   end if
                   jy = jy + incy
               end do
           else
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (m-1)*incx
               end if
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*conjg(y(jy))
                       ix = kx
                       do i = 1,m
                           a(i,j) = a(i,j) + x(ix)*temp
                           ix = ix + incx
                       end do
                   end if
                   jy = jy + incy
               end do
           end if
           return
     end subroutine stdlib_I64_zgerc




     pure module subroutine stdlib_I64_cgeru(m,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_sp
     !! CGERU performs the rank 1 operation
     !! A := alpha*x*y**T + A,
     !! where alpha is a scalar, x is an m element vector, y is an n element
     !! vector and A is an m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, lda, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jy, kx
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (m<0) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,m)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CGERU ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or. (alpha==czero)) return
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (incy>0) then
               jy = 1
           else
               jy = 1 - (n-1)*incy
           end if
           if (incx==1) then
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*y(jy)
                       do i = 1,m
                           a(i,j) = a(i,j) + x(i)*temp
                       end do
                   end if
                   jy = jy + incy
               end do
           else
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (m-1)*incx
               end if
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*y(jy)
                       ix = kx
                       do i = 1,m
                           a(i,j) = a(i,j) + x(ix)*temp
                           ix = ix + incx
                       end do
                   end if
                   jy = jy + incy
               end do
           end if
           return
     end subroutine stdlib_I64_cgeru

     pure module subroutine stdlib_I64_zgeru(m,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_dp
     !! ZGERU performs the rank 1 operation
     !! A := alpha*x*y**T + A,
     !! where alpha is a scalar, x is an m element vector, y is an n element
     !! vector and A is an m by n matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, lda, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jy, kx
           ! Intrinsic Functions 
           intrinsic :: max
           ! test the input parameters.
           info = 0
           if (m<0) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,m)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZGERU ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or. (alpha==czero)) return
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through a.
           if (incy>0) then
               jy = 1
           else
               jy = 1 - (n-1)*incy
           end if
           if (incx==1) then
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*y(jy)
                       do i = 1,m
                           a(i,j) = a(i,j) + x(i)*temp
                       end do
                   end if
                   jy = jy + incy
               end do
           else
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (m-1)*incx
               end if
               do j = 1,n
                   if (y(jy)/=czero) then
                       temp = alpha*y(jy)
                       ix = kx
                       do i = 1,m
                           a(i,j) = a(i,j) + x(ix)*temp
                           ix = ix + incx
                       end do
                   end if
                   jy = jy + incy
               end do
           end if
           return
     end subroutine stdlib_I64_zgeru




     pure module subroutine stdlib_I64_cher(uplo,n,alpha,x,incx,a,lda)
     use stdlib_blas_constants_sp
     !! CHER performs the hermitian rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n hermitian matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kx
           ! Intrinsic Functions 
           intrinsic :: conjg,max,real
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (lda<max(1,n)) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CHER  ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==real(czero,KIND=sp))) return
           ! set the start point in x if the increment is not unity.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in upper triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                           a(j,j) = real(a(j,j),KIND=sp) + real(x(j)*temp,KIND=sp)
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           ix = kx
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(ix)*temp
                               ix = ix + incx
                           end do
                           a(j,j) = real(a(j,j),KIND=sp) + real(x(jx)*temp,KIND=sp)
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                       jx = jx + incx
                   end do
               end if
           else
              ! form  a  when a is stored in lower triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           a(j,j) = real(a(j,j),KIND=sp) + real(temp*x(j),KIND=sp)
                           do i = j + 1,n
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           a(j,j) = real(a(j,j),KIND=sp) + real(temp*x(jx),KIND=sp)
                           ix = jx
                           do i = j + 1,n
                               ix = ix + incx
                               a(i,j) = a(i,j) + x(ix)*temp
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                       jx = jx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_cher

     pure module subroutine stdlib_I64_zher(uplo,n,alpha,x,incx,a,lda)
     use stdlib_blas_constants_dp
     !! ZHER performs the hermitian rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n hermitian matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kx
           ! Intrinsic Functions 
           intrinsic :: real,conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (lda<max(1,n)) then
               info = 7
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZHER  ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==real(czero,KIND=dp))) return
           ! set the start point in x if the increment is not unity.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in upper triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                           a(j,j) = real(a(j,j),KIND=dp) + real(x(j)*temp,KIND=dp)
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           ix = kx
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(ix)*temp
                               ix = ix + incx
                           end do
                           a(j,j) = real(a(j,j),KIND=dp) + real(x(jx)*temp,KIND=dp)
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                       jx = jx + incx
                   end do
               end if
           else
              ! form  a  when a is stored in lower triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           a(j,j) = real(a(j,j),KIND=dp) + real(temp*x(j),KIND=dp)
                           do i = j + 1,n
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           a(j,j) = real(a(j,j),KIND=dp) + real(temp*x(jx),KIND=dp)
                           ix = jx
                           do i = j + 1,n
                               ix = ix + incx
                               a(i,j) = a(i,j) + x(ix)*temp
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                       jx = jx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_zher




     pure module subroutine stdlib_I64_cher2(uplo,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_sp
     !! CHER2 performs the hermitian rank 2 operation
     !! A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an n
     !! by n hermitian matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: conjg,max,real
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,n)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CHER2 ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==czero)) return
           ! set up the start points in x and y if the increments are not both
           ! unity.
           if ((incx/=1) .or. (incy/=1)) then
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (n-1)*incx
               end if
               if (incy>0) then
                   ky = 1
               else
                   ky = 1 - (n-1)*incy
               end if
               jx = kx
               jy = ky
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                           a(j,j) = real(a(j,j),KIND=sp) +real(x(j)*temp1+y(j)*temp2,KIND=sp)
                                     
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           ix = kx
                           iy = ky
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                           a(j,j) = real(a(j,j),KIND=sp) +real(x(jx)*temp1+y(jy)*temp2,KIND=sp)
                                     
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  a  when a is stored in the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           a(j,j) = real(a(j,j),KIND=sp) +real(x(j)*temp1+y(j)*temp2,KIND=sp)
                                     
                           do i = j + 1,n
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           a(j,j) = real(a(j,j),KIND=sp) +real(x(jx)*temp1+y(jy)*temp2,KIND=sp)
                                     
                           ix = jx
                           iy = jy
                           do i = j + 1,n
                               ix = ix + incx
                               iy = iy + incy
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=sp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_cher2

     pure module subroutine stdlib_I64_zher2(uplo,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_dp
     !! ZHER2 performs the hermitian rank 2 operation
     !! A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an n
     !! by n hermitian matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: real,conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           else if (incy==0) then
               info = 7
           else if (lda<max(1,n)) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZHER2 ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==czero)) return
           ! set up the start points in x and y if the increments are not both
           ! unity.
           if ((incx/=1) .or. (incy/=1)) then
               if (incx>0) then
                   kx = 1
               else
                   kx = 1 - (n-1)*incx
               end if
               if (incy>0) then
                   ky = 1
               else
                   ky = 1 - (n-1)*incy
               end if
               jx = kx
               jy = ky
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                           a(j,j) = real(a(j,j),KIND=dp) +real(x(j)*temp1+y(j)*temp2,KIND=dp)
                                     
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           ix = kx
                           iy = ky
                           do i = 1,j - 1
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                           a(j,j) = real(a(j,j),KIND=dp) +real(x(jx)*temp1+y(jy)*temp2,KIND=dp)
                                     
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  a  when a is stored in the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           a(j,j) = real(a(j,j),KIND=dp) +real(x(j)*temp1+y(j)*temp2,KIND=dp)
                                     
                           do i = j + 1,n
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           a(j,j) = real(a(j,j),KIND=dp) +real(x(jx)*temp1+y(jy)*temp2,KIND=dp)
                                     
                           ix = jx
                           iy = jy
                           do i = j + 1,n
                               ix = ix + incx
                               iy = iy + incy
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                           end do
                       else
                           a(j,j) = real(a(j,j),KIND=dp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_zher2




     pure module subroutine stdlib_I64_chemv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! CHEMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n hermitian matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: conjg,max,real
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (lda<max(1,n)) then
               info = 5
           else if (incx==0) then
               info = 7
           else if (incy==0) then
               info = 10
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CHEMV ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. ((alpha==czero).and. (beta==cone))) return
           ! set up the start points in  x  and  y.
           if (incx>0) then
               kx = 1
           else
               kx = 1 - (n-1)*incx
           end if
           if (incy>0) then
               ky = 1
           else
               ky = 1 - (n-1)*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           ! first form  y := beta*y.
           if (beta/=cone) then
               if (incy==1) then
                   if (beta==czero) then
                       do i = 1,n
                           y(i) = czero
                       end do
                   else
                       do i = 1,n
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==czero) then
                       do i = 1,n
                           y(iy) = czero
                           iy = iy + incy
                       end do
                   else
                       do i = 1,n
                           y(iy) = beta*y(iy)
                           iy = iy + incy
                       end do
                   end if
               end if
           end if
           if (alpha==czero) return
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when a is stored in upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(i)
                       end do
                       y(j) = y(j) + temp1*real(a(j,j),KIND=sp) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       ix = kx
                       iy = ky
                       do i = 1,j - 1
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*real(a(j,j),KIND=sp) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  y  when a is stored in lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       y(j) = y(j) + temp1*real(a(j,j),KIND=sp)
                       do i = j + 1,n
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       y(jy) = y(jy) + temp1*real(a(j,j),KIND=sp)
                       ix = jx
                       iy = jy
                       do i = j + 1,n
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_chemv

     pure module subroutine stdlib_I64_zhemv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! ZHEMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n hermitian matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: real,conjg,max
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (lda<max(1,n)) then
               info = 5
           else if (incx==0) then
               info = 7
           else if (incy==0) then
               info = 10
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZHEMV ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. ((alpha==czero).and. (beta==cone))) return
           ! set up the start points in  x  and  y.
           if (incx>0) then
               kx = 1
           else
               kx = 1 - (n-1)*incx
           end if
           if (incy>0) then
               ky = 1
           else
               ky = 1 - (n-1)*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           ! first form  y := beta*y.
           if (beta/=cone) then
               if (incy==1) then
                   if (beta==czero) then
                       do i = 1,n
                           y(i) = czero
                       end do
                   else
                       do i = 1,n
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==czero) then
                       do i = 1,n
                           y(iy) = czero
                           iy = iy + incy
                       end do
                   else
                       do i = 1,n
                           y(iy) = beta*y(iy)
                           iy = iy + incy
                       end do
                   end if
               end if
           end if
           if (alpha==czero) return
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when a is stored in upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(i)
                       end do
                       y(j) = y(j) + temp1*real(a(j,j),KIND=dp) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       ix = kx
                       iy = ky
                       do i = 1,j - 1
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*real(a(j,j),KIND=dp) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  y  when a is stored in lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       y(j) = y(j) + temp1*real(a(j,j),KIND=dp)
                       do i = j + 1,n
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       y(jy) = y(jy) + temp1*real(a(j,j),KIND=dp)
                       ix = jx
                       iy = jy
                       do i = j + 1,n
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + conjg(a(i,j))*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_zhemv



end submodule stdlib_blas_level2_gen
