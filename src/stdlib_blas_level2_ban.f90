submodule(stdlib_blas) stdlib_blas_level2_ban
  implicit none


  contains

     pure module subroutine stdlib_sgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! SGBMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n band matrix, with kl sub-diagonals and ku super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, kl, ku, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kup1, kx, ky, lenx, leny
           ! Intrinsic Functions 
           intrinsic :: max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (kl<0) then
               info = 4
           else if (ku<0) then
               info = 5
           else if (lda< (kl+ku+1)) then
               info = 8
           else if (incx==0) then
               info = 10
           else if (incy==0) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_xerbla('SGBMV ',info)
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
           ! accessed sequentially with one pass through the band part of a.
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
           kup1 = ku + 1
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(i) = y(i) + temp*a(k+i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(iy) = y(iy) + temp*a(k+i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                       if (j>ku) ky = ky + incy
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = zero
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           temp = temp + a(k+i,j)*x(i)
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = zero
                       ix = kx
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           temp = temp + a(k+i,j)*x(ix)
                           ix = ix + incx
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                       if (j>ku) kx = kx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_sgbmv

     pure module subroutine stdlib_dgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! DGBMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n band matrix, with kl sub-diagonals and ku super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, kl, ku, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kup1, kx, ky, lenx, leny
           ! Intrinsic Functions 
           intrinsic :: max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (kl<0) then
               info = 4
           else if (ku<0) then
               info = 5
           else if (lda< (kl+ku+1)) then
               info = 8
           else if (incx==0) then
               info = 10
           else if (incy==0) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_xerbla('DGBMV ',info)
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
           ! accessed sequentially with one pass through the band part of a.
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
           kup1 = ku + 1
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(i) = y(i) + temp*a(k+i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(iy) = y(iy) + temp*a(k+i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                       if (j>ku) ky = ky + incy
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = zero
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           temp = temp + a(k+i,j)*x(i)
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = zero
                       ix = kx
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           temp = temp + a(k+i,j)*x(ix)
                           ix = ix + incx
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                       if (j>ku) kx = kx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_dgbmv


     pure module subroutine stdlib_cgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! CGBMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
     !! y := alpha*A**H*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n band matrix, with kl sub-diagonals and ku super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, kl, ku, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kup1, kx, ky, lenx, leny
           logical(lk) :: noconj
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (kl<0) then
               info = 4
           else if (ku<0) then
               info = 5
           else if (lda< (kl+ku+1)) then
               info = 8
           else if (incx==0) then
               info = 10
           else if (incy==0) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_xerbla('CGBMV ',info)
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
           ! accessed sequentially with cone pass through the band part of a.
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
           kup1 = ku + 1
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(i) = y(i) + temp*a(k+i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(iy) = y(iy) + temp*a(k+i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                       if (j>ku) ky = ky + incy
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y  or  y := alpha*a**h*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = czero
                       k = kup1 - j
                       if (noconj) then
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + a(k+i,j)*x(i)
                           end do
                       else
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + conjg(a(k+i,j))*x(i)
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = czero
                       ix = kx
                       k = kup1 - j
                       if (noconj) then
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + a(k+i,j)*x(ix)
                               ix = ix + incx
                           end do
                       else
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + conjg(a(k+i,j))*x(ix)
                               ix = ix + incx
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                       if (j>ku) kx = kx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_cgbmv

     pure module subroutine stdlib_zgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! ZGBMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
     !! y := alpha*A**H*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n band matrix, with kl sub-diagonals and ku super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, kl, ku, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kup1, kx, ky, lenx, leny
           logical(lk) :: noconj
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (kl<0) then
               info = 4
           else if (ku<0) then
               info = 5
           else if (lda< (kl+ku+1)) then
               info = 8
           else if (incx==0) then
               info = 10
           else if (incy==0) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_xerbla('ZGBMV ',info)
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
           ! accessed sequentially with cone pass through the band part of a.
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
           kup1 = ku + 1
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(i) = y(i) + temp*a(k+i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(iy) = y(iy) + temp*a(k+i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                       if (j>ku) ky = ky + incy
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y  or  y := alpha*a**h*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = czero
                       k = kup1 - j
                       if (noconj) then
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + a(k+i,j)*x(i)
                           end do
                       else
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + conjg(a(k+i,j))*x(i)
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = czero
                       ix = kx
                       k = kup1 - j
                       if (noconj) then
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + a(k+i,j)*x(ix)
                               ix = ix + incx
                           end do
                       else
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + conjg(a(k+i,j))*x(ix)
                               ix = ix + incx
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                       if (j>ku) kx = kx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_zgbmv




     pure module subroutine stdlib_chbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! CHBMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n hermitian band matrix, with k super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, k, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kplus1, kx, ky, l
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min,real
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (k<0) then
               info = 3
           else if (lda< (k+1)) then
               info = 6
           else if (incx==0) then
               info = 8
           else if (incy==0) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('CHBMV ',info)
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
           ! start the operations. in this version the elements of the array a
           ! are accessed sequentially with cone pass through a.
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
              ! form  y  when upper triangle of a is stored.
               kplus1 = k + 1
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(i)
                       end do
                       y(j) = y(j) + temp1*real(a(kplus1,j),KIND=sp) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       ix = kx
                       iy = ky
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*real(a(kplus1,j),KIND=sp) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       if (j>k) then
                           kx = kx + incx
                           ky = ky + incy
                       end if
                   end do
               end if
           else
              ! form  y  when lower triangle of a is stored.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       y(j) = y(j) + temp1*real(a(1,j),KIND=sp)
                       l = 1 - j
                       do i = j + 1,min(n,j+k)
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       y(jy) = y(jy) + temp1*real(a(1,j),KIND=sp)
                       l = 1 - j
                       ix = jx
                       iy = jy
                       do i = j + 1,min(n,j+k)
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_chbmv

     pure module subroutine stdlib_zhbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! ZHBMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n hermitian band matrix, with k super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, k, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kplus1, kx, ky, l
           ! Intrinsic Functions 
           intrinsic :: real,conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (k<0) then
               info = 3
           else if (lda< (k+1)) then
               info = 6
           else if (incx==0) then
               info = 8
           else if (incy==0) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('ZHBMV ',info)
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
           ! start the operations. in this version the elements of the array a
           ! are accessed sequentially with cone pass through a.
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
              ! form  y  when upper triangle of a is stored.
               kplus1 = k + 1
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(i)
                       end do
                       y(j) = y(j) + temp1*real(a(kplus1,j),KIND=dp) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       ix = kx
                       iy = ky
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*real(a(kplus1,j),KIND=dp) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       if (j>k) then
                           kx = kx + incx
                           ky = ky + incy
                       end if
                   end do
               end if
           else
              ! form  y  when lower triangle of a is stored.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       y(j) = y(j) + temp1*real(a(1,j),KIND=dp)
                       l = 1 - j
                       do i = j + 1,min(n,j+k)
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       y(jy) = y(jy) + temp1*real(a(1,j),KIND=dp)
                       l = 1 - j
                       ix = jx
                       iy = jy
                       do i = j + 1,min(n,j+k)
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_zhbmv




     pure module subroutine stdlib_I64_sgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! SGBMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n band matrix, with kl sub-diagonals and ku super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, kl, ku, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kup1, kx, ky, lenx, leny
           ! Intrinsic Functions 
           intrinsic :: max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (kl<0) then
               info = 4
           else if (ku<0) then
               info = 5
           else if (lda< (kl+ku+1)) then
               info = 8
           else if (incx==0) then
               info = 10
           else if (incy==0) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('SGBMV ',info)
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
           ! accessed sequentially with one pass through the band part of a.
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
           kup1 = ku + 1
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(i) = y(i) + temp*a(k+i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(iy) = y(iy) + temp*a(k+i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                       if (j>ku) ky = ky + incy
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = zero
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           temp = temp + a(k+i,j)*x(i)
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = zero
                       ix = kx
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           temp = temp + a(k+i,j)*x(ix)
                           ix = ix + incx
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                       if (j>ku) kx = kx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_sgbmv

     pure module subroutine stdlib_I64_dgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! DGBMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n band matrix, with kl sub-diagonals and ku super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, kl, ku, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kup1, kx, ky, lenx, leny
           ! Intrinsic Functions 
           intrinsic :: max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (kl<0) then
               info = 4
           else if (ku<0) then
               info = 5
           else if (lda< (kl+ku+1)) then
               info = 8
           else if (incx==0) then
               info = 10
           else if (incy==0) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('DGBMV ',info)
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
           ! accessed sequentially with one pass through the band part of a.
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
           kup1 = ku + 1
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(i) = y(i) + temp*a(k+i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(iy) = y(iy) + temp*a(k+i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                       if (j>ku) ky = ky + incy
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = zero
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           temp = temp + a(k+i,j)*x(i)
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = zero
                       ix = kx
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           temp = temp + a(k+i,j)*x(ix)
                           ix = ix + incx
                       end do
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                       if (j>ku) kx = kx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_dgbmv


     pure module subroutine stdlib_I64_cgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! CGBMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
     !! y := alpha*A**H*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n band matrix, with kl sub-diagonals and ku super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, kl, ku, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kup1, kx, ky, lenx, leny
           logical(lk) :: noconj
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (kl<0) then
               info = 4
           else if (ku<0) then
               info = 5
           else if (lda< (kl+ku+1)) then
               info = 8
           else if (incx==0) then
               info = 10
           else if (incy==0) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CGBMV ',info)
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
           ! accessed sequentially with cone pass through the band part of a.
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
           kup1 = ku + 1
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(i) = y(i) + temp*a(k+i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(iy) = y(iy) + temp*a(k+i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                       if (j>ku) ky = ky + incy
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y  or  y := alpha*a**h*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = czero
                       k = kup1 - j
                       if (noconj) then
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + a(k+i,j)*x(i)
                           end do
                       else
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + conjg(a(k+i,j))*x(i)
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = czero
                       ix = kx
                       k = kup1 - j
                       if (noconj) then
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + a(k+i,j)*x(ix)
                               ix = ix + incx
                           end do
                       else
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + conjg(a(k+i,j))*x(ix)
                               ix = ix + incx
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                       if (j>ku) kx = kx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_cgbmv

     pure module subroutine stdlib_I64_zgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! ZGBMV performs one of the matrix-vector operations
     !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
     !! y := alpha*A**H*x + beta*y,
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n band matrix, with kl sub-diagonals and ku super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, kl, ku, lda, m, n
           character, intent(in) :: trans
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kup1, kx, ky, lenx, leny
           logical(lk) :: noconj
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(trans,'N') .and. .not.stdlib_lsame(trans,'T') &
                     .and..not.stdlib_lsame(trans,'C')) then
               info = 1
           else if (m<0) then
               info = 2
           else if (n<0) then
               info = 3
           else if (kl<0) then
               info = 4
           else if (ku<0) then
               info = 5
           else if (lda< (kl+ku+1)) then
               info = 8
           else if (incx==0) then
               info = 10
           else if (incy==0) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZGBMV ',info)
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
           ! accessed sequentially with cone pass through the band part of a.
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
           kup1 = ku + 1
           if (stdlib_lsame(trans,'N')) then
              ! form  y := alpha*a*x + y.
               jx = kx
               if (incy==1) then
                   do j = 1,n
                       temp = alpha*x(jx)
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(i) = y(i) + temp*a(k+i,j)
                       end do
                       jx = jx + incx
                   end do
               else
                   do j = 1,n
                       temp = alpha*x(jx)
                       iy = ky
                       k = kup1 - j
                       do i = max(1,j-ku),min(m,j+kl)
                           y(iy) = y(iy) + temp*a(k+i,j)
                           iy = iy + incy
                       end do
                       jx = jx + incx
                       if (j>ku) ky = ky + incy
                   end do
               end if
           else
              ! form  y := alpha*a**t*x + y  or  y := alpha*a**h*x + y.
               jy = ky
               if (incx==1) then
                   do j = 1,n
                       temp = czero
                       k = kup1 - j
                       if (noconj) then
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + a(k+i,j)*x(i)
                           end do
                       else
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + conjg(a(k+i,j))*x(i)
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                   end do
               else
                   do j = 1,n
                       temp = czero
                       ix = kx
                       k = kup1 - j
                       if (noconj) then
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + a(k+i,j)*x(ix)
                               ix = ix + incx
                           end do
                       else
                           do i = max(1,j-ku),min(m,j+kl)
                               temp = temp + conjg(a(k+i,j))*x(ix)
                               ix = ix + incx
                           end do
                       end if
                       y(jy) = y(jy) + alpha*temp
                       jy = jy + incy
                       if (j>ku) kx = kx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_zgbmv




     pure module subroutine stdlib_I64_chbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! CHBMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n hermitian band matrix, with k super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, k, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kplus1, kx, ky, l
           ! Intrinsic Functions 
           intrinsic :: conjg,max,min,real
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (k<0) then
               info = 3
           else if (lda< (k+1)) then
               info = 6
           else if (incx==0) then
               info = 8
           else if (incy==0) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CHBMV ',info)
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
           ! start the operations. in this version the elements of the array a
           ! are accessed sequentially with cone pass through a.
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
              ! form  y  when upper triangle of a is stored.
               kplus1 = k + 1
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(i)
                       end do
                       y(j) = y(j) + temp1*real(a(kplus1,j),KIND=sp) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       ix = kx
                       iy = ky
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*real(a(kplus1,j),KIND=sp) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       if (j>k) then
                           kx = kx + incx
                           ky = ky + incy
                       end if
                   end do
               end if
           else
              ! form  y  when lower triangle of a is stored.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       y(j) = y(j) + temp1*real(a(1,j),KIND=sp)
                       l = 1 - j
                       do i = j + 1,min(n,j+k)
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       y(jy) = y(jy) + temp1*real(a(1,j),KIND=sp)
                       l = 1 - j
                       ix = jx
                       iy = jy
                       do i = j + 1,min(n,j+k)
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_chbmv

     pure module subroutine stdlib_I64_zhbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! ZHBMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n hermitian band matrix, with k super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, k, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kplus1, kx, ky, l
           ! Intrinsic Functions 
           intrinsic :: real,conjg,max,min
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (k<0) then
               info = 3
           else if (lda< (k+1)) then
               info = 6
           else if (incx==0) then
               info = 8
           else if (incy==0) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZHBMV ',info)
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
           ! start the operations. in this version the elements of the array a
           ! are accessed sequentially with cone pass through a.
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
              ! form  y  when upper triangle of a is stored.
               kplus1 = k + 1
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(i)
                       end do
                       y(j) = y(j) + temp1*real(a(kplus1,j),KIND=dp) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       ix = kx
                       iy = ky
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*real(a(kplus1,j),KIND=dp) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       if (j>k) then
                           kx = kx + incx
                           ky = ky + incy
                       end if
                   end do
               end if
           else
              ! form  y  when lower triangle of a is stored.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       y(j) = y(j) + temp1*real(a(1,j),KIND=dp)
                       l = 1 - j
                       do i = j + 1,min(n,j+k)
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       y(jy) = y(jy) + temp1*real(a(1,j),KIND=dp)
                       l = 1 - j
                       ix = jx
                       iy = jy
                       do i = j + 1,min(n,j+k)
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + conjg(a(l+i,j))*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_zhbmv



end submodule stdlib_blas_level2_ban
