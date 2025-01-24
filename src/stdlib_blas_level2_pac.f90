submodule(stdlib_blas) stdlib_blas_level2_pac
  implicit none


  contains

     pure module subroutine stdlib_sspmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! SSPMV performs the matrix-vector operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(in) :: ap(*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 6
           else if (incy==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('SSPMV ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. ((alpha==zero).and. (beta==one))) return
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with one pass through ap.
           ! first form  y := beta*y.
           if (beta/=one) then
               if (incy==1) then
                   if (beta==zero) then
                       do i = 1,n
                           y(i) = zero
                       end do
                   else
                       do i = 1,n
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==zero) then
                       do i = 1,n
                           y(iy) = zero
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
           if (alpha==zero) return
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when ap contains the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       k = kk
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + temp1*ap(kk+j-1) + alpha*temp2
                       kk = kk + j
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       ix = kx
                       iy = ky
                       do k = kk,kk + j - 2
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*ap(kk+j-1) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  y  when ap contains the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       y(j) = y(j) + temp1*ap(kk)
                       k = kk + 1
                       do i = j + 1,n
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + alpha*temp2
                       kk = kk + (n-j+1)
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       y(jy) = y(jy) + temp1*ap(kk)
                       ix = jx
                       iy = jy
                       do k = kk + 1,kk + n - j
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + (n-j+1)
                   end do
               end if
           end if
           return
     end subroutine stdlib_sspmv

     pure module subroutine stdlib_dspmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! DSPMV performs the matrix-vector operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(in) :: ap(*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 6
           else if (incy==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('DSPMV ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. ((alpha==zero).and. (beta==one))) return
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with one pass through ap.
           ! first form  y := beta*y.
           if (beta/=one) then
               if (incy==1) then
                   if (beta==zero) then
                       do i = 1,n
                           y(i) = zero
                       end do
                   else
                       do i = 1,n
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==zero) then
                       do i = 1,n
                           y(iy) = zero
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
           if (alpha==zero) return
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when ap contains the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       k = kk
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + temp1*ap(kk+j-1) + alpha*temp2
                       kk = kk + j
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       ix = kx
                       iy = ky
                       do k = kk,kk + j - 2
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*ap(kk+j-1) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  y  when ap contains the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       y(j) = y(j) + temp1*ap(kk)
                       k = kk + 1
                       do i = j + 1,n
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + alpha*temp2
                       kk = kk + (n-j+1)
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       y(jy) = y(jy) + temp1*ap(kk)
                       ix = jx
                       iy = jy
                       do k = kk + 1,kk + n - j
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + (n-j+1)
                   end do
               end if
           end if
           return
     end subroutine stdlib_dspmv




     pure module subroutine stdlib_ssbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! SSBMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric band matrix, with k super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, k, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kplus1, kx, ky, l
           ! Intrinsic Functions 
           intrinsic :: max,min
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
               call stdlib_xerbla('SSBMV ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. ((alpha==zero).and. (beta==one))) return
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
           ! are accessed sequentially with one pass through a.
           ! first form  y := beta*y.
           if (beta/=one) then
               if (incy==1) then
                   if (beta==zero) then
                       do i = 1,n
                           y(i) = zero
                       end do
                   else
                       do i = 1,n
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==zero) then
                       do i = 1,n
                           y(iy) = zero
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
           if (alpha==zero) return
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when upper triangle of a is stored.
               kplus1 = k + 1
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(i)
                       end do
                       y(j) = y(j) + temp1*a(kplus1,j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       ix = kx
                       iy = ky
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*a(kplus1,j) + alpha*temp2
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
                       temp2 = zero
                       y(j) = y(j) + temp1*a(1,j)
                       l = 1 - j
                       do i = j + 1,min(n,j+k)
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       y(jy) = y(jy) + temp1*a(1,j)
                       l = 1 - j
                       ix = jx
                       iy = jy
                       do i = j + 1,min(n,j+k)
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_ssbmv

     pure module subroutine stdlib_dsbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! DSBMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric band matrix, with k super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, k, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kplus1, kx, ky, l
           ! Intrinsic Functions 
           intrinsic :: max,min
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
               call stdlib_xerbla('DSBMV ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. ((alpha==zero).and. (beta==one))) return
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
           ! are accessed sequentially with one pass through a.
           ! first form  y := beta*y.
           if (beta/=one) then
               if (incy==1) then
                   if (beta==zero) then
                       do i = 1,n
                           y(i) = zero
                       end do
                   else
                       do i = 1,n
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==zero) then
                       do i = 1,n
                           y(iy) = zero
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
           if (alpha==zero) return
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when upper triangle of a is stored.
               kplus1 = k + 1
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(i)
                       end do
                       y(j) = y(j) + temp1*a(kplus1,j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       ix = kx
                       iy = ky
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*a(kplus1,j) + alpha*temp2
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
                       temp2 = zero
                       y(j) = y(j) + temp1*a(1,j)
                       l = 1 - j
                       do i = j + 1,min(n,j+k)
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       y(jy) = y(jy) + temp1*a(1,j)
                       l = 1 - j
                       ix = jx
                       iy = jy
                       do i = j + 1,min(n,j+k)
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_dsbmv




     pure module subroutine stdlib_sspr(uplo,n,alpha,x,incx,ap)
     use stdlib_blas_constants_sp
     !! SSPR performs the symmetric rank 1 operation
     !! A := alpha*x*x**T + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n symmetric matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           end if
           if (info/=0) then
               call stdlib_xerbla('SSPR  ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==zero)) return
           ! set the start point in x if the increment is not unity.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with one pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           k = kk
                           do i = 1,j
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                       end if
                       kk = kk + j
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = kx
                           do k = kk,kk + j - 1
                               ap(k) = ap(k) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           k = kk
                           do i = j,n
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = jx
                           do k = kk,kk + n - j
                               ap(k) = ap(k) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_sspr

     pure module subroutine stdlib_dspr(uplo,n,alpha,x,incx,ap)
     use stdlib_blas_constants_dp
     !! DSPR performs the symmetric rank 1 operation
     !! A := alpha*x*x**T + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n symmetric matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           end if
           if (info/=0) then
               call stdlib_xerbla('DSPR  ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==zero)) return
           ! set the start point in x if the increment is not unity.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with one pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           k = kk
                           do i = 1,j
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                       end if
                       kk = kk + j
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = kx
                           do k = kk,kk + j - 1
                               ap(k) = ap(k) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           k = kk
                           do i = j,n
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = jx
                           do k = kk,kk + n - j
                               ap(k) = ap(k) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_dspr




     pure module subroutine stdlib_sspr2(uplo,n,alpha,x,incx,y,incy,ap)
     use stdlib_blas_constants_sp
     !! SSPR2 performs the symmetric rank 2 operation
     !! A := alpha*x*y**T + alpha*y*x**T + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an
     !! n by n symmetric matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
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
           end if
           if (info/=0) then
               call stdlib_xerbla('SSPR2 ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==zero)) return
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with one pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           k = kk
                           do i = 1,j
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                       end if
                       kk = kk + j
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = kx
                           iy = ky
                           do k = kk,kk + j - 1
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           k = kk
                           do i = j,n
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = jx
                           iy = jy
                           do k = kk,kk + n - j
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_sspr2

     pure module subroutine stdlib_dspr2(uplo,n,alpha,x,incx,y,incy,ap)
     use stdlib_blas_constants_dp
     !! DSPR2 performs the symmetric rank 2 operation
     !! A := alpha*x*y**T + alpha*y*x**T + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an
     !! n by n symmetric matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
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
           end if
           if (info/=0) then
               call stdlib_xerbla('DSPR2 ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==zero)) return
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with one pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           k = kk
                           do i = 1,j
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                       end if
                       kk = kk + j
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = kx
                           iy = ky
                           do k = kk,kk + j - 1
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           k = kk
                           do i = j,n
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = jx
                           iy = jy
                           do k = kk,kk + n - j
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_dspr2




     pure module subroutine stdlib_chpmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! CHPMV performs the matrix-vector operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n hermitian matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(in) :: ap(*), x(*)
           complex(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           ! Intrinsic Functions 
           intrinsic :: conjg,real
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 6
           else if (incy==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('CHPMV ',info)
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
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
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when ap contains the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       k = kk
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + temp1*real(ap(kk+j-1),KIND=sp) + alpha*temp2
                       kk = kk + j
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       ix = kx
                       iy = ky
                       do k = kk,kk + j - 2
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*real(ap(kk+j-1),KIND=sp) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  y  when ap contains the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       y(j) = y(j) + temp1*real(ap(kk),KIND=sp)
                       k = kk + 1
                       do i = j + 1,n
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + alpha*temp2
                       kk = kk + (n-j+1)
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       y(jy) = y(jy) + temp1*real(ap(kk),KIND=sp)
                       ix = jx
                       iy = jy
                       do k = kk + 1,kk + n - j
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + (n-j+1)
                   end do
               end if
           end if
           return
     end subroutine stdlib_chpmv

     pure module subroutine stdlib_zhpmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! ZHPMV performs the matrix-vector operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n hermitian matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(in) :: ap(*), x(*)
           complex(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           ! Intrinsic Functions 
           intrinsic :: real,conjg
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 6
           else if (incy==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_xerbla('ZHPMV ',info)
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
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
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when ap contains the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       k = kk
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + temp1*real(ap(kk+j-1),KIND=dp) + alpha*temp2
                       kk = kk + j
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       ix = kx
                       iy = ky
                       do k = kk,kk + j - 2
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*real(ap(kk+j-1),KIND=dp) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  y  when ap contains the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       y(j) = y(j) + temp1*real(ap(kk),KIND=dp)
                       k = kk + 1
                       do i = j + 1,n
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + alpha*temp2
                       kk = kk + (n-j+1)
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       y(jy) = y(jy) + temp1*real(ap(kk),KIND=dp)
                       ix = jx
                       iy = jy
                       do k = kk + 1,kk + n - j
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + (n-j+1)
                   end do
               end if
           end if
           return
     end subroutine stdlib_zhpmv




     pure module subroutine stdlib_chpr(uplo,n,alpha,x,incx,ap)
     use stdlib_blas_constants_sp
     !! CHPR performs the hermitian rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n hermitian matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           ! Intrinsic Functions 
           intrinsic :: conjg,real
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           end if
           if (info/=0) then
               call stdlib_xerbla('CHPR  ',info)
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           k = kk
                           do i = 1,j - 1
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp) + real(x(j)*temp,KIND=sp)
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp)
                       end if
                       kk = kk + j
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           ix = kx
                           do k = kk,kk + j - 2
                               ap(k) = ap(k) + x(ix)*temp
                               ix = ix + incx
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp) + real(x(jx)*temp,KIND=sp)
                                     
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp)
                       end if
                       jx = jx + incx
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           ap(kk) = real(ap(kk),KIND=sp) + real(temp*x(j),KIND=sp)
                           k = kk + 1
                           do i = j + 1,n
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=sp)
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           ap(kk) = real(ap(kk),KIND=sp) + real(temp*x(jx),KIND=sp)
                           ix = jx
                           do k = kk + 1,kk + n - j
                               ix = ix + incx
                               ap(k) = ap(k) + x(ix)*temp
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=sp)
                       end if
                       jx = jx + incx
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_chpr

     pure module subroutine stdlib_zhpr(uplo,n,alpha,x,incx,ap)
     use stdlib_blas_constants_dp
     !! ZHPR performs the hermitian rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n hermitian matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           ! Intrinsic Functions 
           intrinsic :: real,conjg
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           end if
           if (info/=0) then
               call stdlib_xerbla('ZHPR  ',info)
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           k = kk
                           do i = 1,j - 1
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp) + real(x(j)*temp,KIND=dp)
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp)
                       end if
                       kk = kk + j
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           ix = kx
                           do k = kk,kk + j - 2
                               ap(k) = ap(k) + x(ix)*temp
                               ix = ix + incx
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp) + real(x(jx)*temp,KIND=dp)
                                     
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp)
                       end if
                       jx = jx + incx
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           ap(kk) = real(ap(kk),KIND=dp) + real(temp*x(j),KIND=dp)
                           k = kk + 1
                           do i = j + 1,n
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=dp)
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           ap(kk) = real(ap(kk),KIND=dp) + real(temp*x(jx),KIND=dp)
                           ix = jx
                           do k = kk + 1,kk + n - j
                               ix = ix + incx
                               ap(k) = ap(k) + x(ix)*temp
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=dp)
                       end if
                       jx = jx + incx
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_zhpr




     pure module subroutine stdlib_chpr2(uplo,n,alpha,x,incx,y,incy,ap)
     use stdlib_blas_constants_sp
     !! CHPR2 performs the hermitian rank 2 operation
     !! A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an
     !! n by n hermitian matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           ! Intrinsic Functions 
           intrinsic :: conjg,real
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
           end if
           if (info/=0) then
               call stdlib_xerbla('CHPR2 ',info)
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           k = kk
                           do i = 1,j - 1
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp) +real(x(j)*temp1+y(j)*temp2,&
                                     KIND=sp)
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp)
                       end if
                       kk = kk + j
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           ix = kx
                           iy = ky
                           do k = kk,kk + j - 2
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp) +real(x(jx)*temp1+y(jy)*temp2,&
                                     KIND=sp)
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           ap(kk) = real(ap(kk),KIND=sp) +real(x(j)*temp1+y(j)*temp2,KIND=sp)
                                     
                           k = kk + 1
                           do i = j + 1,n
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=sp)
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           ap(kk) = real(ap(kk),KIND=sp) +real(x(jx)*temp1+y(jy)*temp2,KIND=sp)
                                     
                           ix = jx
                           iy = jy
                           do k = kk + 1,kk + n - j
                               ix = ix + incx
                               iy = iy + incy
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=sp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_chpr2

     pure module subroutine stdlib_zhpr2(uplo,n,alpha,x,incx,y,incy,ap)
     use stdlib_blas_constants_dp
     !! ZHPR2 performs the hermitian rank 2 operation
     !! A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an
     !! n by n hermitian matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           ! Intrinsic Functions 
           intrinsic :: real,conjg
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
           end if
           if (info/=0) then
               call stdlib_xerbla('ZHPR2 ',info)
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           k = kk
                           do i = 1,j - 1
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp) +real(x(j)*temp1+y(j)*temp2,&
                                     KIND=dp)
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp)
                       end if
                       kk = kk + j
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           ix = kx
                           iy = ky
                           do k = kk,kk + j - 2
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp) +real(x(jx)*temp1+y(jy)*temp2,&
                                     KIND=dp)
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           ap(kk) = real(ap(kk),KIND=dp) +real(x(j)*temp1+y(j)*temp2,KIND=dp)
                                     
                           k = kk + 1
                           do i = j + 1,n
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=dp)
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           ap(kk) = real(ap(kk),KIND=dp) +real(x(jx)*temp1+y(jy)*temp2,KIND=dp)
                                     
                           ix = jx
                           iy = jy
                           do k = kk + 1,kk + n - j
                               ix = ix + incx
                               iy = iy + incy
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=dp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_zhpr2




     pure module subroutine stdlib_I64_sspmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! SSPMV performs the matrix-vector operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(in) :: ap(*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 6
           else if (incy==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('SSPMV ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. ((alpha==zero).and. (beta==one))) return
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with one pass through ap.
           ! first form  y := beta*y.
           if (beta/=one) then
               if (incy==1) then
                   if (beta==zero) then
                       do i = 1,n
                           y(i) = zero
                       end do
                   else
                       do i = 1,n
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==zero) then
                       do i = 1,n
                           y(iy) = zero
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
           if (alpha==zero) return
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when ap contains the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       k = kk
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + temp1*ap(kk+j-1) + alpha*temp2
                       kk = kk + j
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       ix = kx
                       iy = ky
                       do k = kk,kk + j - 2
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*ap(kk+j-1) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  y  when ap contains the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       y(j) = y(j) + temp1*ap(kk)
                       k = kk + 1
                       do i = j + 1,n
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + alpha*temp2
                       kk = kk + (n-j+1)
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       y(jy) = y(jy) + temp1*ap(kk)
                       ix = jx
                       iy = jy
                       do k = kk + 1,kk + n - j
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + (n-j+1)
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_sspmv

     pure module subroutine stdlib_I64_dspmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! DSPMV performs the matrix-vector operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(in) :: ap(*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 6
           else if (incy==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('DSPMV ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. ((alpha==zero).and. (beta==one))) return
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with one pass through ap.
           ! first form  y := beta*y.
           if (beta/=one) then
               if (incy==1) then
                   if (beta==zero) then
                       do i = 1,n
                           y(i) = zero
                       end do
                   else
                       do i = 1,n
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==zero) then
                       do i = 1,n
                           y(iy) = zero
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
           if (alpha==zero) return
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when ap contains the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       k = kk
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + temp1*ap(kk+j-1) + alpha*temp2
                       kk = kk + j
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       ix = kx
                       iy = ky
                       do k = kk,kk + j - 2
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*ap(kk+j-1) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  y  when ap contains the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       y(j) = y(j) + temp1*ap(kk)
                       k = kk + 1
                       do i = j + 1,n
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + alpha*temp2
                       kk = kk + (n-j+1)
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       y(jy) = y(jy) + temp1*ap(kk)
                       ix = jx
                       iy = jy
                       do k = kk + 1,kk + n - j
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + ap(k)*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + (n-j+1)
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_dspmv




     pure module subroutine stdlib_I64_ssbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! SSBMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric band matrix, with k super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, k, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kplus1, kx, ky, l
           ! Intrinsic Functions 
           intrinsic :: max,min
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
               call stdlib_I64_xerbla('SSBMV ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. ((alpha==zero).and. (beta==one))) return
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
           ! are accessed sequentially with one pass through a.
           ! first form  y := beta*y.
           if (beta/=one) then
               if (incy==1) then
                   if (beta==zero) then
                       do i = 1,n
                           y(i) = zero
                       end do
                   else
                       do i = 1,n
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==zero) then
                       do i = 1,n
                           y(iy) = zero
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
           if (alpha==zero) return
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when upper triangle of a is stored.
               kplus1 = k + 1
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(i)
                       end do
                       y(j) = y(j) + temp1*a(kplus1,j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       ix = kx
                       iy = ky
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*a(kplus1,j) + alpha*temp2
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
                       temp2 = zero
                       y(j) = y(j) + temp1*a(1,j)
                       l = 1 - j
                       do i = j + 1,min(n,j+k)
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       y(jy) = y(jy) + temp1*a(1,j)
                       l = 1 - j
                       ix = jx
                       iy = jy
                       do i = j + 1,min(n,j+k)
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_ssbmv

     pure module subroutine stdlib_I64_dsbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! DSBMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric band matrix, with k super-diagonals.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, k, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kplus1, kx, ky, l
           ! Intrinsic Functions 
           intrinsic :: max,min
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
               call stdlib_I64_xerbla('DSBMV ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. ((alpha==zero).and. (beta==one))) return
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
           ! are accessed sequentially with one pass through a.
           ! first form  y := beta*y.
           if (beta/=one) then
               if (incy==1) then
                   if (beta==zero) then
                       do i = 1,n
                           y(i) = zero
                       end do
                   else
                       do i = 1,n
                           y(i) = beta*y(i)
                       end do
                   end if
               else
                   iy = ky
                   if (beta==zero) then
                       do i = 1,n
                           y(iy) = zero
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
           if (alpha==zero) return
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when upper triangle of a is stored.
               kplus1 = k + 1
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(i)
                       end do
                       y(j) = y(j) + temp1*a(kplus1,j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       ix = kx
                       iy = ky
                       l = kplus1 - j
                       do i = max(1,j-k),j - 1
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*a(kplus1,j) + alpha*temp2
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
                       temp2 = zero
                       y(j) = y(j) + temp1*a(1,j)
                       l = 1 - j
                       do i = j + 1,min(n,j+k)
                           y(i) = y(i) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       y(jy) = y(jy) + temp1*a(1,j)
                       l = 1 - j
                       ix = jx
                       iy = jy
                       do i = j + 1,min(n,j+k)
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(l+i,j)
                           temp2 = temp2 + a(l+i,j)*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_dsbmv




     pure module subroutine stdlib_I64_sspr(uplo,n,alpha,x,incx,ap)
     use stdlib_blas_constants_sp
     !! SSPR performs the symmetric rank 1 operation
     !! A := alpha*x*x**T + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n symmetric matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('SSPR  ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==zero)) return
           ! set the start point in x if the increment is not unity.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with one pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           k = kk
                           do i = 1,j
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                       end if
                       kk = kk + j
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = kx
                           do k = kk,kk + j - 1
                               ap(k) = ap(k) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           k = kk
                           do i = j,n
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = jx
                           do k = kk,kk + n - j
                               ap(k) = ap(k) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_sspr

     pure module subroutine stdlib_I64_dspr(uplo,n,alpha,x,incx,ap)
     use stdlib_blas_constants_dp
     !! DSPR performs the symmetric rank 1 operation
     !! A := alpha*x*x**T + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n symmetric matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('DSPR  ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==zero)) return
           ! set the start point in x if the increment is not unity.
           if (incx<=0) then
               kx = 1 - (n-1)*incx
           else if (incx/=1) then
               kx = 1
           end if
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with one pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           k = kk
                           do i = 1,j
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                       end if
                       kk = kk + j
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = kx
                           do k = kk,kk + j - 1
                               ap(k) = ap(k) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           k = kk
                           do i = j,n
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = jx
                           do k = kk,kk + n - j
                               ap(k) = ap(k) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_dspr




     pure module subroutine stdlib_I64_sspr2(uplo,n,alpha,x,incx,y,incy,ap)
     use stdlib_blas_constants_sp
     !! SSPR2 performs the symmetric rank 2 operation
     !! A := alpha*x*y**T + alpha*y*x**T + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an
     !! n by n symmetric matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
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
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('SSPR2 ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==zero)) return
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with one pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           k = kk
                           do i = 1,j
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                       end if
                       kk = kk + j
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = kx
                           iy = ky
                           do k = kk,kk + j - 1
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           k = kk
                           do i = j,n
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = jx
                           iy = jy
                           do k = kk,kk + n - j
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_sspr2

     pure module subroutine stdlib_I64_dspr2(uplo,n,alpha,x,incx,y,incy,ap)
     use stdlib_blas_constants_dp
     !! DSPR2 performs the symmetric rank 2 operation
     !! A := alpha*x*y**T + alpha*y*x**T + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an
     !! n by n symmetric matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
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
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('DSPR2 ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (alpha==zero)) return
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with one pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           k = kk
                           do i = 1,j
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                       end if
                       kk = kk + j
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = kx
                           iy = ky
                           do k = kk,kk + j - 1
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           k = kk
                           do i = j,n
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = jx
                           iy = jy
                           do k = kk,kk + n - j
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_dspr2




     pure module subroutine stdlib_I64_chpmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! CHPMV performs the matrix-vector operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n hermitian matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(in) :: ap(*), x(*)
           complex(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           ! Intrinsic Functions 
           intrinsic :: conjg,real
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 6
           else if (incy==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CHPMV ',info)
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
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
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when ap contains the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       k = kk
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + temp1*real(ap(kk+j-1),KIND=sp) + alpha*temp2
                       kk = kk + j
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       ix = kx
                       iy = ky
                       do k = kk,kk + j - 2
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*real(ap(kk+j-1),KIND=sp) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  y  when ap contains the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       y(j) = y(j) + temp1*real(ap(kk),KIND=sp)
                       k = kk + 1
                       do i = j + 1,n
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + alpha*temp2
                       kk = kk + (n-j+1)
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       y(jy) = y(jy) + temp1*real(ap(kk),KIND=sp)
                       ix = jx
                       iy = jy
                       do k = kk + 1,kk + n - j
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + (n-j+1)
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_chpmv

     pure module subroutine stdlib_I64_zhpmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! ZHPMV performs the matrix-vector operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n hermitian matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(in) :: ap(*), x(*)
           complex(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           ! Intrinsic Functions 
           intrinsic :: real,conjg
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 6
           else if (incy==0) then
               info = 9
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZHPMV ',info)
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
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
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  y  when ap contains the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       k = kk
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + temp1*real(ap(kk+j-1),KIND=dp) + alpha*temp2
                       kk = kk + j
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       ix = kx
                       iy = ky
                       do k = kk,kk + j - 2
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*real(ap(kk+j-1),KIND=dp) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  y  when ap contains the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = czero
                       y(j) = y(j) + temp1*real(ap(kk),KIND=dp)
                       k = kk + 1
                       do i = j + 1,n
                           y(i) = y(i) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(i)
                           k = k + 1
                       end do
                       y(j) = y(j) + alpha*temp2
                       kk = kk + (n-j+1)
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = czero
                       y(jy) = y(jy) + temp1*real(ap(kk),KIND=dp)
                       ix = jx
                       iy = jy
                       do k = kk + 1,kk + n - j
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*ap(k)
                           temp2 = temp2 + conjg(ap(k))*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + (n-j+1)
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_zhpmv




     pure module subroutine stdlib_I64_chpr(uplo,n,alpha,x,incx,ap)
     use stdlib_blas_constants_sp
     !! CHPR performs the hermitian rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n hermitian matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           ! Intrinsic Functions 
           intrinsic :: conjg,real
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CHPR  ',info)
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           k = kk
                           do i = 1,j - 1
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp) + real(x(j)*temp,KIND=sp)
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp)
                       end if
                       kk = kk + j
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           ix = kx
                           do k = kk,kk + j - 2
                               ap(k) = ap(k) + x(ix)*temp
                               ix = ix + incx
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp) + real(x(jx)*temp,KIND=sp)
                                     
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp)
                       end if
                       jx = jx + incx
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           ap(kk) = real(ap(kk),KIND=sp) + real(temp*x(j),KIND=sp)
                           k = kk + 1
                           do i = j + 1,n
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=sp)
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           ap(kk) = real(ap(kk),KIND=sp) + real(temp*x(jx),KIND=sp)
                           ix = jx
                           do k = kk + 1,kk + n - j
                               ix = ix + incx
                               ap(k) = ap(k) + x(ix)*temp
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=sp)
                       end if
                       jx = jx + incx
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_chpr

     pure module subroutine stdlib_I64_zhpr(uplo,n,alpha,x,incx,ap)
     use stdlib_blas_constants_dp
     !! ZHPR performs the hermitian rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n hermitian matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           ! Intrinsic Functions 
           intrinsic :: real,conjg
           ! test the input parameters.
           info = 0
           if (.not.stdlib_lsame(uplo,'U') .and. .not.stdlib_lsame(uplo,'L')) then
               info = 1
           else if (n<0) then
               info = 2
           else if (incx==0) then
               info = 5
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZHPR  ',info)
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           k = kk
                           do i = 1,j - 1
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp) + real(x(j)*temp,KIND=dp)
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp)
                       end if
                       kk = kk + j
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           ix = kx
                           do k = kk,kk + j - 2
                               ap(k) = ap(k) + x(ix)*temp
                               ix = ix + incx
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp) + real(x(jx)*temp,KIND=dp)
                                     
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp)
                       end if
                       jx = jx + incx
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=czero) then
                           temp = alpha*conjg(x(j))
                           ap(kk) = real(ap(kk),KIND=dp) + real(temp*x(j),KIND=dp)
                           k = kk + 1
                           do i = j + 1,n
                               ap(k) = ap(k) + x(i)*temp
                               k = k + 1
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=dp)
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=czero) then
                           temp = alpha*conjg(x(jx))
                           ap(kk) = real(ap(kk),KIND=dp) + real(temp*x(jx),KIND=dp)
                           ix = jx
                           do k = kk + 1,kk + n - j
                               ix = ix + incx
                               ap(k) = ap(k) + x(ix)*temp
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=dp)
                       end if
                       jx = jx + incx
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_zhpr




     pure module subroutine stdlib_I64_chpr2(uplo,n,alpha,x,incx,y,incy,ap)
     use stdlib_blas_constants_sp
     !! CHPR2 performs the hermitian rank 2 operation
     !! A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an
     !! n by n hermitian matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           ! Intrinsic Functions 
           intrinsic :: conjg,real
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
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CHPR2 ',info)
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           k = kk
                           do i = 1,j - 1
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp) +real(x(j)*temp1+y(j)*temp2,&
                                     KIND=sp)
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp)
                       end if
                       kk = kk + j
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           ix = kx
                           iy = ky
                           do k = kk,kk + j - 2
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp) +real(x(jx)*temp1+y(jy)*temp2,&
                                     KIND=sp)
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=sp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           ap(kk) = real(ap(kk),KIND=sp) +real(x(j)*temp1+y(j)*temp2,KIND=sp)
                                     
                           k = kk + 1
                           do i = j + 1,n
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=sp)
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           ap(kk) = real(ap(kk),KIND=sp) +real(x(jx)*temp1+y(jy)*temp2,KIND=sp)
                                     
                           ix = jx
                           iy = jy
                           do k = kk + 1,kk + n - j
                               ix = ix + incx
                               iy = iy + incy
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=sp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_chpr2

     pure module subroutine stdlib_I64_zhpr2(uplo,n,alpha,x,incx,y,incy,ap)
     use stdlib_blas_constants_dp
     !! ZHPR2 performs the hermitian rank 2 operation
     !! A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an
     !! n by n hermitian matrix, supplied in packed form.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           ! Intrinsic Functions 
           intrinsic :: real,conjg
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
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZHPR2 ',info)
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
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           kk = 1
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when upper triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           k = kk
                           do i = 1,j - 1
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp) +real(x(j)*temp1+y(j)*temp2,&
                                     KIND=dp)
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp)
                       end if
                       kk = kk + j
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           ix = kx
                           iy = ky
                           do k = kk,kk + j - 2
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp) +real(x(jx)*temp1+y(jy)*temp2,&
                                     KIND=dp)
                       else
                           ap(kk+j-1) = real(ap(kk+j-1),KIND=dp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + j
                   end do
               end if
           else
              ! form  a  when lower triangle is stored in ap.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=czero) .or. (y(j)/=czero)) then
                           temp1 = alpha*conjg(y(j))
                           temp2 = conjg(alpha*x(j))
                           ap(kk) = real(ap(kk),KIND=dp) +real(x(j)*temp1+y(j)*temp2,KIND=dp)
                                     
                           k = kk + 1
                           do i = j + 1,n
                               ap(k) = ap(k) + x(i)*temp1 + y(i)*temp2
                               k = k + 1
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=dp)
                       end if
                       kk = kk + n - j + 1
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=czero) .or. (y(jy)/=czero)) then
                           temp1 = alpha*conjg(y(jy))
                           temp2 = conjg(alpha*x(jx))
                           ap(kk) = real(ap(kk),KIND=dp) +real(x(jx)*temp1+y(jy)*temp2,KIND=dp)
                                     
                           ix = jx
                           iy = jy
                           do k = kk + 1,kk + n - j
                               ix = ix + incx
                               iy = iy + incy
                               ap(k) = ap(k) + x(ix)*temp1 + y(iy)*temp2
                           end do
                       else
                           ap(kk) = real(ap(kk),KIND=dp)
                       end if
                       jx = jx + incx
                       jy = jy + incy
                       kk = kk + n - j + 1
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_zhpr2



end submodule stdlib_blas_level2_pac
