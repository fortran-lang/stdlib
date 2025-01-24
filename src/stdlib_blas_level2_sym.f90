submodule(stdlib_blas) stdlib_blas_level2_sym
  implicit none


  contains

     pure module subroutine stdlib_ssymv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! SSYMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: max
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
               call stdlib_xerbla('SSYMV ',info)
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
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through the triangular part
           ! of a.
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
              ! form  y  when a is stored in upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(i)
                       end do
                       y(j) = y(j) + temp1*a(j,j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       ix = kx
                       iy = ky
                       do i = 1,j - 1
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*a(j,j) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  y  when a is stored in lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       y(j) = y(j) + temp1*a(j,j)
                       do i = j + 1,n
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       y(jy) = y(jy) + temp1*a(j,j)
                       ix = jx
                       iy = jy
                       do i = j + 1,n
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_ssymv

     pure module subroutine stdlib_dsymv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! DSYMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: max
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
               call stdlib_xerbla('DSYMV ',info)
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
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through the triangular part
           ! of a.
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
              ! form  y  when a is stored in upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(i)
                       end do
                       y(j) = y(j) + temp1*a(j,j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       ix = kx
                       iy = ky
                       do i = 1,j - 1
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*a(j,j) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  y  when a is stored in lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       y(j) = y(j) + temp1*a(j,j)
                       do i = j + 1,n
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       y(jy) = y(jy) + temp1*a(j,j)
                       ix = jx
                       iy = jy
                       do i = j + 1,n
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_dsymv




     pure module subroutine stdlib_ssyr(uplo,n,alpha,x,incx,a,lda)
     use stdlib_blas_constants_sp
     !! SSYR performs the symmetric rank 1 operation
     !! A := alpha*x*x**T + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n symmetric matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kx
           ! Intrinsic Functions 
           intrinsic :: max
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
               call stdlib_xerbla('SSYR  ',info)
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
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in upper triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           do i = 1,j
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = kx
                           do i = 1,j
                               a(i,j) = a(i,j) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                   end do
               end if
           else
              ! form  a  when a is stored in lower triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           do i = j,n
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = jx
                           do i = j,n
                               a(i,j) = a(i,j) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_ssyr

     pure module subroutine stdlib_dsyr(uplo,n,alpha,x,incx,a,lda)
     use stdlib_blas_constants_dp
     !! DSYR performs the symmetric rank 1 operation
     !! A := alpha*x*x**T + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n symmetric matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, ix, j, jx, kx
           ! Intrinsic Functions 
           intrinsic :: max
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
               call stdlib_xerbla('DSYR  ',info)
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
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in upper triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           do i = 1,j
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = kx
                           do i = 1,j
                               a(i,j) = a(i,j) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                   end do
               end if
           else
              ! form  a  when a is stored in lower triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           do i = j,n
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = jx
                           do i = j,n
                               a(i,j) = a(i,j) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_dsyr




     pure module subroutine stdlib_ssyr2(uplo,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_sp
     !! SSYR2 performs the symmetric rank 2 operation
     !! A := alpha*x*y**T + alpha*y*x**T + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an n
     !! by n symmetric matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: max
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
               call stdlib_xerbla('SSYR2 ',info)
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
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           do i = 1,j
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = kx
                           iy = ky
                           do i = 1,j
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  a  when a is stored in the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           do i = j,n
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = jx
                           iy = jy
                           do i = j,n
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_ssyr2

     pure module subroutine stdlib_dsyr2(uplo,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_dp
     !! DSYR2 performs the symmetric rank 2 operation
     !! A := alpha*x*y**T + alpha*y*x**T + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an n
     !! by n symmetric matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp1, temp2
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: max
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
               call stdlib_xerbla('DSYR2 ',info)
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
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           do i = 1,j
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = kx
                           iy = ky
                           do i = 1,j
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  a  when a is stored in the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           do i = j,n
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = jx
                           iy = jy
                           do i = j,n
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_dsyr2




     pure module subroutine stdlib_I64_ssymv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_sp
     !! SSYMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: max
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
               call stdlib_I64_xerbla('SSYMV ',info)
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
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through the triangular part
           ! of a.
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
              ! form  y  when a is stored in upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(i)
                       end do
                       y(j) = y(j) + temp1*a(j,j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       ix = kx
                       iy = ky
                       do i = 1,j - 1
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*a(j,j) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  y  when a is stored in lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       y(j) = y(j) + temp1*a(j,j)
                       do i = j + 1,n
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       y(jy) = y(jy) + temp1*a(j,j)
                       ix = jx
                       iy = jy
                       do i = j + 1,n
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_ssymv

     pure module subroutine stdlib_I64_dsymv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
     use stdlib_blas_constants_dp
     !! DSYMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: max
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
               call stdlib_I64_xerbla('DSYMV ',info)
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
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through the triangular part
           ! of a.
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
              ! form  y  when a is stored in upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       do i = 1,j - 1
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(i)
                       end do
                       y(j) = y(j) + temp1*a(j,j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       ix = kx
                       iy = ky
                       do i = 1,j - 1
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(ix)
                           ix = ix + incx
                           iy = iy + incy
                       end do
                       y(jy) = y(jy) + temp1*a(j,j) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  y  when a is stored in lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       temp1 = alpha*x(j)
                       temp2 = zero
                       y(j) = y(j) + temp1*a(j,j)
                       do i = j + 1,n
                           y(i) = y(i) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(i)
                       end do
                       y(j) = y(j) + alpha*temp2
                   end do
               else
                   jx = kx
                   jy = ky
                   do j = 1,n
                       temp1 = alpha*x(jx)
                       temp2 = zero
                       y(jy) = y(jy) + temp1*a(j,j)
                       ix = jx
                       iy = jy
                       do i = j + 1,n
                           ix = ix + incx
                           iy = iy + incy
                           y(iy) = y(iy) + temp1*a(i,j)
                           temp2 = temp2 + a(i,j)*x(ix)
                       end do
                       y(jy) = y(jy) + alpha*temp2
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_dsymv




     pure module subroutine stdlib_I64_ssyr(uplo,n,alpha,x,incx,a,lda)
     use stdlib_blas_constants_sp
     !! SSYR performs the symmetric rank 1 operation
     !! A := alpha*x*x**T + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n symmetric matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kx
           ! Intrinsic Functions 
           intrinsic :: max
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
               call stdlib_I64_xerbla('SSYR  ',info)
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
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in upper triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           do i = 1,j
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = kx
                           do i = 1,j
                               a(i,j) = a(i,j) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                   end do
               end if
           else
              ! form  a  when a is stored in lower triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           do i = j,n
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = jx
                           do i = j,n
                               a(i,j) = a(i,j) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_ssyr

     pure module subroutine stdlib_I64_dsyr(uplo,n,alpha,x,incx,a,lda)
     use stdlib_blas_constants_dp
     !! DSYR performs the symmetric rank 1 operation
     !! A := alpha*x*x**T + A,
     !! where alpha is a real scalar, x is an n element vector and A is an
     !! n by n symmetric matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp64) :: i, info, ix, j, jx, kx
           ! Intrinsic Functions 
           intrinsic :: max
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
               call stdlib_I64_xerbla('DSYR  ',info)
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
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in upper triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           do i = 1,j
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = kx
                           do i = 1,j
                               a(i,j) = a(i,j) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                   end do
               end if
           else
              ! form  a  when a is stored in lower triangle.
               if (incx==1) then
                   do j = 1,n
                       if (x(j)/=zero) then
                           temp = alpha*x(j)
                           do i = j,n
                               a(i,j) = a(i,j) + x(i)*temp
                           end do
                       end if
                   end do
               else
                   jx = kx
                   do j = 1,n
                       if (x(jx)/=zero) then
                           temp = alpha*x(jx)
                           ix = jx
                           do i = j,n
                               a(i,j) = a(i,j) + x(ix)*temp
                               ix = ix + incx
                           end do
                       end if
                       jx = jx + incx
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_dsyr




     pure module subroutine stdlib_I64_ssyr2(uplo,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_sp
     !! SSYR2 performs the symmetric rank 2 operation
     !! A := alpha*x*y**T + alpha*y*x**T + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an n
     !! by n symmetric matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: max
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
               call stdlib_I64_xerbla('SSYR2 ',info)
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
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           do i = 1,j
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = kx
                           iy = ky
                           do i = 1,j
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  a  when a is stored in the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           do i = j,n
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = jx
                           iy = jy
                           do i = j,n
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_ssyr2

     pure module subroutine stdlib_I64_dsyr2(uplo,n,alpha,x,incx,y,incy,a,lda)
     use stdlib_blas_constants_dp
     !! DSYR2 performs the symmetric rank 2 operation
     !! A := alpha*x*y**T + alpha*y*x**T + A,
     !! where alpha is a scalar, x and y are n element vectors and A is an n
     !! by n symmetric matrix.
        ! -- reference blas level2 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp64), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: temp1, temp2
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky
           ! Intrinsic Functions 
           intrinsic :: max
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
               call stdlib_I64_xerbla('DSYR2 ',info)
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
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with one pass through the triangular part
           ! of a.
           if (stdlib_lsame(uplo,'U')) then
              ! form  a  when a is stored in the upper triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           do i = 1,j
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = kx
                           iy = ky
                           do i = 1,j
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           else
              ! form  a  when a is stored in the lower triangle.
               if ((incx==1) .and. (incy==1)) then
                   do j = 1,n
                       if ((x(j)/=zero) .or. (y(j)/=zero)) then
                           temp1 = alpha*y(j)
                           temp2 = alpha*x(j)
                           do i = j,n
                               a(i,j) = a(i,j) + x(i)*temp1 + y(i)*temp2
                           end do
                       end if
                   end do
               else
                   do j = 1,n
                       if ((x(jx)/=zero) .or. (y(jy)/=zero)) then
                           temp1 = alpha*y(jy)
                           temp2 = alpha*x(jx)
                           ix = jx
                           iy = jy
                           do i = j,n
                               a(i,j) = a(i,j) + x(ix)*temp1 + y(iy)*temp2
                               ix = ix + incx
                               iy = iy + incy
                           end do
                       end if
                       jx = jx + incx
                       jy = jy + incy
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_dsyr2



end submodule stdlib_blas_level2_sym
