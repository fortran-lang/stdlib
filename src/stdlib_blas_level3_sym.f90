submodule(stdlib_blas) stdlib_blas_level3_sym
  implicit none


  contains

     pure module subroutine stdlib_ssyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! SSYRK performs one of the symmetric rank k operations
     !! C := alpha*A*A**T + beta*C,
     !! or
     !! C := alpha*A**T*A + beta*C,
     !! where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
     !! and  A  is an  n by k  matrix in the first case and a  k by n  matrix
     !! in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'T')) .and.(&
                     .not.stdlib_lsame(trans,'C'))) then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldc<max(1,n)) then
               info = 10
           end if
           if (info/=0) then
               call stdlib_xerbla('SSYRK ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==zero).or.(k==0)).and. (beta==one))) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               if (upper) then
                   if (beta==zero) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               else
                   if (beta==zero) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*a**t + beta*c.
               if (upper) then
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if (a(j,l)/=zero) then
                               temp = alpha*a(j,l)
                               do i = 1,j
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==zero) then
                           do i = j,n
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if (a(j,l)/=zero) then
                               temp = alpha*a(j,l)
                               do i = j,n
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**t*a + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j
                           temp = zero
                           do l = 1,k
                               temp = temp + a(l,i)*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = j,n
                           temp = zero
                           do l = 1,k
                               temp = temp + a(l,i)*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_ssyrk

     pure module subroutine stdlib_dsyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! DSYRK performs one of the symmetric rank k operations
     !! C := alpha*A*A**T + beta*C,
     !! or
     !! C := alpha*A**T*A + beta*C,
     !! where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
     !! and  A  is an  n by k  matrix in the first case and a  k by n  matrix
     !! in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'T')) .and.(&
                     .not.stdlib_lsame(trans,'C'))) then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldc<max(1,n)) then
               info = 10
           end if
           if (info/=0) then
               call stdlib_xerbla('DSYRK ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==zero).or.(k==0)).and. (beta==one))) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               if (upper) then
                   if (beta==zero) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               else
                   if (beta==zero) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*a**t + beta*c.
               if (upper) then
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if (a(j,l)/=zero) then
                               temp = alpha*a(j,l)
                               do i = 1,j
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==zero) then
                           do i = j,n
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if (a(j,l)/=zero) then
                               temp = alpha*a(j,l)
                               do i = j,n
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**t*a + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j
                           temp = zero
                           do l = 1,k
                               temp = temp + a(l,i)*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = j,n
                           temp = zero
                           do l = 1,k
                               temp = temp + a(l,i)*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_dsyrk


     pure module subroutine stdlib_csyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! CSYRK performs one of the symmetric rank k operations
     !! C := alpha*A*A**T + beta*C,
     !! or
     !! C := alpha*A**T*A + beta*C,
     !! where  alpha and beta  are scalars,  C is an  n by n symmetric matrix
     !! and  A  is an  n by k  matrix in the first case and a  k by n  matrix
     !! in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'T'))) &
                     then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldc<max(1,n)) then
               info = 10
           end if
           if (info/=0) then
               call stdlib_xerbla('CSYRK ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==czero).or.(k==0)).and. (beta==cone))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (upper) then
                   if (beta==czero) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               else
                   if (beta==czero) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*a**t + beta*c.
               if (upper) then
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if (a(j,l)/=czero) then
                               temp = alpha*a(j,l)
                               do i = 1,j
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==czero) then
                           do i = j,n
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if (a(j,l)/=czero) then
                               temp = alpha*a(j,l)
                               do i = j,n
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**t*a + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*a(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = j,n
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*a(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_csyrk

     pure module subroutine stdlib_zsyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! ZSYRK performs one of the symmetric rank k operations
     !! C := alpha*A*A**T + beta*C,
     !! or
     !! C := alpha*A**T*A + beta*C,
     !! where  alpha and beta  are scalars,  C is an  n by n symmetric matrix
     !! and  A  is an  n by k  matrix in the first case and a  k by n  matrix
     !! in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'T'))) &
                     then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldc<max(1,n)) then
               info = 10
           end if
           if (info/=0) then
               call stdlib_xerbla('ZSYRK ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==czero).or.(k==0)).and. (beta==cone))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (upper) then
                   if (beta==czero) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               else
                   if (beta==czero) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*a**t + beta*c.
               if (upper) then
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if (a(j,l)/=czero) then
                               temp = alpha*a(j,l)
                               do i = 1,j
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==czero) then
                           do i = j,n
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if (a(j,l)/=czero) then
                               temp = alpha*a(j,l)
                               do i = j,n
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**t*a + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*a(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = j,n
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*a(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_zsyrk




     pure module subroutine stdlib_ssyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! SSYR2K performs one of the symmetric rank 2k operations
     !! C := alpha*A*B**T + alpha*B*A**T + beta*C,
     !! or
     !! C := alpha*A**T*B + alpha*B**T*A + beta*C,
     !! where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
     !! and  A and B  are  n by k  matrices  in the  first  case  and  k by n
     !! matrices in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
           real(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(sp) :: temp1, temp2
           integer(ilp) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'T')) .and.(&
                     .not.stdlib_lsame(trans,'C'))) then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,nrowa)) then
               info = 9
           else if (ldc<max(1,n)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_xerbla('SSYR2K',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==zero).or.(k==0)).and. (beta==one))) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               if (upper) then
                   if (beta==zero) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               else
                   if (beta==zero) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*b**t + alpha*b*a**t + c.
               if (upper) then
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if ((a(j,l)/=zero) .or. (b(j,l)/=zero)) then
                               temp1 = alpha*b(j,l)
                               temp2 = alpha*a(j,l)
                               do i = 1,j
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==zero) then
                           do i = j,n
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if ((a(j,l)/=zero) .or. (b(j,l)/=zero)) then
                               temp1 = alpha*b(j,l)
                               temp2 = alpha*a(j,l)
                               do i = j,n
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**t*b + alpha*b**t*a + c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j
                           temp1 = zero
                           temp2 = zero
                           do l = 1,k
                               temp1 = temp1 + a(l,i)*b(l,j)
                               temp2 = temp2 + b(l,i)*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp1 + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + alpha*temp1 +alpha*temp2
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = j,n
                           temp1 = zero
                           temp2 = zero
                           do l = 1,k
                               temp1 = temp1 + a(l,i)*b(l,j)
                               temp2 = temp2 + b(l,i)*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp1 + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + alpha*temp1 +alpha*temp2
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_ssyr2k

     pure module subroutine stdlib_dsyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! DSYR2K performs one of the symmetric rank 2k operations
     !! C := alpha*A*B**T + alpha*B*A**T + beta*C,
     !! or
     !! C := alpha*A**T*B + alpha*B**T*A + beta*C,
     !! where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
     !! and  A and B  are  n by k  matrices  in the  first  case  and  k by n
     !! matrices in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
           real(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(dp) :: temp1, temp2
           integer(ilp) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'T')) .and.(&
                     .not.stdlib_lsame(trans,'C'))) then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,nrowa)) then
               info = 9
           else if (ldc<max(1,n)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_xerbla('DSYR2K',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==zero).or.(k==0)).and. (beta==one))) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               if (upper) then
                   if (beta==zero) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               else
                   if (beta==zero) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*b**t + alpha*b*a**t + c.
               if (upper) then
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if ((a(j,l)/=zero) .or. (b(j,l)/=zero)) then
                               temp1 = alpha*b(j,l)
                               temp2 = alpha*a(j,l)
                               do i = 1,j
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==zero) then
                           do i = j,n
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if ((a(j,l)/=zero) .or. (b(j,l)/=zero)) then
                               temp1 = alpha*b(j,l)
                               temp2 = alpha*a(j,l)
                               do i = j,n
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**t*b + alpha*b**t*a + c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j
                           temp1 = zero
                           temp2 = zero
                           do l = 1,k
                               temp1 = temp1 + a(l,i)*b(l,j)
                               temp2 = temp2 + b(l,i)*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp1 + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + alpha*temp1 +alpha*temp2
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = j,n
                           temp1 = zero
                           temp2 = zero
                           do l = 1,k
                               temp1 = temp1 + a(l,i)*b(l,j)
                               temp2 = temp2 + b(l,i)*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp1 + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + alpha*temp1 +alpha*temp2
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_dsyr2k


     pure module subroutine stdlib_csyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! CSYR2K performs one of the symmetric rank 2k operations
     !! C := alpha*A*B**T + alpha*B*A**T + beta*C,
     !! or
     !! C := alpha*A**T*B + alpha*B**T*A + beta*C,
     !! where  alpha and beta  are scalars,  C is an  n by n symmetric matrix
     !! and  A and B  are  n by k  matrices  in the  first  case  and  k by n
     !! matrices in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'T'))) &
                     then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,nrowa)) then
               info = 9
           else if (ldc<max(1,n)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_xerbla('CSYR2K',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==czero).or.(k==0)).and. (beta==cone))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (upper) then
                   if (beta==czero) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               else
                   if (beta==czero) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*b**t + alpha*b*a**t + c.
               if (upper) then
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if ((a(j,l)/=czero) .or. (b(j,l)/=czero)) then
                               temp1 = alpha*b(j,l)
                               temp2 = alpha*a(j,l)
                               do i = 1,j
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==czero) then
                           do i = j,n
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if ((a(j,l)/=czero) .or. (b(j,l)/=czero)) then
                               temp1 = alpha*b(j,l)
                               temp2 = alpha*a(j,l)
                               do i = j,n
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**t*b + alpha*b**t*a + c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j
                           temp1 = czero
                           temp2 = czero
                           do l = 1,k
                               temp1 = temp1 + a(l,i)*b(l,j)
                               temp2 = temp2 + b(l,i)*a(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp1 + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + alpha*temp1 +alpha*temp2
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = j,n
                           temp1 = czero
                           temp2 = czero
                           do l = 1,k
                               temp1 = temp1 + a(l,i)*b(l,j)
                               temp2 = temp2 + b(l,i)*a(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp1 + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + alpha*temp1 +alpha*temp2
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_csyr2k

     pure module subroutine stdlib_zsyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! ZSYR2K performs one of the symmetric rank 2k operations
     !! C := alpha*A*B**T + alpha*B*A**T + beta*C,
     !! or
     !! C := alpha*A**T*B + alpha*B**T*A + beta*C,
     !! where  alpha and beta  are scalars,  C is an  n by n symmetric matrix
     !! and  A and B  are  n by k  matrices  in the  first  case  and  k by n
     !! matrices in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'T'))) &
                     then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,nrowa)) then
               info = 9
           else if (ldc<max(1,n)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_xerbla('ZSYR2K',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==czero).or.(k==0)).and. (beta==cone))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (upper) then
                   if (beta==czero) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               else
                   if (beta==czero) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*b**t + alpha*b*a**t + c.
               if (upper) then
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,j
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if ((a(j,l)/=czero) .or. (b(j,l)/=czero)) then
                               temp1 = alpha*b(j,l)
                               temp2 = alpha*a(j,l)
                               do i = 1,j
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==czero) then
                           do i = j,n
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = j,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           if ((a(j,l)/=czero) .or. (b(j,l)/=czero)) then
                               temp1 = alpha*b(j,l)
                               temp2 = alpha*a(j,l)
                               do i = j,n
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**t*b + alpha*b**t*a + c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j
                           temp1 = czero
                           temp2 = czero
                           do l = 1,k
                               temp1 = temp1 + a(l,i)*b(l,j)
                               temp2 = temp2 + b(l,i)*a(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp1 + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + alpha*temp1 +alpha*temp2
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = j,n
                           temp1 = czero
                           temp2 = czero
                           do l = 1,k
                               temp1 = temp1 + a(l,i)*b(l,j)
                               temp2 = temp2 + b(l,i)*a(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp1 + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + alpha*temp1 +alpha*temp2
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_zsyr2k




     pure module subroutine stdlib_ssymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! SSYMM performs one of the matrix-matrix operations
     !! C := alpha*A*B + beta*C,
     !! or
     !! C := alpha*B*A + beta*C,
     !! where alpha and beta are scalars,  A is a symmetric matrix and  B and
     !! C are  m by n matrices.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
           real(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(sp) :: temp1, temp2
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: upper
           
           ! set nrowa as the number of rows of a.
           if (stdlib_lsame(side,'L')) then
               nrowa = m
           else
               nrowa = n
           end if
           upper = stdlib_lsame(uplo,'U')
           ! test the input parameters.
           info = 0
           if ((.not.stdlib_lsame(side,'L')) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,m)) then
               info = 9
           else if (ldc<max(1,m)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_xerbla('SSYMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==zero).and. (beta==one))) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               if (beta==zero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = zero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(side,'L')) then
              ! form  c := alpha*a*b + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,m
                           temp1 = alpha*b(i,j)
                           temp2 = zero
                           do k = 1,i - 1
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*a(k,i)
                           end do
                           if (beta==zero) then
                               c(i,j) = temp1*a(i,i) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*a(i,i) +alpha*temp2
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = m,1,-1
                           temp1 = alpha*b(i,j)
                           temp2 = zero
                           do k = i + 1,m
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*a(k,i)
                           end do
                           if (beta==zero) then
                               c(i,j) = temp1*a(i,i) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*a(i,i) +alpha*temp2
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*b*a + beta*c.
               loop_170: do j = 1,n
                   temp1 = alpha*a(j,j)
                   if (beta==zero) then
                       do i = 1,m
                           c(i,j) = temp1*b(i,j)
                       end do
                   else
                       do i = 1,m
                           c(i,j) = beta*c(i,j) + temp1*b(i,j)
                       end do
                   end if
                   do k = 1,j - 1
                       if (upper) then
                           temp1 = alpha*a(k,j)
                       else
                           temp1 = alpha*a(j,k)
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
                   do k = j + 1,n
                       if (upper) then
                           temp1 = alpha*a(j,k)
                       else
                           temp1 = alpha*a(k,j)
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
               end do loop_170
           end if
           return
     end subroutine stdlib_ssymm

     pure module subroutine stdlib_dsymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! DSYMM performs one of the matrix-matrix operations
     !! C := alpha*A*B + beta*C,
     !! or
     !! C := alpha*B*A + beta*C,
     !! where alpha and beta are scalars,  A is a symmetric matrix and  B and
     !! C are  m by n matrices.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
           real(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(dp) :: temp1, temp2
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: upper
           
           ! set nrowa as the number of rows of a.
           if (stdlib_lsame(side,'L')) then
               nrowa = m
           else
               nrowa = n
           end if
           upper = stdlib_lsame(uplo,'U')
           ! test the input parameters.
           info = 0
           if ((.not.stdlib_lsame(side,'L')) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,m)) then
               info = 9
           else if (ldc<max(1,m)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_xerbla('DSYMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==zero).and. (beta==one))) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               if (beta==zero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = zero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(side,'L')) then
              ! form  c := alpha*a*b + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,m
                           temp1 = alpha*b(i,j)
                           temp2 = zero
                           do k = 1,i - 1
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*a(k,i)
                           end do
                           if (beta==zero) then
                               c(i,j) = temp1*a(i,i) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*a(i,i) +alpha*temp2
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = m,1,-1
                           temp1 = alpha*b(i,j)
                           temp2 = zero
                           do k = i + 1,m
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*a(k,i)
                           end do
                           if (beta==zero) then
                               c(i,j) = temp1*a(i,i) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*a(i,i) +alpha*temp2
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*b*a + beta*c.
               loop_170: do j = 1,n
                   temp1 = alpha*a(j,j)
                   if (beta==zero) then
                       do i = 1,m
                           c(i,j) = temp1*b(i,j)
                       end do
                   else
                       do i = 1,m
                           c(i,j) = beta*c(i,j) + temp1*b(i,j)
                       end do
                   end if
                   do k = 1,j - 1
                       if (upper) then
                           temp1 = alpha*a(k,j)
                       else
                           temp1 = alpha*a(j,k)
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
                   do k = j + 1,n
                       if (upper) then
                           temp1 = alpha*a(j,k)
                       else
                           temp1 = alpha*a(k,j)
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
               end do loop_170
           end if
           return
     end subroutine stdlib_dsymm


     pure module subroutine stdlib_csymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! CSYMM performs one of the matrix-matrix operations
     !! C := alpha*A*B + beta*C,
     !! or
     !! C := alpha*B*A + beta*C,
     !! where  alpha and beta are scalars, A is a symmetric matrix and  B and
     !! C are m by n matrices.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: upper
           
           
           ! set nrowa as the number of rows of a.
           if (stdlib_lsame(side,'L')) then
               nrowa = m
           else
               nrowa = n
           end if
           upper = stdlib_lsame(uplo,'U')
           ! test the input parameters.
           info = 0
           if ((.not.stdlib_lsame(side,'L')) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,m)) then
               info = 9
           else if (ldc<max(1,m)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_xerbla('CSYMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==czero).and. (beta==cone))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (beta==czero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = czero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(side,'L')) then
              ! form  c := alpha*a*b + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,m
                           temp1 = alpha*b(i,j)
                           temp2 = czero
                           do k = 1,i - 1
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*a(k,i)
                           end do
                           if (beta==czero) then
                               c(i,j) = temp1*a(i,i) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*a(i,i) +alpha*temp2
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = m,1,-1
                           temp1 = alpha*b(i,j)
                           temp2 = czero
                           do k = i + 1,m
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*a(k,i)
                           end do
                           if (beta==czero) then
                               c(i,j) = temp1*a(i,i) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*a(i,i) +alpha*temp2
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*b*a + beta*c.
               loop_170: do j = 1,n
                   temp1 = alpha*a(j,j)
                   if (beta==czero) then
                       do i = 1,m
                           c(i,j) = temp1*b(i,j)
                       end do
                   else
                       do i = 1,m
                           c(i,j) = beta*c(i,j) + temp1*b(i,j)
                       end do
                   end if
                   do k = 1,j - 1
                       if (upper) then
                           temp1 = alpha*a(k,j)
                       else
                           temp1 = alpha*a(j,k)
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
                   do k = j + 1,n
                       if (upper) then
                           temp1 = alpha*a(j,k)
                       else
                           temp1 = alpha*a(k,j)
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
               end do loop_170
           end if
           return
     end subroutine stdlib_csymm

     pure module subroutine stdlib_zsymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! ZSYMM performs one of the matrix-matrix operations
     !! C := alpha*A*B + beta*C,
     !! or
     !! C := alpha*B*A + beta*C,
     !! where  alpha and beta are scalars, A is a symmetric matrix and  B and
     !! C are m by n matrices.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: upper
           
           
           ! set nrowa as the number of rows of a.
           if (stdlib_lsame(side,'L')) then
               nrowa = m
           else
               nrowa = n
           end if
           upper = stdlib_lsame(uplo,'U')
           ! test the input parameters.
           info = 0
           if ((.not.stdlib_lsame(side,'L')) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,m)) then
               info = 9
           else if (ldc<max(1,m)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_xerbla('ZSYMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==czero).and. (beta==cone))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (beta==czero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = czero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(side,'L')) then
              ! form  c := alpha*a*b + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,m
                           temp1 = alpha*b(i,j)
                           temp2 = czero
                           do k = 1,i - 1
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*a(k,i)
                           end do
                           if (beta==czero) then
                               c(i,j) = temp1*a(i,i) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*a(i,i) +alpha*temp2
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = m,1,-1
                           temp1 = alpha*b(i,j)
                           temp2 = czero
                           do k = i + 1,m
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*a(k,i)
                           end do
                           if (beta==czero) then
                               c(i,j) = temp1*a(i,i) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*a(i,i) +alpha*temp2
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*b*a + beta*c.
               loop_170: do j = 1,n
                   temp1 = alpha*a(j,j)
                   if (beta==czero) then
                       do i = 1,m
                           c(i,j) = temp1*b(i,j)
                       end do
                   else
                       do i = 1,m
                           c(i,j) = beta*c(i,j) + temp1*b(i,j)
                       end do
                   end if
                   do k = 1,j - 1
                       if (upper) then
                           temp1 = alpha*a(k,j)
                       else
                           temp1 = alpha*a(j,k)
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
                   do k = j + 1,n
                       if (upper) then
                           temp1 = alpha*a(j,k)
                       else
                           temp1 = alpha*a(k,j)
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
               end do loop_170
           end if
           return
     end subroutine stdlib_zsymm



end submodule stdlib_blas_level3_sym
