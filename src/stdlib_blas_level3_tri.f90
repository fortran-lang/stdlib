submodule(stdlib_blas) stdlib_blas_level3_tri
  implicit none


  contains

     pure module subroutine stdlib_strmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
     use stdlib_blas_constants_sp
     !! STRMM performs one of the matrix-matrix operations
     !! B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
     !! where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
     !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
     !! op( A ) = A   or   op( A ) = A**T.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: lside, nounit, upper
           
           ! test the input parameters.
           lside = stdlib_lsame(side,'L')
           if (lside) then
               nrowa = m
           else
               nrowa = n
           end if
           nounit = stdlib_lsame(diag,'N')
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.lside) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if ((.not.stdlib_lsame(transa,'N')) .and.(.not.stdlib_lsame(transa,'T')) .and.(&
                     .not.stdlib_lsame(transa,'C'))) then
               info = 3
           else if ((.not.stdlib_lsame(diag,'U')) .and. (.not.stdlib_lsame(diag,'N'))) &
                     then
               info = 4
           else if (m<0) then
               info = 5
           else if (n<0) then
               info = 6
           else if (lda<max(1,nrowa)) then
               info = 9
           else if (ldb<max(1,m)) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('STRMM ',info)
               return
           end if
           ! quick return if possible.
           if (m==0 .or. n==0) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               do j = 1,n
                   do i = 1,m
                       b(i,j) = zero
                   end do
               end do
               return
           end if
           ! start the operations.
           if (lside) then
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*a*b.
                   if (upper) then
                       do j = 1,n
                           do k = 1,m
                               if (b(k,j)/=zero) then
                                   temp = alpha*b(k,j)
                                   do i = 1,k - 1
                                       b(i,j) = b(i,j) + temp*a(i,k)
                                   end do
                                   if (nounit) temp = temp*a(k,k)
                                   b(k,j) = temp
                               end if
                           end do
                       end do
                   else
                       do j = 1,n
                           do k = m,1,-1
                               if (b(k,j)/=zero) then
                                   temp = alpha*b(k,j)
                                   b(k,j) = temp
                                   if (nounit) b(k,j) = b(k,j)*a(k,k)
                                   do i = k + 1,m
                                       b(i,j) = b(i,j) + temp*a(i,k)
                                   end do
                               end if
                           end do
                       end do
                   end if
               else
                 ! form  b := alpha*a**t*b.
                   if (upper) then
                       do j = 1,n
                           do i = m,1,-1
                               temp = b(i,j)
                               if (nounit) temp = temp*a(i,i)
                               do k = 1,i - 1
                                   temp = temp + a(k,i)*b(k,j)
                               end do
                               b(i,j) = alpha*temp
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,m
                               temp = b(i,j)
                               if (nounit) temp = temp*a(i,i)
                               do k = i + 1,m
                                   temp = temp + a(k,i)*b(k,j)
                               end do
                               b(i,j) = alpha*temp
                           end do
                       end do
                   end if
               end if
           else
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*b*a.
                   if (upper) then
                       do j = n,1,-1
                           temp = alpha
                           if (nounit) temp = temp*a(j,j)
                           do i = 1,m
                               b(i,j) = temp*b(i,j)
                           end do
                           do k = 1,j - 1
                               if (a(k,j)/=zero) then
                                   temp = alpha*a(k,j)
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                       end do
                   else
                       do j = 1,n
                           temp = alpha
                           if (nounit) temp = temp*a(j,j)
                           do i = 1,m
                               b(i,j) = temp*b(i,j)
                           end do
                           do k = j + 1,n
                               if (a(k,j)/=zero) then
                                   temp = alpha*a(k,j)
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                       end do
                   end if
               else
                 ! form  b := alpha*b*a**t.
                   if (upper) then
                       do k = 1,n
                           do j = 1,k - 1
                               if (a(j,k)/=zero) then
                                   temp = alpha*a(j,k)
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                           temp = alpha
                           if (nounit) temp = temp*a(k,k)
                           if (temp/=one) then
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                       end do
                   else
                       do k = n,1,-1
                           do j = k + 1,n
                               if (a(j,k)/=zero) then
                                   temp = alpha*a(j,k)
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                           temp = alpha
                           if (nounit) temp = temp*a(k,k)
                           if (temp/=one) then
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_strmm

     pure module subroutine stdlib_dtrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
     use stdlib_blas_constants_dp
     !! DTRMM performs one of the matrix-matrix operations
     !! B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
     !! where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
     !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
     !! op( A ) = A   or   op( A ) = A**T.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: lside, nounit, upper
           
           ! test the input parameters.
           lside = stdlib_lsame(side,'L')
           if (lside) then
               nrowa = m
           else
               nrowa = n
           end if
           nounit = stdlib_lsame(diag,'N')
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.lside) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if ((.not.stdlib_lsame(transa,'N')) .and.(.not.stdlib_lsame(transa,'T')) .and.(&
                     .not.stdlib_lsame(transa,'C'))) then
               info = 3
           else if ((.not.stdlib_lsame(diag,'U')) .and. (.not.stdlib_lsame(diag,'N'))) &
                     then
               info = 4
           else if (m<0) then
               info = 5
           else if (n<0) then
               info = 6
           else if (lda<max(1,nrowa)) then
               info = 9
           else if (ldb<max(1,m)) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('DTRMM ',info)
               return
           end if
           ! quick return if possible.
           if (m==0 .or. n==0) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               do j = 1,n
                   do i = 1,m
                       b(i,j) = zero
                   end do
               end do
               return
           end if
           ! start the operations.
           if (lside) then
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*a*b.
                   if (upper) then
                       do j = 1,n
                           do k = 1,m
                               if (b(k,j)/=zero) then
                                   temp = alpha*b(k,j)
                                   do i = 1,k - 1
                                       b(i,j) = b(i,j) + temp*a(i,k)
                                   end do
                                   if (nounit) temp = temp*a(k,k)
                                   b(k,j) = temp
                               end if
                           end do
                       end do
                   else
                       do j = 1,n
                           do k = m,1,-1
                               if (b(k,j)/=zero) then
                                   temp = alpha*b(k,j)
                                   b(k,j) = temp
                                   if (nounit) b(k,j) = b(k,j)*a(k,k)
                                   do i = k + 1,m
                                       b(i,j) = b(i,j) + temp*a(i,k)
                                   end do
                               end if
                           end do
                       end do
                   end if
               else
                 ! form  b := alpha*a**t*b.
                   if (upper) then
                       do j = 1,n
                           do i = m,1,-1
                               temp = b(i,j)
                               if (nounit) temp = temp*a(i,i)
                               do k = 1,i - 1
                                   temp = temp + a(k,i)*b(k,j)
                               end do
                               b(i,j) = alpha*temp
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,m
                               temp = b(i,j)
                               if (nounit) temp = temp*a(i,i)
                               do k = i + 1,m
                                   temp = temp + a(k,i)*b(k,j)
                               end do
                               b(i,j) = alpha*temp
                           end do
                       end do
                   end if
               end if
           else
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*b*a.
                   if (upper) then
                       do j = n,1,-1
                           temp = alpha
                           if (nounit) temp = temp*a(j,j)
                           do i = 1,m
                               b(i,j) = temp*b(i,j)
                           end do
                           do k = 1,j - 1
                               if (a(k,j)/=zero) then
                                   temp = alpha*a(k,j)
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                       end do
                   else
                       do j = 1,n
                           temp = alpha
                           if (nounit) temp = temp*a(j,j)
                           do i = 1,m
                               b(i,j) = temp*b(i,j)
                           end do
                           do k = j + 1,n
                               if (a(k,j)/=zero) then
                                   temp = alpha*a(k,j)
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                       end do
                   end if
               else
                 ! form  b := alpha*b*a**t.
                   if (upper) then
                       do k = 1,n
                           do j = 1,k - 1
                               if (a(j,k)/=zero) then
                                   temp = alpha*a(j,k)
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                           temp = alpha
                           if (nounit) temp = temp*a(k,k)
                           if (temp/=one) then
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                       end do
                   else
                       do k = n,1,-1
                           do j = k + 1,n
                               if (a(j,k)/=zero) then
                                   temp = alpha*a(j,k)
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                           temp = alpha
                           if (nounit) temp = temp*a(k,k)
                           if (temp/=one) then
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_dtrmm


     pure module subroutine stdlib_ctrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
     use stdlib_blas_constants_sp
     !! CTRMM performs one of the matrix-matrix operations
     !! B := alpha*op( A )*B,   or   B := alpha*B*op( A )
     !! where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
     !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
     !! op( A ) = A   or   op( A ) = A**T   or   op( A ) = A**H.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: lside, noconj, nounit, upper
           
           
           ! test the input parameters.
           lside = stdlib_lsame(side,'L')
           if (lside) then
               nrowa = m
           else
               nrowa = n
           end if
           noconj = stdlib_lsame(transa,'T')
           nounit = stdlib_lsame(diag,'N')
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.lside) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if ((.not.stdlib_lsame(transa,'N')) .and.(.not.stdlib_lsame(transa,'T')) .and.(&
                     .not.stdlib_lsame(transa,'C'))) then
               info = 3
           else if ((.not.stdlib_lsame(diag,'U')) .and. (.not.stdlib_lsame(diag,'N'))) &
                     then
               info = 4
           else if (m<0) then
               info = 5
           else if (n<0) then
               info = 6
           else if (lda<max(1,nrowa)) then
               info = 9
           else if (ldb<max(1,m)) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('CTRMM ',info)
               return
           end if
           ! quick return if possible.
           if (m==0 .or. n==0) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               do j = 1,n
                   do i = 1,m
                       b(i,j) = czero
                   end do
               end do
               return
           end if
           ! start the operations.
           if (lside) then
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*a*b.
                   if (upper) then
                       do j = 1,n
                           do k = 1,m
                               if (b(k,j)/=czero) then
                                   temp = alpha*b(k,j)
                                   do i = 1,k - 1
                                       b(i,j) = b(i,j) + temp*a(i,k)
                                   end do
                                   if (nounit) temp = temp*a(k,k)
                                   b(k,j) = temp
                               end if
                           end do
                       end do
                   else
                       do j = 1,n
                           do k = m,1,-1
                               if (b(k,j)/=czero) then
                                   temp = alpha*b(k,j)
                                   b(k,j) = temp
                                   if (nounit) b(k,j) = b(k,j)*a(k,k)
                                   do i = k + 1,m
                                       b(i,j) = b(i,j) + temp*a(i,k)
                                   end do
                               end if
                           end do
                       end do
                   end if
               else
                 ! form  b := alpha*a**t*b   or   b := alpha*a**h*b.
                   if (upper) then
                       do j = 1,n
                           do i = m,1,-1
                               temp = b(i,j)
                               if (noconj) then
                                   if (nounit) temp = temp*a(i,i)
                                   do k = 1,i - 1
                                       temp = temp + a(k,i)*b(k,j)
                                   end do
                               else
                                   if (nounit) temp = temp*conjg(a(i,i))
                                   do k = 1,i - 1
                                       temp = temp + conjg(a(k,i))*b(k,j)
                                   end do
                               end if
                               b(i,j) = alpha*temp
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,m
                               temp = b(i,j)
                               if (noconj) then
                                   if (nounit) temp = temp*a(i,i)
                                   do k = i + 1,m
                                       temp = temp + a(k,i)*b(k,j)
                                   end do
                               else
                                   if (nounit) temp = temp*conjg(a(i,i))
                                   do k = i + 1,m
                                       temp = temp + conjg(a(k,i))*b(k,j)
                                   end do
                               end if
                               b(i,j) = alpha*temp
                           end do
                       end do
                   end if
               end if
           else
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*b*a.
                   if (upper) then
                       do j = n,1,-1
                           temp = alpha
                           if (nounit) temp = temp*a(j,j)
                           do i = 1,m
                               b(i,j) = temp*b(i,j)
                           end do
                           do k = 1,j - 1
                               if (a(k,j)/=czero) then
                                   temp = alpha*a(k,j)
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                       end do
                   else
                       do j = 1,n
                           temp = alpha
                           if (nounit) temp = temp*a(j,j)
                           do i = 1,m
                               b(i,j) = temp*b(i,j)
                           end do
                           do k = j + 1,n
                               if (a(k,j)/=czero) then
                                   temp = alpha*a(k,j)
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                       end do
                   end if
               else
                 ! form  b := alpha*b*a**t   or   b := alpha*b*a**h.
                   if (upper) then
                       loop_280: do k = 1,n
                           do j = 1,k - 1
                               if (a(j,k)/=czero) then
                                   if (noconj) then
                                       temp = alpha*a(j,k)
                                   else
                                       temp = alpha*conjg(a(j,k))
                                   end if
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                           temp = alpha
                           if (nounit) then
                               if (noconj) then
                                   temp = temp*a(k,k)
                               else
                                   temp = temp*conjg(a(k,k))
                               end if
                           end if
                           if (temp/=cone) then
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                       end do loop_280
                   else
                       loop_320: do k = n,1,-1
                           do j = k + 1,n
                               if (a(j,k)/=czero) then
                                   if (noconj) then
                                       temp = alpha*a(j,k)
                                   else
                                       temp = alpha*conjg(a(j,k))
                                   end if
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                           temp = alpha
                           if (nounit) then
                               if (noconj) then
                                   temp = temp*a(k,k)
                               else
                                   temp = temp*conjg(a(k,k))
                               end if
                           end if
                           if (temp/=cone) then
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                       end do loop_320
                   end if
               end if
           end if
           return
     end subroutine stdlib_ctrmm

     pure module subroutine stdlib_ztrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
     use stdlib_blas_constants_dp
     !! ZTRMM performs one of the matrix-matrix operations
     !! B := alpha*op( A )*B,   or   B := alpha*B*op( A )
     !! where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
     !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
     !! op( A ) = A   or   op( A ) = A**T   or   op( A ) = A**H.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: lside, noconj, nounit, upper
           
           
           ! test the input parameters.
           lside = stdlib_lsame(side,'L')
           if (lside) then
               nrowa = m
           else
               nrowa = n
           end if
           noconj = stdlib_lsame(transa,'T')
           nounit = stdlib_lsame(diag,'N')
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.lside) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if ((.not.stdlib_lsame(transa,'N')) .and.(.not.stdlib_lsame(transa,'T')) .and.(&
                     .not.stdlib_lsame(transa,'C'))) then
               info = 3
           else if ((.not.stdlib_lsame(diag,'U')) .and. (.not.stdlib_lsame(diag,'N'))) &
                     then
               info = 4
           else if (m<0) then
               info = 5
           else if (n<0) then
               info = 6
           else if (lda<max(1,nrowa)) then
               info = 9
           else if (ldb<max(1,m)) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('ZTRMM ',info)
               return
           end if
           ! quick return if possible.
           if (m==0 .or. n==0) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               do j = 1,n
                   do i = 1,m
                       b(i,j) = czero
                   end do
               end do
               return
           end if
           ! start the operations.
           if (lside) then
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*a*b.
                   if (upper) then
                       do j = 1,n
                           do k = 1,m
                               if (b(k,j)/=czero) then
                                   temp = alpha*b(k,j)
                                   do i = 1,k - 1
                                       b(i,j) = b(i,j) + temp*a(i,k)
                                   end do
                                   if (nounit) temp = temp*a(k,k)
                                   b(k,j) = temp
                               end if
                           end do
                       end do
                   else
                       do j = 1,n
                           do k = m,1,-1
                               if (b(k,j)/=czero) then
                                   temp = alpha*b(k,j)
                                   b(k,j) = temp
                                   if (nounit) b(k,j) = b(k,j)*a(k,k)
                                   do i = k + 1,m
                                       b(i,j) = b(i,j) + temp*a(i,k)
                                   end do
                               end if
                           end do
                       end do
                   end if
               else
                 ! form  b := alpha*a**t*b   or   b := alpha*a**h*b.
                   if (upper) then
                       do j = 1,n
                           do i = m,1,-1
                               temp = b(i,j)
                               if (noconj) then
                                   if (nounit) temp = temp*a(i,i)
                                   do k = 1,i - 1
                                       temp = temp + a(k,i)*b(k,j)
                                   end do
                               else
                                   if (nounit) temp = temp*conjg(a(i,i))
                                   do k = 1,i - 1
                                       temp = temp + conjg(a(k,i))*b(k,j)
                                   end do
                               end if
                               b(i,j) = alpha*temp
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,m
                               temp = b(i,j)
                               if (noconj) then
                                   if (nounit) temp = temp*a(i,i)
                                   do k = i + 1,m
                                       temp = temp + a(k,i)*b(k,j)
                                   end do
                               else
                                   if (nounit) temp = temp*conjg(a(i,i))
                                   do k = i + 1,m
                                       temp = temp + conjg(a(k,i))*b(k,j)
                                   end do
                               end if
                               b(i,j) = alpha*temp
                           end do
                       end do
                   end if
               end if
           else
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*b*a.
                   if (upper) then
                       do j = n,1,-1
                           temp = alpha
                           if (nounit) temp = temp*a(j,j)
                           do i = 1,m
                               b(i,j) = temp*b(i,j)
                           end do
                           do k = 1,j - 1
                               if (a(k,j)/=czero) then
                                   temp = alpha*a(k,j)
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                       end do
                   else
                       do j = 1,n
                           temp = alpha
                           if (nounit) temp = temp*a(j,j)
                           do i = 1,m
                               b(i,j) = temp*b(i,j)
                           end do
                           do k = j + 1,n
                               if (a(k,j)/=czero) then
                                   temp = alpha*a(k,j)
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                       end do
                   end if
               else
                 ! form  b := alpha*b*a**t   or   b := alpha*b*a**h.
                   if (upper) then
                       loop_280: do k = 1,n
                           do j = 1,k - 1
                               if (a(j,k)/=czero) then
                                   if (noconj) then
                                       temp = alpha*a(j,k)
                                   else
                                       temp = alpha*conjg(a(j,k))
                                   end if
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                           temp = alpha
                           if (nounit) then
                               if (noconj) then
                                   temp = temp*a(k,k)
                               else
                                   temp = temp*conjg(a(k,k))
                               end if
                           end if
                           if (temp/=cone) then
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                       end do loop_280
                   else
                       loop_320: do k = n,1,-1
                           do j = k + 1,n
                               if (a(j,k)/=czero) then
                                   if (noconj) then
                                       temp = alpha*a(j,k)
                                   else
                                       temp = alpha*conjg(a(j,k))
                                   end if
                                   do i = 1,m
                                       b(i,j) = b(i,j) + temp*b(i,k)
                                   end do
                               end if
                           end do
                           temp = alpha
                           if (nounit) then
                               if (noconj) then
                                   temp = temp*a(k,k)
                               else
                                   temp = temp*conjg(a(k,k))
                               end if
                           end if
                           if (temp/=cone) then
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                       end do loop_320
                   end if
               end if
           end if
           return
     end subroutine stdlib_ztrmm




     pure module subroutine stdlib_strsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
     use stdlib_blas_constants_sp
     !! STRSM solves one of the matrix equations
     !! op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
     !! where alpha is a scalar, X and B are m by n matrices, A is a unit, or
     !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
     !! op( A ) = A   or   op( A ) = A**T.
     !! The matrix X is overwritten on B.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: lside, nounit, upper
           
           ! test the input parameters.
           lside = stdlib_lsame(side,'L')
           if (lside) then
               nrowa = m
           else
               nrowa = n
           end if
           nounit = stdlib_lsame(diag,'N')
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.lside) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if ((.not.stdlib_lsame(transa,'N')) .and.(.not.stdlib_lsame(transa,'T')) .and.(&
                     .not.stdlib_lsame(transa,'C'))) then
               info = 3
           else if ((.not.stdlib_lsame(diag,'U')) .and. (.not.stdlib_lsame(diag,'N'))) &
                     then
               info = 4
           else if (m<0) then
               info = 5
           else if (n<0) then
               info = 6
           else if (lda<max(1,nrowa)) then
               info = 9
           else if (ldb<max(1,m)) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('STRSM ',info)
               return
           end if
           ! quick return if possible.
           if (m==0 .or. n==0) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               do j = 1,n
                   do i = 1,m
                       b(i,j) = zero
                   end do
               end do
               return
           end if
           ! start the operations.
           if (lside) then
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*inv( a )*b.
                   if (upper) then
                       do j = 1,n
                           if (alpha/=one) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = m,1,-1
                               if (b(k,j)/=zero) then
                                   if (nounit) b(k,j) = b(k,j)/a(k,k)
                                   do i = 1,k - 1
                                       b(i,j) = b(i,j) - b(k,j)*a(i,k)
                                   end do
                               end if
                           end do
                       end do
                   else
                       do j = 1,n
                           if (alpha/=one) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = 1,m
                               if (b(k,j)/=zero) then
                                   if (nounit) b(k,j) = b(k,j)/a(k,k)
                                   do i = k + 1,m
                                       b(i,j) = b(i,j) - b(k,j)*a(i,k)
                                   end do
                               end if
                           end do
                       end do
                   end if
               else
                 ! form  b := alpha*inv( a**t )*b.
                   if (upper) then
                       do j = 1,n
                           do i = 1,m
                               temp = alpha*b(i,j)
                               do k = 1,i - 1
                                   temp = temp - a(k,i)*b(k,j)
                               end do
                               if (nounit) temp = temp/a(i,i)
                               b(i,j) = temp
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = m,1,-1
                               temp = alpha*b(i,j)
                               do k = i + 1,m
                                   temp = temp - a(k,i)*b(k,j)
                               end do
                               if (nounit) temp = temp/a(i,i)
                               b(i,j) = temp
                           end do
                       end do
                   end if
               end if
           else
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*b*inv( a ).
                   if (upper) then
                       do j = 1,n
                           if (alpha/=one) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = 1,j - 1
                               if (a(k,j)/=zero) then
                                   do i = 1,m
                                       b(i,j) = b(i,j) - a(k,j)*b(i,k)
                                   end do
                               end if
                           end do
                           if (nounit) then
                               temp = one/a(j,j)
                               do i = 1,m
                                   b(i,j) = temp*b(i,j)
                               end do
                           end if
                       end do
                   else
                       do j = n,1,-1
                           if (alpha/=one) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = j + 1,n
                               if (a(k,j)/=zero) then
                                   do i = 1,m
                                       b(i,j) = b(i,j) - a(k,j)*b(i,k)
                                   end do
                               end if
                           end do
                           if (nounit) then
                               temp = one/a(j,j)
                               do i = 1,m
                                   b(i,j) = temp*b(i,j)
                               end do
                           end if
                       end do
                   end if
               else
                 ! form  b := alpha*b*inv( a**t ).
                   if (upper) then
                       do k = n,1,-1
                           if (nounit) then
                               temp = one/a(k,k)
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                           do j = 1,k - 1
                               if (a(j,k)/=zero) then
                                   temp = a(j,k)
                                   do i = 1,m
                                       b(i,j) = b(i,j) - temp*b(i,k)
                                   end do
                               end if
                           end do
                           if (alpha/=one) then
                               do i = 1,m
                                   b(i,k) = alpha*b(i,k)
                               end do
                           end if
                       end do
                   else
                       do k = 1,n
                           if (nounit) then
                               temp = one/a(k,k)
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                           do j = k + 1,n
                               if (a(j,k)/=zero) then
                                   temp = a(j,k)
                                   do i = 1,m
                                       b(i,j) = b(i,j) - temp*b(i,k)
                                   end do
                               end if
                           end do
                           if (alpha/=one) then
                               do i = 1,m
                                   b(i,k) = alpha*b(i,k)
                               end do
                           end if
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_strsm

     pure module subroutine stdlib_dtrsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
     use stdlib_blas_constants_dp
     !! DTRSM solves one of the matrix equations
     !! op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
     !! where alpha is a scalar, X and B are m by n matrices, A is a unit, or
     !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
     !! op( A ) = A   or   op( A ) = A**T.
     !! The matrix X is overwritten on B.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: lside, nounit, upper
           
           ! test the input parameters.
           lside = stdlib_lsame(side,'L')
           if (lside) then
               nrowa = m
           else
               nrowa = n
           end if
           nounit = stdlib_lsame(diag,'N')
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.lside) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if ((.not.stdlib_lsame(transa,'N')) .and.(.not.stdlib_lsame(transa,'T')) .and.(&
                     .not.stdlib_lsame(transa,'C'))) then
               info = 3
           else if ((.not.stdlib_lsame(diag,'U')) .and. (.not.stdlib_lsame(diag,'N'))) &
                     then
               info = 4
           else if (m<0) then
               info = 5
           else if (n<0) then
               info = 6
           else if (lda<max(1,nrowa)) then
               info = 9
           else if (ldb<max(1,m)) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('DTRSM ',info)
               return
           end if
           ! quick return if possible.
           if (m==0 .or. n==0) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               do j = 1,n
                   do i = 1,m
                       b(i,j) = zero
                   end do
               end do
               return
           end if
           ! start the operations.
           if (lside) then
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*inv( a )*b.
                   if (upper) then
                       do j = 1,n
                           if (alpha/=one) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = m,1,-1
                               if (b(k,j)/=zero) then
                                   if (nounit) b(k,j) = b(k,j)/a(k,k)
                                   do i = 1,k - 1
                                       b(i,j) = b(i,j) - b(k,j)*a(i,k)
                                   end do
                               end if
                           end do
                       end do
                   else
                       do j = 1,n
                           if (alpha/=one) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = 1,m
                               if (b(k,j)/=zero) then
                                   if (nounit) b(k,j) = b(k,j)/a(k,k)
                                   do i = k + 1,m
                                       b(i,j) = b(i,j) - b(k,j)*a(i,k)
                                   end do
                               end if
                           end do
                       end do
                   end if
               else
                 ! form  b := alpha*inv( a**t )*b.
                   if (upper) then
                       do j = 1,n
                           do i = 1,m
                               temp = alpha*b(i,j)
                               do k = 1,i - 1
                                   temp = temp - a(k,i)*b(k,j)
                               end do
                               if (nounit) temp = temp/a(i,i)
                               b(i,j) = temp
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = m,1,-1
                               temp = alpha*b(i,j)
                               do k = i + 1,m
                                   temp = temp - a(k,i)*b(k,j)
                               end do
                               if (nounit) temp = temp/a(i,i)
                               b(i,j) = temp
                           end do
                       end do
                   end if
               end if
           else
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*b*inv( a ).
                   if (upper) then
                       do j = 1,n
                           if (alpha/=one) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = 1,j - 1
                               if (a(k,j)/=zero) then
                                   do i = 1,m
                                       b(i,j) = b(i,j) - a(k,j)*b(i,k)
                                   end do
                               end if
                           end do
                           if (nounit) then
                               temp = one/a(j,j)
                               do i = 1,m
                                   b(i,j) = temp*b(i,j)
                               end do
                           end if
                       end do
                   else
                       do j = n,1,-1
                           if (alpha/=one) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = j + 1,n
                               if (a(k,j)/=zero) then
                                   do i = 1,m
                                       b(i,j) = b(i,j) - a(k,j)*b(i,k)
                                   end do
                               end if
                           end do
                           if (nounit) then
                               temp = one/a(j,j)
                               do i = 1,m
                                   b(i,j) = temp*b(i,j)
                               end do
                           end if
                       end do
                   end if
               else
                 ! form  b := alpha*b*inv( a**t ).
                   if (upper) then
                       do k = n,1,-1
                           if (nounit) then
                               temp = one/a(k,k)
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                           do j = 1,k - 1
                               if (a(j,k)/=zero) then
                                   temp = a(j,k)
                                   do i = 1,m
                                       b(i,j) = b(i,j) - temp*b(i,k)
                                   end do
                               end if
                           end do
                           if (alpha/=one) then
                               do i = 1,m
                                   b(i,k) = alpha*b(i,k)
                               end do
                           end if
                       end do
                   else
                       do k = 1,n
                           if (nounit) then
                               temp = one/a(k,k)
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                           do j = k + 1,n
                               if (a(j,k)/=zero) then
                                   temp = a(j,k)
                                   do i = 1,m
                                       b(i,j) = b(i,j) - temp*b(i,k)
                                   end do
                               end if
                           end do
                           if (alpha/=one) then
                               do i = 1,m
                                   b(i,k) = alpha*b(i,k)
                               end do
                           end if
                       end do
                   end if
               end if
           end if
           return
     end subroutine stdlib_dtrsm


     pure module subroutine stdlib_ctrsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
     use stdlib_blas_constants_sp
     !! CTRSM solves one of the matrix equations
     !! op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
     !! where alpha is a scalar, X and B are m by n matrices, A is a unit, or
     !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
     !! op( A ) = A   or   op( A ) = A**T   or   op( A ) = A**H.
     !! The matrix X is overwritten on B.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: lside, noconj, nounit, upper
           
           
           ! test the input parameters.
           lside = stdlib_lsame(side,'L')
           if (lside) then
               nrowa = m
           else
               nrowa = n
           end if
           noconj = stdlib_lsame(transa,'T')
           nounit = stdlib_lsame(diag,'N')
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.lside) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if ((.not.stdlib_lsame(transa,'N')) .and.(.not.stdlib_lsame(transa,'T')) .and.(&
                     .not.stdlib_lsame(transa,'C'))) then
               info = 3
           else if ((.not.stdlib_lsame(diag,'U')) .and. (.not.stdlib_lsame(diag,'N'))) &
                     then
               info = 4
           else if (m<0) then
               info = 5
           else if (n<0) then
               info = 6
           else if (lda<max(1,nrowa)) then
               info = 9
           else if (ldb<max(1,m)) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('CTRSM ',info)
               return
           end if
           ! quick return if possible.
           if (m==0 .or. n==0) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               do j = 1,n
                   do i = 1,m
                       b(i,j) = czero
                   end do
               end do
               return
           end if
           ! start the operations.
           if (lside) then
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*inv( a )*b.
                   if (upper) then
                       do j = 1,n
                           if (alpha/=cone) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = m,1,-1
                               if (b(k,j)/=czero) then
                                   if (nounit) b(k,j) = b(k,j)/a(k,k)
                                   do i = 1,k - 1
                                       b(i,j) = b(i,j) - b(k,j)*a(i,k)
                                   end do
                               end if
                           end do
                       end do
                   else
                       do j = 1,n
                           if (alpha/=cone) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = 1,m
                               if (b(k,j)/=czero) then
                                   if (nounit) b(k,j) = b(k,j)/a(k,k)
                                   do i = k + 1,m
                                       b(i,j) = b(i,j) - b(k,j)*a(i,k)
                                   end do
                               end if
                           end do
                       end do
                   end if
               else
                 ! form  b := alpha*inv( a**t )*b
                 ! or    b := alpha*inv( a**h )*b.
                   if (upper) then
                       do j = 1,n
                           do i = 1,m
                               temp = alpha*b(i,j)
                               if (noconj) then
                                   do k = 1,i - 1
                                       temp = temp - a(k,i)*b(k,j)
                                   end do
                                   if (nounit) temp = temp/a(i,i)
                               else
                                   do k = 1,i - 1
                                       temp = temp - conjg(a(k,i))*b(k,j)
                                   end do
                                   if (nounit) temp = temp/conjg(a(i,i))
                               end if
                               b(i,j) = temp
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = m,1,-1
                               temp = alpha*b(i,j)
                               if (noconj) then
                                   do k = i + 1,m
                                       temp = temp - a(k,i)*b(k,j)
                                   end do
                                   if (nounit) temp = temp/a(i,i)
                               else
                                   do k = i + 1,m
                                       temp = temp - conjg(a(k,i))*b(k,j)
                                   end do
                                   if (nounit) temp = temp/conjg(a(i,i))
                               end if
                               b(i,j) = temp
                           end do
                       end do
                   end if
               end if
           else
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*b*inv( a ).
                   if (upper) then
                       do j = 1,n
                           if (alpha/=cone) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = 1,j - 1
                               if (a(k,j)/=czero) then
                                   do i = 1,m
                                       b(i,j) = b(i,j) - a(k,j)*b(i,k)
                                   end do
                               end if
                           end do
                           if (nounit) then
                               temp = cone/a(j,j)
                               do i = 1,m
                                   b(i,j) = temp*b(i,j)
                               end do
                           end if
                       end do
                   else
                       do j = n,1,-1
                           if (alpha/=cone) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = j + 1,n
                               if (a(k,j)/=czero) then
                                   do i = 1,m
                                       b(i,j) = b(i,j) - a(k,j)*b(i,k)
                                   end do
                               end if
                           end do
                           if (nounit) then
                               temp = cone/a(j,j)
                               do i = 1,m
                                   b(i,j) = temp*b(i,j)
                               end do
                           end if
                       end do
                   end if
               else
                 ! form  b := alpha*b*inv( a**t )
                 ! or    b := alpha*b*inv( a**h ).
                   if (upper) then
                       loop_330: do k = n,1,-1
                           if (nounit) then
                               if (noconj) then
                                   temp = cone/a(k,k)
                               else
                                   temp = cone/conjg(a(k,k))
                               end if
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                           do j = 1,k - 1
                               if (a(j,k)/=czero) then
                                   if (noconj) then
                                       temp = a(j,k)
                                   else
                                       temp = conjg(a(j,k))
                                   end if
                                   do i = 1,m
                                       b(i,j) = b(i,j) - temp*b(i,k)
                                   end do
                               end if
                           end do
                           if (alpha/=cone) then
                               do i = 1,m
                                   b(i,k) = alpha*b(i,k)
                               end do
                           end if
                       end do loop_330
                   else
                       loop_380: do k = 1,n
                           if (nounit) then
                               if (noconj) then
                                   temp = cone/a(k,k)
                               else
                                   temp = cone/conjg(a(k,k))
                               end if
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                           do j = k + 1,n
                               if (a(j,k)/=czero) then
                                   if (noconj) then
                                       temp = a(j,k)
                                   else
                                       temp = conjg(a(j,k))
                                   end if
                                   do i = 1,m
                                       b(i,j) = b(i,j) - temp*b(i,k)
                                   end do
                               end if
                           end do
                           if (alpha/=cone) then
                               do i = 1,m
                                   b(i,k) = alpha*b(i,k)
                               end do
                           end if
                       end do loop_380
                   end if
               end if
           end if
           return
     end subroutine stdlib_ctrsm

     pure module subroutine stdlib_ztrsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
     use stdlib_blas_constants_dp
     !! ZTRSM solves one of the matrix equations
     !! op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
     !! where alpha is a scalar, X and B are m by n matrices, A is a unit, or
     !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
     !! op( A ) = A   or   op( A ) = A**T   or   op( A ) = A**H.
     !! The matrix X is overwritten on B.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: lside, noconj, nounit, upper
           
           
           ! test the input parameters.
           lside = stdlib_lsame(side,'L')
           if (lside) then
               nrowa = m
           else
               nrowa = n
           end if
           noconj = stdlib_lsame(transa,'T')
           nounit = stdlib_lsame(diag,'N')
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.lside) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if ((.not.stdlib_lsame(transa,'N')) .and.(.not.stdlib_lsame(transa,'T')) .and.(&
                     .not.stdlib_lsame(transa,'C'))) then
               info = 3
           else if ((.not.stdlib_lsame(diag,'U')) .and. (.not.stdlib_lsame(diag,'N'))) &
                     then
               info = 4
           else if (m<0) then
               info = 5
           else if (n<0) then
               info = 6
           else if (lda<max(1,nrowa)) then
               info = 9
           else if (ldb<max(1,m)) then
               info = 11
           end if
           if (info/=0) then
               call stdlib_xerbla('ZTRSM ',info)
               return
           end if
           ! quick return if possible.
           if (m==0 .or. n==0) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               do j = 1,n
                   do i = 1,m
                       b(i,j) = czero
                   end do
               end do
               return
           end if
           ! start the operations.
           if (lside) then
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*inv( a )*b.
                   if (upper) then
                       do j = 1,n
                           if (alpha/=cone) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = m,1,-1
                               if (b(k,j)/=czero) then
                                   if (nounit) b(k,j) = b(k,j)/a(k,k)
                                   do i = 1,k - 1
                                       b(i,j) = b(i,j) - b(k,j)*a(i,k)
                                   end do
                               end if
                           end do
                       end do
                   else
                       do j = 1,n
                           if (alpha/=cone) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = 1,m
                               if (b(k,j)/=czero) then
                                   if (nounit) b(k,j) = b(k,j)/a(k,k)
                                   do i = k + 1,m
                                       b(i,j) = b(i,j) - b(k,j)*a(i,k)
                                   end do
                               end if
                           end do
                       end do
                   end if
               else
                 ! form  b := alpha*inv( a**t )*b
                 ! or    b := alpha*inv( a**h )*b.
                   if (upper) then
                       do j = 1,n
                           do i = 1,m
                               temp = alpha*b(i,j)
                               if (noconj) then
                                   do k = 1,i - 1
                                       temp = temp - a(k,i)*b(k,j)
                                   end do
                                   if (nounit) temp = temp/a(i,i)
                               else
                                   do k = 1,i - 1
                                       temp = temp - conjg(a(k,i))*b(k,j)
                                   end do
                                   if (nounit) temp = temp/conjg(a(i,i))
                               end if
                               b(i,j) = temp
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = m,1,-1
                               temp = alpha*b(i,j)
                               if (noconj) then
                                   do k = i + 1,m
                                       temp = temp - a(k,i)*b(k,j)
                                   end do
                                   if (nounit) temp = temp/a(i,i)
                               else
                                   do k = i + 1,m
                                       temp = temp - conjg(a(k,i))*b(k,j)
                                   end do
                                   if (nounit) temp = temp/conjg(a(i,i))
                               end if
                               b(i,j) = temp
                           end do
                       end do
                   end if
               end if
           else
               if (stdlib_lsame(transa,'N')) then
                 ! form  b := alpha*b*inv( a ).
                   if (upper) then
                       do j = 1,n
                           if (alpha/=cone) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = 1,j - 1
                               if (a(k,j)/=czero) then
                                   do i = 1,m
                                       b(i,j) = b(i,j) - a(k,j)*b(i,k)
                                   end do
                               end if
                           end do
                           if (nounit) then
                               temp = cone/a(j,j)
                               do i = 1,m
                                   b(i,j) = temp*b(i,j)
                               end do
                           end if
                       end do
                   else
                       do j = n,1,-1
                           if (alpha/=cone) then
                               do i = 1,m
                                   b(i,j) = alpha*b(i,j)
                               end do
                           end if
                           do k = j + 1,n
                               if (a(k,j)/=czero) then
                                   do i = 1,m
                                       b(i,j) = b(i,j) - a(k,j)*b(i,k)
                                   end do
                               end if
                           end do
                           if (nounit) then
                               temp = cone/a(j,j)
                               do i = 1,m
                                   b(i,j) = temp*b(i,j)
                               end do
                           end if
                       end do
                   end if
               else
                 ! form  b := alpha*b*inv( a**t )
                 ! or    b := alpha*b*inv( a**h ).
                   if (upper) then
                       loop_330: do k = n,1,-1
                           if (nounit) then
                               if (noconj) then
                                   temp = cone/a(k,k)
                               else
                                   temp = cone/conjg(a(k,k))
                               end if
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                           do j = 1,k - 1
                               if (a(j,k)/=czero) then
                                   if (noconj) then
                                       temp = a(j,k)
                                   else
                                       temp = conjg(a(j,k))
                                   end if
                                   do i = 1,m
                                       b(i,j) = b(i,j) - temp*b(i,k)
                                   end do
                               end if
                           end do
                           if (alpha/=cone) then
                               do i = 1,m
                                   b(i,k) = alpha*b(i,k)
                               end do
                           end if
                       end do loop_330
                   else
                       loop_380: do k = 1,n
                           if (nounit) then
                               if (noconj) then
                                   temp = cone/a(k,k)
                               else
                                   temp = cone/conjg(a(k,k))
                               end if
                               do i = 1,m
                                   b(i,k) = temp*b(i,k)
                               end do
                           end if
                           do j = k + 1,n
                               if (a(j,k)/=czero) then
                                   if (noconj) then
                                       temp = a(j,k)
                                   else
                                       temp = conjg(a(j,k))
                                   end if
                                   do i = 1,m
                                       b(i,j) = b(i,j) - temp*b(i,k)
                                   end do
                               end if
                           end do
                           if (alpha/=cone) then
                               do i = 1,m
                                   b(i,k) = alpha*b(i,k)
                               end do
                           end if
                       end do loop_380
                   end if
               end if
           end if
           return
     end subroutine stdlib_ztrsm



end submodule stdlib_blas_level3_tri
