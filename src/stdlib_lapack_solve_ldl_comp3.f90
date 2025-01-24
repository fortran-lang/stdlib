submodule(stdlib_lapack_solve) stdlib_lapack_solve_ldl_comp3
  implicit none


  contains

     pure module subroutine stdlib_checon( uplo, n, a, lda, ipiv, anorm, rcond, work,info )
     !! CHECON estimates the reciprocal of the condition number of a complex
     !! Hermitian matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by CHETRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, kase
           real(sp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHECON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do i = 1, n
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**h) or inv(u*d*u**h).
              call stdlib_chetrs( uplo, n, 1_ilp, a, lda, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_checon

     pure module subroutine stdlib_zhecon( uplo, n, a, lda, ipiv, anorm, rcond, work,info )
     !! ZHECON estimates the reciprocal of the condition number of a complex
     !! Hermitian matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by ZHETRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, kase
           real(dp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( anorm<zero ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHECON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do i = 1, n
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**h) or inv(u*d*u**h).
              call stdlib_zhetrs( uplo, n, 1_ilp, a, lda, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_zhecon




     pure module subroutine stdlib_chetrf( uplo, n, a, lda, ipiv, work, lwork, info )
     !! CHETRF computes the factorization of a complex Hermitian matrix A
     !! using the Bunch-Kaufman diagonal pivoting method.  The form of the
     !! factorization is
     !! A = U*D*U**H  or  A = L*D*L**H
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is Hermitian and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: iinfo, iws, j, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'CHETRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHETRF', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CHETRF', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**h using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_clahef;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 40
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_clahef( uplo, k, nb, kb, a, lda, ipiv, work, n, iinfo )
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_chetf2( uplo, k, a, lda, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
           else
              ! factorize a as l*d*l**h using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_clahef;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 40
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_clahef( uplo, n-k+1, nb, kb, a( k, k ), lda, ipiv( k ),work, n, &
                           iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_chetf2( uplo, n-k+1, a( k, k ), lda, ipiv( k ), iinfo )
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do j = k, k + kb - 1
                 if( ipiv( j )>0_ilp ) then
                    ipiv( j ) = ipiv( j ) + k - 1_ilp
                 else
                    ipiv( j ) = ipiv( j ) - k + 1_ilp
                 end if
              end do
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
           end if
           40 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_chetrf

     pure module subroutine stdlib_zhetrf( uplo, n, a, lda, ipiv, work, lwork, info )
     !! ZHETRF computes the factorization of a complex Hermitian matrix A
     !! using the Bunch-Kaufman diagonal pivoting method.  The form of the
     !! factorization is
     !! A = U*D*U**H  or  A = L*D*L**H
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is Hermitian and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: iinfo, iws, j, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size
              nb = stdlib_ilaenv( 1_ilp, 'ZHETRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHETRF', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp
           ldwork = n
           if( nb>1_ilp .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp )
                 nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZHETRF', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              end if
           else
              iws = 1_ilp
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**h using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_zlahef;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 40
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_zlahef( uplo, k, nb, kb, a, lda, ipiv, work, n, iinfo )
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_zhetf2( uplo, k, a, lda, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
           else
              ! factorize a as l*d*l**h using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_zlahef;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 40
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_zlahef( uplo, n-k+1, nb, kb, a( k, k ), lda, ipiv( k ),work, n, &
                           iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_zhetf2( uplo, n-k+1, a( k, k ), lda, ipiv( k ), iinfo )
                 kb = n - k + 1_ilp
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp .and. iinfo>0_ilp )info = iinfo + k - 1_ilp
              ! adjust ipiv
              do j = k, k + kb - 1
                 if( ipiv( j )>0_ilp ) then
                    ipiv( j ) = ipiv( j ) + k - 1_ilp
                 else
                    ipiv( j ) = ipiv( j ) - k + 1_ilp
                 end if
              end do
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
           end if
           40 continue
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zhetrf




     pure module subroutine stdlib_clahef( uplo, n, nb, kb, a, lda, ipiv, w, ldw, info )
     !! CLAHEF computes a partial factorization of a complex Hermitian
     !! matrix A using the Bunch-Kaufman diagonal pivoting method. The
     !! partial factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I      0     )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**H U22**H )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**H L21**H )  if UPLO = 'L'
     !! ( L21  I ) (  0  A22 ) (  0      I     )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! Note that U**H denotes the conjugate transpose of U.
     !! CLAHEF is an auxiliary routine called by CHETRF. It uses blocked code
     !! (calling Level 3 BLAS) to update the submatrix A11 (if UPLO = 'U') or
     !! A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           
           ! Local Scalars 
           integer(ilp) :: imax, j, jb, jj, jmax, jp, k, kk, kkw, kp, kstep, kw
           real(sp) :: absakk, alpha, colmax, r1, rowmax, t
           complex(sp) :: d11, d21, d22, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=sp) ) + abs( aimag( z ) )
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11 (note that conjg(w) is actually stored)
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              k = n
              10 continue
              ! kw is the column of w which corresponds to column k of a
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              kstep = 1_ilp
              ! copy column k of a to column kw of w and update it
              call stdlib_ccopy( k-1, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              w( k, kw ) = real( a( k, k ),KIND=sp)
              if( k<n ) then
                 call stdlib_cgemv( 'NO TRANSPOSE', k, n-k, -cone, a( 1_ilp, k+1 ), lda,w( k, kw+1 ), &
                           ldw, cone, w( 1_ilp, kw ), 1_ilp )
                 w( k, kw ) = real( w( k, kw ),KIND=sp)
              end if
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( w( k, kw ),KIND=sp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_icamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = cabs1( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=sp)
              else
                 ! ============================================================
                 ! begin pivot search
                 ! case(1)
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! begin pivot search along imax row
                    ! copy column imax to column kw-1 of w and update it
                    call stdlib_ccopy( imax-1, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                    w( imax, kw-1 ) = real( a( imax, imax ),KIND=sp)
                    call stdlib_ccopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                              
                    call stdlib_clacgv( k-imax, w( imax+1, kw-1 ), 1_ilp )
                    if( k<n ) then
                       call stdlib_cgemv( 'NO TRANSPOSE', k, n-k, -cone,a( 1_ilp, k+1 ), lda, w( imax,&
                                  kw+1 ), ldw,cone, w( 1_ilp, kw-1 ), 1_ilp )
                       w( imax, kw-1 ) = real( w( imax, kw-1 ),KIND=sp)
                    end if
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value.
                    ! determine only rowmax.
                    jmax = imax + stdlib_icamax( k-imax, w( imax+1, kw-1 ), 1_ilp )
                    rowmax = cabs1( w( jmax, kw-1 ) )
                    if( imax>1_ilp ) then
                       jmax = stdlib_icamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                       rowmax = max( rowmax, cabs1( w( jmax, kw-1 ) ) )
                    end if
                    ! case(2)
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    ! case(3)
                    else if( abs( real( w( imax, kw-1 ),KIND=sp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column kw-1 of w to column kw of w
                       call stdlib_ccopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                    ! case(4)
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                    ! end pivot search along imax row
                 end if
                 ! end pivot search
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kkw of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k-1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = real( a( kk, kk ),KIND=sp)
                    call stdlib_ccopy( kk-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    call stdlib_clacgv( kk-1-kp, a( kp, kp+1 ), lda )
                    if( kp>1_ilp )call stdlib_ccopy( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last k+1 to n columns of a
                    ! (columns k (or k and k-1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in last kkw to nb columns of w.
                    if( k<n )call stdlib_cswap( n-k, a( kk, k+1 ), lda, a( kp, k+1 ),lda )
                    call stdlib_cswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(kw) = u(k)*d(k),
                    ! where u(k) is the k-th column of u
                    ! (1) store subdiag. elements of column u(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! (note: diagonal element u(k,k) is a unit element
                    ! and not stored)
                       ! a(k,k) := d(k,k) = w(k,kw)
                       ! a(1:k-1,k) := u(1:k-1,k) = w(1:k-1,kw)/d(k,k)
                    ! (note: no need to use for hermitian matrix
                    ! a( k, k ) = real( w( k, k),KIND=sp) to separately copy diagonal
                    ! element d(k,k) from w (potentially saves only one load))
                    call stdlib_ccopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    if( k>1_ilp ) then
                       ! (note: no need to check if a(k,k) is not zero,
                        ! since that was ensured earlier in pivot search:
                        ! case a(k,k) = 0 falls into 2x2 pivot case(4))
                       r1 = one / real( a( k, k ),KIND=sp)
                       call stdlib_csscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                       ! (2) conjugate column w(kw)
                       call stdlib_clacgv( k-1, w( 1_ilp, kw ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now hold
                    ! ( w(kw-1) w(kw) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! (1) store u(1:k-2,k-1) and u(1:k-2,k) and 2-by-2
                    ! block d(k-1:k,k-1:k) in columns k-1 and k of a.
                    ! (note: 2-by-2 diagonal block u(k-1:k,k-1:k) is a unit
                    ! block and not stored)
                       ! a(k-1:k,k-1:k) := d(k-1:k,k-1:k) = w(k-1:k,kw-1:kw)
                       ! a(1:k-2,k-1:k) := u(1:k-2,k:k-1:k) =
                       ! = w(1:k-2,kw-1:kw) * ( d(k-1:k,k-1:k)**(-1) )
                    if( k>2_ilp ) then
                       ! factor out the columns of the inverse of 2-by-2 pivot
                       ! block d, so that each column contains 1, to reduce the
                       ! number of flops when we multiply panel
                       ! ( w(kw-1) w(kw) ) by this inverse, i.e. by d**(-1).
                       ! d**(-1) = ( d11 cj(d21) )**(-1) =
                                 ! ( d21    d22 )
                       ! = 1/(d11*d22-|d21|**2) * ( ( d22) (-cj(d21) ) ) =
                                                ! ( (-d21) (     d11 ) )
                       ! = 1/(|d21|**2) * 1/((d11/cj(d21))*(d22/d21)-1) *
                         ! * ( d21*( d22/d21 ) conj(d21)*(           - 1 ) ) =
                           ! (     (      -1 )           ( d11/conj(d21) ) )
                       ! = 1/(|d21|**2) * 1/(d22*d11-1) *
                         ! * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                           ! (     (  -1 )           ( d22 ) )
                       ! = (1/|d21|**2) * t * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                                            ! (     (  -1 )           ( d22 ) )
                       ! = ( (t/conj(d21))*( d11 ) (t/d21)*(  -1 ) ) =
                         ! (               (  -1 )         ( d22 ) )
                       ! = ( conj(d21)*( d11 ) d21*(  -1 ) )
                         ! (           (  -1 )     ( d22 ) ),
                       ! where d11 = d22/d21,
                             ! d22 = d11/conj(d21),
                             ! d21 = t/d21,
                             ! t = 1/(d22*d11-1).
                       ! (note: no need to check for division by zero,
                        ! since that was ensured earlier in pivot search:
                        ! (a) d21 != 0, since in 2x2 pivot case(4)
                            ! |d21| should be larger than |d11| and |d22|;
                        ! (b) (d22*d11 - 1) != 0, since from (a),
                            ! both |d11| < 1, |d22| < 1, hence |d22*d11| << 1.)
                       d21 = w( k-1, kw )
                       d11 = w( k, kw ) / conjg( d21 )
                       d22 = w( k-1, kw-1 ) / d21
                       t = one / ( real( d11*d22,KIND=sp)-one )
                       d21 = t / d21
                       ! update elements in columns a(k-1) and a(k) as
                       ! dot products of rows of ( w(kw-1) w(kw) ) and columns
                       ! of d**(-1)
                       do j = 1, k - 2
                          a( j, k-1 ) = d21*( d11*w( j, kw-1 )-w( j, kw ) )
                          a( j, k ) = conjg( d21 )*( d22*w( j, kw )-w( j, kw-1 ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = w( k-1, kw )
                    a( k, k ) = w( k, kw )
                    ! (2) conjugate columns w(kw) and w(kw-1)
                    call stdlib_clacgv( k-1, w( 1_ilp, kw ), 1_ilp )
                    call stdlib_clacgv( k-2, w( 1_ilp, kw-1 ), 1_ilp )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**h = a11 - u12*w**h
              ! computing blocks of nb columns at a time (note that conjg(w) is
              ! actually stored)
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    a( jj, jj ) = real( a( jj, jj ),KIND=sp)
                    call stdlib_cgemv( 'NO TRANSPOSE', jj-j+1, n-k, -cone,a( j, k+1 ), lda, w( jj,&
                               kw+1 ), ldw, cone,a( j, jj ), 1_ilp )
                    a( jj, jj ) = real( a( jj, jj ),KIND=sp)
                 end do
                 ! update the rectangular superdiagonal block
                 call stdlib_cgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb, n-k,-cone, a( 1_ilp, k+1 ), &
                           lda, w( j, kw+1 ), ldw,cone, a( 1_ilp, j ), lda )
              end do
              ! put u12 in standard form by partially undoing the interchanges
              ! in of rows in columns k+1:n looping backwards from k+1 to n
              j = k + 1_ilp
              60 continue
                 ! undo the interchanges (if any) of rows j and jp
                 ! at each step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j + 1_ilp
                 end if
                 ! (note: here, j is used to determine row length. length n-j+1
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j + 1_ilp
                 if( jp/=jj .and. j<=n )call stdlib_cswap( n-j+1, a( jp, j ), lda, a( jj, j ), &
                           lda )
              if( j<=n )go to 60
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22 (note that conjg(w) is actually stored)
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              kstep = 1_ilp
              ! copy column k of a to column k of w and update it
              w( k, k ) = real( a( k, k ),KIND=sp)
              if( k<n )call stdlib_ccopy( n-k, a( k+1, k ), 1_ilp, w( k+1, k ), 1_ilp )
              call stdlib_cgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp ), lda,w( k, 1_ilp ), ldw,&
                         cone, w( k, k ), 1_ilp )
              w( k, k ) = real( w( k, k ),KIND=sp)
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( w( k, k ),KIND=sp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_icamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = cabs1( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=sp)
              else
                 ! ============================================================
                 ! begin pivot search
                 ! case(1)
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! begin pivot search along imax row
                    ! copy column imax to column k+1 of w and update it
                    call stdlib_ccopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp )
                    call stdlib_clacgv( imax-k, w( k, k+1 ), 1_ilp )
                    w( imax, k+1 ) = real( a( imax, imax ),KIND=sp)
                    if( imax<n )call stdlib_ccopy( n-imax, a( imax+1, imax ), 1_ilp,w( imax+1, k+1 ), &
                              1_ilp )
                    call stdlib_cgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp ),lda, w( imax, &
                              1_ilp ), ldw, cone, w( k, k+1 ),1_ilp )
                    w( imax, k+1 ) = real( w( imax, k+1 ),KIND=sp)
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value.
                    ! determine only rowmax.
                    jmax = k - 1_ilp + stdlib_icamax( imax-k, w( k, k+1 ), 1_ilp )
                    rowmax = cabs1( w( jmax, k+1 ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_icamax( n-imax, w( imax+1, k+1 ), 1_ilp )
                       rowmax = max( rowmax, cabs1( w( jmax, k+1 ) ) )
                    end if
                    ! case(2)
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    ! case(3)
                    else if( abs( real( w( imax, k+1 ),KIND=sp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column k+1 of w to column k of w
                       call stdlib_ccopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                    ! case(4)
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                    ! end pivot search along imax row
                 end if
                 ! end pivot search
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k + kstep - 1_ilp
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kk of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k+1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = real( a( kk, kk ),KIND=sp)
                    call stdlib_ccopy( kp-kk-1, a( kk+1, kk ), 1_ilp, a( kp, kk+1 ),lda )
                    call stdlib_clacgv( kp-kk-1, a( kp, kk+1 ), lda )
                    if( kp<n )call stdlib_ccopy( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    ! interchange rows kk and kp in first k-1 columns of a
                    ! (columns k (or k and k+1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in first kk columns of w.
                    if( k>1_ilp )call stdlib_cswap( k-1, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_cswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k),
                    ! where l(k) is the k-th column of l
                    ! (1) store subdiag. elements of column l(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! (note: diagonal element l(k,k) is a unit element
                    ! and not stored)
                       ! a(k,k) := d(k,k) = w(k,k)
                       ! a(k+1:n,k) := l(k+1:n,k) = w(k+1:n,k)/d(k,k)
                    ! (note: no need to use for hermitian matrix
                    ! a( k, k ) = real( w( k, k),KIND=sp) to separately copy diagonal
                    ! element d(k,k) from w (potentially saves only one load))
                    call stdlib_ccopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       ! (note: no need to check if a(k,k) is not zero,
                        ! since that was ensured earlier in pivot search:
                        ! case a(k,k) = 0 falls into 2x2 pivot case(4))
                       r1 = one / real( a( k, k ),KIND=sp)
                       call stdlib_csscal( n-k, r1, a( k+1, k ), 1_ilp )
                       ! (2) conjugate column w(k)
                       call stdlib_clacgv( n-k, w( k+1, k ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! (1) store l(k+2:n,k) and l(k+2:n,k+1) and 2-by-2
                    ! block d(k:k+1,k:k+1) in columns k and k+1 of a.
                    ! (note: 2-by-2 diagonal block l(k:k+1,k:k+1) is a unit
                    ! block and not stored)
                       ! a(k:k+1,k:k+1) := d(k:k+1,k:k+1) = w(k:k+1,k:k+1)
                       ! a(k+2:n,k:k+1) := l(k+2:n,k:k+1) =
                       ! = w(k+2:n,k:k+1) * ( d(k:k+1,k:k+1)**(-1) )
                    if( k<n-1 ) then
                       ! factor out the columns of the inverse of 2-by-2 pivot
                       ! block d, so that each column contains 1, to reduce the
                       ! number of flops when we multiply panel
                       ! ( w(kw-1) w(kw) ) by this inverse, i.e. by d**(-1).
                       ! d**(-1) = ( d11 cj(d21) )**(-1) =
                                 ! ( d21    d22 )
                       ! = 1/(d11*d22-|d21|**2) * ( ( d22) (-cj(d21) ) ) =
                                                ! ( (-d21) (     d11 ) )
                       ! = 1/(|d21|**2) * 1/((d11/cj(d21))*(d22/d21)-1) *
                         ! * ( d21*( d22/d21 ) conj(d21)*(           - 1 ) ) =
                           ! (     (      -1 )           ( d11/conj(d21) ) )
                       ! = 1/(|d21|**2) * 1/(d22*d11-1) *
                         ! * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                           ! (     (  -1 )           ( d22 ) )
                       ! = (1/|d21|**2) * t * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                                            ! (     (  -1 )           ( d22 ) )
                       ! = ( (t/conj(d21))*( d11 ) (t/d21)*(  -1 ) ) =
                         ! (               (  -1 )         ( d22 ) )
                       ! = ( conj(d21)*( d11 ) d21*(  -1 ) )
                         ! (           (  -1 )     ( d22 ) )
                       ! where d11 = d22/d21,
                             ! d22 = d11/conj(d21),
                             ! d21 = t/d21,
                             ! t = 1/(d22*d11-1).
                       ! (note: no need to check for division by zero,
                        ! since that was ensured earlier in pivot search:
                        ! (a) d21 != 0, since in 2x2 pivot case(4)
                            ! |d21| should be larger than |d11| and |d22|;
                        ! (b) (d22*d11 - 1) != 0, since from (a),
                            ! both |d11| < 1, |d22| < 1, hence |d22*d11| << 1.)
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / conjg( d21 )
                       t = one / ( real( d11*d22,KIND=sp)-one )
                       d21 = t / d21
                       ! update elements in columns a(k) and a(k+1) as
                       ! dot products of rows of ( w(k) w(k+1) ) and columns
                       ! of d**(-1)
                       do j = k + 2, n
                          a( j, k ) = conjg( d21 )*( d11*w( j, k )-w( j, k+1 ) )
                          a( j, k+1 ) = d21*( d22*w( j, k+1 )-w( j, k ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = w( k+1, k )
                    a( k+1, k+1 ) = w( k+1, k+1 )
                    ! (2) conjugate columns w(k) and w(k+1)
                    call stdlib_clacgv( n-k, w( k+1, k ), 1_ilp )
                    call stdlib_clacgv( n-k-1, w( k+2, k+1 ), 1_ilp )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**h = a22 - l21*w**h
              ! computing blocks of nb columns at a time (note that conjg(w) is
              ! actually stored)
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    a( jj, jj ) = real( a( jj, jj ),KIND=sp)
                    call stdlib_cgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -cone,a( jj, 1_ilp ), lda, w( jj,&
                               1_ilp ), ldw, cone,a( jj, jj ), 1_ilp )
                    a( jj, jj ) = real( a( jj, jj ),KIND=sp)
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_cgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           cone, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ),ldw, cone, a( j+jb, j ), lda )
              end do
              ! put l21 in standard form by partially undoing the interchanges
              ! of rows in columns 1:k-1 looping backwards from k-1 to 1
              j = k - 1_ilp
              120 continue
                 ! undo the interchanges (if any) of rows j and jp
                 ! at each step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j - 1_ilp
                 end if
                 ! (note: here, j is used to determine row length. length j
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j - 1_ilp
                 if( jp/=jj .and. j>=1_ilp )call stdlib_cswap( j, a( jp, 1_ilp ), lda, a( jj, 1_ilp ), lda )
                           
              if( j>=1 )go to 120
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_clahef

     pure module subroutine stdlib_zlahef( uplo, n, nb, kb, a, lda, ipiv, w, ldw, info )
     !! ZLAHEF computes a partial factorization of a complex Hermitian
     !! matrix A using the Bunch-Kaufman diagonal pivoting method. The
     !! partial factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I      0     )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**H U22**H )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**H L21**H )  if UPLO = 'L'
     !! ( L21  I ) (  0  A22 ) (  0      I     )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! Note that U**H denotes the conjugate transpose of U.
     !! ZLAHEF is an auxiliary routine called by ZHETRF. It uses blocked code
     !! (calling Level 3 BLAS) to update the submatrix A11 (if UPLO = 'U') or
     !! A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, kb
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           
           ! Local Scalars 
           integer(ilp) :: imax, j, jb, jj, jmax, jp, k, kk, kkw, kp, kstep, kw
           real(dp) :: absakk, alpha, colmax, r1, rowmax, t
           complex(dp) :: d11, d21, d22, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=dp) ) + abs( aimag( z ) )
           ! Executable Statements 
           info = 0_ilp
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11 (note that conjg(w) is actually stored)
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              ! kw is the column of w which corresponds to column k of a
              k = n
              10 continue
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              kstep = 1_ilp
              ! copy column k of a to column kw of w and update it
              call stdlib_zcopy( k-1, a( 1_ilp, k ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
              w( k, kw ) = real( a( k, k ),KIND=dp)
              if( k<n ) then
                 call stdlib_zgemv( 'NO TRANSPOSE', k, n-k, -cone, a( 1_ilp, k+1 ), lda,w( k, kw+1 ), &
                           ldw, cone, w( 1_ilp, kw ), 1_ilp )
                 w( k, kw ) = real( w( k, kw ),KIND=dp)
              end if
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( w( k, kw ),KIND=dp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_izamax( k-1, w( 1_ilp, kw ), 1_ilp )
                 colmax = cabs1( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=dp)
              else
                 ! ============================================================
                 ! begin pivot search
                 ! case(1)
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! begin pivot search along imax row
                    ! copy column imax to column kw-1 of w and update it
                    call stdlib_zcopy( imax-1, a( 1_ilp, imax ), 1_ilp, w( 1_ilp, kw-1 ), 1_ilp )
                    w( imax, kw-1 ) = real( a( imax, imax ),KIND=dp)
                    call stdlib_zcopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp )
                              
                    call stdlib_zlacgv( k-imax, w( imax+1, kw-1 ), 1_ilp )
                    if( k<n ) then
                       call stdlib_zgemv( 'NO TRANSPOSE', k, n-k, -cone,a( 1_ilp, k+1 ), lda, w( imax,&
                                  kw+1 ), ldw,cone, w( 1_ilp, kw-1 ), 1_ilp )
                       w( imax, kw-1 ) = real( w( imax, kw-1 ),KIND=dp)
                    end if
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value.
                    ! determine only rowmax.
                    jmax = imax + stdlib_izamax( k-imax, w( imax+1, kw-1 ), 1_ilp )
                    rowmax = cabs1( w( jmax, kw-1 ) )
                    if( imax>1_ilp ) then
                       jmax = stdlib_izamax( imax-1, w( 1_ilp, kw-1 ), 1_ilp )
                       rowmax = max( rowmax, cabs1( w( jmax, kw-1 ) ) )
                    end if
                    ! case(2)
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    ! case(3)
                    else if( abs( real( w( imax, kw-1 ),KIND=dp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column kw-1 of w to column kw of w
                       call stdlib_zcopy( k, w( 1_ilp, kw-1 ), 1_ilp, w( 1_ilp, kw ), 1_ilp )
                    ! case(4)
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                    ! end pivot search along imax row
                 end if
                 ! end pivot search
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k - kstep + 1_ilp
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kkw of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k-1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = real( a( kk, kk ),KIND=dp)
                    call stdlib_zcopy( kk-1-kp, a( kp+1, kk ), 1_ilp, a( kp, kp+1 ),lda )
                    call stdlib_zlacgv( kk-1-kp, a( kp, kp+1 ), lda )
                    if( kp>1_ilp )call stdlib_zcopy( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    ! interchange rows kk and kp in last k+1 to n columns of a
                    ! (columns k (or k and k-1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in last kkw to nb columns of w.
                    if( k<n )call stdlib_zswap( n-k, a( kk, k+1 ), lda, a( kp, k+1 ),lda )
                    call stdlib_zswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(kw) = u(k)*d(k),
                    ! where u(k) is the k-th column of u
                    ! (1) store subdiag. elements of column u(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! (note: diagonal element u(k,k) is a unit element
                    ! and not stored)
                       ! a(k,k) := d(k,k) = w(k,kw)
                       ! a(1:k-1,k) := u(1:k-1,k) = w(1:k-1,kw)/d(k,k)
                    ! (note: no need to use for hermitian matrix
                    ! a( k, k ) = real( w( k, k),KIND=dp) to separately copy diagonal
                    ! element d(k,k) from w (potentially saves only one load))
                    call stdlib_zcopy( k, w( 1_ilp, kw ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    if( k>1_ilp ) then
                       ! (note: no need to check if a(k,k) is not zero,
                        ! since that was ensured earlier in pivot search:
                        ! case a(k,k) = 0 falls into 2x2 pivot case(4))
                       r1 = one / real( a( k, k ),KIND=dp)
                       call stdlib_zdscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                       ! (2) conjugate column w(kw)
                       call stdlib_zlacgv( k-1, w( 1_ilp, kw ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now hold
                    ! ( w(kw-1) w(kw) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! (1) store u(1:k-2,k-1) and u(1:k-2,k) and 2-by-2
                    ! block d(k-1:k,k-1:k) in columns k-1 and k of a.
                    ! (note: 2-by-2 diagonal block u(k-1:k,k-1:k) is a unit
                    ! block and not stored)
                       ! a(k-1:k,k-1:k) := d(k-1:k,k-1:k) = w(k-1:k,kw-1:kw)
                       ! a(1:k-2,k-1:k) := u(1:k-2,k:k-1:k) =
                       ! = w(1:k-2,kw-1:kw) * ( d(k-1:k,k-1:k)**(-1) )
                    if( k>2_ilp ) then
                       ! factor out the columns of the inverse of 2-by-2 pivot
                       ! block d, so that each column contains 1, to reduce the
                       ! number of flops when we multiply panel
                       ! ( w(kw-1) w(kw) ) by this inverse, i.e. by d**(-1).
                       ! d**(-1) = ( d11 cj(d21) )**(-1) =
                                 ! ( d21    d22 )
                       ! = 1/(d11*d22-|d21|**2) * ( ( d22) (-cj(d21) ) ) =
                                                ! ( (-d21) (     d11 ) )
                       ! = 1/(|d21|**2) * 1/((d11/cj(d21))*(d22/d21)-1) *
                         ! * ( d21*( d22/d21 ) conj(d21)*(           - 1 ) ) =
                           ! (     (      -1 )           ( d11/conj(d21) ) )
                       ! = 1/(|d21|**2) * 1/(d22*d11-1) *
                         ! * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                           ! (     (  -1 )           ( d22 ) )
                       ! = (1/|d21|**2) * t * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                                            ! (     (  -1 )           ( d22 ) )
                       ! = ( (t/conj(d21))*( d11 ) (t/d21)*(  -1 ) ) =
                         ! (               (  -1 )         ( d22 ) )
                       ! = ( conj(d21)*( d11 ) d21*(  -1 ) )
                         ! (           (  -1 )     ( d22 ) ),
                       ! where d11 = d22/d21,
                             ! d22 = d11/conj(d21),
                             ! d21 = t/d21,
                             ! t = 1/(d22*d11-1).
                       ! (note: no need to check for division by zero,
                        ! since that was ensured earlier in pivot search:
                        ! (a) d21 != 0, since in 2x2 pivot case(4)
                            ! |d21| should be larger than |d11| and |d22|;
                        ! (b) (d22*d11 - 1) != 0, since from (a),
                            ! both |d11| < 1, |d22| < 1, hence |d22*d11| << 1.)
                       d21 = w( k-1, kw )
                       d11 = w( k, kw ) / conjg( d21 )
                       d22 = w( k-1, kw-1 ) / d21
                       t = one / ( real( d11*d22,KIND=dp)-one )
                       d21 = t / d21
                       ! update elements in columns a(k-1) and a(k) as
                       ! dot products of rows of ( w(kw-1) w(kw) ) and columns
                       ! of d**(-1)
                       do j = 1, k - 2
                          a( j, k-1 ) = d21*( d11*w( j, kw-1 )-w( j, kw ) )
                          a( j, k ) = conjg( d21 )*( d22*w( j, kw )-w( j, kw-1 ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = w( k-1, kw )
                    a( k, k ) = w( k, kw )
                    ! (2) conjugate columns w(kw) and w(kw-1)
                    call stdlib_zlacgv( k-1, w( 1_ilp, kw ), 1_ilp )
                    call stdlib_zlacgv( k-2, w( 1_ilp, kw-1 ), 1_ilp )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**h = a11 - u12*w**h
              ! computing blocks of nb columns at a time (note that conjg(w) is
              ! actually stored)
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    a( jj, jj ) = real( a( jj, jj ),KIND=dp)
                    call stdlib_zgemv( 'NO TRANSPOSE', jj-j+1, n-k, -cone,a( j, k+1 ), lda, w( jj,&
                               kw+1 ), ldw, cone,a( j, jj ), 1_ilp )
                    a( jj, jj ) = real( a( jj, jj ),KIND=dp)
                 end do
                 ! update the rectangular superdiagonal block
                 call stdlib_zgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb, n-k,-cone, a( 1_ilp, k+1 ), &
                           lda, w( j, kw+1 ), ldw,cone, a( 1_ilp, j ), lda )
              end do
              ! put u12 in standard form by partially undoing the interchanges
              ! in columns k+1:n looping backwards from k+1 to n
              j = k + 1_ilp
              60 continue
                 ! undo the interchanges (if any) of rows jj and jp at each
                 ! step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j + 1_ilp
                 end if
                 ! (note: here, j is used to determine row length. length n-j+1
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j + 1_ilp
                 if( jp/=jj .and. j<=n )call stdlib_zswap( n-j+1, a( jp, j ), lda, a( jj, j ), &
                           lda )
              if( j<n )go to 60
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22 (note that conjg(w) is actually stored)
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              kstep = 1_ilp
              ! copy column k of a to column k of w and update it
              w( k, k ) = real( a( k, k ),KIND=dp)
              if( k<n )call stdlib_zcopy( n-k, a( k+1, k ), 1_ilp, w( k+1, k ), 1_ilp )
              call stdlib_zgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp ), lda,w( k, 1_ilp ), ldw,&
                         cone, w( k, k ), 1_ilp )
              w( k, k ) = real( w( k, k ),KIND=dp)
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( w( k, k ),KIND=dp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_izamax( n-k, w( k+1, k ), 1_ilp )
                 colmax = cabs1( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=dp)
              else
                 ! ============================================================
                 ! begin pivot search
                 ! case(1)
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! begin pivot search along imax row
                    ! copy column imax to column k+1 of w and update it
                    call stdlib_zcopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp )
                    call stdlib_zlacgv( imax-k, w( k, k+1 ), 1_ilp )
                    w( imax, k+1 ) = real( a( imax, imax ),KIND=dp)
                    if( imax<n )call stdlib_zcopy( n-imax, a( imax+1, imax ), 1_ilp,w( imax+1, k+1 ), &
                              1_ilp )
                    call stdlib_zgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp ),lda, w( imax, &
                              1_ilp ), ldw, cone, w( k, k+1 ),1_ilp )
                    w( imax, k+1 ) = real( w( imax, k+1 ),KIND=dp)
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value.
                    ! determine only rowmax.
                    jmax = k - 1_ilp + stdlib_izamax( imax-k, w( k, k+1 ), 1_ilp )
                    rowmax = cabs1( w( jmax, k+1 ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_izamax( n-imax, w( imax+1, k+1 ), 1_ilp )
                       rowmax = max( rowmax, cabs1( w( jmax, k+1 ) ) )
                    end if
                    ! case(2)
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    ! case(3)
                    else if( abs( real( w( imax, k+1 ),KIND=dp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column k+1 of w to column k of w
                       call stdlib_zcopy( n-k+1, w( k, k+1 ), 1_ilp, w( k, k ), 1_ilp )
                    ! case(4)
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                    ! end pivot search along imax row
                 end if
                 ! end pivot search
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k + kstep - 1_ilp
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kk of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k+1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = real( a( kk, kk ),KIND=dp)
                    call stdlib_zcopy( kp-kk-1, a( kk+1, kk ), 1_ilp, a( kp, kk+1 ),lda )
                    call stdlib_zlacgv( kp-kk-1, a( kp, kk+1 ), lda )
                    if( kp<n )call stdlib_zcopy( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    ! interchange rows kk and kp in first k-1 columns of a
                    ! (columns k (or k and k+1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in first kk columns of w.
                    if( k>1_ilp )call stdlib_zswap( k-1, a( kk, 1_ilp ), lda, a( kp, 1_ilp ), lda )
                    call stdlib_zswap( kk, w( kk, 1_ilp ), ldw, w( kp, 1_ilp ), ldw )
                 end if
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k),
                    ! where l(k) is the k-th column of l
                    ! (1) store subdiag. elements of column l(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! (note: diagonal element l(k,k) is a unit element
                    ! and not stored)
                       ! a(k,k) := d(k,k) = w(k,k)
                       ! a(k+1:n,k) := l(k+1:n,k) = w(k+1:n,k)/d(k,k)
                    ! (note: no need to use for hermitian matrix
                    ! a( k, k ) = real( w( k, k),KIND=dp) to separately copy diagonal
                    ! element d(k,k) from w (potentially saves only one load))
                    call stdlib_zcopy( n-k+1, w( k, k ), 1_ilp, a( k, k ), 1_ilp )
                    if( k<n ) then
                       ! (note: no need to check if a(k,k) is not zero,
                        ! since that was ensured earlier in pivot search:
                        ! case a(k,k) = 0 falls into 2x2 pivot case(4))
                       r1 = one / real( a( k, k ),KIND=dp)
                       call stdlib_zdscal( n-k, r1, a( k+1, k ), 1_ilp )
                       ! (2) conjugate column w(k)
                       call stdlib_zlacgv( n-k, w( k+1, k ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! (1) store l(k+2:n,k) and l(k+2:n,k+1) and 2-by-2
                    ! block d(k:k+1,k:k+1) in columns k and k+1 of a.
                    ! (note: 2-by-2 diagonal block l(k:k+1,k:k+1) is a unit
                    ! block and not stored)
                       ! a(k:k+1,k:k+1) := d(k:k+1,k:k+1) = w(k:k+1,k:k+1)
                       ! a(k+2:n,k:k+1) := l(k+2:n,k:k+1) =
                       ! = w(k+2:n,k:k+1) * ( d(k:k+1,k:k+1)**(-1) )
                    if( k<n-1 ) then
                       ! factor out the columns of the inverse of 2-by-2 pivot
                       ! block d, so that each column contains 1, to reduce the
                       ! number of flops when we multiply panel
                       ! ( w(kw-1) w(kw) ) by this inverse, i.e. by d**(-1).
                       ! d**(-1) = ( d11 cj(d21) )**(-1) =
                                 ! ( d21    d22 )
                       ! = 1/(d11*d22-|d21|**2) * ( ( d22) (-cj(d21) ) ) =
                                                ! ( (-d21) (     d11 ) )
                       ! = 1/(|d21|**2) * 1/((d11/cj(d21))*(d22/d21)-1) *
                         ! * ( d21*( d22/d21 ) conj(d21)*(           - 1 ) ) =
                           ! (     (      -1 )           ( d11/conj(d21) ) )
                       ! = 1/(|d21|**2) * 1/(d22*d11-1) *
                         ! * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                           ! (     (  -1 )           ( d22 ) )
                       ! = (1/|d21|**2) * t * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                                            ! (     (  -1 )           ( d22 ) )
                       ! = ( (t/conj(d21))*( d11 ) (t/d21)*(  -1 ) ) =
                         ! (               (  -1 )         ( d22 ) )
                       ! = ( conj(d21)*( d11 ) d21*(  -1 ) )
                         ! (           (  -1 )     ( d22 ) ),
                       ! where d11 = d22/d21,
                             ! d22 = d11/conj(d21),
                             ! d21 = t/d21,
                             ! t = 1/(d22*d11-1).
                       ! (note: no need to check for division by zero,
                        ! since that was ensured earlier in pivot search:
                        ! (a) d21 != 0, since in 2x2 pivot case(4)
                            ! |d21| should be larger than |d11| and |d22|;
                        ! (b) (d22*d11 - 1) != 0, since from (a),
                            ! both |d11| < 1, |d22| < 1, hence |d22*d11| << 1.)
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / conjg( d21 )
                       t = one / ( real( d11*d22,KIND=dp)-one )
                       d21 = t / d21
                       ! update elements in columns a(k) and a(k+1) as
                       ! dot products of rows of ( w(k) w(k+1) ) and columns
                       ! of d**(-1)
                       do j = k + 2, n
                          a( j, k ) = conjg( d21 )*( d11*w( j, k )-w( j, k+1 ) )
                          a( j, k+1 ) = d21*( d22*w( j, k+1 )-w( j, k ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = w( k+1, k )
                    a( k+1, k+1 ) = w( k+1, k+1 )
                    ! (2) conjugate columns w(k) and w(k+1)
                    call stdlib_zlacgv( n-k, w( k+1, k ), 1_ilp )
                    call stdlib_zlacgv( n-k-1, w( k+2, k+1 ), 1_ilp )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**h = a22 - l21*w**h
              ! computing blocks of nb columns at a time (note that conjg(w) is
              ! actually stored)
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    a( jj, jj ) = real( a( jj, jj ),KIND=dp)
                    call stdlib_zgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -cone,a( jj, 1_ilp ), lda, w( jj,&
                               1_ilp ), ldw, cone,a( jj, jj ), 1_ilp )
                    a( jj, jj ) = real( a( jj, jj ),KIND=dp)
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_zgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           cone, a( j+jb, 1_ilp ), lda, w( j, 1_ilp ),ldw, cone, a( j+jb, j ), lda )
              end do
              ! put l21 in standard form by partially undoing the interchanges
              ! of rows in columns 1:k-1 looping backwards from k-1 to 1
              j = k - 1_ilp
              120 continue
                 ! undo the interchanges (if any) of rows jj and jp at each
                 ! step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j - 1_ilp
                 end if
                 ! (note: here, j is used to determine row length. length j
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j - 1_ilp
                 if( jp/=jj .and. j>=1_ilp )call stdlib_zswap( j, a( jp, 1_ilp ), lda, a( jj, 1_ilp ), lda )
                           
              if( j>1 )go to 120
              ! set kb to the number of columns factorized
              kb = k - 1_ilp
           end if
           return
     end subroutine stdlib_zlahef




     pure module subroutine stdlib_chetf2( uplo, n, a, lda, ipiv, info )
     !! CHETF2 computes the factorization of a complex Hermitian matrix A
     !! using the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**H  or  A = L*D*L**H
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, U**H is the conjugate transpose of U, and D is
     !! Hermitian and block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, imax, j, jmax, k, kk, kp, kstep
           real(sp) :: absakk, alpha, colmax, d, d11, d22, r1, rowmax, tt
           complex(sp) :: d12, d21, t, wk, wkm1, wkp1, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHETF2', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**h using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 90
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( a( k, k ),KIND=sp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_icamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) .or. stdlib_sisnan(absakk) ) then
                 ! column k is or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=sp)
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = imax + stdlib_icamax( k-imax, a( imax, imax+1 ), lda )
                    rowmax = cabs1( a( imax, jmax ) )
                    if( imax>1_ilp ) then
                       jmax = stdlib_icamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                       rowmax = max( rowmax, cabs1( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( a( imax, imax ),KIND=sp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_cswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    do j = kp + 1, kk - 1
                       t = conjg( a( j, kk ) )
                       a( j, kk ) = conjg( a( kp, j ) )
                       a( kp, j ) = t
                    end do
                    a( kp, kk ) = conjg( a( kp, kk ) )
                    r1 = real( a( kk, kk ),KIND=sp)
                    a( kk, kk ) = real( a( kp, kp ),KIND=sp)
                    a( kp, kp ) = r1
                    if( kstep==2_ilp ) then
                       a( k, k ) = real( a( k, k ),KIND=sp)
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 else
                    a( k, k ) = real( a( k, k ),KIND=sp)
                    if( kstep==2_ilp )a( k-1, k-1 ) = real( a( k-1, k-1 ),KIND=sp)
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**h = a - w(k)*1/d(k)*w(k)**h
                    r1 = one / real( a( k, k ),KIND=sp)
                    call stdlib_cher( uplo, k-1, -r1, a( 1_ilp, k ), 1_ilp, a, lda )
                    ! store u(k) in column k
                    call stdlib_csscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**h
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**h
                    if( k>2_ilp ) then
                       d = stdlib_slapy2( real( a( k-1, k ),KIND=sp),aimag( a( k-1, k ) ) )
                                 
                       d22 = real( a( k-1, k-1 ),KIND=sp) / d
                       d11 = real( a( k, k ),KIND=sp) / d
                       tt = one / ( d11*d22-one )
                       d12 = a( k-1, k ) / d
                       d = tt / d
                       do j = k - 2, 1, -1
                          wkm1 = d*( d11*a( j, k-1 )-conjg( d12 )*a( j, k ) )
                          wk = d*( d22*a( j, k )-d12*a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - a( i, k )*conjg( wk ) -a( i, k-1 )*conjg( &
                                       wkm1 )
                          end do
                          a( j, k ) = wk
                          a( j, k-1 ) = wkm1
                          a( j, j ) = cmplx( real( a( j, j ),KIND=sp), zero,KIND=sp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
           else
              ! factorize a as l*d*l**h using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              50 continue
              ! if k > n, exit from loop
              if( k>n )go to 90
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( a( k, k ),KIND=sp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_icamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) .or. stdlib_sisnan(absakk) ) then
                 ! column k is zero or underflow, contains a nan:
                 ! set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=sp)
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = k - 1_ilp + stdlib_icamax( imax-k, a( imax, k ), lda )
                    rowmax = cabs1( a( imax, jmax ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_icamax( n-imax, a( imax+1, imax ), 1_ilp )
                       rowmax = max( rowmax, cabs1( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( a( imax, imax ),KIND=sp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_cswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    do j = kk + 1, kp - 1
                       t = conjg( a( j, kk ) )
                       a( j, kk ) = conjg( a( kp, j ) )
                       a( kp, j ) = t
                    end do
                    a( kp, kk ) = conjg( a( kp, kk ) )
                    r1 = real( a( kk, kk ),KIND=sp)
                    a( kk, kk ) = real( a( kp, kp ),KIND=sp)
                    a( kp, kp ) = r1
                    if( kstep==2_ilp ) then
                       a( k, k ) = real( a( k, k ),KIND=sp)
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 else
                    a( k, k ) = real( a( k, k ),KIND=sp)
                    if( kstep==2_ilp )a( k+1, k+1 ) = real( a( k+1, k+1 ),KIND=sp)
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**h = a - w(k)*(1/d(k))*w(k)**h
                       r1 = one / real( a( k, k ),KIND=sp)
                       call stdlib_cher( uplo, n-k, -r1, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                 
                       ! store l(k) in column k
                       call stdlib_csscal( n-k, r1, a( k+1, k ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k)
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**h
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**h
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d = stdlib_slapy2( real( a( k+1, k ),KIND=sp),aimag( a( k+1, k ) ) )
                                 
                       d11 = real( a( k+1, k+1 ),KIND=sp) / d
                       d22 = real( a( k, k ),KIND=sp) / d
                       tt = one / ( d11*d22-one )
                       d21 = a( k+1, k ) / d
                       d =  tt / d
                       do j = k + 2, n
                          wk = d*( d11*a( j, k )-d21*a( j, k+1 ) )
                          wkp1 = d*( d22*a( j, k+1 )-conjg( d21 )*a( j, k ) )
                          do i = j, n
                             a( i, j ) = a( i, j ) - a( i, k )*conjg( wk ) -a( i, k+1 )*conjg( &
                                       wkp1 )
                          end do
                          a( j, k ) = wk
                          a( j, k+1 ) = wkp1
                          a( j, j ) = cmplx( real( a( j, j ),KIND=sp), zero,KIND=sp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 50
           end if
           90 continue
           return
     end subroutine stdlib_chetf2

     pure module subroutine stdlib_zhetf2( uplo, n, a, lda, ipiv, info )
     !! ZHETF2 computes the factorization of a complex Hermitian matrix A
     !! using the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**H  or  A = L*D*L**H
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, U**H is the conjugate transpose of U, and D is
     !! Hermitian and block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, imax, j, jmax, k, kk, kp, kstep
           real(dp) :: absakk, alpha, colmax, d, d11, d22, r1, rowmax, tt
           complex(dp) :: d12, d21, t, wk, wkm1, wkp1, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHETF2', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**h using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 90
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( a( k, k ),KIND=dp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp ) then
                 imax = stdlib_izamax( k-1, a( 1_ilp, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) .or. stdlib_disnan(absakk) ) then
                 ! column k is zero or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=dp)
              else
                 ! ============================================================
                 ! test for interchange
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value.
                    ! determine only rowmax.
                    jmax = imax + stdlib_izamax( k-imax, a( imax, imax+1 ), lda )
                    rowmax = cabs1( a( imax, jmax ) )
                    if( imax>1_ilp ) then
                       jmax = stdlib_izamax( imax-1, a( 1_ilp, imax ), 1_ilp )
                       rowmax = max( rowmax, cabs1( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( a( imax, imax ),KIND=dp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 ! ============================================================
                 kk = k - kstep + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_zswap( kp-1, a( 1_ilp, kk ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                    do j = kp + 1, kk - 1
                       t = conjg( a( j, kk ) )
                       a( j, kk ) = conjg( a( kp, j ) )
                       a( kp, j ) = t
                    end do
                    a( kp, kk ) = conjg( a( kp, kk ) )
                    r1 = real( a( kk, kk ),KIND=dp)
                    a( kk, kk ) = real( a( kp, kp ),KIND=dp)
                    a( kp, kp ) = r1
                    if( kstep==2_ilp ) then
                       a( k, k ) = real( a( k, k ),KIND=dp)
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 else
                    a( k, k ) = real( a( k, k ),KIND=dp)
                    if( kstep==2_ilp )a( k-1, k-1 ) = real( a( k-1, k-1 ),KIND=dp)
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**h = a - w(k)*1/d(k)*w(k)**h
                    r1 = one / real( a( k, k ),KIND=dp)
                    call stdlib_zher( uplo, k-1, -r1, a( 1_ilp, k ), 1_ilp, a, lda )
                    ! store u(k) in column k
                    call stdlib_zdscal( k-1, r1, a( 1_ilp, k ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**h
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**h
                    if( k>2_ilp ) then
                       d = stdlib_dlapy2( real( a( k-1, k ),KIND=dp),aimag( a( k-1, k ) ) )
                                 
                       d22 = real( a( k-1, k-1 ),KIND=dp) / d
                       d11 = real( a( k, k ),KIND=dp) / d
                       tt = one / ( d11*d22-one )
                       d12 = a( k-1, k ) / d
                       d = tt / d
                       do j = k - 2, 1, -1
                          wkm1 = d*( d11*a( j, k-1 )-conjg( d12 )*a( j, k ) )
                          wk = d*( d22*a( j, k )-d12*a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - a( i, k )*conjg( wk ) -a( i, k-1 )*conjg( &
                                       wkm1 )
                          end do
                          a( j, k ) = wk
                          a( j, k-1 ) = wkm1
                          a( j, j ) = cmplx( real( a( j, j ),KIND=dp), zero,KIND=dp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
           else
              ! factorize a as l*d*l**h using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              50 continue
              ! if k > n, exit from loop
              if( k>n )go to 90
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( a( k, k ),KIND=dp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_izamax( n-k, a( k+1, k ), 1_ilp )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) .or. stdlib_disnan(absakk) ) then
                 ! column k is zero or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=dp)
              else
                 ! ============================================================
                 ! test for interchange
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value.
                    ! determine only rowmax.
                    jmax = k - 1_ilp + stdlib_izamax( imax-k, a( imax, k ), lda )
                    rowmax = cabs1( a( imax, jmax ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_izamax( n-imax, a( imax+1, imax ), 1_ilp )
                       rowmax = max( rowmax, cabs1( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( a( imax, imax ),KIND=dp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 ! ============================================================
                 kk = k + kstep - 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_zswap( n-kp, a( kp+1, kk ), 1_ilp, a( kp+1, kp ), 1_ilp )
                              
                    do j = kk + 1, kp - 1
                       t = conjg( a( j, kk ) )
                       a( j, kk ) = conjg( a( kp, j ) )
                       a( kp, j ) = t
                    end do
                    a( kp, kk ) = conjg( a( kp, kk ) )
                    r1 = real( a( kk, kk ),KIND=dp)
                    a( kk, kk ) = real( a( kp, kp ),KIND=dp)
                    a( kp, kp ) = r1
                    if( kstep==2_ilp ) then
                       a( k, k ) = real( a( k, k ),KIND=dp)
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 else
                    a( k, k ) = real( a( k, k ),KIND=dp)
                    if( kstep==2_ilp )a( k+1, k+1 ) = real( a( k+1, k+1 ),KIND=dp)
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**h = a - w(k)*(1/d(k))*w(k)**h
                       r1 = one / real( a( k, k ),KIND=dp)
                       call stdlib_zher( uplo, n-k, -r1, a( k+1, k ), 1_ilp,a( k+1, k+1 ), lda )
                                 
                       ! store l(k) in column k
                       call stdlib_zdscal( n-k, r1, a( k+1, k ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k)
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**h
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**h
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d = stdlib_dlapy2( real( a( k+1, k ),KIND=dp),aimag( a( k+1, k ) ) )
                                 
                       d11 = real( a( k+1, k+1 ),KIND=dp) / d
                       d22 = real( a( k, k ),KIND=dp) / d
                       tt = one / ( d11*d22-one )
                       d21 = a( k+1, k ) / d
                       d = tt / d
                       do j = k + 2, n
                          wk = d*( d11*a( j, k )-d21*a( j, k+1 ) )
                          wkp1 = d*( d22*a( j, k+1 )-conjg( d21 )*a( j, k ) )
                          do i = j, n
                             a( i, j ) = a( i, j ) - a( i, k )*conjg( wk ) -a( i, k+1 )*conjg( &
                                       wkp1 )
                          end do
                          a( j, k ) = wk
                          a( j, k+1 ) = wkp1
                          a( j, j ) = cmplx( real( a( j, j ),KIND=dp), zero,KIND=dp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 50
           end if
           90 continue
           return
     end subroutine stdlib_zhetf2




     pure module subroutine stdlib_chetrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
     !! CHETRS solves a system of linear equations A*X = B with a complex
     !! Hermitian matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by CHETRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kp
           real(sp) :: s
           complex(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHETRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**h.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_cgeru( k-1, nrhs, -cone, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb &
                           )
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=sp) / real( a( k, k ),KIND=sp)
                 call stdlib_csscal( nrhs, s, b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_cswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_cgeru( k-2, nrhs, -cone, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb &
                           )
                 call stdlib_cgeru( k-2, nrhs, -cone, a( 1_ilp, k-1 ), 1_ilp, b( k-1, 1_ilp ),ldb, b( 1_ilp, 1_ilp ), &
                           ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k-1, k )
                 akm1 = a( k-1, k-1 ) / akm1k
                 ak = a( k, k ) / conjg( akm1k )
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / conjg( akm1k )
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**h *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**h(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 if( k>1_ilp ) then
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp, k ), &
                              1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**h(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k>1_ilp ) then
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp, k ), &
                              1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k+1, 1_ilp ), ldb )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp, k+1 )&
                              , 1_ilp, cone, b( k+1, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k+1, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**h.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_cgeru( n-k, nrhs, -cone, a( k+1, k ), 1_ilp, b( k, 1_ilp ),ldb, b( &
                           k+1, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=sp) / real( a( k, k ),KIND=sp)
                 call stdlib_csscal( nrhs, s, b( k, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_cswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_cgeru( n-k-1, nrhs, -cone, a( k+2, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, &
                              1_ilp ), ldb )
                    call stdlib_cgeru( n-k-1, nrhs, -cone, a( k+2, k+1 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( &
                              k+2, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k+1, k )
                 akm1 = a( k, k ) / conjg( akm1k )
                 ak = a( k+1, k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / conjg( akm1k )
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**h *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**h(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n ) then
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp ), ldb, &
                              a( k+1, k ), 1_ilp, cone,b( k, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**h(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp ), ldb, &
                              a( k+1, k ), 1_ilp, cone,b( k, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k-1, 1_ilp ), ldb )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp ), ldb, &
                              a( k+1, k-1 ), 1_ilp, cone,b( k-1, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k-1, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_chetrs

     pure module subroutine stdlib_zhetrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
     !! ZHETRS solves a system of linear equations A*X = B with a complex
     !! Hermitian matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by ZHETRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kp
           real(dp) :: s
           complex(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHETRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**h.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_zgeru( k-1, nrhs, -cone, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb &
                           )
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=dp) / real( a( k, k ),KIND=dp)
                 call stdlib_zdscal( nrhs, s, b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_zswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_zgeru( k-2, nrhs, -cone, a( 1_ilp, k ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb &
                           )
                 call stdlib_zgeru( k-2, nrhs, -cone, a( 1_ilp, k-1 ), 1_ilp, b( k-1, 1_ilp ),ldb, b( 1_ilp, 1_ilp ), &
                           ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k-1, k )
                 akm1 = a( k-1, k-1 ) / akm1k
                 ak = a( k, k ) / conjg( akm1k )
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / conjg( akm1k )
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**h *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**h(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 if( k>1_ilp ) then
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp, k ), &
                              1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**h(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k>1_ilp ) then
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp, k ), &
                              1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k+1, 1_ilp ), ldb )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp, k+1 )&
                              , 1_ilp, cone, b( k+1, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k+1, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**h.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_zgeru( n-k, nrhs, -cone, a( k+1, k ), 1_ilp, b( k, 1_ilp ),ldb, b( &
                           k+1, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=dp) / real( a( k, k ),KIND=dp)
                 call stdlib_zdscal( nrhs, s, b( k, 1_ilp ), ldb )
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_zswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_zgeru( n-k-1, nrhs, -cone, a( k+2, k ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, &
                              1_ilp ), ldb )
                    call stdlib_zgeru( n-k-1, nrhs, -cone, a( k+2, k+1 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( &
                              k+2, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k+1, k )
                 akm1 = a( k, k ) / conjg( akm1k )
                 ak = a( k+1, k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / conjg( akm1k )
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**h *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**h(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n ) then
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp ), ldb, &
                              a( k+1, k ), 1_ilp, cone,b( k, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**h(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp ), ldb, &
                              a( k+1, k ), 1_ilp, cone,b( k, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k-1, 1_ilp ), ldb )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp ), ldb, &
                              a( k+1, k-1 ), 1_ilp, cone,b( k-1, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k-1, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_zhetrs




     pure module subroutine stdlib_chetri( uplo, n, a, lda, ipiv, work, info )
     !! CHETRI computes the inverse of a complex Hermitian indefinite matrix
     !! A using the factorization A = U*D*U**H or A = L*D*L**H computed by
     !! CHETRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kp, kstep
           real(sp) :: ak, akp1, d, t
           complex(sp) :: akkp1, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHETRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do info = 1, n
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / real( a( k, k ),KIND=sp)
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_ccopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_chemv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - real( stdlib_cdotc( k-1, work, 1_ilp, a( 1_ilp,k ), 1_ilp ),&
                              KIND=sp)
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k+1 ) )
                 ak = real( a( k, k ),KIND=sp) / t
                 akp1 = real( a( k+1, k+1 ),KIND=sp) / t
                 akkp1 = a( k, k+1 ) / t
                 d = t*( ak*akp1-one )
                 a( k, k ) = akp1 / d
                 a( k+1, k+1 ) = ak / d
                 a( k, k+1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_ccopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_chemv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - real( stdlib_cdotc( k-1, work, 1_ilp, a( 1_ilp,k ), 1_ilp ),&
                              KIND=sp)
                    a( k, k+1 ) = a( k, k+1 ) -stdlib_cdotc( k-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                    call stdlib_ccopy( k-1, a( 1_ilp, k+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_chemv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k+1 ), 1_ilp )
                              
                    a( k+1, k+1 ) = a( k+1, k+1 ) -real( stdlib_cdotc( k-1, work, 1_ilp, a( 1_ilp, k+1 ),&
                              1_ilp ),KIND=sp)
                 end if
                 kstep = 2_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 call stdlib_cswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                 do j = kp + 1, k - 1
                    temp = conjg( a( j, k ) )
                    a( j, k ) = conjg( a( kp, j ) )
                    a( kp, j ) = temp
                 end do
                 a( kp, k ) = conjg( a( kp, k ) )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp ) then
                    temp = a( k, k+1 )
                    a( k, k+1 ) = a( kp, k+1 )
                    a( kp, k+1 ) = temp
                 end if
              end if
              k = k + kstep
              go to 30
              50 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              60 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / real( a( k, k ),KIND=sp)
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_ccopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_chemv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work,1_ilp, czero, a( k+&
                              1_ilp, k ), 1_ilp )
                    a( k, k ) = a( k, k ) - real( stdlib_cdotc( n-k, work, 1_ilp,a( k+1, k ), 1_ilp ),&
                              KIND=sp)
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k-1 ) )
                 ak = real( a( k-1, k-1 ),KIND=sp) / t
                 akp1 = real( a( k, k ),KIND=sp) / t
                 akkp1 = a( k, k-1 ) / t
                 d = t*( ak*akp1-one )
                 a( k-1, k-1 ) = akp1 / d
                 a( k, k ) = ak / d
                 a( k, k-1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_ccopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_chemv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work,1_ilp, czero, a( k+&
                              1_ilp, k ), 1_ilp )
                    a( k, k ) = a( k, k ) - real( stdlib_cdotc( n-k, work, 1_ilp,a( k+1, k ), 1_ilp ),&
                              KIND=sp)
                    a( k, k-1 ) = a( k, k-1 ) -stdlib_cdotc( n-k, a( k+1, k ), 1_ilp, a( k+1, k-1 ),1_ilp &
                              )
                    call stdlib_ccopy( n-k, a( k+1, k-1 ), 1_ilp, work, 1_ilp )
                    call stdlib_chemv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work,1_ilp, czero, a( k+&
                              1_ilp, k-1 ), 1_ilp )
                    a( k-1, k-1 ) = a( k-1, k-1 ) -real( stdlib_cdotc( n-k, work, 1_ilp, a( k+1, k-1 )&
                              ,1_ilp ),KIND=sp)
                 end if
                 kstep = 2_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 if( kp<n )call stdlib_cswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                 do j = k + 1, kp - 1
                    temp = conjg( a( j, k ) )
                    a( j, k ) = conjg( a( kp, j ) )
                    a( kp, j ) = temp
                 end do
                 a( kp, k ) = conjg( a( kp, k ) )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp ) then
                    temp = a( k, k-1 )
                    a( k, k-1 ) = a( kp, k-1 )
                    a( kp, k-1 ) = temp
                 end if
              end if
              k = k - kstep
              go to 60
              80 continue
           end if
           return
     end subroutine stdlib_chetri

     pure module subroutine stdlib_zhetri( uplo, n, a, lda, ipiv, work, info )
     !! ZHETRI computes the inverse of a complex Hermitian indefinite matrix
     !! A using the factorization A = U*D*U**H or A = L*D*L**H computed by
     !! ZHETRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kp, kstep
           real(dp) :: ak, akp1, d, t
           complex(dp) :: akkp1, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHETRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do info = 1, n
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / real( a( k, k ),KIND=dp)
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_zcopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_zhemv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - real( stdlib_zdotc( k-1, work, 1_ilp, a( 1_ilp,k ), 1_ilp ),&
                              KIND=dp)
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k+1 ) )
                 ak = real( a( k, k ),KIND=dp) / t
                 akp1 = real( a( k+1, k+1 ),KIND=dp) / t
                 akkp1 = a( k, k+1 ) / t
                 d = t*( ak*akp1-one )
                 a( k, k ) = akp1 / d
                 a( k+1, k+1 ) = ak / d
                 a( k, k+1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_zcopy( k-1, a( 1_ilp, k ), 1_ilp, work, 1_ilp )
                    call stdlib_zhemv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k ), 1_ilp )
                              
                    a( k, k ) = a( k, k ) - real( stdlib_zdotc( k-1, work, 1_ilp, a( 1_ilp,k ), 1_ilp ),&
                              KIND=dp)
                    a( k, k+1 ) = a( k, k+1 ) -stdlib_zdotc( k-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, k+1 ), 1_ilp )
                              
                    call stdlib_zcopy( k-1, a( 1_ilp, k+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_zhemv( uplo, k-1, -cone, a, lda, work, 1_ilp, czero,a( 1_ilp, k+1 ), 1_ilp )
                              
                    a( k+1, k+1 ) = a( k+1, k+1 ) -real( stdlib_zdotc( k-1, work, 1_ilp, a( 1_ilp, k+1 ),&
                              1_ilp ),KIND=dp)
                 end if
                 kstep = 2_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 call stdlib_zswap( kp-1, a( 1_ilp, k ), 1_ilp, a( 1_ilp, kp ), 1_ilp )
                 do j = kp + 1, k - 1
                    temp = conjg( a( j, k ) )
                    a( j, k ) = conjg( a( kp, j ) )
                    a( kp, j ) = temp
                 end do
                 a( kp, k ) = conjg( a( kp, k ) )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp ) then
                    temp = a( k, k+1 )
                    a( k, k+1 ) = a( kp, k+1 )
                    a( kp, k+1 ) = temp
                 end if
              end if
              k = k + kstep
              go to 30
              50 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              60 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / real( a( k, k ),KIND=dp)
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_zcopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_zhemv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work,1_ilp, czero, a( k+&
                              1_ilp, k ), 1_ilp )
                    a( k, k ) = a( k, k ) - real( stdlib_zdotc( n-k, work, 1_ilp,a( k+1, k ), 1_ilp ),&
                              KIND=dp)
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k-1 ) )
                 ak = real( a( k-1, k-1 ),KIND=dp) / t
                 akp1 = real( a( k, k ),KIND=dp) / t
                 akkp1 = a( k, k-1 ) / t
                 d = t*( ak*akp1-one )
                 a( k-1, k-1 ) = akp1 / d
                 a( k, k ) = ak / d
                 a( k, k-1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_zcopy( n-k, a( k+1, k ), 1_ilp, work, 1_ilp )
                    call stdlib_zhemv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work,1_ilp, czero, a( k+&
                              1_ilp, k ), 1_ilp )
                    a( k, k ) = a( k, k ) - real( stdlib_zdotc( n-k, work, 1_ilp,a( k+1, k ), 1_ilp ),&
                              KIND=dp)
                    a( k, k-1 ) = a( k, k-1 ) -stdlib_zdotc( n-k, a( k+1, k ), 1_ilp, a( k+1, k-1 ),1_ilp &
                              )
                    call stdlib_zcopy( n-k, a( k+1, k-1 ), 1_ilp, work, 1_ilp )
                    call stdlib_zhemv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work,1_ilp, czero, a( k+&
                              1_ilp, k-1 ), 1_ilp )
                    a( k-1, k-1 ) = a( k-1, k-1 ) -real( stdlib_zdotc( n-k, work, 1_ilp, a( k+1, k-1 )&
                              ,1_ilp ),KIND=dp)
                 end if
                 kstep = 2_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 if( kp<n )call stdlib_zswap( n-kp, a( kp+1, k ), 1_ilp, a( kp+1, kp ), 1_ilp )
                 do j = k + 1, kp - 1
                    temp = conjg( a( j, k ) )
                    a( j, k ) = conjg( a( kp, j ) )
                    a( kp, j ) = temp
                 end do
                 a( kp, k ) = conjg( a( kp, k ) )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp ) then
                    temp = a( k, k-1 )
                    a( k, k-1 ) = a( kp, k-1 )
                    a( kp, k-1 ) = temp
                 end if
              end if
              k = k - kstep
              go to 60
              80 continue
           end if
           return
     end subroutine stdlib_zhetri




     pure module subroutine stdlib_cherfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb,x, ldx, ferr, &
     !! CHERFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is Hermitian indefinite, and
     !! provides error bounds and backward error estimates for the solution.
               berr, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHERFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_ccopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_chemv( uplo, n, -cone, a, lda, x( 1_ilp, j ), 1_ilp, cone, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    do i = 1, k - 1
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + abs( real( a( k, k ),KIND=sp) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + abs( real( a( k, k ),KIND=sp) )*xk
                    do i = k + 1, n
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_chetrs( uplo, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 call stdlib_caxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_clacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_clacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**h).
                    call stdlib_chetrs( uplo, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_chetrs( uplo, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_cherfs

     pure module subroutine stdlib_zherfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb,x, ldx, ferr, &
     !! ZHERFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is Hermitian indefinite, and
     !! provides error bounds and backward error estimates for the solution.
               berr, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: count, i, j, k, kase, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHERFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp .or. nrhs==0_ilp ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_zcopy( n, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_zhemv( uplo, n, -cone, a, lda, x( 1_ilp, j ), 1_ilp, cone, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    do i = 1, k - 1
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + abs( real( a( k, k ),KIND=dp) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + abs( real( a( k, k ),KIND=dp) )*xk
                    do i = k + 1, n
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_zhetrs( uplo, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 call stdlib_zaxpy( n, cone, work, 1_ilp, x( 1_ilp, j ), 1_ilp )
                 lstres = berr( j )
                 count = count + 1_ilp
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_zlacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              100 continue
              call stdlib_zlacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(a**h).
                    call stdlib_zhetrs( uplo, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_zhetrs( uplo, n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_zherfs




     pure module subroutine stdlib_cheequb( uplo, n, a, lda, s, scond, amax, work, info )
     !! CHEEQUB computes row and column scalings intended to equilibrate a
     !! Hermitian matrix A (with respect to the Euclidean norm) and reduce
     !! its condition number. The scale factors S are computed by the BIN
     !! algorithm (see references) so that the scaled matrix B with elements
     !! B(i,j) = S(i)*A(i,j)*S(j) has a condition number within a factor N of
     !! the smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: amax, scond
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(out) :: s(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: max_iter = 100_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, j, iter
           real(sp) :: avg, std, tol, c0, c1, c2, t, u, si, d, base, smin, smax, smlnum, bignum, &
                     scale, sumsq
           logical(lk) :: up
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if ( .not. ( stdlib_lsame( uplo, 'U' ) .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -1_ilp
           else if ( n < 0_ilp ) then
              info = -2_ilp
           else if ( lda < max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if ( info /= 0_ilp ) then
              call stdlib_xerbla( 'CHEEQUB', -info )
              return
           end if
           up = stdlib_lsame( uplo, 'U' )
           amax = zero
           ! quick return if possible.
           if ( n == 0_ilp ) then
              scond = one
              return
           end if
           do i = 1, n
              s( i ) = zero
           end do
           amax = zero
           if ( up ) then
              do j = 1, n
                 do i = 1, j-1
                    s( i ) = max( s( i ), cabs1( a( i, j ) ) )
                    s( j ) = max( s( j ), cabs1( a( i, j ) ) )
                    amax = max( amax, cabs1( a( i, j ) ) )
                 end do
                 s( j ) = max( s( j ), cabs1( a( j, j ) ) )
                 amax = max( amax, cabs1( a( j, j ) ) )
              end do
           else
              do j = 1, n
                 s( j ) = max( s( j ), cabs1( a( j, j ) ) )
                 amax = max( amax, cabs1( a( j, j ) ) )
                 do i = j+1, n
                    s( i ) = max( s( i ), cabs1( a( i, j ) ) )
                    s( j ) = max( s( j ), cabs1( a( i, j ) ) )
                    amax = max( amax, cabs1( a( i, j ) ) )
                 end do
              end do
           end if
           do j = 1, n
              s( j ) = one / s( j )
           end do
           tol = one / sqrt( two * n )
           do iter = 1, max_iter
              scale = zero
              sumsq = zero
              ! beta = |a|s
              do i = 1, n
                 work( i ) = zero
              end do
              if ( up ) then
                 do j = 1, n
                    do i = 1, j-1
                       work( i ) = work( i ) + cabs1( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + cabs1( a( i, j ) ) * s( i )
                    end do
                    work( j ) = work( j ) + cabs1( a( j, j ) ) * s( j )
                 end do
              else
                 do j = 1, n
                    work( j ) = work( j ) + cabs1( a( j, j ) ) * s( j )
                    do i = j+1, n
                       work( i ) = work( i ) + cabs1( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + cabs1( a( i, j ) ) * s( i )
                    end do
                 end do
              end if
              ! avg = s^t beta / n
              avg = zero
              do i = 1, n
                 avg = avg + real( s( i )*work( i ),KIND=sp)
              end do
              avg = avg / n
              std = zero
              do i = n+1, 2*n
                 work( i ) = s( i-n ) * work( i-n ) - avg
              end do
              call stdlib_classq( n, work( n+1 ), 1_ilp, scale, sumsq )
              std = scale * sqrt( sumsq / n )
              if ( std < tol * avg ) goto 999
              do i = 1, n
                 t = cabs1( a( i, i ) )
                 si = s( i )
                 c2 = ( n-1 ) * t
                 c1 = real( ( n-2 ) * ( work( i ) - t*si ),KIND=sp)
                 c0 = real( -(t*si)*si + 2_ilp*work( i )*si - n*avg,KIND=sp)
                 d = c1*c1 - 4_ilp*c0*c2
                 if ( d <= 0_ilp ) then
                    info = -1_ilp
                    return
                 end if
                 si = -2_ilp*c0 / ( c1 + sqrt( d ) )
                 d = si - s( i )
                 u = zero
                 if ( up ) then
                    do j = 1, i
                       t = cabs1( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = cabs1( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 else
                    do j = 1, i
                       t = cabs1( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = cabs1( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 end if
                 avg = avg + real( ( u + work( i ) ) * d / n,KIND=sp)
                 s( i ) = si
              end do
           end do
           999 continue
           smlnum = stdlib_slamch( 'SAFEMIN' )
           bignum = one / smlnum
           smin = bignum
           smax = zero
           t = one / sqrt( avg )
           base = stdlib_slamch( 'B' )
           u = one / log( base )
           do i = 1, n
              s( i ) = base ** int( u * log( s( i ) * t ),KIND=ilp)
              smin = min( smin, s( i ) )
              smax = max( smax, s( i ) )
           end do
           scond = max( smin, smlnum ) / min( smax, bignum )
     end subroutine stdlib_cheequb

     pure module subroutine stdlib_zheequb( uplo, n, a, lda, s, scond, amax, work, info )
     !! ZHEEQUB computes row and column scalings intended to equilibrate a
     !! Hermitian matrix A (with respect to the Euclidean norm) and reduce
     !! its condition number. The scale factors S are computed by the BIN
     !! algorithm (see references) so that the scaled matrix B with elements
     !! B(i,j) = S(i)*A(i,j)*S(j) has a condition number within a factor N of
     !! the smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: amax, scond
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(out) :: s(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: max_iter = 100_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, j, iter
           real(dp) :: avg, std, tol, c0, c1, c2, t, u, si, d, base, smin, smax, smlnum, bignum, &
                     scale, sumsq
           logical(lk) :: up
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if ( .not. ( stdlib_lsame( uplo, 'U' ) .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -1_ilp
           else if ( n < 0_ilp ) then
              info = -2_ilp
           else if ( lda < max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if ( info /= 0_ilp ) then
              call stdlib_xerbla( 'ZHEEQUB', -info )
              return
           end if
           up = stdlib_lsame( uplo, 'U' )
           amax = zero
           ! quick return if possible.
           if ( n == 0_ilp ) then
              scond = one
              return
           end if
           do i = 1, n
              s( i ) = zero
           end do
           amax = zero
           if ( up ) then
              do j = 1, n
                 do i = 1, j-1
                    s( i ) = max( s( i ), cabs1( a( i, j ) ) )
                    s( j ) = max( s( j ), cabs1( a( i, j ) ) )
                    amax = max( amax, cabs1( a( i, j ) ) )
                 end do
                 s( j ) = max( s( j ), cabs1( a( j, j ) ) )
                 amax = max( amax, cabs1( a( j, j ) ) )
              end do
           else
              do j = 1, n
                 s( j ) = max( s( j ), cabs1( a( j, j ) ) )
                 amax = max( amax, cabs1( a( j, j ) ) )
                 do i = j+1, n
                    s( i ) = max( s( i ), cabs1( a( i, j ) ) )
                    s( j ) = max( s( j ), cabs1( a( i, j ) ) )
                    amax = max( amax, cabs1( a( i, j ) ) )
                 end do
              end do
           end if
           do j = 1, n
              s( j ) = one / s( j )
           end do
           tol = one / sqrt( two * n )
           do iter = 1, max_iter
              scale = zero
              sumsq = zero
              ! beta = |a|s
              do i = 1, n
                 work( i ) = zero
              end do
              if ( up ) then
                 do j = 1, n
                    do i = 1, j-1
                       work( i ) = work( i ) + cabs1( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + cabs1( a( i, j ) ) * s( i )
                    end do
                    work( j ) = work( j ) + cabs1( a( j, j ) ) * s( j )
                 end do
              else
                 do j = 1, n
                    work( j ) = work( j ) + cabs1( a( j, j ) ) * s( j )
                    do i = j+1, n
                       work( i ) = work( i ) + cabs1( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + cabs1( a( i, j ) ) * s( i )
                    end do
                 end do
              end if
              ! avg = s^t beta / n
              avg = zero
              do i = 1, n
                 avg = avg + real( s( i )*work( i ),KIND=dp)
              end do
              avg = avg / n
              std = zero
              do i = n+1, 2*n
                 work( i ) = s( i-n ) * work( i-n ) - avg
              end do
              call stdlib_zlassq( n, work( n+1 ), 1_ilp, scale, sumsq )
              std = scale * sqrt( sumsq / n )
              if ( std < tol * avg ) goto 999
              do i = 1, n
                 t = cabs1( a( i, i ) )
                 si = s( i )
                 c2 = ( n-1 ) * t
                 c1 = ( n-2 ) * ( real( work( i ),KIND=dp) - t*si )
                 c0 = -(t*si)*si + 2_ilp * real( work( i ),KIND=dp) * si - n*avg
                 d = c1*c1 - 4_ilp*c0*c2
                 if ( d <= 0_ilp ) then
                    info = -1_ilp
                    return
                 end if
                 si = -2_ilp*c0 / ( c1 + sqrt( d ) )
                 d = si - s( i )
                 u = zero
                 if ( up ) then
                    do j = 1, i
                       t = cabs1( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = cabs1( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 else
                    do j = 1, i
                       t = cabs1( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = cabs1( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 end if
                 avg = avg + ( u + real( work( i ),KIND=dp) ) * d / n
                 s( i ) = si
              end do
           end do
           999 continue
           smlnum = stdlib_dlamch( 'SAFEMIN' )
           bignum = one / smlnum
           smin = bignum
           smax = zero
           t = one / sqrt( avg )
           base = stdlib_dlamch( 'B' )
           u = one / log( base )
           do i = 1, n
              s( i ) = base ** int( u * log( s( i ) * t ),KIND=ilp)
              smin = min( smin, s( i ) )
              smax = max( smax, s( i ) )
           end do
           scond = max( smin, smlnum ) / min( smax, bignum )
     end subroutine stdlib_zheequb




     pure module subroutine stdlib_chetrs2( uplo, n, nrhs, a, lda, ipiv, b, ldb,work, info )
     !! CHETRS2 solves a system of linear equations A*X = B with a complex
     !! Hermitian matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by CHETRF and converted by CSYCONV.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, iinfo, j, k, kp
           real(sp) :: s
           complex(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHETRS2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! convert a
           call stdlib_csyconv( uplo, 'C', n, a, lda, ipiv, work, iinfo )
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**h.
             ! p**t * b
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp==-ipiv( k-1 ) )call stdlib_cswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb &
                           )
                 k=k-2
              end if
             end do
        ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
             call stdlib_ctrsm('L','U','N','U',n,nrhs,cone,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i=n
              do while ( i >= 1 )
                 if( ipiv(i) > 0_ilp ) then
                   s = real( cone,KIND=sp) / real( a( i, i ),KIND=sp)
                   call stdlib_csscal( nrhs, s, b( i, 1_ilp ), ldb )
                 elseif ( i > 1_ilp) then
                    if ( ipiv(i-1) == ipiv(i) ) then
                       akm1k = work(i)
                       akm1 = a( i-1, i-1 ) / akm1k
                       ak = a( i, i ) / conjg( akm1k )
                       denom = akm1*ak - cone
                       do j = 1, nrhs
                          bkm1 = b( i-1, j ) / akm1k
                          bk = b( i, j ) / conjg( akm1k )
                          b( i-1, j ) = ( ak*bkm1-bk ) / denom
                          b( i, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                    i = i - 1_ilp
                    endif
                 endif
                 i = i - 1_ilp
              end do
            ! compute (u**h \ b) -> b   [ u**h \ (d \ (u \p**t * b) ) ]
              call stdlib_ctrsm('L','U','C','U',n,nrhs,cone,a,lda,b,ldb)
             ! p * b  [ p * (u**h \ (d \ (u \p**t * b) )) ]
             k=1_ilp
             do while ( k <= n )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k < n .and. kp==-ipiv( k+1 ) )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp,&
                            1_ilp ), ldb )
                 k=k+2
              endif
             end do
           else
              ! solve a*x = b, where a = l*d*l**h.
             ! p**t * b
             k=1_ilp
             do while ( k <= n )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k+1).
                 kp = -ipiv( k+1 )
                 if( kp==-ipiv( k ) )call stdlib_cswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                           
                 k=k+2
              endif
             end do
        ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
             call stdlib_ctrsm('L','L','N','U',n,nrhs,cone,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i=1_ilp
              do while ( i <= n )
                 if( ipiv(i) > 0_ilp ) then
                   s = real( cone,KIND=sp) / real( a( i, i ),KIND=sp)
                   call stdlib_csscal( nrhs, s, b( i, 1_ilp ), ldb )
                 else
                       akm1k = work(i)
                       akm1 = a( i, i ) / conjg( akm1k )
                       ak = a( i+1, i+1 ) / akm1k
                       denom = akm1*ak - cone
                       do j = 1, nrhs
                          bkm1 = b( i, j ) / conjg( akm1k )
                          bk = b( i+1, j ) / akm1k
                          b( i, j ) = ( ak*bkm1-bk ) / denom
                          b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                       i = i + 1_ilp
                 endif
                 i = i + 1_ilp
              end do
        ! compute (l**h \ b) -> b   [ l**h \ (d \ (l \p**t * b) ) ]
             call stdlib_ctrsm('L','L','C','U',n,nrhs,cone,a,lda,b,ldb)
             ! p * b  [ p * (l**h \ (d \ (l \p**t * b) )) ]
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k>1_ilp .and. kp==-ipiv( k-1 ) )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, &
                           1_ilp ), ldb )
                 k=k-2
              endif
             end do
           end if
           ! revert a
           call stdlib_csyconv( uplo, 'R', n, a, lda, ipiv, work, iinfo )
           return
     end subroutine stdlib_chetrs2

     pure module subroutine stdlib_zhetrs2( uplo, n, nrhs, a, lda, ipiv, b, ldb,work, info )
     !! ZHETRS2 solves a system of linear equations A*X = B with a complex
     !! Hermitian matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by ZHETRF and converted by ZSYCONV.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, iinfo, j, k, kp
           real(dp) :: s
           complex(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHETRS2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! convert a
           call stdlib_zsyconv( uplo, 'C', n, a, lda, ipiv, work, iinfo )
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**h.
             ! p**t * b
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp==-ipiv( k-1 ) )call stdlib_zswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb &
                           )
                 k=k-2
              end if
             end do
        ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
             call stdlib_ztrsm('L','U','N','U',n,nrhs,cone,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i=n
              do while ( i >= 1 )
                 if( ipiv(i) > 0_ilp ) then
                   s = real( cone,KIND=dp) / real( a( i, i ),KIND=dp)
                   call stdlib_zdscal( nrhs, s, b( i, 1_ilp ), ldb )
                 elseif ( i > 1_ilp) then
                    if ( ipiv(i-1) == ipiv(i) ) then
                       akm1k = work(i)
                       akm1 = a( i-1, i-1 ) / akm1k
                       ak = a( i, i ) / conjg( akm1k )
                       denom = akm1*ak - cone
                       do j = 1, nrhs
                          bkm1 = b( i-1, j ) / akm1k
                          bk = b( i, j ) / conjg( akm1k )
                          b( i-1, j ) = ( ak*bkm1-bk ) / denom
                          b( i, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                    i = i - 1_ilp
                    endif
                 endif
                 i = i - 1_ilp
              end do
            ! compute (u**h \ b) -> b   [ u**h \ (d \ (u \p**t * b) ) ]
              call stdlib_ztrsm('L','U','C','U',n,nrhs,cone,a,lda,b,ldb)
             ! p * b  [ p * (u**h \ (d \ (u \p**t * b) )) ]
             k=1_ilp
             do while ( k <= n )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k < n .and. kp==-ipiv( k+1 ) )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp,&
                            1_ilp ), ldb )
                 k=k+2
              endif
             end do
           else
              ! solve a*x = b, where a = l*d*l**h.
             ! p**t * b
             k=1_ilp
             do while ( k <= n )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k+1).
                 kp = -ipiv( k+1 )
                 if( kp==-ipiv( k ) )call stdlib_zswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                           
                 k=k+2
              endif
             end do
        ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
             call stdlib_ztrsm('L','L','N','U',n,nrhs,cone,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i=1_ilp
              do while ( i <= n )
                 if( ipiv(i) > 0_ilp ) then
                   s = real( cone,KIND=dp) / real( a( i, i ),KIND=dp)
                   call stdlib_zdscal( nrhs, s, b( i, 1_ilp ), ldb )
                 else
                       akm1k = work(i)
                       akm1 = a( i, i ) / conjg( akm1k )
                       ak = a( i+1, i+1 ) / akm1k
                       denom = akm1*ak - cone
                       do j = 1, nrhs
                          bkm1 = b( i, j ) / conjg( akm1k )
                          bk = b( i+1, j ) / akm1k
                          b( i, j ) = ( ak*bkm1-bk ) / denom
                          b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                       i = i + 1_ilp
                 endif
                 i = i + 1_ilp
              end do
        ! compute (l**h \ b) -> b   [ l**h \ (d \ (l \p**t * b) ) ]
             call stdlib_ztrsm('L','L','C','U',n,nrhs,cone,a,lda,b,ldb)
             ! p * b  [ p * (l**h \ (d \ (l \p**t * b) )) ]
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k>1_ilp .and. kp==-ipiv( k-1 ) )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, &
                           1_ilp ), ldb )
                 k=k-2
              endif
             end do
           end if
           ! revert a
           call stdlib_zsyconv( uplo, 'R', n, a, lda, ipiv, work, iinfo )
           return
     end subroutine stdlib_zhetrs2




     pure module subroutine stdlib_chetrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb,info )
     !! CHETRS_3 solves a system of linear equations A * X = B with a complex
     !! Hermitian matrix A using the factorization computed
     !! by CHETRF_RK or CHETRF_BK:
     !! A = P*U*D*(U**H)*(P**T) or A = P*L*D*(L**H)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**H (or L**H) is the conjugate of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is Hermitian and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This algorithm is using Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), e(*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j, k, kp
           real(sp) :: s
           complex(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHETRS_3', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! begin upper
              ! solve a*x = b, where a = u*d*u**h.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
              call stdlib_ctrsm( 'L', 'U', 'N', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i = n
              do while ( i>=1 )
                 if( ipiv( i )>0_ilp ) then
                    s = real( cone,KIND=sp) / real( a( i, i ),KIND=sp)
                    call stdlib_csscal( nrhs, s, b( i, 1_ilp ), ldb )
                 else if ( i>1_ilp ) then
                    akm1k = e( i )
                    akm1 = a( i-1, i-1 ) / akm1k
                    ak = a( i, i ) / conjg( akm1k )
                    denom = akm1*ak - cone
                    do j = 1, nrhs
                       bkm1 = b( i-1, j ) / akm1k
                       bk = b( i, j ) / conjg( akm1k )
                       b( i-1, j ) = ( ak*bkm1-bk ) / denom
                       b( i, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i - 1_ilp
                 end if
                 i = i - 1_ilp
              end do
              ! compute (u**h \ b) -> b   [ u**h \ (d \ (u \p**t * b) ) ]
              call stdlib_ctrsm( 'L', 'U', 'C', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! p * b  [ p * (u**h \ (d \ (u \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
           else
              ! begin lower
              ! solve a*x = b, where a = l*d*l**h.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
              call stdlib_ctrsm( 'L', 'L', 'N', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i = 1_ilp
              do while ( i<=n )
                 if( ipiv( i )>0_ilp ) then
                    s = real( cone,KIND=sp) / real( a( i, i ),KIND=sp)
                    call stdlib_csscal( nrhs, s, b( i, 1_ilp ), ldb )
                 else if( i<n ) then
                    akm1k = e( i )
                    akm1 = a( i, i ) / conjg( akm1k )
                    ak = a( i+1, i+1 ) / akm1k
                    denom = akm1*ak - cone
                    do  j = 1, nrhs
                       bkm1 = b( i, j ) / conjg( akm1k )
                       bk = b( i+1, j ) / akm1k
                       b( i, j ) = ( ak*bkm1-bk ) / denom
                       b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i + 1_ilp
                 end if
                 i = i + 1_ilp
              end do
              ! compute (l**h \ b) -> b   [ l**h \ (d \ (l \p**t * b) ) ]
              call stdlib_ctrsm('L', 'L', 'C', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! p * b  [ p * (l**h \ (d \ (l \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! end lower
           end if
           return
     end subroutine stdlib_chetrs_3

     pure module subroutine stdlib_zhetrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb,info )
     !! ZHETRS_3 solves a system of linear equations A * X = B with a complex
     !! Hermitian matrix A using the factorization computed
     !! by ZHETRF_RK or ZHETRF_BK:
     !! A = P*U*D*(U**H)*(P**T) or A = P*L*D*(L**H)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**H (or L**H) is the conjugate of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is Hermitian and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This algorithm is using Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), e(*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j, k, kp
           real(dp) :: s
           complex(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHETRS_3', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! begin upper
              ! solve a*x = b, where a = u*d*u**h.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
              call stdlib_ztrsm( 'L', 'U', 'N', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i = n
              do while ( i>=1 )
                 if( ipiv( i )>0_ilp ) then
                    s = real( cone,KIND=dp) / real( a( i, i ),KIND=dp)
                    call stdlib_zdscal( nrhs, s, b( i, 1_ilp ), ldb )
                 else if ( i>1_ilp ) then
                    akm1k = e( i )
                    akm1 = a( i-1, i-1 ) / akm1k
                    ak = a( i, i ) / conjg( akm1k )
                    denom = akm1*ak - cone
                    do j = 1, nrhs
                       bkm1 = b( i-1, j ) / akm1k
                       bk = b( i, j ) / conjg( akm1k )
                       b( i-1, j ) = ( ak*bkm1-bk ) / denom
                       b( i, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i - 1_ilp
                 end if
                 i = i - 1_ilp
              end do
              ! compute (u**h \ b) -> b   [ u**h \ (d \ (u \p**t * b) ) ]
              call stdlib_ztrsm( 'L', 'U', 'C', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! p * b  [ p * (u**h \ (d \ (u \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
           else
              ! begin lower
              ! solve a*x = b, where a = l*d*l**h.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
              call stdlib_ztrsm( 'L', 'L', 'N', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i = 1_ilp
              do while ( i<=n )
                 if( ipiv( i )>0_ilp ) then
                    s = real( cone,KIND=dp) / real( a( i, i ),KIND=dp)
                    call stdlib_zdscal( nrhs, s, b( i, 1_ilp ), ldb )
                 else if( i<n ) then
                    akm1k = e( i )
                    akm1 = a( i, i ) / conjg( akm1k )
                    ak = a( i+1, i+1 ) / akm1k
                    denom = akm1*ak - cone
                    do  j = 1, nrhs
                       bkm1 = b( i, j ) / conjg( akm1k )
                       bk = b( i+1, j ) / akm1k
                       b( i, j ) = ( ak*bkm1-bk ) / denom
                       b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i + 1_ilp
                 end if
                 i = i + 1_ilp
              end do
              ! compute (l**h \ b) -> b   [ l**h \ (d \ (l \p**t * b) ) ]
              call stdlib_ztrsm('L', 'L', 'C', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! p * b  [ p * (l**h \ (d \ (l \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 end if
              end do
              ! end lower
           end if
           return
     end subroutine stdlib_zhetrs_3




     pure module subroutine stdlib_cheswapr( uplo, n, a, lda, i1, i2)
     !! CHESWAPR applies an elementary permutation on the rows and the columns of
     !! a hermitian matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: i1, i2, lda, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,n)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           complex(sp) :: tmp
           ! Executable Statements 
           upper = stdlib_lsame( uplo, 'U' )
           if (upper) then
               ! upper
               ! first swap
                ! - swap column i1 and i2 from i1 to i1-1
              call stdlib_cswap( i1-1, a(1_ilp,i1), 1_ilp, a(1_ilp,i2), 1_ilp )
                ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap row i1 from i1+1 to i2-1 with col i2 from i1+1 to i2-1
                ! - swap a(i2,i1) and a(i1,i2)
              tmp=a(i1,i1)
              a(i1,i1)=a(i2,i2)
              a(i2,i2)=tmp
              do i=1,i2-i1-1
                 tmp=a(i1,i1+i)
                 a(i1,i1+i)=conjg(a(i1+i,i2))
                 a(i1+i,i2)=conjg(tmp)
              end do
               a(i1,i2)=conjg(a(i1,i2))
                ! third swap
                ! - swap row i1 and i2 from i2+1 to n
              do i=i2+1,n
                 tmp=a(i1,i)
                 a(i1,i)=a(i2,i)
                 a(i2,i)=tmp
              end do
             else
               ! lower
               ! first swap
                ! - swap row i1 and i2 from 1 to i1-1
              call stdlib_cswap ( i1-1, a(i1,1_ilp), lda, a(i2,1_ilp), lda )
               ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap col i1 from i1+1 to i2-1 with row i2 from i1+1 to i2-1
                ! - swap a(i2,i1) and a(i1,i2)
               tmp=a(i1,i1)
               a(i1,i1)=a(i2,i2)
               a(i2,i2)=tmp
               do i=1,i2-i1-1
                  tmp=a(i1+i,i1)
                  a(i1+i,i1)=conjg(a(i2,i1+i))
                  a(i2,i1+i)=conjg(tmp)
               end do
               a(i2,i1)=conjg(a(i2,i1))
               ! third swap
                ! - swap col i1 and i2 from i2+1 to n
               do i=i2+1,n
                  tmp=a(i,i1)
                  a(i,i1)=a(i,i2)
                  a(i,i2)=tmp
               end do
           endif
     end subroutine stdlib_cheswapr

     pure module subroutine stdlib_zheswapr( uplo, n, a, lda, i1, i2)
     !! ZHESWAPR applies an elementary permutation on the rows and the columns of
     !! a hermitian matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: i1, i2, lda, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,n)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           complex(dp) :: tmp
           ! Executable Statements 
           upper = stdlib_lsame( uplo, 'U' )
           if (upper) then
               ! upper
               ! first swap
                ! - swap column i1 and i2 from i1 to i1-1
              call stdlib_zswap( i1-1, a(1_ilp,i1), 1_ilp, a(1_ilp,i2), 1_ilp )
                ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap row i1 from i1+1 to i2-1 with col i2 from i1+1 to i2-1
                ! - swap a(i2,i1) and a(i1,i2)
              tmp=a(i1,i1)
              a(i1,i1)=a(i2,i2)
              a(i2,i2)=tmp
              do i=1,i2-i1-1
                 tmp=a(i1,i1+i)
                 a(i1,i1+i)=conjg(a(i1+i,i2))
                 a(i1+i,i2)=conjg(tmp)
              end do
               a(i1,i2)=conjg(a(i1,i2))
                ! third swap
                ! - swap row i1 and i2 from i2+1 to n
              do i=i2+1,n
                 tmp=a(i1,i)
                 a(i1,i)=a(i2,i)
                 a(i2,i)=tmp
              end do
             else
               ! lower
               ! first swap
                ! - swap row i1 and i2 from 1 to i1-1
              call stdlib_zswap ( i1-1, a(i1,1_ilp), lda, a(i2,1_ilp), lda )
               ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap col i1 from i1+1 to i2-1 with row i2 from i1+1 to i2-1
                ! - swap a(i2,i1) and a(i1,i2)
               tmp=a(i1,i1)
               a(i1,i1)=a(i2,i2)
               a(i2,i2)=tmp
               do i=1,i2-i1-1
                  tmp=a(i1+i,i1)
                  a(i1+i,i1)=conjg(a(i2,i1+i))
                  a(i2,i1+i)=conjg(tmp)
               end do
               a(i2,i1)=conjg(a(i2,i1))
               ! third swap
                ! - swap col i1 and i2 from i2+1 to n
               do i=i2+1,n
                  tmp=a(i,i1)
                  a(i,i1)=a(i,i2)
                  a(i,i2)=tmp
               end do
           endif
     end subroutine stdlib_zheswapr




     pure module subroutine stdlib_chpcon( uplo, n, ap, ipiv, anorm, rcond, work, info )
     !! CHPCON estimates the reciprocal of the condition number of a complex
     !! Hermitian packed matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by CHPTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, ip, kase
           real(sp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHPCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              ip = n*( n+1 ) / 2_ilp
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip - i
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              ip = 1_ilp
              do i = 1, n
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip + n - i + 1_ilp
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**h) or inv(u*d*u**h).
              call stdlib_chptrs( uplo, n, 1_ilp, ap, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_chpcon

     pure module subroutine stdlib_zhpcon( uplo, n, ap, ipiv, anorm, rcond, work, info )
     !! ZHPCON estimates the reciprocal of the condition number of a complex
     !! Hermitian packed matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by ZHPTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, ip, kase
           real(dp) :: ainvnm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( anorm<zero ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHPCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              ip = n*( n+1 ) / 2_ilp
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip - i
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              ip = 1_ilp
              do i = 1, n
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip + n - i + 1_ilp
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp
           30 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              ! multiply by inv(l*d*l**h) or inv(u*d*u**h).
              call stdlib_zhptrs( uplo, n, 1_ilp, ap, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_zhpcon




     pure module subroutine stdlib_chptrf( uplo, n, ap, ipiv, info )
     !! CHPTRF computes the factorization of a complex Hermitian packed
     !! matrix A using the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**H  or  A = L*D*L**H
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is Hermitian and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: ap(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, imax, j, jmax, k, kc, kk, knc, kp, kpc, kstep, kx, npp
           real(sp) :: absakk, alpha, colmax, d, d11, d22, r1, rowmax, tt
           complex(sp) :: d12, d21, t, wk, wkm1, wkp1, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHPTRF', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**h using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              kc = ( n-1 )*n / 2_ilp + 1_ilp
              10 continue
              knc = kc
              ! if k < 1, exit from loop
              if( k<1 )go to 110
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( ap( kc+k-1 ),KIND=sp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k>1_ilp ) then
                 imax = stdlib_icamax( k-1, ap( kc ), 1_ilp )
                 colmax = cabs1( ap( kc+imax-1 ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 ap( kc+k-1 ) = real( ap( kc+k-1 ),KIND=sp)
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    rowmax = zero
                    jmax = imax
                    kx = imax*( imax+1 ) / 2_ilp + imax
                    do j = imax + 1, k
                       if( cabs1( ap( kx ) )>rowmax ) then
                          rowmax = cabs1( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + j
                    end do
                    kpc = ( imax-1 )*imax / 2_ilp + 1_ilp
                    if( imax>1_ilp ) then
                       jmax = stdlib_icamax( imax-1, ap( kpc ), 1_ilp )
                       rowmax = max( rowmax, cabs1( ap( kpc+jmax-1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( ap( kpc+imax-1 ),KIND=sp) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k - kstep + 1_ilp
                 if( kstep==2_ilp )knc = knc - k + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_cswap( kp-1, ap( knc ), 1_ilp, ap( kpc ), 1_ilp )
                    kx = kpc + kp - 1_ilp
                    do j = kp + 1, kk - 1
                       kx = kx + j - 1_ilp
                       t = conjg( ap( knc+j-1 ) )
                       ap( knc+j-1 ) = conjg( ap( kx ) )
                       ap( kx ) = t
                    end do
                    ap( kx+kk-1 ) = conjg( ap( kx+kk-1 ) )
                    r1 = real( ap( knc+kk-1 ),KIND=sp)
                    ap( knc+kk-1 ) = real( ap( kpc+kp-1 ),KIND=sp)
                    ap( kpc+kp-1 ) = r1
                    if( kstep==2_ilp ) then
                       ap( kc+k-1 ) = real( ap( kc+k-1 ),KIND=sp)
                       t = ap( kc+k-2 )
                       ap( kc+k-2 ) = ap( kc+kp-1 )
                       ap( kc+kp-1 ) = t
                    end if
                 else
                    ap( kc+k-1 ) = real( ap( kc+k-1 ),KIND=sp)
                    if( kstep==2_ilp )ap( kc-1 ) = real( ap( kc-1 ),KIND=sp)
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**h = a - w(k)*1/d(k)*w(k)**h
                    r1 = one / real( ap( kc+k-1 ),KIND=sp)
                    call stdlib_chpr( uplo, k-1, -r1, ap( kc ), 1_ilp, ap )
                    ! store u(k) in column k
                    call stdlib_csscal( k-1, r1, ap( kc ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**h
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**h
                    if( k>2_ilp ) then
                       d = stdlib_slapy2( real( ap( k-1+( k-1 )*k / 2_ilp ),KIND=sp),aimag( ap( k-1+( &
                                 k-1 )*k / 2_ilp ) ) )
                       d22 = real( ap( k-1+( k-2 )*( k-1 ) / 2_ilp ),KIND=sp) / d
                       d11 = real( ap( k+( k-1 )*k / 2_ilp ),KIND=sp) / d
                       tt = one / ( d11*d22-one )
                       d12 = ap( k-1+( k-1 )*k / 2_ilp ) / d
                       d = tt / d
                       do j = k - 2, 1, -1
                          wkm1 = d*( d11*ap( j+( k-2 )*( k-1 ) / 2_ilp )-conjg( d12 )*ap( j+( k-1 )*k &
                                    / 2_ilp ) )
                          wk = d*( d22*ap( j+( k-1 )*k / 2_ilp )-d12*ap( j+( k-2 )*( k-1 ) / 2_ilp ) )
                                    
                          do i = j, 1, -1
                             ap( i+( j-1 )*j / 2_ilp ) = ap( i+( j-1 )*j / 2_ilp ) -ap( i+( k-1 )*k / 2_ilp )&
                                       *conjg( wk ) -ap( i+( k-2 )*( k-1 ) / 2_ilp )*conjg( wkm1 )
                          end do
                          ap( j+( k-1 )*k / 2_ilp ) = wk
                          ap( j+( k-2 )*( k-1 ) / 2_ilp ) = wkm1
                          ap( j+( j-1 )*j / 2_ilp ) = cmplx( real( ap( j+( j-1 )*j / 2_ilp ),KIND=sp), &
                                    zero,KIND=sp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              kc = knc - k
              go to 10
           else
              ! factorize a as l*d*l**h using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              kc = 1_ilp
              npp = n*( n+1 ) / 2_ilp
              60 continue
              knc = kc
              ! if k > n, exit from loop
              if( k>n )go to 110
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( ap( kc ),KIND=sp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k<n ) then
                 imax = k + stdlib_icamax( n-k, ap( kc+1 ), 1_ilp )
                 colmax = cabs1( ap( kc+imax-k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 ap( kc ) = real( ap( kc ),KIND=sp)
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    rowmax = zero
                    kx = kc + imax - k
                    do j = k, imax - 1
                       if( cabs1( ap( kx ) )>rowmax ) then
                          rowmax = cabs1( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + n - j
                    end do
                    kpc = npp - ( n-imax+1 )*( n-imax+2 ) / 2_ilp + 1_ilp
                    if( imax<n ) then
                       jmax = imax + stdlib_icamax( n-imax, ap( kpc+1 ), 1_ilp )
                       rowmax = max( rowmax, cabs1( ap( kpc+jmax-imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( ap( kpc ),KIND=sp) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k + kstep - 1_ilp
                 if( kstep==2_ilp )knc = knc + n - k + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_cswap( n-kp, ap( knc+kp-kk+1 ), 1_ilp, ap( kpc+1 ),1_ilp )
                              
                    kx = knc + kp - kk
                    do j = kk + 1, kp - 1
                       kx = kx + n - j + 1_ilp
                       t = conjg( ap( knc+j-kk ) )
                       ap( knc+j-kk ) = conjg( ap( kx ) )
                       ap( kx ) = t
                    end do
                    ap( knc+kp-kk ) = conjg( ap( knc+kp-kk ) )
                    r1 = real( ap( knc ),KIND=sp)
                    ap( knc ) = real( ap( kpc ),KIND=sp)
                    ap( kpc ) = r1
                    if( kstep==2_ilp ) then
                       ap( kc ) = real( ap( kc ),KIND=sp)
                       t = ap( kc+1 )
                       ap( kc+1 ) = ap( kc+kp-k )
                       ap( kc+kp-k ) = t
                    end if
                 else
                    ap( kc ) = real( ap( kc ),KIND=sp)
                    if( kstep==2_ilp )ap( knc ) = real( ap( knc ),KIND=sp)
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**h = a - w(k)*(1/d(k))*w(k)**h
                       r1 = one / real( ap( kc ),KIND=sp)
                       call stdlib_chpr( uplo, n-k, -r1, ap( kc+1 ), 1_ilp,ap( kc+n-k+1 ) )
                       ! store l(k) in column k
                       call stdlib_csscal( n-k, r1, ap( kc+1 ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**h
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**h
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d = stdlib_slapy2( real( ap( k+1+( k-1 )*( 2_ilp*n-k ) / 2_ilp ),KIND=sp),aimag( &
                                 ap( k+1+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) ) )
                       d11 = real( ap( k+1+k*( 2_ilp*n-k-1 ) / 2_ilp ),KIND=sp) / d
                       d22 = real( ap( k+( k-1 )*( 2_ilp*n-k ) / 2_ilp ),KIND=sp) / d
                       tt = one / ( d11*d22-one )
                       d21 = ap( k+1+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) / d
                       d = tt / d
                       do j = k + 2, n
                          wk = d*( d11*ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp )-d21*ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp )&
                                     )
                          wkp1 = d*( d22*ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp )-conjg( d21 )*ap( j+( k-1 )*( &
                                    2_ilp*n-k ) / 2_ilp ) )
                          do i = j, n
                             ap( i+( j-1 )*( 2_ilp*n-j ) / 2_ilp ) = ap( i+( j-1 )*( 2_ilp*n-j ) / 2_ilp ) - ap( &
                             i+( k-1 )*( 2_ilp*n-k ) /2_ilp )*conjg( wk ) - ap( i+k*( 2_ilp*n-k-1 ) / 2_ilp )&
                                       *conjg( wkp1 )
                          end do
                          ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) = wk
                          ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp ) = wkp1
                          ap( j+( j-1 )*( 2_ilp*n-j ) / 2_ilp )= cmplx( real( ap( j+( j-1 )*( 2_ilp*n-j ) / 2_ilp &
                                    ),KIND=sp),zero,KIND=sp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              kc = knc + n - k + 2_ilp
              go to 60
           end if
           110 continue
           return
     end subroutine stdlib_chptrf

     pure module subroutine stdlib_zhptrf( uplo, n, ap, ipiv, info )
     !! ZHPTRF computes the factorization of a complex Hermitian packed
     !! matrix A using the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**H  or  A = L*D*L**H
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is Hermitian and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: ap(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, imax, j, jmax, k, kc, kk, knc, kp, kpc, kstep, kx, npp
           real(dp) :: absakk, alpha, colmax, d, d11, d22, r1, rowmax, tt
           complex(dp) :: d12, d21, t, wk, wkm1, wkp1, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHPTRF', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**h using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              kc = ( n-1 )*n / 2_ilp + 1_ilp
              10 continue
              knc = kc
              ! if k < 1, exit from loop
              if( k<1 )go to 110
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( ap( kc+k-1 ),KIND=dp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k>1_ilp ) then
                 imax = stdlib_izamax( k-1, ap( kc ), 1_ilp )
                 colmax = cabs1( ap( kc+imax-1 ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 ap( kc+k-1 ) = real( ap( kc+k-1 ),KIND=dp)
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    rowmax = zero
                    jmax = imax
                    kx = imax*( imax+1 ) / 2_ilp + imax
                    do j = imax + 1, k
                       if( cabs1( ap( kx ) )>rowmax ) then
                          rowmax = cabs1( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + j
                    end do
                    kpc = ( imax-1 )*imax / 2_ilp + 1_ilp
                    if( imax>1_ilp ) then
                       jmax = stdlib_izamax( imax-1, ap( kpc ), 1_ilp )
                       rowmax = max( rowmax, cabs1( ap( kpc+jmax-1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( ap( kpc+imax-1 ),KIND=dp) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k - kstep + 1_ilp
                 if( kstep==2_ilp )knc = knc - k + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_zswap( kp-1, ap( knc ), 1_ilp, ap( kpc ), 1_ilp )
                    kx = kpc + kp - 1_ilp
                    do j = kp + 1, kk - 1
                       kx = kx + j - 1_ilp
                       t = conjg( ap( knc+j-1 ) )
                       ap( knc+j-1 ) = conjg( ap( kx ) )
                       ap( kx ) = t
                    end do
                    ap( kx+kk-1 ) = conjg( ap( kx+kk-1 ) )
                    r1 = real( ap( knc+kk-1 ),KIND=dp)
                    ap( knc+kk-1 ) = real( ap( kpc+kp-1 ),KIND=dp)
                    ap( kpc+kp-1 ) = r1
                    if( kstep==2_ilp ) then
                       ap( kc+k-1 ) = real( ap( kc+k-1 ),KIND=dp)
                       t = ap( kc+k-2 )
                       ap( kc+k-2 ) = ap( kc+kp-1 )
                       ap( kc+kp-1 ) = t
                    end if
                 else
                    ap( kc+k-1 ) = real( ap( kc+k-1 ),KIND=dp)
                    if( kstep==2_ilp )ap( kc-1 ) = real( ap( kc-1 ),KIND=dp)
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**h = a - w(k)*1/d(k)*w(k)**h
                    r1 = one / real( ap( kc+k-1 ),KIND=dp)
                    call stdlib_zhpr( uplo, k-1, -r1, ap( kc ), 1_ilp, ap )
                    ! store u(k) in column k
                    call stdlib_zdscal( k-1, r1, ap( kc ), 1_ilp )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**h
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**h
                    if( k>2_ilp ) then
                       d = stdlib_dlapy2( real( ap( k-1+( k-1 )*k / 2_ilp ),KIND=dp),aimag( ap( k-1+( &
                                 k-1 )*k / 2_ilp ) ) )
                       d22 = real( ap( k-1+( k-2 )*( k-1 ) / 2_ilp ),KIND=dp) / d
                       d11 = real( ap( k+( k-1 )*k / 2_ilp ),KIND=dp) / d
                       tt = one / ( d11*d22-one )
                       d12 = ap( k-1+( k-1 )*k / 2_ilp ) / d
                       d = tt / d
                       do j = k - 2, 1, -1
                          wkm1 = d*( d11*ap( j+( k-2 )*( k-1 ) / 2_ilp )-conjg( d12 )*ap( j+( k-1 )*k &
                                    / 2_ilp ) )
                          wk = d*( d22*ap( j+( k-1 )*k / 2_ilp )-d12*ap( j+( k-2 )*( k-1 ) / 2_ilp ) )
                                    
                          do i = j, 1, -1
                             ap( i+( j-1 )*j / 2_ilp ) = ap( i+( j-1 )*j / 2_ilp ) -ap( i+( k-1 )*k / 2_ilp )&
                                       *conjg( wk ) -ap( i+( k-2 )*( k-1 ) / 2_ilp )*conjg( wkm1 )
                          end do
                          ap( j+( k-1 )*k / 2_ilp ) = wk
                          ap( j+( k-2 )*( k-1 ) / 2_ilp ) = wkm1
                          ap( j+( j-1 )*j / 2_ilp ) = cmplx( real( ap( j+( j-1 )*j / 2_ilp ),KIND=dp), &
                                    zero,KIND=dp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              kc = knc - k
              go to 10
           else
              ! factorize a as l*d*l**h using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp
              kc = 1_ilp
              npp = n*( n+1 ) / 2_ilp
              60 continue
              knc = kc
              ! if k > n, exit from loop
              if( k>n )go to 110
              kstep = 1_ilp
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( ap( kc ),KIND=dp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k<n ) then
                 imax = k + stdlib_izamax( n-k, ap( kc+1 ), 1_ilp )
                 colmax = cabs1( ap( kc+imax-k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp )info = k
                 kp = k
                 ap( kc ) = real( ap( kc ),KIND=dp)
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    rowmax = zero
                    kx = kc + imax - k
                    do j = k, imax - 1
                       if( cabs1( ap( kx ) )>rowmax ) then
                          rowmax = cabs1( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + n - j
                    end do
                    kpc = npp - ( n-imax+1 )*( n-imax+2 ) / 2_ilp + 1_ilp
                    if( imax<n ) then
                       jmax = imax + stdlib_izamax( n-imax, ap( kpc+1 ), 1_ilp )
                       rowmax = max( rowmax, cabs1( ap( kpc+jmax-imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( ap( kpc ),KIND=dp) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp
                    end if
                 end if
                 kk = k + kstep - 1_ilp
                 if( kstep==2_ilp )knc = knc + n - k + 1_ilp
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_zswap( n-kp, ap( knc+kp-kk+1 ), 1_ilp, ap( kpc+1 ),1_ilp )
                              
                    kx = knc + kp - kk
                    do j = kk + 1, kp - 1
                       kx = kx + n - j + 1_ilp
                       t = conjg( ap( knc+j-kk ) )
                       ap( knc+j-kk ) = conjg( ap( kx ) )
                       ap( kx ) = t
                    end do
                    ap( knc+kp-kk ) = conjg( ap( knc+kp-kk ) )
                    r1 = real( ap( knc ),KIND=dp)
                    ap( knc ) = real( ap( kpc ),KIND=dp)
                    ap( kpc ) = r1
                    if( kstep==2_ilp ) then
                       ap( kc ) = real( ap( kc ),KIND=dp)
                       t = ap( kc+1 )
                       ap( kc+1 ) = ap( kc+kp-k )
                       ap( kc+kp-k ) = t
                    end if
                 else
                    ap( kc ) = real( ap( kc ),KIND=dp)
                    if( kstep==2_ilp )ap( knc ) = real( ap( knc ),KIND=dp)
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**h = a - w(k)*(1/d(k))*w(k)**h
                       r1 = one / real( ap( kc ),KIND=dp)
                       call stdlib_zhpr( uplo, n-k, -r1, ap( kc+1 ), 1_ilp,ap( kc+n-k+1 ) )
                       ! store l(k) in column k
                       call stdlib_zdscal( n-k, r1, ap( kc+1 ), 1_ilp )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**h
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**h
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d = stdlib_dlapy2( real( ap( k+1+( k-1 )*( 2_ilp*n-k ) / 2_ilp ),KIND=dp),aimag( &
                                 ap( k+1+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) ) )
                       d11 = real( ap( k+1+k*( 2_ilp*n-k-1 ) / 2_ilp ),KIND=dp) / d
                       d22 = real( ap( k+( k-1 )*( 2_ilp*n-k ) / 2_ilp ),KIND=dp) / d
                       tt = one / ( d11*d22-one )
                       d21 = ap( k+1+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) / d
                       d = tt / d
                       do j = k + 2, n
                          wk = d*( d11*ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp )-d21*ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp )&
                                     )
                          wkp1 = d*( d22*ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp )-conjg( d21 )*ap( j+( k-1 )*( &
                                    2_ilp*n-k ) /2_ilp ) )
                          do i = j, n
                             ap( i+( j-1 )*( 2_ilp*n-j ) / 2_ilp ) = ap( i+( j-1 )*( 2_ilp*n-j ) / 2_ilp ) - ap( &
                             i+( k-1 )*( 2_ilp*n-k ) /2_ilp )*conjg( wk ) - ap( i+k*( 2_ilp*n-k-1 ) / 2_ilp )&
                                       *conjg( wkp1 )
                          end do
                          ap( j+( k-1 )*( 2_ilp*n-k ) / 2_ilp ) = wk
                          ap( j+k*( 2_ilp*n-k-1 ) / 2_ilp ) = wkp1
                          ap( j+( j-1 )*( 2_ilp*n-j ) / 2_ilp )= cmplx( real( ap( j+( j-1 )*( 2_ilp*n-j ) / 2_ilp &
                                    ),KIND=dp),zero,KIND=dp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              kc = knc + n - k + 2_ilp
              go to 60
           end if
           110 continue
           return
     end subroutine stdlib_zhptrf




     pure module subroutine stdlib_chptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! CHPTRS solves a system of linear equations A*X = B with a complex
     !! Hermitian matrix A stored in packed format using the factorization
     !! A = U*D*U**H or A = L*D*L**H computed by CHPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kc, kp
           real(sp) :: s
           complex(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**h.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp + 1_ilp
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              kc = kc - k
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_cgeru( k-1, nrhs, -cone, ap( kc ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=sp) / real( ap( kc+k-1 ),KIND=sp)
                 call stdlib_csscal( nrhs, s, b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_cswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_cgeru( k-2, nrhs, -cone, ap( kc ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 call stdlib_cgeru( k-2, nrhs, -cone, ap( kc-( k-1 ) ), 1_ilp,b( k-1, 1_ilp ), ldb, b( 1_ilp, &
                           1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+k-2 )
                 akm1 = ap( kc-1 ) / akm1k
                 ak = ap( kc+k-1 ) / conjg( akm1k )
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / conjg( akm1k )
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc - k + 1_ilp
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**h *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**h(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 if( k>1_ilp ) then
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, ap( kc ), &
                              1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc + k
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**h(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k>1_ilp ) then
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, ap( kc ), &
                              1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k+1, 1_ilp ), ldb )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, ap( kc+k ),&
                               1_ilp, cone, b( k+1, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k+1, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc + 2_ilp*k + 1_ilp
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**h.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_cgeru( n-k, nrhs, -cone, ap( kc+1 ), 1_ilp, b( k, 1_ilp ),ldb, b( k+&
                           1_ilp, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=sp) / real( ap( kc ),KIND=sp)
                 call stdlib_csscal( nrhs, s, b( k, 1_ilp ), ldb )
                 kc = kc + n - k + 1_ilp
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_cswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_cgeru( n-k-1, nrhs, -cone, ap( kc+2 ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, &
                              1_ilp ), ldb )
                    call stdlib_cgeru( n-k-1, nrhs, -cone, ap( kc+n-k+2 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( &
                              k+2, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+1 )
                 akm1 = ap( kc ) / conjg( akm1k )
                 ak = ap( kc+n-k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / conjg( akm1k )
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc + 2_ilp*( n-k ) + 1_ilp
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**h *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp + 1_ilp
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              kc = kc - ( n-k+1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**h(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n ) then
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp ), ldb, &
                              ap( kc+1 ), 1_ilp, cone,b( k, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**h(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp ), ldb, &
                              ap( kc+1 ), 1_ilp, cone,b( k, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k-1, 1_ilp ), ldb )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp ), ldb, &
                              ap( kc-( n-k ) ), 1_ilp, cone,b( k-1, 1_ilp ), ldb )
                    call stdlib_clacgv( nrhs, b( k-1, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_cswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc - ( n-k+2 )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_chptrs

     pure module subroutine stdlib_zhptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! ZHPTRS solves a system of linear equations A*X = B with a complex
     !! Hermitian matrix A stored in packed format using the factorization
     !! A = U*D*U**H or A = L*D*L**H computed by ZHPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kc, kp
           real(dp) :: s
           complex(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**h.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp + 1_ilp
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              kc = kc - k
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_zgeru( k-1, nrhs, -cone, ap( kc ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=dp) / real( ap( kc+k-1 ),KIND=dp)
                 call stdlib_zdscal( nrhs, s, b( k, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_zswap( nrhs, b( k-1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_zgeru( k-2, nrhs, -cone, ap( kc ), 1_ilp, b( k, 1_ilp ), ldb,b( 1_ilp, 1_ilp ), ldb )
                           
                 call stdlib_zgeru( k-2, nrhs, -cone, ap( kc-( k-1 ) ), 1_ilp,b( k-1, 1_ilp ), ldb, b( 1_ilp, &
                           1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+k-2 )
                 akm1 = ap( kc-1 ) / akm1k
                 ak = ap( kc+k-1 ) / conjg( akm1k )
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / conjg( akm1k )
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc - k + 1_ilp
                 k = k - 2_ilp
              end if
              go to 10
              30 continue
              ! next solve u**h *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**h(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 if( k>1_ilp ) then
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, ap( kc ), &
                              1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc + k
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**h(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k>1_ilp ) then
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, ap( kc ), &
                              1_ilp, cone, b( k, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k+1, 1_ilp ), ldb )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, ap( kc+k ),&
                               1_ilp, cone, b( k+1, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k+1, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc + 2_ilp*k + 1_ilp
                 k = k + 2_ilp
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**h.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_zgeru( n-k, nrhs, -cone, ap( kc+1 ), 1_ilp, b( k, 1_ilp ),ldb, b( k+&
                           1_ilp, 1_ilp ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=dp) / real( ap( kc ),KIND=dp)
                 call stdlib_zdscal( nrhs, s, b( k, 1_ilp ), ldb )
                 kc = kc + n - k + 1_ilp
                 k = k + 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_zswap( nrhs, b( k+1, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_zgeru( n-k-1, nrhs, -cone, ap( kc+2 ), 1_ilp, b( k, 1_ilp ),ldb, b( k+2, &
                              1_ilp ), ldb )
                    call stdlib_zgeru( n-k-1, nrhs, -cone, ap( kc+n-k+2 ), 1_ilp,b( k+1, 1_ilp ), ldb, b( &
                              k+2, 1_ilp ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+1 )
                 akm1 = ap( kc ) / conjg( akm1k )
                 ak = ap( kc+n-k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / conjg( akm1k )
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc + 2_ilp*( n-k ) + 1_ilp
                 k = k + 2_ilp
              end if
              go to 60
              80 continue
              ! next solve l**h *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp + 1_ilp
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              kc = kc - ( n-k+1 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**h(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n ) then
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp ), ldb, &
                              ap( kc+1 ), 1_ilp, cone,b( k, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 k = k - 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**h(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp ), ldb, &
                              ap( kc+1 ), 1_ilp, cone,b( k, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k-1, 1_ilp ), ldb )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp ), ldb, &
                              ap( kc-( n-k ) ), 1_ilp, cone,b( k-1, 1_ilp ), ldb )
                    call stdlib_zlacgv( nrhs, b( k-1, 1_ilp ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_zswap( nrhs, b( k, 1_ilp ), ldb, b( kp, 1_ilp ), ldb )
                 kc = kc - ( n-k+2 )
                 k = k - 2_ilp
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_zhptrs




     pure module subroutine stdlib_chptri( uplo, n, ap, ipiv, work, info )
     !! CHPTRI computes the inverse of a complex Hermitian indefinite matrix
     !! A in packed storage using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by CHPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kc, kcnext, kp, kpc, kstep, kx, npp
           real(sp) :: ak, akp1, d, t
           complex(sp) :: akkp1, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHPTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              kp = n*( n+1 ) / 2_ilp
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. ap( kp )==czero )return
                 kp = kp - info
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              kp = 1_ilp
              do info = 1, n
                 if( ipiv( info )>0 .and. ap( kp )==czero )return
                 kp = kp + n - info + 1_ilp
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              kcnext = kc + k
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc+k-1 ) = one / real( ap( kc+k-1 ),KIND=sp)
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_ccopy( k-1, ap( kc ), 1_ilp, work, 1_ilp )
                    call stdlib_chpmv( uplo, k-1, -cone, ap, work, 1_ilp, czero,ap( kc ), 1_ilp )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -real( stdlib_cdotc( k-1, work, 1_ilp, ap( kc ), 1_ilp ),&
                              KIND=sp)
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( ap( kcnext+k-1 ) )
                 ak = real( ap( kc+k-1 ),KIND=sp) / t
                 akp1 = real( ap( kcnext+k ),KIND=sp) / t
                 akkp1 = ap( kcnext+k-1 ) / t
                 d = t*( ak*akp1-one )
                 ap( kc+k-1 ) = akp1 / d
                 ap( kcnext+k ) = ak / d
                 ap( kcnext+k-1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_ccopy( k-1, ap( kc ), 1_ilp, work, 1_ilp )
                    call stdlib_chpmv( uplo, k-1, -cone, ap, work, 1_ilp, czero,ap( kc ), 1_ilp )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -real( stdlib_cdotc( k-1, work, 1_ilp, ap( kc ), 1_ilp ),&
                              KIND=sp)
                    ap( kcnext+k-1 ) = ap( kcnext+k-1 ) -stdlib_cdotc( k-1, ap( kc ), 1_ilp, ap( &
                              kcnext ),1_ilp )
                    call stdlib_ccopy( k-1, ap( kcnext ), 1_ilp, work, 1_ilp )
                    call stdlib_chpmv( uplo, k-1, -cone, ap, work, 1_ilp, czero,ap( kcnext ), 1_ilp )
                              
                    ap( kcnext+k ) = ap( kcnext+k ) -real( stdlib_cdotc( k-1, work, 1_ilp, ap( kcnext &
                              ),1_ilp ),KIND=sp)
                 end if
                 kstep = 2_ilp
                 kcnext = kcnext + k + 1_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 kpc = ( kp-1 )*kp / 2_ilp + 1_ilp
                 call stdlib_cswap( kp-1, ap( kc ), 1_ilp, ap( kpc ), 1_ilp )
                 kx = kpc + kp - 1_ilp
                 do j = kp + 1, k - 1
                    kx = kx + j - 1_ilp
                    temp = conjg( ap( kc+j-1 ) )
                    ap( kc+j-1 ) = conjg( ap( kx ) )
                    ap( kx ) = temp
                 end do
                 ap( kc+kp-1 ) = conjg( ap( kc+kp-1 ) )
                 temp = ap( kc+k-1 )
                 ap( kc+k-1 ) = ap( kpc+kp-1 )
                 ap( kpc+kp-1 ) = temp
                 if( kstep==2_ilp ) then
                    temp = ap( kc+k+k-1 )
                    ap( kc+k+k-1 ) = ap( kc+k+kp-1 )
                    ap( kc+k+kp-1 ) = temp
                 end if
              end if
              k = k + kstep
              kc = kcnext
              go to 30
              50 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              npp = n*( n+1 ) / 2_ilp
              k = n
              kc = npp
              60 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 80
              kcnext = kc - ( n-k+2 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc ) = one / real( ap( kc ),KIND=sp)
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_ccopy( n-k, ap( kc+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_chpmv( uplo, n-k, -cone, ap( kc+n-k+1 ), work, 1_ilp,czero, ap( kc+1 )&
                              , 1_ilp )
                    ap( kc ) = ap( kc ) - real( stdlib_cdotc( n-k, work, 1_ilp,ap( kc+1 ), 1_ilp ),&
                              KIND=sp)
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( ap( kcnext+1 ) )
                 ak = real( ap( kcnext ),KIND=sp) / t
                 akp1 = real( ap( kc ),KIND=sp) / t
                 akkp1 = ap( kcnext+1 ) / t
                 d = t*( ak*akp1-one )
                 ap( kcnext ) = akp1 / d
                 ap( kc ) = ak / d
                 ap( kcnext+1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_ccopy( n-k, ap( kc+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_chpmv( uplo, n-k, -cone, ap( kc+( n-k+1 ) ), work,1_ilp, czero, ap( &
                              kc+1 ), 1_ilp )
                    ap( kc ) = ap( kc ) - real( stdlib_cdotc( n-k, work, 1_ilp,ap( kc+1 ), 1_ilp ),&
                              KIND=sp)
                    ap( kcnext+1 ) = ap( kcnext+1 ) -stdlib_cdotc( n-k, ap( kc+1 ), 1_ilp,ap( kcnext+&
                              2_ilp ), 1_ilp )
                    call stdlib_ccopy( n-k, ap( kcnext+2 ), 1_ilp, work, 1_ilp )
                    call stdlib_chpmv( uplo, n-k, -cone, ap( kc+( n-k+1 ) ), work,1_ilp, czero, ap( &
                              kcnext+2 ), 1_ilp )
                    ap( kcnext ) = ap( kcnext ) -real( stdlib_cdotc( n-k, work, 1_ilp, ap( kcnext+2 ),&
                              1_ilp ),KIND=sp)
                 end if
                 kstep = 2_ilp
                 kcnext = kcnext - ( n-k+3 )
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 kpc = npp - ( n-kp+1 )*( n-kp+2 ) / 2_ilp + 1_ilp
                 if( kp<n )call stdlib_cswap( n-kp, ap( kc+kp-k+1 ), 1_ilp, ap( kpc+1 ), 1_ilp )
                 kx = kc + kp - k
                 do j = k + 1, kp - 1
                    kx = kx + n - j + 1_ilp
                    temp = conjg( ap( kc+j-k ) )
                    ap( kc+j-k ) = conjg( ap( kx ) )
                    ap( kx ) = temp
                 end do
                 ap( kc+kp-k ) = conjg( ap( kc+kp-k ) )
                 temp = ap( kc )
                 ap( kc ) = ap( kpc )
                 ap( kpc ) = temp
                 if( kstep==2_ilp ) then
                    temp = ap( kc-n+k-1 )
                    ap( kc-n+k-1 ) = ap( kc-n+kp-1 )
                    ap( kc-n+kp-1 ) = temp
                 end if
              end if
              k = k - kstep
              kc = kcnext
              go to 60
              80 continue
           end if
           return
     end subroutine stdlib_chptri

     pure module subroutine stdlib_zhptri( uplo, n, ap, ipiv, work, info )
     !! ZHPTRI computes the inverse of a complex Hermitian indefinite matrix
     !! A in packed storage using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by ZHPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, k, kc, kcnext, kp, kpc, kstep, kx, npp
           real(dp) :: ak, akp1, d, t
           complex(dp) :: akkp1, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHPTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              kp = n*( n+1 ) / 2_ilp
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. ap( kp )==czero )return
                 kp = kp - info
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              kp = 1_ilp
              do info = 1, n
                 if( ipiv( info )>0 .and. ap( kp )==czero )return
                 kp = kp + n - info + 1_ilp
              end do
           end if
           info = 0_ilp
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp
              kc = 1_ilp
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              kcnext = kc + k
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc+k-1 ) = one / real( ap( kc+k-1 ),KIND=dp)
                 ! compute column k of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_zcopy( k-1, ap( kc ), 1_ilp, work, 1_ilp )
                    call stdlib_zhpmv( uplo, k-1, -cone, ap, work, 1_ilp, czero,ap( kc ), 1_ilp )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -real( stdlib_zdotc( k-1, work, 1_ilp, ap( kc ), 1_ilp ),&
                              KIND=dp)
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( ap( kcnext+k-1 ) )
                 ak = real( ap( kc+k-1 ),KIND=dp) / t
                 akp1 = real( ap( kcnext+k ),KIND=dp) / t
                 akkp1 = ap( kcnext+k-1 ) / t
                 d = t*( ak*akp1-one )
                 ap( kc+k-1 ) = akp1 / d
                 ap( kcnext+k ) = ak / d
                 ap( kcnext+k-1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp ) then
                    call stdlib_zcopy( k-1, ap( kc ), 1_ilp, work, 1_ilp )
                    call stdlib_zhpmv( uplo, k-1, -cone, ap, work, 1_ilp, czero,ap( kc ), 1_ilp )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -real( stdlib_zdotc( k-1, work, 1_ilp, ap( kc ), 1_ilp ),&
                              KIND=dp)
                    ap( kcnext+k-1 ) = ap( kcnext+k-1 ) -stdlib_zdotc( k-1, ap( kc ), 1_ilp, ap( &
                              kcnext ),1_ilp )
                    call stdlib_zcopy( k-1, ap( kcnext ), 1_ilp, work, 1_ilp )
                    call stdlib_zhpmv( uplo, k-1, -cone, ap, work, 1_ilp, czero,ap( kcnext ), 1_ilp )
                              
                    ap( kcnext+k ) = ap( kcnext+k ) -real( stdlib_zdotc( k-1, work, 1_ilp, ap( kcnext &
                              ),1_ilp ),KIND=dp)
                 end if
                 kstep = 2_ilp
                 kcnext = kcnext + k + 1_ilp
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 kpc = ( kp-1 )*kp / 2_ilp + 1_ilp
                 call stdlib_zswap( kp-1, ap( kc ), 1_ilp, ap( kpc ), 1_ilp )
                 kx = kpc + kp - 1_ilp
                 do j = kp + 1, k - 1
                    kx = kx + j - 1_ilp
                    temp = conjg( ap( kc+j-1 ) )
                    ap( kc+j-1 ) = conjg( ap( kx ) )
                    ap( kx ) = temp
                 end do
                 ap( kc+kp-1 ) = conjg( ap( kc+kp-1 ) )
                 temp = ap( kc+k-1 )
                 ap( kc+k-1 ) = ap( kpc+kp-1 )
                 ap( kpc+kp-1 ) = temp
                 if( kstep==2_ilp ) then
                    temp = ap( kc+k+k-1 )
                    ap( kc+k+k-1 ) = ap( kc+k+kp-1 )
                    ap( kc+k+kp-1 ) = temp
                 end if
              end if
              k = k + kstep
              kc = kcnext
              go to 30
              50 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              npp = n*( n+1 ) / 2_ilp
              k = n
              kc = npp
              60 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 80
              kcnext = kc - ( n-k+2 )
              if( ipiv( k )>0_ilp ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc ) = one / real( ap( kc ),KIND=dp)
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_zcopy( n-k, ap( kc+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_zhpmv( uplo, n-k, -cone, ap( kc+n-k+1 ), work, 1_ilp,czero, ap( kc+1 )&
                              , 1_ilp )
                    ap( kc ) = ap( kc ) - real( stdlib_zdotc( n-k, work, 1_ilp,ap( kc+1 ), 1_ilp ),&
                              KIND=dp)
                 end if
                 kstep = 1_ilp
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( ap( kcnext+1 ) )
                 ak = real( ap( kcnext ),KIND=dp) / t
                 akp1 = real( ap( kc ),KIND=dp) / t
                 akkp1 = ap( kcnext+1 ) / t
                 d = t*( ak*akp1-one )
                 ap( kcnext ) = akp1 / d
                 ap( kc ) = ak / d
                 ap( kcnext+1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_zcopy( n-k, ap( kc+1 ), 1_ilp, work, 1_ilp )
                    call stdlib_zhpmv( uplo, n-k, -cone, ap( kc+( n-k+1 ) ), work,1_ilp, czero, ap( &
                              kc+1 ), 1_ilp )
                    ap( kc ) = ap( kc ) - real( stdlib_zdotc( n-k, work, 1_ilp,ap( kc+1 ), 1_ilp ),&
                              KIND=dp)
                    ap( kcnext+1 ) = ap( kcnext+1 ) -stdlib_zdotc( n-k, ap( kc+1 ), 1_ilp,ap( kcnext+&
                              2_ilp ), 1_ilp )
                    call stdlib_zcopy( n-k, ap( kcnext+2 ), 1_ilp, work, 1_ilp )
                    call stdlib_zhpmv( uplo, n-k, -cone, ap( kc+( n-k+1 ) ), work,1_ilp, czero, ap( &
                              kcnext+2 ), 1_ilp )
                    ap( kcnext ) = ap( kcnext ) -real( stdlib_zdotc( n-k, work, 1_ilp, ap( kcnext+2 ),&
                              1_ilp ),KIND=dp)
                 end if
                 kstep = 2_ilp
                 kcnext = kcnext - ( n-k+3 )
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 kpc = npp - ( n-kp+1 )*( n-kp+2 ) / 2_ilp + 1_ilp
                 if( kp<n )call stdlib_zswap( n-kp, ap( kc+kp-k+1 ), 1_ilp, ap( kpc+1 ), 1_ilp )
                 kx = kc + kp - k
                 do j = k + 1, kp - 1
                    kx = kx + n - j + 1_ilp
                    temp = conjg( ap( kc+j-k ) )
                    ap( kc+j-k ) = conjg( ap( kx ) )
                    ap( kx ) = temp
                 end do
                 ap( kc+kp-k ) = conjg( ap( kc+kp-k ) )
                 temp = ap( kc )
                 ap( kc ) = ap( kpc )
                 ap( kpc ) = temp
                 if( kstep==2_ilp ) then
                    temp = ap( kc-n+k-1 )
                    ap( kc-n+k-1 ) = ap( kc-n+kp-1 )
                    ap( kc-n+kp-1 ) = temp
                 end if
              end if
              k = k - kstep
              kc = kcnext
              go to 60
              80 continue
           end if
           return
     end subroutine stdlib_zhptri




     pure module subroutine stdlib_I64_checon( uplo, n, a, lda, ipiv, anorm, rcond, work,info )
     !! CHECON estimates the reciprocal of the condition number of a complex
     !! Hermitian matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by CHETRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, kase
           real(sp) :: ainvnm
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( anorm<zero ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHECON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp64 ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do i = 1, n
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp64
           30 continue
           call stdlib_I64_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              ! multiply by inv(l*d*l**h) or inv(u*d*u**h).
              call stdlib_I64_chetrs( uplo, n, 1_ilp64, a, lda, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_I64_checon

     pure module subroutine stdlib_I64_zhecon( uplo, n, a, lda, ipiv, anorm, rcond, work,info )
     !! ZHECON estimates the reciprocal of the condition number of a complex
     !! Hermitian matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by ZHETRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, kase
           real(dp) :: ainvnm
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( anorm<zero ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHECON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp64 ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do i = 1, n
                 if( ipiv( i )>0 .and. a( i, i )==zero )return
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp64
           30 continue
           call stdlib_I64_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              ! multiply by inv(l*d*l**h) or inv(u*d*u**h).
              call stdlib_I64_zhetrs( uplo, n, 1_ilp64, a, lda, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_I64_zhecon




     pure module subroutine stdlib_I64_chetrf( uplo, n, a, lda, ipiv, work, lwork, info )
     !! CHETRF computes the factorization of a complex Hermitian matrix A
     !! using the Bunch-Kaufman diagonal pivoting method.  The form of the
     !! factorization is
     !! A = U*D*U**H  or  A = L*D*L**H
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is Hermitian and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: iinfo, iws, j, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( lwork<1_ilp64 .and. .not.lquery ) then
              info = -7_ilp64
           end if
           if( info==0_ilp64 ) then
              ! determine the block size
              nb = stdlib_I64_ilaenv( 1_ilp64, 'CHETRF', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = n*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHETRF', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp64
           ldwork = n
           if( nb>1_ilp64 .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp64 )
                 nbmin = max( 2_ilp64, stdlib_I64_ilaenv( 2_ilp64, 'CHETRF', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 ) )
              end if
           else
              iws = 1_ilp64
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**h using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_I64_clahef;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 40
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_I64_clahef( uplo, k, nb, kb, a, lda, ipiv, work, n, iinfo )
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_I64_chetf2( uplo, k, a, lda, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp64 .and. iinfo>0_ilp64 )info = iinfo
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
           else
              ! factorize a as l*d*l**h using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_I64_clahef;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp64
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 40
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_I64_clahef( uplo, n-k+1, nb, kb, a( k, k ), lda, ipiv( k ),work, n, &
                           iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_I64_chetf2( uplo, n-k+1, a( k, k ), lda, ipiv( k ), iinfo )
                 kb = n - k + 1_ilp64
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp64 .and. iinfo>0_ilp64 )info = iinfo + k - 1_ilp64
              ! adjust ipiv
              do j = k, k + kb - 1
                 if( ipiv( j )>0_ilp64 ) then
                    ipiv( j ) = ipiv( j ) + k - 1_ilp64
                 else
                    ipiv( j ) = ipiv( j ) - k + 1_ilp64
                 end if
              end do
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
           end if
           40 continue
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_chetrf

     pure module subroutine stdlib_I64_zhetrf( uplo, n, a, lda, ipiv, work, lwork, info )
     !! ZHETRF computes the factorization of a complex Hermitian matrix A
     !! using the Bunch-Kaufman diagonal pivoting method.  The form of the
     !! factorization is
     !! A = U*D*U**H  or  A = L*D*L**H
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is Hermitian and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the blocked version of the algorithm, calling Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: iinfo, iws, j, k, kb, ldwork, lwkopt, nb, nbmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( lwork<1_ilp64 .and. .not.lquery ) then
              info = -7_ilp64
           end if
           if( info==0_ilp64 ) then
              ! determine the block size
              nb = stdlib_I64_ilaenv( 1_ilp64, 'ZHETRF', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = n*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHETRF', -info )
              return
           else if( lquery ) then
              return
           end if
           nbmin = 2_ilp64
           ldwork = n
           if( nb>1_ilp64 .and. nb<n ) then
              iws = ldwork*nb
              if( lwork<iws ) then
                 nb = max( lwork / ldwork, 1_ilp64 )
                 nbmin = max( 2_ilp64, stdlib_I64_ilaenv( 2_ilp64, 'ZHETRF', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 ) )
              end if
           else
              iws = 1_ilp64
           end if
           if( nb<nbmin )nb = n
           if( upper ) then
              ! factorize a as u*d*u**h using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! kb, where kb is the number of columns factorized by stdlib_I64_zlahef;
              ! kb is either nb or nb-1, or k for the last block
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 40
              if( k>nb ) then
                 ! factorize columns k-kb+1:k of a and use blocked code to
                 ! update columns 1:k-kb
                 call stdlib_I64_zlahef( uplo, k, nb, kb, a, lda, ipiv, work, n, iinfo )
              else
                 ! use unblocked code to factorize columns 1:k of a
                 call stdlib_I64_zhetf2( uplo, k, a, lda, ipiv, iinfo )
                 kb = k
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp64 .and. iinfo>0_ilp64 )info = iinfo
              ! decrease k and return to the start of the main loop
              k = k - kb
              go to 10
           else
              ! factorize a as l*d*l**h using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! kb, where kb is the number of columns factorized by stdlib_I64_zlahef;
              ! kb is either nb or nb-1, or n-k+1 for the last block
              k = 1_ilp64
              20 continue
              ! if k > n, exit from loop
              if( k>n )go to 40
              if( k<=n-nb ) then
                 ! factorize columns k:k+kb-1 of a and use blocked code to
                 ! update columns k+kb:n
                 call stdlib_I64_zlahef( uplo, n-k+1, nb, kb, a( k, k ), lda, ipiv( k ),work, n, &
                           iinfo )
              else
                 ! use unblocked code to factorize columns k:n of a
                 call stdlib_I64_zhetf2( uplo, n-k+1, a( k, k ), lda, ipiv( k ), iinfo )
                 kb = n - k + 1_ilp64
              end if
              ! set info on the first occurrence of a zero pivot
              if( info==0_ilp64 .and. iinfo>0_ilp64 )info = iinfo + k - 1_ilp64
              ! adjust ipiv
              do j = k, k + kb - 1
                 if( ipiv( j )>0_ilp64 ) then
                    ipiv( j ) = ipiv( j ) + k - 1_ilp64
                 else
                    ipiv( j ) = ipiv( j ) - k + 1_ilp64
                 end if
              end do
              ! increase k and return to the start of the main loop
              k = k + kb
              go to 20
           end if
           40 continue
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_zhetrf




     pure module subroutine stdlib_I64_clahef( uplo, n, nb, kb, a, lda, ipiv, w, ldw, info )
     !! CLAHEF computes a partial factorization of a complex Hermitian
     !! matrix A using the Bunch-Kaufman diagonal pivoting method. The
     !! partial factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I      0     )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**H U22**H )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**H L21**H )  if UPLO = 'L'
     !! ( L21  I ) (  0  A22 ) (  0      I     )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! Note that U**H denotes the conjugate transpose of U.
     !! CLAHEF is an auxiliary routine called by CHETRF. It uses blocked code
     !! (calling Level 3 BLAS) to update the submatrix A11 (if UPLO = 'U') or
     !! A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info, kb
           integer(ilp64), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           
           ! Local Scalars 
           integer(ilp64) :: imax, j, jb, jj, jmax, jp, k, kk, kkw, kp, kstep, kw
           real(sp) :: absakk, alpha, colmax, r1, rowmax, t
           complex(sp) :: d11, d21, d22, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=sp) ) + abs( aimag( z ) )
           ! Executable Statements 
           info = 0_ilp64
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11 (note that conjg(w) is actually stored)
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              k = n
              10 continue
              ! kw is the column of w which corresponds to column k of a
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              kstep = 1_ilp64
              ! copy column k of a to column kw of w and update it
              call stdlib_I64_ccopy( k-1, a( 1_ilp64, k ), 1_ilp64, w( 1_ilp64, kw ), 1_ilp64 )
              w( k, kw ) = real( a( k, k ),KIND=sp)
              if( k<n ) then
                 call stdlib_I64_cgemv( 'NO TRANSPOSE', k, n-k, -cone, a( 1_ilp64, k+1 ), lda,w( k, kw+1 ), &
                           ldw, cone, w( 1_ilp64, kw ), 1_ilp64 )
                 w( k, kw ) = real( w( k, kw ),KIND=sp)
              end if
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( w( k, kw ),KIND=sp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp64 ) then
                 imax = stdlib_I64_icamax( k-1, w( 1_ilp64, kw ), 1_ilp64 )
                 colmax = cabs1( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp64 )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=sp)
              else
                 ! ============================================================
                 ! begin pivot search
                 ! case(1)
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! begin pivot search along imax row
                    ! copy column imax to column kw-1 of w and update it
                    call stdlib_I64_ccopy( imax-1, a( 1_ilp64, imax ), 1_ilp64, w( 1_ilp64, kw-1 ), 1_ilp64 )
                    w( imax, kw-1 ) = real( a( imax, imax ),KIND=sp)
                    call stdlib_I64_ccopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp64 )
                              
                    call stdlib_I64_clacgv( k-imax, w( imax+1, kw-1 ), 1_ilp64 )
                    if( k<n ) then
                       call stdlib_I64_cgemv( 'NO TRANSPOSE', k, n-k, -cone,a( 1_ilp64, k+1 ), lda, w( imax,&
                                  kw+1 ), ldw,cone, w( 1_ilp64, kw-1 ), 1_ilp64 )
                       w( imax, kw-1 ) = real( w( imax, kw-1 ),KIND=sp)
                    end if
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value.
                    ! determine only rowmax.
                    jmax = imax + stdlib_I64_icamax( k-imax, w( imax+1, kw-1 ), 1_ilp64 )
                    rowmax = cabs1( w( jmax, kw-1 ) )
                    if( imax>1_ilp64 ) then
                       jmax = stdlib_I64_icamax( imax-1, w( 1_ilp64, kw-1 ), 1_ilp64 )
                       rowmax = max( rowmax, cabs1( w( jmax, kw-1 ) ) )
                    end if
                    ! case(2)
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    ! case(3)
                    else if( abs( real( w( imax, kw-1 ),KIND=sp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column kw-1 of w to column kw of w
                       call stdlib_I64_ccopy( k, w( 1_ilp64, kw-1 ), 1_ilp64, w( 1_ilp64, kw ), 1_ilp64 )
                    ! case(4)
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp64
                    end if
                    ! end pivot search along imax row
                 end if
                 ! end pivot search
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k - kstep + 1_ilp64
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kkw of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k-1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = real( a( kk, kk ),KIND=sp)
                    call stdlib_I64_ccopy( kk-1-kp, a( kp+1, kk ), 1_ilp64, a( kp, kp+1 ),lda )
                    call stdlib_I64_clacgv( kk-1-kp, a( kp, kp+1 ), lda )
                    if( kp>1_ilp64 )call stdlib_I64_ccopy( kp-1, a( 1_ilp64, kk ), 1_ilp64, a( 1_ilp64, kp ), 1_ilp64 )
                    ! interchange rows kk and kp in last k+1 to n columns of a
                    ! (columns k (or k and k-1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in last kkw to nb columns of w.
                    if( k<n )call stdlib_I64_cswap( n-k, a( kk, k+1 ), lda, a( kp, k+1 ),lda )
                    call stdlib_I64_cswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp64 ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(kw) = u(k)*d(k),
                    ! where u(k) is the k-th column of u
                    ! (1) store subdiag. elements of column u(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! (note: diagonal element u(k,k) is a unit element
                    ! and not stored)
                       ! a(k,k) := d(k,k) = w(k,kw)
                       ! a(1:k-1,k) := u(1:k-1,k) = w(1:k-1,kw)/d(k,k)
                    ! (note: no need to use for hermitian matrix
                    ! a( k, k ) = real( w( k, k),KIND=sp) to separately copy diagonal
                    ! element d(k,k) from w (potentially saves only one load))
                    call stdlib_I64_ccopy( k, w( 1_ilp64, kw ), 1_ilp64, a( 1_ilp64, k ), 1_ilp64 )
                    if( k>1_ilp64 ) then
                       ! (note: no need to check if a(k,k) is not zero,
                        ! since that was ensured earlier in pivot search:
                        ! case a(k,k) = 0 falls into 2x2 pivot case(4))
                       r1 = one / real( a( k, k ),KIND=sp)
                       call stdlib_I64_csscal( k-1, r1, a( 1_ilp64, k ), 1_ilp64 )
                       ! (2) conjugate column w(kw)
                       call stdlib_I64_clacgv( k-1, w( 1_ilp64, kw ), 1_ilp64 )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now hold
                    ! ( w(kw-1) w(kw) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! (1) store u(1:k-2,k-1) and u(1:k-2,k) and 2-by-2
                    ! block d(k-1:k,k-1:k) in columns k-1 and k of a.
                    ! (note: 2-by-2 diagonal block u(k-1:k,k-1:k) is a unit
                    ! block and not stored)
                       ! a(k-1:k,k-1:k) := d(k-1:k,k-1:k) = w(k-1:k,kw-1:kw)
                       ! a(1:k-2,k-1:k) := u(1:k-2,k:k-1:k) =
                       ! = w(1:k-2,kw-1:kw) * ( d(k-1:k,k-1:k)**(-1) )
                    if( k>2_ilp64 ) then
                       ! factor out the columns of the inverse of 2-by-2 pivot
                       ! block d, so that each column contains 1, to reduce the
                       ! number of flops when we multiply panel
                       ! ( w(kw-1) w(kw) ) by this inverse, i.e. by d**(-1).
                       ! d**(-1) = ( d11 cj(d21) )**(-1) =
                                 ! ( d21    d22 )
                       ! = 1/(d11*d22-|d21|**2) * ( ( d22) (-cj(d21) ) ) =
                                                ! ( (-d21) (     d11 ) )
                       ! = 1/(|d21|**2) * 1/((d11/cj(d21))*(d22/d21)-1) *
                         ! * ( d21*( d22/d21 ) conj(d21)*(           - 1 ) ) =
                           ! (     (      -1 )           ( d11/conj(d21) ) )
                       ! = 1/(|d21|**2) * 1/(d22*d11-1) *
                         ! * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                           ! (     (  -1 )           ( d22 ) )
                       ! = (1/|d21|**2) * t * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                                            ! (     (  -1 )           ( d22 ) )
                       ! = ( (t/conj(d21))*( d11 ) (t/d21)*(  -1 ) ) =
                         ! (               (  -1 )         ( d22 ) )
                       ! = ( conj(d21)*( d11 ) d21*(  -1 ) )
                         ! (           (  -1 )     ( d22 ) ),
                       ! where d11 = d22/d21,
                             ! d22 = d11/conj(d21),
                             ! d21 = t/d21,
                             ! t = 1/(d22*d11-1).
                       ! (note: no need to check for division by zero,
                        ! since that was ensured earlier in pivot search:
                        ! (a) d21 != 0, since in 2x2 pivot case(4)
                            ! |d21| should be larger than |d11| and |d22|;
                        ! (b) (d22*d11 - 1) != 0, since from (a),
                            ! both |d11| < 1, |d22| < 1, hence |d22*d11| << 1.)
                       d21 = w( k-1, kw )
                       d11 = w( k, kw ) / conjg( d21 )
                       d22 = w( k-1, kw-1 ) / d21
                       t = one / ( real( d11*d22,KIND=sp)-one )
                       d21 = t / d21
                       ! update elements in columns a(k-1) and a(k) as
                       ! dot products of rows of ( w(kw-1) w(kw) ) and columns
                       ! of d**(-1)
                       do j = 1, k - 2
                          a( j, k-1 ) = d21*( d11*w( j, kw-1 )-w( j, kw ) )
                          a( j, k ) = conjg( d21 )*( d22*w( j, kw )-w( j, kw-1 ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = w( k-1, kw )
                    a( k, k ) = w( k, kw )
                    ! (2) conjugate columns w(kw) and w(kw-1)
                    call stdlib_I64_clacgv( k-1, w( 1_ilp64, kw ), 1_ilp64 )
                    call stdlib_I64_clacgv( k-2, w( 1_ilp64, kw-1 ), 1_ilp64 )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp64 ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**h = a11 - u12*w**h
              ! computing blocks of nb columns at a time (note that conjg(w) is
              ! actually stored)
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    a( jj, jj ) = real( a( jj, jj ),KIND=sp)
                    call stdlib_I64_cgemv( 'NO TRANSPOSE', jj-j+1, n-k, -cone,a( j, k+1 ), lda, w( jj,&
                               kw+1 ), ldw, cone,a( j, jj ), 1_ilp64 )
                    a( jj, jj ) = real( a( jj, jj ),KIND=sp)
                 end do
                 ! update the rectangular superdiagonal block
                 call stdlib_I64_cgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb, n-k,-cone, a( 1_ilp64, k+1 ), &
                           lda, w( j, kw+1 ), ldw,cone, a( 1_ilp64, j ), lda )
              end do
              ! put u12 in standard form by partially undoing the interchanges
              ! in of rows in columns k+1:n looping backwards from k+1 to n
              j = k + 1_ilp64
              60 continue
                 ! undo the interchanges (if any) of rows j and jp
                 ! at each step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp64 ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j + 1_ilp64
                 end if
                 ! (note: here, j is used to determine row length. length n-j+1
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j + 1_ilp64
                 if( jp/=jj .and. j<=n )call stdlib_I64_cswap( n-j+1, a( jp, j ), lda, a( jj, j ), &
                           lda )
              if( j<=n )go to 60
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22 (note that conjg(w) is actually stored)
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp64
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              kstep = 1_ilp64
              ! copy column k of a to column k of w and update it
              w( k, k ) = real( a( k, k ),KIND=sp)
              if( k<n )call stdlib_I64_ccopy( n-k, a( k+1, k ), 1_ilp64, w( k+1, k ), 1_ilp64 )
              call stdlib_I64_cgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp64 ), lda,w( k, 1_ilp64 ), ldw,&
                         cone, w( k, k ), 1_ilp64 )
              w( k, k ) = real( w( k, k ),KIND=sp)
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( w( k, k ),KIND=sp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_I64_icamax( n-k, w( k+1, k ), 1_ilp64 )
                 colmax = cabs1( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp64 )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=sp)
              else
                 ! ============================================================
                 ! begin pivot search
                 ! case(1)
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! begin pivot search along imax row
                    ! copy column imax to column k+1 of w and update it
                    call stdlib_I64_ccopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp64 )
                    call stdlib_I64_clacgv( imax-k, w( k, k+1 ), 1_ilp64 )
                    w( imax, k+1 ) = real( a( imax, imax ),KIND=sp)
                    if( imax<n )call stdlib_I64_ccopy( n-imax, a( imax+1, imax ), 1_ilp64,w( imax+1, k+1 ), &
                              1_ilp64 )
                    call stdlib_I64_cgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp64 ),lda, w( imax, &
                              1_ilp64 ), ldw, cone, w( k, k+1 ),1_ilp64 )
                    w( imax, k+1 ) = real( w( imax, k+1 ),KIND=sp)
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value.
                    ! determine only rowmax.
                    jmax = k - 1_ilp64 + stdlib_I64_icamax( imax-k, w( k, k+1 ), 1_ilp64 )
                    rowmax = cabs1( w( jmax, k+1 ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_I64_icamax( n-imax, w( imax+1, k+1 ), 1_ilp64 )
                       rowmax = max( rowmax, cabs1( w( jmax, k+1 ) ) )
                    end if
                    ! case(2)
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    ! case(3)
                    else if( abs( real( w( imax, k+1 ),KIND=sp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column k+1 of w to column k of w
                       call stdlib_I64_ccopy( n-k+1, w( k, k+1 ), 1_ilp64, w( k, k ), 1_ilp64 )
                    ! case(4)
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp64
                    end if
                    ! end pivot search along imax row
                 end if
                 ! end pivot search
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k + kstep - 1_ilp64
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kk of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k+1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = real( a( kk, kk ),KIND=sp)
                    call stdlib_I64_ccopy( kp-kk-1, a( kk+1, kk ), 1_ilp64, a( kp, kk+1 ),lda )
                    call stdlib_I64_clacgv( kp-kk-1, a( kp, kk+1 ), lda )
                    if( kp<n )call stdlib_I64_ccopy( n-kp, a( kp+1, kk ), 1_ilp64, a( kp+1, kp ), 1_ilp64 )
                              
                    ! interchange rows kk and kp in first k-1 columns of a
                    ! (columns k (or k and k+1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in first kk columns of w.
                    if( k>1_ilp64 )call stdlib_I64_cswap( k-1, a( kk, 1_ilp64 ), lda, a( kp, 1_ilp64 ), lda )
                    call stdlib_I64_cswap( kk, w( kk, 1_ilp64 ), ldw, w( kp, 1_ilp64 ), ldw )
                 end if
                 if( kstep==1_ilp64 ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k),
                    ! where l(k) is the k-th column of l
                    ! (1) store subdiag. elements of column l(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! (note: diagonal element l(k,k) is a unit element
                    ! and not stored)
                       ! a(k,k) := d(k,k) = w(k,k)
                       ! a(k+1:n,k) := l(k+1:n,k) = w(k+1:n,k)/d(k,k)
                    ! (note: no need to use for hermitian matrix
                    ! a( k, k ) = real( w( k, k),KIND=sp) to separately copy diagonal
                    ! element d(k,k) from w (potentially saves only one load))
                    call stdlib_I64_ccopy( n-k+1, w( k, k ), 1_ilp64, a( k, k ), 1_ilp64 )
                    if( k<n ) then
                       ! (note: no need to check if a(k,k) is not zero,
                        ! since that was ensured earlier in pivot search:
                        ! case a(k,k) = 0 falls into 2x2 pivot case(4))
                       r1 = one / real( a( k, k ),KIND=sp)
                       call stdlib_I64_csscal( n-k, r1, a( k+1, k ), 1_ilp64 )
                       ! (2) conjugate column w(k)
                       call stdlib_I64_clacgv( n-k, w( k+1, k ), 1_ilp64 )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! (1) store l(k+2:n,k) and l(k+2:n,k+1) and 2-by-2
                    ! block d(k:k+1,k:k+1) in columns k and k+1 of a.
                    ! (note: 2-by-2 diagonal block l(k:k+1,k:k+1) is a unit
                    ! block and not stored)
                       ! a(k:k+1,k:k+1) := d(k:k+1,k:k+1) = w(k:k+1,k:k+1)
                       ! a(k+2:n,k:k+1) := l(k+2:n,k:k+1) =
                       ! = w(k+2:n,k:k+1) * ( d(k:k+1,k:k+1)**(-1) )
                    if( k<n-1 ) then
                       ! factor out the columns of the inverse of 2-by-2 pivot
                       ! block d, so that each column contains 1, to reduce the
                       ! number of flops when we multiply panel
                       ! ( w(kw-1) w(kw) ) by this inverse, i.e. by d**(-1).
                       ! d**(-1) = ( d11 cj(d21) )**(-1) =
                                 ! ( d21    d22 )
                       ! = 1/(d11*d22-|d21|**2) * ( ( d22) (-cj(d21) ) ) =
                                                ! ( (-d21) (     d11 ) )
                       ! = 1/(|d21|**2) * 1/((d11/cj(d21))*(d22/d21)-1) *
                         ! * ( d21*( d22/d21 ) conj(d21)*(           - 1 ) ) =
                           ! (     (      -1 )           ( d11/conj(d21) ) )
                       ! = 1/(|d21|**2) * 1/(d22*d11-1) *
                         ! * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                           ! (     (  -1 )           ( d22 ) )
                       ! = (1/|d21|**2) * t * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                                            ! (     (  -1 )           ( d22 ) )
                       ! = ( (t/conj(d21))*( d11 ) (t/d21)*(  -1 ) ) =
                         ! (               (  -1 )         ( d22 ) )
                       ! = ( conj(d21)*( d11 ) d21*(  -1 ) )
                         ! (           (  -1 )     ( d22 ) )
                       ! where d11 = d22/d21,
                             ! d22 = d11/conj(d21),
                             ! d21 = t/d21,
                             ! t = 1/(d22*d11-1).
                       ! (note: no need to check for division by zero,
                        ! since that was ensured earlier in pivot search:
                        ! (a) d21 != 0, since in 2x2 pivot case(4)
                            ! |d21| should be larger than |d11| and |d22|;
                        ! (b) (d22*d11 - 1) != 0, since from (a),
                            ! both |d11| < 1, |d22| < 1, hence |d22*d11| << 1.)
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / conjg( d21 )
                       t = one / ( real( d11*d22,KIND=sp)-one )
                       d21 = t / d21
                       ! update elements in columns a(k) and a(k+1) as
                       ! dot products of rows of ( w(k) w(k+1) ) and columns
                       ! of d**(-1)
                       do j = k + 2, n
                          a( j, k ) = conjg( d21 )*( d11*w( j, k )-w( j, k+1 ) )
                          a( j, k+1 ) = d21*( d22*w( j, k+1 )-w( j, k ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = w( k+1, k )
                    a( k+1, k+1 ) = w( k+1, k+1 )
                    ! (2) conjugate columns w(k) and w(k+1)
                    call stdlib_I64_clacgv( n-k, w( k+1, k ), 1_ilp64 )
                    call stdlib_I64_clacgv( n-k-1, w( k+2, k+1 ), 1_ilp64 )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp64 ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**h = a22 - l21*w**h
              ! computing blocks of nb columns at a time (note that conjg(w) is
              ! actually stored)
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    a( jj, jj ) = real( a( jj, jj ),KIND=sp)
                    call stdlib_I64_cgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -cone,a( jj, 1_ilp64 ), lda, w( jj,&
                               1_ilp64 ), ldw, cone,a( jj, jj ), 1_ilp64 )
                    a( jj, jj ) = real( a( jj, jj ),KIND=sp)
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_I64_cgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           cone, a( j+jb, 1_ilp64 ), lda, w( j, 1_ilp64 ),ldw, cone, a( j+jb, j ), lda )
              end do
              ! put l21 in standard form by partially undoing the interchanges
              ! of rows in columns 1:k-1 looping backwards from k-1 to 1
              j = k - 1_ilp64
              120 continue
                 ! undo the interchanges (if any) of rows j and jp
                 ! at each step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp64 ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j - 1_ilp64
                 end if
                 ! (note: here, j is used to determine row length. length j
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j - 1_ilp64
                 if( jp/=jj .and. j>=1_ilp64 )call stdlib_I64_cswap( j, a( jp, 1_ilp64 ), lda, a( jj, 1_ilp64 ), lda )
                           
              if( j>=1 )go to 120
              ! set kb to the number of columns factorized
              kb = k - 1_ilp64
           end if
           return
     end subroutine stdlib_I64_clahef

     pure module subroutine stdlib_I64_zlahef( uplo, n, nb, kb, a, lda, ipiv, w, ldw, info )
     !! ZLAHEF computes a partial factorization of a complex Hermitian
     !! matrix A using the Bunch-Kaufman diagonal pivoting method. The
     !! partial factorization has the form:
     !! A  =  ( I  U12 ) ( A11  0  ) (  I      0     )  if UPLO = 'U', or:
     !! ( 0  U22 ) (  0   D  ) ( U12**H U22**H )
     !! A  =  ( L11  0 ) (  D   0  ) ( L11**H L21**H )  if UPLO = 'L'
     !! ( L21  I ) (  0  A22 ) (  0      I     )
     !! where the order of D is at most NB. The actual order is returned in
     !! the argument KB, and is either NB or NB-1, or N if N <= NB.
     !! Note that U**H denotes the conjugate transpose of U.
     !! ZLAHEF is an auxiliary routine called by ZHETRF. It uses blocked code
     !! (calling Level 3 BLAS) to update the submatrix A11 (if UPLO = 'U') or
     !! A22 (if UPLO = 'L').
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info, kb
           integer(ilp64), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: w(ldw,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           
           ! Local Scalars 
           integer(ilp64) :: imax, j, jb, jj, jmax, jp, k, kk, kkw, kp, kstep, kw
           real(dp) :: absakk, alpha, colmax, r1, rowmax, t
           complex(dp) :: d11, d21, d22, z
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( z ) = abs( real( z,KIND=dp) ) + abs( aimag( z ) )
           ! Executable Statements 
           info = 0_ilp64
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! factorize the trailing columns of a using the upper triangle
              ! of a and working backwards, and compute the matrix w = u12*d
              ! for use in updating a11 (note that conjg(w) is actually stored)
              ! k is the main loop index, decreasing from n in steps of 1 or 2
              ! kw is the column of w which corresponds to column k of a
              k = n
              10 continue
              kw = nb + k - n
              ! exit from loop
              if( ( k<=n-nb+1 .and. nb<n ) .or. k<1 )go to 30
              kstep = 1_ilp64
              ! copy column k of a to column kw of w and update it
              call stdlib_I64_zcopy( k-1, a( 1_ilp64, k ), 1_ilp64, w( 1_ilp64, kw ), 1_ilp64 )
              w( k, kw ) = real( a( k, k ),KIND=dp)
              if( k<n ) then
                 call stdlib_I64_zgemv( 'NO TRANSPOSE', k, n-k, -cone, a( 1_ilp64, k+1 ), lda,w( k, kw+1 ), &
                           ldw, cone, w( 1_ilp64, kw ), 1_ilp64 )
                 w( k, kw ) = real( w( k, kw ),KIND=dp)
              end if
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( w( k, kw ),KIND=dp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp64 ) then
                 imax = stdlib_I64_izamax( k-1, w( 1_ilp64, kw ), 1_ilp64 )
                 colmax = cabs1( w( imax, kw ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp64 )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=dp)
              else
                 ! ============================================================
                 ! begin pivot search
                 ! case(1)
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! begin pivot search along imax row
                    ! copy column imax to column kw-1 of w and update it
                    call stdlib_I64_zcopy( imax-1, a( 1_ilp64, imax ), 1_ilp64, w( 1_ilp64, kw-1 ), 1_ilp64 )
                    w( imax, kw-1 ) = real( a( imax, imax ),KIND=dp)
                    call stdlib_I64_zcopy( k-imax, a( imax, imax+1 ), lda,w( imax+1, kw-1 ), 1_ilp64 )
                              
                    call stdlib_I64_zlacgv( k-imax, w( imax+1, kw-1 ), 1_ilp64 )
                    if( k<n ) then
                       call stdlib_I64_zgemv( 'NO TRANSPOSE', k, n-k, -cone,a( 1_ilp64, k+1 ), lda, w( imax,&
                                  kw+1 ), ldw,cone, w( 1_ilp64, kw-1 ), 1_ilp64 )
                       w( imax, kw-1 ) = real( w( imax, kw-1 ),KIND=dp)
                    end if
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value.
                    ! determine only rowmax.
                    jmax = imax + stdlib_I64_izamax( k-imax, w( imax+1, kw-1 ), 1_ilp64 )
                    rowmax = cabs1( w( jmax, kw-1 ) )
                    if( imax>1_ilp64 ) then
                       jmax = stdlib_I64_izamax( imax-1, w( 1_ilp64, kw-1 ), 1_ilp64 )
                       rowmax = max( rowmax, cabs1( w( jmax, kw-1 ) ) )
                    end if
                    ! case(2)
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    ! case(3)
                    else if( abs( real( w( imax, kw-1 ),KIND=dp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column kw-1 of w to column kw of w
                       call stdlib_I64_zcopy( k, w( 1_ilp64, kw-1 ), 1_ilp64, w( 1_ilp64, kw ), 1_ilp64 )
                    ! case(4)
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp64
                    end if
                    ! end pivot search along imax row
                 end if
                 ! end pivot search
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k - kstep + 1_ilp64
                 ! kkw is the column of w which corresponds to column kk of a
                 kkw = nb + kk - n
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kkw of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k-1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = real( a( kk, kk ),KIND=dp)
                    call stdlib_I64_zcopy( kk-1-kp, a( kp+1, kk ), 1_ilp64, a( kp, kp+1 ),lda )
                    call stdlib_I64_zlacgv( kk-1-kp, a( kp, kp+1 ), lda )
                    if( kp>1_ilp64 )call stdlib_I64_zcopy( kp-1, a( 1_ilp64, kk ), 1_ilp64, a( 1_ilp64, kp ), 1_ilp64 )
                    ! interchange rows kk and kp in last k+1 to n columns of a
                    ! (columns k (or k and k-1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in last kkw to nb columns of w.
                    if( k<n )call stdlib_I64_zswap( n-k, a( kk, k+1 ), lda, a( kp, k+1 ),lda )
                    call stdlib_I64_zswap( n-kk+1, w( kk, kkw ), ldw, w( kp, kkw ),ldw )
                 end if
                 if( kstep==1_ilp64 ) then
                    ! 1-by-1 pivot block d(k): column kw of w now holds
                    ! w(kw) = u(k)*d(k),
                    ! where u(k) is the k-th column of u
                    ! (1) store subdiag. elements of column u(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! (note: diagonal element u(k,k) is a unit element
                    ! and not stored)
                       ! a(k,k) := d(k,k) = w(k,kw)
                       ! a(1:k-1,k) := u(1:k-1,k) = w(1:k-1,kw)/d(k,k)
                    ! (note: no need to use for hermitian matrix
                    ! a( k, k ) = real( w( k, k),KIND=dp) to separately copy diagonal
                    ! element d(k,k) from w (potentially saves only one load))
                    call stdlib_I64_zcopy( k, w( 1_ilp64, kw ), 1_ilp64, a( 1_ilp64, k ), 1_ilp64 )
                    if( k>1_ilp64 ) then
                       ! (note: no need to check if a(k,k) is not zero,
                        ! since that was ensured earlier in pivot search:
                        ! case a(k,k) = 0 falls into 2x2 pivot case(4))
                       r1 = one / real( a( k, k ),KIND=dp)
                       call stdlib_I64_zdscal( k-1, r1, a( 1_ilp64, k ), 1_ilp64 )
                       ! (2) conjugate column w(kw)
                       call stdlib_I64_zlacgv( k-1, w( 1_ilp64, kw ), 1_ilp64 )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns kw and kw-1 of w now hold
                    ! ( w(kw-1) w(kw) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! (1) store u(1:k-2,k-1) and u(1:k-2,k) and 2-by-2
                    ! block d(k-1:k,k-1:k) in columns k-1 and k of a.
                    ! (note: 2-by-2 diagonal block u(k-1:k,k-1:k) is a unit
                    ! block and not stored)
                       ! a(k-1:k,k-1:k) := d(k-1:k,k-1:k) = w(k-1:k,kw-1:kw)
                       ! a(1:k-2,k-1:k) := u(1:k-2,k:k-1:k) =
                       ! = w(1:k-2,kw-1:kw) * ( d(k-1:k,k-1:k)**(-1) )
                    if( k>2_ilp64 ) then
                       ! factor out the columns of the inverse of 2-by-2 pivot
                       ! block d, so that each column contains 1, to reduce the
                       ! number of flops when we multiply panel
                       ! ( w(kw-1) w(kw) ) by this inverse, i.e. by d**(-1).
                       ! d**(-1) = ( d11 cj(d21) )**(-1) =
                                 ! ( d21    d22 )
                       ! = 1/(d11*d22-|d21|**2) * ( ( d22) (-cj(d21) ) ) =
                                                ! ( (-d21) (     d11 ) )
                       ! = 1/(|d21|**2) * 1/((d11/cj(d21))*(d22/d21)-1) *
                         ! * ( d21*( d22/d21 ) conj(d21)*(           - 1 ) ) =
                           ! (     (      -1 )           ( d11/conj(d21) ) )
                       ! = 1/(|d21|**2) * 1/(d22*d11-1) *
                         ! * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                           ! (     (  -1 )           ( d22 ) )
                       ! = (1/|d21|**2) * t * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                                            ! (     (  -1 )           ( d22 ) )
                       ! = ( (t/conj(d21))*( d11 ) (t/d21)*(  -1 ) ) =
                         ! (               (  -1 )         ( d22 ) )
                       ! = ( conj(d21)*( d11 ) d21*(  -1 ) )
                         ! (           (  -1 )     ( d22 ) ),
                       ! where d11 = d22/d21,
                             ! d22 = d11/conj(d21),
                             ! d21 = t/d21,
                             ! t = 1/(d22*d11-1).
                       ! (note: no need to check for division by zero,
                        ! since that was ensured earlier in pivot search:
                        ! (a) d21 != 0, since in 2x2 pivot case(4)
                            ! |d21| should be larger than |d11| and |d22|;
                        ! (b) (d22*d11 - 1) != 0, since from (a),
                            ! both |d11| < 1, |d22| < 1, hence |d22*d11| << 1.)
                       d21 = w( k-1, kw )
                       d11 = w( k, kw ) / conjg( d21 )
                       d22 = w( k-1, kw-1 ) / d21
                       t = one / ( real( d11*d22,KIND=dp)-one )
                       d21 = t / d21
                       ! update elements in columns a(k-1) and a(k) as
                       ! dot products of rows of ( w(kw-1) w(kw) ) and columns
                       ! of d**(-1)
                       do j = 1, k - 2
                          a( j, k-1 ) = d21*( d11*w( j, kw-1 )-w( j, kw ) )
                          a( j, k ) = conjg( d21 )*( d22*w( j, kw )-w( j, kw-1 ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k-1, k-1 ) = w( k-1, kw-1 )
                    a( k-1, k ) = w( k-1, kw )
                    a( k, k ) = w( k, kw )
                    ! (2) conjugate columns w(kw) and w(kw-1)
                    call stdlib_I64_zlacgv( k-1, w( 1_ilp64, kw ), 1_ilp64 )
                    call stdlib_I64_zlacgv( k-2, w( 1_ilp64, kw-1 ), 1_ilp64 )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp64 ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
              30 continue
              ! update the upper triangle of a11 (= a(1:k,1:k)) as
              ! a11 := a11 - u12*d*u12**h = a11 - u12*w**h
              ! computing blocks of nb columns at a time (note that conjg(w) is
              ! actually stored)
              do j = ( ( k-1 ) / nb )*nb + 1, 1, -nb
                 jb = min( nb, k-j+1 )
                 ! update the upper triangle of the diagonal block
                 do jj = j, j + jb - 1
                    a( jj, jj ) = real( a( jj, jj ),KIND=dp)
                    call stdlib_I64_zgemv( 'NO TRANSPOSE', jj-j+1, n-k, -cone,a( j, k+1 ), lda, w( jj,&
                               kw+1 ), ldw, cone,a( j, jj ), 1_ilp64 )
                    a( jj, jj ) = real( a( jj, jj ),KIND=dp)
                 end do
                 ! update the rectangular superdiagonal block
                 call stdlib_I64_zgemm( 'NO TRANSPOSE', 'TRANSPOSE', j-1, jb, n-k,-cone, a( 1_ilp64, k+1 ), &
                           lda, w( j, kw+1 ), ldw,cone, a( 1_ilp64, j ), lda )
              end do
              ! put u12 in standard form by partially undoing the interchanges
              ! in columns k+1:n looping backwards from k+1 to n
              j = k + 1_ilp64
              60 continue
                 ! undo the interchanges (if any) of rows jj and jp at each
                 ! step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp64 ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j + 1_ilp64
                 end if
                 ! (note: here, j is used to determine row length. length n-j+1
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j + 1_ilp64
                 if( jp/=jj .and. j<=n )call stdlib_I64_zswap( n-j+1, a( jp, j ), lda, a( jj, j ), &
                           lda )
              if( j<n )go to 60
              ! set kb to the number of columns factorized
              kb = n - k
           else
              ! factorize the leading columns of a using the lower triangle
              ! of a and working forwards, and compute the matrix w = l21*d
              ! for use in updating a22 (note that conjg(w) is actually stored)
              ! k is the main loop index, increasing from 1 in steps of 1 or 2
              k = 1_ilp64
              70 continue
              ! exit from loop
              if( ( k>=nb .and. nb<n ) .or. k>n )go to 90
              kstep = 1_ilp64
              ! copy column k of a to column k of w and update it
              w( k, k ) = real( a( k, k ),KIND=dp)
              if( k<n )call stdlib_I64_zcopy( n-k, a( k+1, k ), 1_ilp64, w( k+1, k ), 1_ilp64 )
              call stdlib_I64_zgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp64 ), lda,w( k, 1_ilp64 ), ldw,&
                         cone, w( k, k ), 1_ilp64 )
              w( k, k ) = real( w( k, k ),KIND=dp)
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( w( k, k ),KIND=dp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_I64_izamax( n-k, w( k+1, k ), 1_ilp64 )
                 colmax = cabs1( w( imax, k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero or underflow: set info and continue
                 if( info==0_ilp64 )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=dp)
              else
                 ! ============================================================
                 ! begin pivot search
                 ! case(1)
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! begin pivot search along imax row
                    ! copy column imax to column k+1 of w and update it
                    call stdlib_I64_zcopy( imax-k, a( imax, k ), lda, w( k, k+1 ), 1_ilp64 )
                    call stdlib_I64_zlacgv( imax-k, w( k, k+1 ), 1_ilp64 )
                    w( imax, k+1 ) = real( a( imax, imax ),KIND=dp)
                    if( imax<n )call stdlib_I64_zcopy( n-imax, a( imax+1, imax ), 1_ilp64,w( imax+1, k+1 ), &
                              1_ilp64 )
                    call stdlib_I64_zgemv( 'NO TRANSPOSE', n-k+1, k-1, -cone, a( k, 1_ilp64 ),lda, w( imax, &
                              1_ilp64 ), ldw, cone, w( k, k+1 ),1_ilp64 )
                    w( imax, k+1 ) = real( w( imax, k+1 ),KIND=dp)
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value.
                    ! determine only rowmax.
                    jmax = k - 1_ilp64 + stdlib_I64_izamax( imax-k, w( k, k+1 ), 1_ilp64 )
                    rowmax = cabs1( w( jmax, k+1 ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_I64_izamax( n-imax, w( imax+1, k+1 ), 1_ilp64 )
                       rowmax = max( rowmax, cabs1( w( jmax, k+1 ) ) )
                    end if
                    ! case(2)
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    ! case(3)
                    else if( abs( real( w( imax, k+1 ),KIND=dp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                       ! copy column k+1 of w to column k of w
                       call stdlib_I64_zcopy( n-k+1, w( k, k+1 ), 1_ilp64, w( k, k ), 1_ilp64 )
                    ! case(4)
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp64
                    end if
                    ! end pivot search along imax row
                 end if
                 ! end pivot search
                 ! ============================================================
                 ! kk is the column of a where pivoting step stopped
                 kk = k + kstep - 1_ilp64
                 ! interchange rows and columns kp and kk.
                 ! updated column kp is already stored in column kk of w.
                 if( kp/=kk ) then
                    ! copy non-updated column kk to column kp of submatrix a
                    ! at step k. no need to copy element into column k
                    ! (or k and k+1 for 2-by-2 pivot) of a, since these columns
                    ! will be later overwritten.
                    a( kp, kp ) = real( a( kk, kk ),KIND=dp)
                    call stdlib_I64_zcopy( kp-kk-1, a( kk+1, kk ), 1_ilp64, a( kp, kk+1 ),lda )
                    call stdlib_I64_zlacgv( kp-kk-1, a( kp, kk+1 ), lda )
                    if( kp<n )call stdlib_I64_zcopy( n-kp, a( kp+1, kk ), 1_ilp64, a( kp+1, kp ), 1_ilp64 )
                              
                    ! interchange rows kk and kp in first k-1 columns of a
                    ! (columns k (or k and k+1 for 2-by-2 pivot) of a will be
                    ! later overwritten). interchange rows kk and kp
                    ! in first kk columns of w.
                    if( k>1_ilp64 )call stdlib_I64_zswap( k-1, a( kk, 1_ilp64 ), lda, a( kp, 1_ilp64 ), lda )
                    call stdlib_I64_zswap( kk, w( kk, 1_ilp64 ), ldw, w( kp, 1_ilp64 ), ldw )
                 end if
                 if( kstep==1_ilp64 ) then
                    ! 1-by-1 pivot block d(k): column k of w now holds
                    ! w(k) = l(k)*d(k),
                    ! where l(k) is the k-th column of l
                    ! (1) store subdiag. elements of column l(k)
                    ! and 1-by-1 block d(k) in column k of a.
                    ! (note: diagonal element l(k,k) is a unit element
                    ! and not stored)
                       ! a(k,k) := d(k,k) = w(k,k)
                       ! a(k+1:n,k) := l(k+1:n,k) = w(k+1:n,k)/d(k,k)
                    ! (note: no need to use for hermitian matrix
                    ! a( k, k ) = real( w( k, k),KIND=dp) to separately copy diagonal
                    ! element d(k,k) from w (potentially saves only one load))
                    call stdlib_I64_zcopy( n-k+1, w( k, k ), 1_ilp64, a( k, k ), 1_ilp64 )
                    if( k<n ) then
                       ! (note: no need to check if a(k,k) is not zero,
                        ! since that was ensured earlier in pivot search:
                        ! case a(k,k) = 0 falls into 2x2 pivot case(4))
                       r1 = one / real( a( k, k ),KIND=dp)
                       call stdlib_I64_zdscal( n-k, r1, a( k+1, k ), 1_ilp64 )
                       ! (2) conjugate column w(k)
                       call stdlib_I64_zlacgv( n-k, w( k+1, k ), 1_ilp64 )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 of w now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    ! (1) store l(k+2:n,k) and l(k+2:n,k+1) and 2-by-2
                    ! block d(k:k+1,k:k+1) in columns k and k+1 of a.
                    ! (note: 2-by-2 diagonal block l(k:k+1,k:k+1) is a unit
                    ! block and not stored)
                       ! a(k:k+1,k:k+1) := d(k:k+1,k:k+1) = w(k:k+1,k:k+1)
                       ! a(k+2:n,k:k+1) := l(k+2:n,k:k+1) =
                       ! = w(k+2:n,k:k+1) * ( d(k:k+1,k:k+1)**(-1) )
                    if( k<n-1 ) then
                       ! factor out the columns of the inverse of 2-by-2 pivot
                       ! block d, so that each column contains 1, to reduce the
                       ! number of flops when we multiply panel
                       ! ( w(kw-1) w(kw) ) by this inverse, i.e. by d**(-1).
                       ! d**(-1) = ( d11 cj(d21) )**(-1) =
                                 ! ( d21    d22 )
                       ! = 1/(d11*d22-|d21|**2) * ( ( d22) (-cj(d21) ) ) =
                                                ! ( (-d21) (     d11 ) )
                       ! = 1/(|d21|**2) * 1/((d11/cj(d21))*(d22/d21)-1) *
                         ! * ( d21*( d22/d21 ) conj(d21)*(           - 1 ) ) =
                           ! (     (      -1 )           ( d11/conj(d21) ) )
                       ! = 1/(|d21|**2) * 1/(d22*d11-1) *
                         ! * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                           ! (     (  -1 )           ( d22 ) )
                       ! = (1/|d21|**2) * t * ( d21*( d11 ) conj(d21)*(  -1 ) ) =
                                            ! (     (  -1 )           ( d22 ) )
                       ! = ( (t/conj(d21))*( d11 ) (t/d21)*(  -1 ) ) =
                         ! (               (  -1 )         ( d22 ) )
                       ! = ( conj(d21)*( d11 ) d21*(  -1 ) )
                         ! (           (  -1 )     ( d22 ) ),
                       ! where d11 = d22/d21,
                             ! d22 = d11/conj(d21),
                             ! d21 = t/d21,
                             ! t = 1/(d22*d11-1).
                       ! (note: no need to check for division by zero,
                        ! since that was ensured earlier in pivot search:
                        ! (a) d21 != 0, since in 2x2 pivot case(4)
                            ! |d21| should be larger than |d11| and |d22|;
                        ! (b) (d22*d11 - 1) != 0, since from (a),
                            ! both |d11| < 1, |d22| < 1, hence |d22*d11| << 1.)
                       d21 = w( k+1, k )
                       d11 = w( k+1, k+1 ) / d21
                       d22 = w( k, k ) / conjg( d21 )
                       t = one / ( real( d11*d22,KIND=dp)-one )
                       d21 = t / d21
                       ! update elements in columns a(k) and a(k+1) as
                       ! dot products of rows of ( w(k) w(k+1) ) and columns
                       ! of d**(-1)
                       do j = k + 2, n
                          a( j, k ) = conjg( d21 )*( d11*w( j, k )-w( j, k+1 ) )
                          a( j, k+1 ) = d21*( d22*w( j, k+1 )-w( j, k ) )
                       end do
                    end if
                    ! copy d(k) to a
                    a( k, k ) = w( k, k )
                    a( k+1, k ) = w( k+1, k )
                    a( k+1, k+1 ) = w( k+1, k+1 )
                    ! (2) conjugate columns w(k) and w(k+1)
                    call stdlib_I64_zlacgv( n-k, w( k+1, k ), 1_ilp64 )
                    call stdlib_I64_zlacgv( n-k-1, w( k+2, k+1 ), 1_ilp64 )
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp64 ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 70
              90 continue
              ! update the lower triangle of a22 (= a(k:n,k:n)) as
              ! a22 := a22 - l21*d*l21**h = a22 - l21*w**h
              ! computing blocks of nb columns at a time (note that conjg(w) is
              ! actually stored)
              do j = k, n, nb
                 jb = min( nb, n-j+1 )
                 ! update the lower triangle of the diagonal block
                 do jj = j, j + jb - 1
                    a( jj, jj ) = real( a( jj, jj ),KIND=dp)
                    call stdlib_I64_zgemv( 'NO TRANSPOSE', j+jb-jj, k-1, -cone,a( jj, 1_ilp64 ), lda, w( jj,&
                               1_ilp64 ), ldw, cone,a( jj, jj ), 1_ilp64 )
                    a( jj, jj ) = real( a( jj, jj ),KIND=dp)
                 end do
                 ! update the rectangular subdiagonal block
                 if( j+jb<=n )call stdlib_I64_zgemm( 'NO TRANSPOSE', 'TRANSPOSE', n-j-jb+1, jb,k-1, -&
                           cone, a( j+jb, 1_ilp64 ), lda, w( j, 1_ilp64 ),ldw, cone, a( j+jb, j ), lda )
              end do
              ! put l21 in standard form by partially undoing the interchanges
              ! of rows in columns 1:k-1 looping backwards from k-1 to 1
              j = k - 1_ilp64
              120 continue
                 ! undo the interchanges (if any) of rows jj and jp at each
                 ! step j
                 ! (here, j is a diagonal index)
                 jj = j
                 jp = ipiv( j )
                 if( jp<0_ilp64 ) then
                    jp = -jp
                    ! (here, j is a diagonal index)
                    j = j - 1_ilp64
                 end if
                 ! (note: here, j is used to determine row length. length j
                 ! of the rows to swap back doesn't include diagonal element)
                 j = j - 1_ilp64
                 if( jp/=jj .and. j>=1_ilp64 )call stdlib_I64_zswap( j, a( jp, 1_ilp64 ), lda, a( jj, 1_ilp64 ), lda )
                           
              if( j>1 )go to 120
              ! set kb to the number of columns factorized
              kb = k - 1_ilp64
           end if
           return
     end subroutine stdlib_I64_zlahef




     pure module subroutine stdlib_I64_chetf2( uplo, n, a, lda, ipiv, info )
     !! CHETF2 computes the factorization of a complex Hermitian matrix A
     !! using the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**H  or  A = L*D*L**H
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, U**H is the conjugate transpose of U, and D is
     !! Hermitian and block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, imax, j, jmax, k, kk, kp, kstep
           real(sp) :: absakk, alpha, colmax, d, d11, d22, r1, rowmax, tt
           complex(sp) :: d12, d21, t, wk, wkm1, wkp1, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHETF2', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**h using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 90
              kstep = 1_ilp64
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( a( k, k ),KIND=sp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp64 ) then
                 imax = stdlib_I64_icamax( k-1, a( 1_ilp64, k ), 1_ilp64 )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) .or. stdlib_I64_sisnan(absakk) ) then
                 ! column k is or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp64 )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=sp)
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = imax + stdlib_I64_icamax( k-imax, a( imax, imax+1 ), lda )
                    rowmax = cabs1( a( imax, jmax ) )
                    if( imax>1_ilp64 ) then
                       jmax = stdlib_I64_icamax( imax-1, a( 1_ilp64, imax ), 1_ilp64 )
                       rowmax = max( rowmax, cabs1( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( a( imax, imax ),KIND=sp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp64
                    end if
                 end if
                 kk = k - kstep + 1_ilp64
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_I64_cswap( kp-1, a( 1_ilp64, kk ), 1_ilp64, a( 1_ilp64, kp ), 1_ilp64 )
                    do j = kp + 1, kk - 1
                       t = conjg( a( j, kk ) )
                       a( j, kk ) = conjg( a( kp, j ) )
                       a( kp, j ) = t
                    end do
                    a( kp, kk ) = conjg( a( kp, kk ) )
                    r1 = real( a( kk, kk ),KIND=sp)
                    a( kk, kk ) = real( a( kp, kp ),KIND=sp)
                    a( kp, kp ) = r1
                    if( kstep==2_ilp64 ) then
                       a( k, k ) = real( a( k, k ),KIND=sp)
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 else
                    a( k, k ) = real( a( k, k ),KIND=sp)
                    if( kstep==2_ilp64 )a( k-1, k-1 ) = real( a( k-1, k-1 ),KIND=sp)
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp64 ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**h = a - w(k)*1/d(k)*w(k)**h
                    r1 = one / real( a( k, k ),KIND=sp)
                    call stdlib_I64_cher( uplo, k-1, -r1, a( 1_ilp64, k ), 1_ilp64, a, lda )
                    ! store u(k) in column k
                    call stdlib_I64_csscal( k-1, r1, a( 1_ilp64, k ), 1_ilp64 )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**h
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**h
                    if( k>2_ilp64 ) then
                       d = stdlib_I64_slapy2( real( a( k-1, k ),KIND=sp),aimag( a( k-1, k ) ) )
                                 
                       d22 = real( a( k-1, k-1 ),KIND=sp) / d
                       d11 = real( a( k, k ),KIND=sp) / d
                       tt = one / ( d11*d22-one )
                       d12 = a( k-1, k ) / d
                       d = tt / d
                       do j = k - 2, 1, -1
                          wkm1 = d*( d11*a( j, k-1 )-conjg( d12 )*a( j, k ) )
                          wk = d*( d22*a( j, k )-d12*a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - a( i, k )*conjg( wk ) -a( i, k-1 )*conjg( &
                                       wkm1 )
                          end do
                          a( j, k ) = wk
                          a( j, k-1 ) = wkm1
                          a( j, j ) = cmplx( real( a( j, j ),KIND=sp), zero,KIND=sp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp64 ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
           else
              ! factorize a as l*d*l**h using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp64
              50 continue
              ! if k > n, exit from loop
              if( k>n )go to 90
              kstep = 1_ilp64
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( a( k, k ),KIND=sp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_I64_icamax( n-k, a( k+1, k ), 1_ilp64 )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) .or. stdlib_I64_sisnan(absakk) ) then
                 ! column k is zero or underflow, contains a nan:
                 ! set info and continue
                 if( info==0_ilp64 )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=sp)
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    jmax = k - 1_ilp64 + stdlib_I64_icamax( imax-k, a( imax, k ), lda )
                    rowmax = cabs1( a( imax, jmax ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_I64_icamax( n-imax, a( imax+1, imax ), 1_ilp64 )
                       rowmax = max( rowmax, cabs1( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( a( imax, imax ),KIND=sp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp64
                    end if
                 end if
                 kk = k + kstep - 1_ilp64
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_I64_cswap( n-kp, a( kp+1, kk ), 1_ilp64, a( kp+1, kp ), 1_ilp64 )
                              
                    do j = kk + 1, kp - 1
                       t = conjg( a( j, kk ) )
                       a( j, kk ) = conjg( a( kp, j ) )
                       a( kp, j ) = t
                    end do
                    a( kp, kk ) = conjg( a( kp, kk ) )
                    r1 = real( a( kk, kk ),KIND=sp)
                    a( kk, kk ) = real( a( kp, kp ),KIND=sp)
                    a( kp, kp ) = r1
                    if( kstep==2_ilp64 ) then
                       a( k, k ) = real( a( k, k ),KIND=sp)
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 else
                    a( k, k ) = real( a( k, k ),KIND=sp)
                    if( kstep==2_ilp64 )a( k+1, k+1 ) = real( a( k+1, k+1 ),KIND=sp)
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp64 ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**h = a - w(k)*(1/d(k))*w(k)**h
                       r1 = one / real( a( k, k ),KIND=sp)
                       call stdlib_I64_cher( uplo, n-k, -r1, a( k+1, k ), 1_ilp64,a( k+1, k+1 ), lda )
                                 
                       ! store l(k) in column k
                       call stdlib_I64_csscal( n-k, r1, a( k+1, k ), 1_ilp64 )
                    end if
                 else
                    ! 2-by-2 pivot block d(k)
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**h
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**h
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d = stdlib_I64_slapy2( real( a( k+1, k ),KIND=sp),aimag( a( k+1, k ) ) )
                                 
                       d11 = real( a( k+1, k+1 ),KIND=sp) / d
                       d22 = real( a( k, k ),KIND=sp) / d
                       tt = one / ( d11*d22-one )
                       d21 = a( k+1, k ) / d
                       d =  tt / d
                       do j = k + 2, n
                          wk = d*( d11*a( j, k )-d21*a( j, k+1 ) )
                          wkp1 = d*( d22*a( j, k+1 )-conjg( d21 )*a( j, k ) )
                          do i = j, n
                             a( i, j ) = a( i, j ) - a( i, k )*conjg( wk ) -a( i, k+1 )*conjg( &
                                       wkp1 )
                          end do
                          a( j, k ) = wk
                          a( j, k+1 ) = wkp1
                          a( j, j ) = cmplx( real( a( j, j ),KIND=sp), zero,KIND=sp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp64 ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 50
           end if
           90 continue
           return
     end subroutine stdlib_I64_chetf2

     pure module subroutine stdlib_I64_zhetf2( uplo, n, a, lda, ipiv, info )
     !! ZHETF2 computes the factorization of a complex Hermitian matrix A
     !! using the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**H  or  A = L*D*L**H
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, U**H is the conjugate transpose of U, and D is
     !! Hermitian and block diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This is the unblocked version of the algorithm, calling Level 2 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, imax, j, jmax, k, kk, kp, kstep
           real(dp) :: absakk, alpha, colmax, d, d11, d22, r1, rowmax, tt
           complex(dp) :: d12, d21, t, wk, wkm1, wkp1, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHETF2', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**h using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              10 continue
              ! if k < 1, exit from loop
              if( k<1 )go to 90
              kstep = 1_ilp64
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( a( k, k ),KIND=dp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k>1_ilp64 ) then
                 imax = stdlib_I64_izamax( k-1, a( 1_ilp64, k ), 1_ilp64 )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) .or. stdlib_I64_disnan(absakk) ) then
                 ! column k is zero or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp64 )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=dp)
              else
                 ! ============================================================
                 ! test for interchange
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value.
                    ! determine only rowmax.
                    jmax = imax + stdlib_I64_izamax( k-imax, a( imax, imax+1 ), lda )
                    rowmax = cabs1( a( imax, jmax ) )
                    if( imax>1_ilp64 ) then
                       jmax = stdlib_I64_izamax( imax-1, a( 1_ilp64, imax ), 1_ilp64 )
                       rowmax = max( rowmax, cabs1( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( a( imax, imax ),KIND=dp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp64
                    end if
                 end if
                 ! ============================================================
                 kk = k - kstep + 1_ilp64
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_I64_zswap( kp-1, a( 1_ilp64, kk ), 1_ilp64, a( 1_ilp64, kp ), 1_ilp64 )
                    do j = kp + 1, kk - 1
                       t = conjg( a( j, kk ) )
                       a( j, kk ) = conjg( a( kp, j ) )
                       a( kp, j ) = t
                    end do
                    a( kp, kk ) = conjg( a( kp, kk ) )
                    r1 = real( a( kk, kk ),KIND=dp)
                    a( kk, kk ) = real( a( kp, kp ),KIND=dp)
                    a( kp, kp ) = r1
                    if( kstep==2_ilp64 ) then
                       a( k, k ) = real( a( k, k ),KIND=dp)
                       t = a( k-1, k )
                       a( k-1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 else
                    a( k, k ) = real( a( k, k ),KIND=dp)
                    if( kstep==2_ilp64 )a( k-1, k-1 ) = real( a( k-1, k-1 ),KIND=dp)
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp64 ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**h = a - w(k)*1/d(k)*w(k)**h
                    r1 = one / real( a( k, k ),KIND=dp)
                    call stdlib_I64_zher( uplo, k-1, -r1, a( 1_ilp64, k ), 1_ilp64, a, lda )
                    ! store u(k) in column k
                    call stdlib_I64_zdscal( k-1, r1, a( 1_ilp64, k ), 1_ilp64 )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**h
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**h
                    if( k>2_ilp64 ) then
                       d = stdlib_I64_dlapy2( real( a( k-1, k ),KIND=dp),aimag( a( k-1, k ) ) )
                                 
                       d22 = real( a( k-1, k-1 ),KIND=dp) / d
                       d11 = real( a( k, k ),KIND=dp) / d
                       tt = one / ( d11*d22-one )
                       d12 = a( k-1, k ) / d
                       d = tt / d
                       do j = k - 2, 1, -1
                          wkm1 = d*( d11*a( j, k-1 )-conjg( d12 )*a( j, k ) )
                          wk = d*( d22*a( j, k )-d12*a( j, k-1 ) )
                          do i = j, 1, -1
                             a( i, j ) = a( i, j ) - a( i, k )*conjg( wk ) -a( i, k-1 )*conjg( &
                                       wkm1 )
                          end do
                          a( j, k ) = wk
                          a( j, k-1 ) = wkm1
                          a( j, j ) = cmplx( real( a( j, j ),KIND=dp), zero,KIND=dp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp64 ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              go to 10
           else
              ! factorize a as l*d*l**h using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp64
              50 continue
              ! if k > n, exit from loop
              if( k>n )go to 90
              kstep = 1_ilp64
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( a( k, k ),KIND=dp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value.
              ! determine both colmax and imax.
              if( k<n ) then
                 imax = k + stdlib_I64_izamax( n-k, a( k+1, k ), 1_ilp64 )
                 colmax = cabs1( a( imax, k ) )
              else
                 colmax = zero
              end if
              if( (max( absakk, colmax )==zero) .or. stdlib_I64_disnan(absakk) ) then
                 ! column k is zero or underflow, or contains a nan:
                 ! set info and continue
                 if( info==0_ilp64 )info = k
                 kp = k
                 a( k, k ) = real( a( k, k ),KIND=dp)
              else
                 ! ============================================================
                 ! test for interchange
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value.
                    ! determine only rowmax.
                    jmax = k - 1_ilp64 + stdlib_I64_izamax( imax-k, a( imax, k ), lda )
                    rowmax = cabs1( a( imax, jmax ) )
                    if( imax<n ) then
                       jmax = imax + stdlib_I64_izamax( n-imax, a( imax+1, imax ), 1_ilp64 )
                       rowmax = max( rowmax, cabs1( a( jmax, imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( a( imax, imax ),KIND=dp) )>=alpha*rowmax )then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp64
                    end if
                 end if
                 ! ============================================================
                 kk = k + kstep - 1_ilp64
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_I64_zswap( n-kp, a( kp+1, kk ), 1_ilp64, a( kp+1, kp ), 1_ilp64 )
                              
                    do j = kk + 1, kp - 1
                       t = conjg( a( j, kk ) )
                       a( j, kk ) = conjg( a( kp, j ) )
                       a( kp, j ) = t
                    end do
                    a( kp, kk ) = conjg( a( kp, kk ) )
                    r1 = real( a( kk, kk ),KIND=dp)
                    a( kk, kk ) = real( a( kp, kp ),KIND=dp)
                    a( kp, kp ) = r1
                    if( kstep==2_ilp64 ) then
                       a( k, k ) = real( a( k, k ),KIND=dp)
                       t = a( k+1, k )
                       a( k+1, k ) = a( kp, k )
                       a( kp, k ) = t
                    end if
                 else
                    a( k, k ) = real( a( k, k ),KIND=dp)
                    if( kstep==2_ilp64 )a( k+1, k+1 ) = real( a( k+1, k+1 ),KIND=dp)
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp64 ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**h = a - w(k)*(1/d(k))*w(k)**h
                       r1 = one / real( a( k, k ),KIND=dp)
                       call stdlib_I64_zher( uplo, n-k, -r1, a( k+1, k ), 1_ilp64,a( k+1, k+1 ), lda )
                                 
                       ! store l(k) in column k
                       call stdlib_I64_zdscal( n-k, r1, a( k+1, k ), 1_ilp64 )
                    end if
                 else
                    ! 2-by-2 pivot block d(k)
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**h
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**h
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d = stdlib_I64_dlapy2( real( a( k+1, k ),KIND=dp),aimag( a( k+1, k ) ) )
                                 
                       d11 = real( a( k+1, k+1 ),KIND=dp) / d
                       d22 = real( a( k, k ),KIND=dp) / d
                       tt = one / ( d11*d22-one )
                       d21 = a( k+1, k ) / d
                       d = tt / d
                       do j = k + 2, n
                          wk = d*( d11*a( j, k )-d21*a( j, k+1 ) )
                          wkp1 = d*( d22*a( j, k+1 )-conjg( d21 )*a( j, k ) )
                          do i = j, n
                             a( i, j ) = a( i, j ) - a( i, k )*conjg( wk ) -a( i, k+1 )*conjg( &
                                       wkp1 )
                          end do
                          a( j, k ) = wk
                          a( j, k+1 ) = wkp1
                          a( j, j ) = cmplx( real( a( j, j ),KIND=dp), zero,KIND=dp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp64 ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              go to 50
           end if
           90 continue
           return
     end subroutine stdlib_I64_zhetf2




     pure module subroutine stdlib_I64_chetrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
     !! CHETRS solves a system of linear equations A*X = B with a complex
     !! Hermitian matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by CHETRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: j, k, kp
           real(sp) :: s
           complex(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHETRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**h.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_I64_cgeru( k-1, nrhs, -cone, a( 1_ilp64, k ), 1_ilp64, b( k, 1_ilp64 ), ldb,b( 1_ilp64, 1_ilp64 ),&
                     & ldb &
                           )
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=sp) / real( a( k, k ),KIND=sp)
                 call stdlib_I64_csscal( nrhs, s, b( k, 1_ilp64 ), ldb )
                 k = k - 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_I64_cswap( nrhs, b( k-1, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_I64_cgeru( k-2, nrhs, -cone, a( 1_ilp64, k ), 1_ilp64, b( k, 1_ilp64 ), ldb,b( 1_ilp64, 1_ilp64 ),&
                     & ldb &
                           )
                 call stdlib_I64_cgeru( k-2, nrhs, -cone, a( 1_ilp64, k-1 ), 1_ilp64, b( k-1, 1_ilp64 ),ldb, b( 1_ilp64, 1_ilp64&
                     & ), &
                           ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k-1, k )
                 akm1 = a( k-1, k-1 ) / akm1k
                 ak = a( k, k ) / conjg( akm1k )
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / conjg( akm1k )
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k - 2_ilp64
              end if
              go to 10
              30 continue
              ! next solve u**h *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp64
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**h(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp64, k ), &
                              1_ilp64, cone, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k = k + 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**h(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp64, k ), &
                              1_ilp64, cone, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k+1, 1_ilp64 ), ldb )
                    call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp64, k+1 )&
                              , 1_ilp64, cone, b( k+1, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k+1, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k = k + 2_ilp64
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**h.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp64
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_I64_cgeru( n-k, nrhs, -cone, a( k+1, k ), 1_ilp64, b( k, 1_ilp64 ),ldb, b( &
                           k+1, 1_ilp64 ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=sp) / real( a( k, k ),KIND=sp)
                 call stdlib_I64_csscal( nrhs, s, b( k, 1_ilp64 ), ldb )
                 k = k + 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_I64_cswap( nrhs, b( k+1, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_I64_cgeru( n-k-1, nrhs, -cone, a( k+2, k ), 1_ilp64, b( k, 1_ilp64 ),ldb, b( k+2, &
                              1_ilp64 ), ldb )
                    call stdlib_I64_cgeru( n-k-1, nrhs, -cone, a( k+2, k+1 ), 1_ilp64,b( k+1, 1_ilp64 ), ldb, b( &
                              k+2, 1_ilp64 ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k+1, k )
                 akm1 = a( k, k ) / conjg( akm1k )
                 ak = a( k+1, k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / conjg( akm1k )
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k + 2_ilp64
              end if
              go to 60
              80 continue
              ! next solve l**h *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**h(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n ) then
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp64 ), ldb, &
                              a( k+1, k ), 1_ilp64, cone,b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k = k - 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**h(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp64 ), ldb, &
                              a( k+1, k ), 1_ilp64, cone,b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k-1, 1_ilp64 ), ldb )
                    call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp64 ), ldb, &
                              a( k+1, k-1 ), 1_ilp64, cone,b( k-1, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k-1, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k = k - 2_ilp64
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_I64_chetrs

     pure module subroutine stdlib_I64_zhetrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
     !! ZHETRS solves a system of linear equations A*X = B with a complex
     !! Hermitian matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by ZHETRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: j, k, kp
           real(dp) :: s
           complex(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHETRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**h.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_I64_zgeru( k-1, nrhs, -cone, a( 1_ilp64, k ), 1_ilp64, b( k, 1_ilp64 ), ldb,b( 1_ilp64, 1_ilp64 ),&
                     & ldb &
                           )
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=dp) / real( a( k, k ),KIND=dp)
                 call stdlib_I64_zdscal( nrhs, s, b( k, 1_ilp64 ), ldb )
                 k = k - 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_I64_zswap( nrhs, b( k-1, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_I64_zgeru( k-2, nrhs, -cone, a( 1_ilp64, k ), 1_ilp64, b( k, 1_ilp64 ), ldb,b( 1_ilp64, 1_ilp64 ),&
                     & ldb &
                           )
                 call stdlib_I64_zgeru( k-2, nrhs, -cone, a( 1_ilp64, k-1 ), 1_ilp64, b( k-1, 1_ilp64 ),ldb, b( 1_ilp64, 1_ilp64&
                     & ), &
                           ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k-1, k )
                 akm1 = a( k-1, k-1 ) / akm1k
                 ak = a( k, k ) / conjg( akm1k )
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / conjg( akm1k )
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k - 2_ilp64
              end if
              go to 10
              30 continue
              ! next solve u**h *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp64
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**h(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp64, k ), &
                              1_ilp64, cone, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k = k + 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**h(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp64, k ), &
                              1_ilp64, cone, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k+1, 1_ilp64 ), ldb )
                    call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, a( 1_ilp64, k+1 )&
                              , 1_ilp64, cone, b( k+1, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k+1, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k = k + 2_ilp64
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**h.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp64
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_I64_zgeru( n-k, nrhs, -cone, a( k+1, k ), 1_ilp64, b( k, 1_ilp64 ),ldb, b( &
                           k+1, 1_ilp64 ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=dp) / real( a( k, k ),KIND=dp)
                 call stdlib_I64_zdscal( nrhs, s, b( k, 1_ilp64 ), ldb )
                 k = k + 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_I64_zswap( nrhs, b( k+1, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_I64_zgeru( n-k-1, nrhs, -cone, a( k+2, k ), 1_ilp64, b( k, 1_ilp64 ),ldb, b( k+2, &
                              1_ilp64 ), ldb )
                    call stdlib_I64_zgeru( n-k-1, nrhs, -cone, a( k+2, k+1 ), 1_ilp64,b( k+1, 1_ilp64 ), ldb, b( &
                              k+2, 1_ilp64 ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = a( k+1, k )
                 akm1 = a( k, k ) / conjg( akm1k )
                 ak = a( k+1, k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / conjg( akm1k )
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 k = k + 2_ilp64
              end if
              go to 60
              80 continue
              ! next solve l**h *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**h(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n ) then
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp64 ), ldb, &
                              a( k+1, k ), 1_ilp64, cone,b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k = k - 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**h(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp64 ), ldb, &
                              a( k+1, k ), 1_ilp64, cone,b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k-1, 1_ilp64 ), ldb )
                    call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp64 ), ldb, &
                              a( k+1, k-1 ), 1_ilp64, cone,b( k-1, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k-1, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k = k - 2_ilp64
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_I64_zhetrs




     pure module subroutine stdlib_I64_chetri( uplo, n, a, lda, ipiv, work, info )
     !! CHETRI computes the inverse of a complex Hermitian indefinite matrix
     !! A using the factorization A = U*D*U**H or A = L*D*L**H computed by
     !! CHETRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: j, k, kp, kstep
           real(sp) :: ak, akp1, d, t
           complex(sp) :: akkp1, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHETRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do info = 1, n
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           end if
           info = 0_ilp64
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp64
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / real( a( k, k ),KIND=sp)
                 ! compute column k of the inverse.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_ccopy( k-1, a( 1_ilp64, k ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_chemv( uplo, k-1, -cone, a, lda, work, 1_ilp64, czero,a( 1_ilp64, k ), 1_ilp64 )
                              
                    a( k, k ) = a( k, k ) - real( stdlib_I64_cdotc( k-1, work, 1_ilp64, a( 1_ilp64,k ), 1_ilp64 ),&
                              KIND=sp)
                 end if
                 kstep = 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k+1 ) )
                 ak = real( a( k, k ),KIND=sp) / t
                 akp1 = real( a( k+1, k+1 ),KIND=sp) / t
                 akkp1 = a( k, k+1 ) / t
                 d = t*( ak*akp1-one )
                 a( k, k ) = akp1 / d
                 a( k+1, k+1 ) = ak / d
                 a( k, k+1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_ccopy( k-1, a( 1_ilp64, k ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_chemv( uplo, k-1, -cone, a, lda, work, 1_ilp64, czero,a( 1_ilp64, k ), 1_ilp64 )
                              
                    a( k, k ) = a( k, k ) - real( stdlib_I64_cdotc( k-1, work, 1_ilp64, a( 1_ilp64,k ), 1_ilp64 ),&
                              KIND=sp)
                    a( k, k+1 ) = a( k, k+1 ) -stdlib_I64_cdotc( k-1, a( 1_ilp64, k ), 1_ilp64, a( 1_ilp64, k+1 ), 1_ilp64 )
                              
                    call stdlib_I64_ccopy( k-1, a( 1_ilp64, k+1 ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_chemv( uplo, k-1, -cone, a, lda, work, 1_ilp64, czero,a( 1_ilp64, k+1 ), 1_ilp64 )
                              
                    a( k+1, k+1 ) = a( k+1, k+1 ) -real( stdlib_I64_cdotc( k-1, work, 1_ilp64, a( 1_ilp64, k+1 ),&
                              1_ilp64 ),KIND=sp)
                 end if
                 kstep = 2_ilp64
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 call stdlib_I64_cswap( kp-1, a( 1_ilp64, k ), 1_ilp64, a( 1_ilp64, kp ), 1_ilp64 )
                 do j = kp + 1, k - 1
                    temp = conjg( a( j, k ) )
                    a( j, k ) = conjg( a( kp, j ) )
                    a( kp, j ) = temp
                 end do
                 a( kp, k ) = conjg( a( kp, k ) )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp64 ) then
                    temp = a( k, k+1 )
                    a( k, k+1 ) = a( kp, k+1 )
                    a( kp, k+1 ) = temp
                 end if
              end if
              k = k + kstep
              go to 30
              50 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              60 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 80
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / real( a( k, k ),KIND=sp)
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_I64_ccopy( n-k, a( k+1, k ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_chemv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work,1_ilp64, czero, a( k+&
                              1_ilp64, k ), 1_ilp64 )
                    a( k, k ) = a( k, k ) - real( stdlib_I64_cdotc( n-k, work, 1_ilp64,a( k+1, k ), 1_ilp64 ),&
                              KIND=sp)
                 end if
                 kstep = 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k-1 ) )
                 ak = real( a( k-1, k-1 ),KIND=sp) / t
                 akp1 = real( a( k, k ),KIND=sp) / t
                 akkp1 = a( k, k-1 ) / t
                 d = t*( ak*akp1-one )
                 a( k-1, k-1 ) = akp1 / d
                 a( k, k ) = ak / d
                 a( k, k-1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_I64_ccopy( n-k, a( k+1, k ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_chemv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work,1_ilp64, czero, a( k+&
                              1_ilp64, k ), 1_ilp64 )
                    a( k, k ) = a( k, k ) - real( stdlib_I64_cdotc( n-k, work, 1_ilp64,a( k+1, k ), 1_ilp64 ),&
                              KIND=sp)
                    a( k, k-1 ) = a( k, k-1 ) -stdlib_I64_cdotc( n-k, a( k+1, k ), 1_ilp64, a( k+1, k-1 ),1_ilp64 &
                              )
                    call stdlib_I64_ccopy( n-k, a( k+1, k-1 ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_chemv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work,1_ilp64, czero, a( k+&
                              1_ilp64, k-1 ), 1_ilp64 )
                    a( k-1, k-1 ) = a( k-1, k-1 ) -real( stdlib_I64_cdotc( n-k, work, 1_ilp64, a( k+1, k-1 )&
                              ,1_ilp64 ),KIND=sp)
                 end if
                 kstep = 2_ilp64
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 if( kp<n )call stdlib_I64_cswap( n-kp, a( kp+1, k ), 1_ilp64, a( kp+1, kp ), 1_ilp64 )
                 do j = k + 1, kp - 1
                    temp = conjg( a( j, k ) )
                    a( j, k ) = conjg( a( kp, j ) )
                    a( kp, j ) = temp
                 end do
                 a( kp, k ) = conjg( a( kp, k ) )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp64 ) then
                    temp = a( k, k-1 )
                    a( k, k-1 ) = a( kp, k-1 )
                    a( kp, k-1 ) = temp
                 end if
              end if
              k = k - kstep
              go to 60
              80 continue
           end if
           return
     end subroutine stdlib_I64_chetri

     pure module subroutine stdlib_I64_zhetri( uplo, n, a, lda, ipiv, work, info )
     !! ZHETRI computes the inverse of a complex Hermitian indefinite matrix
     !! A using the factorization A = U*D*U**H or A = L*D*L**H computed by
     !! ZHETRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, n
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: j, k, kp, kstep
           real(dp) :: ak, akp1, d, t
           complex(dp) :: akkp1, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHETRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              do info = 1, n
                 if( ipiv( info )>0 .and. a( info, info )==czero )return
              end do
           end if
           info = 0_ilp64
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp64
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / real( a( k, k ),KIND=dp)
                 ! compute column k of the inverse.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_zcopy( k-1, a( 1_ilp64, k ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_zhemv( uplo, k-1, -cone, a, lda, work, 1_ilp64, czero,a( 1_ilp64, k ), 1_ilp64 )
                              
                    a( k, k ) = a( k, k ) - real( stdlib_I64_zdotc( k-1, work, 1_ilp64, a( 1_ilp64,k ), 1_ilp64 ),&
                              KIND=dp)
                 end if
                 kstep = 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k+1 ) )
                 ak = real( a( k, k ),KIND=dp) / t
                 akp1 = real( a( k+1, k+1 ),KIND=dp) / t
                 akkp1 = a( k, k+1 ) / t
                 d = t*( ak*akp1-one )
                 a( k, k ) = akp1 / d
                 a( k+1, k+1 ) = ak / d
                 a( k, k+1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_zcopy( k-1, a( 1_ilp64, k ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_zhemv( uplo, k-1, -cone, a, lda, work, 1_ilp64, czero,a( 1_ilp64, k ), 1_ilp64 )
                              
                    a( k, k ) = a( k, k ) - real( stdlib_I64_zdotc( k-1, work, 1_ilp64, a( 1_ilp64,k ), 1_ilp64 ),&
                              KIND=dp)
                    a( k, k+1 ) = a( k, k+1 ) -stdlib_I64_zdotc( k-1, a( 1_ilp64, k ), 1_ilp64, a( 1_ilp64, k+1 ), 1_ilp64 )
                              
                    call stdlib_I64_zcopy( k-1, a( 1_ilp64, k+1 ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_zhemv( uplo, k-1, -cone, a, lda, work, 1_ilp64, czero,a( 1_ilp64, k+1 ), 1_ilp64 )
                              
                    a( k+1, k+1 ) = a( k+1, k+1 ) -real( stdlib_I64_zdotc( k-1, work, 1_ilp64, a( 1_ilp64, k+1 ),&
                              1_ilp64 ),KIND=dp)
                 end if
                 kstep = 2_ilp64
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 call stdlib_I64_zswap( kp-1, a( 1_ilp64, k ), 1_ilp64, a( 1_ilp64, kp ), 1_ilp64 )
                 do j = kp + 1, k - 1
                    temp = conjg( a( j, k ) )
                    a( j, k ) = conjg( a( kp, j ) )
                    a( kp, j ) = temp
                 end do
                 a( kp, k ) = conjg( a( kp, k ) )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp64 ) then
                    temp = a( k, k+1 )
                    a( k, k+1 ) = a( kp, k+1 )
                    a( kp, k+1 ) = temp
                 end if
              end if
              k = k + kstep
              go to 30
              50 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              60 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 80
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 a( k, k ) = one / real( a( k, k ),KIND=dp)
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_I64_zcopy( n-k, a( k+1, k ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_zhemv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work,1_ilp64, czero, a( k+&
                              1_ilp64, k ), 1_ilp64 )
                    a( k, k ) = a( k, k ) - real( stdlib_I64_zdotc( n-k, work, 1_ilp64,a( k+1, k ), 1_ilp64 ),&
                              KIND=dp)
                 end if
                 kstep = 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( a( k, k-1 ) )
                 ak = real( a( k-1, k-1 ),KIND=dp) / t
                 akp1 = real( a( k, k ),KIND=dp) / t
                 akkp1 = a( k, k-1 ) / t
                 d = t*( ak*akp1-one )
                 a( k-1, k-1 ) = akp1 / d
                 a( k, k ) = ak / d
                 a( k, k-1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_I64_zcopy( n-k, a( k+1, k ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_zhemv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work,1_ilp64, czero, a( k+&
                              1_ilp64, k ), 1_ilp64 )
                    a( k, k ) = a( k, k ) - real( stdlib_I64_zdotc( n-k, work, 1_ilp64,a( k+1, k ), 1_ilp64 ),&
                              KIND=dp)
                    a( k, k-1 ) = a( k, k-1 ) -stdlib_I64_zdotc( n-k, a( k+1, k ), 1_ilp64, a( k+1, k-1 ),1_ilp64 &
                              )
                    call stdlib_I64_zcopy( n-k, a( k+1, k-1 ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_zhemv( uplo, n-k, -cone, a( k+1, k+1 ), lda, work,1_ilp64, czero, a( k+&
                              1_ilp64, k-1 ), 1_ilp64 )
                    a( k-1, k-1 ) = a( k-1, k-1 ) -real( stdlib_I64_zdotc( n-k, work, 1_ilp64, a( k+1, k-1 )&
                              ,1_ilp64 ),KIND=dp)
                 end if
                 kstep = 2_ilp64
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 if( kp<n )call stdlib_I64_zswap( n-kp, a( kp+1, k ), 1_ilp64, a( kp+1, kp ), 1_ilp64 )
                 do j = k + 1, kp - 1
                    temp = conjg( a( j, k ) )
                    a( j, k ) = conjg( a( kp, j ) )
                    a( kp, j ) = temp
                 end do
                 a( kp, k ) = conjg( a( kp, k ) )
                 temp = a( k, k )
                 a( k, k ) = a( kp, kp )
                 a( kp, kp ) = temp
                 if( kstep==2_ilp64 ) then
                    temp = a( k, k-1 )
                    a( k, k-1 ) = a( kp, k-1 )
                    a( kp, k-1 ) = temp
                 end if
              end if
              k = k - kstep
              go to 60
              80 continue
           end if
           return
     end subroutine stdlib_I64_zhetri




     pure module subroutine stdlib_I64_cherfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb,x, ldx, ferr, &
     !! CHERFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is Hermitian indefinite, and
     !! provides error bounds and backward error estimates for the solution.
               berr, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: itmax = 5_ilp64
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: count, i, j, k, kase, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -10_ilp64
           else if( ldx<max( 1_ilp64, n ) ) then
              info = -12_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHERFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 .or. nrhs==0_ilp64 ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp64
           eps = stdlib_I64_slamch( 'EPSILON' )
           safmin = stdlib_I64_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp64
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_I64_ccopy( n, b( 1_ilp64, j ), 1_ilp64, work, 1_ilp64 )
              call stdlib_I64_chemv( uplo, n, -cone, a, lda, x( 1_ilp64, j ), 1_ilp64, cone, work, 1_ilp64 )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    do i = 1, k - 1
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + abs( real( a( k, k ),KIND=sp) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + abs( real( a( k, k ),KIND=sp) )*xk
                    do i = k + 1, n
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_I64_chetrs( uplo, n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                 call stdlib_I64_caxpy( n, cone, work, 1_ilp64, x( 1_ilp64, j ), 1_ilp64 )
                 lstres = berr( j )
                 count = count + 1_ilp64
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_clacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp64
              100 continue
              call stdlib_I64_clacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp64 ) then
                 if( kase==1_ilp64 ) then
                    ! multiply by diag(w)*inv(a**h).
                    call stdlib_I64_chetrs( uplo, n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp64 ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_I64_chetrs( uplo, n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_I64_cherfs

     pure module subroutine stdlib_I64_zherfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb,x, ldx, ferr, &
     !! ZHERFS improves the computed solution to a system of linear
     !! equations when the coefficient matrix is Hermitian indefinite, and
     !! provides error bounds and backward error estimates for the solution.
               berr, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: itmax = 5_ilp64
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: count, i, j, k, kase, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -10_ilp64
           else if( ldx<max( 1_ilp64, n ) ) then
              info = -12_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHERFS', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 .or. nrhs==0_ilp64 ) then
              do j = 1, nrhs
                 ferr( j ) = zero
                 berr( j ) = zero
              end do
              return
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp64
           eps = stdlib_I64_dlamch( 'EPSILON' )
           safmin = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_140: do j = 1, nrhs
              count = 1_ilp64
              lstres = three
              20 continue
              ! loop until stopping criterion is satisfied.
              ! compute residual r = b - a * x
              call stdlib_I64_zcopy( n, b( 1_ilp64, j ), 1_ilp64, work, 1_ilp64 )
              call stdlib_I64_zhemv( uplo, n, -cone, a, lda, x( 1_ilp64, j ), 1_ilp64, cone, work, 1_ilp64 )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(a)*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              ! compute abs(a)*abs(x) + abs(b).
              if( upper ) then
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    do i = 1, k - 1
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + abs( real( a( k, k ),KIND=dp) )*xk + s
                 end do
              else
                 do k = 1, n
                    s = zero
                    xk = cabs1( x( k, j ) )
                    rwork( k ) = rwork( k ) + abs( real( a( k, k ),KIND=dp) )*xk
                    do i = k + 1, n
                       rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                       s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                    end do
                    rwork( k ) = rwork( k ) + s
                 end do
              end if
              s = zero
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    s = max( s, cabs1( work( i ) ) / rwork( i ) )
                 else
                    s = max( s, ( cabs1( work( i ) )+safe1 ) /( rwork( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! test stopping criterion. continue iterating if
                 ! 1) the residual berr(j) is larger than machine epsilon, and
                 ! 2) berr(j) decreased by at least a factor of 2 during the
                    ! last iteration, and
                 ! 3) at most itmax iterations tried.
              if( berr( j )>eps .and. two*berr( j )<=lstres .and.count<=itmax ) then
                 ! update solution and try again.
                 call stdlib_I64_zhetrs( uplo, n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                 call stdlib_I64_zaxpy( n, cone, work, 1_ilp64, x( 1_ilp64, j ), 1_ilp64 )
                 lstres = berr( j )
                 count = count + 1_ilp64
                 go to 20
              end if
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(a))*
                 ! ( abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(a) is the inverse of a
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(a)*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(a)*abs(x) + abs(b) is less than safe2.
              ! use stdlib_zlacn2 to estimate the infinity-norm of the matrix
                 ! inv(a) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(a)*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp64
              100 continue
              call stdlib_I64_zlacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp64 ) then
                 if( kase==1_ilp64 ) then
                    ! multiply by diag(w)*inv(a**h).
                    call stdlib_I64_zhetrs( uplo, n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else if( kase==2_ilp64 ) then
                    ! multiply by inv(a)*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_I64_zhetrs( uplo, n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                 end if
                 go to 100
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_140
           return
     end subroutine stdlib_I64_zherfs




     pure module subroutine stdlib_I64_cheequb( uplo, n, a, lda, s, scond, amax, work, info )
     !! CHEEQUB computes row and column scalings intended to equilibrate a
     !! Hermitian matrix A (with respect to the Euclidean norm) and reduce
     !! its condition number. The scale factors S are computed by the BIN
     !! algorithm (see references) so that the scaled matrix B with elements
     !! B(i,j) = S(i)*A(i,j)*S(j) has a condition number within a factor N of
     !! the smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, n
           real(sp), intent(out) :: amax, scond
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(out) :: s(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: max_iter = 100_ilp64
           
           
           ! Local Scalars 
           integer(ilp64) :: i, j, iter
           real(sp) :: avg, std, tol, c0, c1, c2, t, u, si, d, base, smin, smax, smlnum, bignum, &
                     scale, sumsq
           logical(lk) :: up
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if ( .not. ( stdlib_lsame( uplo, 'U' ) .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -1_ilp64
           else if ( n < 0_ilp64 ) then
              info = -2_ilp64
           else if ( lda < max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if ( info /= 0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHEEQUB', -info )
              return
           end if
           up = stdlib_lsame( uplo, 'U' )
           amax = zero
           ! quick return if possible.
           if ( n == 0_ilp64 ) then
              scond = one
              return
           end if
           do i = 1, n
              s( i ) = zero
           end do
           amax = zero
           if ( up ) then
              do j = 1, n
                 do i = 1, j-1
                    s( i ) = max( s( i ), cabs1( a( i, j ) ) )
                    s( j ) = max( s( j ), cabs1( a( i, j ) ) )
                    amax = max( amax, cabs1( a( i, j ) ) )
                 end do
                 s( j ) = max( s( j ), cabs1( a( j, j ) ) )
                 amax = max( amax, cabs1( a( j, j ) ) )
              end do
           else
              do j = 1, n
                 s( j ) = max( s( j ), cabs1( a( j, j ) ) )
                 amax = max( amax, cabs1( a( j, j ) ) )
                 do i = j+1, n
                    s( i ) = max( s( i ), cabs1( a( i, j ) ) )
                    s( j ) = max( s( j ), cabs1( a( i, j ) ) )
                    amax = max( amax, cabs1( a( i, j ) ) )
                 end do
              end do
           end if
           do j = 1, n
              s( j ) = one / s( j )
           end do
           tol = one / sqrt( two * n )
           do iter = 1, max_iter
              scale = zero
              sumsq = zero
              ! beta = |a|s
              do i = 1, n
                 work( i ) = zero
              end do
              if ( up ) then
                 do j = 1, n
                    do i = 1, j-1
                       work( i ) = work( i ) + cabs1( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + cabs1( a( i, j ) ) * s( i )
                    end do
                    work( j ) = work( j ) + cabs1( a( j, j ) ) * s( j )
                 end do
              else
                 do j = 1, n
                    work( j ) = work( j ) + cabs1( a( j, j ) ) * s( j )
                    do i = j+1, n
                       work( i ) = work( i ) + cabs1( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + cabs1( a( i, j ) ) * s( i )
                    end do
                 end do
              end if
              ! avg = s^t beta / n
              avg = zero
              do i = 1, n
                 avg = avg + real( s( i )*work( i ),KIND=sp)
              end do
              avg = avg / n
              std = zero
              do i = n+1, 2*n
                 work( i ) = s( i-n ) * work( i-n ) - avg
              end do
              call stdlib_I64_classq( n, work( n+1 ), 1_ilp64, scale, sumsq )
              std = scale * sqrt( sumsq / n )
              if ( std < tol * avg ) goto 999
              do i = 1, n
                 t = cabs1( a( i, i ) )
                 si = s( i )
                 c2 = ( n-1 ) * t
                 c1 = real( ( n-2 ) * ( work( i ) - t*si ),KIND=sp)
                 c0 = real( -(t*si)*si + 2_ilp64*work( i )*si - n*avg,KIND=sp)
                 d = c1*c1 - 4_ilp64*c0*c2
                 if ( d <= 0_ilp64 ) then
                    info = -1_ilp64
                    return
                 end if
                 si = -2_ilp64*c0 / ( c1 + sqrt( d ) )
                 d = si - s( i )
                 u = zero
                 if ( up ) then
                    do j = 1, i
                       t = cabs1( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = cabs1( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 else
                    do j = 1, i
                       t = cabs1( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = cabs1( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 end if
                 avg = avg + real( ( u + work( i ) ) * d / n,KIND=sp)
                 s( i ) = si
              end do
           end do
           999 continue
           smlnum = stdlib_I64_slamch( 'SAFEMIN' )
           bignum = one / smlnum
           smin = bignum
           smax = zero
           t = one / sqrt( avg )
           base = stdlib_I64_slamch( 'B' )
           u = one / log( base )
           do i = 1, n
              s( i ) = base ** int( u * log( s( i ) * t ),KIND=ilp64)
              smin = min( smin, s( i ) )
              smax = max( smax, s( i ) )
           end do
           scond = max( smin, smlnum ) / min( smax, bignum )
     end subroutine stdlib_I64_cheequb

     pure module subroutine stdlib_I64_zheequb( uplo, n, a, lda, s, scond, amax, work, info )
     !! ZHEEQUB computes row and column scalings intended to equilibrate a
     !! Hermitian matrix A (with respect to the Euclidean norm) and reduce
     !! its condition number. The scale factors S are computed by the BIN
     !! algorithm (see references) so that the scaled matrix B with elements
     !! B(i,j) = S(i)*A(i,j)*S(j) has a condition number within a factor N of
     !! the smallest possible condition number over all possible diagonal
     !! scalings.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, n
           real(dp), intent(out) :: amax, scond
           character, intent(in) :: uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(out) :: s(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: max_iter = 100_ilp64
           
           
           ! Local Scalars 
           integer(ilp64) :: i, j, iter
           real(dp) :: avg, std, tol, c0, c1, c2, t, u, si, d, base, smin, smax, smlnum, bignum, &
                     scale, sumsq
           logical(lk) :: up
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if ( .not. ( stdlib_lsame( uplo, 'U' ) .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -1_ilp64
           else if ( n < 0_ilp64 ) then
              info = -2_ilp64
           else if ( lda < max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if ( info /= 0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHEEQUB', -info )
              return
           end if
           up = stdlib_lsame( uplo, 'U' )
           amax = zero
           ! quick return if possible.
           if ( n == 0_ilp64 ) then
              scond = one
              return
           end if
           do i = 1, n
              s( i ) = zero
           end do
           amax = zero
           if ( up ) then
              do j = 1, n
                 do i = 1, j-1
                    s( i ) = max( s( i ), cabs1( a( i, j ) ) )
                    s( j ) = max( s( j ), cabs1( a( i, j ) ) )
                    amax = max( amax, cabs1( a( i, j ) ) )
                 end do
                 s( j ) = max( s( j ), cabs1( a( j, j ) ) )
                 amax = max( amax, cabs1( a( j, j ) ) )
              end do
           else
              do j = 1, n
                 s( j ) = max( s( j ), cabs1( a( j, j ) ) )
                 amax = max( amax, cabs1( a( j, j ) ) )
                 do i = j+1, n
                    s( i ) = max( s( i ), cabs1( a( i, j ) ) )
                    s( j ) = max( s( j ), cabs1( a( i, j ) ) )
                    amax = max( amax, cabs1( a( i, j ) ) )
                 end do
              end do
           end if
           do j = 1, n
              s( j ) = one / s( j )
           end do
           tol = one / sqrt( two * n )
           do iter = 1, max_iter
              scale = zero
              sumsq = zero
              ! beta = |a|s
              do i = 1, n
                 work( i ) = zero
              end do
              if ( up ) then
                 do j = 1, n
                    do i = 1, j-1
                       work( i ) = work( i ) + cabs1( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + cabs1( a( i, j ) ) * s( i )
                    end do
                    work( j ) = work( j ) + cabs1( a( j, j ) ) * s( j )
                 end do
              else
                 do j = 1, n
                    work( j ) = work( j ) + cabs1( a( j, j ) ) * s( j )
                    do i = j+1, n
                       work( i ) = work( i ) + cabs1( a( i, j ) ) * s( j )
                       work( j ) = work( j ) + cabs1( a( i, j ) ) * s( i )
                    end do
                 end do
              end if
              ! avg = s^t beta / n
              avg = zero
              do i = 1, n
                 avg = avg + real( s( i )*work( i ),KIND=dp)
              end do
              avg = avg / n
              std = zero
              do i = n+1, 2*n
                 work( i ) = s( i-n ) * work( i-n ) - avg
              end do
              call stdlib_I64_zlassq( n, work( n+1 ), 1_ilp64, scale, sumsq )
              std = scale * sqrt( sumsq / n )
              if ( std < tol * avg ) goto 999
              do i = 1, n
                 t = cabs1( a( i, i ) )
                 si = s( i )
                 c2 = ( n-1 ) * t
                 c1 = ( n-2 ) * ( real( work( i ),KIND=dp) - t*si )
                 c0 = -(t*si)*si + 2_ilp64 * real( work( i ),KIND=dp) * si - n*avg
                 d = c1*c1 - 4_ilp64*c0*c2
                 if ( d <= 0_ilp64 ) then
                    info = -1_ilp64
                    return
                 end if
                 si = -2_ilp64*c0 / ( c1 + sqrt( d ) )
                 d = si - s( i )
                 u = zero
                 if ( up ) then
                    do j = 1, i
                       t = cabs1( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = cabs1( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 else
                    do j = 1, i
                       t = cabs1( a( i, j ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                    do j = i+1,n
                       t = cabs1( a( j, i ) )
                       u = u + s( j )*t
                       work( j ) = work( j ) + d*t
                    end do
                 end if
                 avg = avg + ( u + real( work( i ),KIND=dp) ) * d / n
                 s( i ) = si
              end do
           end do
           999 continue
           smlnum = stdlib_I64_dlamch( 'SAFEMIN' )
           bignum = one / smlnum
           smin = bignum
           smax = zero
           t = one / sqrt( avg )
           base = stdlib_I64_dlamch( 'B' )
           u = one / log( base )
           do i = 1, n
              s( i ) = base ** int( u * log( s( i ) * t ),KIND=ilp64)
              smin = min( smin, s( i ) )
              smax = max( smax, s( i ) )
           end do
           scond = max( smin, smlnum ) / min( smax, bignum )
     end subroutine stdlib_I64_zheequb




     pure module subroutine stdlib_I64_chetrs2( uplo, n, nrhs, a, lda, ipiv, b, ldb,work, info )
     !! CHETRS2 solves a system of linear equations A*X = B with a complex
     !! Hermitian matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by CHETRF and converted by CSYCONV.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, iinfo, j, k, kp
           real(sp) :: s
           complex(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHETRS2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! convert a
           call stdlib_I64_csyconv( uplo, 'C', n, a, lda, ipiv, work, iinfo )
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**h.
             ! p**t * b
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp==-ipiv( k-1 ) )call stdlib_I64_cswap( nrhs, b( k-1, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb &
                           )
                 k=k-2
              end if
             end do
        ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
             call stdlib_I64_ctrsm('L','U','N','U',n,nrhs,cone,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i=n
              do while ( i >= 1 )
                 if( ipiv(i) > 0_ilp64 ) then
                   s = real( cone,KIND=sp) / real( a( i, i ),KIND=sp)
                   call stdlib_I64_csscal( nrhs, s, b( i, 1_ilp64 ), ldb )
                 elseif ( i > 1_ilp64) then
                    if ( ipiv(i-1) == ipiv(i) ) then
                       akm1k = work(i)
                       akm1 = a( i-1, i-1 ) / akm1k
                       ak = a( i, i ) / conjg( akm1k )
                       denom = akm1*ak - cone
                       do j = 1, nrhs
                          bkm1 = b( i-1, j ) / akm1k
                          bk = b( i, j ) / conjg( akm1k )
                          b( i-1, j ) = ( ak*bkm1-bk ) / denom
                          b( i, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                    i = i - 1_ilp64
                    endif
                 endif
                 i = i - 1_ilp64
              end do
            ! compute (u**h \ b) -> b   [ u**h \ (d \ (u \p**t * b) ) ]
              call stdlib_I64_ctrsm('L','U','C','U',n,nrhs,cone,a,lda,b,ldb)
             ! p * b  [ p * (u**h \ (d \ (u \p**t * b) )) ]
             k=1_ilp64
             do while ( k <= n )
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k < n .and. kp==-ipiv( k+1 ) )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp,&
                            1_ilp64 ), ldb )
                 k=k+2
              endif
             end do
           else
              ! solve a*x = b, where a = l*d*l**h.
             ! p**t * b
             k=1_ilp64
             do while ( k <= n )
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k+1).
                 kp = -ipiv( k+1 )
                 if( kp==-ipiv( k ) )call stdlib_I64_cswap( nrhs, b( k+1, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                           
                 k=k+2
              endif
             end do
        ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
             call stdlib_I64_ctrsm('L','L','N','U',n,nrhs,cone,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i=1_ilp64
              do while ( i <= n )
                 if( ipiv(i) > 0_ilp64 ) then
                   s = real( cone,KIND=sp) / real( a( i, i ),KIND=sp)
                   call stdlib_I64_csscal( nrhs, s, b( i, 1_ilp64 ), ldb )
                 else
                       akm1k = work(i)
                       akm1 = a( i, i ) / conjg( akm1k )
                       ak = a( i+1, i+1 ) / akm1k
                       denom = akm1*ak - cone
                       do j = 1, nrhs
                          bkm1 = b( i, j ) / conjg( akm1k )
                          bk = b( i+1, j ) / akm1k
                          b( i, j ) = ( ak*bkm1-bk ) / denom
                          b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                       i = i + 1_ilp64
                 endif
                 i = i + 1_ilp64
              end do
        ! compute (l**h \ b) -> b   [ l**h \ (d \ (l \p**t * b) ) ]
             call stdlib_I64_ctrsm('L','L','C','U',n,nrhs,cone,a,lda,b,ldb)
             ! p * b  [ p * (l**h \ (d \ (l \p**t * b) )) ]
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k>1_ilp64 .and. kp==-ipiv( k-1 ) )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, &
                           1_ilp64 ), ldb )
                 k=k-2
              endif
             end do
           end if
           ! revert a
           call stdlib_I64_csyconv( uplo, 'R', n, a, lda, ipiv, work, iinfo )
           return
     end subroutine stdlib_I64_chetrs2

     pure module subroutine stdlib_I64_zhetrs2( uplo, n, nrhs, a, lda, ipiv, b, ldb,work, info )
     !! ZHETRS2 solves a system of linear equations A*X = B with a complex
     !! Hermitian matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by ZHETRF and converted by ZSYCONV.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, iinfo, j, k, kp
           real(dp) :: s
           complex(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHETRS2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           ! convert a
           call stdlib_I64_zsyconv( uplo, 'C', n, a, lda, ipiv, work, iinfo )
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**h.
             ! p**t * b
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp==-ipiv( k-1 ) )call stdlib_I64_zswap( nrhs, b( k-1, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb &
                           )
                 k=k-2
              end if
             end do
        ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
             call stdlib_I64_ztrsm('L','U','N','U',n,nrhs,cone,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i=n
              do while ( i >= 1 )
                 if( ipiv(i) > 0_ilp64 ) then
                   s = real( cone,KIND=dp) / real( a( i, i ),KIND=dp)
                   call stdlib_I64_zdscal( nrhs, s, b( i, 1_ilp64 ), ldb )
                 elseif ( i > 1_ilp64) then
                    if ( ipiv(i-1) == ipiv(i) ) then
                       akm1k = work(i)
                       akm1 = a( i-1, i-1 ) / akm1k
                       ak = a( i, i ) / conjg( akm1k )
                       denom = akm1*ak - cone
                       do j = 1, nrhs
                          bkm1 = b( i-1, j ) / akm1k
                          bk = b( i, j ) / conjg( akm1k )
                          b( i-1, j ) = ( ak*bkm1-bk ) / denom
                          b( i, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                    i = i - 1_ilp64
                    endif
                 endif
                 i = i - 1_ilp64
              end do
            ! compute (u**h \ b) -> b   [ u**h \ (d \ (u \p**t * b) ) ]
              call stdlib_I64_ztrsm('L','U','C','U',n,nrhs,cone,a,lda,b,ldb)
             ! p * b  [ p * (u**h \ (d \ (u \p**t * b) )) ]
             k=1_ilp64
             do while ( k <= n )
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k < n .and. kp==-ipiv( k+1 ) )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp,&
                            1_ilp64 ), ldb )
                 k=k+2
              endif
             end do
           else
              ! solve a*x = b, where a = l*d*l**h.
             ! p**t * b
             k=1_ilp64
             do while ( k <= n )
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k=k+1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k and -ipiv(k+1).
                 kp = -ipiv( k+1 )
                 if( kp==-ipiv( k ) )call stdlib_I64_zswap( nrhs, b( k+1, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                           
                 k=k+2
              endif
             end do
        ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
             call stdlib_I64_ztrsm('L','L','N','U',n,nrhs,cone,a,lda,b,ldb)
        ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i=1_ilp64
              do while ( i <= n )
                 if( ipiv(i) > 0_ilp64 ) then
                   s = real( cone,KIND=dp) / real( a( i, i ),KIND=dp)
                   call stdlib_I64_zdscal( nrhs, s, b( i, 1_ilp64 ), ldb )
                 else
                       akm1k = work(i)
                       akm1 = a( i, i ) / conjg( akm1k )
                       ak = a( i+1, i+1 ) / akm1k
                       denom = akm1*ak - cone
                       do j = 1, nrhs
                          bkm1 = b( i, j ) / conjg( akm1k )
                          bk = b( i+1, j ) / akm1k
                          b( i, j ) = ( ak*bkm1-bk ) / denom
                          b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                       end do
                       i = i + 1_ilp64
                 endif
                 i = i + 1_ilp64
              end do
        ! compute (l**h \ b) -> b   [ l**h \ (d \ (l \p**t * b) ) ]
             call stdlib_I64_ztrsm('L','L','C','U',n,nrhs,cone,a,lda,b,ldb)
             ! p * b  [ p * (l**h \ (d \ (l \p**t * b) )) ]
             k=n
             do while ( k >= 1 )
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k=k-1
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( k>1_ilp64 .and. kp==-ipiv( k-1 ) )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, &
                           1_ilp64 ), ldb )
                 k=k-2
              endif
             end do
           end if
           ! revert a
           call stdlib_I64_zsyconv( uplo, 'R', n, a, lda, ipiv, work, iinfo )
           return
     end subroutine stdlib_I64_zhetrs2




     pure module subroutine stdlib_I64_chetrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb,info )
     !! CHETRS_3 solves a system of linear equations A * X = B with a complex
     !! Hermitian matrix A using the factorization computed
     !! by CHETRF_RK or CHETRF_BK:
     !! A = P*U*D*(U**H)*(P**T) or A = P*L*D*(L**H)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**H (or L**H) is the conjugate of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is Hermitian and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This algorithm is using Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), e(*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, j, k, kp
           real(sp) :: s
           complex(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHETRS_3', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! begin upper
              ! solve a*x = b, where a = u*d*u**h.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 end if
              end do
              ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
              call stdlib_I64_ctrsm( 'L', 'U', 'N', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i = n
              do while ( i>=1 )
                 if( ipiv( i )>0_ilp64 ) then
                    s = real( cone,KIND=sp) / real( a( i, i ),KIND=sp)
                    call stdlib_I64_csscal( nrhs, s, b( i, 1_ilp64 ), ldb )
                 else if ( i>1_ilp64 ) then
                    akm1k = e( i )
                    akm1 = a( i-1, i-1 ) / akm1k
                    ak = a( i, i ) / conjg( akm1k )
                    denom = akm1*ak - cone
                    do j = 1, nrhs
                       bkm1 = b( i-1, j ) / akm1k
                       bk = b( i, j ) / conjg( akm1k )
                       b( i-1, j ) = ( ak*bkm1-bk ) / denom
                       b( i, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i - 1_ilp64
                 end if
                 i = i - 1_ilp64
              end do
              ! compute (u**h \ b) -> b   [ u**h \ (d \ (u \p**t * b) ) ]
              call stdlib_I64_ctrsm( 'L', 'U', 'C', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! p * b  [ p * (u**h \ (d \ (u \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 end if
              end do
           else
              ! begin lower
              ! solve a*x = b, where a = l*d*l**h.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 end if
              end do
              ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
              call stdlib_I64_ctrsm( 'L', 'L', 'N', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i = 1_ilp64
              do while ( i<=n )
                 if( ipiv( i )>0_ilp64 ) then
                    s = real( cone,KIND=sp) / real( a( i, i ),KIND=sp)
                    call stdlib_I64_csscal( nrhs, s, b( i, 1_ilp64 ), ldb )
                 else if( i<n ) then
                    akm1k = e( i )
                    akm1 = a( i, i ) / conjg( akm1k )
                    ak = a( i+1, i+1 ) / akm1k
                    denom = akm1*ak - cone
                    do  j = 1, nrhs
                       bkm1 = b( i, j ) / conjg( akm1k )
                       bk = b( i+1, j ) / akm1k
                       b( i, j ) = ( ak*bkm1-bk ) / denom
                       b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i + 1_ilp64
                 end if
                 i = i + 1_ilp64
              end do
              ! compute (l**h \ b) -> b   [ l**h \ (d \ (l \p**t * b) ) ]
              call stdlib_I64_ctrsm('L', 'L', 'C', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! p * b  [ p * (l**h \ (d \ (l \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 end if
              end do
              ! end lower
           end if
           return
     end subroutine stdlib_I64_chetrs_3

     pure module subroutine stdlib_I64_zhetrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb,info )
     !! ZHETRS_3 solves a system of linear equations A * X = B with a complex
     !! Hermitian matrix A using the factorization computed
     !! by ZHETRF_RK or ZHETRF_BK:
     !! A = P*U*D*(U**H)*(P**T) or A = P*L*D*(L**H)*(P**T),
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**H (or L**H) is the conjugate of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is Hermitian and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! This algorithm is using Level 3 BLAS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), e(*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, j, k, kp
           real(dp) :: s
           complex(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHETRS_3', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! begin upper
              ! solve a*x = b, where a = u*d*u**h.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 end if
              end do
              ! compute (u \p**t * b) -> b    [ (u \p**t * b) ]
              call stdlib_I64_ztrsm( 'L', 'U', 'N', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (u \p**t * b) ]
              i = n
              do while ( i>=1 )
                 if( ipiv( i )>0_ilp64 ) then
                    s = real( cone,KIND=dp) / real( a( i, i ),KIND=dp)
                    call stdlib_I64_zdscal( nrhs, s, b( i, 1_ilp64 ), ldb )
                 else if ( i>1_ilp64 ) then
                    akm1k = e( i )
                    akm1 = a( i-1, i-1 ) / akm1k
                    ak = a( i, i ) / conjg( akm1k )
                    denom = akm1*ak - cone
                    do j = 1, nrhs
                       bkm1 = b( i-1, j ) / akm1k
                       bk = b( i, j ) / conjg( akm1k )
                       b( i-1, j ) = ( ak*bkm1-bk ) / denom
                       b( i, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i - 1_ilp64
                 end if
                 i = i - 1_ilp64
              end do
              ! compute (u**h \ b) -> b   [ u**h \ (d \ (u \p**t * b) ) ]
              call stdlib_I64_ztrsm( 'L', 'U', 'C', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! p * b  [ p * (u**h \ (d \ (u \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for upper case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 end if
              end do
           else
              ! begin lower
              ! solve a*x = b, where a = l*d*l**h.
              ! p**t * b
              ! interchange rows k and ipiv(k) of matrix b in the same order
              ! that the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with increment 1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = 1, n, 1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 end if
              end do
              ! compute (l \p**t * b) -> b    [ (l \p**t * b) ]
              call stdlib_I64_ztrsm( 'L', 'L', 'N', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! compute d \ b -> b   [ d \ (l \p**t * b) ]
              i = 1_ilp64
              do while ( i<=n )
                 if( ipiv( i )>0_ilp64 ) then
                    s = real( cone,KIND=dp) / real( a( i, i ),KIND=dp)
                    call stdlib_I64_zdscal( nrhs, s, b( i, 1_ilp64 ), ldb )
                 else if( i<n ) then
                    akm1k = e( i )
                    akm1 = a( i, i ) / conjg( akm1k )
                    ak = a( i+1, i+1 ) / akm1k
                    denom = akm1*ak - cone
                    do  j = 1, nrhs
                       bkm1 = b( i, j ) / conjg( akm1k )
                       bk = b( i+1, j ) / akm1k
                       b( i, j ) = ( ak*bkm1-bk ) / denom
                       b( i+1, j ) = ( akm1*bk-bkm1 ) / denom
                    end do
                    i = i + 1_ilp64
                 end if
                 i = i + 1_ilp64
              end do
              ! compute (l**h \ b) -> b   [ l**h \ (d \ (l \p**t * b) ) ]
              call stdlib_I64_ztrsm('L', 'L', 'C', 'U', n, nrhs, cone, a, lda, b, ldb )
              ! p * b  [ p * (l**h \ (d \ (l \p**t * b) )) ]
              ! interchange rows k and ipiv(k) of matrix b in reverse order
              ! from the formation order of ipiv(i) vector for lower case.
              ! (we can do the simple loop over ipiv with decrement -1,
              ! since the abs value of ipiv(i) represents the row index
              ! of the interchange with row i in both 1x1 and 2x2 pivot cases)
              do k = n, 1, -1
                 kp = abs( ipiv( k ) )
                 if( kp/=k ) then
                    call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 end if
              end do
              ! end lower
           end if
           return
     end subroutine stdlib_I64_zhetrs_3




     pure module subroutine stdlib_I64_cheswapr( uplo, n, a, lda, i1, i2)
     !! CHESWAPR applies an elementary permutation on the rows and the columns of
     !! a hermitian matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: i1, i2, lda, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,n)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i
           complex(sp) :: tmp
           ! Executable Statements 
           upper = stdlib_lsame( uplo, 'U' )
           if (upper) then
               ! upper
               ! first swap
                ! - swap column i1 and i2 from i1 to i1-1
              call stdlib_I64_cswap( i1-1, a(1_ilp64,i1), 1_ilp64, a(1_ilp64,i2), 1_ilp64 )
                ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap row i1 from i1+1 to i2-1 with col i2 from i1+1 to i2-1
                ! - swap a(i2,i1) and a(i1,i2)
              tmp=a(i1,i1)
              a(i1,i1)=a(i2,i2)
              a(i2,i2)=tmp
              do i=1,i2-i1-1
                 tmp=a(i1,i1+i)
                 a(i1,i1+i)=conjg(a(i1+i,i2))
                 a(i1+i,i2)=conjg(tmp)
              end do
               a(i1,i2)=conjg(a(i1,i2))
                ! third swap
                ! - swap row i1 and i2 from i2+1 to n
              do i=i2+1,n
                 tmp=a(i1,i)
                 a(i1,i)=a(i2,i)
                 a(i2,i)=tmp
              end do
             else
               ! lower
               ! first swap
                ! - swap row i1 and i2 from 1 to i1-1
              call stdlib_I64_cswap ( i1-1, a(i1,1_ilp64), lda, a(i2,1_ilp64), lda )
               ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap col i1 from i1+1 to i2-1 with row i2 from i1+1 to i2-1
                ! - swap a(i2,i1) and a(i1,i2)
               tmp=a(i1,i1)
               a(i1,i1)=a(i2,i2)
               a(i2,i2)=tmp
               do i=1,i2-i1-1
                  tmp=a(i1+i,i1)
                  a(i1+i,i1)=conjg(a(i2,i1+i))
                  a(i2,i1+i)=conjg(tmp)
               end do
               a(i2,i1)=conjg(a(i2,i1))
               ! third swap
                ! - swap col i1 and i2 from i2+1 to n
               do i=i2+1,n
                  tmp=a(i,i1)
                  a(i,i1)=a(i,i2)
                  a(i,i2)=tmp
               end do
           endif
     end subroutine stdlib_I64_cheswapr

     pure module subroutine stdlib_I64_zheswapr( uplo, n, a, lda, i1, i2)
     !! ZHESWAPR applies an elementary permutation on the rows and the columns of
     !! a hermitian matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: i1, i2, lda, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,n)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i
           complex(dp) :: tmp
           ! Executable Statements 
           upper = stdlib_lsame( uplo, 'U' )
           if (upper) then
               ! upper
               ! first swap
                ! - swap column i1 and i2 from i1 to i1-1
              call stdlib_I64_zswap( i1-1, a(1_ilp64,i1), 1_ilp64, a(1_ilp64,i2), 1_ilp64 )
                ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap row i1 from i1+1 to i2-1 with col i2 from i1+1 to i2-1
                ! - swap a(i2,i1) and a(i1,i2)
              tmp=a(i1,i1)
              a(i1,i1)=a(i2,i2)
              a(i2,i2)=tmp
              do i=1,i2-i1-1
                 tmp=a(i1,i1+i)
                 a(i1,i1+i)=conjg(a(i1+i,i2))
                 a(i1+i,i2)=conjg(tmp)
              end do
               a(i1,i2)=conjg(a(i1,i2))
                ! third swap
                ! - swap row i1 and i2 from i2+1 to n
              do i=i2+1,n
                 tmp=a(i1,i)
                 a(i1,i)=a(i2,i)
                 a(i2,i)=tmp
              end do
             else
               ! lower
               ! first swap
                ! - swap row i1 and i2 from 1 to i1-1
              call stdlib_I64_zswap ( i1-1, a(i1,1_ilp64), lda, a(i2,1_ilp64), lda )
               ! second swap :
                ! - swap a(i1,i1) and a(i2,i2)
                ! - swap col i1 from i1+1 to i2-1 with row i2 from i1+1 to i2-1
                ! - swap a(i2,i1) and a(i1,i2)
               tmp=a(i1,i1)
               a(i1,i1)=a(i2,i2)
               a(i2,i2)=tmp
               do i=1,i2-i1-1
                  tmp=a(i1+i,i1)
                  a(i1+i,i1)=conjg(a(i2,i1+i))
                  a(i2,i1+i)=conjg(tmp)
               end do
               a(i2,i1)=conjg(a(i2,i1))
               ! third swap
                ! - swap col i1 and i2 from i2+1 to n
               do i=i2+1,n
                  tmp=a(i,i1)
                  a(i,i1)=a(i,i2)
                  a(i,i2)=tmp
               end do
           endif
     end subroutine stdlib_I64_zheswapr




     pure module subroutine stdlib_I64_chpcon( uplo, n, ap, ipiv, anorm, rcond, work, info )
     !! CHPCON estimates the reciprocal of the condition number of a complex
     !! Hermitian packed matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by CHPTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           real(sp), intent(in) :: anorm
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, ip, kase
           real(sp) :: ainvnm
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( anorm<zero ) then
              info = -5_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHPCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp64 ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              ip = n*( n+1 ) / 2_ilp64
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip - i
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              ip = 1_ilp64
              do i = 1, n
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip + n - i + 1_ilp64
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp64
           30 continue
           call stdlib_I64_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              ! multiply by inv(l*d*l**h) or inv(u*d*u**h).
              call stdlib_I64_chptrs( uplo, n, 1_ilp64, ap, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_I64_chpcon

     pure module subroutine stdlib_I64_zhpcon( uplo, n, ap, ipiv, anorm, rcond, work, info )
     !! ZHPCON estimates the reciprocal of the condition number of a complex
     !! Hermitian packed matrix A using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by ZHPTRF.
     !! An estimate is obtained for norm(inv(A)), and the reciprocal of the
     !! condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           real(dp), intent(in) :: anorm
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, ip, kase
           real(dp) :: ainvnm
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( anorm<zero ) then
              info = -5_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHPCON', -info )
              return
           end if
           ! quick return if possible
           rcond = zero
           if( n==0_ilp64 ) then
              rcond = one
              return
           else if( anorm<=zero ) then
              return
           end if
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              ip = n*( n+1 ) / 2_ilp64
              do i = n, 1, -1
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip - i
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              ip = 1_ilp64
              do i = 1, n
                 if( ipiv( i )>0 .and. ap( ip )==zero )return
                 ip = ip + n - i + 1_ilp64
              end do
           end if
           ! estimate the 1-norm of the inverse.
           kase = 0_ilp64
           30 continue
           call stdlib_I64_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              ! multiply by inv(l*d*l**h) or inv(u*d*u**h).
              call stdlib_I64_zhptrs( uplo, n, 1_ilp64, ap, ipiv, work, n, info )
              go to 30
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm/=zero )rcond = ( one / ainvnm ) / anorm
           return
     end subroutine stdlib_I64_zhpcon




     pure module subroutine stdlib_I64_chptrf( uplo, n, ap, ipiv, info )
     !! CHPTRF computes the factorization of a complex Hermitian packed
     !! matrix A using the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**H  or  A = L*D*L**H
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is Hermitian and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: ap(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sevten = 17.0e+0_sp
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, imax, j, jmax, k, kc, kk, knc, kp, kpc, kstep, kx, npp
           real(sp) :: absakk, alpha, colmax, d, d11, d22, r1, rowmax, tt
           complex(sp) :: d12, d21, t, wk, wkm1, wkp1, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHPTRF', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**h using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              kc = ( n-1 )*n / 2_ilp64 + 1_ilp64
              10 continue
              knc = kc
              ! if k < 1, exit from loop
              if( k<1 )go to 110
              kstep = 1_ilp64
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( ap( kc+k-1 ),KIND=sp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k>1_ilp64 ) then
                 imax = stdlib_I64_icamax( k-1, ap( kc ), 1_ilp64 )
                 colmax = cabs1( ap( kc+imax-1 ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp64 )info = k
                 kp = k
                 ap( kc+k-1 ) = real( ap( kc+k-1 ),KIND=sp)
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    rowmax = zero
                    jmax = imax
                    kx = imax*( imax+1 ) / 2_ilp64 + imax
                    do j = imax + 1, k
                       if( cabs1( ap( kx ) )>rowmax ) then
                          rowmax = cabs1( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + j
                    end do
                    kpc = ( imax-1 )*imax / 2_ilp64 + 1_ilp64
                    if( imax>1_ilp64 ) then
                       jmax = stdlib_I64_icamax( imax-1, ap( kpc ), 1_ilp64 )
                       rowmax = max( rowmax, cabs1( ap( kpc+jmax-1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( ap( kpc+imax-1 ),KIND=sp) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp64
                    end if
                 end if
                 kk = k - kstep + 1_ilp64
                 if( kstep==2_ilp64 )knc = knc - k + 1_ilp64
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_I64_cswap( kp-1, ap( knc ), 1_ilp64, ap( kpc ), 1_ilp64 )
                    kx = kpc + kp - 1_ilp64
                    do j = kp + 1, kk - 1
                       kx = kx + j - 1_ilp64
                       t = conjg( ap( knc+j-1 ) )
                       ap( knc+j-1 ) = conjg( ap( kx ) )
                       ap( kx ) = t
                    end do
                    ap( kx+kk-1 ) = conjg( ap( kx+kk-1 ) )
                    r1 = real( ap( knc+kk-1 ),KIND=sp)
                    ap( knc+kk-1 ) = real( ap( kpc+kp-1 ),KIND=sp)
                    ap( kpc+kp-1 ) = r1
                    if( kstep==2_ilp64 ) then
                       ap( kc+k-1 ) = real( ap( kc+k-1 ),KIND=sp)
                       t = ap( kc+k-2 )
                       ap( kc+k-2 ) = ap( kc+kp-1 )
                       ap( kc+kp-1 ) = t
                    end if
                 else
                    ap( kc+k-1 ) = real( ap( kc+k-1 ),KIND=sp)
                    if( kstep==2_ilp64 )ap( kc-1 ) = real( ap( kc-1 ),KIND=sp)
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp64 ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**h = a - w(k)*1/d(k)*w(k)**h
                    r1 = one / real( ap( kc+k-1 ),KIND=sp)
                    call stdlib_I64_chpr( uplo, k-1, -r1, ap( kc ), 1_ilp64, ap )
                    ! store u(k) in column k
                    call stdlib_I64_csscal( k-1, r1, ap( kc ), 1_ilp64 )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**h
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**h
                    if( k>2_ilp64 ) then
                       d = stdlib_I64_slapy2( real( ap( k-1+( k-1 )*k / 2_ilp64 ),KIND=sp),aimag( ap( k-1+( &
                                 k-1 )*k / 2_ilp64 ) ) )
                       d22 = real( ap( k-1+( k-2 )*( k-1 ) / 2_ilp64 ),KIND=sp) / d
                       d11 = real( ap( k+( k-1 )*k / 2_ilp64 ),KIND=sp) / d
                       tt = one / ( d11*d22-one )
                       d12 = ap( k-1+( k-1 )*k / 2_ilp64 ) / d
                       d = tt / d
                       do j = k - 2, 1, -1
                          wkm1 = d*( d11*ap( j+( k-2 )*( k-1 ) / 2_ilp64 )-conjg( d12 )*ap( j+( k-1 )*k &
                                    / 2_ilp64 ) )
                          wk = d*( d22*ap( j+( k-1 )*k / 2_ilp64 )-d12*ap( j+( k-2 )*( k-1 ) / 2_ilp64 ) )
                                    
                          do i = j, 1, -1
                             ap( i+( j-1 )*j / 2_ilp64 ) = ap( i+( j-1 )*j / 2_ilp64 ) -ap( i+( k-1 )*k / 2_ilp64 )&
                                       *conjg( wk ) -ap( i+( k-2 )*( k-1 ) / 2_ilp64 )*conjg( wkm1 )
                          end do
                          ap( j+( k-1 )*k / 2_ilp64 ) = wk
                          ap( j+( k-2 )*( k-1 ) / 2_ilp64 ) = wkm1
                          ap( j+( j-1 )*j / 2_ilp64 ) = cmplx( real( ap( j+( j-1 )*j / 2_ilp64 ),KIND=sp), &
                                    zero,KIND=sp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp64 ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              kc = knc - k
              go to 10
           else
              ! factorize a as l*d*l**h using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp64
              kc = 1_ilp64
              npp = n*( n+1 ) / 2_ilp64
              60 continue
              knc = kc
              ! if k > n, exit from loop
              if( k>n )go to 110
              kstep = 1_ilp64
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( ap( kc ),KIND=sp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k<n ) then
                 imax = k + stdlib_I64_icamax( n-k, ap( kc+1 ), 1_ilp64 )
                 colmax = cabs1( ap( kc+imax-k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp64 )info = k
                 kp = k
                 ap( kc ) = real( ap( kc ),KIND=sp)
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    rowmax = zero
                    kx = kc + imax - k
                    do j = k, imax - 1
                       if( cabs1( ap( kx ) )>rowmax ) then
                          rowmax = cabs1( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + n - j
                    end do
                    kpc = npp - ( n-imax+1 )*( n-imax+2 ) / 2_ilp64 + 1_ilp64
                    if( imax<n ) then
                       jmax = imax + stdlib_I64_icamax( n-imax, ap( kpc+1 ), 1_ilp64 )
                       rowmax = max( rowmax, cabs1( ap( kpc+jmax-imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( ap( kpc ),KIND=sp) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp64
                    end if
                 end if
                 kk = k + kstep - 1_ilp64
                 if( kstep==2_ilp64 )knc = knc + n - k + 1_ilp64
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_I64_cswap( n-kp, ap( knc+kp-kk+1 ), 1_ilp64, ap( kpc+1 ),1_ilp64 )
                              
                    kx = knc + kp - kk
                    do j = kk + 1, kp - 1
                       kx = kx + n - j + 1_ilp64
                       t = conjg( ap( knc+j-kk ) )
                       ap( knc+j-kk ) = conjg( ap( kx ) )
                       ap( kx ) = t
                    end do
                    ap( knc+kp-kk ) = conjg( ap( knc+kp-kk ) )
                    r1 = real( ap( knc ),KIND=sp)
                    ap( knc ) = real( ap( kpc ),KIND=sp)
                    ap( kpc ) = r1
                    if( kstep==2_ilp64 ) then
                       ap( kc ) = real( ap( kc ),KIND=sp)
                       t = ap( kc+1 )
                       ap( kc+1 ) = ap( kc+kp-k )
                       ap( kc+kp-k ) = t
                    end if
                 else
                    ap( kc ) = real( ap( kc ),KIND=sp)
                    if( kstep==2_ilp64 )ap( knc ) = real( ap( knc ),KIND=sp)
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp64 ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**h = a - w(k)*(1/d(k))*w(k)**h
                       r1 = one / real( ap( kc ),KIND=sp)
                       call stdlib_I64_chpr( uplo, n-k, -r1, ap( kc+1 ), 1_ilp64,ap( kc+n-k+1 ) )
                       ! store l(k) in column k
                       call stdlib_I64_csscal( n-k, r1, ap( kc+1 ), 1_ilp64 )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**h
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**h
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d = stdlib_I64_slapy2( real( ap( k+1+( k-1 )*( 2_ilp64*n-k ) / 2_ilp64 ),KIND=sp),aimag( &
                                 ap( k+1+( k-1 )*( 2_ilp64*n-k ) / 2_ilp64 ) ) )
                       d11 = real( ap( k+1+k*( 2_ilp64*n-k-1 ) / 2_ilp64 ),KIND=sp) / d
                       d22 = real( ap( k+( k-1 )*( 2_ilp64*n-k ) / 2_ilp64 ),KIND=sp) / d
                       tt = one / ( d11*d22-one )
                       d21 = ap( k+1+( k-1 )*( 2_ilp64*n-k ) / 2_ilp64 ) / d
                       d = tt / d
                       do j = k + 2, n
                          wk = d*( d11*ap( j+( k-1 )*( 2_ilp64*n-k ) / 2_ilp64 )-d21*ap( j+k*( 2_ilp64*n-k-1 ) / 2_ilp64 )&
                                     )
                          wkp1 = d*( d22*ap( j+k*( 2_ilp64*n-k-1 ) / 2_ilp64 )-conjg( d21 )*ap( j+( k-1 )*( &
                                    2_ilp64*n-k ) / 2_ilp64 ) )
                          do i = j, n
                             ap( i+( j-1 )*( 2_ilp64*n-j ) / 2_ilp64 ) = ap( i+( j-1 )*( 2_ilp64*n-j ) / 2_ilp64 ) - ap( &
                             i+( k-1 )*( 2_ilp64*n-k ) /2_ilp64 )*conjg( wk ) - ap( i+k*( 2_ilp64*n-k-1 ) / 2_ilp64 )&
                                       *conjg( wkp1 )
                          end do
                          ap( j+( k-1 )*( 2_ilp64*n-k ) / 2_ilp64 ) = wk
                          ap( j+k*( 2_ilp64*n-k-1 ) / 2_ilp64 ) = wkp1
                          ap( j+( j-1 )*( 2_ilp64*n-j ) / 2_ilp64 )= cmplx( real( ap( j+( j-1 )*( 2_ilp64*n-j ) / 2_ilp64 &
                                    ),KIND=sp),zero,KIND=sp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp64 ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              kc = knc + n - k + 2_ilp64
              go to 60
           end if
           110 continue
           return
     end subroutine stdlib_I64_chptrf

     pure module subroutine stdlib_I64_zhptrf( uplo, n, ap, ipiv, info )
     !! ZHPTRF computes the factorization of a complex Hermitian packed
     !! matrix A using the Bunch-Kaufman diagonal pivoting method:
     !! A = U*D*U**H  or  A = L*D*L**H
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is Hermitian and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: ap(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sevten = 17.0e+0_dp
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, imax, j, jmax, k, kc, kk, knc, kp, kpc, kstep, kx, npp
           real(dp) :: absakk, alpha, colmax, d, d11, d22, r1, rowmax, tt
           complex(dp) :: d12, d21, t, wk, wkm1, wkp1, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHPTRF', -info )
              return
           end if
           ! initialize alpha for use in choosing pivot block size.
           alpha = ( one+sqrt( sevten ) ) / eight
           if( upper ) then
              ! factorize a as u*d*u**h using the upper triangle of a
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2
              k = n
              kc = ( n-1 )*n / 2_ilp64 + 1_ilp64
              10 continue
              knc = kc
              ! if k < 1, exit from loop
              if( k<1 )go to 110
              kstep = 1_ilp64
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( ap( kc+k-1 ),KIND=dp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k>1_ilp64 ) then
                 imax = stdlib_I64_izamax( k-1, ap( kc ), 1_ilp64 )
                 colmax = cabs1( ap( kc+imax-1 ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp64 )info = k
                 kp = k
                 ap( kc+k-1 ) = real( ap( kc+k-1 ),KIND=dp)
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    rowmax = zero
                    jmax = imax
                    kx = imax*( imax+1 ) / 2_ilp64 + imax
                    do j = imax + 1, k
                       if( cabs1( ap( kx ) )>rowmax ) then
                          rowmax = cabs1( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + j
                    end do
                    kpc = ( imax-1 )*imax / 2_ilp64 + 1_ilp64
                    if( imax>1_ilp64 ) then
                       jmax = stdlib_I64_izamax( imax-1, ap( kpc ), 1_ilp64 )
                       rowmax = max( rowmax, cabs1( ap( kpc+jmax-1 ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( ap( kpc+imax-1 ),KIND=dp) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k-1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp64
                    end if
                 end if
                 kk = k - kstep + 1_ilp64
                 if( kstep==2_ilp64 )knc = knc - k + 1_ilp64
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the leading
                    ! submatrix a(1:k,1:k)
                    call stdlib_I64_zswap( kp-1, ap( knc ), 1_ilp64, ap( kpc ), 1_ilp64 )
                    kx = kpc + kp - 1_ilp64
                    do j = kp + 1, kk - 1
                       kx = kx + j - 1_ilp64
                       t = conjg( ap( knc+j-1 ) )
                       ap( knc+j-1 ) = conjg( ap( kx ) )
                       ap( kx ) = t
                    end do
                    ap( kx+kk-1 ) = conjg( ap( kx+kk-1 ) )
                    r1 = real( ap( knc+kk-1 ),KIND=dp)
                    ap( knc+kk-1 ) = real( ap( kpc+kp-1 ),KIND=dp)
                    ap( kpc+kp-1 ) = r1
                    if( kstep==2_ilp64 ) then
                       ap( kc+k-1 ) = real( ap( kc+k-1 ),KIND=dp)
                       t = ap( kc+k-2 )
                       ap( kc+k-2 ) = ap( kc+kp-1 )
                       ap( kc+kp-1 ) = t
                    end if
                 else
                    ap( kc+k-1 ) = real( ap( kc+k-1 ),KIND=dp)
                    if( kstep==2_ilp64 )ap( kc-1 ) = real( ap( kc-1 ),KIND=dp)
                 end if
                 ! update the leading submatrix
                 if( kstep==1_ilp64 ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = u(k)*d(k)
                    ! where u(k) is the k-th column of u
                    ! perform a rank-1 update of a(1:k-1,1:k-1) as
                    ! a := a - u(k)*d(k)*u(k)**h = a - w(k)*1/d(k)*w(k)**h
                    r1 = one / real( ap( kc+k-1 ),KIND=dp)
                    call stdlib_I64_zhpr( uplo, k-1, -r1, ap( kc ), 1_ilp64, ap )
                    ! store u(k) in column k
                    call stdlib_I64_zdscal( k-1, r1, ap( kc ), 1_ilp64 )
                 else
                    ! 2-by-2 pivot block d(k): columns k and k-1 now hold
                    ! ( w(k-1) w(k) ) = ( u(k-1) u(k) )*d(k)
                    ! where u(k) and u(k-1) are the k-th and (k-1)-th columns
                    ! of u
                    ! perform a rank-2 update of a(1:k-2,1:k-2) as
                    ! a := a - ( u(k-1) u(k) )*d(k)*( u(k-1) u(k) )**h
                       ! = a - ( w(k-1) w(k) )*inv(d(k))*( w(k-1) w(k) )**h
                    if( k>2_ilp64 ) then
                       d = stdlib_I64_dlapy2( real( ap( k-1+( k-1 )*k / 2_ilp64 ),KIND=dp),aimag( ap( k-1+( &
                                 k-1 )*k / 2_ilp64 ) ) )
                       d22 = real( ap( k-1+( k-2 )*( k-1 ) / 2_ilp64 ),KIND=dp) / d
                       d11 = real( ap( k+( k-1 )*k / 2_ilp64 ),KIND=dp) / d
                       tt = one / ( d11*d22-one )
                       d12 = ap( k-1+( k-1 )*k / 2_ilp64 ) / d
                       d = tt / d
                       do j = k - 2, 1, -1
                          wkm1 = d*( d11*ap( j+( k-2 )*( k-1 ) / 2_ilp64 )-conjg( d12 )*ap( j+( k-1 )*k &
                                    / 2_ilp64 ) )
                          wk = d*( d22*ap( j+( k-1 )*k / 2_ilp64 )-d12*ap( j+( k-2 )*( k-1 ) / 2_ilp64 ) )
                                    
                          do i = j, 1, -1
                             ap( i+( j-1 )*j / 2_ilp64 ) = ap( i+( j-1 )*j / 2_ilp64 ) -ap( i+( k-1 )*k / 2_ilp64 )&
                                       *conjg( wk ) -ap( i+( k-2 )*( k-1 ) / 2_ilp64 )*conjg( wkm1 )
                          end do
                          ap( j+( k-1 )*k / 2_ilp64 ) = wk
                          ap( j+( k-2 )*( k-1 ) / 2_ilp64 ) = wkm1
                          ap( j+( j-1 )*j / 2_ilp64 ) = cmplx( real( ap( j+( j-1 )*j / 2_ilp64 ),KIND=dp), &
                                    zero,KIND=dp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp64 ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k-1 ) = -kp
              end if
              ! decrease k and return to the start of the main loop
              k = k - kstep
              kc = knc - k
              go to 10
           else
              ! factorize a as l*d*l**h using the lower triangle of a
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2
              k = 1_ilp64
              kc = 1_ilp64
              npp = n*( n+1 ) / 2_ilp64
              60 continue
              knc = kc
              ! if k > n, exit from loop
              if( k>n )go to 110
              kstep = 1_ilp64
              ! determine rows and columns to be interchanged and whether
              ! a 1-by-1 or 2-by-2 pivot block will be used
              absakk = abs( real( ap( kc ),KIND=dp) )
              ! imax is the row-index of the largest off-diagonal element in
              ! column k, and colmax is its absolute value
              if( k<n ) then
                 imax = k + stdlib_I64_izamax( n-k, ap( kc+1 ), 1_ilp64 )
                 colmax = cabs1( ap( kc+imax-k ) )
              else
                 colmax = zero
              end if
              if( max( absakk, colmax )==zero ) then
                 ! column k is zero: set info and continue
                 if( info==0_ilp64 )info = k
                 kp = k
                 ap( kc ) = real( ap( kc ),KIND=dp)
              else
                 if( absakk>=alpha*colmax ) then
                    ! no interchange, use 1-by-1 pivot block
                    kp = k
                 else
                    ! jmax is the column-index of the largest off-diagonal
                    ! element in row imax, and rowmax is its absolute value
                    rowmax = zero
                    kx = kc + imax - k
                    do j = k, imax - 1
                       if( cabs1( ap( kx ) )>rowmax ) then
                          rowmax = cabs1( ap( kx ) )
                          jmax = j
                       end if
                       kx = kx + n - j
                    end do
                    kpc = npp - ( n-imax+1 )*( n-imax+2 ) / 2_ilp64 + 1_ilp64
                    if( imax<n ) then
                       jmax = imax + stdlib_I64_izamax( n-imax, ap( kpc+1 ), 1_ilp64 )
                       rowmax = max( rowmax, cabs1( ap( kpc+jmax-imax ) ) )
                    end if
                    if( absakk>=alpha*colmax*( colmax / rowmax ) ) then
                       ! no interchange, use 1-by-1 pivot block
                       kp = k
                    else if( abs( real( ap( kpc ),KIND=dp) )>=alpha*rowmax ) then
                       ! interchange rows and columns k and imax, use 1-by-1
                       ! pivot block
                       kp = imax
                    else
                       ! interchange rows and columns k+1 and imax, use 2-by-2
                       ! pivot block
                       kp = imax
                       kstep = 2_ilp64
                    end if
                 end if
                 kk = k + kstep - 1_ilp64
                 if( kstep==2_ilp64 )knc = knc + n - k + 1_ilp64
                 if( kp/=kk ) then
                    ! interchange rows and columns kk and kp in the trailing
                    ! submatrix a(k:n,k:n)
                    if( kp<n )call stdlib_I64_zswap( n-kp, ap( knc+kp-kk+1 ), 1_ilp64, ap( kpc+1 ),1_ilp64 )
                              
                    kx = knc + kp - kk
                    do j = kk + 1, kp - 1
                       kx = kx + n - j + 1_ilp64
                       t = conjg( ap( knc+j-kk ) )
                       ap( knc+j-kk ) = conjg( ap( kx ) )
                       ap( kx ) = t
                    end do
                    ap( knc+kp-kk ) = conjg( ap( knc+kp-kk ) )
                    r1 = real( ap( knc ),KIND=dp)
                    ap( knc ) = real( ap( kpc ),KIND=dp)
                    ap( kpc ) = r1
                    if( kstep==2_ilp64 ) then
                       ap( kc ) = real( ap( kc ),KIND=dp)
                       t = ap( kc+1 )
                       ap( kc+1 ) = ap( kc+kp-k )
                       ap( kc+kp-k ) = t
                    end if
                 else
                    ap( kc ) = real( ap( kc ),KIND=dp)
                    if( kstep==2_ilp64 )ap( knc ) = real( ap( knc ),KIND=dp)
                 end if
                 ! update the trailing submatrix
                 if( kstep==1_ilp64 ) then
                    ! 1-by-1 pivot block d(k): column k now holds
                    ! w(k) = l(k)*d(k)
                    ! where l(k) is the k-th column of l
                    if( k<n ) then
                       ! perform a rank-1 update of a(k+1:n,k+1:n) as
                       ! a := a - l(k)*d(k)*l(k)**h = a - w(k)*(1/d(k))*w(k)**h
                       r1 = one / real( ap( kc ),KIND=dp)
                       call stdlib_I64_zhpr( uplo, n-k, -r1, ap( kc+1 ), 1_ilp64,ap( kc+n-k+1 ) )
                       ! store l(k) in column k
                       call stdlib_I64_zdscal( n-k, r1, ap( kc+1 ), 1_ilp64 )
                    end if
                 else
                    ! 2-by-2 pivot block d(k): columns k and k+1 now hold
                    ! ( w(k) w(k+1) ) = ( l(k) l(k+1) )*d(k)
                    ! where l(k) and l(k+1) are the k-th and (k+1)-th columns
                    ! of l
                    if( k<n-1 ) then
                       ! perform a rank-2 update of a(k+2:n,k+2:n) as
                       ! a := a - ( l(k) l(k+1) )*d(k)*( l(k) l(k+1) )**h
                          ! = a - ( w(k) w(k+1) )*inv(d(k))*( w(k) w(k+1) )**h
                       ! where l(k) and l(k+1) are the k-th and (k+1)-th
                       ! columns of l
                       d = stdlib_I64_dlapy2( real( ap( k+1+( k-1 )*( 2_ilp64*n-k ) / 2_ilp64 ),KIND=dp),aimag( &
                                 ap( k+1+( k-1 )*( 2_ilp64*n-k ) / 2_ilp64 ) ) )
                       d11 = real( ap( k+1+k*( 2_ilp64*n-k-1 ) / 2_ilp64 ),KIND=dp) / d
                       d22 = real( ap( k+( k-1 )*( 2_ilp64*n-k ) / 2_ilp64 ),KIND=dp) / d
                       tt = one / ( d11*d22-one )
                       d21 = ap( k+1+( k-1 )*( 2_ilp64*n-k ) / 2_ilp64 ) / d
                       d = tt / d
                       do j = k + 2, n
                          wk = d*( d11*ap( j+( k-1 )*( 2_ilp64*n-k ) / 2_ilp64 )-d21*ap( j+k*( 2_ilp64*n-k-1 ) / 2_ilp64 )&
                                     )
                          wkp1 = d*( d22*ap( j+k*( 2_ilp64*n-k-1 ) / 2_ilp64 )-conjg( d21 )*ap( j+( k-1 )*( &
                                    2_ilp64*n-k ) /2_ilp64 ) )
                          do i = j, n
                             ap( i+( j-1 )*( 2_ilp64*n-j ) / 2_ilp64 ) = ap( i+( j-1 )*( 2_ilp64*n-j ) / 2_ilp64 ) - ap( &
                             i+( k-1 )*( 2_ilp64*n-k ) /2_ilp64 )*conjg( wk ) - ap( i+k*( 2_ilp64*n-k-1 ) / 2_ilp64 )&
                                       *conjg( wkp1 )
                          end do
                          ap( j+( k-1 )*( 2_ilp64*n-k ) / 2_ilp64 ) = wk
                          ap( j+k*( 2_ilp64*n-k-1 ) / 2_ilp64 ) = wkp1
                          ap( j+( j-1 )*( 2_ilp64*n-j ) / 2_ilp64 )= cmplx( real( ap( j+( j-1 )*( 2_ilp64*n-j ) / 2_ilp64 &
                                    ),KIND=dp),zero,KIND=dp)
                       end do
                    end if
                 end if
              end if
              ! store details of the interchanges in ipiv
              if( kstep==1_ilp64 ) then
                 ipiv( k ) = kp
              else
                 ipiv( k ) = -kp
                 ipiv( k+1 ) = -kp
              end if
              ! increase k and return to the start of the main loop
              k = k + kstep
              kc = knc + n - k + 2_ilp64
              go to 60
           end if
           110 continue
           return
     end subroutine stdlib_I64_zhptrf




     pure module subroutine stdlib_I64_chptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! CHPTRS solves a system of linear equations A*X = B with a complex
     !! Hermitian matrix A stored in packed format using the factorization
     !! A = U*D*U**H or A = L*D*L**H computed by CHPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: j, k, kc, kp
           real(sp) :: s
           complex(sp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**h.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp64 + 1_ilp64
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              kc = kc - k
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_I64_cgeru( k-1, nrhs, -cone, ap( kc ), 1_ilp64, b( k, 1_ilp64 ), ldb,b( 1_ilp64, 1_ilp64 ), ldb )
                           
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=sp) / real( ap( kc+k-1 ),KIND=sp)
                 call stdlib_I64_csscal( nrhs, s, b( k, 1_ilp64 ), ldb )
                 k = k - 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_I64_cswap( nrhs, b( k-1, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_I64_cgeru( k-2, nrhs, -cone, ap( kc ), 1_ilp64, b( k, 1_ilp64 ), ldb,b( 1_ilp64, 1_ilp64 ), ldb )
                           
                 call stdlib_I64_cgeru( k-2, nrhs, -cone, ap( kc-( k-1 ) ), 1_ilp64,b( k-1, 1_ilp64 ), ldb, b( 1_ilp64, &
                           1_ilp64 ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+k-2 )
                 akm1 = ap( kc-1 ) / akm1k
                 ak = ap( kc+k-1 ) / conjg( akm1k )
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / conjg( akm1k )
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc - k + 1_ilp64
                 k = k - 2_ilp64
              end if
              go to 10
              30 continue
              ! next solve u**h *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp64
              kc = 1_ilp64
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**h(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, ap( kc ), &
                              1_ilp64, cone, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 kc = kc + k
                 k = k + 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**h(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, ap( kc ), &
                              1_ilp64, cone, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k+1, 1_ilp64 ), ldb )
                    call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, ap( kc+k ),&
                               1_ilp64, cone, b( k+1, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k+1, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 kc = kc + 2_ilp64*k + 1_ilp64
                 k = k + 2_ilp64
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**h.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp64
              kc = 1_ilp64
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_I64_cgeru( n-k, nrhs, -cone, ap( kc+1 ), 1_ilp64, b( k, 1_ilp64 ),ldb, b( k+&
                           1_ilp64, 1_ilp64 ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=sp) / real( ap( kc ),KIND=sp)
                 call stdlib_I64_csscal( nrhs, s, b( k, 1_ilp64 ), ldb )
                 kc = kc + n - k + 1_ilp64
                 k = k + 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_I64_cswap( nrhs, b( k+1, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_I64_cgeru( n-k-1, nrhs, -cone, ap( kc+2 ), 1_ilp64, b( k, 1_ilp64 ),ldb, b( k+2, &
                              1_ilp64 ), ldb )
                    call stdlib_I64_cgeru( n-k-1, nrhs, -cone, ap( kc+n-k+2 ), 1_ilp64,b( k+1, 1_ilp64 ), ldb, b( &
                              k+2, 1_ilp64 ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+1 )
                 akm1 = ap( kc ) / conjg( akm1k )
                 ak = ap( kc+n-k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / conjg( akm1k )
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc + 2_ilp64*( n-k ) + 1_ilp64
                 k = k + 2_ilp64
              end if
              go to 60
              80 continue
              ! next solve l**h *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp64 + 1_ilp64
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              kc = kc - ( n-k+1 )
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**h(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n ) then
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp64 ), ldb, &
                              ap( kc+1 ), 1_ilp64, cone,b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k = k - 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**h(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp64 ), ldb, &
                              ap( kc+1 ), 1_ilp64, cone,b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k-1, 1_ilp64 ), ldb )
                    call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp64 ), ldb, &
                              ap( kc-( n-k ) ), 1_ilp64, cone,b( k-1, 1_ilp64 ), ldb )
                    call stdlib_I64_clacgv( nrhs, b( k-1, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_I64_cswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 kc = kc - ( n-k+2 )
                 k = k - 2_ilp64
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_I64_chptrs

     pure module subroutine stdlib_I64_zhptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! ZHPTRS solves a system of linear equations A*X = B with a complex
     !! Hermitian matrix A stored in packed format using the factorization
     !! A = U*D*U**H or A = L*D*L**H computed by ZHPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: j, k, kc, kp
           real(dp) :: s
           complex(dp) :: ak, akm1, akm1k, bk, bkm1, denom
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. nrhs==0 )return
           if( upper ) then
              ! solve a*x = b, where a = u*d*u**h.
              ! first solve u*d*x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp64 + 1_ilp64
              10 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 30
              kc = kc - k
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 call stdlib_I64_zgeru( k-1, nrhs, -cone, ap( kc ), 1_ilp64, b( k, 1_ilp64 ), ldb,b( 1_ilp64, 1_ilp64 ), ldb )
                           
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=dp) / real( ap( kc+k-1 ),KIND=dp)
                 call stdlib_I64_zdscal( nrhs, s, b( k, 1_ilp64 ), ldb )
                 k = k - 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k-1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k-1 )call stdlib_I64_zswap( nrhs, b( k-1, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(u(k)), where u(k) is the transformation
                 ! stored in columns k-1 and k of a.
                 call stdlib_I64_zgeru( k-2, nrhs, -cone, ap( kc ), 1_ilp64, b( k, 1_ilp64 ), ldb,b( 1_ilp64, 1_ilp64 ), ldb )
                           
                 call stdlib_I64_zgeru( k-2, nrhs, -cone, ap( kc-( k-1 ) ), 1_ilp64,b( k-1, 1_ilp64 ), ldb, b( 1_ilp64, &
                           1_ilp64 ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+k-2 )
                 akm1 = ap( kc-1 ) / akm1k
                 ak = ap( kc+k-1 ) / conjg( akm1k )
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k-1, j ) / akm1k
                    bk = b( k, j ) / conjg( akm1k )
                    b( k-1, j ) = ( ak*bkm1-bk ) / denom
                    b( k, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc - k + 1_ilp64
                 k = k - 2_ilp64
              end if
              go to 10
              30 continue
              ! next solve u**h *x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp64
              kc = 1_ilp64
              40 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(u**h(k)), where u(k) is the transformation
                 ! stored in column k of a.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, ap( kc ), &
                              1_ilp64, cone, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 kc = kc + k
                 k = k + 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(u**h(k+1)), where u(k+1) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, ap( kc ), &
                              1_ilp64, cone, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k+1, 1_ilp64 ), ldb )
                    call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', k-1, nrhs, -cone, b,ldb, ap( kc+k ),&
                               1_ilp64, cone, b( k+1, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k+1, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 kc = kc + 2_ilp64*k + 1_ilp64
                 k = k + 2_ilp64
              end if
              go to 40
              50 continue
           else
              ! solve a*x = b, where a = l*d*l**h.
              ! first solve l*d*x = b, overwriting b with x.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp64
              kc = 1_ilp64
              60 continue
              ! if k > n, exit from loop.
              if( k>n )go to 80
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n )call stdlib_I64_zgeru( n-k, nrhs, -cone, ap( kc+1 ), 1_ilp64, b( k, 1_ilp64 ),ldb, b( k+&
                           1_ilp64, 1_ilp64 ), ldb )
                 ! multiply by the inverse of the diagonal block.
                 s = real( cone,KIND=dp) / real( ap( kc ),KIND=dp)
                 call stdlib_I64_zdscal( nrhs, s, b( k, 1_ilp64 ), ldb )
                 kc = kc + n - k + 1_ilp64
                 k = k + 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! interchange rows k+1 and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k+1 )call stdlib_I64_zswap( nrhs, b( k+1, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 ! multiply by inv(l(k)), where l(k) is the transformation
                 ! stored in columns k and k+1 of a.
                 if( k<n-1 ) then
                    call stdlib_I64_zgeru( n-k-1, nrhs, -cone, ap( kc+2 ), 1_ilp64, b( k, 1_ilp64 ),ldb, b( k+2, &
                              1_ilp64 ), ldb )
                    call stdlib_I64_zgeru( n-k-1, nrhs, -cone, ap( kc+n-k+2 ), 1_ilp64,b( k+1, 1_ilp64 ), ldb, b( &
                              k+2, 1_ilp64 ), ldb )
                 end if
                 ! multiply by the inverse of the diagonal block.
                 akm1k = ap( kc+1 )
                 akm1 = ap( kc ) / conjg( akm1k )
                 ak = ap( kc+n-k+1 ) / akm1k
                 denom = akm1*ak - cone
                 do j = 1, nrhs
                    bkm1 = b( k, j ) / conjg( akm1k )
                    bk = b( k+1, j ) / akm1k
                    b( k, j ) = ( ak*bkm1-bk ) / denom
                    b( k+1, j ) = ( akm1*bk-bkm1 ) / denom
                 end do
                 kc = kc + 2_ilp64*( n-k ) + 1_ilp64
                 k = k + 2_ilp64
              end if
              go to 60
              80 continue
              ! next solve l**h *x = b, overwriting b with x.
              ! k is the main loop index, decreasing from n to 1 in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = n
              kc = n*( n+1 ) / 2_ilp64 + 1_ilp64
              90 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 100
              kc = kc - ( n-k+1 )
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! multiply by inv(l**h(k)), where l(k) is the transformation
                 ! stored in column k of a.
                 if( k<n ) then
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp64 ), ldb, &
                              ap( kc+1 ), 1_ilp64, cone,b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and ipiv(k).
                 kp = ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 k = k - 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! multiply by inv(l**h(k-1)), where l(k-1) is the transformation
                 ! stored in columns k-1 and k of a.
                 if( k<n ) then
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp64 ), ldb, &
                              ap( kc+1 ), 1_ilp64, cone,b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k-1, 1_ilp64 ), ldb )
                    call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', n-k, nrhs, -cone,b( k+1, 1_ilp64 ), ldb, &
                              ap( kc-( n-k ) ), 1_ilp64, cone,b( k-1, 1_ilp64 ), ldb )
                    call stdlib_I64_zlacgv( nrhs, b( k-1, 1_ilp64 ), ldb )
                 end if
                 ! interchange rows k and -ipiv(k).
                 kp = -ipiv( k )
                 if( kp/=k )call stdlib_I64_zswap( nrhs, b( k, 1_ilp64 ), ldb, b( kp, 1_ilp64 ), ldb )
                 kc = kc - ( n-k+2 )
                 k = k - 2_ilp64
              end if
              go to 90
              100 continue
           end if
           return
     end subroutine stdlib_I64_zhptrs




     pure module subroutine stdlib_I64_chptri( uplo, n, ap, ipiv, work, info )
     !! CHPTRI computes the inverse of a complex Hermitian indefinite matrix
     !! A in packed storage using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by CHPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: j, k, kc, kcnext, kp, kpc, kstep, kx, npp
           real(sp) :: ak, akp1, d, t
           complex(sp) :: akkp1, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHPTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              kp = n*( n+1 ) / 2_ilp64
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. ap( kp )==czero )return
                 kp = kp - info
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              kp = 1_ilp64
              do info = 1, n
                 if( ipiv( info )>0 .and. ap( kp )==czero )return
                 kp = kp + n - info + 1_ilp64
              end do
           end if
           info = 0_ilp64
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp64
              kc = 1_ilp64
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              kcnext = kc + k
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc+k-1 ) = one / real( ap( kc+k-1 ),KIND=sp)
                 ! compute column k of the inverse.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_ccopy( k-1, ap( kc ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_chpmv( uplo, k-1, -cone, ap, work, 1_ilp64, czero,ap( kc ), 1_ilp64 )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -real( stdlib_I64_cdotc( k-1, work, 1_ilp64, ap( kc ), 1_ilp64 ),&
                              KIND=sp)
                 end if
                 kstep = 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( ap( kcnext+k-1 ) )
                 ak = real( ap( kc+k-1 ),KIND=sp) / t
                 akp1 = real( ap( kcnext+k ),KIND=sp) / t
                 akkp1 = ap( kcnext+k-1 ) / t
                 d = t*( ak*akp1-one )
                 ap( kc+k-1 ) = akp1 / d
                 ap( kcnext+k ) = ak / d
                 ap( kcnext+k-1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_ccopy( k-1, ap( kc ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_chpmv( uplo, k-1, -cone, ap, work, 1_ilp64, czero,ap( kc ), 1_ilp64 )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -real( stdlib_I64_cdotc( k-1, work, 1_ilp64, ap( kc ), 1_ilp64 ),&
                              KIND=sp)
                    ap( kcnext+k-1 ) = ap( kcnext+k-1 ) -stdlib_I64_cdotc( k-1, ap( kc ), 1_ilp64, ap( &
                              kcnext ),1_ilp64 )
                    call stdlib_I64_ccopy( k-1, ap( kcnext ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_chpmv( uplo, k-1, -cone, ap, work, 1_ilp64, czero,ap( kcnext ), 1_ilp64 )
                              
                    ap( kcnext+k ) = ap( kcnext+k ) -real( stdlib_I64_cdotc( k-1, work, 1_ilp64, ap( kcnext &
                              ),1_ilp64 ),KIND=sp)
                 end if
                 kstep = 2_ilp64
                 kcnext = kcnext + k + 1_ilp64
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 kpc = ( kp-1 )*kp / 2_ilp64 + 1_ilp64
                 call stdlib_I64_cswap( kp-1, ap( kc ), 1_ilp64, ap( kpc ), 1_ilp64 )
                 kx = kpc + kp - 1_ilp64
                 do j = kp + 1, k - 1
                    kx = kx + j - 1_ilp64
                    temp = conjg( ap( kc+j-1 ) )
                    ap( kc+j-1 ) = conjg( ap( kx ) )
                    ap( kx ) = temp
                 end do
                 ap( kc+kp-1 ) = conjg( ap( kc+kp-1 ) )
                 temp = ap( kc+k-1 )
                 ap( kc+k-1 ) = ap( kpc+kp-1 )
                 ap( kpc+kp-1 ) = temp
                 if( kstep==2_ilp64 ) then
                    temp = ap( kc+k+k-1 )
                    ap( kc+k+k-1 ) = ap( kc+k+kp-1 )
                    ap( kc+k+kp-1 ) = temp
                 end if
              end if
              k = k + kstep
              kc = kcnext
              go to 30
              50 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              npp = n*( n+1 ) / 2_ilp64
              k = n
              kc = npp
              60 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 80
              kcnext = kc - ( n-k+2 )
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc ) = one / real( ap( kc ),KIND=sp)
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_I64_ccopy( n-k, ap( kc+1 ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_chpmv( uplo, n-k, -cone, ap( kc+n-k+1 ), work, 1_ilp64,czero, ap( kc+1 )&
                              , 1_ilp64 )
                    ap( kc ) = ap( kc ) - real( stdlib_I64_cdotc( n-k, work, 1_ilp64,ap( kc+1 ), 1_ilp64 ),&
                              KIND=sp)
                 end if
                 kstep = 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( ap( kcnext+1 ) )
                 ak = real( ap( kcnext ),KIND=sp) / t
                 akp1 = real( ap( kc ),KIND=sp) / t
                 akkp1 = ap( kcnext+1 ) / t
                 d = t*( ak*akp1-one )
                 ap( kcnext ) = akp1 / d
                 ap( kc ) = ak / d
                 ap( kcnext+1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_I64_ccopy( n-k, ap( kc+1 ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_chpmv( uplo, n-k, -cone, ap( kc+( n-k+1 ) ), work,1_ilp64, czero, ap( &
                              kc+1 ), 1_ilp64 )
                    ap( kc ) = ap( kc ) - real( stdlib_I64_cdotc( n-k, work, 1_ilp64,ap( kc+1 ), 1_ilp64 ),&
                              KIND=sp)
                    ap( kcnext+1 ) = ap( kcnext+1 ) -stdlib_I64_cdotc( n-k, ap( kc+1 ), 1_ilp64,ap( kcnext+&
                              2_ilp64 ), 1_ilp64 )
                    call stdlib_I64_ccopy( n-k, ap( kcnext+2 ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_chpmv( uplo, n-k, -cone, ap( kc+( n-k+1 ) ), work,1_ilp64, czero, ap( &
                              kcnext+2 ), 1_ilp64 )
                    ap( kcnext ) = ap( kcnext ) -real( stdlib_I64_cdotc( n-k, work, 1_ilp64, ap( kcnext+2 ),&
                              1_ilp64 ),KIND=sp)
                 end if
                 kstep = 2_ilp64
                 kcnext = kcnext - ( n-k+3 )
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 kpc = npp - ( n-kp+1 )*( n-kp+2 ) / 2_ilp64 + 1_ilp64
                 if( kp<n )call stdlib_I64_cswap( n-kp, ap( kc+kp-k+1 ), 1_ilp64, ap( kpc+1 ), 1_ilp64 )
                 kx = kc + kp - k
                 do j = k + 1, kp - 1
                    kx = kx + n - j + 1_ilp64
                    temp = conjg( ap( kc+j-k ) )
                    ap( kc+j-k ) = conjg( ap( kx ) )
                    ap( kx ) = temp
                 end do
                 ap( kc+kp-k ) = conjg( ap( kc+kp-k ) )
                 temp = ap( kc )
                 ap( kc ) = ap( kpc )
                 ap( kpc ) = temp
                 if( kstep==2_ilp64 ) then
                    temp = ap( kc-n+k-1 )
                    ap( kc-n+k-1 ) = ap( kc-n+kp-1 )
                    ap( kc-n+kp-1 ) = temp
                 end if
              end if
              k = k - kstep
              kc = kcnext
              go to 60
              80 continue
           end if
           return
     end subroutine stdlib_I64_chptri

     pure module subroutine stdlib_I64_zhptri( uplo, n, ap, ipiv, work, info )
     !! ZHPTRI computes the inverse of a complex Hermitian indefinite matrix
     !! A in packed storage using the factorization A = U*D*U**H or
     !! A = L*D*L**H computed by ZHPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: j, k, kc, kcnext, kp, kpc, kstep, kx, npp
           real(dp) :: ak, akp1, d, t
           complex(dp) :: akkp1, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHPTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check that the diagonal matrix d is nonsingular.
           if( upper ) then
              ! upper triangular storage: examine d from bottom to top
              kp = n*( n+1 ) / 2_ilp64
              do info = n, 1, -1
                 if( ipiv( info )>0 .and. ap( kp )==czero )return
                 kp = kp - info
              end do
           else
              ! lower triangular storage: examine d from top to bottom.
              kp = 1_ilp64
              do info = 1, n
                 if( ipiv( info )>0 .and. ap( kp )==czero )return
                 kp = kp + n - info + 1_ilp64
              end do
           end if
           info = 0_ilp64
           if( upper ) then
              ! compute inv(a) from the factorization a = u*d*u**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              k = 1_ilp64
              kc = 1_ilp64
              30 continue
              ! if k > n, exit from loop.
              if( k>n )go to 50
              kcnext = kc + k
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc+k-1 ) = one / real( ap( kc+k-1 ),KIND=dp)
                 ! compute column k of the inverse.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_zcopy( k-1, ap( kc ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_zhpmv( uplo, k-1, -cone, ap, work, 1_ilp64, czero,ap( kc ), 1_ilp64 )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -real( stdlib_I64_zdotc( k-1, work, 1_ilp64, ap( kc ), 1_ilp64 ),&
                              KIND=dp)
                 end if
                 kstep = 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( ap( kcnext+k-1 ) )
                 ak = real( ap( kc+k-1 ),KIND=dp) / t
                 akp1 = real( ap( kcnext+k ),KIND=dp) / t
                 akkp1 = ap( kcnext+k-1 ) / t
                 d = t*( ak*akp1-one )
                 ap( kc+k-1 ) = akp1 / d
                 ap( kcnext+k ) = ak / d
                 ap( kcnext+k-1 ) = -akkp1 / d
                 ! compute columns k and k+1 of the inverse.
                 if( k>1_ilp64 ) then
                    call stdlib_I64_zcopy( k-1, ap( kc ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_zhpmv( uplo, k-1, -cone, ap, work, 1_ilp64, czero,ap( kc ), 1_ilp64 )
                    ap( kc+k-1 ) = ap( kc+k-1 ) -real( stdlib_I64_zdotc( k-1, work, 1_ilp64, ap( kc ), 1_ilp64 ),&
                              KIND=dp)
                    ap( kcnext+k-1 ) = ap( kcnext+k-1 ) -stdlib_I64_zdotc( k-1, ap( kc ), 1_ilp64, ap( &
                              kcnext ),1_ilp64 )
                    call stdlib_I64_zcopy( k-1, ap( kcnext ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_zhpmv( uplo, k-1, -cone, ap, work, 1_ilp64, czero,ap( kcnext ), 1_ilp64 )
                              
                    ap( kcnext+k ) = ap( kcnext+k ) -real( stdlib_I64_zdotc( k-1, work, 1_ilp64, ap( kcnext &
                              ),1_ilp64 ),KIND=dp)
                 end if
                 kstep = 2_ilp64
                 kcnext = kcnext + k + 1_ilp64
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the leading
                 ! submatrix a(1:k+1,1:k+1)
                 kpc = ( kp-1 )*kp / 2_ilp64 + 1_ilp64
                 call stdlib_I64_zswap( kp-1, ap( kc ), 1_ilp64, ap( kpc ), 1_ilp64 )
                 kx = kpc + kp - 1_ilp64
                 do j = kp + 1, k - 1
                    kx = kx + j - 1_ilp64
                    temp = conjg( ap( kc+j-1 ) )
                    ap( kc+j-1 ) = conjg( ap( kx ) )
                    ap( kx ) = temp
                 end do
                 ap( kc+kp-1 ) = conjg( ap( kc+kp-1 ) )
                 temp = ap( kc+k-1 )
                 ap( kc+k-1 ) = ap( kpc+kp-1 )
                 ap( kpc+kp-1 ) = temp
                 if( kstep==2_ilp64 ) then
                    temp = ap( kc+k+k-1 )
                    ap( kc+k+k-1 ) = ap( kc+k+kp-1 )
                    ap( kc+k+kp-1 ) = temp
                 end if
              end if
              k = k + kstep
              kc = kcnext
              go to 30
              50 continue
           else
              ! compute inv(a) from the factorization a = l*d*l**h.
              ! k is the main loop index, increasing from 1 to n in steps of
              ! 1 or 2, depending on the size of the diagonal blocks.
              npp = n*( n+1 ) / 2_ilp64
              k = n
              kc = npp
              60 continue
              ! if k < 1, exit from loop.
              if( k<1 )go to 80
              kcnext = kc - ( n-k+2 )
              if( ipiv( k )>0_ilp64 ) then
                 ! 1 x 1 diagonal block
                 ! invert the diagonal block.
                 ap( kc ) = one / real( ap( kc ),KIND=dp)
                 ! compute column k of the inverse.
                 if( k<n ) then
                    call stdlib_I64_zcopy( n-k, ap( kc+1 ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_zhpmv( uplo, n-k, -cone, ap( kc+n-k+1 ), work, 1_ilp64,czero, ap( kc+1 )&
                              , 1_ilp64 )
                    ap( kc ) = ap( kc ) - real( stdlib_I64_zdotc( n-k, work, 1_ilp64,ap( kc+1 ), 1_ilp64 ),&
                              KIND=dp)
                 end if
                 kstep = 1_ilp64
              else
                 ! 2 x 2 diagonal block
                 ! invert the diagonal block.
                 t = abs( ap( kcnext+1 ) )
                 ak = real( ap( kcnext ),KIND=dp) / t
                 akp1 = real( ap( kc ),KIND=dp) / t
                 akkp1 = ap( kcnext+1 ) / t
                 d = t*( ak*akp1-one )
                 ap( kcnext ) = akp1 / d
                 ap( kc ) = ak / d
                 ap( kcnext+1 ) = -akkp1 / d
                 ! compute columns k-1 and k of the inverse.
                 if( k<n ) then
                    call stdlib_I64_zcopy( n-k, ap( kc+1 ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_zhpmv( uplo, n-k, -cone, ap( kc+( n-k+1 ) ), work,1_ilp64, czero, ap( &
                              kc+1 ), 1_ilp64 )
                    ap( kc ) = ap( kc ) - real( stdlib_I64_zdotc( n-k, work, 1_ilp64,ap( kc+1 ), 1_ilp64 ),&
                              KIND=dp)
                    ap( kcnext+1 ) = ap( kcnext+1 ) -stdlib_I64_zdotc( n-k, ap( kc+1 ), 1_ilp64,ap( kcnext+&
                              2_ilp64 ), 1_ilp64 )
                    call stdlib_I64_zcopy( n-k, ap( kcnext+2 ), 1_ilp64, work, 1_ilp64 )
                    call stdlib_I64_zhpmv( uplo, n-k, -cone, ap( kc+( n-k+1 ) ), work,1_ilp64, czero, ap( &
                              kcnext+2 ), 1_ilp64 )
                    ap( kcnext ) = ap( kcnext ) -real( stdlib_I64_zdotc( n-k, work, 1_ilp64, ap( kcnext+2 ),&
                              1_ilp64 ),KIND=dp)
                 end if
                 kstep = 2_ilp64
                 kcnext = kcnext - ( n-k+3 )
              end if
              kp = abs( ipiv( k ) )
              if( kp/=k ) then
                 ! interchange rows and columns k and kp in the trailing
                 ! submatrix a(k-1:n,k-1:n)
                 kpc = npp - ( n-kp+1 )*( n-kp+2 ) / 2_ilp64 + 1_ilp64
                 if( kp<n )call stdlib_I64_zswap( n-kp, ap( kc+kp-k+1 ), 1_ilp64, ap( kpc+1 ), 1_ilp64 )
                 kx = kc + kp - k
                 do j = k + 1, kp - 1
                    kx = kx + n - j + 1_ilp64
                    temp = conjg( ap( kc+j-k ) )
                    ap( kc+j-k ) = conjg( ap( kx ) )
                    ap( kx ) = temp
                 end do
                 ap( kc+kp-k ) = conjg( ap( kc+kp-k ) )
                 temp = ap( kc )
                 ap( kc ) = ap( kpc )
                 ap( kpc ) = temp
                 if( kstep==2_ilp64 ) then
                    temp = ap( kc-n+k-1 )
                    ap( kc-n+k-1 ) = ap( kc-n+kp-1 )
                    ap( kc-n+kp-1 ) = temp
                 end if
              end if
              k = k - kstep
              kc = kcnext
              go to 60
              80 continue
           end if
           return
     end subroutine stdlib_I64_zhptri



end submodule stdlib_lapack_solve_ldl_comp3
