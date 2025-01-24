submodule(stdlib_lapack_solve) stdlib_lapack_solve_ldl
  implicit none


  contains

     pure module subroutine stdlib_ssysv( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! SSYSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
     !! used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_ssytrf( uplo, n, a, lda, ipiv, work, -1_ilp, info )
                 lwkopt = work(1_ilp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYSV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_ssytrf( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              if ( lwork<n ) then
              ! solve with trs ( use level blas 2)
                 call stdlib_ssytrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
              else
              ! solve with trs2 ( use level blas 3)
                 call stdlib_ssytrs2( uplo,n,nrhs,a,lda,ipiv,b,ldb,work,info )
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssysv

     pure module subroutine stdlib_dsysv( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! DSYSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
     !! used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_dsytrf( uplo, n, a, lda, ipiv, work, -1_ilp, info )
                 lwkopt = work(1_ilp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYSV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_dsytrf( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              if ( lwork<n ) then
              ! solve with trs ( use level blas 2)
                 call stdlib_dsytrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
              else
              ! solve with trs2 ( use level blas 3)
                 call stdlib_dsytrs2( uplo,n,nrhs,a,lda,ipiv,b,ldb,work,info )
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsysv


     pure module subroutine stdlib_csysv( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! CSYSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
     !! used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_csytrf( uplo, n, a, lda, ipiv, work, -1_ilp, info )
                 lwkopt = real( work(1_ilp),KIND=sp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYSV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_csytrf( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              if ( lwork<n ) then
              ! solve with trs ( use level blas 2)
                 call stdlib_csytrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
              else
              ! solve with trs2 ( use level blas 3)
                 call stdlib_csytrs2( uplo,n,nrhs,a,lda,ipiv,b,ldb,work,info )
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_csysv

     pure module subroutine stdlib_zsysv( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! ZSYSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
     !! used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_zsytrf( uplo, n, a, lda, ipiv, work, -1_ilp, info )
                 lwkopt = real( work(1_ilp),KIND=dp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYSV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_zsytrf( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              if ( lwork<n ) then
              ! solve with trs ( use level blas 2)
                 call stdlib_zsytrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
              else
              ! solve with trs2 ( use level blas 3)
                 call stdlib_zsytrs2( uplo,n,nrhs,a,lda,ipiv,b,ldb,work,info )
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zsysv




     module subroutine stdlib_ssysvx( fact, uplo, n, nrhs, a, lda, af, ldaf, ipiv, b,ldb, x, ldx, rcond, &
     !! SSYSVX uses the diagonal pivoting factorization to compute the
     !! solution to a real system of linear equations A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ferr, berr, work, lwork,iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, lwork, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
           real(sp), intent(inout) :: af(ldaf,*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, nofact
           integer(ilp) :: lwkopt, nb
           real(sp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -11_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -13_ilp
           else if( lwork<max( 1_ilp, 3_ilp*n ) .and. .not.lquery ) then
              info = -18_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = max( 1_ilp, 3_ilp*n )
              if( nofact ) then
                 nb = stdlib_ilaenv( 1_ilp, 'SSYTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 lwkopt = max( lwkopt, n*nb )
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYSVX', -info )
              return
           else if( lquery ) then
              return
           end if
           if( nofact ) then
              ! compute the factorization a = u*d*u**t or a = l*d*l**t.
              call stdlib_slacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_ssytrf( uplo, n, af, ldaf, ipiv, work, lwork, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_slansy( 'I', uplo, n, a, lda, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_ssycon( uplo, n, af, ldaf, ipiv, anorm, rcond, work, iwork,info )
           ! compute the solution vectors x.
           call stdlib_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_ssytrs( uplo, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_ssyrfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, iwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssysvx

     module subroutine stdlib_dsysvx( fact, uplo, n, nrhs, a, lda, af, ldaf, ipiv, b,ldb, x, ldx, rcond, &
     !! DSYSVX uses the diagonal pivoting factorization to compute the
     !! solution to a real system of linear equations A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ferr, berr, work, lwork,iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, lwork, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
           real(dp), intent(inout) :: af(ldaf,*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, nofact
           integer(ilp) :: lwkopt, nb
           real(dp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -11_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -13_ilp
           else if( lwork<max( 1_ilp, 3_ilp*n ) .and. .not.lquery ) then
              info = -18_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = max( 1_ilp, 3_ilp*n )
              if( nofact ) then
                 nb = stdlib_ilaenv( 1_ilp, 'DSYTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 lwkopt = max( lwkopt, n*nb )
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYSVX', -info )
              return
           else if( lquery ) then
              return
           end if
           if( nofact ) then
              ! compute the factorization a = u*d*u**t or a = l*d*l**t.
              call stdlib_dlacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_dsytrf( uplo, n, af, ldaf, ipiv, work, lwork, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_dlansy( 'I', uplo, n, a, lda, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_dsycon( uplo, n, af, ldaf, ipiv, anorm, rcond, work, iwork,info )
           ! compute the solution vectors x.
           call stdlib_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_dsytrs( uplo, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_dsyrfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, iwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsysvx


     module subroutine stdlib_csysvx( fact, uplo, n, nrhs, a, lda, af, ldaf, ipiv, b,ldb, x, ldx, rcond, &
     !! CSYSVX uses the diagonal pivoting factorization to compute the
     !! solution to a complex system of linear equations A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ferr, berr, work, lwork,rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, lwork, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: af(ldaf,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, nofact
           integer(ilp) :: lwkopt, nb
           real(sp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -11_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -13_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -18_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = max( 1_ilp, 2_ilp*n )
              if( nofact ) then
                 nb = stdlib_ilaenv( 1_ilp, 'CSYTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 lwkopt = max( lwkopt, n*nb )
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYSVX', -info )
              return
           else if( lquery ) then
              return
           end if
           if( nofact ) then
              ! compute the factorization a = u*d*u**t or a = l*d*l**t.
              call stdlib_clacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_csytrf( uplo, n, af, ldaf, ipiv, work, lwork, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_clansy( 'I', uplo, n, a, lda, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_csycon( uplo, n, af, ldaf, ipiv, anorm, rcond, work, info )
           ! compute the solution vectors x.
           call stdlib_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_csytrs( uplo, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_csyrfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_csysvx

     module subroutine stdlib_zsysvx( fact, uplo, n, nrhs, a, lda, af, ldaf, ipiv, b,ldb, x, ldx, rcond, &
     !! ZSYSVX uses the diagonal pivoting factorization to compute the
     !! solution to a complex system of linear equations A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ferr, berr, work, lwork,rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, lwork, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: af(ldaf,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, nofact
           integer(ilp) :: lwkopt, nb
           real(dp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -11_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -13_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -18_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = max( 1_ilp, 2_ilp*n )
              if( nofact ) then
                 nb = stdlib_ilaenv( 1_ilp, 'ZSYTRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 lwkopt = max( lwkopt, n*nb )
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYSVX', -info )
              return
           else if( lquery ) then
              return
           end if
           if( nofact ) then
              ! compute the factorization a = u*d*u**t or a = l*d*l**t.
              call stdlib_zlacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_zsytrf( uplo, n, af, ldaf, ipiv, work, lwork, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_zlansy( 'I', uplo, n, a, lda, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_zsycon( uplo, n, af, ldaf, ipiv, anorm, rcond, work, info )
           ! compute the solution vectors x.
           call stdlib_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_zsytrs( uplo, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_zsyrfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zsysvx




     pure module subroutine stdlib_ssysv_rk( uplo, n, nrhs, a, lda, e, ipiv, b, ldb,work, lwork, info )
     !! SSYSV_RK computes the solution to a real system of linear
     !! equations A * X = B, where A is an N-by-N symmetric matrix
     !! and X and B are N-by-NRHS matrices.
     !! The bounded Bunch-Kaufman (rook) diagonal pivoting method is used
     !! to factor A as
     !! A = P*U*D*(U**T)*(P**T),  if UPLO = 'U', or
     !! A = P*L*D*(L**T)*(P**T),  if UPLO = 'L',
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! SSYTRF_RK is called to compute the factorization of a real
     !! symmetric matrix.  The factored form of A is then used to solve
     !! the system of equations A * X = B by calling BLAS3 routine SSYTRS_3.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: e(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_ssytrf_rk( uplo, n, a, lda, e, ipiv, work, -1_ilp, info )
                 lwkopt = work(1_ilp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYSV_RK ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = p*u*d*(u**t)*(p**t) or
           ! a = p*u*d*(u**t)*(p**t).
           call stdlib_ssytrf_rk( uplo, n, a, lda, e, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b with blas3 solver, overwriting b with x.
              call stdlib_ssytrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb, info )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssysv_rk

     pure module subroutine stdlib_dsysv_rk( uplo, n, nrhs, a, lda, e, ipiv, b, ldb,work, lwork, info )
     !! DSYSV_RK computes the solution to a real system of linear
     !! equations A * X = B, where A is an N-by-N symmetric matrix
     !! and X and B are N-by-NRHS matrices.
     !! The bounded Bunch-Kaufman (rook) diagonal pivoting method is used
     !! to factor A as
     !! A = P*U*D*(U**T)*(P**T),  if UPLO = 'U', or
     !! A = P*L*D*(L**T)*(P**T),  if UPLO = 'L',
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! DSYTRF_RK is called to compute the factorization of a real
     !! symmetric matrix.  The factored form of A is then used to solve
     !! the system of equations A * X = B by calling BLAS3 routine DSYTRS_3.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: e(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_dsytrf_rk( uplo, n, a, lda, e, ipiv, work, -1_ilp, info )
                 lwkopt = work(1_ilp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYSV_RK ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = p*u*d*(u**t)*(p**t) or
           ! a = p*u*d*(u**t)*(p**t).
           call stdlib_dsytrf_rk( uplo, n, a, lda, e, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b with blas3 solver, overwriting b with x.
              call stdlib_dsytrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb, info )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsysv_rk


     pure module subroutine stdlib_csysv_rk( uplo, n, nrhs, a, lda, e, ipiv, b, ldb, work,lwork, info )
     !! CSYSV_RK computes the solution to a complex system of linear
     !! equations A * X = B, where A is an N-by-N symmetric matrix
     !! and X and B are N-by-NRHS matrices.
     !! The bounded Bunch-Kaufman (rook) diagonal pivoting method is used
     !! to factor A as
     !! A = P*U*D*(U**T)*(P**T),  if UPLO = 'U', or
     !! A = P*L*D*(L**T)*(P**T),  if UPLO = 'L',
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! CSYTRF_RK is called to compute the factorization of a complex
     !! symmetric matrix.  The factored form of A is then used to solve
     !! the system of equations A * X = B by calling BLAS3 routine CSYTRS_3.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: e(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_csytrf_rk( uplo, n, a, lda, e, ipiv, work, -1_ilp, info )
                 lwkopt = real( work(1_ilp),KIND=sp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYSV_RK ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_csytrf_rk( uplo, n, a, lda, e, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b with blas3 solver, overwriting b with x.
              call stdlib_csytrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb, info )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_csysv_rk

     pure module subroutine stdlib_zsysv_rk( uplo, n, nrhs, a, lda, e, ipiv, b, ldb, work,lwork, info )
     !! ZSYSV_RK computes the solution to a complex system of linear
     !! equations A * X = B, where A is an N-by-N symmetric matrix
     !! and X and B are N-by-NRHS matrices.
     !! The bounded Bunch-Kaufman (rook) diagonal pivoting method is used
     !! to factor A as
     !! A = P*U*D*(U**T)*(P**T),  if UPLO = 'U', or
     !! A = P*L*D*(L**T)*(P**T),  if UPLO = 'L',
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**T (or L**T) is the transpose of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is symmetric and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! ZSYTRF_RK is called to compute the factorization of a complex
     !! symmetric matrix.  The factored form of A is then used to solve
     !! the system of equations A * X = B by calling BLAS3 routine ZSYTRS_3.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: e(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_zsytrf_rk( uplo, n, a, lda, e, ipiv, work, -1_ilp, info )
                 lwkopt = real( work(1_ilp),KIND=dp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYSV_RK ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = p*u*d*(u**t)*(p**t) or
           ! a = p*u*d*(u**t)*(p**t).
           call stdlib_zsytrf_rk( uplo, n, a, lda, e, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b with blas3 solver, overwriting b with x.
              call stdlib_zsytrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb, info )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zsysv_rk




     pure module subroutine stdlib_ssysv_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! SSYSV_ROOK computes the solution to a real system of linear
     !! equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! SSYTRF_ROOK is called to compute the factorization of a real
     !! symmetric matrix A using the bounded Bunch-Kaufman ("rook") diagonal
     !! pivoting method.
     !! The factored form of A is then used to solve the system
     !! of equations A * X = B by calling SSYTRS_ROOK.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_ssytrf_rook( uplo, n, a, lda, ipiv, work, -1_ilp, info )
                 lwkopt = work(1_ilp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYSV_ROOK ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_ssytrf_rook( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              ! solve with trs_rook ( use level 2 blas)
              call stdlib_ssytrs_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssysv_rook

     pure module subroutine stdlib_dsysv_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! DSYSV_ROOK computes the solution to a real system of linear
     !! equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! DSYTRF_ROOK is called to compute the factorization of a real
     !! symmetric matrix A using the bounded Bunch-Kaufman ("rook") diagonal
     !! pivoting method.
     !! The factored form of A is then used to solve the system
     !! of equations A * X = B by calling DSYTRS_ROOK.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_dsytrf_rook( uplo, n, a, lda, ipiv, work, -1_ilp, info )
                 lwkopt = work(1_ilp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYSV_ROOK ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_dsytrf_rook( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              ! solve with trs_rook ( use level 2 blas)
              call stdlib_dsytrs_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsysv_rook


     pure module subroutine stdlib_csysv_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! CSYSV_ROOK computes the solution to a complex system of linear
     !! equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! CSYTRF_ROOK is called to compute the factorization of a complex
     !! symmetric matrix A using the bounded Bunch-Kaufman ("rook") diagonal
     !! pivoting method.
     !! The factored form of A is then used to solve the system
     !! of equations A * X = B by calling CSYTRS_ROOK.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_csytrf_rook( uplo, n, a, lda, ipiv, work, -1_ilp, info )
                 lwkopt = real( work(1_ilp),KIND=sp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYSV_ROOK ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_csytrf_rook( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              ! solve with trs_rook ( use level 2 blas)
              call stdlib_csytrs_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_csysv_rook

     pure module subroutine stdlib_zsysv_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! ZSYSV_ROOK computes the solution to a complex system of linear
     !! equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is symmetric and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! ZSYTRF_ROOK is called to compute the factorization of a complex
     !! symmetric matrix A using the bounded Bunch-Kaufman ("rook") diagonal
     !! pivoting method.
     !! The factored form of A is then used to solve the system
     !! of equations A * X = B by calling ZSYTRS_ROOK.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_zsytrf_rook( uplo, n, a, lda, ipiv, work, -1_ilp, info )
                 lwkopt = real( work(1_ilp),KIND=dp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYSV_ROOK ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_zsytrf_rook( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              ! solve with trs_rook ( use level 2 blas)
              call stdlib_zsytrs_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zsysv_rook




     pure module subroutine stdlib_chesv( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! CHESV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian matrix and X and B are N-by-NRHS
     !! matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**H,  if UPLO = 'U', or
     !! A = L * D * L**H,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is Hermitian and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
     !! used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'CHETRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHESV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**h or a = l*d*l**h.
           call stdlib_chetrf( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              if ( lwork<n ) then
              ! solve with trs ( use level blas 2)
                 call stdlib_chetrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
              else
              ! solve with trs2 ( use level blas 3)
                 call stdlib_chetrs2( uplo,n,nrhs,a,lda,ipiv,b,ldb,work,info )
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_chesv

     pure module subroutine stdlib_zhesv( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! ZHESV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian matrix and X and B are N-by-NRHS
     !! matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**H,  if UPLO = 'U', or
     !! A = L * D * L**H,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is Hermitian and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
     !! used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'ZHETRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHESV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**h or a = l*d*l**h.
           call stdlib_zhetrf( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              if ( lwork<n ) then
              ! solve with trs ( use level blas 2)
                 call stdlib_zhetrs( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
              else
              ! solve with trs2 ( use level blas 3)
                 call stdlib_zhetrs2( uplo,n,nrhs,a,lda,ipiv,b,ldb,work,info )
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zhesv




     module subroutine stdlib_chesvx( fact, uplo, n, nrhs, a, lda, af, ldaf, ipiv, b,ldb, x, ldx, rcond, &
     !! CHESVX uses the diagonal pivoting factorization to compute the
     !! solution to a complex system of linear equations A * X = B,
     !! where A is an N-by-N Hermitian matrix and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ferr, berr, work, lwork,rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, lwork, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: af(ldaf,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, nofact
           integer(ilp) :: lwkopt, nb
           real(sp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -11_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -13_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -18_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = max( 1_ilp, 2_ilp*n )
              if( nofact ) then
                 nb = stdlib_ilaenv( 1_ilp, 'CHETRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 lwkopt = max( lwkopt, n*nb )
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHESVX', -info )
              return
           else if( lquery ) then
              return
           end if
           if( nofact ) then
              ! compute the factorization a = u*d*u**h or a = l*d*l**h.
              call stdlib_clacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_chetrf( uplo, n, af, ldaf, ipiv, work, lwork, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_clanhe( 'I', uplo, n, a, lda, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_checon( uplo, n, af, ldaf, ipiv, anorm, rcond, work, info )
           ! compute the solution vectors x.
           call stdlib_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_chetrs( uplo, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_cherfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_chesvx

     module subroutine stdlib_zhesvx( fact, uplo, n, nrhs, a, lda, af, ldaf, ipiv, b,ldb, x, ldx, rcond, &
     !! ZHESVX uses the diagonal pivoting factorization to compute the
     !! solution to a complex system of linear equations A * X = B,
     !! where A is an N-by-N Hermitian matrix and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ferr, berr, work, lwork,rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, lwork, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: af(ldaf,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, nofact
           integer(ilp) :: lwkopt, nb
           real(dp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           lquery = ( lwork==-1_ilp )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -11_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -13_ilp
           else if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
              info = -18_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = max( 1_ilp, 2_ilp*n )
              if( nofact ) then
                 nb = stdlib_ilaenv( 1_ilp, 'ZHETRF', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 lwkopt = max( lwkopt, n*nb )
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHESVX', -info )
              return
           else if( lquery ) then
              return
           end if
           if( nofact ) then
              ! compute the factorization a = u*d*u**h or a = l*d*l**h.
              call stdlib_zlacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_zhetrf( uplo, n, af, ldaf, ipiv, work, lwork, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_zlanhe( 'I', uplo, n, a, lda, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_zhecon( uplo, n, af, ldaf, ipiv, anorm, rcond, work, info )
           ! compute the solution vectors x.
           call stdlib_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_zhetrs( uplo, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_zherfs( uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zhesvx




     pure module subroutine stdlib_chesv_rk( uplo, n, nrhs, a, lda, e, ipiv, b, ldb, work,lwork, info )
     !! CHESV_RK computes the solution to a complex system of linear
     !! equations A * X = B, where A is an N-by-N Hermitian matrix
     !! and X and B are N-by-NRHS matrices.
     !! The bounded Bunch-Kaufman (rook) diagonal pivoting method is used
     !! to factor A as
     !! A = P*U*D*(U**H)*(P**T),  if UPLO = 'U', or
     !! A = P*L*D*(L**H)*(P**T),  if UPLO = 'L',
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**H (or L**H) is the conjugate of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is Hermitian and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! CHETRF_RK is called to compute the factorization of a complex
     !! Hermitian matrix.  The factored form of A is then used to solve
     !! the system of equations A * X = B by calling BLAS3 routine CHETRS_3.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: e(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_chetrf_rk( uplo, n, a, lda, e, ipiv, work, -1_ilp, info )
                 lwkopt = real( work(1_ilp),KIND=sp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHESV_RK ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_chetrf_rk( uplo, n, a, lda, e, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b with blas3 solver, overwriting b with x.
              call stdlib_chetrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb, info )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_chesv_rk

     pure module subroutine stdlib_zhesv_rk( uplo, n, nrhs, a, lda, e, ipiv, b, ldb, work,lwork, info )
     !! ZHESV_RK computes the solution to a complex system of linear
     !! equations A * X = B, where A is an N-by-N Hermitian matrix
     !! and X and B are N-by-NRHS matrices.
     !! The bounded Bunch-Kaufman (rook) diagonal pivoting method is used
     !! to factor A as
     !! A = P*U*D*(U**H)*(P**T),  if UPLO = 'U', or
     !! A = P*L*D*(L**H)*(P**T),  if UPLO = 'L',
     !! where U (or L) is unit upper (or lower) triangular matrix,
     !! U**H (or L**H) is the conjugate of U (or L), P is a permutation
     !! matrix, P**T is the transpose of P, and D is Hermitian and block
     !! diagonal with 1-by-1 and 2-by-2 diagonal blocks.
     !! ZHETRF_RK is called to compute the factorization of a complex
     !! Hermitian matrix.  The factored form of A is then used to solve
     !! the system of equations A * X = B by calling BLAS3 routine ZHETRS_3.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: e(*), work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -11_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 call stdlib_zhetrf_rk( uplo, n, a, lda, e, ipiv, work, -1_ilp, info )
                 lwkopt = real( work(1_ilp),KIND=dp)
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHESV_RK ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = p*u*d*(u**h)*(p**t) or
           ! a = p*u*d*(u**h)*(p**t).
           call stdlib_zhetrf_rk( uplo, n, a, lda, e, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b with blas3 solver, overwriting b with x.
              call stdlib_zhetrs_3( uplo, n, nrhs, a, lda, e, ipiv, b, ldb, info )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zhesv_rk




     pure module subroutine stdlib_chesv_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! CHESV_ROOK computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian matrix and X and B are N-by-NRHS
     !! matrices.
     !! The bounded Bunch-Kaufman ("rook") diagonal pivoting method is used
     !! to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is Hermitian and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! CHETRF_ROOK is called to compute the factorization of a complex
     !! Hermition matrix A using the bounded Bunch-Kaufman ("rook") diagonal
     !! pivoting method.
     !! The factored form of A is then used to solve the system
     !! of equations A * X = B by calling CHETRS_ROOK (uses BLAS 2).
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'CHETRF_ROOK', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHESV_ROOK ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**h or a = l*d*l**h.
           call stdlib_chetrf_rook( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              ! solve with trs ( use level blas 2)
              call stdlib_chetrs_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_chesv_rook

     pure module subroutine stdlib_zhesv_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! ZHESV_ROOK computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian matrix and X and B are N-by-NRHS
     !! matrices.
     !! The bounded Bunch-Kaufman ("rook") diagonal pivoting method is used
     !! to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and D is Hermitian and block diagonal with
     !! 1-by-1 and 2-by-2 diagonal blocks.
     !! ZHETRF_ROOK is called to compute the factorization of a complex
     !! Hermition matrix A using the bounded Bunch-Kaufman ("rook") diagonal
     !! pivoting method.
     !! The factored form of A is then used to solve the system
     !! of equations A * X = B by calling ZHETRS_ROOK (uses BLAS 2).
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              if( n==0_ilp ) then
                 lwkopt = 1_ilp
              else
                 nb = stdlib_ilaenv( 1_ilp, 'ZHETRF_ROOK', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                 lwkopt = n*nb
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHESV_ROOK ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u*d*u**h or a = l*d*l**h.
           call stdlib_zhetrf_rook( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              ! solve with trs ( use level blas 2)
              call stdlib_zhetrs_rook( uplo, n, nrhs, a, lda, ipiv, b, ldb, info )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zhesv_rook




     pure module subroutine stdlib_sspsv( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! SSPSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix stored in packed format and X
     !! and B are N-by-NRHS matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, D is symmetric and block diagonal with 1-by-1
     !! and 2-by-2 diagonal blocks.  The factored form of A is then used to
     !! solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: ap(*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPSV ', -info )
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_ssptrf( uplo, n, ap, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_ssptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
           end if
           return
     end subroutine stdlib_sspsv

     pure module subroutine stdlib_dspsv( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! DSPSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix stored in packed format and X
     !! and B are N-by-NRHS matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, D is symmetric and block diagonal with 1-by-1
     !! and 2-by-2 diagonal blocks.  The factored form of A is then used to
     !! solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: ap(*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPSV ', -info )
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_dsptrf( uplo, n, ap, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_dsptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
           end if
           return
     end subroutine stdlib_dspsv


     pure module subroutine stdlib_cspsv( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! CSPSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix stored in packed format and X
     !! and B are N-by-NRHS matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, D is symmetric and block diagonal with 1-by-1
     !! and 2-by-2 diagonal blocks.  The factored form of A is then used to
     !! solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: ap(*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSPSV ', -info )
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_csptrf( uplo, n, ap, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_csptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
           end if
           return
     end subroutine stdlib_cspsv

     pure module subroutine stdlib_zspsv( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! ZSPSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix stored in packed format and X
     !! and B are N-by-NRHS matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**T,  if UPLO = 'U', or
     !! A = L * D * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, D is symmetric and block diagonal with 1-by-1
     !! and 2-by-2 diagonal blocks.  The factored form of A is then used to
     !! solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: ap(*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSPSV ', -info )
              return
           end if
           ! compute the factorization a = u*d*u**t or a = l*d*l**t.
           call stdlib_zsptrf( uplo, n, ap, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_zsptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
           end if
           return
     end subroutine stdlib_zspsv




     module subroutine stdlib_sspsvx( fact, uplo, n, nrhs, ap, afp, ipiv, b, ldb, x,ldx, rcond, ferr, &
     !! SSPSVX uses the diagonal pivoting factorization A = U*D*U**T or
     !! A = L*D*L**T to compute the solution to a real system of linear
     !! equations A * X = B, where A is an N-by-N symmetric matrix stored
     !! in packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               berr, work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: afp(*)
           real(sp), intent(in) :: ap(*), b(ldb,*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact
           real(sp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the factorization a = u*d*u**t or a = l*d*l**t.
              call stdlib_scopy( n*( n+1 ) / 2_ilp, ap, 1_ilp, afp, 1_ilp )
              call stdlib_ssptrf( uplo, n, afp, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_slansp( 'I', uplo, n, ap, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_sspcon( uplo, n, afp, ipiv, anorm, rcond, work, iwork, info )
           ! compute the solution vectors x.
           call stdlib_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_ssptrs( uplo, n, nrhs, afp, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_ssprfs( uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx, ferr,berr, work, &
                     iwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_sspsvx

     module subroutine stdlib_dspsvx( fact, uplo, n, nrhs, ap, afp, ipiv, b, ldb, x,ldx, rcond, ferr, &
     !! DSPSVX uses the diagonal pivoting factorization A = U*D*U**T or
     !! A = L*D*L**T to compute the solution to a real system of linear
     !! equations A * X = B, where A is an N-by-N symmetric matrix stored
     !! in packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               berr, work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: afp(*)
           real(dp), intent(in) :: ap(*), b(ldb,*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact
           real(dp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the factorization a = u*d*u**t or a = l*d*l**t.
              call stdlib_dcopy( n*( n+1 ) / 2_ilp, ap, 1_ilp, afp, 1_ilp )
              call stdlib_dsptrf( uplo, n, afp, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_dlansp( 'I', uplo, n, ap, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_dspcon( uplo, n, afp, ipiv, anorm, rcond, work, iwork, info )
           ! compute the solution vectors x.
           call stdlib_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_dsptrs( uplo, n, nrhs, afp, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_dsprfs( uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx, ferr,berr, work, &
                     iwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_dspsvx


     module subroutine stdlib_cspsvx( fact, uplo, n, nrhs, ap, afp, ipiv, b, ldb, x,ldx, rcond, ferr, &
     !! CSPSVX uses the diagonal pivoting factorization A = U*D*U**T or
     !! A = L*D*L**T to compute the solution to a complex system of linear
     !! equations A * X = B, where A is an N-by-N symmetric matrix stored
     !! in packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               berr, work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(inout) :: afp(*)
           complex(sp), intent(in) :: ap(*), b(ldb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact
           real(sp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSPSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the factorization a = u*d*u**t or a = l*d*l**t.
              call stdlib_ccopy( n*( n+1 ) / 2_ilp, ap, 1_ilp, afp, 1_ilp )
              call stdlib_csptrf( uplo, n, afp, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_clansp( 'I', uplo, n, ap, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_cspcon( uplo, n, afp, ipiv, anorm, rcond, work, info )
           ! compute the solution vectors x.
           call stdlib_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_csptrs( uplo, n, nrhs, afp, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_csprfs( uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx, ferr,berr, work, &
                     rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_cspsvx

     module subroutine stdlib_zspsvx( fact, uplo, n, nrhs, ap, afp, ipiv, b, ldb, x,ldx, rcond, ferr, &
     !! ZSPSVX uses the diagonal pivoting factorization A = U*D*U**T or
     !! A = L*D*L**T to compute the solution to a complex system of linear
     !! equations A * X = B, where A is an N-by-N symmetric matrix stored
     !! in packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               berr, work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(inout) :: afp(*)
           complex(dp), intent(in) :: ap(*), b(ldb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact
           real(dp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSPSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the factorization a = u*d*u**t or a = l*d*l**t.
              call stdlib_zcopy( n*( n+1 ) / 2_ilp, ap, 1_ilp, afp, 1_ilp )
              call stdlib_zsptrf( uplo, n, afp, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_zlansp( 'I', uplo, n, ap, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_zspcon( uplo, n, afp, ipiv, anorm, rcond, work, info )
           ! compute the solution vectors x.
           call stdlib_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_zsptrs( uplo, n, nrhs, afp, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_zsprfs( uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx, ferr,berr, work, &
                     rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_zspsvx




     pure module subroutine stdlib_chpsv( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! CHPSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian matrix stored in packed format and X
     !! and B are N-by-NRHS matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**H,  if UPLO = 'U', or
     !! A = L * D * L**H,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, D is Hermitian and block diagonal with 1-by-1
     !! and 2-by-2 diagonal blocks.  The factored form of A is then used to
     !! solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: ap(*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHPSV ', -info )
              return
           end if
           ! compute the factorization a = u*d*u**h or a = l*d*l**h.
           call stdlib_chptrf( uplo, n, ap, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_chptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
           end if
           return
     end subroutine stdlib_chpsv

     pure module subroutine stdlib_zhpsv( uplo, n, nrhs, ap, ipiv, b, ldb, info )
     !! ZHPSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian matrix stored in packed format and X
     !! and B are N-by-NRHS matrices.
     !! The diagonal pivoting method is used to factor A as
     !! A = U * D * U**H,  if UPLO = 'U', or
     !! A = L * D * L**H,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, D is Hermitian and block diagonal with 1-by-1
     !! and 2-by-2 diagonal blocks.  The factored form of A is then used to
     !! solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: ap(*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHPSV ', -info )
              return
           end if
           ! compute the factorization a = u*d*u**h or a = l*d*l**h.
           call stdlib_zhptrf( uplo, n, ap, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_zhptrs( uplo, n, nrhs, ap, ipiv, b, ldb, info )
           end if
           return
     end subroutine stdlib_zhpsv




     module subroutine stdlib_chpsvx( fact, uplo, n, nrhs, ap, afp, ipiv, b, ldb, x,ldx, rcond, ferr, &
     !! CHPSVX uses the diagonal pivoting factorization A = U*D*U**H or
     !! A = L*D*L**H to compute the solution to a complex system of linear
     !! equations A * X = B, where A is an N-by-N Hermitian matrix stored
     !! in packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               berr, work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(inout) :: afp(*)
           complex(sp), intent(in) :: ap(*), b(ldb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact
           real(sp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHPSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the factorization a = u*d*u**h or a = l*d*l**h.
              call stdlib_ccopy( n*( n+1 ) / 2_ilp, ap, 1_ilp, afp, 1_ilp )
              call stdlib_chptrf( uplo, n, afp, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_clanhp( 'I', uplo, n, ap, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_chpcon( uplo, n, afp, ipiv, anorm, rcond, work, info )
           ! compute the solution vectors x.
           call stdlib_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_chptrs( uplo, n, nrhs, afp, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_chprfs( uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx, ferr,berr, work, &
                     rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_chpsvx

     module subroutine stdlib_zhpsvx( fact, uplo, n, nrhs, ap, afp, ipiv, b, ldb, x,ldx, rcond, ferr, &
     !! ZHPSVX uses the diagonal pivoting factorization A = U*D*U**H or
     !! A = L*D*L**H to compute the solution to a complex system of linear
     !! equations A * X = B, where A is an N-by-N Hermitian matrix stored
     !! in packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               berr, work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(inout) :: afp(*)
           complex(dp), intent(in) :: ap(*), b(ldb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact
           real(dp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHPSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the factorization a = u*d*u**h or a = l*d*l**h.
              call stdlib_zcopy( n*( n+1 ) / 2_ilp, ap, 1_ilp, afp, 1_ilp )
              call stdlib_zhptrf( uplo, n, afp, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_zlanhp( 'I', uplo, n, ap, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_zhpcon( uplo, n, afp, ipiv, anorm, rcond, work, info )
           ! compute the solution vectors x.
           call stdlib_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_zhptrs( uplo, n, nrhs, afp, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_zhprfs( uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx, ferr,berr, work, &
                     rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_zhpsvx




     pure module subroutine stdlib_ssysv_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! SSYSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! Aasen's algorithm is used to factor A as
     !! A = U**T * T * U,  if UPLO = 'U', or
     !! A = L * T * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and T is symmetric tridiagonal. The factored
     !! form of A is then used to solve the system of equations A * X = B.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt, lwkopt_sytrf, lwkopt_sytrs
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max(2_ilp*n, 3_ilp*n-2) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              call stdlib_ssytrf_aa( uplo, n, a, lda, ipiv, work, -1_ilp, info )
              lwkopt_sytrf = int( work(1_ilp),KIND=ilp)
              call stdlib_ssytrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,-1_ilp, info )
              lwkopt_sytrs = int( work(1_ilp),KIND=ilp)
              lwkopt = max( lwkopt_sytrf, lwkopt_sytrs )
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYSV_AA', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u**t*t*u or a = l*t*l**t.
           call stdlib_ssytrf_aa( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_ssytrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
                        
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssysv_aa

     pure module subroutine stdlib_dsysv_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! DSYSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! Aasen's algorithm is used to factor A as
     !! A = U**T * T * U,  if UPLO = 'U', or
     !! A = L * T * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and T is symmetric tridiagonal. The factored
     !! form of A is then used to solve the system of equations A * X = B.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt, lwkopt_sytrf, lwkopt_sytrs
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max(2_ilp*n, 3_ilp*n-2) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              call stdlib_dsytrf_aa( uplo, n, a, lda, ipiv, work, -1_ilp, info )
              lwkopt_sytrf = int( work(1_ilp),KIND=ilp)
              call stdlib_dsytrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,-1_ilp, info )
              lwkopt_sytrs = int( work(1_ilp),KIND=ilp)
              lwkopt = max( lwkopt_sytrf, lwkopt_sytrs )
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYSV_AA ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u**t*t*u or a = l*t*l**t.
           call stdlib_dsytrf_aa( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_dsytrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
                        
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsysv_aa


     pure module subroutine stdlib_csysv_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! CSYSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! Aasen's algorithm is used to factor A as
     !! A = U**T * T * U,  if UPLO = 'U', or
     !! A = L * T * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and T is symmetric tridiagonal. The factored
     !! form of A is then used to solve the system of equations A * X = B.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt, lwkopt_sytrf, lwkopt_sytrs
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max(2_ilp*n, 3_ilp*n-2) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              call stdlib_csytrf_aa( uplo, n, a, lda, ipiv, work, -1_ilp, info )
              lwkopt_sytrf = int( work(1_ilp),KIND=ilp)
              call stdlib_csytrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,-1_ilp, info )
              lwkopt_sytrs = int( work(1_ilp),KIND=ilp)
              lwkopt = max( lwkopt_sytrf, lwkopt_sytrs )
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYSV_AA ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u**t*t*u or a = l*t*l**t.
           call stdlib_csytrf_aa( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_csytrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
                        
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_csysv_aa

     pure module subroutine stdlib_zsysv_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! ZSYSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
     !! matrices.
     !! Aasen's algorithm is used to factor A as
     !! A = U**T * T * U,  if UPLO = 'U', or
     !! A = L * T * L**T,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and T is symmetric tridiagonal. The factored
     !! form of A is then used to solve the system of equations A * X = B.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt, lwkopt_sytrf, lwkopt_sytrs
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max(2_ilp*n, 3_ilp*n-2) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              call stdlib_zsytrf_aa( uplo, n, a, lda, ipiv, work, -1_ilp, info )
              lwkopt_sytrf = int( work(1_ilp),KIND=ilp)
              call stdlib_zsytrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,-1_ilp, info )
              lwkopt_sytrs = int( work(1_ilp),KIND=ilp)
              lwkopt = max( lwkopt_sytrf, lwkopt_sytrs )
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYSV_AA ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u**t*t*u or a = l*t*l**t.
           call stdlib_zsytrf_aa( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_zsytrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
                        
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zsysv_aa




     pure module subroutine stdlib_chesv_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! CHESV_AA computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian matrix and X and B are N-by-NRHS
     !! matrices.
     !! Aasen's algorithm is used to factor A as
     !! A = U**H * T * U,  if UPLO = 'U', or
     !! A = L * T * L**H,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and T is Hermitian and tridiagonal. The factored form
     !! of A is then used to solve the system of equations A * X = B.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt, lwkopt_hetrf, lwkopt_hetrs
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max( 2_ilp*n, 3_ilp*n-2 ) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              call stdlib_chetrf_aa( uplo, n, a, lda, ipiv, work, -1_ilp, info )
              lwkopt_hetrf = int( work(1_ilp),KIND=ilp)
              call stdlib_chetrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,-1_ilp, info )
              lwkopt_hetrs = int( work(1_ilp),KIND=ilp)
              lwkopt = max( lwkopt_hetrf, lwkopt_hetrs )
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHESV_AA ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u**h*t*u or a = l*t*l**h.
           call stdlib_chetrf_aa( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_chetrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
                        
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_chesv_aa

     pure module subroutine stdlib_zhesv_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
     !! ZHESV_AA computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian matrix and X and B are N-by-NRHS
     !! matrices.
     !! Aasen's algorithm is used to factor A as
     !! A = U**H * T * U,  if UPLO = 'U', or
     !! A = L * T * L**H,  if UPLO = 'L',
     !! where U (or L) is a product of permutation and unit upper (lower)
     !! triangular matrices, and T is Hermitian and tridiagonal. The factored form
     !! of A is then used to solve the system of equations A * X = B.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lwkopt, lwkopt_hetrf, lwkopt_hetrs
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( lwork<max(2_ilp*n, 3_ilp*n-2) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info==0_ilp ) then
              call stdlib_zhetrf_aa( uplo, n, a, lda, ipiv, work, -1_ilp, info )
              lwkopt_hetrf = int( work(1_ilp),KIND=ilp)
              call stdlib_zhetrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,-1_ilp, info )
              lwkopt_hetrs = int( work(1_ilp),KIND=ilp)
              lwkopt = max( lwkopt_hetrf, lwkopt_hetrs )
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHESV_AA ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! compute the factorization a = u**h*t*u or a = l*t*l**h.
           call stdlib_zhetrf_aa( uplo, n, a, lda, ipiv, work, lwork, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_zhetrs_aa( uplo, n, nrhs, a, lda, ipiv, b, ldb, work,lwork, info )
                        
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zhesv_aa



end submodule stdlib_lapack_solve_ldl
