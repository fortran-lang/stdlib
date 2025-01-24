submodule(stdlib_lapack_solve) stdlib_lapack_solve_chol
  implicit none


  contains

     pure module subroutine stdlib_sposv( uplo, n, nrhs, a, lda, b, ldb, info )
     !! SPOSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**T* U,  if UPLO = 'U', or
     !! A = L * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
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
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPOSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**t*u or a = l*l**t.
           call stdlib_spotrf( uplo, n, a, lda, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_spotrs( uplo, n, nrhs, a, lda, b, ldb, info )
           end if
           return
     end subroutine stdlib_sposv

     pure module subroutine stdlib_dposv( uplo, n, nrhs, a, lda, b, ldb, info )
     !! DPOSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**T* U,  if UPLO = 'U', or
     !! A = L * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
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
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPOSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**t*u or a = l*l**t.
           call stdlib_dpotrf( uplo, n, a, lda, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_dpotrs( uplo, n, nrhs, a, lda, b, ldb, info )
           end if
           return
     end subroutine stdlib_dposv


     pure module subroutine stdlib_cposv( uplo, n, nrhs, a, lda, b, ldb, info )
     !! CPOSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**H* U,  if UPLO = 'U', or
     !! A = L * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and  L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
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
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPOSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**h*u or a = l*l**h.
           call stdlib_cpotrf( uplo, n, a, lda, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_cpotrs( uplo, n, nrhs, a, lda, b, ldb, info )
           end if
           return
     end subroutine stdlib_cposv

     pure module subroutine stdlib_zposv( uplo, n, nrhs, a, lda, b, ldb, info )
     !! ZPOSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**H* U,  if UPLO = 'U', or
     !! A = L * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and  L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
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
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPOSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**h *u or a = l*l**h.
           call stdlib_zpotrf( uplo, n, a, lda, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_zpotrs( uplo, n, nrhs, a, lda, b, ldb, info )
           end if
           return
     end subroutine stdlib_zposv




     module subroutine stdlib_sposvx( fact, uplo, n, nrhs, a, lda, af, ldaf, equed,s, b, ldb, x, ldx, &
     !! SPOSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
     !! compute the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               rcond, ferr, berr, work,iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*), s(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp) :: i, infequ, j
           real(sp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
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
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -9_ilp
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -10_ilp
                 else if( n>0_ilp ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -12_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -14_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPOSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_spoequ( n, a, lda, s, scond, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_slaqsy( uplo, n, a, lda, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**t *u or a = l*l**t.
              call stdlib_slacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_spotrf( uplo, n, af, ldaf, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_slansy( '1', uplo, n, a, lda, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_spocon( uplo, n, af, ldaf, anorm, rcond, work, iwork, info )
           ! compute the solution matrix x.
           call stdlib_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_spotrs( uplo, n, nrhs, af, ldaf, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_sporfs( uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x, ldx,ferr, berr, work, &
                     iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_sposvx

     module subroutine stdlib_dposvx( fact, uplo, n, nrhs, a, lda, af, ldaf, equed,s, b, ldb, x, ldx, &
     !! DPOSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
     !! compute the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               rcond, ferr, berr, work,iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*), s(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp) :: i, infequ, j
           real(dp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
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
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -9_ilp
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -10_ilp
                 else if( n>0_ilp ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -12_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -14_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPOSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_dpoequ( n, a, lda, s, scond, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_dlaqsy( uplo, n, a, lda, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**t *u or a = l*l**t.
              call stdlib_dlacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_dpotrf( uplo, n, af, ldaf, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_dlansy( '1', uplo, n, a, lda, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_dpocon( uplo, n, af, ldaf, anorm, rcond, work, iwork, info )
           ! compute the solution matrix x.
           call stdlib_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_dpotrs( uplo, n, nrhs, af, ldaf, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_dporfs( uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x, ldx,ferr, berr, work, &
                     iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_dposvx


     module subroutine stdlib_cposvx( fact, uplo, n, nrhs, a, lda, af, ldaf, equed,s, b, ldb, x, ldx, &
     !! CPOSVX uses the Cholesky factorization A = U**H*U or A = L*L**H to
     !! compute the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               rcond, ferr, berr, work,rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(sp), intent(inout) :: s(*)
           complex(sp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp) :: i, infequ, j
           real(sp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
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
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -9_ilp
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -10_ilp
                 else if( n>0_ilp ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -12_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -14_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPOSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_cpoequ( n, a, lda, s, scond, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_claqhe( uplo, n, a, lda, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**h *u or a = l*l**h.
              call stdlib_clacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_cpotrf( uplo, n, af, ldaf, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_clanhe( '1', uplo, n, a, lda, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_cpocon( uplo, n, af, ldaf, anorm, rcond, work, rwork, info )
           ! compute the solution matrix x.
           call stdlib_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_cpotrs( uplo, n, nrhs, af, ldaf, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_cporfs( uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x, ldx,ferr, berr, work, &
                     rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_cposvx

     module subroutine stdlib_zposvx( fact, uplo, n, nrhs, a, lda, af, ldaf, equed,s, b, ldb, x, ldx, &
     !! ZPOSVX uses the Cholesky factorization A = U**H*U or A = L*L**H to
     !! compute the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               rcond, ferr, berr, work,rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(dp), intent(inout) :: s(*)
           complex(dp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp) :: i, infequ, j
           real(dp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
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
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -9_ilp
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -10_ilp
                 else if( n>0_ilp ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -12_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -14_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPOSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_zpoequ( n, a, lda, s, scond, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_zlaqhe( uplo, n, a, lda, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**h *u or a = l*l**h.
              call stdlib_zlacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_zpotrf( uplo, n, af, ldaf, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_zlanhe( '1', uplo, n, a, lda, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_zpocon( uplo, n, af, ldaf, anorm, rcond, work, rwork, info )
           ! compute the solution matrix x.
           call stdlib_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_zpotrs( uplo, n, nrhs, af, ldaf, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_zporfs( uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x, ldx,ferr, berr, work, &
                     rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_zposvx




     pure module subroutine stdlib_sppsv( uplo, n, nrhs, ap, b, ldb, info )
     !! SPPSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**T* U,  if UPLO = 'U', or
     !! A = L * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
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
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPPSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**t*u or a = l*l**t.
           call stdlib_spptrf( uplo, n, ap, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_spptrs( uplo, n, nrhs, ap, b, ldb, info )
           end if
           return
     end subroutine stdlib_sppsv

     pure module subroutine stdlib_dppsv( uplo, n, nrhs, ap, b, ldb, info )
     !! DPPSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**T* U,  if UPLO = 'U', or
     !! A = L * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
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
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPPSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**t*u or a = l*l**t.
           call stdlib_dpptrf( uplo, n, ap, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_dpptrs( uplo, n, nrhs, ap, b, ldb, info )
           end if
           return
     end subroutine stdlib_dppsv


     pure module subroutine stdlib_cppsv( uplo, n, nrhs, ap, b, ldb, info )
     !! CPPSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
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
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPPSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**h *u or a = l*l**h.
           call stdlib_cpptrf( uplo, n, ap, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_cpptrs( uplo, n, nrhs, ap, b, ldb, info )
           end if
           return
     end subroutine stdlib_cppsv

     pure module subroutine stdlib_zppsv( uplo, n, nrhs, ap, b, ldb, info )
     !! ZPPSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
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
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPPSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**h *u or a = l*l**h.
           call stdlib_zpptrf( uplo, n, ap, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_zpptrs( uplo, n, nrhs, ap, b, ldb, info )
           end if
           return
     end subroutine stdlib_zppsv




     module subroutine stdlib_sppsvx( fact, uplo, n, nrhs, ap, afp, equed, s, b, ldb,x, ldx, rcond, ferr,&
     !! SPPSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
     !! compute the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                berr, work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: afp(*), ap(*), b(ldb,*), s(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp) :: i, infequ, j
           real(sp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -7_ilp
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -8_ilp
                 else if( n>0_ilp ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -10_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -12_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPPSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_sppequ( uplo, n, ap, s, scond, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_slaqsp( uplo, n, ap, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**t * u or a = l * l**t.
              call stdlib_scopy( n*( n+1 ) / 2_ilp, ap, 1_ilp, afp, 1_ilp )
              call stdlib_spptrf( uplo, n, afp, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_slansp( 'I', uplo, n, ap, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_sppcon( uplo, n, afp, anorm, rcond, work, iwork, info )
           ! compute the solution matrix x.
           call stdlib_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_spptrs( uplo, n, nrhs, afp, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_spprfs( uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr, berr,work, iwork, &
                     info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_sppsvx

     module subroutine stdlib_dppsvx( fact, uplo, n, nrhs, ap, afp, equed, s, b, ldb,x, ldx, rcond, ferr,&
     !! DPPSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
     !! compute the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                berr, work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: afp(*), ap(*), b(ldb,*), s(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp) :: i, infequ, j
           real(dp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -7_ilp
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -8_ilp
                 else if( n>0_ilp ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -10_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -12_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPPSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_dppequ( uplo, n, ap, s, scond, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_dlaqsp( uplo, n, ap, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**t * u or a = l * l**t.
              call stdlib_dcopy( n*( n+1 ) / 2_ilp, ap, 1_ilp, afp, 1_ilp )
              call stdlib_dpptrf( uplo, n, afp, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_dlansp( 'I', uplo, n, ap, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_dppcon( uplo, n, afp, anorm, rcond, work, iwork, info )
           ! compute the solution matrix x.
           call stdlib_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_dpptrs( uplo, n, nrhs, afp, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_dpprfs( uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr, berr,work, iwork, &
                     info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_dppsvx


     module subroutine stdlib_cppsvx( fact, uplo, n, nrhs, ap, afp, equed, s, b, ldb,x, ldx, rcond, ferr,&
     !! CPPSVX uses the Cholesky factorization A = U**H*U or A = L*L**H to
     !! compute the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                berr, work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(sp), intent(inout) :: s(*)
           complex(sp), intent(inout) :: afp(*), ap(*), b(ldb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp) :: i, infequ, j
           real(sp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -7_ilp
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -8_ilp
                 else if( n>0_ilp ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -10_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -12_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPPSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_cppequ( uplo, n, ap, s, scond, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_claqhp( uplo, n, ap, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**h * u or a = l * l**h.
              call stdlib_ccopy( n*( n+1 ) / 2_ilp, ap, 1_ilp, afp, 1_ilp )
              call stdlib_cpptrf( uplo, n, afp, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_clanhp( 'I', uplo, n, ap, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_cppcon( uplo, n, afp, anorm, rcond, work, rwork, info )
           ! compute the solution matrix x.
           call stdlib_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_cpptrs( uplo, n, nrhs, afp, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_cpprfs( uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr, berr,work, rwork, &
                     info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_cppsvx

     module subroutine stdlib_zppsvx( fact, uplo, n, nrhs, ap, afp, equed, s, b, ldb,x, ldx, rcond, ferr,&
     !! ZPPSVX uses the Cholesky factorization A = U**H * U or A = L * L**H to
     !! compute the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                berr, work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(dp), intent(inout) :: s(*)
           complex(dp), intent(inout) :: afp(*), ap(*), b(ldb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp) :: i, infequ, j
           real(dp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -7_ilp
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -8_ilp
                 else if( n>0_ilp ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -10_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -12_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPPSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_zppequ( uplo, n, ap, s, scond, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_zlaqhp( uplo, n, ap, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**h * u or a = l * l**h.
              call stdlib_zcopy( n*( n+1 ) / 2_ilp, ap, 1_ilp, afp, 1_ilp )
              call stdlib_zpptrf( uplo, n, afp, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_zlanhp( 'I', uplo, n, ap, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_zppcon( uplo, n, afp, anorm, rcond, work, rwork, info )
           ! compute the solution matrix x.
           call stdlib_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_zpptrs( uplo, n, nrhs, afp, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_zpprfs( uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr, berr,work, rwork, &
                     info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_zppsvx




     pure module subroutine stdlib_spbsv( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
     !! SPBSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L * L**T,  if UPLO = 'L',
     !! where U is an upper triangular band matrix, and L is a lower
     !! triangular band matrix, with the same number of superdiagonals or
     !! subdiagonals as A.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPBSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**t*u or a = l*l**t.
           call stdlib_spbtrf( uplo, n, kd, ab, ldab, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_spbtrs( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
           end if
           return
     end subroutine stdlib_spbsv

     pure module subroutine stdlib_dpbsv( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
     !! DPBSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L * L**T,  if UPLO = 'L',
     !! where U is an upper triangular band matrix, and L is a lower
     !! triangular band matrix, with the same number of superdiagonals or
     !! subdiagonals as A.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPBSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**t*u or a = l*l**t.
           call stdlib_dpbtrf( uplo, n, kd, ab, ldab, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_dpbtrs( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
           end if
           return
     end subroutine stdlib_dpbsv


     pure module subroutine stdlib_cpbsv( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
     !! CPBSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L * L**H,  if UPLO = 'L',
     !! where U is an upper triangular band matrix, and L is a lower
     !! triangular band matrix, with the same number of superdiagonals or
     !! subdiagonals as A.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPBSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**h*u or a = l*l**h.
           call stdlib_cpbtrf( uplo, n, kd, ab, ldab, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_cpbtrs( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
           end if
           return
     end subroutine stdlib_cpbsv

     pure module subroutine stdlib_zpbsv( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
     !! ZPBSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L * L**H,  if UPLO = 'L',
     !! where U is an upper triangular band matrix, and L is a lower
     !! triangular band matrix, with the same number of superdiagonals or
     !! subdiagonals as A.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd+1 ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPBSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**h *u or a = l*l**h.
           call stdlib_zpbtrf( uplo, n, kd, ab, ldab, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_zpbtrs( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
           end if
           return
     end subroutine stdlib_zpbsv




     module subroutine stdlib_spbsvx( fact, uplo, n, kd, nrhs, ab, ldab, afb, ldafb,equed, s, b, ldb, x, &
     !! SPBSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
     !! compute the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldx, rcond, ferr, berr,work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*), s(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ, upper
           integer(ilp) :: i, infequ, j, j1, j2
           real(sp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           upper = stdlib_lsame( uplo, 'U' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldab<kd+1 ) then
              info = -7_ilp
           else if( ldafb<kd+1 ) then
              info = -9_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -10_ilp
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -11_ilp
                 else if( n>0_ilp ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -13_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -15_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_spbequ( uplo, n, kd, ab, ldab, s, scond, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_slaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**t *u or a = l*l**t.
              if( upper ) then
                 do j = 1, n
                    j1 = max( j-kd, 1_ilp )
                    call stdlib_scopy( j-j1+1, ab( kd+1-j+j1, j ), 1_ilp,afb( kd+1-j+j1, j ), 1_ilp )
                              
                 end do
              else
                 do j = 1, n
                    j2 = min( j+kd, n )
                    call stdlib_scopy( j2-j+1, ab( 1_ilp, j ), 1_ilp, afb( 1_ilp, j ), 1_ilp )
                 end do
              end if
              call stdlib_spbtrf( uplo, n, kd, afb, ldafb, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_slansb( '1', uplo, n, kd, ab, ldab, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_spbcon( uplo, n, kd, afb, ldafb, anorm, rcond, work, iwork,info )
           ! compute the solution matrix x.
           call stdlib_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_spbtrs( uplo, n, kd, nrhs, afb, ldafb, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_spbrfs( uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b, ldb, x,ldx, ferr, berr,&
                      work, iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_spbsvx

     module subroutine stdlib_dpbsvx( fact, uplo, n, kd, nrhs, ab, ldab, afb, ldafb,equed, s, b, ldb, x, &
     !! DPBSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
     !! compute the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldx, rcond, ferr, berr,work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*), s(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ, upper
           integer(ilp) :: i, infequ, j, j1, j2
           real(dp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           upper = stdlib_lsame( uplo, 'U' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldab<kd+1 ) then
              info = -7_ilp
           else if( ldafb<kd+1 ) then
              info = -9_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -10_ilp
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -11_ilp
                 else if( n>0_ilp ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -13_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -15_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_dpbequ( uplo, n, kd, ab, ldab, s, scond, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_dlaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**t *u or a = l*l**t.
              if( upper ) then
                 do j = 1, n
                    j1 = max( j-kd, 1_ilp )
                    call stdlib_dcopy( j-j1+1, ab( kd+1-j+j1, j ), 1_ilp,afb( kd+1-j+j1, j ), 1_ilp )
                              
                 end do
              else
                 do j = 1, n
                    j2 = min( j+kd, n )
                    call stdlib_dcopy( j2-j+1, ab( 1_ilp, j ), 1_ilp, afb( 1_ilp, j ), 1_ilp )
                 end do
              end if
              call stdlib_dpbtrf( uplo, n, kd, afb, ldafb, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_dlansb( '1', uplo, n, kd, ab, ldab, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_dpbcon( uplo, n, kd, afb, ldafb, anorm, rcond, work, iwork,info )
           ! compute the solution matrix x.
           call stdlib_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_dpbtrs( uplo, n, kd, nrhs, afb, ldafb, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_dpbrfs( uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b, ldb, x,ldx, ferr, berr,&
                      work, iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_dpbsvx


     module subroutine stdlib_cpbsvx( fact, uplo, n, kd, nrhs, ab, ldab, afb, ldafb,equed, s, b, ldb, x, &
     !! CPBSVX uses the Cholesky factorization A = U**H*U or A = L*L**H to
     !! compute the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldx, rcond, ferr, berr,work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(sp), intent(inout) :: s(*)
           complex(sp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ, upper
           integer(ilp) :: i, infequ, j, j1, j2
           real(sp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           upper = stdlib_lsame( uplo, 'U' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldab<kd+1 ) then
              info = -7_ilp
           else if( ldafb<kd+1 ) then
              info = -9_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -10_ilp
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -11_ilp
                 else if( n>0_ilp ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -13_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -15_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_cpbequ( uplo, n, kd, ab, ldab, s, scond, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_claqhb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**h *u or a = l*l**h.
              if( upper ) then
                 do j = 1, n
                    j1 = max( j-kd, 1_ilp )
                    call stdlib_ccopy( j-j1+1, ab( kd+1-j+j1, j ), 1_ilp,afb( kd+1-j+j1, j ), 1_ilp )
                              
                 end do
              else
                 do j = 1, n
                    j2 = min( j+kd, n )
                    call stdlib_ccopy( j2-j+1, ab( 1_ilp, j ), 1_ilp, afb( 1_ilp, j ), 1_ilp )
                 end do
              end if
              call stdlib_cpbtrf( uplo, n, kd, afb, ldafb, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_clanhb( '1', uplo, n, kd, ab, ldab, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_cpbcon( uplo, n, kd, afb, ldafb, anorm, rcond, work, rwork,info )
           ! compute the solution matrix x.
           call stdlib_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_cpbtrs( uplo, n, kd, nrhs, afb, ldafb, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_cpbrfs( uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b, ldb, x,ldx, ferr, berr,&
                      work, rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_cpbsvx

     module subroutine stdlib_zpbsvx( fact, uplo, n, kd, nrhs, ab, ldab, afb, ldafb,equed, s, b, ldb, x, &
     !! ZPBSVX uses the Cholesky factorization A = U**H*U or A = L*L**H to
     !! compute the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldx, rcond, ferr, berr,work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(dp), intent(inout) :: s(*)
           complex(dp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ, upper
           integer(ilp) :: i, infequ, j, j1, j2
           real(dp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           upper = stdlib_lsame( uplo, 'U' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldab<kd+1 ) then
              info = -7_ilp
           else if( ldafb<kd+1 ) then
              info = -9_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -10_ilp
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -11_ilp
                 else if( n>0_ilp ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -13_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -15_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_zpbequ( uplo, n, kd, ab, ldab, s, scond, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_zlaqhb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**h *u or a = l*l**h.
              if( upper ) then
                 do j = 1, n
                    j1 = max( j-kd, 1_ilp )
                    call stdlib_zcopy( j-j1+1, ab( kd+1-j+j1, j ), 1_ilp,afb( kd+1-j+j1, j ), 1_ilp )
                              
                 end do
              else
                 do j = 1, n
                    j2 = min( j+kd, n )
                    call stdlib_zcopy( j2-j+1, ab( 1_ilp, j ), 1_ilp, afb( 1_ilp, j ), 1_ilp )
                 end do
              end if
              call stdlib_zpbtrf( uplo, n, kd, afb, ldafb, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_zlanhb( '1', uplo, n, kd, ab, ldab, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_zpbcon( uplo, n, kd, afb, ldafb, anorm, rcond, work, rwork,info )
           ! compute the solution matrix x.
           call stdlib_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_zpbtrs( uplo, n, kd, nrhs, afb, ldafb, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_zpbrfs( uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b, ldb, x,ldx, ferr, berr,&
                      work, rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_zpbsvx




     pure module subroutine stdlib_sptsv( n, nrhs, d, e, b, ldb, info )
     !! SPTSV computes the solution to a real system of linear equations
     !! A*X = B, where A is an N-by-N symmetric positive definite tridiagonal
     !! matrix, and X and B are N-by-NRHS matrices.
     !! A is factored as A = L*D*L**T, and the factored form of A is then
     !! used to solve the system of equations.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: b(ldb,*), d(*), e(*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPTSV ', -info )
              return
           end if
           ! compute the l*d*l**t (or u**t*d*u) factorization of a.
           call stdlib_spttrf( n, d, e, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_spttrs( n, nrhs, d, e, b, ldb, info )
           end if
           return
     end subroutine stdlib_sptsv

     pure module subroutine stdlib_dptsv( n, nrhs, d, e, b, ldb, info )
     !! DPTSV computes the solution to a real system of linear equations
     !! A*X = B, where A is an N-by-N symmetric positive definite tridiagonal
     !! matrix, and X and B are N-by-NRHS matrices.
     !! A is factored as A = L*D*L**T, and the factored form of A is then
     !! used to solve the system of equations.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: b(ldb,*), d(*), e(*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPTSV ', -info )
              return
           end if
           ! compute the l*d*l**t (or u**t*d*u) factorization of a.
           call stdlib_dpttrf( n, d, e, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_dpttrs( n, nrhs, d, e, b, ldb, info )
           end if
           return
     end subroutine stdlib_dptsv


     pure module subroutine stdlib_cptsv( n, nrhs, d, e, b, ldb, info )
     !! CPTSV computes the solution to a complex system of linear equations
     !! A*X = B, where A is an N-by-N Hermitian positive definite tridiagonal
     !! matrix, and X and B are N-by-NRHS matrices.
     !! A is factored as A = L*D*L**H, and the factored form of A is then
     !! used to solve the system of equations.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: d(*)
           complex(sp), intent(inout) :: b(ldb,*), e(*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPTSV ', -info )
              return
           end if
           ! compute the l*d*l**h (or u**h*d*u) factorization of a.
           call stdlib_cpttrf( n, d, e, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_cpttrs( 'LOWER', n, nrhs, d, e, b, ldb, info )
           end if
           return
     end subroutine stdlib_cptsv

     pure module subroutine stdlib_zptsv( n, nrhs, d, e, b, ldb, info )
     !! ZPTSV computes the solution to a complex system of linear equations
     !! A*X = B, where A is an N-by-N Hermitian positive definite tridiagonal
     !! matrix, and X and B are N-by-NRHS matrices.
     !! A is factored as A = L*D*L**H, and the factored form of A is then
     !! used to solve the system of equations.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: d(*)
           complex(dp), intent(inout) :: b(ldb,*), e(*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPTSV ', -info )
              return
           end if
           ! compute the l*d*l**h (or u**h*d*u) factorization of a.
           call stdlib_zpttrf( n, d, e, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_zpttrs( 'LOWER', n, nrhs, d, e, b, ldb, info )
           end if
           return
     end subroutine stdlib_zptsv




     pure module subroutine stdlib_sptsvx( fact, n, nrhs, d, e, df, ef, b, ldb, x, ldx,rcond, ferr, berr,&
     !! SPTSVX uses the factorization A = L*D*L**T to compute the solution
     !! to a real system of linear equations A*X = B, where A is an N-by-N
     !! symmetric positive definite tridiagonal matrix and X and B are
     !! N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                work, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(in) :: b(ldb,*), d(*), e(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
           real(sp), intent(inout) :: df(*), ef(*)
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
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the l*d*l**t (or u**t*d*u) factorization of a.
              call stdlib_scopy( n, d, 1_ilp, df, 1_ilp )
              if( n>1_ilp )call stdlib_scopy( n-1, e, 1_ilp, ef, 1_ilp )
              call stdlib_spttrf( n, df, ef, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_slanst( '1', n, d, e )
           ! compute the reciprocal of the condition number of a.
           call stdlib_sptcon( n, df, ef, anorm, rcond, work, info )
           ! compute the solution vectors x.
           call stdlib_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_spttrs( n, nrhs, df, ef, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_sptrfs( n, nrhs, d, e, df, ef, b, ldb, x, ldx, ferr, berr,work, info )
                     
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_sptsvx

     pure module subroutine stdlib_dptsvx( fact, n, nrhs, d, e, df, ef, b, ldb, x, ldx,rcond, ferr, berr,&
     !! DPTSVX uses the factorization A = L*D*L**T to compute the solution
     !! to a real system of linear equations A*X = B, where A is an N-by-N
     !! symmetric positive definite tridiagonal matrix and X and B are
     !! N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                work, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(in) :: b(ldb,*), d(*), e(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
           real(dp), intent(inout) :: df(*), ef(*)
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
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the l*d*l**t (or u**t*d*u) factorization of a.
              call stdlib_dcopy( n, d, 1_ilp, df, 1_ilp )
              if( n>1_ilp )call stdlib_dcopy( n-1, e, 1_ilp, ef, 1_ilp )
              call stdlib_dpttrf( n, df, ef, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_dlanst( '1', n, d, e )
           ! compute the reciprocal of the condition number of a.
           call stdlib_dptcon( n, df, ef, anorm, rcond, work, info )
           ! compute the solution vectors x.
           call stdlib_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_dpttrs( n, nrhs, df, ef, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_dptrfs( n, nrhs, d, e, df, ef, b, ldb, x, ldx, ferr, berr,work, info )
                     
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_dptsvx


     pure module subroutine stdlib_cptsvx( fact, n, nrhs, d, e, df, ef, b, ldb, x, ldx,rcond, ferr, berr,&
     !! CPTSVX uses the factorization A = L*D*L**H to compute the solution
     !! to a complex system of linear equations A*X = B, where A is an
     !! N-by-N Hermitian positive definite tridiagonal matrix and X and B
     !! are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(sp), intent(in) :: d(*)
           real(sp), intent(inout) :: df(*)
           complex(sp), intent(in) :: b(ldb,*), e(*)
           complex(sp), intent(inout) :: ef(*)
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
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the l*d*l**h (or u**h*d*u) factorization of a.
              call stdlib_scopy( n, d, 1_ilp, df, 1_ilp )
              if( n>1_ilp )call stdlib_ccopy( n-1, e, 1_ilp, ef, 1_ilp )
              call stdlib_cpttrf( n, df, ef, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_clanht( '1', n, d, e )
           ! compute the reciprocal of the condition number of a.
           call stdlib_cptcon( n, df, ef, anorm, rcond, rwork, info )
           ! compute the solution vectors x.
           call stdlib_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_cpttrs( 'LOWER', n, nrhs, df, ef, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_cptrfs( 'LOWER', n, nrhs, d, e, df, ef, b, ldb, x, ldx, ferr,berr, work, &
                     rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_cptsvx

     pure module subroutine stdlib_zptsvx( fact, n, nrhs, d, e, df, ef, b, ldb, x, ldx,rcond, ferr, berr,&
     !! ZPTSVX uses the factorization A = L*D*L**H to compute the solution
     !! to a complex system of linear equations A*X = B, where A is an
     !! N-by-N Hermitian positive definite tridiagonal matrix and X and B
     !! are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(dp), intent(in) :: d(*)
           real(dp), intent(inout) :: df(*)
           complex(dp), intent(in) :: b(ldb,*), e(*)
           complex(dp), intent(inout) :: ef(*)
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
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( nrhs<0_ilp ) then
              info = -3_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the l*d*l**h (or u**h*d*u) factorization of a.
              call stdlib_dcopy( n, d, 1_ilp, df, 1_ilp )
              if( n>1_ilp )call stdlib_zcopy( n-1, e, 1_ilp, ef, 1_ilp )
              call stdlib_zpttrf( n, df, ef, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_zlanht( '1', n, d, e )
           ! compute the reciprocal of the condition number of a.
           call stdlib_zptcon( n, df, ef, anorm, rcond, rwork, info )
           ! compute the solution vectors x.
           call stdlib_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_zpttrs( 'LOWER', n, nrhs, df, ef, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_zptrfs( 'LOWER', n, nrhs, d, e, df, ef, b, ldb, x, ldx, ferr,berr, work, &
                     rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_zptsvx




     pure module subroutine stdlib_I64_sposv( uplo, n, nrhs, a, lda, b, ldb, info )
     !! SPOSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**T* U,  if UPLO = 'U', or
     !! A = L * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SPOSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**t*u or a = l*l**t.
           call stdlib_I64_spotrf( uplo, n, a, lda, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_spotrs( uplo, n, nrhs, a, lda, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_sposv

     pure module subroutine stdlib_I64_dposv( uplo, n, nrhs, a, lda, b, ldb, info )
     !! DPOSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**T* U,  if UPLO = 'U', or
     !! A = L * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DPOSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**t*u or a = l*l**t.
           call stdlib_I64_dpotrf( uplo, n, a, lda, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_dpotrs( uplo, n, nrhs, a, lda, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_dposv


     pure module subroutine stdlib_I64_cposv( uplo, n, nrhs, a, lda, b, ldb, info )
     !! CPOSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**H* U,  if UPLO = 'U', or
     !! A = L * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and  L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CPOSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**h*u or a = l*l**h.
           call stdlib_I64_cpotrf( uplo, n, a, lda, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_cpotrs( uplo, n, nrhs, a, lda, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_cposv

     pure module subroutine stdlib_I64_zposv( uplo, n, nrhs, a, lda, b, ldb, info )
     !! ZPOSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**H* U,  if UPLO = 'U', or
     !! A = L * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and  L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZPOSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**h *u or a = l*l**h.
           call stdlib_I64_zpotrf( uplo, n, a, lda, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_zpotrs( uplo, n, nrhs, a, lda, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_zposv




     module subroutine stdlib_I64_sposvx( fact, uplo, n, nrhs, a, lda, af, ldaf, equed,s, b, ldb, x, ldx, &
     !! SPOSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
     !! compute the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               rcond, ferr, berr, work,iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*), s(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp64) :: i, infequ, j
           real(sp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_I64_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -9_ilp64
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -10_ilp64
                 else if( n>0_ilp64 ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -12_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -14_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SPOSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_spoequ( n, a, lda, s, scond, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_slaqsy( uplo, n, a, lda, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**t *u or a = l*l**t.
              call stdlib_I64_slacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_I64_spotrf( uplo, n, af, ldaf, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_slansy( '1', uplo, n, a, lda, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_spocon( uplo, n, af, ldaf, anorm, rcond, work, iwork, info )
           ! compute the solution matrix x.
           call stdlib_I64_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_spotrs( uplo, n, nrhs, af, ldaf, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_sporfs( uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x, ldx,ferr, berr, work, &
                     iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_sposvx

     module subroutine stdlib_I64_dposvx( fact, uplo, n, nrhs, a, lda, af, ldaf, equed,s, b, ldb, x, ldx, &
     !! DPOSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
     !! compute the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               rcond, ferr, berr, work,iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*), s(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp64) :: i, infequ, j
           real(dp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_I64_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -9_ilp64
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -10_ilp64
                 else if( n>0_ilp64 ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -12_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -14_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DPOSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_dpoequ( n, a, lda, s, scond, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_dlaqsy( uplo, n, a, lda, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**t *u or a = l*l**t.
              call stdlib_I64_dlacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_I64_dpotrf( uplo, n, af, ldaf, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_dlansy( '1', uplo, n, a, lda, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_dpocon( uplo, n, af, ldaf, anorm, rcond, work, iwork, info )
           ! compute the solution matrix x.
           call stdlib_I64_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_dpotrs( uplo, n, nrhs, af, ldaf, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_dporfs( uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x, ldx,ferr, berr, work, &
                     iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_dposvx


     module subroutine stdlib_I64_cposvx( fact, uplo, n, nrhs, a, lda, af, ldaf, equed,s, b, ldb, x, ldx, &
     !! CPOSVX uses the Cholesky factorization A = U**H*U or A = L*L**H to
     !! compute the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               rcond, ferr, berr, work,rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(sp), intent(inout) :: s(*)
           complex(sp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp64) :: i, infequ, j
           real(sp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_I64_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -9_ilp64
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -10_ilp64
                 else if( n>0_ilp64 ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -12_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -14_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CPOSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_cpoequ( n, a, lda, s, scond, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_claqhe( uplo, n, a, lda, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**h *u or a = l*l**h.
              call stdlib_I64_clacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_I64_cpotrf( uplo, n, af, ldaf, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_clanhe( '1', uplo, n, a, lda, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_cpocon( uplo, n, af, ldaf, anorm, rcond, work, rwork, info )
           ! compute the solution matrix x.
           call stdlib_I64_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_cpotrs( uplo, n, nrhs, af, ldaf, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_cporfs( uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x, ldx,ferr, berr, work, &
                     rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_cposvx

     module subroutine stdlib_I64_zposvx( fact, uplo, n, nrhs, a, lda, af, ldaf, equed,s, b, ldb, x, ldx, &
     !! ZPOSVX uses the Cholesky factorization A = U**H*U or A = L*L**H to
     !! compute the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix and X and B
     !! are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               rcond, ferr, berr, work,rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(dp), intent(inout) :: s(*)
           complex(dp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp64) :: i, infequ, j
           real(dp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_I64_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -9_ilp64
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -10_ilp64
                 else if( n>0_ilp64 ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -12_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -14_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZPOSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_zpoequ( n, a, lda, s, scond, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_zlaqhe( uplo, n, a, lda, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**h *u or a = l*l**h.
              call stdlib_I64_zlacpy( uplo, n, n, a, lda, af, ldaf )
              call stdlib_I64_zpotrf( uplo, n, af, ldaf, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_zlanhe( '1', uplo, n, a, lda, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_zpocon( uplo, n, af, ldaf, anorm, rcond, work, rwork, info )
           ! compute the solution matrix x.
           call stdlib_I64_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_zpotrs( uplo, n, nrhs, af, ldaf, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_zporfs( uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x, ldx,ferr, berr, work, &
                     rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_zposvx




     pure module subroutine stdlib_I64_sppsv( uplo, n, nrhs, ap, b, ldb, info )
     !! SPPSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**T* U,  if UPLO = 'U', or
     !! A = L * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SPPSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**t*u or a = l*l**t.
           call stdlib_I64_spptrf( uplo, n, ap, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_spptrs( uplo, n, nrhs, ap, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_sppsv

     pure module subroutine stdlib_I64_dppsv( uplo, n, nrhs, ap, b, ldb, info )
     !! DPPSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**T* U,  if UPLO = 'U', or
     !! A = L * L**T,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DPPSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**t*u or a = l*l**t.
           call stdlib_I64_dpptrf( uplo, n, ap, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_dpptrs( uplo, n, nrhs, ap, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_dppsv


     pure module subroutine stdlib_I64_cppsv( uplo, n, nrhs, ap, b, ldb, info )
     !! CPPSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(inout) :: ap(*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CPPSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**h *u or a = l*l**h.
           call stdlib_I64_cpptrf( uplo, n, ap, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_cpptrs( uplo, n, nrhs, ap, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_cppsv

     pure module subroutine stdlib_I64_zppsv( uplo, n, nrhs, ap, b, ldb, info )
     !! ZPPSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L * L**H,  if UPLO = 'L',
     !! where U is an upper triangular matrix and L is a lower triangular
     !! matrix.  The factored form of A is then used to solve the system of
     !! equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(inout) :: ap(*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZPPSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**h *u or a = l*l**h.
           call stdlib_I64_zpptrf( uplo, n, ap, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_zpptrs( uplo, n, nrhs, ap, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_zppsv




     module subroutine stdlib_I64_sppsvx( fact, uplo, n, nrhs, ap, afp, equed, s, b, ldb,x, ldx, rcond, ferr,&
     !! SPPSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
     !! compute the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                berr, work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(inout) :: afp(*), ap(*), b(ldb,*), s(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp64) :: i, infequ, j
           real(sp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_I64_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -7_ilp64
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -8_ilp64
                 else if( n>0_ilp64 ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -10_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -12_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SPPSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_sppequ( uplo, n, ap, s, scond, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_slaqsp( uplo, n, ap, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**t * u or a = l * l**t.
              call stdlib_I64_scopy( n*( n+1 ) / 2_ilp64, ap, 1_ilp64, afp, 1_ilp64 )
              call stdlib_I64_spptrf( uplo, n, afp, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_slansp( 'I', uplo, n, ap, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_sppcon( uplo, n, afp, anorm, rcond, work, iwork, info )
           ! compute the solution matrix x.
           call stdlib_I64_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_spptrs( uplo, n, nrhs, afp, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_spprfs( uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr, berr,work, iwork, &
                     info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_sppsvx

     module subroutine stdlib_I64_dppsvx( fact, uplo, n, nrhs, ap, afp, equed, s, b, ldb,x, ldx, rcond, ferr,&
     !! DPPSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
     !! compute the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                berr, work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(inout) :: afp(*), ap(*), b(ldb,*), s(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp64) :: i, infequ, j
           real(dp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_I64_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -7_ilp64
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -8_ilp64
                 else if( n>0_ilp64 ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -10_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -12_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DPPSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_dppequ( uplo, n, ap, s, scond, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_dlaqsp( uplo, n, ap, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**t * u or a = l * l**t.
              call stdlib_I64_dcopy( n*( n+1 ) / 2_ilp64, ap, 1_ilp64, afp, 1_ilp64 )
              call stdlib_I64_dpptrf( uplo, n, afp, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_dlansp( 'I', uplo, n, ap, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_dppcon( uplo, n, afp, anorm, rcond, work, iwork, info )
           ! compute the solution matrix x.
           call stdlib_I64_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_dpptrs( uplo, n, nrhs, afp, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_dpprfs( uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr, berr,work, iwork, &
                     info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_dppsvx


     module subroutine stdlib_I64_cppsvx( fact, uplo, n, nrhs, ap, afp, equed, s, b, ldb,x, ldx, rcond, ferr,&
     !! CPPSVX uses the Cholesky factorization A = U**H*U or A = L*L**H to
     !! compute the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                berr, work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(sp), intent(inout) :: s(*)
           complex(sp), intent(inout) :: afp(*), ap(*), b(ldb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp64) :: i, infequ, j
           real(sp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_I64_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -7_ilp64
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -8_ilp64
                 else if( n>0_ilp64 ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -10_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -12_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CPPSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_cppequ( uplo, n, ap, s, scond, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_claqhp( uplo, n, ap, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**h * u or a = l * l**h.
              call stdlib_I64_ccopy( n*( n+1 ) / 2_ilp64, ap, 1_ilp64, afp, 1_ilp64 )
              call stdlib_I64_cpptrf( uplo, n, afp, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_clanhp( 'I', uplo, n, ap, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_cppcon( uplo, n, afp, anorm, rcond, work, rwork, info )
           ! compute the solution matrix x.
           call stdlib_I64_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_cpptrs( uplo, n, nrhs, afp, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_cpprfs( uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr, berr,work, rwork, &
                     info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_cppsvx

     module subroutine stdlib_I64_zppsvx( fact, uplo, n, nrhs, ap, afp, equed, s, b, ldb,x, ldx, rcond, ferr,&
     !! ZPPSVX uses the Cholesky factorization A = U**H * U or A = L * L**H to
     !! compute the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite matrix stored in
     !! packed format and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                berr, work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(dp), intent(inout) :: s(*)
           complex(dp), intent(inout) :: afp(*), ap(*), b(ldb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ
           integer(ilp64) :: i, infequ, j
           real(dp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_I64_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) )&
                     then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -7_ilp64
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -8_ilp64
                 else if( n>0_ilp64 ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -10_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -12_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZPPSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_zppequ( uplo, n, ap, s, scond, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_zlaqhp( uplo, n, ap, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**h * u or a = l * l**h.
              call stdlib_I64_zcopy( n*( n+1 ) / 2_ilp64, ap, 1_ilp64, afp, 1_ilp64 )
              call stdlib_I64_zpptrf( uplo, n, afp, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_zlanhp( 'I', uplo, n, ap, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_zppcon( uplo, n, afp, anorm, rcond, work, rwork, info )
           ! compute the solution matrix x.
           call stdlib_I64_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_zpptrs( uplo, n, nrhs, afp, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_zpprfs( uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr, berr,work, rwork, &
                     info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_zppsvx




     pure module subroutine stdlib_I64_spbsv( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
     !! SPBSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L * L**T,  if UPLO = 'L',
     !! where U is an upper triangular band matrix, and L is a lower
     !! triangular band matrix, with the same number of superdiagonals or
     !! subdiagonals as A.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( kd<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( ldab<kd+1 ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SPBSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**t*u or a = l*l**t.
           call stdlib_I64_spbtrf( uplo, n, kd, ab, ldab, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_spbtrs( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_spbsv

     pure module subroutine stdlib_I64_dpbsv( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
     !! DPBSV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**T * U,  if UPLO = 'U', or
     !! A = L * L**T,  if UPLO = 'L',
     !! where U is an upper triangular band matrix, and L is a lower
     !! triangular band matrix, with the same number of superdiagonals or
     !! subdiagonals as A.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( kd<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( ldab<kd+1 ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DPBSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**t*u or a = l*l**t.
           call stdlib_I64_dpbtrf( uplo, n, kd, ab, ldab, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_dpbtrs( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_dpbsv


     pure module subroutine stdlib_I64_cpbsv( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
     !! CPBSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L * L**H,  if UPLO = 'L',
     !! where U is an upper triangular band matrix, and L is a lower
     !! triangular band matrix, with the same number of superdiagonals or
     !! subdiagonals as A.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( kd<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( ldab<kd+1 ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CPBSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**h*u or a = l*l**h.
           call stdlib_I64_cpbtrf( uplo, n, kd, ab, ldab, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_cpbtrs( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_cpbsv

     pure module subroutine stdlib_I64_zpbsv( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
     !! ZPBSV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! The Cholesky decomposition is used to factor A as
     !! A = U**H * U,  if UPLO = 'U', or
     !! A = L * L**H,  if UPLO = 'L',
     !! where U is an upper triangular band matrix, and L is a lower
     !! triangular band matrix, with the same number of superdiagonals or
     !! subdiagonals as A.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( kd<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( ldab<kd+1 ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZPBSV ', -info )
              return
           end if
           ! compute the cholesky factorization a = u**h *u or a = l*l**h.
           call stdlib_I64_zpbtrf( uplo, n, kd, ab, ldab, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_zpbtrs( uplo, n, kd, nrhs, ab, ldab, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_zpbsv




     module subroutine stdlib_I64_spbsvx( fact, uplo, n, kd, nrhs, ab, ldab, afb, ldafb,equed, s, b, ldb, x, &
     !! SPBSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
     !! compute the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldx, rcond, ferr, berr,work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*), s(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ, upper
           integer(ilp64) :: i, infequ, j, j1, j2
           real(sp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           upper = stdlib_lsame( uplo, 'U' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_I64_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( kd<0_ilp64 ) then
              info = -4_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -5_ilp64
           else if( ldab<kd+1 ) then
              info = -7_ilp64
           else if( ldafb<kd+1 ) then
              info = -9_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -10_ilp64
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -11_ilp64
                 else if( n>0_ilp64 ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -13_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -15_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SPBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_spbequ( uplo, n, kd, ab, ldab, s, scond, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_slaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**t *u or a = l*l**t.
              if( upper ) then
                 do j = 1, n
                    j1 = max( j-kd, 1_ilp64 )
                    call stdlib_I64_scopy( j-j1+1, ab( kd+1-j+j1, j ), 1_ilp64,afb( kd+1-j+j1, j ), 1_ilp64 )
                              
                 end do
              else
                 do j = 1, n
                    j2 = min( j+kd, n )
                    call stdlib_I64_scopy( j2-j+1, ab( 1_ilp64, j ), 1_ilp64, afb( 1_ilp64, j ), 1_ilp64 )
                 end do
              end if
              call stdlib_I64_spbtrf( uplo, n, kd, afb, ldafb, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_slansb( '1', uplo, n, kd, ab, ldab, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_spbcon( uplo, n, kd, afb, ldafb, anorm, rcond, work, iwork,info )
           ! compute the solution matrix x.
           call stdlib_I64_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_spbtrs( uplo, n, kd, nrhs, afb, ldafb, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_spbrfs( uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b, ldb, x,ldx, ferr, berr,&
                      work, iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_spbsvx

     module subroutine stdlib_I64_dpbsvx( fact, uplo, n, kd, nrhs, ab, ldab, afb, ldafb,equed, s, b, ldb, x, &
     !! DPBSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
     !! compute the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N symmetric positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldx, rcond, ferr, berr,work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*), s(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ, upper
           integer(ilp64) :: i, infequ, j, j1, j2
           real(dp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           upper = stdlib_lsame( uplo, 'U' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_I64_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( kd<0_ilp64 ) then
              info = -4_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -5_ilp64
           else if( ldab<kd+1 ) then
              info = -7_ilp64
           else if( ldafb<kd+1 ) then
              info = -9_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -10_ilp64
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -11_ilp64
                 else if( n>0_ilp64 ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -13_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -15_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DPBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_dpbequ( uplo, n, kd, ab, ldab, s, scond, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_dlaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**t *u or a = l*l**t.
              if( upper ) then
                 do j = 1, n
                    j1 = max( j-kd, 1_ilp64 )
                    call stdlib_I64_dcopy( j-j1+1, ab( kd+1-j+j1, j ), 1_ilp64,afb( kd+1-j+j1, j ), 1_ilp64 )
                              
                 end do
              else
                 do j = 1, n
                    j2 = min( j+kd, n )
                    call stdlib_I64_dcopy( j2-j+1, ab( 1_ilp64, j ), 1_ilp64, afb( 1_ilp64, j ), 1_ilp64 )
                 end do
              end if
              call stdlib_I64_dpbtrf( uplo, n, kd, afb, ldafb, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_dlansb( '1', uplo, n, kd, ab, ldab, work )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_dpbcon( uplo, n, kd, afb, ldafb, anorm, rcond, work, iwork,info )
           ! compute the solution matrix x.
           call stdlib_I64_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_dpbtrs( uplo, n, kd, nrhs, afb, ldafb, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_dpbrfs( uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b, ldb, x,ldx, ferr, berr,&
                      work, iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_dpbsvx


     module subroutine stdlib_I64_cpbsvx( fact, uplo, n, kd, nrhs, ab, ldab, afb, ldafb,equed, s, b, ldb, x, &
     !! CPBSVX uses the Cholesky factorization A = U**H*U or A = L*L**H to
     !! compute the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldx, rcond, ferr, berr,work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(sp), intent(inout) :: s(*)
           complex(sp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ, upper
           integer(ilp64) :: i, infequ, j, j1, j2
           real(sp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           upper = stdlib_lsame( uplo, 'U' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_I64_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( kd<0_ilp64 ) then
              info = -4_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -5_ilp64
           else if( ldab<kd+1 ) then
              info = -7_ilp64
           else if( ldafb<kd+1 ) then
              info = -9_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -10_ilp64
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -11_ilp64
                 else if( n>0_ilp64 ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -13_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -15_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CPBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_cpbequ( uplo, n, kd, ab, ldab, s, scond, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_claqhb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**h *u or a = l*l**h.
              if( upper ) then
                 do j = 1, n
                    j1 = max( j-kd, 1_ilp64 )
                    call stdlib_I64_ccopy( j-j1+1, ab( kd+1-j+j1, j ), 1_ilp64,afb( kd+1-j+j1, j ), 1_ilp64 )
                              
                 end do
              else
                 do j = 1, n
                    j2 = min( j+kd, n )
                    call stdlib_I64_ccopy( j2-j+1, ab( 1_ilp64, j ), 1_ilp64, afb( 1_ilp64, j ), 1_ilp64 )
                 end do
              end if
              call stdlib_I64_cpbtrf( uplo, n, kd, afb, ldafb, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_clanhb( '1', uplo, n, kd, ab, ldab, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_cpbcon( uplo, n, kd, afb, ldafb, anorm, rcond, work, rwork,info )
           ! compute the solution matrix x.
           call stdlib_I64_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_cpbtrs( uplo, n, kd, nrhs, afb, ldafb, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_cpbrfs( uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b, ldb, x,ldx, ferr, berr,&
                      work, rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_cpbsvx

     module subroutine stdlib_I64_zpbsvx( fact, uplo, n, kd, nrhs, ab, ldab, afb, ldafb,equed, s, b, ldb, x, &
     !! ZPBSVX uses the Cholesky factorization A = U**H*U or A = L*L**H to
     !! compute the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N Hermitian positive definite band matrix and X
     !! and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldx, rcond, ferr, berr,work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(dp), intent(inout) :: s(*)
           complex(dp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: equil, nofact, rcequ, upper
           integer(ilp64) :: i, infequ, j, j1, j2
           real(dp) :: amax, anorm, bignum, scond, smax, smin, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           upper = stdlib_lsame( uplo, 'U' )
           if( nofact .or. equil ) then
              equed = 'N'
              rcequ = .false.
           else
              rcequ = stdlib_lsame( equed, 'Y' )
              smlnum = stdlib_I64_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( kd<0_ilp64 ) then
              info = -4_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -5_ilp64
           else if( ldab<kd+1 ) then
              info = -7_ilp64
           else if( ldafb<kd+1 ) then
              info = -9_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rcequ .or. stdlib_lsame( equed, 'N' ) )&
                      ) then
              info = -10_ilp64
           else
              if( rcequ ) then
                 smin = bignum
                 smax = zero
                 do j = 1, n
                    smin = min( smin, s( j ) )
                    smax = max( smax, s( j ) )
                 end do
                 if( smin<=zero ) then
                    info = -11_ilp64
                 else if( n>0_ilp64 ) then
                    scond = max( smin, smlnum ) / min( smax, bignum )
                 else
                    scond = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -13_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -15_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZPBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_zpbequ( uplo, n, kd, ab, ldab, s, scond, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_zlaqhb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
                 rcequ = stdlib_lsame( equed, 'Y' )
              end if
           end if
           ! scale the right-hand side.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = s( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the cholesky factorization a = u**h *u or a = l*l**h.
              if( upper ) then
                 do j = 1, n
                    j1 = max( j-kd, 1_ilp64 )
                    call stdlib_I64_zcopy( j-j1+1, ab( kd+1-j+j1, j ), 1_ilp64,afb( kd+1-j+j1, j ), 1_ilp64 )
                              
                 end do
              else
                 do j = 1, n
                    j2 = min( j+kd, n )
                    call stdlib_I64_zcopy( j2-j+1, ab( 1_ilp64, j ), 1_ilp64, afb( 1_ilp64, j ), 1_ilp64 )
                 end do
              end if
              call stdlib_I64_zpbtrf( uplo, n, kd, afb, ldafb, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_zlanhb( '1', uplo, n, kd, ab, ldab, rwork )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_zpbcon( uplo, n, kd, afb, ldafb, anorm, rcond, work, rwork,info )
           ! compute the solution matrix x.
           call stdlib_I64_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_zpbtrs( uplo, n, kd, nrhs, afb, ldafb, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_zpbrfs( uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b, ldb, x,ldx, ferr, berr,&
                      work, rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( rcequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = s( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / scond
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_zpbsvx




     pure module subroutine stdlib_I64_sptsv( n, nrhs, d, e, b, ldb, info )
     !! SPTSV computes the solution to a real system of linear equations
     !! A*X = B, where A is an N-by-N symmetric positive definite tridiagonal
     !! matrix, and X and B are N-by-NRHS matrices.
     !! A is factored as A = L*D*L**T, and the factored form of A is then
     !! used to solve the system of equations.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: b(ldb,*), d(*), e(*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -2_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SPTSV ', -info )
              return
           end if
           ! compute the l*d*l**t (or u**t*d*u) factorization of a.
           call stdlib_I64_spttrf( n, d, e, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_spttrs( n, nrhs, d, e, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_sptsv

     pure module subroutine stdlib_I64_dptsv( n, nrhs, d, e, b, ldb, info )
     !! DPTSV computes the solution to a real system of linear equations
     !! A*X = B, where A is an N-by-N symmetric positive definite tridiagonal
     !! matrix, and X and B are N-by-NRHS matrices.
     !! A is factored as A = L*D*L**T, and the factored form of A is then
     !! used to solve the system of equations.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: b(ldb,*), d(*), e(*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -2_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DPTSV ', -info )
              return
           end if
           ! compute the l*d*l**t (or u**t*d*u) factorization of a.
           call stdlib_I64_dpttrf( n, d, e, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_dpttrs( n, nrhs, d, e, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_dptsv


     pure module subroutine stdlib_I64_cptsv( n, nrhs, d, e, b, ldb, info )
     !! CPTSV computes the solution to a complex system of linear equations
     !! A*X = B, where A is an N-by-N Hermitian positive definite tridiagonal
     !! matrix, and X and B are N-by-NRHS matrices.
     !! A is factored as A = L*D*L**H, and the factored form of A is then
     !! used to solve the system of equations.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: d(*)
           complex(sp), intent(inout) :: b(ldb,*), e(*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -2_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CPTSV ', -info )
              return
           end if
           ! compute the l*d*l**h (or u**h*d*u) factorization of a.
           call stdlib_I64_cpttrf( n, d, e, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_cpttrs( 'LOWER', n, nrhs, d, e, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_cptsv

     pure module subroutine stdlib_I64_zptsv( n, nrhs, d, e, b, ldb, info )
     !! ZPTSV computes the solution to a complex system of linear equations
     !! A*X = B, where A is an N-by-N Hermitian positive definite tridiagonal
     !! matrix, and X and B are N-by-NRHS matrices.
     !! A is factored as A = L*D*L**H, and the factored form of A is then
     !! used to solve the system of equations.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: d(*)
           complex(dp), intent(inout) :: b(ldb,*), e(*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -2_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZPTSV ', -info )
              return
           end if
           ! compute the l*d*l**h (or u**h*d*u) factorization of a.
           call stdlib_I64_zpttrf( n, d, e, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_zpttrs( 'LOWER', n, nrhs, d, e, b, ldb, info )
           end if
           return
     end subroutine stdlib_I64_zptsv




     pure module subroutine stdlib_I64_sptsvx( fact, n, nrhs, d, e, df, ef, b, ldb, x, ldx,rcond, ferr, berr,&
     !! SPTSVX uses the factorization A = L*D*L**T to compute the solution
     !! to a real system of linear equations A*X = B, where A is an N-by-N
     !! symmetric positive definite tridiagonal matrix and X and B are
     !! N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                work, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(in) :: b(ldb,*), d(*), e(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
           real(sp), intent(inout) :: df(*), ef(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact
           real(sp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           else if( ldx<max( 1_ilp64, n ) ) then
              info = -11_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SPTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the l*d*l**t (or u**t*d*u) factorization of a.
              call stdlib_I64_scopy( n, d, 1_ilp64, df, 1_ilp64 )
              if( n>1_ilp64 )call stdlib_I64_scopy( n-1, e, 1_ilp64, ef, 1_ilp64 )
              call stdlib_I64_spttrf( n, df, ef, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_slanst( '1', n, d, e )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_sptcon( n, df, ef, anorm, rcond, work, info )
           ! compute the solution vectors x.
           call stdlib_I64_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_spttrs( n, nrhs, df, ef, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_I64_sptrfs( n, nrhs, d, e, df, ef, b, ldb, x, ldx, ferr, berr,work, info )
                     
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_sptsvx

     pure module subroutine stdlib_I64_dptsvx( fact, n, nrhs, d, e, df, ef, b, ldb, x, ldx,rcond, ferr, berr,&
     !! DPTSVX uses the factorization A = L*D*L**T to compute the solution
     !! to a real system of linear equations A*X = B, where A is an N-by-N
     !! symmetric positive definite tridiagonal matrix and X and B are
     !! N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                work, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(in) :: b(ldb,*), d(*), e(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
           real(dp), intent(inout) :: df(*), ef(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact
           real(dp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           else if( ldx<max( 1_ilp64, n ) ) then
              info = -11_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DPTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the l*d*l**t (or u**t*d*u) factorization of a.
              call stdlib_I64_dcopy( n, d, 1_ilp64, df, 1_ilp64 )
              if( n>1_ilp64 )call stdlib_I64_dcopy( n-1, e, 1_ilp64, ef, 1_ilp64 )
              call stdlib_I64_dpttrf( n, df, ef, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_dlanst( '1', n, d, e )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_dptcon( n, df, ef, anorm, rcond, work, info )
           ! compute the solution vectors x.
           call stdlib_I64_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_dpttrs( n, nrhs, df, ef, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_I64_dptrfs( n, nrhs, d, e, df, ef, b, ldb, x, ldx, ferr, berr,work, info )
                     
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_dptsvx


     pure module subroutine stdlib_I64_cptsvx( fact, n, nrhs, d, e, df, ef, b, ldb, x, ldx,rcond, ferr, berr,&
     !! CPTSVX uses the factorization A = L*D*L**H to compute the solution
     !! to a complex system of linear equations A*X = B, where A is an
     !! N-by-N Hermitian positive definite tridiagonal matrix and X and B
     !! are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(sp), intent(in) :: d(*)
           real(sp), intent(inout) :: df(*)
           complex(sp), intent(in) :: b(ldb,*), e(*)
           complex(sp), intent(inout) :: ef(*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact
           real(sp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           else if( ldx<max( 1_ilp64, n ) ) then
              info = -11_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CPTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the l*d*l**h (or u**h*d*u) factorization of a.
              call stdlib_I64_scopy( n, d, 1_ilp64, df, 1_ilp64 )
              if( n>1_ilp64 )call stdlib_I64_ccopy( n-1, e, 1_ilp64, ef, 1_ilp64 )
              call stdlib_I64_cpttrf( n, df, ef, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_clanht( '1', n, d, e )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_cptcon( n, df, ef, anorm, rcond, rwork, info )
           ! compute the solution vectors x.
           call stdlib_I64_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_cpttrs( 'LOWER', n, nrhs, df, ef, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_I64_cptrfs( 'LOWER', n, nrhs, d, e, df, ef, b, ldb, x, ldx, ferr,berr, work, &
                     rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_cptsvx

     pure module subroutine stdlib_I64_zptsvx( fact, n, nrhs, d, e, df, ef, b, ldb, x, ldx,rcond, ferr, berr,&
     !! ZPTSVX uses the factorization A = L*D*L**H to compute the solution
     !! to a complex system of linear equations A*X = B, where A is an
     !! N-by-N Hermitian positive definite tridiagonal matrix and X and B
     !! are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
                work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(dp), intent(in) :: d(*)
           real(dp), intent(inout) :: df(*)
           complex(dp), intent(in) :: b(ldb,*), e(*)
           complex(dp), intent(inout) :: ef(*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact
           real(dp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -3_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           else if( ldx<max( 1_ilp64, n ) ) then
              info = -11_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZPTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the l*d*l**h (or u**h*d*u) factorization of a.
              call stdlib_I64_dcopy( n, d, 1_ilp64, df, 1_ilp64 )
              if( n>1_ilp64 )call stdlib_I64_zcopy( n-1, e, 1_ilp64, ef, 1_ilp64 )
              call stdlib_I64_zpttrf( n, df, ef, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           anorm = stdlib_I64_zlanht( '1', n, d, e )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_zptcon( n, df, ef, anorm, rcond, rwork, info )
           ! compute the solution vectors x.
           call stdlib_I64_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_zpttrs( 'LOWER', n, nrhs, df, ef, x, ldx, info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_I64_zptrfs( 'LOWER', n, nrhs, d, e, df, ef, b, ldb, x, ldx, ferr,berr, work, &
                     rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_zptsvx



end submodule stdlib_lapack_solve_chol
