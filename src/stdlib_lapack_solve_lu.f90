submodule(stdlib_lapack_solve) stdlib_lapack_solve_lu
  implicit none


  contains

     pure module subroutine stdlib_sgesv( n, nrhs, a, lda, ipiv, b, ldb, info )
     !! SGESV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as
     !! A = P * L * U,
     !! where P is a permutation matrix, L is unit lower triangular, and U is
     !! upper triangular.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGESV ', -info )
              return
           end if
           ! compute the lu factorization of a.
           call stdlib_sgetrf( n, n, a, lda, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_sgetrs( 'NO TRANSPOSE', n, nrhs, a, lda, ipiv, b, ldb,info )
           end if
           return
     end subroutine stdlib_sgesv

     pure module subroutine stdlib_dgesv( n, nrhs, a, lda, ipiv, b, ldb, info )
     !! DGESV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as
     !! A = P * L * U,
     !! where P is a permutation matrix, L is unit lower triangular, and U is
     !! upper triangular.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGESV ', -info )
              return
           end if
           ! compute the lu factorization of a.
           call stdlib_dgetrf( n, n, a, lda, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_dgetrs( 'NO TRANSPOSE', n, nrhs, a, lda, ipiv, b, ldb,info )
           end if
           return
     end subroutine stdlib_dgesv


     pure module subroutine stdlib_cgesv( n, nrhs, a, lda, ipiv, b, ldb, info )
     !! CGESV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as
     !! A = P * L * U,
     !! where P is a permutation matrix, L is unit lower triangular, and U is
     !! upper triangular.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGESV ', -info )
              return
           end if
           ! compute the lu factorization of a.
           call stdlib_cgetrf( n, n, a, lda, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_cgetrs( 'NO TRANSPOSE', n, nrhs, a, lda, ipiv, b, ldb,info )
           end if
           return
     end subroutine stdlib_cgesv

     pure module subroutine stdlib_zgesv( n, nrhs, a, lda, ipiv, b, ldb, info )
     !! ZGESV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as
     !! A = P * L * U,
     !! where P is a permutation matrix, L is unit lower triangular, and U is
     !! upper triangular.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGESV ', -info )
              return
           end if
           ! compute the lu factorization of a.
           call stdlib_zgetrf( n, n, a, lda, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_zgetrs( 'NO TRANSPOSE', n, nrhs, a, lda, ipiv, b, ldb,info )
           end if
           return
     end subroutine stdlib_zgesv




     module subroutine stdlib_sgesvx( fact, trans, n, nrhs, a, lda, af, ldaf, ipiv,equed, r, c, b, ldb, &
     !! SGESVX uses the LU factorization to compute the solution to a real
     !! system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               x, ldx, rcond, ferr, berr,work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*), c(*), r(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp) :: i, infequ, j
           real(sp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -10_ilp
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -11_ilp
                 else if( n>0_ilp ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -12_ilp
                 else if( n>0_ilp ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -14_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -16_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGESVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_sgeequ( n, n, a, lda, r, c, rowcnd, colcnd, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_slaqge( n, n, a, lda, r, c, rowcnd, colcnd, amax,equed )
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of a.
              call stdlib_slacpy( 'FULL', n, n, a, lda, af, ldaf )
              call stdlib_sgetrf( n, n, af, ldaf, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 rpvgrw = stdlib_slantr( 'M', 'U', 'N', info, info, af, ldaf,work )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = stdlib_slange( 'M', n, info, a, lda, work ) / rpvgrw
                 end if
                 work( 1_ilp ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_slange( norm, n, n, a, lda, work )
           rpvgrw = stdlib_slantr( 'M', 'U', 'N', n, n, af, ldaf, work )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_slange( 'M', n, n, a, lda, work ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_sgecon( norm, n, af, ldaf, anorm, rcond, work, iwork, info )
           ! compute the solution matrix x.
           call stdlib_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_sgetrs( trans, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_sgerfs( trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           work( 1_ilp ) = rpvgrw
           return
     end subroutine stdlib_sgesvx

     module subroutine stdlib_dgesvx( fact, trans, n, nrhs, a, lda, af, ldaf, ipiv,equed, r, c, b, ldb, &
     !! DGESVX uses the LU factorization to compute the solution to a real
     !! system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               x, ldx, rcond, ferr, berr,work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*), c(*), r(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp) :: i, infequ, j
           real(dp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -10_ilp
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -11_ilp
                 else if( n>0_ilp ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -12_ilp
                 else if( n>0_ilp ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -14_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -16_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGESVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_dgeequ( n, n, a, lda, r, c, rowcnd, colcnd, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_dlaqge( n, n, a, lda, r, c, rowcnd, colcnd, amax,equed )
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of a.
              call stdlib_dlacpy( 'FULL', n, n, a, lda, af, ldaf )
              call stdlib_dgetrf( n, n, af, ldaf, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 rpvgrw = stdlib_dlantr( 'M', 'U', 'N', info, info, af, ldaf,work )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = stdlib_dlange( 'M', n, info, a, lda, work ) / rpvgrw
                 end if
                 work( 1_ilp ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_dlange( norm, n, n, a, lda, work )
           rpvgrw = stdlib_dlantr( 'M', 'U', 'N', n, n, af, ldaf, work )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_dlange( 'M', n, n, a, lda, work ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_dgecon( norm, n, af, ldaf, anorm, rcond, work, iwork, info )
           ! compute the solution matrix x.
           call stdlib_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_dgetrs( trans, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_dgerfs( trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           work( 1_ilp ) = rpvgrw
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_dgesvx


     module subroutine stdlib_cgesvx( fact, trans, n, nrhs, a, lda, af, ldaf, ipiv,equed, r, c, b, ldb, &
     !! CGESVX uses the LU factorization to compute the solution to a complex
     !! system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               x, ldx, rcond, ferr, berr,work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(sp), intent(inout) :: c(*), r(*)
           complex(sp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp) :: i, infequ, j
           real(sp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -10_ilp
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -11_ilp
                 else if( n>0_ilp ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -12_ilp
                 else if( n>0_ilp ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -14_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -16_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGESVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_cgeequ( n, n, a, lda, r, c, rowcnd, colcnd, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_claqge( n, n, a, lda, r, c, rowcnd, colcnd, amax,equed )
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of a.
              call stdlib_clacpy( 'FULL', n, n, a, lda, af, ldaf )
              call stdlib_cgetrf( n, n, af, ldaf, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 rpvgrw = stdlib_clantr( 'M', 'U', 'N', info, info, af, ldaf,rwork )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = stdlib_clange( 'M', n, info, a, lda, rwork ) /rpvgrw
                 end if
                 rwork( 1_ilp ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_clange( norm, n, n, a, lda, rwork )
           rpvgrw = stdlib_clantr( 'M', 'U', 'N', n, n, af, ldaf, rwork )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_clange( 'M', n, n, a, lda, rwork ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_cgecon( norm, n, af, ldaf, anorm, rcond, work, rwork, info )
           ! compute the solution matrix x.
           call stdlib_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_cgetrs( trans, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_cgerfs( trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           rwork( 1_ilp ) = rpvgrw
           return
     end subroutine stdlib_cgesvx

     module subroutine stdlib_zgesvx( fact, trans, n, nrhs, a, lda, af, ldaf, ipiv,equed, r, c, b, ldb, &
     !! ZGESVX uses the LU factorization to compute the solution to a complex
     !! system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               x, ldx, rcond, ferr, berr,work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(dp), intent(inout) :: c(*), r(*)
           complex(dp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp) :: i, infequ, j
           real(dp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -10_ilp
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -11_ilp
                 else if( n>0_ilp ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -12_ilp
                 else if( n>0_ilp ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -14_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -16_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGESVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_zgeequ( n, n, a, lda, r, c, rowcnd, colcnd, amax, infequ )
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_zlaqge( n, n, a, lda, r, c, rowcnd, colcnd, amax,equed )
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of a.
              call stdlib_zlacpy( 'FULL', n, n, a, lda, af, ldaf )
              call stdlib_zgetrf( n, n, af, ldaf, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 rpvgrw = stdlib_zlantr( 'M', 'U', 'N', info, info, af, ldaf,rwork )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = stdlib_zlange( 'M', n, info, a, lda, rwork ) /rpvgrw
                 end if
                 rwork( 1_ilp ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_zlange( norm, n, n, a, lda, rwork )
           rpvgrw = stdlib_zlantr( 'M', 'U', 'N', n, n, af, ldaf, rwork )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_zlange( 'M', n, n, a, lda, rwork ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_zgecon( norm, n, af, ldaf, anorm, rcond, work, rwork, info )
           ! compute the solution matrix x.
           call stdlib_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_zgetrs( trans, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_zgerfs( trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           rwork( 1_ilp ) = rpvgrw
           return
     end subroutine stdlib_zgesvx




     pure module subroutine stdlib_sgbsv( n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info )
     !! SGBSV computes the solution to a real system of linear equations
     !! A * X = B, where A is a band matrix of order N with KL subdiagonals
     !! and KU superdiagonals, and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as A = L * U, where L is a product of permutation
     !! and unit lower triangular matrices with KL subdiagonals, and U is
     !! upper triangular with KL+KU superdiagonals.  The factored form of A
     !! is then used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( kl<0_ilp ) then
              info = -2_ilp
           else if( ku<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<2_ilp*kl+ku+1 ) then
              info = -6_ilp
           else if( ldb<max( n, 1_ilp ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGBSV ', -info )
              return
           end if
           ! compute the lu factorization of the band matrix a.
           call stdlib_sgbtrf( n, n, kl, ku, ab, ldab, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_sgbtrs( 'NO TRANSPOSE', n, kl, ku, nrhs, ab, ldab, ipiv,b, ldb, info )
                        
           end if
           return
     end subroutine stdlib_sgbsv

     pure module subroutine stdlib_dgbsv( n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info )
     !! DGBSV computes the solution to a real system of linear equations
     !! A * X = B, where A is a band matrix of order N with KL subdiagonals
     !! and KU superdiagonals, and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as A = L * U, where L is a product of permutation
     !! and unit lower triangular matrices with KL subdiagonals, and U is
     !! upper triangular with KL+KU superdiagonals.  The factored form of A
     !! is then used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( kl<0_ilp ) then
              info = -2_ilp
           else if( ku<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<2_ilp*kl+ku+1 ) then
              info = -6_ilp
           else if( ldb<max( n, 1_ilp ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGBSV ', -info )
              return
           end if
           ! compute the lu factorization of the band matrix a.
           call stdlib_dgbtrf( n, n, kl, ku, ab, ldab, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_dgbtrs( 'NO TRANSPOSE', n, kl, ku, nrhs, ab, ldab, ipiv,b, ldb, info )
                        
           end if
           return
     end subroutine stdlib_dgbsv


     pure module subroutine stdlib_cgbsv( n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info )
     !! CGBSV computes the solution to a complex system of linear equations
     !! A * X = B, where A is a band matrix of order N with KL subdiagonals
     !! and KU superdiagonals, and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as A = L * U, where L is a product of permutation
     !! and unit lower triangular matrices with KL subdiagonals, and U is
     !! upper triangular with KL+KU superdiagonals.  The factored form of A
     !! is then used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( kl<0_ilp ) then
              info = -2_ilp
           else if( ku<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<2_ilp*kl+ku+1 ) then
              info = -6_ilp
           else if( ldb<max( n, 1_ilp ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGBSV ', -info )
              return
           end if
           ! compute the lu factorization of the band matrix a.
           call stdlib_cgbtrf( n, n, kl, ku, ab, ldab, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_cgbtrs( 'NO TRANSPOSE', n, kl, ku, nrhs, ab, ldab, ipiv,b, ldb, info )
                        
           end if
           return
     end subroutine stdlib_cgbsv

     pure module subroutine stdlib_zgbsv( n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info )
     !! ZGBSV computes the solution to a complex system of linear equations
     !! A * X = B, where A is a band matrix of order N with KL subdiagonals
     !! and KU superdiagonals, and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as A = L * U, where L is a product of permutation
     !! and unit lower triangular matrices with KL subdiagonals, and U is
     !! upper triangular with KL+KU superdiagonals.  The factored form of A
     !! is then used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( kl<0_ilp ) then
              info = -2_ilp
           else if( ku<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldab<2_ilp*kl+ku+1 ) then
              info = -6_ilp
           else if( ldb<max( n, 1_ilp ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGBSV ', -info )
              return
           end if
           ! compute the lu factorization of the band matrix a.
           call stdlib_zgbtrf( n, n, kl, ku, ab, ldab, ipiv, info )
           if( info==0_ilp ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_zgbtrs( 'NO TRANSPOSE', n, kl, ku, nrhs, ab, ldab, ipiv,b, ldb, info )
                        
           end if
           return
     end subroutine stdlib_zgbsv




     module subroutine stdlib_sgbsvx( fact, trans, n, kl, ku, nrhs, ab, ldab, afb,ldafb, ipiv, equed, r, &
     !! SGBSVX uses the LU factorization to compute the solution to a real
     !! system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
     !! where A is a band matrix of order N with KL subdiagonals and KU
     !! superdiagonals, and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               c, b, ldb, x, ldx,rcond, ferr, berr, work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*), c(*), r(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
        ! moved setting of info = n+1 so info does not subsequently get
        ! overwritten.  sven, 17 mar 05.
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp) :: i, infequ, j, j1, j2
           real(sp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kl<0_ilp ) then
              info = -4_ilp
           else if( ku<0_ilp ) then
              info = -5_ilp
           else if( nrhs<0_ilp ) then
              info = -6_ilp
           else if( ldab<kl+ku+1 ) then
              info = -8_ilp
           else if( ldafb<2_ilp*kl+ku+1 ) then
              info = -10_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -12_ilp
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -13_ilp
                 else if( n>0_ilp ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -14_ilp
                 else if( n>0_ilp ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -16_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -18_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_sgbequ( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, infequ )
                        
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_slaqgb( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, equed )
                           
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of the band matrix a.
              do j = 1, n
                 j1 = max( j-ku, 1_ilp )
                 j2 = min( j+kl, n )
                 call stdlib_scopy( j2-j1+1, ab( ku+1-j+j1, j ), 1_ilp,afb( kl+ku+1-j+j1, j ), 1_ilp )
                           
              end do
              call stdlib_sgbtrf( n, n, kl, ku, afb, ldafb, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 anorm = zero
                 do j = 1, info
                    do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                       anorm = max( anorm, abs( ab( i, j ) ) )
                    end do
                 end do
                 rpvgrw = stdlib_slantb( 'M', 'U', 'N', info, min( info-1, kl+ku ),afb( max( 1_ilp, &
                           kl+ku+2-info ), 1_ilp ), ldafb,work )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = anorm / rpvgrw
                 end if
                 work( 1_ilp ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_slangb( norm, n, kl, ku, ab, ldab, work )
           rpvgrw = stdlib_slantb( 'M', 'U', 'N', n, kl+ku, afb, ldafb, work )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_slangb( 'M', n, kl, ku, ab, ldab, work ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_sgbcon( norm, n, kl, ku, afb, ldafb, ipiv, anorm, rcond,work, iwork, info )
                     
           ! compute the solution matrix x.
           call stdlib_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_sgbtrs( trans, n, kl, ku, nrhs, afb, ldafb, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_sgbrfs( trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           work( 1_ilp ) = rpvgrw
           return
     end subroutine stdlib_sgbsvx

     module subroutine stdlib_dgbsvx( fact, trans, n, kl, ku, nrhs, ab, ldab, afb,ldafb, ipiv, equed, r, &
     !! DGBSVX uses the LU factorization to compute the solution to a real
     !! system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
     !! where A is a band matrix of order N with KL subdiagonals and KU
     !! superdiagonals, and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               c, b, ldb, x, ldx,rcond, ferr, berr, work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*), c(*), r(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp) :: i, infequ, j, j1, j2
           real(dp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kl<0_ilp ) then
              info = -4_ilp
           else if( ku<0_ilp ) then
              info = -5_ilp
           else if( nrhs<0_ilp ) then
              info = -6_ilp
           else if( ldab<kl+ku+1 ) then
              info = -8_ilp
           else if( ldafb<2_ilp*kl+ku+1 ) then
              info = -10_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -12_ilp
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -13_ilp
                 else if( n>0_ilp ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -14_ilp
                 else if( n>0_ilp ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -16_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -18_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_dgbequ( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, infequ )
                        
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_dlaqgb( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, equed )
                           
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of the band matrix a.
              do j = 1, n
                 j1 = max( j-ku, 1_ilp )
                 j2 = min( j+kl, n )
                 call stdlib_dcopy( j2-j1+1, ab( ku+1-j+j1, j ), 1_ilp,afb( kl+ku+1-j+j1, j ), 1_ilp )
                           
              end do
              call stdlib_dgbtrf( n, n, kl, ku, afb, ldafb, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 anorm = zero
                 do j = 1, info
                    do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                       anorm = max( anorm, abs( ab( i, j ) ) )
                    end do
                 end do
                 rpvgrw = stdlib_dlantb( 'M', 'U', 'N', info, min( info-1, kl+ku ),afb( max( 1_ilp, &
                           kl+ku+2-info ), 1_ilp ), ldafb,work )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = anorm / rpvgrw
                 end if
                 work( 1_ilp ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_dlangb( norm, n, kl, ku, ab, ldab, work )
           rpvgrw = stdlib_dlantb( 'M', 'U', 'N', n, kl+ku, afb, ldafb, work )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_dlangb( 'M', n, kl, ku, ab, ldab, work ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_dgbcon( norm, n, kl, ku, afb, ldafb, ipiv, anorm, rcond,work, iwork, info )
                     
           ! compute the solution matrix x.
           call stdlib_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_dgbtrs( trans, n, kl, ku, nrhs, afb, ldafb, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_dgbrfs( trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           work( 1_ilp ) = rpvgrw
           return
     end subroutine stdlib_dgbsvx


     module subroutine stdlib_cgbsvx( fact, trans, n, kl, ku, nrhs, ab, ldab, afb,ldafb, ipiv, equed, r, &
     !! CGBSVX uses the LU factorization to compute the solution to a complex
     !! system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
     !! where A is a band matrix of order N with KL subdiagonals and KU
     !! superdiagonals, and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               c, b, ldb, x, ldx,rcond, ferr, berr, work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(sp), intent(inout) :: c(*), r(*)
           complex(sp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
        ! moved setting of info = n+1 so info does not subsequently get
        ! overwritten.  sven, 17 mar 05.
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp) :: i, infequ, j, j1, j2
           real(sp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kl<0_ilp ) then
              info = -4_ilp
           else if( ku<0_ilp ) then
              info = -5_ilp
           else if( nrhs<0_ilp ) then
              info = -6_ilp
           else if( ldab<kl+ku+1 ) then
              info = -8_ilp
           else if( ldafb<2_ilp*kl+ku+1 ) then
              info = -10_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -12_ilp
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -13_ilp
                 else if( n>0_ilp ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -14_ilp
                 else if( n>0_ilp ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -16_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -18_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_cgbequ( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, infequ )
                        
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_claqgb( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, equed )
                           
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of the band matrix a.
              do j = 1, n
                 j1 = max( j-ku, 1_ilp )
                 j2 = min( j+kl, n )
                 call stdlib_ccopy( j2-j1+1, ab( ku+1-j+j1, j ), 1_ilp,afb( kl+ku+1-j+j1, j ), 1_ilp )
                           
              end do
              call stdlib_cgbtrf( n, n, kl, ku, afb, ldafb, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 anorm = zero
                 do j = 1, info
                    do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                       anorm = max( anorm, abs( ab( i, j ) ) )
                    end do
                 end do
                 rpvgrw = stdlib_clantb( 'M', 'U', 'N', info, min( info-1, kl+ku ),afb( max( 1_ilp, &
                           kl+ku+2-info ), 1_ilp ), ldafb,rwork )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = anorm / rpvgrw
                 end if
                 rwork( 1_ilp ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_clangb( norm, n, kl, ku, ab, ldab, rwork )
           rpvgrw = stdlib_clantb( 'M', 'U', 'N', n, kl+ku, afb, ldafb, rwork )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_clangb( 'M', n, kl, ku, ab, ldab, rwork ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_cgbcon( norm, n, kl, ku, afb, ldafb, ipiv, anorm, rcond,work, rwork, info )
                     
           ! compute the solution matrix x.
           call stdlib_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_cgbtrs( trans, n, kl, ku, nrhs, afb, ldafb, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_cgbrfs( trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           rwork( 1_ilp ) = rpvgrw
           return
     end subroutine stdlib_cgbsvx

     module subroutine stdlib_zgbsvx( fact, trans, n, kl, ku, nrhs, ab, ldab, afb,ldafb, ipiv, equed, r, &
     !! ZGBSVX uses the LU factorization to compute the solution to a complex
     !! system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
     !! where A is a band matrix of order N with KL subdiagonals and KU
     !! superdiagonals, and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               c, b, ldb, x, ldx,rcond, ferr, berr, work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(dp), intent(inout) :: c(*), r(*)
           complex(dp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
        ! moved setting of info = n+1 so info does not subsequently get
        ! overwritten.  sven, 17 mar 05.
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp) :: i, infequ, j, j1, j2
           real(dp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kl<0_ilp ) then
              info = -4_ilp
           else if( ku<0_ilp ) then
              info = -5_ilp
           else if( nrhs<0_ilp ) then
              info = -6_ilp
           else if( ldab<kl+ku+1 ) then
              info = -8_ilp
           else if( ldafb<2_ilp*kl+ku+1 ) then
              info = -10_ilp
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -12_ilp
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -13_ilp
                 else if( n>0_ilp ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -14_ilp
                 else if( n>0_ilp ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp ) then
                 if( ldb<max( 1_ilp, n ) ) then
                    info = -16_ilp
                 else if( ldx<max( 1_ilp, n ) ) then
                    info = -18_ilp
                 end if
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_zgbequ( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, infequ )
                        
              if( infequ==0_ilp ) then
                 ! equilibrate the matrix.
                 call stdlib_zlaqgb( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, equed )
                           
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of the band matrix a.
              do j = 1, n
                 j1 = max( j-ku, 1_ilp )
                 j2 = min( j+kl, n )
                 call stdlib_zcopy( j2-j1+1, ab( ku+1-j+j1, j ), 1_ilp,afb( kl+ku+1-j+j1, j ), 1_ilp )
                           
              end do
              call stdlib_zgbtrf( n, n, kl, ku, afb, ldafb, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 anorm = zero
                 do j = 1, info
                    do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                       anorm = max( anorm, abs( ab( i, j ) ) )
                    end do
                 end do
                 rpvgrw = stdlib_zlantb( 'M', 'U', 'N', info, min( info-1, kl+ku ),afb( max( 1_ilp, &
                           kl+ku+2-info ), 1_ilp ), ldafb,rwork )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = anorm / rpvgrw
                 end if
                 rwork( 1_ilp ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_zlangb( norm, n, kl, ku, ab, ldab, rwork )
           rpvgrw = stdlib_zlantb( 'M', 'U', 'N', n, kl+ku, afb, ldafb, rwork )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_zlangb( 'M', n, kl, ku, ab, ldab, rwork ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_zgbcon( norm, n, kl, ku, afb, ldafb, ipiv, anorm, rcond,work, rwork, info )
                     
           ! compute the solution matrix x.
           call stdlib_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_zgbtrs( trans, n, kl, ku, nrhs, afb, ldafb, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_zgbrfs( trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           rwork( 1_ilp ) = rpvgrw
           return
     end subroutine stdlib_zgbsvx




     pure module subroutine stdlib_sgtsv( n, nrhs, dl, d, du, b, ldb, info )
     !! SGTSV solves the equation
     !! A*X = B,
     !! where A is an n by n tridiagonal matrix, by Gaussian elimination with
     !! partial pivoting.
     !! Note that the equation  A**T*X = B  may be solved by interchanging the
     !! order of the arguments DU and DL.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: b(ldb,*), d(*), dl(*), du(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: fact, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGTSV ', -info )
              return
           end if
           if( n==0 )return
           if( nrhs==1_ilp ) then
              loop_10: do i = 1, n - 2
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    ! no row interchange required
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       b( i+1, 1_ilp ) = b( i+1, 1_ilp ) - fact*b( i, 1_ilp )
                    else
                       info = i
                       return
                    end if
                    dl( i ) = zero
                 else
                    ! interchange rows i and i+1
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    dl( i ) = du( i+1 )
                    du( i+1 ) = -fact*dl( i )
                    du( i ) = temp
                    temp = b( i, 1_ilp )
                    b( i, 1_ilp ) = b( i+1, 1_ilp )
                    b( i+1, 1_ilp ) = temp - fact*b( i+1, 1_ilp )
                 end if
              end do loop_10
              if( n>1_ilp ) then
                 i = n - 1_ilp
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       b( i+1, 1_ilp ) = b( i+1, 1_ilp ) - fact*b( i, 1_ilp )
                    else
                       info = i
                       return
                    end if
                 else
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    du( i ) = temp
                    temp = b( i, 1_ilp )
                    b( i, 1_ilp ) = b( i+1, 1_ilp )
                    b( i+1, 1_ilp ) = temp - fact*b( i+1, 1_ilp )
                 end if
              end if
              if( d( n )==zero ) then
                 info = n
                 return
              end if
           else
              loop_40: do i = 1, n - 2
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    ! no row interchange required
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       do j = 1, nrhs
                          b( i+1, j ) = b( i+1, j ) - fact*b( i, j )
                       end do
                    else
                       info = i
                       return
                    end if
                    dl( i ) = zero
                 else
                    ! interchange rows i and i+1
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    dl( i ) = du( i+1 )
                    du( i+1 ) = -fact*dl( i )
                    du( i ) = temp
                    do j = 1, nrhs
                       temp = b( i, j )
                       b( i, j ) = b( i+1, j )
                       b( i+1, j ) = temp - fact*b( i+1, j )
                    end do
                 end if
              end do loop_40
              if( n>1_ilp ) then
                 i = n - 1_ilp
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       do j = 1, nrhs
                          b( i+1, j ) = b( i+1, j ) - fact*b( i, j )
                       end do
                    else
                       info = i
                       return
                    end if
                 else
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    du( i ) = temp
                    do j = 1, nrhs
                       temp = b( i, j )
                       b( i, j ) = b( i+1, j )
                       b( i+1, j ) = temp - fact*b( i+1, j )
                    end do
                 end if
              end if
              if( d( n )==zero ) then
                 info = n
                 return
              end if
           end if
           ! back solve with the matrix u from the factorization.
           if( nrhs<=2_ilp ) then
              j = 1_ilp
              70 continue
              b( n, j ) = b( n, j ) / d( n )
              if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) / d( n-1 )
              do i = n - 2, 1, -1
                 b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-dl( i )*b( i+2, j ) ) / d( i )
                           
              end do
              if( j<nrhs ) then
                 j = j + 1_ilp
                 go to 70
              end if
           else
              do j = 1, nrhs
                 b( n, j ) = b( n, j ) / d( n )
                 if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) /d( n-1 )
                 do i = n - 2, 1, -1
                    b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-dl( i )*b( i+2, j ) ) / d( i )
                              
                 end do
              end do
           end if
           return
     end subroutine stdlib_sgtsv

     pure module subroutine stdlib_dgtsv( n, nrhs, dl, d, du, b, ldb, info )
     !! DGTSV solves the equation
     !! A*X = B,
     !! where A is an n by n tridiagonal matrix, by Gaussian elimination with
     !! partial pivoting.
     !! Note that the equation  A**T*X = B  may be solved by interchanging the
     !! order of the arguments DU and DL.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: b(ldb,*), d(*), dl(*), du(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: fact, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGTSV ', -info )
              return
           end if
           if( n==0 )return
           if( nrhs==1_ilp ) then
              loop_10: do i = 1, n - 2
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    ! no row interchange required
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       b( i+1, 1_ilp ) = b( i+1, 1_ilp ) - fact*b( i, 1_ilp )
                    else
                       info = i
                       return
                    end if
                    dl( i ) = zero
                 else
                    ! interchange rows i and i+1
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    dl( i ) = du( i+1 )
                    du( i+1 ) = -fact*dl( i )
                    du( i ) = temp
                    temp = b( i, 1_ilp )
                    b( i, 1_ilp ) = b( i+1, 1_ilp )
                    b( i+1, 1_ilp ) = temp - fact*b( i+1, 1_ilp )
                 end if
              end do loop_10
              if( n>1_ilp ) then
                 i = n - 1_ilp
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       b( i+1, 1_ilp ) = b( i+1, 1_ilp ) - fact*b( i, 1_ilp )
                    else
                       info = i
                       return
                    end if
                 else
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    du( i ) = temp
                    temp = b( i, 1_ilp )
                    b( i, 1_ilp ) = b( i+1, 1_ilp )
                    b( i+1, 1_ilp ) = temp - fact*b( i+1, 1_ilp )
                 end if
              end if
              if( d( n )==zero ) then
                 info = n
                 return
              end if
           else
              loop_40: do i = 1, n - 2
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    ! no row interchange required
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       do j = 1, nrhs
                          b( i+1, j ) = b( i+1, j ) - fact*b( i, j )
                       end do
                    else
                       info = i
                       return
                    end if
                    dl( i ) = zero
                 else
                    ! interchange rows i and i+1
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    dl( i ) = du( i+1 )
                    du( i+1 ) = -fact*dl( i )
                    du( i ) = temp
                    do j = 1, nrhs
                       temp = b( i, j )
                       b( i, j ) = b( i+1, j )
                       b( i+1, j ) = temp - fact*b( i+1, j )
                    end do
                 end if
              end do loop_40
              if( n>1_ilp ) then
                 i = n - 1_ilp
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       do j = 1, nrhs
                          b( i+1, j ) = b( i+1, j ) - fact*b( i, j )
                       end do
                    else
                       info = i
                       return
                    end if
                 else
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    du( i ) = temp
                    do j = 1, nrhs
                       temp = b( i, j )
                       b( i, j ) = b( i+1, j )
                       b( i+1, j ) = temp - fact*b( i+1, j )
                    end do
                 end if
              end if
              if( d( n )==zero ) then
                 info = n
                 return
              end if
           end if
           ! back solve with the matrix u from the factorization.
           if( nrhs<=2_ilp ) then
              j = 1_ilp
              70 continue
              b( n, j ) = b( n, j ) / d( n )
              if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) / d( n-1 )
              do i = n - 2, 1, -1
                 b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-dl( i )*b( i+2, j ) ) / d( i )
                           
              end do
              if( j<nrhs ) then
                 j = j + 1_ilp
                 go to 70
              end if
           else
              do j = 1, nrhs
                 b( n, j ) = b( n, j ) / d( n )
                 if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) /d( n-1 )
                 do i = n - 2, 1, -1
                    b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-dl( i )*b( i+2, j ) ) / d( i )
                              
                 end do
              end do
           end if
           return
     end subroutine stdlib_dgtsv


     pure module subroutine stdlib_cgtsv( n, nrhs, dl, d, du, b, ldb, info )
     !! CGTSV solves the equation
     !! A*X = B,
     !! where A is an N-by-N tridiagonal matrix, by Gaussian elimination with
     !! partial pivoting.
     !! Note that the equation  A**T *X = B  may be solved by interchanging the
     !! order of the arguments DU and DL.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(inout) :: b(ldb,*), d(*), dl(*), du(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, k
           complex(sp) :: mult, temp, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGTSV ', -info )
              return
           end if
           if( n==0 )return
           loop_30: do k = 1, n - 1
              if( dl( k )==czero ) then
                 ! subdiagonal is czero, no elimination is required.
                 if( d( k )==czero ) then
                    ! diagonal is czero: set info = k and return; a unique
                    ! solution can not be found.
                    info = k
                    return
                 end if
              else if( cabs1( d( k ) )>=cabs1( dl( k ) ) ) then
                 ! no row interchange required
                 mult = dl( k ) / d( k )
                 d( k+1 ) = d( k+1 ) - mult*du( k )
                 do j = 1, nrhs
                    b( k+1, j ) = b( k+1, j ) - mult*b( k, j )
                 end do
                 if( k<( n-1 ) )dl( k ) = czero
              else
                 ! interchange rows k and k+1
                 mult = d( k ) / dl( k )
                 d( k ) = dl( k )
                 temp = d( k+1 )
                 d( k+1 ) = du( k ) - mult*temp
                 if( k<( n-1 ) ) then
                    dl( k ) = du( k+1 )
                    du( k+1 ) = -mult*dl( k )
                 end if
                 du( k ) = temp
                 do j = 1, nrhs
                    temp = b( k, j )
                    b( k, j ) = b( k+1, j )
                    b( k+1, j ) = temp - mult*b( k+1, j )
                 end do
              end if
           end do loop_30
           if( d( n )==czero ) then
              info = n
              return
           end if
           ! back solve with the matrix u from the factorization.
           do j = 1, nrhs
              b( n, j ) = b( n, j ) / d( n )
              if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) / d( n-1 )
              do k = n - 2, 1, -1
                 b( k, j ) = ( b( k, j )-du( k )*b( k+1, j )-dl( k )*b( k+2, j ) ) / d( k )
                           
              end do
           end do
           return
     end subroutine stdlib_cgtsv

     pure module subroutine stdlib_zgtsv( n, nrhs, dl, d, du, b, ldb, info )
     !! ZGTSV solves the equation
     !! A*X = B,
     !! where A is an N-by-N tridiagonal matrix, by Gaussian elimination with
     !! partial pivoting.
     !! Note that the equation  A**T *X = B  may be solved by interchanging the
     !! order of the arguments DU and DL.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(inout) :: b(ldb,*), d(*), dl(*), du(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, k
           complex(dp) :: mult, temp, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( nrhs<0_ilp ) then
              info = -2_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGTSV ', -info )
              return
           end if
           if( n==0 )return
           loop_30: do k = 1, n - 1
              if( dl( k )==czero ) then
                 ! subdiagonal is czero, no elimination is required.
                 if( d( k )==czero ) then
                    ! diagonal is czero: set info = k and return; a unique
                    ! solution can not be found.
                    info = k
                    return
                 end if
              else if( cabs1( d( k ) )>=cabs1( dl( k ) ) ) then
                 ! no row interchange required
                 mult = dl( k ) / d( k )
                 d( k+1 ) = d( k+1 ) - mult*du( k )
                 do j = 1, nrhs
                    b( k+1, j ) = b( k+1, j ) - mult*b( k, j )
                 end do
                 if( k<( n-1 ) )dl( k ) = czero
              else
                 ! interchange rows k and k+1
                 mult = d( k ) / dl( k )
                 d( k ) = dl( k )
                 temp = d( k+1 )
                 d( k+1 ) = du( k ) - mult*temp
                 if( k<( n-1 ) ) then
                    dl( k ) = du( k+1 )
                    du( k+1 ) = -mult*dl( k )
                 end if
                 du( k ) = temp
                 do j = 1, nrhs
                    temp = b( k, j )
                    b( k, j ) = b( k+1, j )
                    b( k+1, j ) = temp - mult*b( k+1, j )
                 end do
              end if
           end do loop_30
           if( d( n )==czero ) then
              info = n
              return
           end if
           ! back solve with the matrix u from the factorization.
           do j = 1, nrhs
              b( n, j ) = b( n, j ) / d( n )
              if( n>1_ilp )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) / d( n-1 )
              do k = n - 2, 1, -1
                 b( k, j ) = ( b( k, j )-du( k )*b( k+1, j )-dl( k )*b( k+2, j ) ) / d( k )
                           
              end do
           end do
           return
     end subroutine stdlib_zgtsv




     pure module subroutine stdlib_sgtsvx( fact, trans, n, nrhs, dl, d, du, dlf, df, duf,du2, ipiv, b, &
     !! SGTSVX uses the LU factorization to compute the solution to a real
     !! system of linear equations A * X = B or A**T * X = B,
     !! where A is a tridiagonal matrix of order N and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldb, x, ldx, rcond, ferr, berr,work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: b(ldb,*), d(*), dl(*), du(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
           real(sp), intent(inout) :: df(*), dlf(*), du2(*), duf(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact, notran
           character :: norm
           real(sp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           notran = stdlib_lsame( trans, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -14_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the lu factorization of a.
              call stdlib_scopy( n, d, 1_ilp, df, 1_ilp )
              if( n>1_ilp ) then
                 call stdlib_scopy( n-1, dl, 1_ilp, dlf, 1_ilp )
                 call stdlib_scopy( n-1, du, 1_ilp, duf, 1_ilp )
              end if
              call stdlib_sgttrf( n, dlf, df, duf, du2, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_slangt( norm, n, dl, d, du )
           ! compute the reciprocal of the condition number of a.
           call stdlib_sgtcon( norm, n, dlf, df, duf, du2, ipiv, anorm, rcond, work,iwork, info )
                     
           ! compute the solution vectors x.
           call stdlib_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_sgttrs( trans, n, nrhs, dlf, df, duf, du2, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_sgtrfs( trans, n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, iwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_sgtsvx

     pure module subroutine stdlib_dgtsvx( fact, trans, n, nrhs, dl, d, du, dlf, df, duf,du2, ipiv, b, &
     !! DGTSVX uses the LU factorization to compute the solution to a real
     !! system of linear equations A * X = B or A**T * X = B,
     !! where A is a tridiagonal matrix of order N and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldb, x, ldx, rcond, ferr, berr,work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: b(ldb,*), d(*), dl(*), du(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
           real(dp), intent(inout) :: df(*), dlf(*), du2(*), duf(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact, notran
           character :: norm
           real(dp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           notran = stdlib_lsame( trans, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -14_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the lu factorization of a.
              call stdlib_dcopy( n, d, 1_ilp, df, 1_ilp )
              if( n>1_ilp ) then
                 call stdlib_dcopy( n-1, dl, 1_ilp, dlf, 1_ilp )
                 call stdlib_dcopy( n-1, du, 1_ilp, duf, 1_ilp )
              end if
              call stdlib_dgttrf( n, dlf, df, duf, du2, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_dlangt( norm, n, dl, d, du )
           ! compute the reciprocal of the condition number of a.
           call stdlib_dgtcon( norm, n, dlf, df, duf, du2, ipiv, anorm, rcond, work,iwork, info )
                     
           ! compute the solution vectors x.
           call stdlib_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_dgttrs( trans, n, nrhs, dlf, df, duf, du2, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_dgtrfs( trans, n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, iwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_dgtsvx


     pure module subroutine stdlib_cgtsvx( fact, trans, n, nrhs, dl, d, du, dlf, df, duf,du2, ipiv, b, &
     !! CGTSVX uses the LU factorization to compute the solution to a complex
     !! system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
     !! where A is a tridiagonal matrix of order N and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldb, x, ldx, rcond, ferr, berr,work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: b(ldb,*), d(*), dl(*), du(*)
           complex(sp), intent(inout) :: df(*), dlf(*), du2(*), duf(*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact, notran
           character :: norm
           real(sp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           notran = stdlib_lsame( trans, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -14_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the lu factorization of a.
              call stdlib_ccopy( n, d, 1_ilp, df, 1_ilp )
              if( n>1_ilp ) then
                 call stdlib_ccopy( n-1, dl, 1_ilp, dlf, 1_ilp )
                 call stdlib_ccopy( n-1, du, 1_ilp, duf, 1_ilp )
              end if
              call stdlib_cgttrf( n, dlf, df, duf, du2, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_clangt( norm, n, dl, d, du )
           ! compute the reciprocal of the condition number of a.
           call stdlib_cgtcon( norm, n, dlf, df, duf, du2, ipiv, anorm, rcond, work,info )
           ! compute the solution vectors x.
           call stdlib_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_cgttrs( trans, n, nrhs, dlf, df, duf, du2, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_cgtrfs( trans, n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_slamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_cgtsvx

     pure module subroutine stdlib_zgtsvx( fact, trans, n, nrhs, dl, d, du, dlf, df, duf,du2, ipiv, b, &
     !! ZGTSVX uses the LU factorization to compute the solution to a complex
     !! system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
     !! where A is a tridiagonal matrix of order N and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldb, x, ldx, rcond, ferr, berr,work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(inout) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: b(ldb,*), d(*), dl(*), du(*)
           complex(dp), intent(inout) :: df(*), dlf(*), du2(*), duf(*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact, notran
           character :: norm
           real(dp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           nofact = stdlib_lsame( fact, 'N' )
           notran = stdlib_lsame( trans, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( nrhs<0_ilp ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -14_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the lu factorization of a.
              call stdlib_zcopy( n, d, 1_ilp, df, 1_ilp )
              if( n>1_ilp ) then
                 call stdlib_zcopy( n-1, dl, 1_ilp, dlf, 1_ilp )
                 call stdlib_zcopy( n-1, du, 1_ilp, duf, 1_ilp )
              end if
              call stdlib_zgttrf( n, dlf, df, duf, du2, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_zlangt( norm, n, dl, d, du )
           ! compute the reciprocal of the condition number of a.
           call stdlib_zgtcon( norm, n, dlf, df, duf, du2, ipiv, anorm, rcond, work,info )
           ! compute the solution vectors x.
           call stdlib_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_zgttrs( trans, n, nrhs, dlf, df, duf, du2, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_zgtrfs( trans, n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_dlamch( 'EPSILON' ) )info = n + 1_ilp
           return
     end subroutine stdlib_zgtsvx




     pure module subroutine stdlib_I64_sgesv( n, nrhs, a, lda, ipiv, b, ldb, info )
     !! SGESV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as
     !! A = P * L * U,
     !! where P is a permutation matrix, L is unit lower triangular, and U is
     !! upper triangular.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SGESV ', -info )
              return
           end if
           ! compute the lu factorization of a.
           call stdlib_I64_sgetrf( n, n, a, lda, ipiv, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_sgetrs( 'NO TRANSPOSE', n, nrhs, a, lda, ipiv, b, ldb,info )
           end if
           return
     end subroutine stdlib_I64_sgesv

     pure module subroutine stdlib_I64_dgesv( n, nrhs, a, lda, ipiv, b, ldb, info )
     !! DGESV computes the solution to a real system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as
     !! A = P * L * U,
     !! where P is a permutation matrix, L is unit lower triangular, and U is
     !! upper triangular.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DGESV ', -info )
              return
           end if
           ! compute the lu factorization of a.
           call stdlib_I64_dgetrf( n, n, a, lda, ipiv, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_dgetrs( 'NO TRANSPOSE', n, nrhs, a, lda, ipiv, b, ldb,info )
           end if
           return
     end subroutine stdlib_I64_dgesv


     pure module subroutine stdlib_I64_cgesv( n, nrhs, a, lda, ipiv, b, ldb, info )
     !! CGESV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as
     !! A = P * L * U,
     !! where P is a permutation matrix, L is unit lower triangular, and U is
     !! upper triangular.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CGESV ', -info )
              return
           end if
           ! compute the lu factorization of a.
           call stdlib_I64_cgetrf( n, n, a, lda, ipiv, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_cgetrs( 'NO TRANSPOSE', n, nrhs, a, lda, ipiv, b, ldb,info )
           end if
           return
     end subroutine stdlib_I64_cgesv

     pure module subroutine stdlib_I64_zgesv( n, nrhs, a, lda, ipiv, b, ldb, info )
     !! ZGESV computes the solution to a complex system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as
     !! A = P * L * U,
     !! where P is a permutation matrix, L is unit lower triangular, and U is
     !! upper triangular.  The factored form of A is then used to solve the
     !! system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZGESV ', -info )
              return
           end if
           ! compute the lu factorization of a.
           call stdlib_I64_zgetrf( n, n, a, lda, ipiv, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_zgetrs( 'NO TRANSPOSE', n, nrhs, a, lda, ipiv, b, ldb,info )
           end if
           return
     end subroutine stdlib_I64_zgesv




     module subroutine stdlib_I64_sgesvx( fact, trans, n, nrhs, a, lda, af, ldaf, ipiv,equed, r, c, b, ldb, &
     !! SGESVX uses the LU factorization to compute the solution to a real
     !! system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               x, ldx, rcond, ferr, berr,work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(inout) :: ipiv(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*), c(*), r(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp64) :: i, infequ, j
           real(sp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_I64_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -10_ilp64
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -11_ilp64
                 else if( n>0_ilp64 ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp64 ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -12_ilp64
                 else if( n>0_ilp64 ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -14_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -16_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SGESVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_sgeequ( n, n, a, lda, r, c, rowcnd, colcnd, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_slaqge( n, n, a, lda, r, c, rowcnd, colcnd, amax,equed )
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of a.
              call stdlib_I64_slacpy( 'FULL', n, n, a, lda, af, ldaf )
              call stdlib_I64_sgetrf( n, n, af, ldaf, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp64 ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 rpvgrw = stdlib_I64_slantr( 'M', 'U', 'N', info, info, af, ldaf,work )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = stdlib_I64_slange( 'M', n, info, a, lda, work ) / rpvgrw
                 end if
                 work( 1_ilp64 ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_I64_slange( norm, n, n, a, lda, work )
           rpvgrw = stdlib_I64_slantr( 'M', 'U', 'N', n, n, af, ldaf, work )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_I64_slange( 'M', n, n, a, lda, work ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_sgecon( norm, n, af, ldaf, anorm, rcond, work, iwork, info )
           ! compute the solution matrix x.
           call stdlib_I64_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_sgetrs( trans, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_sgerfs( trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           work( 1_ilp64 ) = rpvgrw
           return
     end subroutine stdlib_I64_sgesvx

     module subroutine stdlib_I64_dgesvx( fact, trans, n, nrhs, a, lda, af, ldaf, ipiv,equed, r, c, b, ldb, &
     !! DGESVX uses the LU factorization to compute the solution to a real
     !! system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               x, ldx, rcond, ferr, berr,work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(inout) :: ipiv(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*), c(*), r(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp64) :: i, infequ, j
           real(dp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_I64_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -10_ilp64
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -11_ilp64
                 else if( n>0_ilp64 ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp64 ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -12_ilp64
                 else if( n>0_ilp64 ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -14_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -16_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DGESVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_dgeequ( n, n, a, lda, r, c, rowcnd, colcnd, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_dlaqge( n, n, a, lda, r, c, rowcnd, colcnd, amax,equed )
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of a.
              call stdlib_I64_dlacpy( 'FULL', n, n, a, lda, af, ldaf )
              call stdlib_I64_dgetrf( n, n, af, ldaf, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp64 ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 rpvgrw = stdlib_I64_dlantr( 'M', 'U', 'N', info, info, af, ldaf,work )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = stdlib_I64_dlange( 'M', n, info, a, lda, work ) / rpvgrw
                 end if
                 work( 1_ilp64 ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_I64_dlange( norm, n, n, a, lda, work )
           rpvgrw = stdlib_I64_dlantr( 'M', 'U', 'N', n, n, af, ldaf, work )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_I64_dlange( 'M', n, n, a, lda, work ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_dgecon( norm, n, af, ldaf, anorm, rcond, work, iwork, info )
           ! compute the solution matrix x.
           call stdlib_I64_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_dgetrs( trans, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_dgerfs( trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           work( 1_ilp64 ) = rpvgrw
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_dgesvx


     module subroutine stdlib_I64_cgesvx( fact, trans, n, nrhs, a, lda, af, ldaf, ipiv,equed, r, c, b, ldb, &
     !! CGESVX uses the LU factorization to compute the solution to a complex
     !! system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               x, ldx, rcond, ferr, berr,work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(inout) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(sp), intent(inout) :: c(*), r(*)
           complex(sp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp64) :: i, infequ, j
           real(sp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_I64_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -10_ilp64
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -11_ilp64
                 else if( n>0_ilp64 ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp64 ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -12_ilp64
                 else if( n>0_ilp64 ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -14_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -16_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CGESVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_cgeequ( n, n, a, lda, r, c, rowcnd, colcnd, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_claqge( n, n, a, lda, r, c, rowcnd, colcnd, amax,equed )
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of a.
              call stdlib_I64_clacpy( 'FULL', n, n, a, lda, af, ldaf )
              call stdlib_I64_cgetrf( n, n, af, ldaf, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp64 ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 rpvgrw = stdlib_I64_clantr( 'M', 'U', 'N', info, info, af, ldaf,rwork )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = stdlib_I64_clange( 'M', n, info, a, lda, rwork ) /rpvgrw
                 end if
                 rwork( 1_ilp64 ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_I64_clange( norm, n, n, a, lda, rwork )
           rpvgrw = stdlib_I64_clantr( 'M', 'U', 'N', n, n, af, ldaf, rwork )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_I64_clange( 'M', n, n, a, lda, rwork ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_cgecon( norm, n, af, ldaf, anorm, rcond, work, rwork, info )
           ! compute the solution matrix x.
           call stdlib_I64_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_cgetrs( trans, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_cgerfs( trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           rwork( 1_ilp64 ) = rpvgrw
           return
     end subroutine stdlib_I64_cgesvx

     module subroutine stdlib_I64_zgesvx( fact, trans, n, nrhs, a, lda, af, ldaf, ipiv,equed, r, c, b, ldb, &
     !! ZGESVX uses the LU factorization to compute the solution to a complex
     !! system of linear equations
     !! A * X = B,
     !! where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               x, ldx, rcond, ferr, berr,work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldaf, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(inout) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(dp), intent(inout) :: c(*), r(*)
           complex(dp), intent(inout) :: a(lda,*), af(ldaf,*), b(ldb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp64) :: i, infequ, j
           real(dp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_I64_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -10_ilp64
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -11_ilp64
                 else if( n>0_ilp64 ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp64 ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -12_ilp64
                 else if( n>0_ilp64 ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -14_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -16_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZGESVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_zgeequ( n, n, a, lda, r, c, rowcnd, colcnd, amax, infequ )
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_zlaqge( n, n, a, lda, r, c, rowcnd, colcnd, amax,equed )
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of a.
              call stdlib_I64_zlacpy( 'FULL', n, n, a, lda, af, ldaf )
              call stdlib_I64_zgetrf( n, n, af, ldaf, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp64 ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 rpvgrw = stdlib_I64_zlantr( 'M', 'U', 'N', info, info, af, ldaf,rwork )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = stdlib_I64_zlange( 'M', n, info, a, lda, rwork ) /rpvgrw
                 end if
                 rwork( 1_ilp64 ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_I64_zlange( norm, n, n, a, lda, rwork )
           rpvgrw = stdlib_I64_zlantr( 'M', 'U', 'N', n, n, af, ldaf, rwork )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_I64_zlange( 'M', n, n, a, lda, rwork ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_zgecon( norm, n, af, ldaf, anorm, rcond, work, rwork, info )
           ! compute the solution matrix x.
           call stdlib_I64_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_zgetrs( trans, n, nrhs, af, ldaf, ipiv, x, ldx, info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_zgerfs( trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x,ldx, ferr, berr, &
                     work, rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           rwork( 1_ilp64 ) = rpvgrw
           return
     end subroutine stdlib_I64_zgesvx




     pure module subroutine stdlib_I64_sgbsv( n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info )
     !! SGBSV computes the solution to a real system of linear equations
     !! A * X = B, where A is a band matrix of order N with KL subdiagonals
     !! and KU superdiagonals, and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as A = L * U, where L is a product of permutation
     !! and unit lower triangular matrices with KL subdiagonals, and U is
     !! upper triangular with KL+KU superdiagonals.  The factored form of A
     !! is then used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kl, ku, ldab, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           real(sp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( kl<0_ilp64 ) then
              info = -2_ilp64
           else if( ku<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( ldab<2_ilp64*kl+ku+1 ) then
              info = -6_ilp64
           else if( ldb<max( n, 1_ilp64 ) ) then
              info = -9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SGBSV ', -info )
              return
           end if
           ! compute the lu factorization of the band matrix a.
           call stdlib_I64_sgbtrf( n, n, kl, ku, ab, ldab, ipiv, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_sgbtrs( 'NO TRANSPOSE', n, kl, ku, nrhs, ab, ldab, ipiv,b, ldb, info )
                        
           end if
           return
     end subroutine stdlib_I64_sgbsv

     pure module subroutine stdlib_I64_dgbsv( n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info )
     !! DGBSV computes the solution to a real system of linear equations
     !! A * X = B, where A is a band matrix of order N with KL subdiagonals
     !! and KU superdiagonals, and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as A = L * U, where L is a product of permutation
     !! and unit lower triangular matrices with KL subdiagonals, and U is
     !! upper triangular with KL+KU superdiagonals.  The factored form of A
     !! is then used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kl, ku, ldab, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           real(dp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( kl<0_ilp64 ) then
              info = -2_ilp64
           else if( ku<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( ldab<2_ilp64*kl+ku+1 ) then
              info = -6_ilp64
           else if( ldb<max( n, 1_ilp64 ) ) then
              info = -9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DGBSV ', -info )
              return
           end if
           ! compute the lu factorization of the band matrix a.
           call stdlib_I64_dgbtrf( n, n, kl, ku, ab, ldab, ipiv, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_dgbtrs( 'NO TRANSPOSE', n, kl, ku, nrhs, ab, ldab, ipiv,b, ldb, info )
                        
           end if
           return
     end subroutine stdlib_I64_dgbsv


     pure module subroutine stdlib_I64_cgbsv( n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info )
     !! CGBSV computes the solution to a complex system of linear equations
     !! A * X = B, where A is a band matrix of order N with KL subdiagonals
     !! and KU superdiagonals, and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as A = L * U, where L is a product of permutation
     !! and unit lower triangular matrices with KL subdiagonals, and U is
     !! upper triangular with KL+KU superdiagonals.  The factored form of A
     !! is then used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kl, ku, ldab, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           complex(sp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( kl<0_ilp64 ) then
              info = -2_ilp64
           else if( ku<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( ldab<2_ilp64*kl+ku+1 ) then
              info = -6_ilp64
           else if( ldb<max( n, 1_ilp64 ) ) then
              info = -9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CGBSV ', -info )
              return
           end if
           ! compute the lu factorization of the band matrix a.
           call stdlib_I64_cgbtrf( n, n, kl, ku, ab, ldab, ipiv, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_cgbtrs( 'NO TRANSPOSE', n, kl, ku, nrhs, ab, ldab, ipiv,b, ldb, info )
                        
           end if
           return
     end subroutine stdlib_I64_cgbsv

     pure module subroutine stdlib_I64_zgbsv( n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info )
     !! ZGBSV computes the solution to a complex system of linear equations
     !! A * X = B, where A is a band matrix of order N with KL subdiagonals
     !! and KU superdiagonals, and X and B are N-by-NRHS matrices.
     !! The LU decomposition with partial pivoting and row interchanges is
     !! used to factor A as A = L * U, where L is a product of permutation
     !! and unit lower triangular matrices with KL subdiagonals, and U is
     !! upper triangular with KL+KU superdiagonals.  The factored form of A
     !! is then used to solve the system of equations A * X = B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kl, ku, ldab, ldb, n, nrhs
           ! Array Arguments 
           integer(ilp64), intent(out) :: ipiv(*)
           complex(dp), intent(inout) :: ab(ldab,*), b(ldb,*)
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( kl<0_ilp64 ) then
              info = -2_ilp64
           else if( ku<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( ldab<2_ilp64*kl+ku+1 ) then
              info = -6_ilp64
           else if( ldb<max( n, 1_ilp64 ) ) then
              info = -9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZGBSV ', -info )
              return
           end if
           ! compute the lu factorization of the band matrix a.
           call stdlib_I64_zgbtrf( n, n, kl, ku, ab, ldab, ipiv, info )
           if( info==0_ilp64 ) then
              ! solve the system a*x = b, overwriting b with x.
              call stdlib_I64_zgbtrs( 'NO TRANSPOSE', n, kl, ku, nrhs, ab, ldab, ipiv,b, ldb, info )
                        
           end if
           return
     end subroutine stdlib_I64_zgbsv




     module subroutine stdlib_I64_sgbsvx( fact, trans, n, kl, ku, nrhs, ab, ldab, afb,ldafb, ipiv, equed, r, &
     !! SGBSVX uses the LU factorization to compute the solution to a real
     !! system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
     !! where A is a band matrix of order N with KL subdiagonals and KU
     !! superdiagonals, and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               c, b, ldb, x, ldx,rcond, ferr, berr, work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(inout) :: ipiv(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*), c(*), r(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
        ! moved setting of info = n+1 so info does not subsequently get
        ! overwritten.  sven, 17 mar 05.
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp64) :: i, infequ, j, j1, j2
           real(sp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_I64_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( kl<0_ilp64 ) then
              info = -4_ilp64
           else if( ku<0_ilp64 ) then
              info = -5_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -6_ilp64
           else if( ldab<kl+ku+1 ) then
              info = -8_ilp64
           else if( ldafb<2_ilp64*kl+ku+1 ) then
              info = -10_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -12_ilp64
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -13_ilp64
                 else if( n>0_ilp64 ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp64 ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -14_ilp64
                 else if( n>0_ilp64 ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -16_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -18_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SGBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_sgbequ( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, infequ )
                        
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_slaqgb( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, equed )
                           
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of the band matrix a.
              do j = 1, n
                 j1 = max( j-ku, 1_ilp64 )
                 j2 = min( j+kl, n )
                 call stdlib_I64_scopy( j2-j1+1, ab( ku+1-j+j1, j ), 1_ilp64,afb( kl+ku+1-j+j1, j ), 1_ilp64 )
                           
              end do
              call stdlib_I64_sgbtrf( n, n, kl, ku, afb, ldafb, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp64 ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 anorm = zero
                 do j = 1, info
                    do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                       anorm = max( anorm, abs( ab( i, j ) ) )
                    end do
                 end do
                 rpvgrw = stdlib_I64_slantb( 'M', 'U', 'N', info, min( info-1, kl+ku ),afb( max( 1_ilp64, &
                           kl+ku+2-info ), 1_ilp64 ), ldafb,work )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = anorm / rpvgrw
                 end if
                 work( 1_ilp64 ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_I64_slangb( norm, n, kl, ku, ab, ldab, work )
           rpvgrw = stdlib_I64_slantb( 'M', 'U', 'N', n, kl+ku, afb, ldafb, work )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_I64_slangb( 'M', n, kl, ku, ab, ldab, work ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_sgbcon( norm, n, kl, ku, afb, ldafb, ipiv, anorm, rcond,work, iwork, info )
                     
           ! compute the solution matrix x.
           call stdlib_I64_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_sgbtrs( trans, n, kl, ku, nrhs, afb, ldafb, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_sgbrfs( trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           work( 1_ilp64 ) = rpvgrw
           return
     end subroutine stdlib_I64_sgbsvx

     module subroutine stdlib_I64_dgbsvx( fact, trans, n, kl, ku, nrhs, ab, ldab, afb,ldafb, ipiv, equed, r, &
     !! DGBSVX uses the LU factorization to compute the solution to a real
     !! system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
     !! where A is a band matrix of order N with KL subdiagonals and KU
     !! superdiagonals, and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               c, b, ldb, x, ldx,rcond, ferr, berr, work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(inout) :: ipiv(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*), c(*), r(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp64) :: i, infequ, j, j1, j2
           real(dp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_I64_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( kl<0_ilp64 ) then
              info = -4_ilp64
           else if( ku<0_ilp64 ) then
              info = -5_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -6_ilp64
           else if( ldab<kl+ku+1 ) then
              info = -8_ilp64
           else if( ldafb<2_ilp64*kl+ku+1 ) then
              info = -10_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -12_ilp64
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -13_ilp64
                 else if( n>0_ilp64 ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp64 ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -14_ilp64
                 else if( n>0_ilp64 ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -16_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -18_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DGBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_dgbequ( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, infequ )
                        
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_dlaqgb( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, equed )
                           
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of the band matrix a.
              do j = 1, n
                 j1 = max( j-ku, 1_ilp64 )
                 j2 = min( j+kl, n )
                 call stdlib_I64_dcopy( j2-j1+1, ab( ku+1-j+j1, j ), 1_ilp64,afb( kl+ku+1-j+j1, j ), 1_ilp64 )
                           
              end do
              call stdlib_I64_dgbtrf( n, n, kl, ku, afb, ldafb, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp64 ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 anorm = zero
                 do j = 1, info
                    do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                       anorm = max( anorm, abs( ab( i, j ) ) )
                    end do
                 end do
                 rpvgrw = stdlib_I64_dlantb( 'M', 'U', 'N', info, min( info-1, kl+ku ),afb( max( 1_ilp64, &
                           kl+ku+2-info ), 1_ilp64 ), ldafb,work )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = anorm / rpvgrw
                 end if
                 work( 1_ilp64 ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_I64_dlangb( norm, n, kl, ku, ab, ldab, work )
           rpvgrw = stdlib_I64_dlantb( 'M', 'U', 'N', n, kl+ku, afb, ldafb, work )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_I64_dlangb( 'M', n, kl, ku, ab, ldab, work ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_dgbcon( norm, n, kl, ku, afb, ldafb, ipiv, anorm, rcond,work, iwork, info )
                     
           ! compute the solution matrix x.
           call stdlib_I64_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_dgbtrs( trans, n, kl, ku, nrhs, afb, ldafb, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_dgbrfs( trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, iwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           work( 1_ilp64 ) = rpvgrw
           return
     end subroutine stdlib_I64_dgbsvx


     module subroutine stdlib_I64_cgbsvx( fact, trans, n, kl, ku, nrhs, ab, ldab, afb,ldafb, ipiv, equed, r, &
     !! CGBSVX uses the LU factorization to compute the solution to a complex
     !! system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
     !! where A is a band matrix of order N with KL subdiagonals and KU
     !! superdiagonals, and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               c, b, ldb, x, ldx,rcond, ferr, berr, work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(inout) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(sp), intent(inout) :: c(*), r(*)
           complex(sp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
        ! moved setting of info = n+1 so info does not subsequently get
        ! overwritten.  sven, 17 mar 05.
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp64) :: i, infequ, j, j1, j2
           real(sp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_I64_slamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( kl<0_ilp64 ) then
              info = -4_ilp64
           else if( ku<0_ilp64 ) then
              info = -5_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -6_ilp64
           else if( ldab<kl+ku+1 ) then
              info = -8_ilp64
           else if( ldafb<2_ilp64*kl+ku+1 ) then
              info = -10_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -12_ilp64
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -13_ilp64
                 else if( n>0_ilp64 ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp64 ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -14_ilp64
                 else if( n>0_ilp64 ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -16_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -18_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CGBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_cgbequ( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, infequ )
                        
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_claqgb( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, equed )
                           
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of the band matrix a.
              do j = 1, n
                 j1 = max( j-ku, 1_ilp64 )
                 j2 = min( j+kl, n )
                 call stdlib_I64_ccopy( j2-j1+1, ab( ku+1-j+j1, j ), 1_ilp64,afb( kl+ku+1-j+j1, j ), 1_ilp64 )
                           
              end do
              call stdlib_I64_cgbtrf( n, n, kl, ku, afb, ldafb, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp64 ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 anorm = zero
                 do j = 1, info
                    do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                       anorm = max( anorm, abs( ab( i, j ) ) )
                    end do
                 end do
                 rpvgrw = stdlib_I64_clantb( 'M', 'U', 'N', info, min( info-1, kl+ku ),afb( max( 1_ilp64, &
                           kl+ku+2-info ), 1_ilp64 ), ldafb,rwork )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = anorm / rpvgrw
                 end if
                 rwork( 1_ilp64 ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_I64_clangb( norm, n, kl, ku, ab, ldab, rwork )
           rpvgrw = stdlib_I64_clantb( 'M', 'U', 'N', n, kl+ku, afb, ldafb, rwork )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_I64_clangb( 'M', n, kl, ku, ab, ldab, rwork ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_cgbcon( norm, n, kl, ku, afb, ldafb, ipiv, anorm, rcond,work, rwork, info )
                     
           ! compute the solution matrix x.
           call stdlib_I64_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_cgbtrs( trans, n, kl, ku, nrhs, afb, ldafb, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_cgbrfs( trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           rwork( 1_ilp64 ) = rpvgrw
           return
     end subroutine stdlib_I64_cgbsvx

     module subroutine stdlib_I64_zgbsvx( fact, trans, n, kl, ku, nrhs, ab, ldab, afb,ldafb, ipiv, equed, r, &
     !! ZGBSVX uses the LU factorization to compute the solution to a complex
     !! system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
     !! where A is a band matrix of order N with KL subdiagonals and KU
     !! superdiagonals, and X and B are N-by-NRHS matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               c, b, ldb, x, ldx,rcond, ferr, berr, work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(inout) :: equed
           character, intent(in) :: fact, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(inout) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           real(dp), intent(inout) :: c(*), r(*)
           complex(dp), intent(inout) :: ab(ldab,*), afb(ldafb,*), b(ldb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
        ! moved setting of info = n+1 so info does not subsequently get
        ! overwritten.  sven, 17 mar 05.
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: colequ, equil, nofact, notran, rowequ
           character :: norm
           integer(ilp64) :: i, infequ, j, j1, j2
           real(dp) :: amax, anorm, bignum, colcnd, rcmax, rcmin, rowcnd, rpvgrw, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           equil = stdlib_lsame( fact, 'E' )
           notran = stdlib_lsame( trans, 'N' )
           if( nofact .or. equil ) then
              equed = 'N'
              rowequ = .false.
              colequ = .false.
           else
              rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
              colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              smlnum = stdlib_I64_dlamch( 'SAFE MINIMUM' )
              bignum = one / smlnum
           end if
           ! test the input parameters.
           if( .not.nofact .and. .not.equil .and. .not.stdlib_lsame( fact, 'F' ) )then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( kl<0_ilp64 ) then
              info = -4_ilp64
           else if( ku<0_ilp64 ) then
              info = -5_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -6_ilp64
           else if( ldab<kl+ku+1 ) then
              info = -8_ilp64
           else if( ldafb<2_ilp64*kl+ku+1 ) then
              info = -10_ilp64
           else if( stdlib_lsame( fact, 'F' ) .and. .not.( rowequ .or. colequ .or. stdlib_lsame( &
                     equed, 'N' ) ) ) then
              info = -12_ilp64
           else
              if( rowequ ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, r( j ) )
                    rcmax = max( rcmax, r( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -13_ilp64
                 else if( n>0_ilp64 ) then
                    rowcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    rowcnd = one
                 end if
              end if
              if( colequ .and. info==0_ilp64 ) then
                 rcmin = bignum
                 rcmax = zero
                 do j = 1, n
                    rcmin = min( rcmin, c( j ) )
                    rcmax = max( rcmax, c( j ) )
                 end do
                 if( rcmin<=zero ) then
                    info = -14_ilp64
                 else if( n>0_ilp64 ) then
                    colcnd = max( rcmin, smlnum ) / min( rcmax, bignum )
                 else
                    colcnd = one
                 end if
              end if
              if( info==0_ilp64 ) then
                 if( ldb<max( 1_ilp64, n ) ) then
                    info = -16_ilp64
                 else if( ldx<max( 1_ilp64, n ) ) then
                    info = -18_ilp64
                 end if
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZGBSVX', -info )
              return
           end if
           if( equil ) then
              ! compute row and column scalings to equilibrate the matrix a.
              call stdlib_I64_zgbequ( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, infequ )
                        
              if( infequ==0_ilp64 ) then
                 ! equilibrate the matrix.
                 call stdlib_I64_zlaqgb( n, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd,amax, equed )
                           
                 rowequ = stdlib_lsame( equed, 'R' ) .or. stdlib_lsame( equed, 'B' )
                 colequ = stdlib_lsame( equed, 'C' ) .or. stdlib_lsame( equed, 'B' )
              end if
           end if
           ! scale the right hand side.
           if( notran ) then
              if( rowequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       b( i, j ) = r( i )*b( i, j )
                    end do
                 end do
              end if
           else if( colequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = c( i )*b( i, j )
                 end do
              end do
           end if
           if( nofact .or. equil ) then
              ! compute the lu factorization of the band matrix a.
              do j = 1, n
                 j1 = max( j-ku, 1_ilp64 )
                 j2 = min( j+kl, n )
                 call stdlib_I64_zcopy( j2-j1+1, ab( ku+1-j+j1, j ), 1_ilp64,afb( kl+ku+1-j+j1, j ), 1_ilp64 )
                           
              end do
              call stdlib_I64_zgbtrf( n, n, kl, ku, afb, ldafb, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp64 ) then
                 ! compute the reciprocal pivot growth factor of the
                 ! leading rank-deficient info columns of a.
                 anorm = zero
                 do j = 1, info
                    do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                       anorm = max( anorm, abs( ab( i, j ) ) )
                    end do
                 end do
                 rpvgrw = stdlib_I64_zlantb( 'M', 'U', 'N', info, min( info-1, kl+ku ),afb( max( 1_ilp64, &
                           kl+ku+2-info ), 1_ilp64 ), ldafb,rwork )
                 if( rpvgrw==zero ) then
                    rpvgrw = one
                 else
                    rpvgrw = anorm / rpvgrw
                 end if
                 rwork( 1_ilp64 ) = rpvgrw
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a and the
           ! reciprocal pivot growth factor rpvgrw.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_I64_zlangb( norm, n, kl, ku, ab, ldab, rwork )
           rpvgrw = stdlib_I64_zlantb( 'M', 'U', 'N', n, kl+ku, afb, ldafb, rwork )
           if( rpvgrw==zero ) then
              rpvgrw = one
           else
              rpvgrw = stdlib_I64_zlangb( 'M', n, kl, ku, ab, ldab, rwork ) / rpvgrw
           end if
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_zgbcon( norm, n, kl, ku, afb, ldafb, ipiv, anorm, rcond,work, rwork, info )
                     
           ! compute the solution matrix x.
           call stdlib_I64_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_zgbtrs( trans, n, kl, ku, nrhs, afb, ldafb, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solution and
           ! compute error bounds and backward error estimates for it.
           call stdlib_I64_zgbrfs( trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, rwork, info )
           ! transform the solution matrix x to a solution of the original
           ! system.
           if( notran ) then
              if( colequ ) then
                 do j = 1, nrhs
                    do i = 1, n
                       x( i, j ) = c( i )*x( i, j )
                    end do
                 end do
                 do j = 1, nrhs
                    ferr( j ) = ferr( j ) / colcnd
                 end do
              end if
           else if( rowequ ) then
              do j = 1, nrhs
                 do i = 1, n
                    x( i, j ) = r( i )*x( i, j )
                 end do
              end do
              do j = 1, nrhs
                 ferr( j ) = ferr( j ) / rowcnd
              end do
           end if
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           rwork( 1_ilp64 ) = rpvgrw
           return
     end subroutine stdlib_I64_zgbsvx




     pure module subroutine stdlib_I64_sgtsv( n, nrhs, dl, d, du, b, ldb, info )
     !! SGTSV solves the equation
     !! A*X = B,
     !! where A is an n by n tridiagonal matrix, by Gaussian elimination with
     !! partial pivoting.
     !! Note that the equation  A**T*X = B  may be solved by interchanging the
     !! order of the arguments DU and DL.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(inout) :: b(ldb,*), d(*), dl(*), du(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, j
           real(sp) :: fact, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -2_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SGTSV ', -info )
              return
           end if
           if( n==0 )return
           if( nrhs==1_ilp64 ) then
              loop_10: do i = 1, n - 2
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    ! no row interchange required
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       b( i+1, 1_ilp64 ) = b( i+1, 1_ilp64 ) - fact*b( i, 1_ilp64 )
                    else
                       info = i
                       return
                    end if
                    dl( i ) = zero
                 else
                    ! interchange rows i and i+1
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    dl( i ) = du( i+1 )
                    du( i+1 ) = -fact*dl( i )
                    du( i ) = temp
                    temp = b( i, 1_ilp64 )
                    b( i, 1_ilp64 ) = b( i+1, 1_ilp64 )
                    b( i+1, 1_ilp64 ) = temp - fact*b( i+1, 1_ilp64 )
                 end if
              end do loop_10
              if( n>1_ilp64 ) then
                 i = n - 1_ilp64
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       b( i+1, 1_ilp64 ) = b( i+1, 1_ilp64 ) - fact*b( i, 1_ilp64 )
                    else
                       info = i
                       return
                    end if
                 else
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    du( i ) = temp
                    temp = b( i, 1_ilp64 )
                    b( i, 1_ilp64 ) = b( i+1, 1_ilp64 )
                    b( i+1, 1_ilp64 ) = temp - fact*b( i+1, 1_ilp64 )
                 end if
              end if
              if( d( n )==zero ) then
                 info = n
                 return
              end if
           else
              loop_40: do i = 1, n - 2
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    ! no row interchange required
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       do j = 1, nrhs
                          b( i+1, j ) = b( i+1, j ) - fact*b( i, j )
                       end do
                    else
                       info = i
                       return
                    end if
                    dl( i ) = zero
                 else
                    ! interchange rows i and i+1
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    dl( i ) = du( i+1 )
                    du( i+1 ) = -fact*dl( i )
                    du( i ) = temp
                    do j = 1, nrhs
                       temp = b( i, j )
                       b( i, j ) = b( i+1, j )
                       b( i+1, j ) = temp - fact*b( i+1, j )
                    end do
                 end if
              end do loop_40
              if( n>1_ilp64 ) then
                 i = n - 1_ilp64
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       do j = 1, nrhs
                          b( i+1, j ) = b( i+1, j ) - fact*b( i, j )
                       end do
                    else
                       info = i
                       return
                    end if
                 else
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    du( i ) = temp
                    do j = 1, nrhs
                       temp = b( i, j )
                       b( i, j ) = b( i+1, j )
                       b( i+1, j ) = temp - fact*b( i+1, j )
                    end do
                 end if
              end if
              if( d( n )==zero ) then
                 info = n
                 return
              end if
           end if
           ! back solve with the matrix u from the factorization.
           if( nrhs<=2_ilp64 ) then
              j = 1_ilp64
              70 continue
              b( n, j ) = b( n, j ) / d( n )
              if( n>1_ilp64 )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) / d( n-1 )
              do i = n - 2, 1, -1
                 b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-dl( i )*b( i+2, j ) ) / d( i )
                           
              end do
              if( j<nrhs ) then
                 j = j + 1_ilp64
                 go to 70
              end if
           else
              do j = 1, nrhs
                 b( n, j ) = b( n, j ) / d( n )
                 if( n>1_ilp64 )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) /d( n-1 )
                 do i = n - 2, 1, -1
                    b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-dl( i )*b( i+2, j ) ) / d( i )
                              
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_sgtsv

     pure module subroutine stdlib_I64_dgtsv( n, nrhs, dl, d, du, b, ldb, info )
     !! DGTSV solves the equation
     !! A*X = B,
     !! where A is an n by n tridiagonal matrix, by Gaussian elimination with
     !! partial pivoting.
     !! Note that the equation  A**T*X = B  may be solved by interchanging the
     !! order of the arguments DU and DL.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(inout) :: b(ldb,*), d(*), dl(*), du(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, j
           real(dp) :: fact, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -2_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DGTSV ', -info )
              return
           end if
           if( n==0 )return
           if( nrhs==1_ilp64 ) then
              loop_10: do i = 1, n - 2
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    ! no row interchange required
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       b( i+1, 1_ilp64 ) = b( i+1, 1_ilp64 ) - fact*b( i, 1_ilp64 )
                    else
                       info = i
                       return
                    end if
                    dl( i ) = zero
                 else
                    ! interchange rows i and i+1
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    dl( i ) = du( i+1 )
                    du( i+1 ) = -fact*dl( i )
                    du( i ) = temp
                    temp = b( i, 1_ilp64 )
                    b( i, 1_ilp64 ) = b( i+1, 1_ilp64 )
                    b( i+1, 1_ilp64 ) = temp - fact*b( i+1, 1_ilp64 )
                 end if
              end do loop_10
              if( n>1_ilp64 ) then
                 i = n - 1_ilp64
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       b( i+1, 1_ilp64 ) = b( i+1, 1_ilp64 ) - fact*b( i, 1_ilp64 )
                    else
                       info = i
                       return
                    end if
                 else
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    du( i ) = temp
                    temp = b( i, 1_ilp64 )
                    b( i, 1_ilp64 ) = b( i+1, 1_ilp64 )
                    b( i+1, 1_ilp64 ) = temp - fact*b( i+1, 1_ilp64 )
                 end if
              end if
              if( d( n )==zero ) then
                 info = n
                 return
              end if
           else
              loop_40: do i = 1, n - 2
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    ! no row interchange required
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       do j = 1, nrhs
                          b( i+1, j ) = b( i+1, j ) - fact*b( i, j )
                       end do
                    else
                       info = i
                       return
                    end if
                    dl( i ) = zero
                 else
                    ! interchange rows i and i+1
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    dl( i ) = du( i+1 )
                    du( i+1 ) = -fact*dl( i )
                    du( i ) = temp
                    do j = 1, nrhs
                       temp = b( i, j )
                       b( i, j ) = b( i+1, j )
                       b( i+1, j ) = temp - fact*b( i+1, j )
                    end do
                 end if
              end do loop_40
              if( n>1_ilp64 ) then
                 i = n - 1_ilp64
                 if( abs( d( i ) )>=abs( dl( i ) ) ) then
                    if( d( i )/=zero ) then
                       fact = dl( i ) / d( i )
                       d( i+1 ) = d( i+1 ) - fact*du( i )
                       do j = 1, nrhs
                          b( i+1, j ) = b( i+1, j ) - fact*b( i, j )
                       end do
                    else
                       info = i
                       return
                    end if
                 else
                    fact = d( i ) / dl( i )
                    d( i ) = dl( i )
                    temp = d( i+1 )
                    d( i+1 ) = du( i ) - fact*temp
                    du( i ) = temp
                    do j = 1, nrhs
                       temp = b( i, j )
                       b( i, j ) = b( i+1, j )
                       b( i+1, j ) = temp - fact*b( i+1, j )
                    end do
                 end if
              end if
              if( d( n )==zero ) then
                 info = n
                 return
              end if
           end if
           ! back solve with the matrix u from the factorization.
           if( nrhs<=2_ilp64 ) then
              j = 1_ilp64
              70 continue
              b( n, j ) = b( n, j ) / d( n )
              if( n>1_ilp64 )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) / d( n-1 )
              do i = n - 2, 1, -1
                 b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-dl( i )*b( i+2, j ) ) / d( i )
                           
              end do
              if( j<nrhs ) then
                 j = j + 1_ilp64
                 go to 70
              end if
           else
              do j = 1, nrhs
                 b( n, j ) = b( n, j ) / d( n )
                 if( n>1_ilp64 )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) /d( n-1 )
                 do i = n - 2, 1, -1
                    b( i, j ) = ( b( i, j )-du( i )*b( i+1, j )-dl( i )*b( i+2, j ) ) / d( i )
                              
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_dgtsv


     pure module subroutine stdlib_I64_cgtsv( n, nrhs, dl, d, du, b, ldb, info )
     !! CGTSV solves the equation
     !! A*X = B,
     !! where A is an N-by-N tridiagonal matrix, by Gaussian elimination with
     !! partial pivoting.
     !! Note that the equation  A**T *X = B  may be solved by interchanging the
     !! order of the arguments DU and DL.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(inout) :: b(ldb,*), d(*), dl(*), du(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j, k
           complex(sp) :: mult, temp, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -2_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CGTSV ', -info )
              return
           end if
           if( n==0 )return
           loop_30: do k = 1, n - 1
              if( dl( k )==czero ) then
                 ! subdiagonal is czero, no elimination is required.
                 if( d( k )==czero ) then
                    ! diagonal is czero: set info = k and return; a unique
                    ! solution can not be found.
                    info = k
                    return
                 end if
              else if( cabs1( d( k ) )>=cabs1( dl( k ) ) ) then
                 ! no row interchange required
                 mult = dl( k ) / d( k )
                 d( k+1 ) = d( k+1 ) - mult*du( k )
                 do j = 1, nrhs
                    b( k+1, j ) = b( k+1, j ) - mult*b( k, j )
                 end do
                 if( k<( n-1 ) )dl( k ) = czero
              else
                 ! interchange rows k and k+1
                 mult = d( k ) / dl( k )
                 d( k ) = dl( k )
                 temp = d( k+1 )
                 d( k+1 ) = du( k ) - mult*temp
                 if( k<( n-1 ) ) then
                    dl( k ) = du( k+1 )
                    du( k+1 ) = -mult*dl( k )
                 end if
                 du( k ) = temp
                 do j = 1, nrhs
                    temp = b( k, j )
                    b( k, j ) = b( k+1, j )
                    b( k+1, j ) = temp - mult*b( k+1, j )
                 end do
              end if
           end do loop_30
           if( d( n )==czero ) then
              info = n
              return
           end if
           ! back solve with the matrix u from the factorization.
           do j = 1, nrhs
              b( n, j ) = b( n, j ) / d( n )
              if( n>1_ilp64 )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) / d( n-1 )
              do k = n - 2, 1, -1
                 b( k, j ) = ( b( k, j )-du( k )*b( k+1, j )-dl( k )*b( k+2, j ) ) / d( k )
                           
              end do
           end do
           return
     end subroutine stdlib_I64_cgtsv

     pure module subroutine stdlib_I64_zgtsv( n, nrhs, dl, d, du, b, ldb, info )
     !! ZGTSV solves the equation
     !! A*X = B,
     !! where A is an N-by-N tridiagonal matrix, by Gaussian elimination with
     !! partial pivoting.
     !! Note that the equation  A**T *X = B  may be solved by interchanging the
     !! order of the arguments DU and DL.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(inout) :: b(ldb,*), d(*), dl(*), du(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j, k
           complex(dp) :: mult, temp, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -1_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -2_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZGTSV ', -info )
              return
           end if
           if( n==0 )return
           loop_30: do k = 1, n - 1
              if( dl( k )==czero ) then
                 ! subdiagonal is czero, no elimination is required.
                 if( d( k )==czero ) then
                    ! diagonal is czero: set info = k and return; a unique
                    ! solution can not be found.
                    info = k
                    return
                 end if
              else if( cabs1( d( k ) )>=cabs1( dl( k ) ) ) then
                 ! no row interchange required
                 mult = dl( k ) / d( k )
                 d( k+1 ) = d( k+1 ) - mult*du( k )
                 do j = 1, nrhs
                    b( k+1, j ) = b( k+1, j ) - mult*b( k, j )
                 end do
                 if( k<( n-1 ) )dl( k ) = czero
              else
                 ! interchange rows k and k+1
                 mult = d( k ) / dl( k )
                 d( k ) = dl( k )
                 temp = d( k+1 )
                 d( k+1 ) = du( k ) - mult*temp
                 if( k<( n-1 ) ) then
                    dl( k ) = du( k+1 )
                    du( k+1 ) = -mult*dl( k )
                 end if
                 du( k ) = temp
                 do j = 1, nrhs
                    temp = b( k, j )
                    b( k, j ) = b( k+1, j )
                    b( k+1, j ) = temp - mult*b( k+1, j )
                 end do
              end if
           end do loop_30
           if( d( n )==czero ) then
              info = n
              return
           end if
           ! back solve with the matrix u from the factorization.
           do j = 1, nrhs
              b( n, j ) = b( n, j ) / d( n )
              if( n>1_ilp64 )b( n-1, j ) = ( b( n-1, j )-du( n-1 )*b( n, j ) ) / d( n-1 )
              do k = n - 2, 1, -1
                 b( k, j ) = ( b( k, j )-du( k )*b( k+1, j )-dl( k )*b( k+2, j ) ) / d( k )
                           
              end do
           end do
           return
     end subroutine stdlib_I64_zgtsv




     pure module subroutine stdlib_I64_sgtsvx( fact, trans, n, nrhs, dl, d, du, dlf, df, duf,du2, ipiv, b, &
     !! SGTSVX uses the LU factorization to compute the solution to a real
     !! system of linear equations A * X = B or A**T * X = B,
     !! where A is a tridiagonal matrix of order N and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldb, x, ldx, rcond, ferr, berr,work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(inout) :: ipiv(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(in) :: b(ldb,*), d(*), dl(*), du(*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
           real(sp), intent(inout) :: df(*), dlf(*), du2(*), duf(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact, notran
           character :: norm
           real(sp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           notran = stdlib_lsame( trans, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -14_ilp64
           else if( ldx<max( 1_ilp64, n ) ) then
              info = -16_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SGTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the lu factorization of a.
              call stdlib_I64_scopy( n, d, 1_ilp64, df, 1_ilp64 )
              if( n>1_ilp64 ) then
                 call stdlib_I64_scopy( n-1, dl, 1_ilp64, dlf, 1_ilp64 )
                 call stdlib_I64_scopy( n-1, du, 1_ilp64, duf, 1_ilp64 )
              end if
              call stdlib_I64_sgttrf( n, dlf, df, duf, du2, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_I64_slangt( norm, n, dl, d, du )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_sgtcon( norm, n, dlf, df, duf, du2, ipiv, anorm, rcond, work,iwork, info )
                     
           ! compute the solution vectors x.
           call stdlib_I64_slacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_sgttrs( trans, n, nrhs, dlf, df, duf, du2, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_I64_sgtrfs( trans, n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, iwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_sgtsvx

     pure module subroutine stdlib_I64_dgtsvx( fact, trans, n, nrhs, dl, d, du, dlf, df, duf,du2, ipiv, b, &
     !! DGTSVX uses the LU factorization to compute the solution to a real
     !! system of linear equations A * X = B or A**T * X = B,
     !! where A is a tridiagonal matrix of order N and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldb, x, ldx, rcond, ferr, berr,work, iwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(inout) :: ipiv(*)
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(in) :: b(ldb,*), d(*), dl(*), du(*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*), x(ldx,*)
           real(dp), intent(inout) :: df(*), dlf(*), du2(*), duf(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact, notran
           character :: norm
           real(dp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           notran = stdlib_lsame( trans, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -14_ilp64
           else if( ldx<max( 1_ilp64, n ) ) then
              info = -16_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DGTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the lu factorization of a.
              call stdlib_I64_dcopy( n, d, 1_ilp64, df, 1_ilp64 )
              if( n>1_ilp64 ) then
                 call stdlib_I64_dcopy( n-1, dl, 1_ilp64, dlf, 1_ilp64 )
                 call stdlib_I64_dcopy( n-1, du, 1_ilp64, duf, 1_ilp64 )
              end if
              call stdlib_I64_dgttrf( n, dlf, df, duf, du2, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_I64_dlangt( norm, n, dl, d, du )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_dgtcon( norm, n, dlf, df, duf, du2, ipiv, anorm, rcond, work,iwork, info )
                     
           ! compute the solution vectors x.
           call stdlib_I64_dlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_dgttrs( trans, n, nrhs, dlf, df, duf, du2, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_I64_dgtrfs( trans, n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, iwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_dgtsvx


     pure module subroutine stdlib_I64_cgtsvx( fact, trans, n, nrhs, dl, d, du, dlf, df, duf,du2, ipiv, b, &
     !! CGTSVX uses the LU factorization to compute the solution to a complex
     !! system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
     !! where A is a tridiagonal matrix of order N and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldb, x, ldx, rcond, ferr, berr,work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(inout) :: ipiv(*)
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: b(ldb,*), d(*), dl(*), du(*)
           complex(sp), intent(inout) :: df(*), dlf(*), du2(*), duf(*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact, notran
           character :: norm
           real(sp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           notran = stdlib_lsame( trans, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -14_ilp64
           else if( ldx<max( 1_ilp64, n ) ) then
              info = -16_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CGTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the lu factorization of a.
              call stdlib_I64_ccopy( n, d, 1_ilp64, df, 1_ilp64 )
              if( n>1_ilp64 ) then
                 call stdlib_I64_ccopy( n-1, dl, 1_ilp64, dlf, 1_ilp64 )
                 call stdlib_I64_ccopy( n-1, du, 1_ilp64, duf, 1_ilp64 )
              end if
              call stdlib_I64_cgttrf( n, dlf, df, duf, du2, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_I64_clangt( norm, n, dl, d, du )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_cgtcon( norm, n, dlf, df, duf, du2, ipiv, anorm, rcond, work,info )
           ! compute the solution vectors x.
           call stdlib_I64_clacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_cgttrs( trans, n, nrhs, dlf, df, duf, du2, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_I64_cgtrfs( trans, n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_slamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_cgtsvx

     pure module subroutine stdlib_I64_zgtsvx( fact, trans, n, nrhs, dl, d, du, dlf, df, duf,du2, ipiv, b, &
     !! ZGTSVX uses the LU factorization to compute the solution to a complex
     !! system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
     !! where A is a tridiagonal matrix of order N and X and B are N-by-NRHS
     !! matrices.
     !! Error bounds on the solution and a condition estimate are also
     !! provided.
               ldb, x, ldx, rcond, ferr, berr,work, rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: fact, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp64), intent(inout) :: ipiv(*)
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: b(ldb,*), d(*), dl(*), du(*)
           complex(dp), intent(inout) :: df(*), dlf(*), du2(*), duf(*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nofact, notran
           character :: norm
           real(dp) :: anorm
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp64
           nofact = stdlib_lsame( fact, 'N' )
           notran = stdlib_lsame( trans, 'N' )
           if( .not.nofact .and. .not.stdlib_lsame( fact, 'F' ) ) then
              info = -1_ilp64
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( nrhs<0_ilp64 ) then
              info = -4_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -14_ilp64
           else if( ldx<max( 1_ilp64, n ) ) then
              info = -16_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZGTSVX', -info )
              return
           end if
           if( nofact ) then
              ! compute the lu factorization of a.
              call stdlib_I64_zcopy( n, d, 1_ilp64, df, 1_ilp64 )
              if( n>1_ilp64 ) then
                 call stdlib_I64_zcopy( n-1, dl, 1_ilp64, dlf, 1_ilp64 )
                 call stdlib_I64_zcopy( n-1, du, 1_ilp64, duf, 1_ilp64 )
              end if
              call stdlib_I64_zgttrf( n, dlf, df, duf, du2, ipiv, info )
              ! return if info is non-zero.
              if( info>0_ilp64 )then
                 rcond = zero
                 return
              end if
           end if
           ! compute the norm of the matrix a.
           if( notran ) then
              norm = '1'
           else
              norm = 'I'
           end if
           anorm = stdlib_I64_zlangt( norm, n, dl, d, du )
           ! compute the reciprocal of the condition number of a.
           call stdlib_I64_zgtcon( norm, n, dlf, df, duf, du2, ipiv, anorm, rcond, work,info )
           ! compute the solution vectors x.
           call stdlib_I64_zlacpy( 'FULL', n, nrhs, b, ldb, x, ldx )
           call stdlib_I64_zgttrs( trans, n, nrhs, dlf, df, duf, du2, ipiv, x, ldx,info )
           ! use iterative refinement to improve the computed solutions and
           ! compute error bounds and backward error estimates for them.
           call stdlib_I64_zgtrfs( trans, n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv,b, ldb, x, ldx, &
                     ferr, berr, work, rwork, info )
           ! set info = n+1 if the matrix is singular to working precision.
           if( rcond<stdlib_I64_dlamch( 'EPSILON' ) )info = n + 1_ilp64
           return
     end subroutine stdlib_I64_zgtsvx



end submodule stdlib_lapack_solve_lu
