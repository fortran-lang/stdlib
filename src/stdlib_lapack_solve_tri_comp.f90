submodule(stdlib_lapack_solve) stdlib_lapack_solve_tri_comp
  implicit none


  contains

     module subroutine stdlib_strcon( norm, uplo, diag, n, a, lda, rcond, work,iwork, info )
     !! STRCON estimates the reciprocal of the condition number of a
     !! triangular matrix A, in either the 1-norm or the infinity-norm.
     !! The norm of A is computed and an estimate is obtained for
     !! norm(inv(A)), then the reciprocal of the condition number is
     !! computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, onenrm, upper
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(sp) :: ainvnm, anorm, scale, smlnum, xnorm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STRCON', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              rcond = one
              return
           end if
           rcond = zero
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )*real( max( 1_ilp, n ),KIND=sp)
           ! compute the norm of the triangular matrix a.
           anorm = stdlib_slantr( norm, uplo, diag, n, n, a, lda, work )
           ! continue only if anorm > 0.
           if( anorm>zero ) then
              ! estimate the norm of the inverse of a.
              ainvnm = zero
              normin = 'N'
              if( onenrm ) then
                 kase1 = 1_ilp
              else
                 kase1 = 2_ilp
              end if
              kase = 0_ilp
              10 continue
              call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==kase1 ) then
                    ! multiply by inv(a).
                    call stdlib_slatrs( uplo, 'NO TRANSPOSE', diag, normin, n, a,lda, work, scale,&
                               work( 2_ilp*n+1 ), info )
                 else
                    ! multiply by inv(a**t).
                    call stdlib_slatrs( uplo, 'TRANSPOSE', diag, normin, n, a, lda,work, scale, &
                              work( 2_ilp*n+1 ), info )
                 end if
                 normin = 'Y'
                 ! multiply by 1/scale if doing so will not cause overflow.
                 if( scale/=one ) then
                    ix = stdlib_isamax( n, work, 1_ilp )
                    xnorm = abs( work( ix ) )
                    if( scale<xnorm*smlnum .or. scale==zero )go to 20
                    call stdlib_srscl( n, scale, work, 1_ilp )
                 end if
                 go to 10
              end if
              ! compute the estimate of the reciprocal condition number.
              if( ainvnm/=zero )rcond = ( one / anorm ) / ainvnm
           end if
           20 continue
           return
     end subroutine stdlib_strcon

     module subroutine stdlib_dtrcon( norm, uplo, diag, n, a, lda, rcond, work,iwork, info )
     !! DTRCON estimates the reciprocal of the condition number of a
     !! triangular matrix A, in either the 1-norm or the infinity-norm.
     !! The norm of A is computed and an estimate is obtained for
     !! norm(inv(A)), then the reciprocal of the condition number is
     !! computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, onenrm, upper
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(dp) :: ainvnm, anorm, scale, smlnum, xnorm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTRCON', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              rcond = one
              return
           end if
           rcond = zero
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )*real( max( 1_ilp, n ),KIND=dp)
           ! compute the norm of the triangular matrix a.
           anorm = stdlib_dlantr( norm, uplo, diag, n, n, a, lda, work )
           ! continue only if anorm > 0.
           if( anorm>zero ) then
              ! estimate the norm of the inverse of a.
              ainvnm = zero
              normin = 'N'
              if( onenrm ) then
                 kase1 = 1_ilp
              else
                 kase1 = 2_ilp
              end if
              kase = 0_ilp
              10 continue
              call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==kase1 ) then
                    ! multiply by inv(a).
                    call stdlib_dlatrs( uplo, 'NO TRANSPOSE', diag, normin, n, a,lda, work, scale,&
                               work( 2_ilp*n+1 ), info )
                 else
                    ! multiply by inv(a**t).
                    call stdlib_dlatrs( uplo, 'TRANSPOSE', diag, normin, n, a, lda,work, scale, &
                              work( 2_ilp*n+1 ), info )
                 end if
                 normin = 'Y'
                 ! multiply by 1/scale if doing so will not cause overflow.
                 if( scale/=one ) then
                    ix = stdlib_idamax( n, work, 1_ilp )
                    xnorm = abs( work( ix ) )
                    if( scale<xnorm*smlnum .or. scale==zero )go to 20
                    call stdlib_drscl( n, scale, work, 1_ilp )
                 end if
                 go to 10
              end if
              ! compute the estimate of the reciprocal condition number.
              if( ainvnm/=zero )rcond = ( one / anorm ) / ainvnm
           end if
           20 continue
           return
     end subroutine stdlib_dtrcon


     module subroutine stdlib_ctrcon( norm, uplo, diag, n, a, lda, rcond, work,rwork, info )
     !! CTRCON estimates the reciprocal of the condition number of a
     !! triangular matrix A, in either the 1-norm or the infinity-norm.
     !! The norm of A is computed and an estimate is obtained for
     !! norm(inv(A)), then the reciprocal of the condition number is
     !! computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, onenrm, upper
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(sp) :: ainvnm, anorm, scale, smlnum, xnorm
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
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTRCON', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              rcond = one
              return
           end if
           rcond = zero
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )*real( max( 1_ilp, n ),KIND=sp)
           ! compute the norm of the triangular matrix a.
           anorm = stdlib_clantr( norm, uplo, diag, n, n, a, lda, rwork )
           ! continue only if anorm > 0.
           if( anorm>zero ) then
              ! estimate the norm of the inverse of a.
              ainvnm = zero
              normin = 'N'
              if( onenrm ) then
                 kase1 = 1_ilp
              else
                 kase1 = 2_ilp
              end if
              kase = 0_ilp
              10 continue
              call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==kase1 ) then
                    ! multiply by inv(a).
                    call stdlib_clatrs( uplo, 'NO TRANSPOSE', diag, normin, n, a,lda, work, scale,&
                               rwork, info )
                 else
                    ! multiply by inv(a**h).
                    call stdlib_clatrs( uplo, 'CONJUGATE TRANSPOSE', diag, normin,n, a, lda, work,&
                               scale, rwork, info )
                 end if
                 normin = 'Y'
                 ! multiply by 1/scale if doing so will not cause overflow.
                 if( scale/=one ) then
                    ix = stdlib_icamax( n, work, 1_ilp )
                    xnorm = cabs1( work( ix ) )
                    if( scale<xnorm*smlnum .or. scale==zero )go to 20
                    call stdlib_csrscl( n, scale, work, 1_ilp )
                 end if
                 go to 10
              end if
              ! compute the estimate of the reciprocal condition number.
              if( ainvnm/=zero )rcond = ( one / anorm ) / ainvnm
           end if
           20 continue
           return
     end subroutine stdlib_ctrcon

     module subroutine stdlib_ztrcon( norm, uplo, diag, n, a, lda, rcond, work,rwork, info )
     !! ZTRCON estimates the reciprocal of the condition number of a
     !! triangular matrix A, in either the 1-norm or the infinity-norm.
     !! The norm of A is computed and an estimate is obtained for
     !! norm(inv(A)), then the reciprocal of the condition number is
     !! computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, onenrm, upper
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(dp) :: ainvnm, anorm, scale, smlnum, xnorm
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
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTRCON', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              rcond = one
              return
           end if
           rcond = zero
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )*real( max( 1_ilp, n ),KIND=dp)
           ! compute the norm of the triangular matrix a.
           anorm = stdlib_zlantr( norm, uplo, diag, n, n, a, lda, rwork )
           ! continue only if anorm > 0.
           if( anorm>zero ) then
              ! estimate the norm of the inverse of a.
              ainvnm = zero
              normin = 'N'
              if( onenrm ) then
                 kase1 = 1_ilp
              else
                 kase1 = 2_ilp
              end if
              kase = 0_ilp
              10 continue
              call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==kase1 ) then
                    ! multiply by inv(a).
                    call stdlib_zlatrs( uplo, 'NO TRANSPOSE', diag, normin, n, a,lda, work, scale,&
                               rwork, info )
                 else
                    ! multiply by inv(a**h).
                    call stdlib_zlatrs( uplo, 'CONJUGATE TRANSPOSE', diag, normin,n, a, lda, work,&
                               scale, rwork, info )
                 end if
                 normin = 'Y'
                 ! multiply by 1/scale if doing so will not cause overflow.
                 if( scale/=one ) then
                    ix = stdlib_izamax( n, work, 1_ilp )
                    xnorm = cabs1( work( ix ) )
                    if( scale<xnorm*smlnum .or. scale==zero )go to 20
                    call stdlib_zdrscl( n, scale, work, 1_ilp )
                 end if
                 go to 10
              end if
              ! compute the estimate of the reciprocal condition number.
              if( ainvnm/=zero )rcond = ( one / anorm ) / ainvnm
           end if
           20 continue
           return
     end subroutine stdlib_ztrcon




     pure module subroutine stdlib_strtrs( uplo, trans, diag, n, nrhs, a, lda, b, ldb,info )
     !! STRTRS solves a triangular system of the form
     !! A * X = B  or  A**T * X = B,
     !! where A is a triangular matrix of order N, and B is an N-by-NRHS
     !! matrix.  A check is made to verify that A is nonsingular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) .and. &
                     .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STRTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity.
           if( nounit ) then
              do info = 1, n
                 if( a( info, info )==zero )return
              end do
           end if
           info = 0_ilp
           ! solve a * x = b  or  a**t * x = b.
           call stdlib_strsm( 'LEFT', uplo, trans, diag, n, nrhs, one, a, lda, b,ldb )
           return
     end subroutine stdlib_strtrs

     pure module subroutine stdlib_dtrtrs( uplo, trans, diag, n, nrhs, a, lda, b, ldb,info )
     !! DTRTRS solves a triangular system of the form
     !! A * X = B  or  A**T * X = B,
     !! where A is a triangular matrix of order N, and B is an N-by-NRHS
     !! matrix.  A check is made to verify that A is nonsingular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) .and. &
                     .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTRTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity.
           if( nounit ) then
              do info = 1, n
                 if( a( info, info )==zero )return
              end do
           end if
           info = 0_ilp
           ! solve a * x = b  or  a**t * x = b.
           call stdlib_dtrsm( 'LEFT', uplo, trans, diag, n, nrhs, one, a, lda, b,ldb )
           return
     end subroutine stdlib_dtrtrs


     pure module subroutine stdlib_ctrtrs( uplo, trans, diag, n, nrhs, a, lda, b, ldb,info )
     !! CTRTRS solves a triangular system of the form
     !! A * X = B,  A**T * X = B,  or  A**H * X = B,
     !! where A is a triangular matrix of order N, and B is an N-by-NRHS
     !! matrix.  A check is made to verify that A is nonsingular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) .and. &
                     .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTRTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity.
           if( nounit ) then
              do info = 1, n
                 if( a( info, info )==czero )return
              end do
           end if
           info = 0_ilp
           ! solve a * x = b,  a**t * x = b,  or  a**h * x = b.
           call stdlib_ctrsm( 'LEFT', uplo, trans, diag, n, nrhs, cone, a, lda, b,ldb )
           return
     end subroutine stdlib_ctrtrs

     pure module subroutine stdlib_ztrtrs( uplo, trans, diag, n, nrhs, a, lda, b, ldb,info )
     !! ZTRTRS solves a triangular system of the form
     !! A * X = B,  A**T * X = B,  or  A**H * X = B,
     !! where A is a triangular matrix of order N, and B is an N-by-NRHS
     !! matrix.  A check is made to verify that A is nonsingular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) .and. &
                     .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTRTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity.
           if( nounit ) then
              do info = 1, n
                 if( a( info, info )==czero )return
              end do
           end if
           info = 0_ilp
           ! solve a * x = b,  a**t * x = b,  or  a**h * x = b.
           call stdlib_ztrsm( 'LEFT', uplo, trans, diag, n, nrhs, cone, a, lda, b,ldb )
           return
     end subroutine stdlib_ztrtrs




     pure module subroutine stdlib_slatrs( uplo, trans, diag, normin, n, a, lda, x, scale,cnorm, info )
     !! SLATRS solves one of the triangular systems
     !! A *x = s*b  or  A**T*x = s*b
     !! with scaling to prevent overflow.  Here A is an upper or lower
     !! triangular matrix, A**T denotes the transpose of A, x and b are
     !! n-element vectors, and s is a scaling factor, usually less than
     !! or equal to 1, chosen so that the components of x will be less than
     !! the overflow threshold.  If the unscaled problem will not cause
     !! overflow, the Level 2 BLAS routine STRSV is called.  If the matrix A
     !! is singular (A(j,j) = 0 for some j), then s is set to 0 and a
     !! non-trivial solution to A*x = 0 is returned.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, normin, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: scale
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: cnorm(*), x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           integer(ilp) :: i, imax, j, jfirst, jinc, jlast
           real(sp) :: bignum, grow, rec, smlnum, sumj, tjj, tjjs, tmax, tscal, uscal, xbnd, xj, &
                     xmax
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           ! test the input parameters.
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( normin, 'Y' ) .and. .not.stdlib_lsame( normin, 'N' ) ) &
                     then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLATRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine machine dependent parameters to control overflow.
           smlnum = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           bignum = one / smlnum
           scale = one
           if( stdlib_lsame( normin, 'N' ) ) then
              ! compute the 1-norm of each column, not including the diagonal.
              if( upper ) then
                 ! a is upper triangular.
                 do j = 1, n
                    cnorm( j ) = stdlib_sasum( j-1, a( 1_ilp, j ), 1_ilp )
                 end do
              else
                 ! a is lower triangular.
                 do j = 1, n - 1
                    cnorm( j ) = stdlib_sasum( n-j, a( j+1, j ), 1_ilp )
                 end do
                 cnorm( n ) = zero
              end if
           end if
           ! scale the column norms by tscal if the maximum element in cnorm is
           ! greater than bignum.
           imax = stdlib_isamax( n, cnorm, 1_ilp )
           tmax = cnorm( imax )
           if( tmax<=bignum ) then
              tscal = one
           else
              tscal = one / ( smlnum*tmax )
              call stdlib_sscal( n, tscal, cnorm, 1_ilp )
           end if
           ! compute a bound on the computed solution vector to see if the
           ! level 2 blas routine stdlib_strsv can be used.
           j = stdlib_isamax( n, x, 1_ilp )
           xmax = abs( x( j ) )
           xbnd = xmax
           if( notran ) then
              ! compute the growth in a * x = b.
              if( upper ) then
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              else
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 50
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, g(0) = max{x(i), i=1,...,n}.
                 grow = one / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 50
                    ! m(j) = g(j-1) / abs(a(j,j))
                    tjj = abs( a( j, j ) )
                    xbnd = min( xbnd, min( one, tjj )*grow )
                    if( tjj+cnorm( j )>=smlnum ) then
                       ! g(j) = g(j-1)*( 1 + cnorm(j) / abs(a(j,j)) )
                       grow = grow*( tjj / ( tjj+cnorm( j ) ) )
                    else
                       ! g(j) could overflow, set grow to 0.
                       grow = zero
                    end if
                 end do
                 grow = xbnd
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, one / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 50
                    ! g(j) = g(j-1)*( 1 + cnorm(j) )
                    grow = grow*( one / ( one+cnorm( j ) ) )
                 end do
              end if
              50 continue
           else
              ! compute the growth in a**t * x = b.
              if( upper ) then
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              else
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 80
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, m(0) = max{x(i), i=1,...,n}.
                 grow = one / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 80
                    ! g(j) = max( g(j-1), m(j-1)*( 1 + cnorm(j) ) )
                    xj = one + cnorm( j )
                    grow = min( grow, xbnd / xj )
                    ! m(j) = m(j-1)*( 1 + cnorm(j) ) / abs(a(j,j))
                    tjj = abs( a( j, j ) )
                    if( xj>tjj )xbnd = xbnd*( tjj / xj )
                 end do
                 grow = min( grow, xbnd )
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, one / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 80
                    ! g(j) = ( 1 + cnorm(j) )*g(j-1)
                    xj = one + cnorm( j )
                    grow = grow / xj
                 end do
              end if
              80 continue
           end if
           if( ( grow*tscal )>smlnum ) then
              ! use the level 2 blas solve if the reciprocal of the bound on
              ! elements of x is not too small.
              call stdlib_strsv( uplo, trans, diag, n, a, lda, x, 1_ilp )
           else
              ! use a level 1 blas solve, scaling intermediate results.
              if( xmax>bignum ) then
                 ! scale x so that its components are less than or equal to
                 ! bignum in absolute value.
                 scale = bignum / xmax
                 call stdlib_sscal( n, scale, x, 1_ilp )
                 xmax = bignum
              end if
              if( notran ) then
                 ! solve a * x = b
                 loop_100: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) / a(j,j), scaling x if necessary.
                    xj = abs( x( j ) )
                    if( nounit ) then
                       tjjs = a( j, j )*tscal
                    else
                       tjjs = tscal
                       if( tscal==one )go to 95
                    end if
                       tjj = abs( tjjs )
                       if( tjj>smlnum ) then
                          ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                ! scale x by 1/b(j).
                                rec = one / xj
                                call stdlib_sscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = x( j ) / tjjs
                          xj = abs( x( j ) )
                       else if( tjj>zero ) then
                          ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                             ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum
                             ! to avoid overflow when dividing by a(j,j).
                             rec = ( tjj*bignum ) / xj
                             if( cnorm( j )>one ) then
                                ! scale by 1/cnorm(j) to avoid overflow when
                                ! multiplying x(j) times column j.
                                rec = rec / cnorm( j )
                             end if
                             call stdlib_sscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = x( j ) / tjjs
                          xj = abs( x( j ) )
                       else
                          ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                          ! scale = 0, and compute a solution to a*x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          xj = one
                          scale = zero
                          xmax = zero
                       end if
                       95 continue
                    ! scale x if necessary to avoid overflow when adding a
                    ! multiple of column j of a.
                    if( xj>one ) then
                       rec = one / xj
                       if( cnorm( j )>( bignum-xmax )*rec ) then
                          ! scale x by 1/(2*abs(x(j))).
                          rec = rec*half
                          call stdlib_sscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                       end if
                    else if( xj*cnorm( j )>( bignum-xmax ) ) then
                       ! scale x by 1/2.
                       call stdlib_sscal( n, half, x, 1_ilp )
                       scale = scale*half
                    end if
                    if( upper ) then
                       if( j>1_ilp ) then
                          ! compute the update
                             ! x(1:j-1) := x(1:j-1) - x(j) * a(1:j-1,j)
                          call stdlib_saxpy( j-1, -x( j )*tscal, a( 1_ilp, j ), 1_ilp, x,1_ilp )
                          i = stdlib_isamax( j-1, x, 1_ilp )
                          xmax = abs( x( i ) )
                       end if
                    else
                       if( j<n ) then
                          ! compute the update
                             ! x(j+1:n) := x(j+1:n) - x(j) * a(j+1:n,j)
                          call stdlib_saxpy( n-j, -x( j )*tscal, a( j+1, j ), 1_ilp,x( j+1 ), 1_ilp )
                                    
                          i = j + stdlib_isamax( n-j, x( j+1 ), 1_ilp )
                          xmax = abs( x( i ) )
                       end if
                    end if
                 end do loop_100
              else
                 ! solve a**t * x = b
                 loop_140: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = abs( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = a( j, j )*tscal
                       else
                          tjjs = tscal
                       end if
                          tjj = abs( tjjs )
                          if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                             rec = min( one, rec*tjj )
                             uscal = uscal / tjjs
                          end if
                       if( rec<one ) then
                          call stdlib_sscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    sumj = zero
                    if( uscal==one ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_sdot to perform the dot product.
                       if( upper ) then
                          sumj = stdlib_sdot( j-1, a( 1_ilp, j ), 1_ilp, x, 1_ilp )
                       else if( j<n ) then
                          sumj = stdlib_sdot( n-j, a( j+1, j ), 1_ilp, x( j+1 ), 1_ilp )
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          do i = 1, j - 1
                             sumj = sumj + ( a( i, j )*uscal )*x( i )
                          end do
                       else if( j<n ) then
                          do i = j + 1, n
                             sumj = sumj + ( a( i, j )*uscal )*x( i )
                          end do
                       end if
                    end if
                    if( uscal==tscal ) then
                       ! compute x(j) := ( x(j) - sumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - sumj
                       xj = abs( x( j ) )
                       if( nounit ) then
                          tjjs = a( j, j )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 135
                       end if
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjj = abs( tjjs )
                          if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                             if( tjj<one ) then
                                if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                   rec = one / xj
                                   call stdlib_sscal( n, rec, x, 1_ilp )
                                   scale = scale*rec
                                   xmax = xmax*rec
                                end if
                             end if
                             x( j ) = x( j ) / tjjs
                          else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                             if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                                rec = ( tjj*bignum ) / xj
                                call stdlib_sscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                             x( j ) = x( j ) / tjjs
                          else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0, and compute a solution to a**t*x = 0.
                             do i = 1, n
                                x( i ) = zero
                             end do
                             x( j ) = one
                             scale = zero
                             xmax = zero
                          end if
                          135 continue
                    else
                       ! compute x(j) := x(j) / a(j,j)  - sumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = x( j ) / tjjs - sumj
                    end if
                    xmax = max( xmax, abs( x( j ) ) )
                 end do loop_140
              end if
              scale = scale / tscal
           end if
           ! scale the column norms by 1/tscal for return.
           if( tscal/=one ) then
              call stdlib_sscal( n, one / tscal, cnorm, 1_ilp )
           end if
           return
     end subroutine stdlib_slatrs

     pure module subroutine stdlib_dlatrs( uplo, trans, diag, normin, n, a, lda, x, scale,cnorm, info )
     !! DLATRS solves one of the triangular systems
     !! A *x = s*b  or  A**T *x = s*b
     !! with scaling to prevent overflow.  Here A is an upper or lower
     !! triangular matrix, A**T denotes the transpose of A, x and b are
     !! n-element vectors, and s is a scaling factor, usually less than
     !! or equal to 1, chosen so that the components of x will be less than
     !! the overflow threshold.  If the unscaled problem will not cause
     !! overflow, the Level 2 BLAS routine DTRSV is called.  If the matrix A
     !! is singular (A(j,j) = 0 for some j), then s is set to 0 and a
     !! non-trivial solution to A*x = 0 is returned.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, normin, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: scale
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: cnorm(*), x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           integer(ilp) :: i, imax, j, jfirst, jinc, jlast
           real(dp) :: bignum, grow, rec, smlnum, sumj, tjj, tjjs, tmax, tscal, uscal, xbnd, xj, &
                     xmax
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           ! test the input parameters.
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( normin, 'Y' ) .and. .not.stdlib_lsame( normin, 'N' ) ) &
                     then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLATRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine machine dependent parameters to control overflow.
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           bignum = one / smlnum
           scale = one
           if( stdlib_lsame( normin, 'N' ) ) then
              ! compute the 1-norm of each column, not including the diagonal.
              if( upper ) then
                 ! a is upper triangular.
                 do j = 1, n
                    cnorm( j ) = stdlib_dasum( j-1, a( 1_ilp, j ), 1_ilp )
                 end do
              else
                 ! a is lower triangular.
                 do j = 1, n - 1
                    cnorm( j ) = stdlib_dasum( n-j, a( j+1, j ), 1_ilp )
                 end do
                 cnorm( n ) = zero
              end if
           end if
           ! scale the column norms by tscal if the maximum element in cnorm is
           ! greater than bignum.
           imax = stdlib_idamax( n, cnorm, 1_ilp )
           tmax = cnorm( imax )
           if( tmax<=bignum ) then
              tscal = one
           else
              tscal = one / ( smlnum*tmax )
              call stdlib_dscal( n, tscal, cnorm, 1_ilp )
           end if
           ! compute a bound on the computed solution vector to see if the
           ! level 2 blas routine stdlib_dtrsv can be used.
           j = stdlib_idamax( n, x, 1_ilp )
           xmax = abs( x( j ) )
           xbnd = xmax
           if( notran ) then
              ! compute the growth in a * x = b.
              if( upper ) then
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              else
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 50
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, g(0) = max{x(i), i=1,...,n}.
                 grow = one / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 50
                    ! m(j) = g(j-1) / abs(a(j,j))
                    tjj = abs( a( j, j ) )
                    xbnd = min( xbnd, min( one, tjj )*grow )
                    if( tjj+cnorm( j )>=smlnum ) then
                       ! g(j) = g(j-1)*( 1 + cnorm(j) / abs(a(j,j)) )
                       grow = grow*( tjj / ( tjj+cnorm( j ) ) )
                    else
                       ! g(j) could overflow, set grow to 0.
                       grow = zero
                    end if
                 end do
                 grow = xbnd
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, one / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 50
                    ! g(j) = g(j-1)*( 1 + cnorm(j) )
                    grow = grow*( one / ( one+cnorm( j ) ) )
                 end do
              end if
              50 continue
           else
              ! compute the growth in a**t * x = b.
              if( upper ) then
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              else
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 80
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, m(0) = max{x(i), i=1,...,n}.
                 grow = one / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 80
                    ! g(j) = max( g(j-1), m(j-1)*( 1 + cnorm(j) ) )
                    xj = one + cnorm( j )
                    grow = min( grow, xbnd / xj )
                    ! m(j) = m(j-1)*( 1 + cnorm(j) ) / abs(a(j,j))
                    tjj = abs( a( j, j ) )
                    if( xj>tjj )xbnd = xbnd*( tjj / xj )
                 end do
                 grow = min( grow, xbnd )
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, one / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 80
                    ! g(j) = ( 1 + cnorm(j) )*g(j-1)
                    xj = one + cnorm( j )
                    grow = grow / xj
                 end do
              end if
              80 continue
           end if
           if( ( grow*tscal )>smlnum ) then
              ! use the level 2 blas solve if the reciprocal of the bound on
              ! elements of x is not too small.
              call stdlib_dtrsv( uplo, trans, diag, n, a, lda, x, 1_ilp )
           else
              ! use a level 1 blas solve, scaling intermediate results.
              if( xmax>bignum ) then
                 ! scale x so that its components are less than or equal to
                 ! bignum in absolute value.
                 scale = bignum / xmax
                 call stdlib_dscal( n, scale, x, 1_ilp )
                 xmax = bignum
              end if
              if( notran ) then
                 ! solve a * x = b
                 loop_110: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) / a(j,j), scaling x if necessary.
                    xj = abs( x( j ) )
                    if( nounit ) then
                       tjjs = a( j, j )*tscal
                    else
                       tjjs = tscal
                       if( tscal==one )go to 100
                    end if
                    tjj = abs( tjjs )
                    if( tjj>smlnum ) then
                          ! abs(a(j,j)) > smlnum:
                       if( tjj<one ) then
                          if( xj>tjj*bignum ) then
                                ! scale x by 1/b(j).
                             rec = one / xj
                             call stdlib_dscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j ) = x( j ) / tjjs
                       xj = abs( x( j ) )
                    else if( tjj>zero ) then
                          ! 0 < abs(a(j,j)) <= smlnum:
                       if( xj>tjj*bignum ) then
                             ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum
                             ! to avoid overflow when dividing by a(j,j).
                          rec = ( tjj*bignum ) / xj
                          if( cnorm( j )>one ) then
                                ! scale by 1/cnorm(j) to avoid overflow when
                                ! multiplying x(j) times column j.
                             rec = rec / cnorm( j )
                          end if
                          call stdlib_dscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                       x( j ) = x( j ) / tjjs
                       xj = abs( x( j ) )
                    else
                          ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                          ! scale = 0, and compute a solution to a*x = 0.
                       do i = 1, n
                          x( i ) = zero
                       end do
                       x( j ) = one
                       xj = one
                       scale = zero
                       xmax = zero
                    end if
                    100 continue
                    ! scale x if necessary to avoid overflow when adding a
                    ! multiple of column j of a.
                    if( xj>one ) then
                       rec = one / xj
                       if( cnorm( j )>( bignum-xmax )*rec ) then
                          ! scale x by 1/(2*abs(x(j))).
                          rec = rec*half
                          call stdlib_dscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                       end if
                    else if( xj*cnorm( j )>( bignum-xmax ) ) then
                       ! scale x by 1/2.
                       call stdlib_dscal( n, half, x, 1_ilp )
                       scale = scale*half
                    end if
                    if( upper ) then
                       if( j>1_ilp ) then
                          ! compute the update
                             ! x(1:j-1) := x(1:j-1) - x(j) * a(1:j-1,j)
                          call stdlib_daxpy( j-1, -x( j )*tscal, a( 1_ilp, j ), 1_ilp, x,1_ilp )
                          i = stdlib_idamax( j-1, x, 1_ilp )
                          xmax = abs( x( i ) )
                       end if
                    else
                       if( j<n ) then
                          ! compute the update
                             ! x(j+1:n) := x(j+1:n) - x(j) * a(j+1:n,j)
                          call stdlib_daxpy( n-j, -x( j )*tscal, a( j+1, j ), 1_ilp,x( j+1 ), 1_ilp )
                                    
                          i = j + stdlib_idamax( n-j, x( j+1 ), 1_ilp )
                          xmax = abs( x( i ) )
                       end if
                    end if
                 end do loop_110
              else
                 ! solve a**t * x = b
                 loop_160: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = abs( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = a( j, j )*tscal
                       else
                          tjjs = tscal
                       end if
                       tjj = abs( tjjs )
                       if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                          rec = min( one, rec*tjj )
                          uscal = uscal / tjjs
                       end if
                       if( rec<one ) then
                          call stdlib_dscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    sumj = zero
                    if( uscal==one ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_ddot to perform the dot product.
                       if( upper ) then
                          sumj = stdlib_ddot( j-1, a( 1_ilp, j ), 1_ilp, x, 1_ilp )
                       else if( j<n ) then
                          sumj = stdlib_ddot( n-j, a( j+1, j ), 1_ilp, x( j+1 ), 1_ilp )
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          do i = 1, j - 1
                             sumj = sumj + ( a( i, j )*uscal )*x( i )
                          end do
                       else if( j<n ) then
                          do i = j + 1, n
                             sumj = sumj + ( a( i, j )*uscal )*x( i )
                          end do
                       end if
                    end if
                    if( uscal==tscal ) then
                       ! compute x(j) := ( x(j) - sumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - sumj
                       xj = abs( x( j ) )
                       if( nounit ) then
                          tjjs = a( j, j )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 150
                       end if
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                       tjj = abs( tjjs )
                       if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                rec = one / xj
                                call stdlib_dscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = x( j ) / tjjs
                       else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                             rec = ( tjj*bignum ) / xj
                             call stdlib_dscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = x( j ) / tjjs
                       else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0, and compute a solution to a**t*x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          scale = zero
                          xmax = zero
                       end if
                       150 continue
                    else
                       ! compute x(j) := x(j) / a(j,j)  - sumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = x( j ) / tjjs - sumj
                    end if
                    xmax = max( xmax, abs( x( j ) ) )
                 end do loop_160
              end if
              scale = scale / tscal
           end if
           ! scale the column norms by 1/tscal for return.
           if( tscal/=one ) then
              call stdlib_dscal( n, one / tscal, cnorm, 1_ilp )
           end if
           return
     end subroutine stdlib_dlatrs


     pure module subroutine stdlib_clatrs( uplo, trans, diag, normin, n, a, lda, x, scale,cnorm, info )
     !! CLATRS solves one of the triangular systems
     !! A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b,
     !! with scaling to prevent overflow.  Here A is an upper or lower
     !! triangular matrix, A**T denotes the transpose of A, A**H denotes the
     !! conjugate transpose of A, x and b are n-element vectors, and s is a
     !! scaling factor, usually less than or equal to 1, chosen so that the
     !! components of x will be less than the overflow threshold.  If the
     !! unscaled problem will not cause overflow, the Level 2 BLAS routine
     !! CTRSV is called. If the matrix A is singular (A(j,j) = 0 for some j),
     !! then s is set to 0 and a non-trivial solution to A*x = 0 is returned.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, normin, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: scale
           ! Array Arguments 
           real(sp), intent(inout) :: cnorm(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           integer(ilp) :: i, imax, j, jfirst, jinc, jlast
           real(sp) :: bignum, grow, rec, smlnum, tjj, tmax, tscal, xbnd, xj, xmax
           complex(sp) :: csumj, tjjs, uscal, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1, cabs2
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           cabs2( zdum ) = abs( real( zdum,KIND=sp) / 2. ) +abs( aimag( zdum ) / 2. )
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           ! test the input parameters.
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( normin, 'Y' ) .and. .not.stdlib_lsame( normin, 'N' ) ) &
                     then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLATRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine machine dependent parameters to control overflow.
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = smlnum / stdlib_slamch( 'PRECISION' )
           bignum = one / smlnum
           scale = one
           if( stdlib_lsame( normin, 'N' ) ) then
              ! compute the 1-norm of each column, not including the diagonal.
              if( upper ) then
                 ! a is upper triangular.
                 do j = 1, n
                    cnorm( j ) = stdlib_scasum( j-1, a( 1_ilp, j ), 1_ilp )
                 end do
              else
                 ! a is lower triangular.
                 do j = 1, n - 1
                    cnorm( j ) = stdlib_scasum( n-j, a( j+1, j ), 1_ilp )
                 end do
                 cnorm( n ) = zero
              end if
           end if
           ! scale the column norms by tscal if the maximum element in cnorm is
           ! greater than bignum/2.
           imax = stdlib_isamax( n, cnorm, 1_ilp )
           tmax = cnorm( imax )
           if( tmax<=bignum*half ) then
              tscal = one
           else
              tscal = half / ( smlnum*tmax )
              call stdlib_sscal( n, tscal, cnorm, 1_ilp )
           end if
           ! compute a bound on the computed solution vector to see if the
           ! level 2 blas routine stdlib_ctrsv can be used.
           xmax = zero
           do j = 1, n
              xmax = max( xmax, cabs2( x( j ) ) )
           end do
           xbnd = xmax
           if( notran ) then
              ! compute the growth in a * x = b.
              if( upper ) then
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              else
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 60
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, g(0) = max{x(i), i=1,...,n}.
                 grow = half / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 60
                    tjjs = a( j, j )
                    tjj = cabs1( tjjs )
                    if( tjj>=smlnum ) then
                       ! m(j) = g(j-1) / abs(a(j,j))
                       xbnd = min( xbnd, min( one, tjj )*grow )
                    else
                       ! m(j) could overflow, set xbnd to 0.
                       xbnd = zero
                    end if
                    if( tjj+cnorm( j )>=smlnum ) then
                       ! g(j) = g(j-1)*( 1 + cnorm(j) / abs(a(j,j)) )
                       grow = grow*( tjj / ( tjj+cnorm( j ) ) )
                    else
                       ! g(j) could overflow, set grow to 0.
                       grow = zero
                    end if
                 end do
                 grow = xbnd
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, half / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 60
                    ! g(j) = g(j-1)*( 1 + cnorm(j) )
                    grow = grow*( one / ( one+cnorm( j ) ) )
                 end do
              end if
              60 continue
           else
              ! compute the growth in a**t * x = b  or  a**h * x = b.
              if( upper ) then
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              else
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 90
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, m(0) = max{x(i), i=1,...,n}.
                 grow = half / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 90
                    ! g(j) = max( g(j-1), m(j-1)*( 1 + cnorm(j) ) )
                    xj = one + cnorm( j )
                    grow = min( grow, xbnd / xj )
                    tjjs = a( j, j )
                    tjj = cabs1( tjjs )
                    if( tjj>=smlnum ) then
                       ! m(j) = m(j-1)*( 1 + cnorm(j) ) / abs(a(j,j))
                       if( xj>tjj )xbnd = xbnd*( tjj / xj )
                    else
                       ! m(j) could overflow, set xbnd to 0.
                       xbnd = zero
                    end if
                 end do
                 grow = min( grow, xbnd )
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, half / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 90
                    ! g(j) = ( 1 + cnorm(j) )*g(j-1)
                    xj = one + cnorm( j )
                    grow = grow / xj
                 end do
              end if
              90 continue
           end if
           if( ( grow*tscal )>smlnum ) then
              ! use the level 2 blas solve if the reciprocal of the bound on
              ! elements of x is not too small.
              call stdlib_ctrsv( uplo, trans, diag, n, a, lda, x, 1_ilp )
           else
              ! use a level 1 blas solve, scaling intermediate results.
              if( xmax>bignum*half ) then
                 ! scale x so that its components are less than or equal to
                 ! bignum in absolute value.
                 scale = ( bignum*half ) / xmax
                 call stdlib_csscal( n, scale, x, 1_ilp )
                 xmax = bignum
              else
                 xmax = xmax*two
              end if
              if( notran ) then
                 ! solve a * x = b
                 loop_110: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) / a(j,j), scaling x if necessary.
                    xj = cabs1( x( j ) )
                    if( nounit ) then
                       tjjs = a( j, j )*tscal
                    else
                       tjjs = tscal
                       if( tscal==one )go to 105
                    end if
                       tjj = cabs1( tjjs )
                       if( tjj>smlnum ) then
                          ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                ! scale x by 1/b(j).
                                rec = one / xj
                                call stdlib_csscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = stdlib_cladiv( x( j ), tjjs )
                          xj = cabs1( x( j ) )
                       else if( tjj>zero ) then
                          ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                             ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum
                             ! to avoid overflow when dividing by a(j,j).
                             rec = ( tjj*bignum ) / xj
                             if( cnorm( j )>one ) then
                                ! scale by 1/cnorm(j) to avoid overflow when
                                ! multiplying x(j) times column j.
                                rec = rec / cnorm( j )
                             end if
                             call stdlib_csscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = stdlib_cladiv( x( j ), tjjs )
                          xj = cabs1( x( j ) )
                       else
                          ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                          ! scale = 0, and compute a solution to a*x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          xj = one
                          scale = zero
                          xmax = zero
                       end if
                       105 continue
                    ! scale x if necessary to avoid overflow when adding a
                    ! multiple of column j of a.
                    if( xj>one ) then
                       rec = one / xj
                       if( cnorm( j )>( bignum-xmax )*rec ) then
                          ! scale x by 1/(2*abs(x(j))).
                          rec = rec*half
                          call stdlib_csscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                       end if
                    else if( xj*cnorm( j )>( bignum-xmax ) ) then
                       ! scale x by 1/2.
                       call stdlib_csscal( n, half, x, 1_ilp )
                       scale = scale*half
                    end if
                    if( upper ) then
                       if( j>1_ilp ) then
                          ! compute the update
                             ! x(1:j-1) := x(1:j-1) - x(j) * a(1:j-1,j)
                          call stdlib_caxpy( j-1, -x( j )*tscal, a( 1_ilp, j ), 1_ilp, x,1_ilp )
                          i = stdlib_icamax( j-1, x, 1_ilp )
                          xmax = cabs1( x( i ) )
                       end if
                    else
                       if( j<n ) then
                          ! compute the update
                             ! x(j+1:n) := x(j+1:n) - x(j) * a(j+1:n,j)
                          call stdlib_caxpy( n-j, -x( j )*tscal, a( j+1, j ), 1_ilp,x( j+1 ), 1_ilp )
                                    
                          i = j + stdlib_icamax( n-j, x( j+1 ), 1_ilp )
                          xmax = cabs1( x( i ) )
                       end if
                    end if
                 end do loop_110
              else if( stdlib_lsame( trans, 'T' ) ) then
                 ! solve a**t * x = b
                 loop_150: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = cabs1( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = a( j, j )*tscal
                       else
                          tjjs = tscal
                       end if
                          tjj = cabs1( tjjs )
                          if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                             rec = min( one, rec*tjj )
                             uscal = stdlib_cladiv( uscal, tjjs )
                          end if
                       if( rec<one ) then
                          call stdlib_csscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    csumj = zero
                    if( uscal==cmplx( one,KIND=sp) ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_cdotu to perform the dot product.
                       if( upper ) then
                          csumj = stdlib_cdotu( j-1, a( 1_ilp, j ), 1_ilp, x, 1_ilp )
                       else if( j<n ) then
                          csumj = stdlib_cdotu( n-j, a( j+1, j ), 1_ilp, x( j+1 ), 1_ilp )
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          do i = 1, j - 1
                             csumj = csumj + ( a( i, j )*uscal )*x( i )
                          end do
                       else if( j<n ) then
                          do i = j + 1, n
                             csumj = csumj + ( a( i, j )*uscal )*x( i )
                          end do
                       end if
                    end if
                    if( uscal==cmplx( tscal,KIND=sp) ) then
                       ! compute x(j) := ( x(j) - csumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - csumj
                       xj = cabs1( x( j ) )
                       if( nounit ) then
                          tjjs = a( j, j )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 145
                       end if
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjj = cabs1( tjjs )
                          if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                             if( tjj<one ) then
                                if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                   rec = one / xj
                                   call stdlib_csscal( n, rec, x, 1_ilp )
                                   scale = scale*rec
                                   xmax = xmax*rec
                                end if
                             end if
                             x( j ) = stdlib_cladiv( x( j ), tjjs )
                          else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                             if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                                rec = ( tjj*bignum ) / xj
                                call stdlib_csscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                             x( j ) = stdlib_cladiv( x( j ), tjjs )
                          else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0 and compute a solution to a**t *x = 0.
                             do i = 1, n
                                x( i ) = zero
                             end do
                             x( j ) = one
                             scale = zero
                             xmax = zero
                          end if
                          145 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - csumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = stdlib_cladiv( x( j ), tjjs ) - csumj
                    end if
                    xmax = max( xmax, cabs1( x( j ) ) )
                 end do loop_150
              else
                 ! solve a**h * x = b
                 loop_190: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = cabs1( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = conjg( a( j, j ) )*tscal
                       else
                          tjjs = tscal
                       end if
                          tjj = cabs1( tjjs )
                          if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                             rec = min( one, rec*tjj )
                             uscal = stdlib_cladiv( uscal, tjjs )
                          end if
                       if( rec<one ) then
                          call stdlib_csscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    csumj = zero
                    if( uscal==cmplx( one,KIND=sp) ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_cdotc to perform the dot product.
                       if( upper ) then
                          csumj = stdlib_cdotc( j-1, a( 1_ilp, j ), 1_ilp, x, 1_ilp )
                       else if( j<n ) then
                          csumj = stdlib_cdotc( n-j, a( j+1, j ), 1_ilp, x( j+1 ), 1_ilp )
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          do i = 1, j - 1
                             csumj = csumj + ( conjg( a( i, j ) )*uscal )*x( i )
                          end do
                       else if( j<n ) then
                          do i = j + 1, n
                             csumj = csumj + ( conjg( a( i, j ) )*uscal )*x( i )
                          end do
                       end if
                    end if
                    if( uscal==cmplx( tscal,KIND=sp) ) then
                       ! compute x(j) := ( x(j) - csumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - csumj
                       xj = cabs1( x( j ) )
                       if( nounit ) then
                          tjjs = conjg( a( j, j ) )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 185
                       end if
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjj = cabs1( tjjs )
                          if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                             if( tjj<one ) then
                                if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                   rec = one / xj
                                   call stdlib_csscal( n, rec, x, 1_ilp )
                                   scale = scale*rec
                                   xmax = xmax*rec
                                end if
                             end if
                             x( j ) = stdlib_cladiv( x( j ), tjjs )
                          else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                             if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                                rec = ( tjj*bignum ) / xj
                                call stdlib_csscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                             x( j ) = stdlib_cladiv( x( j ), tjjs )
                          else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0 and compute a solution to a**h *x = 0.
                             do i = 1, n
                                x( i ) = zero
                             end do
                             x( j ) = one
                             scale = zero
                             xmax = zero
                          end if
                          185 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - csumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = stdlib_cladiv( x( j ), tjjs ) - csumj
                    end if
                    xmax = max( xmax, cabs1( x( j ) ) )
                 end do loop_190
              end if
              scale = scale / tscal
           end if
           ! scale the column norms by 1/tscal for return.
           if( tscal/=one ) then
              call stdlib_sscal( n, one / tscal, cnorm, 1_ilp )
           end if
           return
     end subroutine stdlib_clatrs

     pure module subroutine stdlib_zlatrs( uplo, trans, diag, normin, n, a, lda, x, scale,cnorm, info )
     !! ZLATRS solves one of the triangular systems
     !! A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b,
     !! with scaling to prevent overflow.  Here A is an upper or lower
     !! triangular matrix, A**T denotes the transpose of A, A**H denotes the
     !! conjugate transpose of A, x and b are n-element vectors, and s is a
     !! scaling factor, usually less than or equal to 1, chosen so that the
     !! components of x will be less than the overflow threshold.  If the
     !! unscaled problem will not cause overflow, the Level 2 BLAS routine
     !! ZTRSV is called. If the matrix A is singular (A(j,j) = 0 for some j),
     !! then s is set to 0 and a non-trivial solution to A*x = 0 is returned.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, normin, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: scale
           ! Array Arguments 
           real(dp), intent(inout) :: cnorm(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           integer(ilp) :: i, imax, j, jfirst, jinc, jlast
           real(dp) :: bignum, grow, rec, smlnum, tjj, tmax, tscal, xbnd, xj, xmax
           complex(dp) :: csumj, tjjs, uscal, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1, cabs2
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           cabs2( zdum ) = abs( real( zdum,KIND=dp) / 2._dp ) +abs( aimag( zdum ) / 2._dp )
                     
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           ! test the input parameters.
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( normin, 'Y' ) .and. .not.stdlib_lsame( normin, 'N' ) ) &
                     then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLATRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine machine dependent parameters to control overflow.
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = smlnum / stdlib_dlamch( 'PRECISION' )
           bignum = one / smlnum
           scale = one
           if( stdlib_lsame( normin, 'N' ) ) then
              ! compute the 1-norm of each column, not including the diagonal.
              if( upper ) then
                 ! a is upper triangular.
                 do j = 1, n
                    cnorm( j ) = stdlib_dzasum( j-1, a( 1_ilp, j ), 1_ilp )
                 end do
              else
                 ! a is lower triangular.
                 do j = 1, n - 1
                    cnorm( j ) = stdlib_dzasum( n-j, a( j+1, j ), 1_ilp )
                 end do
                 cnorm( n ) = zero
              end if
           end if
           ! scale the column norms by tscal if the maximum element in cnorm is
           ! greater than bignum/2.
           imax = stdlib_idamax( n, cnorm, 1_ilp )
           tmax = cnorm( imax )
           if( tmax<=bignum*half ) then
              tscal = one
           else
              tscal = half / ( smlnum*tmax )
              call stdlib_dscal( n, tscal, cnorm, 1_ilp )
           end if
           ! compute a bound on the computed solution vector to see if the
           ! level 2 blas routine stdlib_ztrsv can be used.
           xmax = zero
           do j = 1, n
              xmax = max( xmax, cabs2( x( j ) ) )
           end do
           xbnd = xmax
           if( notran ) then
              ! compute the growth in a * x = b.
              if( upper ) then
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              else
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 60
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, g(0) = max{x(i), i=1,...,n}.
                 grow = half / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 60
                    tjjs = a( j, j )
                    tjj = cabs1( tjjs )
                    if( tjj>=smlnum ) then
                       ! m(j) = g(j-1) / abs(a(j,j))
                       xbnd = min( xbnd, min( one, tjj )*grow )
                    else
                       ! m(j) could overflow, set xbnd to 0.
                       xbnd = zero
                    end if
                    if( tjj+cnorm( j )>=smlnum ) then
                       ! g(j) = g(j-1)*( 1 + cnorm(j) / abs(a(j,j)) )
                       grow = grow*( tjj / ( tjj+cnorm( j ) ) )
                    else
                       ! g(j) could overflow, set grow to 0.
                       grow = zero
                    end if
                 end do
                 grow = xbnd
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, half / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 60
                    ! g(j) = g(j-1)*( 1 + cnorm(j) )
                    grow = grow*( one / ( one+cnorm( j ) ) )
                 end do
              end if
              60 continue
           else
              ! compute the growth in a**t * x = b  or  a**h * x = b.
              if( upper ) then
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              else
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 90
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, m(0) = max{x(i), i=1,...,n}.
                 grow = half / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 90
                    ! g(j) = max( g(j-1), m(j-1)*( 1 + cnorm(j) ) )
                    xj = one + cnorm( j )
                    grow = min( grow, xbnd / xj )
                    tjjs = a( j, j )
                    tjj = cabs1( tjjs )
                    if( tjj>=smlnum ) then
                       ! m(j) = m(j-1)*( 1 + cnorm(j) ) / abs(a(j,j))
                       if( xj>tjj )xbnd = xbnd*( tjj / xj )
                    else
                       ! m(j) could overflow, set xbnd to 0.
                       xbnd = zero
                    end if
                 end do
                 grow = min( grow, xbnd )
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, half / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 90
                    ! g(j) = ( 1 + cnorm(j) )*g(j-1)
                    xj = one + cnorm( j )
                    grow = grow / xj
                 end do
              end if
              90 continue
           end if
           if( ( grow*tscal )>smlnum ) then
              ! use the level 2 blas solve if the reciprocal of the bound on
              ! elements of x is not too small.
              call stdlib_ztrsv( uplo, trans, diag, n, a, lda, x, 1_ilp )
           else
              ! use a level 1 blas solve, scaling intermediate results.
              if( xmax>bignum*half ) then
                 ! scale x so that its components are less than or equal to
                 ! bignum in absolute value.
                 scale = ( bignum*half ) / xmax
                 call stdlib_zdscal( n, scale, x, 1_ilp )
                 xmax = bignum
              else
                 xmax = xmax*two
              end if
              if( notran ) then
                 ! solve a * x = b
                 loop_120: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) / a(j,j), scaling x if necessary.
                    xj = cabs1( x( j ) )
                    if( nounit ) then
                       tjjs = a( j, j )*tscal
                    else
                       tjjs = tscal
                       if( tscal==one )go to 110
                    end if
                    tjj = cabs1( tjjs )
                    if( tjj>smlnum ) then
                          ! abs(a(j,j)) > smlnum:
                       if( tjj<one ) then
                          if( xj>tjj*bignum ) then
                                ! scale x by 1/b(j).
                             rec = one / xj
                             call stdlib_zdscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j ) = stdlib_zladiv( x( j ), tjjs )
                       xj = cabs1( x( j ) )
                    else if( tjj>zero ) then
                          ! 0 < abs(a(j,j)) <= smlnum:
                       if( xj>tjj*bignum ) then
                             ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum
                             ! to avoid overflow when dividing by a(j,j).
                          rec = ( tjj*bignum ) / xj
                          if( cnorm( j )>one ) then
                                ! scale by 1/cnorm(j) to avoid overflow when
                                ! multiplying x(j) times column j.
                             rec = rec / cnorm( j )
                          end if
                          call stdlib_zdscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                       x( j ) = stdlib_zladiv( x( j ), tjjs )
                       xj = cabs1( x( j ) )
                    else
                          ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                          ! scale = 0, and compute a solution to a*x = 0.
                       do i = 1, n
                          x( i ) = zero
                       end do
                       x( j ) = one
                       xj = one
                       scale = zero
                       xmax = zero
                    end if
                    110 continue
                    ! scale x if necessary to avoid overflow when adding a
                    ! multiple of column j of a.
                    if( xj>one ) then
                       rec = one / xj
                       if( cnorm( j )>( bignum-xmax )*rec ) then
                          ! scale x by 1/(2*abs(x(j))).
                          rec = rec*half
                          call stdlib_zdscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                       end if
                    else if( xj*cnorm( j )>( bignum-xmax ) ) then
                       ! scale x by 1/2.
                       call stdlib_zdscal( n, half, x, 1_ilp )
                       scale = scale*half
                    end if
                    if( upper ) then
                       if( j>1_ilp ) then
                          ! compute the update
                             ! x(1:j-1) := x(1:j-1) - x(j) * a(1:j-1,j)
                          call stdlib_zaxpy( j-1, -x( j )*tscal, a( 1_ilp, j ), 1_ilp, x,1_ilp )
                          i = stdlib_izamax( j-1, x, 1_ilp )
                          xmax = cabs1( x( i ) )
                       end if
                    else
                       if( j<n ) then
                          ! compute the update
                             ! x(j+1:n) := x(j+1:n) - x(j) * a(j+1:n,j)
                          call stdlib_zaxpy( n-j, -x( j )*tscal, a( j+1, j ), 1_ilp,x( j+1 ), 1_ilp )
                                    
                          i = j + stdlib_izamax( n-j, x( j+1 ), 1_ilp )
                          xmax = cabs1( x( i ) )
                       end if
                    end if
                 end do loop_120
              else if( stdlib_lsame( trans, 'T' ) ) then
                 ! solve a**t * x = b
                 loop_170: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = cabs1( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = a( j, j )*tscal
                       else
                          tjjs = tscal
                       end if
                       tjj = cabs1( tjjs )
                       if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                          rec = min( one, rec*tjj )
                          uscal = stdlib_zladiv( uscal, tjjs )
                       end if
                       if( rec<one ) then
                          call stdlib_zdscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    csumj = zero
                    if( uscal==cmplx( one,KIND=dp) ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_zdotu to perform the dot product.
                       if( upper ) then
                          csumj = stdlib_zdotu( j-1, a( 1_ilp, j ), 1_ilp, x, 1_ilp )
                       else if( j<n ) then
                          csumj = stdlib_zdotu( n-j, a( j+1, j ), 1_ilp, x( j+1 ), 1_ilp )
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          do i = 1, j - 1
                             csumj = csumj + ( a( i, j )*uscal )*x( i )
                          end do
                       else if( j<n ) then
                          do i = j + 1, n
                             csumj = csumj + ( a( i, j )*uscal )*x( i )
                          end do
                       end if
                    end if
                    if( uscal==cmplx( tscal,KIND=dp) ) then
                       ! compute x(j) := ( x(j) - csumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - csumj
                       xj = cabs1( x( j ) )
                       if( nounit ) then
                          tjjs = a( j, j )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 160
                       end if
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                       tjj = cabs1( tjjs )
                       if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                rec = one / xj
                                call stdlib_zdscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = stdlib_zladiv( x( j ), tjjs )
                       else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                             rec = ( tjj*bignum ) / xj
                             call stdlib_zdscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = stdlib_zladiv( x( j ), tjjs )
                       else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0 and compute a solution to a**t *x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          scale = zero
                          xmax = zero
                       end if
                       160 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - csumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = stdlib_zladiv( x( j ), tjjs ) - csumj
                    end if
                    xmax = max( xmax, cabs1( x( j ) ) )
                 end do loop_170
              else
                 ! solve a**h * x = b
                 loop_220: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = cabs1( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = conjg( a( j, j ) )*tscal
                       else
                          tjjs = tscal
                       end if
                       tjj = cabs1( tjjs )
                       if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                          rec = min( one, rec*tjj )
                          uscal = stdlib_zladiv( uscal, tjjs )
                       end if
                       if( rec<one ) then
                          call stdlib_zdscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    csumj = zero
                    if( uscal==cmplx( one,KIND=dp) ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_zdotc to perform the dot product.
                       if( upper ) then
                          csumj = stdlib_zdotc( j-1, a( 1_ilp, j ), 1_ilp, x, 1_ilp )
                       else if( j<n ) then
                          csumj = stdlib_zdotc( n-j, a( j+1, j ), 1_ilp, x( j+1 ), 1_ilp )
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          do i = 1, j - 1
                             csumj = csumj + ( conjg( a( i, j ) )*uscal )*x( i )
                          end do
                       else if( j<n ) then
                          do i = j + 1, n
                             csumj = csumj + ( conjg( a( i, j ) )*uscal )*x( i )
                          end do
                       end if
                    end if
                    if( uscal==cmplx( tscal,KIND=dp) ) then
                       ! compute x(j) := ( x(j) - csumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - csumj
                       xj = cabs1( x( j ) )
                       if( nounit ) then
                          tjjs = conjg( a( j, j ) )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 210
                       end if
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                       tjj = cabs1( tjjs )
                       if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                rec = one / xj
                                call stdlib_zdscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = stdlib_zladiv( x( j ), tjjs )
                       else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                             rec = ( tjj*bignum ) / xj
                             call stdlib_zdscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = stdlib_zladiv( x( j ), tjjs )
                       else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0 and compute a solution to a**h *x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          scale = zero
                          xmax = zero
                       end if
                       210 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - csumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = stdlib_zladiv( x( j ), tjjs ) - csumj
                    end if
                    xmax = max( xmax, cabs1( x( j ) ) )
                 end do loop_220
              end if
              scale = scale / tscal
           end if
           ! scale the column norms by 1/tscal for return.
           if( tscal/=one ) then
              call stdlib_dscal( n, one / tscal, cnorm, 1_ilp )
           end if
           return
     end subroutine stdlib_zlatrs




     pure module subroutine stdlib_strtri( uplo, diag, n, a, lda, info )
     !! STRTRI computes the inverse of a real upper or lower triangular
     !! matrix A.
     !! This is the Level 3 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j, jb, nb, nn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STRTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity if non-unit.
           if( nounit ) then
              do info = 1, n
                 if( a( info, info )==zero )return
              end do
              info = 0_ilp
           end if
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'STRTRI', uplo // diag, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_strti2( uplo, diag, n, a, lda, info )
           else
              ! use blocked code
              if( upper ) then
                 ! compute inverse of upper triangular matrix
                 do j = 1, n, nb
                    jb = min( nb, n-j+1 )
                    ! compute rows 1:j-1 of current block column
                    call stdlib_strmm( 'LEFT', 'UPPER', 'NO TRANSPOSE', diag, j-1,jb, one, a, lda,&
                               a( 1_ilp, j ), lda )
                    call stdlib_strsm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', diag, j-1,jb, -one, a( j,&
                               j ), lda, a( 1_ilp, j ), lda )
                    ! compute inverse of current diagonal block
                    call stdlib_strti2( 'UPPER', diag, jb, a( j, j ), lda, info )
                 end do
              else
                 ! compute inverse of lower triangular matrix
                 nn = ( ( n-1 ) / nb )*nb + 1_ilp
                 do j = nn, 1, -nb
                    jb = min( nb, n-j+1 )
                    if( j+jb<=n ) then
                       ! compute rows j+jb:n of current block column
                       call stdlib_strmm( 'LEFT', 'LOWER', 'NO TRANSPOSE', diag,n-j-jb+1, jb, one,&
                                  a( j+jb, j+jb ), lda,a( j+jb, j ), lda )
                       call stdlib_strsm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', diag,n-j-jb+1, jb, -&
                                 one, a( j, j ), lda,a( j+jb, j ), lda )
                    end if
                    ! compute inverse of current diagonal block
                    call stdlib_strti2( 'LOWER', diag, jb, a( j, j ), lda, info )
                 end do
              end if
           end if
           return
     end subroutine stdlib_strtri

     pure module subroutine stdlib_dtrtri( uplo, diag, n, a, lda, info )
     !! DTRTRI computes the inverse of a real upper or lower triangular
     !! matrix A.
     !! This is the Level 3 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j, jb, nb, nn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTRTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity if non-unit.
           if( nounit ) then
              do info = 1, n
                 if( a( info, info )==zero )return
              end do
              info = 0_ilp
           end if
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'DTRTRI', uplo // diag, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_dtrti2( uplo, diag, n, a, lda, info )
           else
              ! use blocked code
              if( upper ) then
                 ! compute inverse of upper triangular matrix
                 do j = 1, n, nb
                    jb = min( nb, n-j+1 )
                    ! compute rows 1:j-1 of current block column
                    call stdlib_dtrmm( 'LEFT', 'UPPER', 'NO TRANSPOSE', diag, j-1,jb, one, a, lda,&
                               a( 1_ilp, j ), lda )
                    call stdlib_dtrsm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', diag, j-1,jb, -one, a( j,&
                               j ), lda, a( 1_ilp, j ), lda )
                    ! compute inverse of current diagonal block
                    call stdlib_dtrti2( 'UPPER', diag, jb, a( j, j ), lda, info )
                 end do
              else
                 ! compute inverse of lower triangular matrix
                 nn = ( ( n-1 ) / nb )*nb + 1_ilp
                 do j = nn, 1, -nb
                    jb = min( nb, n-j+1 )
                    if( j+jb<=n ) then
                       ! compute rows j+jb:n of current block column
                       call stdlib_dtrmm( 'LEFT', 'LOWER', 'NO TRANSPOSE', diag,n-j-jb+1, jb, one,&
                                  a( j+jb, j+jb ), lda,a( j+jb, j ), lda )
                       call stdlib_dtrsm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', diag,n-j-jb+1, jb, -&
                                 one, a( j, j ), lda,a( j+jb, j ), lda )
                    end if
                    ! compute inverse of current diagonal block
                    call stdlib_dtrti2( 'LOWER', diag, jb, a( j, j ), lda, info )
                 end do
              end if
           end if
           return
     end subroutine stdlib_dtrtri


     pure module subroutine stdlib_ctrtri( uplo, diag, n, a, lda, info )
     !! CTRTRI computes the inverse of a complex upper or lower triangular
     !! matrix A.
     !! This is the Level 3 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j, jb, nb, nn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTRTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity if non-unit.
           if( nounit ) then
              do info = 1, n
                 if( a( info, info )==czero )return
              end do
              info = 0_ilp
           end if
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'CTRTRI', uplo // diag, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_ctrti2( uplo, diag, n, a, lda, info )
           else
              ! use blocked code
              if( upper ) then
                 ! compute inverse of upper triangular matrix
                 do j = 1, n, nb
                    jb = min( nb, n-j+1 )
                    ! compute rows 1:j-1 of current block column
                    call stdlib_ctrmm( 'LEFT', 'UPPER', 'NO TRANSPOSE', diag, j-1,jb, cone, a, &
                              lda, a( 1_ilp, j ), lda )
                    call stdlib_ctrsm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', diag, j-1,jb, -cone, a( &
                              j, j ), lda, a( 1_ilp, j ), lda )
                    ! compute inverse of current diagonal block
                    call stdlib_ctrti2( 'UPPER', diag, jb, a( j, j ), lda, info )
                 end do
              else
                 ! compute inverse of lower triangular matrix
                 nn = ( ( n-1 ) / nb )*nb + 1_ilp
                 do j = nn, 1, -nb
                    jb = min( nb, n-j+1 )
                    if( j+jb<=n ) then
                       ! compute rows j+jb:n of current block column
                       call stdlib_ctrmm( 'LEFT', 'LOWER', 'NO TRANSPOSE', diag,n-j-jb+1, jb, &
                                 cone, a( j+jb, j+jb ), lda,a( j+jb, j ), lda )
                       call stdlib_ctrsm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', diag,n-j-jb+1, jb, -&
                                 cone, a( j, j ), lda,a( j+jb, j ), lda )
                    end if
                    ! compute inverse of current diagonal block
                    call stdlib_ctrti2( 'LOWER', diag, jb, a( j, j ), lda, info )
                 end do
              end if
           end if
           return
     end subroutine stdlib_ctrtri

     pure module subroutine stdlib_ztrtri( uplo, diag, n, a, lda, info )
     !! ZTRTRI computes the inverse of a complex upper or lower triangular
     !! matrix A.
     !! This is the Level 3 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j, jb, nb, nn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTRTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity if non-unit.
           if( nounit ) then
              do info = 1, n
                 if( a( info, info )==czero )return
              end do
              info = 0_ilp
           end if
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'ZTRTRI', uplo // diag, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_ztrti2( uplo, diag, n, a, lda, info )
           else
              ! use blocked code
              if( upper ) then
                 ! compute inverse of upper triangular matrix
                 do j = 1, n, nb
                    jb = min( nb, n-j+1 )
                    ! compute rows 1:j-1 of current block column
                    call stdlib_ztrmm( 'LEFT', 'UPPER', 'NO TRANSPOSE', diag, j-1,jb, cone, a, &
                              lda, a( 1_ilp, j ), lda )
                    call stdlib_ztrsm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', diag, j-1,jb, -cone, a( &
                              j, j ), lda, a( 1_ilp, j ), lda )
                    ! compute inverse of current diagonal block
                    call stdlib_ztrti2( 'UPPER', diag, jb, a( j, j ), lda, info )
                 end do
              else
                 ! compute inverse of lower triangular matrix
                 nn = ( ( n-1 ) / nb )*nb + 1_ilp
                 do j = nn, 1, -nb
                    jb = min( nb, n-j+1 )
                    if( j+jb<=n ) then
                       ! compute rows j+jb:n of current block column
                       call stdlib_ztrmm( 'LEFT', 'LOWER', 'NO TRANSPOSE', diag,n-j-jb+1, jb, &
                                 cone, a( j+jb, j+jb ), lda,a( j+jb, j ), lda )
                       call stdlib_ztrsm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', diag,n-j-jb+1, jb, -&
                                 cone, a( j, j ), lda,a( j+jb, j ), lda )
                    end if
                    ! compute inverse of current diagonal block
                    call stdlib_ztrti2( 'LOWER', diag, jb, a( j, j ), lda, info )
                 end do
              end if
           end if
           return
     end subroutine stdlib_ztrtri




     pure module subroutine stdlib_strti2( uplo, diag, n, a, lda, info )
     !! STRTI2 computes the inverse of a real upper or lower triangular
     !! matrix.
     !! This is the Level 2 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j
           real(sp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STRTI2', -info )
              return
           end if
           if( upper ) then
              ! compute inverse of upper triangular matrix.
              do j = 1, n
                 if( nounit ) then
                    a( j, j ) = one / a( j, j )
                    ajj = -a( j, j )
                 else
                    ajj = -one
                 end if
                 ! compute elements 1:j-1 of j-th column.
                 call stdlib_strmv( 'UPPER', 'NO TRANSPOSE', diag, j-1, a, lda,a( 1_ilp, j ), 1_ilp )
                           
                 call stdlib_sscal( j-1, ajj, a( 1_ilp, j ), 1_ilp )
              end do
           else
              ! compute inverse of lower triangular matrix.
              do j = n, 1, -1
                 if( nounit ) then
                    a( j, j ) = one / a( j, j )
                    ajj = -a( j, j )
                 else
                    ajj = -one
                 end if
                 if( j<n ) then
                    ! compute elements j+1:n of j-th column.
                    call stdlib_strmv( 'LOWER', 'NO TRANSPOSE', diag, n-j,a( j+1, j+1 ), lda, a( &
                              j+1, j ), 1_ilp )
                    call stdlib_sscal( n-j, ajj, a( j+1, j ), 1_ilp )
                 end if
              end do
           end if
           return
     end subroutine stdlib_strti2

     pure module subroutine stdlib_dtrti2( uplo, diag, n, a, lda, info )
     !! DTRTI2 computes the inverse of a real upper or lower triangular
     !! matrix.
     !! This is the Level 2 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j
           real(dp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTRTI2', -info )
              return
           end if
           if( upper ) then
              ! compute inverse of upper triangular matrix.
              do j = 1, n
                 if( nounit ) then
                    a( j, j ) = one / a( j, j )
                    ajj = -a( j, j )
                 else
                    ajj = -one
                 end if
                 ! compute elements 1:j-1 of j-th column.
                 call stdlib_dtrmv( 'UPPER', 'NO TRANSPOSE', diag, j-1, a, lda,a( 1_ilp, j ), 1_ilp )
                           
                 call stdlib_dscal( j-1, ajj, a( 1_ilp, j ), 1_ilp )
              end do
           else
              ! compute inverse of lower triangular matrix.
              do j = n, 1, -1
                 if( nounit ) then
                    a( j, j ) = one / a( j, j )
                    ajj = -a( j, j )
                 else
                    ajj = -one
                 end if
                 if( j<n ) then
                    ! compute elements j+1:n of j-th column.
                    call stdlib_dtrmv( 'LOWER', 'NO TRANSPOSE', diag, n-j,a( j+1, j+1 ), lda, a( &
                              j+1, j ), 1_ilp )
                    call stdlib_dscal( n-j, ajj, a( j+1, j ), 1_ilp )
                 end if
              end do
           end if
           return
     end subroutine stdlib_dtrti2


     pure module subroutine stdlib_ctrti2( uplo, diag, n, a, lda, info )
     !! CTRTI2 computes the inverse of a complex upper or lower triangular
     !! matrix.
     !! This is the Level 2 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j
           complex(sp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTRTI2', -info )
              return
           end if
           if( upper ) then
              ! compute inverse of upper triangular matrix.
              do j = 1, n
                 if( nounit ) then
                    a( j, j ) = cone / a( j, j )
                    ajj = -a( j, j )
                 else
                    ajj = -cone
                 end if
                 ! compute elements 1:j-1 of j-th column.
                 call stdlib_ctrmv( 'UPPER', 'NO TRANSPOSE', diag, j-1, a, lda,a( 1_ilp, j ), 1_ilp )
                           
                 call stdlib_cscal( j-1, ajj, a( 1_ilp, j ), 1_ilp )
              end do
           else
              ! compute inverse of lower triangular matrix.
              do j = n, 1, -1
                 if( nounit ) then
                    a( j, j ) = cone / a( j, j )
                    ajj = -a( j, j )
                 else
                    ajj = -cone
                 end if
                 if( j<n ) then
                    ! compute elements j+1:n of j-th column.
                    call stdlib_ctrmv( 'LOWER', 'NO TRANSPOSE', diag, n-j,a( j+1, j+1 ), lda, a( &
                              j+1, j ), 1_ilp )
                    call stdlib_cscal( n-j, ajj, a( j+1, j ), 1_ilp )
                 end if
              end do
           end if
           return
     end subroutine stdlib_ctrti2

     pure module subroutine stdlib_ztrti2( uplo, diag, n, a, lda, info )
     !! ZTRTI2 computes the inverse of a complex upper or lower triangular
     !! matrix.
     !! This is the Level 2 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j
           complex(dp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTRTI2', -info )
              return
           end if
           if( upper ) then
              ! compute inverse of upper triangular matrix.
              do j = 1, n
                 if( nounit ) then
                    a( j, j ) = cone / a( j, j )
                    ajj = -a( j, j )
                 else
                    ajj = -cone
                 end if
                 ! compute elements 1:j-1 of j-th column.
                 call stdlib_ztrmv( 'UPPER', 'NO TRANSPOSE', diag, j-1, a, lda,a( 1_ilp, j ), 1_ilp )
                           
                 call stdlib_zscal( j-1, ajj, a( 1_ilp, j ), 1_ilp )
              end do
           else
              ! compute inverse of lower triangular matrix.
              do j = n, 1, -1
                 if( nounit ) then
                    a( j, j ) = cone / a( j, j )
                    ajj = -a( j, j )
                 else
                    ajj = -cone
                 end if
                 if( j<n ) then
                    ! compute elements j+1:n of j-th column.
                    call stdlib_ztrmv( 'LOWER', 'NO TRANSPOSE', diag, n-j,a( j+1, j+1 ), lda, a( &
                              j+1, j ), 1_ilp )
                    call stdlib_zscal( n-j, ajj, a( j+1, j ), 1_ilp )
                 end if
              end do
           end if
           return
     end subroutine stdlib_ztrti2




     pure module subroutine stdlib_strrfs( uplo, trans, diag, n, nrhs, a, lda, b, ldb, x,ldx, ferr, berr,&
     !! STRRFS provides error bounds and backward error estimates for the
     !! solution to a system of linear equations with a triangular
     !! coefficient matrix.
     !! The solution matrix X must be computed by STRTRS or some other
     !! means before entering this routine.  STRRFS does not do iterative
     !! refinement because doing so cannot improve the backward error.
                work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), b(ldb,*), x(ldx,*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           character :: transt
           integer(ilp) :: i, j, k, kase, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STRRFS', -info )
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
           if( notran ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_250: do j = 1, nrhs
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a or a**t, depending on trans.
              call stdlib_scopy( n, x( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_strmv( uplo, trans, diag, n, a, lda, work( n+1 ), 1_ilp )
              call stdlib_saxpy( n, -one, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              if( notran ) then
                 ! compute abs(a)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = 1, k
                             work( i ) = work( i ) + abs( a( i, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = 1, k - 1
                             work( i ) = work( i ) + abs( a( i, k ) )*xk
                          end do
                          work( k ) = work( k ) + xk
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = k, n
                             work( i ) = work( i ) + abs( a( i, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = k + 1, n
                             work( i ) = work( i ) + abs( a( i, k ) )*xk
                          end do
                          work( k ) = work( k ) + xk
                       end do
                    end if
                 end if
              else
                 ! compute abs(a**t)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = 1, k
                             s = s + abs( a( i, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = abs( x( k, j ) )
                          do i = 1, k - 1
                             s = s + abs( a( i, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = k, n
                             s = s + abs( a( i, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = abs( x( k, j ) )
                          do i = k + 1, n
                             s = s + abs( a( i, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    end if
                 end if
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_slacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              210 continue
              call stdlib_slacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**t).
                    call stdlib_strsv( uplo, transt, diag, n, a, lda, work( n+1 ),1_ilp )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_strsv( uplo, trans, diag, n, a, lda, work( n+1 ),1_ilp )
                 end if
                 go to 210
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_250
           return
     end subroutine stdlib_strrfs

     pure module subroutine stdlib_dtrrfs( uplo, trans, diag, n, nrhs, a, lda, b, ldb, x,ldx, ferr, berr,&
     !! DTRRFS provides error bounds and backward error estimates for the
     !! solution to a system of linear equations with a triangular
     !! coefficient matrix.
     !! The solution matrix X must be computed by DTRTRS or some other
     !! means before entering this routine.  DTRRFS does not do iterative
     !! refinement because doing so cannot improve the backward error.
                work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), b(ldb,*), x(ldx,*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           character :: transt
           integer(ilp) :: i, j, k, kase, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTRRFS', -info )
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
           if( notran ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_250: do j = 1, nrhs
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a or a**t, depending on trans.
              call stdlib_dcopy( n, x( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_dtrmv( uplo, trans, diag, n, a, lda, work( n+1 ), 1_ilp )
              call stdlib_daxpy( n, -one, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              if( notran ) then
                 ! compute abs(a)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = 1, k
                             work( i ) = work( i ) + abs( a( i, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = 1, k - 1
                             work( i ) = work( i ) + abs( a( i, k ) )*xk
                          end do
                          work( k ) = work( k ) + xk
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = k, n
                             work( i ) = work( i ) + abs( a( i, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = k + 1, n
                             work( i ) = work( i ) + abs( a( i, k ) )*xk
                          end do
                          work( k ) = work( k ) + xk
                       end do
                    end if
                 end if
              else
                 ! compute abs(a**t)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = 1, k
                             s = s + abs( a( i, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = abs( x( k, j ) )
                          do i = 1, k - 1
                             s = s + abs( a( i, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = k, n
                             s = s + abs( a( i, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = abs( x( k, j ) )
                          do i = k + 1, n
                             s = s + abs( a( i, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    end if
                 end if
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_dlacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              210 continue
              call stdlib_dlacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**t).
                    call stdlib_dtrsv( uplo, transt, diag, n, a, lda, work( n+1 ),1_ilp )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_dtrsv( uplo, trans, diag, n, a, lda, work( n+1 ),1_ilp )
                 end if
                 go to 210
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_250
           return
     end subroutine stdlib_dtrrfs


     pure module subroutine stdlib_ctrrfs( uplo, trans, diag, n, nrhs, a, lda, b, ldb, x,ldx, ferr, berr,&
     !! CTRRFS provides error bounds and backward error estimates for the
     !! solution to a system of linear equations with a triangular
     !! coefficient matrix.
     !! The solution matrix X must be computed by CTRTRS or some other
     !! means before entering this routine.  CTRRFS does not do iterative
     !! refinement because doing so cannot improve the backward error.
                work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldx, n, nrhs
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: a(lda,*), b(ldb,*), x(ldx,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           character :: transn, transt
           integer(ilp) :: i, j, k, kase, nz
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
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTRRFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'C'
           else
              transn = 'C'
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_250: do j = 1, nrhs
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_ccopy( n, x( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_ctrmv( uplo, trans, diag, n, a, lda, work, 1_ilp )
              call stdlib_caxpy( n, -cone, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              if( notran ) then
                 ! compute abs(a)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = 1, k
                             rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = 1, k - 1
                             rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                          end do
                          rwork( k ) = rwork( k ) + xk
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = k, n
                             rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = k + 1, n
                             rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                          end do
                          rwork( k ) = rwork( k ) + xk
                       end do
                    end if
                 end if
              else
                 ! compute abs(a**h)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = 1, k
                             s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = cabs1( x( k, j ) )
                          do i = 1, k - 1
                             s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = k, n
                             s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = cabs1( x( k, j ) )
                          do i = k + 1, n
                             s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    end if
                 end if
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
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_clacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              210 continue
              call stdlib_clacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**h).
                    call stdlib_ctrsv( uplo, transt, diag, n, a, lda, work, 1_ilp )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_ctrsv( uplo, transn, diag, n, a, lda, work, 1_ilp )
                 end if
                 go to 210
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_250
           return
     end subroutine stdlib_ctrrfs

     pure module subroutine stdlib_ztrrfs( uplo, trans, diag, n, nrhs, a, lda, b, ldb, x,ldx, ferr, berr,&
     !! ZTRRFS provides error bounds and backward error estimates for the
     !! solution to a system of linear equations with a triangular
     !! coefficient matrix.
     !! The solution matrix X must be computed by ZTRTRS or some other
     !! means before entering this routine.  ZTRRFS does not do iterative
     !! refinement because doing so cannot improve the backward error.
                work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldx, n, nrhs
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: a(lda,*), b(ldb,*), x(ldx,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           character :: transn, transt
           integer(ilp) :: i, j, k, kase, nz
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
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTRRFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'C'
           else
              transn = 'C'
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_250: do j = 1, nrhs
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_zcopy( n, x( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_ztrmv( uplo, trans, diag, n, a, lda, work, 1_ilp )
              call stdlib_zaxpy( n, -cone, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              if( notran ) then
                 ! compute abs(a)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = 1, k
                             rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = 1, k - 1
                             rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                          end do
                          rwork( k ) = rwork( k ) + xk
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = k, n
                             rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = k + 1, n
                             rwork( i ) = rwork( i ) + cabs1( a( i, k ) )*xk
                          end do
                          rwork( k ) = rwork( k ) + xk
                       end do
                    end if
                 end if
              else
                 ! compute abs(a**h)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = 1, k
                             s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = cabs1( x( k, j ) )
                          do i = 1, k - 1
                             s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = k, n
                             s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = cabs1( x( k, j ) )
                          do i = k + 1, n
                             s = s + cabs1( a( i, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    end if
                 end if
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
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_zlacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              210 continue
              call stdlib_zlacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**h).
                    call stdlib_ztrsv( uplo, transt, diag, n, a, lda, work, 1_ilp )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_ztrsv( uplo, transn, diag, n, a, lda, work, 1_ilp )
                 end if
                 go to 210
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_250
           return
     end subroutine stdlib_ztrrfs




     pure module subroutine stdlib_slauum( uplo, n, a, lda, info )
     !! SLAUUM computes the product U * U**T or L**T * L, where the triangular
     !! factor U or L is stored in the upper or lower triangular part of
     !! the array A.
     !! If UPLO = 'U' or 'u' then the upper triangle of the result is stored,
     !! overwriting the factor U in A.
     !! If UPLO = 'L' or 'l' then the lower triangle of the result is stored,
     !! overwriting the factor L in A.
     !! This is the blocked form of the algorithm, calling Level 3 BLAS.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, ib, nb
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
              call stdlib_xerbla( 'SLAUUM', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'SLAUUM', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_slauu2( uplo, n, a, lda, info )
           else
              ! use blocked code
              if( upper ) then
                 ! compute the product u * u**t.
                 do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    call stdlib_strmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'NON-UNIT',i-1, ib, one, a( &
                              i, i ), lda, a( 1_ilp, i ),lda )
                    call stdlib_slauu2( 'UPPER', ib, a( i, i ), lda, info )
                    if( i+ib<=n ) then
                       call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', i-1, ib,n-i-ib+1, one, a( &
                                 1_ilp, i+ib ), lda,a( i, i+ib ), lda, one, a( 1_ilp, i ), lda )
                       call stdlib_ssyrk( 'UPPER', 'NO TRANSPOSE', ib, n-i-ib+1,one, a( i, i+ib ),&
                                  lda, one, a( i, i ),lda )
                    end if
                 end do
              else
                 ! compute the product l**t * l.
                 do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    call stdlib_strmm( 'LEFT', 'LOWER', 'TRANSPOSE', 'NON-UNIT', ib,i-1, one, a( &
                              i, i ), lda, a( i, 1_ilp ), lda )
                    call stdlib_slauu2( 'LOWER', ib, a( i, i ), lda, info )
                    if( i+ib<=n ) then
                       call stdlib_sgemm( 'TRANSPOSE', 'NO TRANSPOSE', ib, i-1,n-i-ib+1, one, a( &
                                 i+ib, i ), lda,a( i+ib, 1_ilp ), lda, one, a( i, 1_ilp ), lda )
                       call stdlib_ssyrk( 'LOWER', 'TRANSPOSE', ib, n-i-ib+1, one,a( i+ib, i ), &
                                 lda, one, a( i, i ), lda )
                    end if
                 end do
              end if
           end if
           return
     end subroutine stdlib_slauum

     pure module subroutine stdlib_dlauum( uplo, n, a, lda, info )
     !! DLAUUM computes the product U * U**T or L**T * L, where the triangular
     !! factor U or L is stored in the upper or lower triangular part of
     !! the array A.
     !! If UPLO = 'U' or 'u' then the upper triangle of the result is stored,
     !! overwriting the factor U in A.
     !! If UPLO = 'L' or 'l' then the lower triangle of the result is stored,
     !! overwriting the factor L in A.
     !! This is the blocked form of the algorithm, calling Level 3 BLAS.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, ib, nb
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
              call stdlib_xerbla( 'DLAUUM', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'DLAUUM', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_dlauu2( uplo, n, a, lda, info )
           else
              ! use blocked code
              if( upper ) then
                 ! compute the product u * u**t.
                 do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'NON-UNIT',i-1, ib, one, a( &
                              i, i ), lda, a( 1_ilp, i ),lda )
                    call stdlib_dlauu2( 'UPPER', ib, a( i, i ), lda, info )
                    if( i+ib<=n ) then
                       call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', i-1, ib,n-i-ib+1, one, a( &
                                 1_ilp, i+ib ), lda,a( i, i+ib ), lda, one, a( 1_ilp, i ), lda )
                       call stdlib_dsyrk( 'UPPER', 'NO TRANSPOSE', ib, n-i-ib+1,one, a( i, i+ib ),&
                                  lda, one, a( i, i ),lda )
                    end if
                 end do
              else
                 ! compute the product l**t * l.
                 do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    call stdlib_dtrmm( 'LEFT', 'LOWER', 'TRANSPOSE', 'NON-UNIT', ib,i-1, one, a( &
                              i, i ), lda, a( i, 1_ilp ), lda )
                    call stdlib_dlauu2( 'LOWER', ib, a( i, i ), lda, info )
                    if( i+ib<=n ) then
                       call stdlib_dgemm( 'TRANSPOSE', 'NO TRANSPOSE', ib, i-1,n-i-ib+1, one, a( &
                                 i+ib, i ), lda,a( i+ib, 1_ilp ), lda, one, a( i, 1_ilp ), lda )
                       call stdlib_dsyrk( 'LOWER', 'TRANSPOSE', ib, n-i-ib+1, one,a( i+ib, i ), &
                                 lda, one, a( i, i ), lda )
                    end if
                 end do
              end if
           end if
           return
     end subroutine stdlib_dlauum


     pure module subroutine stdlib_clauum( uplo, n, a, lda, info )
     !! CLAUUM computes the product U * U**H or L**H * L, where the triangular
     !! factor U or L is stored in the upper or lower triangular part of
     !! the array A.
     !! If UPLO = 'U' or 'u' then the upper triangle of the result is stored,
     !! overwriting the factor U in A.
     !! If UPLO = 'L' or 'l' then the lower triangle of the result is stored,
     !! overwriting the factor L in A.
     !! This is the blocked form of the algorithm, calling Level 3 BLAS.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, ib, nb
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
              call stdlib_xerbla( 'CLAUUM', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'CLAUUM', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_clauu2( uplo, n, a, lda, info )
           else
              ! use blocked code
              if( upper ) then
                 ! compute the product u * u**h.
                 do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','NON-UNIT', i-1, &
                              ib, cone, a( i, i ), lda,a( 1_ilp, i ), lda )
                    call stdlib_clauu2( 'UPPER', ib, a( i, i ), lda, info )
                    if( i+ib<=n ) then
                       call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',i-1, ib, n-i-ib+1,&
                                  cone, a( 1_ilp, i+ib ),lda, a( i, i+ib ), lda, cone, a( 1_ilp, i ),lda )
                       call stdlib_cherk( 'UPPER', 'NO TRANSPOSE', ib, n-i-ib+1,one, a( i, i+ib ),&
                                  lda, one, a( i, i ),lda )
                    end if
                 end do
              else
                 ! compute the product l**h * l.
                 do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    call stdlib_ctrmm( 'LEFT', 'LOWER', 'CONJUGATE TRANSPOSE','NON-UNIT', ib, i-1,&
                               cone, a( i, i ), lda,a( i, 1_ilp ), lda )
                    call stdlib_clauu2( 'LOWER', ib, a( i, i ), lda, info )
                    if( i+ib<=n ) then
                       call stdlib_cgemm( 'CONJUGATE TRANSPOSE', 'NO TRANSPOSE', ib,i-1, n-i-ib+1,&
                                  cone, a( i+ib, i ), lda,a( i+ib, 1_ilp ), lda, cone, a( i, 1_ilp ), lda )
                       call stdlib_cherk( 'LOWER', 'CONJUGATE TRANSPOSE', ib,n-i-ib+1, one, a( i+&
                                 ib, i ), lda, one,a( i, i ), lda )
                    end if
                 end do
              end if
           end if
           return
     end subroutine stdlib_clauum

     pure module subroutine stdlib_zlauum( uplo, n, a, lda, info )
     !! ZLAUUM computes the product U * U**H or L**H * L, where the triangular
     !! factor U or L is stored in the upper or lower triangular part of
     !! the array A.
     !! If UPLO = 'U' or 'u' then the upper triangle of the result is stored,
     !! overwriting the factor U in A.
     !! If UPLO = 'L' or 'l' then the lower triangle of the result is stored,
     !! overwriting the factor L in A.
     !! This is the blocked form of the algorithm, calling Level 3 BLAS.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, ib, nb
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
              call stdlib_xerbla( 'ZLAUUM', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'ZLAUUM', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_zlauu2( uplo, n, a, lda, info )
           else
              ! use blocked code
              if( upper ) then
                 ! compute the product u * u**h.
                 do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','NON-UNIT', i-1, &
                              ib, cone, a( i, i ), lda,a( 1_ilp, i ), lda )
                    call stdlib_zlauu2( 'UPPER', ib, a( i, i ), lda, info )
                    if( i+ib<=n ) then
                       call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',i-1, ib, n-i-ib+1,&
                                  cone, a( 1_ilp, i+ib ),lda, a( i, i+ib ), lda, cone, a( 1_ilp, i ),lda )
                       call stdlib_zherk( 'UPPER', 'NO TRANSPOSE', ib, n-i-ib+1,one, a( i, i+ib ),&
                                  lda, one, a( i, i ),lda )
                    end if
                 end do
              else
                 ! compute the product l**h * l.
                 do i = 1, n, nb
                    ib = min( nb, n-i+1 )
                    call stdlib_ztrmm( 'LEFT', 'LOWER', 'CONJUGATE TRANSPOSE','NON-UNIT', ib, i-1,&
                               cone, a( i, i ), lda,a( i, 1_ilp ), lda )
                    call stdlib_zlauu2( 'LOWER', ib, a( i, i ), lda, info )
                    if( i+ib<=n ) then
                       call stdlib_zgemm( 'CONJUGATE TRANSPOSE', 'NO TRANSPOSE', ib,i-1, n-i-ib+1,&
                                  cone, a( i+ib, i ), lda,a( i+ib, 1_ilp ), lda, cone, a( i, 1_ilp ), lda )
                       call stdlib_zherk( 'LOWER', 'CONJUGATE TRANSPOSE', ib,n-i-ib+1, one, a( i+&
                                 ib, i ), lda, one,a( i, i ), lda )
                    end if
                 end do
              end if
           end if
           return
     end subroutine stdlib_zlauum




     pure module subroutine stdlib_slauu2( uplo, n, a, lda, info )
     !! SLAUU2 computes the product U * U**T or L**T * L, where the triangular
     !! factor U or L is stored in the upper or lower triangular part of
     !! the array A.
     !! If UPLO = 'U' or 'u' then the upper triangle of the result is stored,
     !! overwriting the factor U in A.
     !! If UPLO = 'L' or 'l' then the lower triangle of the result is stored,
     !! overwriting the factor L in A.
     !! This is the unblocked form of the algorithm, calling Level 2 BLAS.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           real(sp) :: aii
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
              call stdlib_xerbla( 'SLAUU2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! compute the product u * u**t.
              do i = 1, n
                 aii = a( i, i )
                 if( i<n ) then
                    a( i, i ) = stdlib_sdot( n-i+1, a( i, i ), lda, a( i, i ), lda )
                    call stdlib_sgemv( 'NO TRANSPOSE', i-1, n-i, one, a( 1_ilp, i+1 ),lda, a( i, i+1 )&
                              , lda, aii, a( 1_ilp, i ), 1_ilp )
                 else
                    call stdlib_sscal( i, aii, a( 1_ilp, i ), 1_ilp )
                 end if
              end do
           else
              ! compute the product l**t * l.
              do i = 1, n
                 aii = a( i, i )
                 if( i<n ) then
                    a( i, i ) = stdlib_sdot( n-i+1, a( i, i ), 1_ilp, a( i, i ), 1_ilp )
                    call stdlib_sgemv( 'TRANSPOSE', n-i, i-1, one, a( i+1, 1_ilp ), lda,a( i+1, i ), &
                              1_ilp, aii, a( i, 1_ilp ), lda )
                 else
                    call stdlib_sscal( i, aii, a( i, 1_ilp ), lda )
                 end if
              end do
           end if
           return
     end subroutine stdlib_slauu2

     pure module subroutine stdlib_dlauu2( uplo, n, a, lda, info )
     !! DLAUU2 computes the product U * U**T or L**T * L, where the triangular
     !! factor U or L is stored in the upper or lower triangular part of
     !! the array A.
     !! If UPLO = 'U' or 'u' then the upper triangle of the result is stored,
     !! overwriting the factor U in A.
     !! If UPLO = 'L' or 'l' then the lower triangle of the result is stored,
     !! overwriting the factor L in A.
     !! This is the unblocked form of the algorithm, calling Level 2 BLAS.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           real(dp) :: aii
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
              call stdlib_xerbla( 'DLAUU2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! compute the product u * u**t.
              do i = 1, n
                 aii = a( i, i )
                 if( i<n ) then
                    a( i, i ) = stdlib_ddot( n-i+1, a( i, i ), lda, a( i, i ), lda )
                    call stdlib_dgemv( 'NO TRANSPOSE', i-1, n-i, one, a( 1_ilp, i+1 ),lda, a( i, i+1 )&
                              , lda, aii, a( 1_ilp, i ), 1_ilp )
                 else
                    call stdlib_dscal( i, aii, a( 1_ilp, i ), 1_ilp )
                 end if
              end do
           else
              ! compute the product l**t * l.
              do i = 1, n
                 aii = a( i, i )
                 if( i<n ) then
                    a( i, i ) = stdlib_ddot( n-i+1, a( i, i ), 1_ilp, a( i, i ), 1_ilp )
                    call stdlib_dgemv( 'TRANSPOSE', n-i, i-1, one, a( i+1, 1_ilp ), lda,a( i+1, i ), &
                              1_ilp, aii, a( i, 1_ilp ), lda )
                 else
                    call stdlib_dscal( i, aii, a( i, 1_ilp ), lda )
                 end if
              end do
           end if
           return
     end subroutine stdlib_dlauu2


     pure module subroutine stdlib_clauu2( uplo, n, a, lda, info )
     !! CLAUU2 computes the product U * U**H or L**H * L, where the triangular
     !! factor U or L is stored in the upper or lower triangular part of
     !! the array A.
     !! If UPLO = 'U' or 'u' then the upper triangle of the result is stored,
     !! overwriting the factor U in A.
     !! If UPLO = 'L' or 'l' then the lower triangle of the result is stored,
     !! overwriting the factor L in A.
     !! This is the unblocked form of the algorithm, calling Level 2 BLAS.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           real(sp) :: aii
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
              call stdlib_xerbla( 'CLAUU2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! compute the product u * u**h.
              do i = 1, n
                 aii = real( a( i, i ),KIND=sp)
                 if( i<n ) then
                    a( i, i ) = aii*aii + real( stdlib_cdotc( n-i, a( i, i+1 ), lda,a( i, i+1 ), &
                              lda ),KIND=sp)
                    call stdlib_clacgv( n-i, a( i, i+1 ), lda )
                    call stdlib_cgemv( 'NO TRANSPOSE', i-1, n-i, cone, a( 1_ilp, i+1 ),lda, a( i, i+1 &
                              ), lda, cmplx( aii,KIND=sp),a( 1_ilp, i ), 1_ilp )
                    call stdlib_clacgv( n-i, a( i, i+1 ), lda )
                 else
                    call stdlib_csscal( i, aii, a( 1_ilp, i ), 1_ilp )
                 end if
              end do
           else
              ! compute the product l**h * l.
              do i = 1, n
                 aii = real( a( i, i ),KIND=sp)
                 if( i<n ) then
                    a( i, i ) = aii*aii + real( stdlib_cdotc( n-i, a( i+1, i ), 1_ilp,a( i+1, i ), 1_ilp )&
                              ,KIND=sp)
                    call stdlib_clacgv( i-1, a( i, 1_ilp ), lda )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-i, i-1, cone,a( i+1, 1_ilp ), lda, a( &
                              i+1, i ), 1_ilp,cmplx( aii,KIND=sp), a( i, 1_ilp ), lda )
                    call stdlib_clacgv( i-1, a( i, 1_ilp ), lda )
                 else
                    call stdlib_csscal( i, aii, a( i, 1_ilp ), lda )
                 end if
              end do
           end if
           return
     end subroutine stdlib_clauu2

     pure module subroutine stdlib_zlauu2( uplo, n, a, lda, info )
     !! ZLAUU2 computes the product U * U**H or L**H * L, where the triangular
     !! factor U or L is stored in the upper or lower triangular part of
     !! the array A.
     !! If UPLO = 'U' or 'u' then the upper triangle of the result is stored,
     !! overwriting the factor U in A.
     !! If UPLO = 'L' or 'l' then the lower triangle of the result is stored,
     !! overwriting the factor L in A.
     !! This is the unblocked form of the algorithm, calling Level 2 BLAS.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           real(dp) :: aii
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
              call stdlib_xerbla( 'ZLAUU2', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! compute the product u * u**h.
              do i = 1, n
                 aii = real( a( i, i ),KIND=dp)
                 if( i<n ) then
                    a( i, i ) = aii*aii + real( stdlib_zdotc( n-i, a( i, i+1 ), lda,a( i, i+1 ), &
                              lda ),KIND=dp)
                    call stdlib_zlacgv( n-i, a( i, i+1 ), lda )
                    call stdlib_zgemv( 'NO TRANSPOSE', i-1, n-i, cone, a( 1_ilp, i+1 ),lda, a( i, i+1 &
                              ), lda, cmplx( aii,KIND=dp),a( 1_ilp, i ), 1_ilp )
                    call stdlib_zlacgv( n-i, a( i, i+1 ), lda )
                 else
                    call stdlib_zdscal( i, aii, a( 1_ilp, i ), 1_ilp )
                 end if
              end do
           else
              ! compute the product l**h * l.
              do i = 1, n
                 aii = real( a( i, i ),KIND=dp)
                 if( i<n ) then
                    a( i, i ) = aii*aii + real( stdlib_zdotc( n-i, a( i+1, i ), 1_ilp,a( i+1, i ), 1_ilp )&
                              ,KIND=dp)
                    call stdlib_zlacgv( i-1, a( i, 1_ilp ), lda )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-i, i-1, cone,a( i+1, 1_ilp ), lda, a( &
                              i+1, i ), 1_ilp,cmplx( aii,KIND=dp), a( i, 1_ilp ), lda )
                    call stdlib_zlacgv( i-1, a( i, 1_ilp ), lda )
                 else
                    call stdlib_zdscal( i, aii, a( i, 1_ilp ), lda )
                 end if
              end do
           end if
           return
     end subroutine stdlib_zlauu2




     module subroutine stdlib_stpcon( norm, uplo, diag, n, ap, rcond, work, iwork,info )
     !! STPCON estimates the reciprocal of the condition number of a packed
     !! triangular matrix A, in either the 1-norm or the infinity-norm.
     !! The norm of A is computed and an estimate is obtained for
     !! norm(inv(A)), then the reciprocal of the condition number is
     !! computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: ap(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, onenrm, upper
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(sp) :: ainvnm, anorm, scale, smlnum, xnorm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STPCON', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              rcond = one
              return
           end if
           rcond = zero
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )*real( max( 1_ilp, n ),KIND=sp)
           ! compute the norm of the triangular matrix a.
           anorm = stdlib_slantp( norm, uplo, diag, n, ap, work )
           ! continue only if anorm > 0.
           if( anorm>zero ) then
              ! estimate the norm of the inverse of a.
              ainvnm = zero
              normin = 'N'
              if( onenrm ) then
                 kase1 = 1_ilp
              else
                 kase1 = 2_ilp
              end if
              kase = 0_ilp
              10 continue
              call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==kase1 ) then
                    ! multiply by inv(a).
                    call stdlib_slatps( uplo, 'NO TRANSPOSE', diag, normin, n, ap,work, scale, &
                              work( 2_ilp*n+1 ), info )
                 else
                    ! multiply by inv(a**t).
                    call stdlib_slatps( uplo, 'TRANSPOSE', diag, normin, n, ap,work, scale, work( &
                              2_ilp*n+1 ), info )
                 end if
                 normin = 'Y'
                 ! multiply by 1/scale if doing so will not cause overflow.
                 if( scale/=one ) then
                    ix = stdlib_isamax( n, work, 1_ilp )
                    xnorm = abs( work( ix ) )
                    if( scale<xnorm*smlnum .or. scale==zero )go to 20
                    call stdlib_srscl( n, scale, work, 1_ilp )
                 end if
                 go to 10
              end if
              ! compute the estimate of the reciprocal condition number.
              if( ainvnm/=zero )rcond = ( one / anorm ) / ainvnm
           end if
           20 continue
           return
     end subroutine stdlib_stpcon

     module subroutine stdlib_dtpcon( norm, uplo, diag, n, ap, rcond, work, iwork,info )
     !! DTPCON estimates the reciprocal of the condition number of a packed
     !! triangular matrix A, in either the 1-norm or the infinity-norm.
     !! The norm of A is computed and an estimate is obtained for
     !! norm(inv(A)), then the reciprocal of the condition number is
     !! computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: ap(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, onenrm, upper
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(dp) :: ainvnm, anorm, scale, smlnum, xnorm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTPCON', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              rcond = one
              return
           end if
           rcond = zero
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )*real( max( 1_ilp, n ),KIND=dp)
           ! compute the norm of the triangular matrix a.
           anorm = stdlib_dlantp( norm, uplo, diag, n, ap, work )
           ! continue only if anorm > 0.
           if( anorm>zero ) then
              ! estimate the norm of the inverse of a.
              ainvnm = zero
              normin = 'N'
              if( onenrm ) then
                 kase1 = 1_ilp
              else
                 kase1 = 2_ilp
              end if
              kase = 0_ilp
              10 continue
              call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==kase1 ) then
                    ! multiply by inv(a).
                    call stdlib_dlatps( uplo, 'NO TRANSPOSE', diag, normin, n, ap,work, scale, &
                              work( 2_ilp*n+1 ), info )
                 else
                    ! multiply by inv(a**t).
                    call stdlib_dlatps( uplo, 'TRANSPOSE', diag, normin, n, ap,work, scale, work( &
                              2_ilp*n+1 ), info )
                 end if
                 normin = 'Y'
                 ! multiply by 1/scale if doing so will not cause overflow.
                 if( scale/=one ) then
                    ix = stdlib_idamax( n, work, 1_ilp )
                    xnorm = abs( work( ix ) )
                    if( scale<xnorm*smlnum .or. scale==zero )go to 20
                    call stdlib_drscl( n, scale, work, 1_ilp )
                 end if
                 go to 10
              end if
              ! compute the estimate of the reciprocal condition number.
              if( ainvnm/=zero )rcond = ( one / anorm ) / ainvnm
           end if
           20 continue
           return
     end subroutine stdlib_dtpcon


     module subroutine stdlib_ctpcon( norm, uplo, diag, n, ap, rcond, work, rwork,info )
     !! CTPCON estimates the reciprocal of the condition number of a packed
     !! triangular matrix A, in either the 1-norm or the infinity-norm.
     !! The norm of A is computed and an estimate is obtained for
     !! norm(inv(A)), then the reciprocal of the condition number is
     !! computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, onenrm, upper
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(sp) :: ainvnm, anorm, scale, smlnum, xnorm
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
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTPCON', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              rcond = one
              return
           end if
           rcond = zero
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )*real( max( 1_ilp, n ),KIND=sp)
           ! compute the norm of the triangular matrix a.
           anorm = stdlib_clantp( norm, uplo, diag, n, ap, rwork )
           ! continue only if anorm > 0.
           if( anorm>zero ) then
              ! estimate the norm of the inverse of a.
              ainvnm = zero
              normin = 'N'
              if( onenrm ) then
                 kase1 = 1_ilp
              else
                 kase1 = 2_ilp
              end if
              kase = 0_ilp
              10 continue
              call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==kase1 ) then
                    ! multiply by inv(a).
                    call stdlib_clatps( uplo, 'NO TRANSPOSE', diag, normin, n, ap,work, scale, &
                              rwork, info )
                 else
                    ! multiply by inv(a**h).
                    call stdlib_clatps( uplo, 'CONJUGATE TRANSPOSE', diag, normin,n, ap, work, &
                              scale, rwork, info )
                 end if
                 normin = 'Y'
                 ! multiply by 1/scale if doing so will not cause overflow.
                 if( scale/=one ) then
                    ix = stdlib_icamax( n, work, 1_ilp )
                    xnorm = cabs1( work( ix ) )
                    if( scale<xnorm*smlnum .or. scale==zero )go to 20
                    call stdlib_csrscl( n, scale, work, 1_ilp )
                 end if
                 go to 10
              end if
              ! compute the estimate of the reciprocal condition number.
              if( ainvnm/=zero )rcond = ( one / anorm ) / ainvnm
           end if
           20 continue
           return
     end subroutine stdlib_ctpcon

     module subroutine stdlib_ztpcon( norm, uplo, diag, n, ap, rcond, work, rwork,info )
     !! ZTPCON estimates the reciprocal of the condition number of a packed
     !! triangular matrix A, in either the 1-norm or the infinity-norm.
     !! The norm of A is computed and an estimate is obtained for
     !! norm(inv(A)), then the reciprocal of the condition number is
     !! computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, onenrm, upper
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(dp) :: ainvnm, anorm, scale, smlnum, xnorm
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
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTPCON', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              rcond = one
              return
           end if
           rcond = zero
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )*real( max( 1_ilp, n ),KIND=dp)
           ! compute the norm of the triangular matrix a.
           anorm = stdlib_zlantp( norm, uplo, diag, n, ap, rwork )
           ! continue only if anorm > 0.
           if( anorm>zero ) then
              ! estimate the norm of the inverse of a.
              ainvnm = zero
              normin = 'N'
              if( onenrm ) then
                 kase1 = 1_ilp
              else
                 kase1 = 2_ilp
              end if
              kase = 0_ilp
              10 continue
              call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==kase1 ) then
                    ! multiply by inv(a).
                    call stdlib_zlatps( uplo, 'NO TRANSPOSE', diag, normin, n, ap,work, scale, &
                              rwork, info )
                 else
                    ! multiply by inv(a**h).
                    call stdlib_zlatps( uplo, 'CONJUGATE TRANSPOSE', diag, normin,n, ap, work, &
                              scale, rwork, info )
                 end if
                 normin = 'Y'
                 ! multiply by 1/scale if doing so will not cause overflow.
                 if( scale/=one ) then
                    ix = stdlib_izamax( n, work, 1_ilp )
                    xnorm = cabs1( work( ix ) )
                    if( scale<xnorm*smlnum .or. scale==zero )go to 20
                    call stdlib_zdrscl( n, scale, work, 1_ilp )
                 end if
                 go to 10
              end if
              ! compute the estimate of the reciprocal condition number.
              if( ainvnm/=zero )rcond = ( one / anorm ) / ainvnm
           end if
           20 continue
           return
     end subroutine stdlib_ztpcon




     pure module subroutine stdlib_stptrs( uplo, trans, diag, n, nrhs, ap, b, ldb, info )
     !! STPTRS solves a triangular system of the form
     !! A * X = B  or  A**T * X = B,
     !! where A is a triangular matrix of order N stored in packed format,
     !! and B is an N-by-NRHS matrix.  A check is made to verify that A is
     !! nonsingular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: ap(*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j, jc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) .and. &
                     .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity.
           if( nounit ) then
              if( upper ) then
                 jc = 1_ilp
                 do info = 1, n
                    if( ap( jc+info-1 )==zero )return
                    jc = jc + info
                 end do
              else
                 jc = 1_ilp
                 do info = 1, n
                    if( ap( jc )==zero )return
                    jc = jc + n - info + 1_ilp
                 end do
              end if
           end if
           info = 0_ilp
           ! solve a * x = b  or  a**t * x = b.
           do j = 1, nrhs
              call stdlib_stpsv( uplo, trans, diag, n, ap, b( 1_ilp, j ), 1_ilp )
           end do
           return
     end subroutine stdlib_stptrs

     pure module subroutine stdlib_dtptrs( uplo, trans, diag, n, nrhs, ap, b, ldb, info )
     !! DTPTRS solves a triangular system of the form
     !! A * X = B  or  A**T * X = B,
     !! where A is a triangular matrix of order N stored in packed format,
     !! and B is an N-by-NRHS matrix.  A check is made to verify that A is
     !! nonsingular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: ap(*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j, jc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) .and. &
                     .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity.
           if( nounit ) then
              if( upper ) then
                 jc = 1_ilp
                 do info = 1, n
                    if( ap( jc+info-1 )==zero )return
                    jc = jc + info
                 end do
              else
                 jc = 1_ilp
                 do info = 1, n
                    if( ap( jc )==zero )return
                    jc = jc + n - info + 1_ilp
                 end do
              end if
           end if
           info = 0_ilp
           ! solve a * x = b  or  a**t * x = b.
           do j = 1, nrhs
              call stdlib_dtpsv( uplo, trans, diag, n, ap, b( 1_ilp, j ), 1_ilp )
           end do
           return
     end subroutine stdlib_dtptrs


     pure module subroutine stdlib_ctptrs( uplo, trans, diag, n, nrhs, ap, b, ldb, info )
     !! CTPTRS solves a triangular system of the form
     !! A * X = B,  A**T * X = B,  or  A**H * X = B,
     !! where A is a triangular matrix of order N stored in packed format,
     !! and B is an N-by-NRHS matrix.  A check is made to verify that A is
     !! nonsingular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j, jc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) .and. &
                     .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity.
           if( nounit ) then
              if( upper ) then
                 jc = 1_ilp
                 do info = 1, n
                    if( ap( jc+info-1 )==czero )return
                    jc = jc + info
                 end do
              else
                 jc = 1_ilp
                 do info = 1, n
                    if( ap( jc )==czero )return
                    jc = jc + n - info + 1_ilp
                 end do
              end if
           end if
           info = 0_ilp
           ! solve  a * x = b,  a**t * x = b,  or  a**h * x = b.
           do j = 1, nrhs
              call stdlib_ctpsv( uplo, trans, diag, n, ap, b( 1_ilp, j ), 1_ilp )
           end do
           return
     end subroutine stdlib_ctptrs

     pure module subroutine stdlib_ztptrs( uplo, trans, diag, n, nrhs, ap, b, ldb, info )
     !! ZTPTRS solves a triangular system of the form
     !! A * X = B,  A**T * X = B,  or  A**H * X = B,
     !! where A is a triangular matrix of order N stored in packed format,
     !! and B is an N-by-NRHS matrix.  A check is made to verify that A is
     !! nonsingular.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j, jc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) .and. &
                     .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTPTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity.
           if( nounit ) then
              if( upper ) then
                 jc = 1_ilp
                 do info = 1, n
                    if( ap( jc+info-1 )==czero )return
                    jc = jc + info
                 end do
              else
                 jc = 1_ilp
                 do info = 1, n
                    if( ap( jc )==czero )return
                    jc = jc + n - info + 1_ilp
                 end do
              end if
           end if
           info = 0_ilp
           ! solve  a * x = b,  a**t * x = b,  or  a**h * x = b.
           do j = 1, nrhs
              call stdlib_ztpsv( uplo, trans, diag, n, ap, b( 1_ilp, j ), 1_ilp )
           end do
           return
     end subroutine stdlib_ztptrs




     pure module subroutine stdlib_slatps( uplo, trans, diag, normin, n, ap, x, scale,cnorm, info )
     !! SLATPS solves one of the triangular systems
     !! A *x = s*b  or  A**T*x = s*b
     !! with scaling to prevent overflow, where A is an upper or lower
     !! triangular matrix stored in packed form.  Here A**T denotes the
     !! transpose of A, x and b are n-element vectors, and s is a scaling
     !! factor, usually less than or equal to 1, chosen so that the
     !! components of x will be less than the overflow threshold.  If the
     !! unscaled problem will not cause overflow, the Level 2 BLAS routine
     !! STPSV is called. If the matrix A is singular (A(j,j) = 0 for some j),
     !! then s is set to 0 and a non-trivial solution to A*x = 0 is returned.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, normin, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: scale
           ! Array Arguments 
           real(sp), intent(in) :: ap(*)
           real(sp), intent(inout) :: cnorm(*), x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           integer(ilp) :: i, imax, ip, j, jfirst, jinc, jlast, jlen
           real(sp) :: bignum, grow, rec, smlnum, sumj, tjj, tjjs, tmax, tscal, uscal, xbnd, xj, &
                     xmax
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           ! test the input parameters.
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( normin, 'Y' ) .and. .not.stdlib_lsame( normin, 'N' ) ) &
                     then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLATPS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine machine dependent parameters to control overflow.
           smlnum = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           bignum = one / smlnum
           scale = one
           if( stdlib_lsame( normin, 'N' ) ) then
              ! compute the 1-norm of each column, not including the diagonal.
              if( upper ) then
                 ! a is upper triangular.
                 ip = 1_ilp
                 do j = 1, n
                    cnorm( j ) = stdlib_sasum( j-1, ap( ip ), 1_ilp )
                    ip = ip + j
                 end do
              else
                 ! a is lower triangular.
                 ip = 1_ilp
                 do j = 1, n - 1
                    cnorm( j ) = stdlib_sasum( n-j, ap( ip+1 ), 1_ilp )
                    ip = ip + n - j + 1_ilp
                 end do
                 cnorm( n ) = zero
              end if
           end if
           ! scale the column norms by tscal if the maximum element in cnorm is
           ! greater than bignum.
           imax = stdlib_isamax( n, cnorm, 1_ilp )
           tmax = cnorm( imax )
           if( tmax<=bignum ) then
              tscal = one
           else
              tscal = one / ( smlnum*tmax )
              call stdlib_sscal( n, tscal, cnorm, 1_ilp )
           end if
           ! compute a bound on the computed solution vector to see if the
           ! level 2 blas routine stdlib_stpsv can be used.
           j = stdlib_isamax( n, x, 1_ilp )
           xmax = abs( x( j ) )
           xbnd = xmax
           if( notran ) then
              ! compute the growth in a * x = b.
              if( upper ) then
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              else
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 50
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, g(0) = max{x(i), i=1,...,n}.
                 grow = one / max( xbnd, smlnum )
                 xbnd = grow
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = n
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 50
                    ! m(j) = g(j-1) / abs(a(j,j))
                    tjj = abs( ap( ip ) )
                    xbnd = min( xbnd, min( one, tjj )*grow )
                    if( tjj+cnorm( j )>=smlnum ) then
                       ! g(j) = g(j-1)*( 1 + cnorm(j) / abs(a(j,j)) )
                       grow = grow*( tjj / ( tjj+cnorm( j ) ) )
                    else
                       ! g(j) could overflow, set grow to 0.
                       grow = zero
                    end if
                    ip = ip + jinc*jlen
                    jlen = jlen - 1_ilp
                 end do
                 grow = xbnd
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, one / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 50
                    ! g(j) = g(j-1)*( 1 + cnorm(j) )
                    grow = grow*( one / ( one+cnorm( j ) ) )
                 end do
              end if
              50 continue
           else
              ! compute the growth in a**t * x = b.
              if( upper ) then
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              else
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 80
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, m(0) = max{x(i), i=1,...,n}.
                 grow = one / max( xbnd, smlnum )
                 xbnd = grow
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = 1_ilp
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 80
                    ! g(j) = max( g(j-1), m(j-1)*( 1 + cnorm(j) ) )
                    xj = one + cnorm( j )
                    grow = min( grow, xbnd / xj )
                    ! m(j) = m(j-1)*( 1 + cnorm(j) ) / abs(a(j,j))
                    tjj = abs( ap( ip ) )
                    if( xj>tjj )xbnd = xbnd*( tjj / xj )
                    jlen = jlen + 1_ilp
                    ip = ip + jinc*jlen
                 end do
                 grow = min( grow, xbnd )
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, one / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 80
                    ! g(j) = ( 1 + cnorm(j) )*g(j-1)
                    xj = one + cnorm( j )
                    grow = grow / xj
                 end do
              end if
              80 continue
           end if
           if( ( grow*tscal )>smlnum ) then
              ! use the level 2 blas solve if the reciprocal of the bound on
              ! elements of x is not too small.
              call stdlib_stpsv( uplo, trans, diag, n, ap, x, 1_ilp )
           else
              ! use a level 1 blas solve, scaling intermediate results.
              if( xmax>bignum ) then
                 ! scale x so that its components are less than or equal to
                 ! bignum in absolute value.
                 scale = bignum / xmax
                 call stdlib_sscal( n, scale, x, 1_ilp )
                 xmax = bignum
              end if
              if( notran ) then
                 ! solve a * x = b
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 loop_100: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) / a(j,j), scaling x if necessary.
                    xj = abs( x( j ) )
                    if( nounit ) then
                       tjjs = ap( ip )*tscal
                    else
                       tjjs = tscal
                       if( tscal==one )go to 95
                    end if
                       tjj = abs( tjjs )
                       if( tjj>smlnum ) then
                          ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                ! scale x by 1/b(j).
                                rec = one / xj
                                call stdlib_sscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = x( j ) / tjjs
                          xj = abs( x( j ) )
                       else if( tjj>zero ) then
                          ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                             ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum
                             ! to avoid overflow when dividing by a(j,j).
                             rec = ( tjj*bignum ) / xj
                             if( cnorm( j )>one ) then
                                ! scale by 1/cnorm(j) to avoid overflow when
                                ! multiplying x(j) times column j.
                                rec = rec / cnorm( j )
                             end if
                             call stdlib_sscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = x( j ) / tjjs
                          xj = abs( x( j ) )
                       else
                          ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                          ! scale = 0, and compute a solution to a*x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          xj = one
                          scale = zero
                          xmax = zero
                       end if
                       95 continue
                    ! scale x if necessary to avoid overflow when adding a
                    ! multiple of column j of a.
                    if( xj>one ) then
                       rec = one / xj
                       if( cnorm( j )>( bignum-xmax )*rec ) then
                          ! scale x by 1/(2*abs(x(j))).
                          rec = rec*half
                          call stdlib_sscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                       end if
                    else if( xj*cnorm( j )>( bignum-xmax ) ) then
                       ! scale x by 1/2.
                       call stdlib_sscal( n, half, x, 1_ilp )
                       scale = scale*half
                    end if
                    if( upper ) then
                       if( j>1_ilp ) then
                          ! compute the update
                             ! x(1:j-1) := x(1:j-1) - x(j) * a(1:j-1,j)
                          call stdlib_saxpy( j-1, -x( j )*tscal, ap( ip-j+1 ), 1_ilp, x,1_ilp )
                          i = stdlib_isamax( j-1, x, 1_ilp )
                          xmax = abs( x( i ) )
                       end if
                       ip = ip - j
                    else
                       if( j<n ) then
                          ! compute the update
                             ! x(j+1:n) := x(j+1:n) - x(j) * a(j+1:n,j)
                          call stdlib_saxpy( n-j, -x( j )*tscal, ap( ip+1 ), 1_ilp,x( j+1 ), 1_ilp )
                                    
                          i = j + stdlib_isamax( n-j, x( j+1 ), 1_ilp )
                          xmax = abs( x( i ) )
                       end if
                       ip = ip + n - j + 1_ilp
                    end if
                 end do loop_100
              else
                 ! solve a**t * x = b
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = 1_ilp
                 loop_140: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = abs( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = ap( ip )*tscal
                       else
                          tjjs = tscal
                       end if
                          tjj = abs( tjjs )
                          if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                             rec = min( one, rec*tjj )
                             uscal = uscal / tjjs
                          end if
                       if( rec<one ) then
                          call stdlib_sscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    sumj = zero
                    if( uscal==one ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_sdot to perform the dot product.
                       if( upper ) then
                          sumj = stdlib_sdot( j-1, ap( ip-j+1 ), 1_ilp, x, 1_ilp )
                       else if( j<n ) then
                          sumj = stdlib_sdot( n-j, ap( ip+1 ), 1_ilp, x( j+1 ), 1_ilp )
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          do i = 1, j - 1
                             sumj = sumj + ( ap( ip-j+i )*uscal )*x( i )
                          end do
                       else if( j<n ) then
                          do i = 1, n - j
                             sumj = sumj + ( ap( ip+i )*uscal )*x( j+i )
                          end do
                       end if
                    end if
                    if( uscal==tscal ) then
                       ! compute x(j) := ( x(j) - sumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - sumj
                       xj = abs( x( j ) )
                       if( nounit ) then
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjjs = ap( ip )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 135
                       end if
                          tjj = abs( tjjs )
                          if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                             if( tjj<one ) then
                                if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                   rec = one / xj
                                   call stdlib_sscal( n, rec, x, 1_ilp )
                                   scale = scale*rec
                                   xmax = xmax*rec
                                end if
                             end if
                             x( j ) = x( j ) / tjjs
                          else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                             if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                                rec = ( tjj*bignum ) / xj
                                call stdlib_sscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                             x( j ) = x( j ) / tjjs
                          else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0, and compute a solution to a**t*x = 0.
                             do i = 1, n
                                x( i ) = zero
                             end do
                             x( j ) = one
                             scale = zero
                             xmax = zero
                          end if
                          135 continue
                    else
                       ! compute x(j) := x(j) / a(j,j)  - sumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = x( j ) / tjjs - sumj
                    end if
                    xmax = max( xmax, abs( x( j ) ) )
                    jlen = jlen + 1_ilp
                    ip = ip + jinc*jlen
                 end do loop_140
              end if
              scale = scale / tscal
           end if
           ! scale the column norms by 1/tscal for return.
           if( tscal/=one ) then
              call stdlib_sscal( n, one / tscal, cnorm, 1_ilp )
           end if
           return
     end subroutine stdlib_slatps

     pure module subroutine stdlib_dlatps( uplo, trans, diag, normin, n, ap, x, scale,cnorm, info )
     !! DLATPS solves one of the triangular systems
     !! A *x = s*b  or  A**T*x = s*b
     !! with scaling to prevent overflow, where A is an upper or lower
     !! triangular matrix stored in packed form.  Here A**T denotes the
     !! transpose of A, x and b are n-element vectors, and s is a scaling
     !! factor, usually less than or equal to 1, chosen so that the
     !! components of x will be less than the overflow threshold.  If the
     !! unscaled problem will not cause overflow, the Level 2 BLAS routine
     !! DTPSV is called. If the matrix A is singular (A(j,j) = 0 for some j),
     !! then s is set to 0 and a non-trivial solution to A*x = 0 is returned.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, normin, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: scale
           ! Array Arguments 
           real(dp), intent(in) :: ap(*)
           real(dp), intent(inout) :: cnorm(*), x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           integer(ilp) :: i, imax, ip, j, jfirst, jinc, jlast, jlen
           real(dp) :: bignum, grow, rec, smlnum, sumj, tjj, tjjs, tmax, tscal, uscal, xbnd, xj, &
                     xmax
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           ! test the input parameters.
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( normin, 'Y' ) .and. .not.stdlib_lsame( normin, 'N' ) ) &
                     then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLATPS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine machine dependent parameters to control overflow.
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           bignum = one / smlnum
           scale = one
           if( stdlib_lsame( normin, 'N' ) ) then
              ! compute the 1-norm of each column, not including the diagonal.
              if( upper ) then
                 ! a is upper triangular.
                 ip = 1_ilp
                 do j = 1, n
                    cnorm( j ) = stdlib_dasum( j-1, ap( ip ), 1_ilp )
                    ip = ip + j
                 end do
              else
                 ! a is lower triangular.
                 ip = 1_ilp
                 do j = 1, n - 1
                    cnorm( j ) = stdlib_dasum( n-j, ap( ip+1 ), 1_ilp )
                    ip = ip + n - j + 1_ilp
                 end do
                 cnorm( n ) = zero
              end if
           end if
           ! scale the column norms by tscal if the maximum element in cnorm is
           ! greater than bignum.
           imax = stdlib_idamax( n, cnorm, 1_ilp )
           tmax = cnorm( imax )
           if( tmax<=bignum ) then
              tscal = one
           else
              tscal = one / ( smlnum*tmax )
              call stdlib_dscal( n, tscal, cnorm, 1_ilp )
           end if
           ! compute a bound on the computed solution vector to see if the
           ! level 2 blas routine stdlib_dtpsv can be used.
           j = stdlib_idamax( n, x, 1_ilp )
           xmax = abs( x( j ) )
           xbnd = xmax
           if( notran ) then
              ! compute the growth in a * x = b.
              if( upper ) then
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              else
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 50
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, g(0) = max{x(i), i=1,...,n}.
                 grow = one / max( xbnd, smlnum )
                 xbnd = grow
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = n
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 50
                    ! m(j) = g(j-1) / abs(a(j,j))
                    tjj = abs( ap( ip ) )
                    xbnd = min( xbnd, min( one, tjj )*grow )
                    if( tjj+cnorm( j )>=smlnum ) then
                       ! g(j) = g(j-1)*( 1 + cnorm(j) / abs(a(j,j)) )
                       grow = grow*( tjj / ( tjj+cnorm( j ) ) )
                    else
                       ! g(j) could overflow, set grow to 0.
                       grow = zero
                    end if
                    ip = ip + jinc*jlen
                    jlen = jlen - 1_ilp
                 end do
                 grow = xbnd
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, one / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 50
                    ! g(j) = g(j-1)*( 1 + cnorm(j) )
                    grow = grow*( one / ( one+cnorm( j ) ) )
                 end do
              end if
              50 continue
           else
              ! compute the growth in a**t * x = b.
              if( upper ) then
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              else
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 80
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, m(0) = max{x(i), i=1,...,n}.
                 grow = one / max( xbnd, smlnum )
                 xbnd = grow
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = 1_ilp
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 80
                    ! g(j) = max( g(j-1), m(j-1)*( 1 + cnorm(j) ) )
                    xj = one + cnorm( j )
                    grow = min( grow, xbnd / xj )
                    ! m(j) = m(j-1)*( 1 + cnorm(j) ) / abs(a(j,j))
                    tjj = abs( ap( ip ) )
                    if( xj>tjj )xbnd = xbnd*( tjj / xj )
                    jlen = jlen + 1_ilp
                    ip = ip + jinc*jlen
                 end do
                 grow = min( grow, xbnd )
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, one / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 80
                    ! g(j) = ( 1 + cnorm(j) )*g(j-1)
                    xj = one + cnorm( j )
                    grow = grow / xj
                 end do
              end if
              80 continue
           end if
           if( ( grow*tscal )>smlnum ) then
              ! use the level 2 blas solve if the reciprocal of the bound on
              ! elements of x is not too small.
              call stdlib_dtpsv( uplo, trans, diag, n, ap, x, 1_ilp )
           else
              ! use a level 1 blas solve, scaling intermediate results.
              if( xmax>bignum ) then
                 ! scale x so that its components are less than or equal to
                 ! bignum in absolute value.
                 scale = bignum / xmax
                 call stdlib_dscal( n, scale, x, 1_ilp )
                 xmax = bignum
              end if
              if( notran ) then
                 ! solve a * x = b
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 loop_110: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) / a(j,j), scaling x if necessary.
                    xj = abs( x( j ) )
                    if( nounit ) then
                       tjjs = ap( ip )*tscal
                    else
                       tjjs = tscal
                       if( tscal==one )go to 100
                    end if
                    tjj = abs( tjjs )
                    if( tjj>smlnum ) then
                          ! abs(a(j,j)) > smlnum:
                       if( tjj<one ) then
                          if( xj>tjj*bignum ) then
                                ! scale x by 1/b(j).
                             rec = one / xj
                             call stdlib_dscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j ) = x( j ) / tjjs
                       xj = abs( x( j ) )
                    else if( tjj>zero ) then
                          ! 0 < abs(a(j,j)) <= smlnum:
                       if( xj>tjj*bignum ) then
                             ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum
                             ! to avoid overflow when dividing by a(j,j).
                          rec = ( tjj*bignum ) / xj
                          if( cnorm( j )>one ) then
                                ! scale by 1/cnorm(j) to avoid overflow when
                                ! multiplying x(j) times column j.
                             rec = rec / cnorm( j )
                          end if
                          call stdlib_dscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                       x( j ) = x( j ) / tjjs
                       xj = abs( x( j ) )
                    else
                          ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                          ! scale = 0, and compute a solution to a*x = 0.
                       do i = 1, n
                          x( i ) = zero
                       end do
                       x( j ) = one
                       xj = one
                       scale = zero
                       xmax = zero
                    end if
                    100 continue
                    ! scale x if necessary to avoid overflow when adding a
                    ! multiple of column j of a.
                    if( xj>one ) then
                       rec = one / xj
                       if( cnorm( j )>( bignum-xmax )*rec ) then
                          ! scale x by 1/(2*abs(x(j))).
                          rec = rec*half
                          call stdlib_dscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                       end if
                    else if( xj*cnorm( j )>( bignum-xmax ) ) then
                       ! scale x by 1/2.
                       call stdlib_dscal( n, half, x, 1_ilp )
                       scale = scale*half
                    end if
                    if( upper ) then
                       if( j>1_ilp ) then
                          ! compute the update
                             ! x(1:j-1) := x(1:j-1) - x(j) * a(1:j-1,j)
                          call stdlib_daxpy( j-1, -x( j )*tscal, ap( ip-j+1 ), 1_ilp, x,1_ilp )
                          i = stdlib_idamax( j-1, x, 1_ilp )
                          xmax = abs( x( i ) )
                       end if
                       ip = ip - j
                    else
                       if( j<n ) then
                          ! compute the update
                             ! x(j+1:n) := x(j+1:n) - x(j) * a(j+1:n,j)
                          call stdlib_daxpy( n-j, -x( j )*tscal, ap( ip+1 ), 1_ilp,x( j+1 ), 1_ilp )
                                    
                          i = j + stdlib_idamax( n-j, x( j+1 ), 1_ilp )
                          xmax = abs( x( i ) )
                       end if
                       ip = ip + n - j + 1_ilp
                    end if
                 end do loop_110
              else
                 ! solve a**t * x = b
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = 1_ilp
                 loop_160: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = abs( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = ap( ip )*tscal
                       else
                          tjjs = tscal
                       end if
                       tjj = abs( tjjs )
                       if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                          rec = min( one, rec*tjj )
                          uscal = uscal / tjjs
                       end if
                       if( rec<one ) then
                          call stdlib_dscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    sumj = zero
                    if( uscal==one ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_ddot to perform the dot product.
                       if( upper ) then
                          sumj = stdlib_ddot( j-1, ap( ip-j+1 ), 1_ilp, x, 1_ilp )
                       else if( j<n ) then
                          sumj = stdlib_ddot( n-j, ap( ip+1 ), 1_ilp, x( j+1 ), 1_ilp )
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          do i = 1, j - 1
                             sumj = sumj + ( ap( ip-j+i )*uscal )*x( i )
                          end do
                       else if( j<n ) then
                          do i = 1, n - j
                             sumj = sumj + ( ap( ip+i )*uscal )*x( j+i )
                          end do
                       end if
                    end if
                    if( uscal==tscal ) then
                       ! compute x(j) := ( x(j) - sumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - sumj
                       xj = abs( x( j ) )
                       if( nounit ) then
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjjs = ap( ip )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 150
                       end if
                       tjj = abs( tjjs )
                       if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                rec = one / xj
                                call stdlib_dscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = x( j ) / tjjs
                       else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                             rec = ( tjj*bignum ) / xj
                             call stdlib_dscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = x( j ) / tjjs
                       else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0, and compute a solution to a**t*x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          scale = zero
                          xmax = zero
                       end if
                       150 continue
                    else
                       ! compute x(j) := x(j) / a(j,j)  - sumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = x( j ) / tjjs - sumj
                    end if
                    xmax = max( xmax, abs( x( j ) ) )
                    jlen = jlen + 1_ilp
                    ip = ip + jinc*jlen
                 end do loop_160
              end if
              scale = scale / tscal
           end if
           ! scale the column norms by 1/tscal for return.
           if( tscal/=one ) then
              call stdlib_dscal( n, one / tscal, cnorm, 1_ilp )
           end if
           return
     end subroutine stdlib_dlatps


     pure module subroutine stdlib_clatps( uplo, trans, diag, normin, n, ap, x, scale,cnorm, info )
     !! CLATPS solves one of the triangular systems
     !! A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b,
     !! with scaling to prevent overflow, where A is an upper or lower
     !! triangular matrix stored in packed form.  Here A**T denotes the
     !! transpose of A, A**H denotes the conjugate transpose of A, x and b
     !! are n-element vectors, and s is a scaling factor, usually less than
     !! or equal to 1, chosen so that the components of x will be less than
     !! the overflow threshold.  If the unscaled problem will not cause
     !! overflow, the Level 2 BLAS routine CTPSV is called. If the matrix A
     !! is singular (A(j,j) = 0 for some j), then s is set to 0 and a
     !! non-trivial solution to A*x = 0 is returned.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, normin, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: scale
           ! Array Arguments 
           real(sp), intent(inout) :: cnorm(*)
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           integer(ilp) :: i, imax, ip, j, jfirst, jinc, jlast, jlen
           real(sp) :: bignum, grow, rec, smlnum, tjj, tmax, tscal, xbnd, xj, xmax
           complex(sp) :: csumj, tjjs, uscal, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1, cabs2
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           cabs2( zdum ) = abs( real( zdum,KIND=sp) / 2. ) +abs( aimag( zdum ) / 2. )
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           ! test the input parameters.
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( normin, 'Y' ) .and. .not.stdlib_lsame( normin, 'N' ) ) &
                     then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLATPS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine machine dependent parameters to control overflow.
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = smlnum / stdlib_slamch( 'PRECISION' )
           bignum = one / smlnum
           scale = one
           if( stdlib_lsame( normin, 'N' ) ) then
              ! compute the 1-norm of each column, not including the diagonal.
              if( upper ) then
                 ! a is upper triangular.
                 ip = 1_ilp
                 do j = 1, n
                    cnorm( j ) = stdlib_scasum( j-1, ap( ip ), 1_ilp )
                    ip = ip + j
                 end do
              else
                 ! a is lower triangular.
                 ip = 1_ilp
                 do j = 1, n - 1
                    cnorm( j ) = stdlib_scasum( n-j, ap( ip+1 ), 1_ilp )
                    ip = ip + n - j + 1_ilp
                 end do
                 cnorm( n ) = zero
              end if
           end if
           ! scale the column norms by tscal if the maximum element in cnorm is
           ! greater than bignum/2.
           imax = stdlib_isamax( n, cnorm, 1_ilp )
           tmax = cnorm( imax )
           if( tmax<=bignum*half ) then
              tscal = one
           else
              tscal = half / ( smlnum*tmax )
              call stdlib_sscal( n, tscal, cnorm, 1_ilp )
           end if
           ! compute a bound on the computed solution vector to see if the
           ! level 2 blas routine stdlib_ctpsv can be used.
           xmax = zero
           do j = 1, n
              xmax = max( xmax, cabs2( x( j ) ) )
           end do
           xbnd = xmax
           if( notran ) then
              ! compute the growth in a * x = b.
              if( upper ) then
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              else
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 60
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, g(0) = max{x(i), i=1,...,n}.
                 grow = half / max( xbnd, smlnum )
                 xbnd = grow
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = n
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 60
                    tjjs = ap( ip )
                    tjj = cabs1( tjjs )
                    if( tjj>=smlnum ) then
                       ! m(j) = g(j-1) / abs(a(j,j))
                       xbnd = min( xbnd, min( one, tjj )*grow )
                    else
                       ! m(j) could overflow, set xbnd to 0.
                       xbnd = zero
                    end if
                    if( tjj+cnorm( j )>=smlnum ) then
                       ! g(j) = g(j-1)*( 1 + cnorm(j) / abs(a(j,j)) )
                       grow = grow*( tjj / ( tjj+cnorm( j ) ) )
                    else
                       ! g(j) could overflow, set grow to 0.
                       grow = zero
                    end if
                    ip = ip + jinc*jlen
                    jlen = jlen - 1_ilp
                 end do
                 grow = xbnd
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, half / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 60
                    ! g(j) = g(j-1)*( 1 + cnorm(j) )
                    grow = grow*( one / ( one+cnorm( j ) ) )
                 end do
              end if
              60 continue
           else
              ! compute the growth in a**t * x = b  or  a**h * x = b.
              if( upper ) then
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              else
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 90
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, m(0) = max{x(i), i=1,...,n}.
                 grow = half / max( xbnd, smlnum )
                 xbnd = grow
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = 1_ilp
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 90
                    ! g(j) = max( g(j-1), m(j-1)*( 1 + cnorm(j) ) )
                    xj = one + cnorm( j )
                    grow = min( grow, xbnd / xj )
                    tjjs = ap( ip )
                    tjj = cabs1( tjjs )
                    if( tjj>=smlnum ) then
                       ! m(j) = m(j-1)*( 1 + cnorm(j) ) / abs(a(j,j))
                       if( xj>tjj )xbnd = xbnd*( tjj / xj )
                    else
                       ! m(j) could overflow, set xbnd to 0.
                       xbnd = zero
                    end if
                    jlen = jlen + 1_ilp
                    ip = ip + jinc*jlen
                 end do
                 grow = min( grow, xbnd )
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, half / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 90
                    ! g(j) = ( 1 + cnorm(j) )*g(j-1)
                    xj = one + cnorm( j )
                    grow = grow / xj
                 end do
              end if
              90 continue
           end if
           if( ( grow*tscal )>smlnum ) then
              ! use the level 2 blas solve if the reciprocal of the bound on
              ! elements of x is not too small.
              call stdlib_ctpsv( uplo, trans, diag, n, ap, x, 1_ilp )
           else
              ! use a level 1 blas solve, scaling intermediate results.
              if( xmax>bignum*half ) then
                 ! scale x so that its components are less than or equal to
                 ! bignum in absolute value.
                 scale = ( bignum*half ) / xmax
                 call stdlib_csscal( n, scale, x, 1_ilp )
                 xmax = bignum
              else
                 xmax = xmax*two
              end if
              if( notran ) then
                 ! solve a * x = b
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 loop_110: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) / a(j,j), scaling x if necessary.
                    xj = cabs1( x( j ) )
                    if( nounit ) then
                       tjjs = ap( ip )*tscal
                    else
                       tjjs = tscal
                       if( tscal==one )go to 105
                    end if
                       tjj = cabs1( tjjs )
                       if( tjj>smlnum ) then
                          ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                ! scale x by 1/b(j).
                                rec = one / xj
                                call stdlib_csscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = stdlib_cladiv( x( j ), tjjs )
                          xj = cabs1( x( j ) )
                       else if( tjj>zero ) then
                          ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                             ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum
                             ! to avoid overflow when dividing by a(j,j).
                             rec = ( tjj*bignum ) / xj
                             if( cnorm( j )>one ) then
                                ! scale by 1/cnorm(j) to avoid overflow when
                                ! multiplying x(j) times column j.
                                rec = rec / cnorm( j )
                             end if
                             call stdlib_csscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = stdlib_cladiv( x( j ), tjjs )
                          xj = cabs1( x( j ) )
                       else
                          ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                          ! scale = 0, and compute a solution to a*x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          xj = one
                          scale = zero
                          xmax = zero
                       end if
                       105 continue
                    ! scale x if necessary to avoid overflow when adding a
                    ! multiple of column j of a.
                    if( xj>one ) then
                       rec = one / xj
                       if( cnorm( j )>( bignum-xmax )*rec ) then
                          ! scale x by 1/(2*abs(x(j))).
                          rec = rec*half
                          call stdlib_csscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                       end if
                    else if( xj*cnorm( j )>( bignum-xmax ) ) then
                       ! scale x by 1/2.
                       call stdlib_csscal( n, half, x, 1_ilp )
                       scale = scale*half
                    end if
                    if( upper ) then
                       if( j>1_ilp ) then
                          ! compute the update
                             ! x(1:j-1) := x(1:j-1) - x(j) * a(1:j-1,j)
                          call stdlib_caxpy( j-1, -x( j )*tscal, ap( ip-j+1 ), 1_ilp, x,1_ilp )
                          i = stdlib_icamax( j-1, x, 1_ilp )
                          xmax = cabs1( x( i ) )
                       end if
                       ip = ip - j
                    else
                       if( j<n ) then
                          ! compute the update
                             ! x(j+1:n) := x(j+1:n) - x(j) * a(j+1:n,j)
                          call stdlib_caxpy( n-j, -x( j )*tscal, ap( ip+1 ), 1_ilp,x( j+1 ), 1_ilp )
                                    
                          i = j + stdlib_icamax( n-j, x( j+1 ), 1_ilp )
                          xmax = cabs1( x( i ) )
                       end if
                       ip = ip + n - j + 1_ilp
                    end if
                 end do loop_110
              else if( stdlib_lsame( trans, 'T' ) ) then
                 ! solve a**t * x = b
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = 1_ilp
                 loop_150: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = cabs1( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = ap( ip )*tscal
                       else
                          tjjs = tscal
                       end if
                          tjj = cabs1( tjjs )
                          if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                             rec = min( one, rec*tjj )
                             uscal = stdlib_cladiv( uscal, tjjs )
                          end if
                       if( rec<one ) then
                          call stdlib_csscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    csumj = zero
                    if( uscal==cmplx( one,KIND=sp) ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_cdotu to perform the dot product.
                       if( upper ) then
                          csumj = stdlib_cdotu( j-1, ap( ip-j+1 ), 1_ilp, x, 1_ilp )
                       else if( j<n ) then
                          csumj = stdlib_cdotu( n-j, ap( ip+1 ), 1_ilp, x( j+1 ), 1_ilp )
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          do i = 1, j - 1
                             csumj = csumj + ( ap( ip-j+i )*uscal )*x( i )
                          end do
                       else if( j<n ) then
                          do i = 1, n - j
                             csumj = csumj + ( ap( ip+i )*uscal )*x( j+i )
                          end do
                       end if
                    end if
                    if( uscal==cmplx( tscal,KIND=sp) ) then
                       ! compute x(j) := ( x(j) - csumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - csumj
                       xj = cabs1( x( j ) )
                       if( nounit ) then
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjjs = ap( ip )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 145
                       end if
                          tjj = cabs1( tjjs )
                          if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                             if( tjj<one ) then
                                if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                   rec = one / xj
                                   call stdlib_csscal( n, rec, x, 1_ilp )
                                   scale = scale*rec
                                   xmax = xmax*rec
                                end if
                             end if
                             x( j ) = stdlib_cladiv( x( j ), tjjs )
                          else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                             if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                                rec = ( tjj*bignum ) / xj
                                call stdlib_csscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                             x( j ) = stdlib_cladiv( x( j ), tjjs )
                          else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0 and compute a solution to a**t *x = 0.
                             do i = 1, n
                                x( i ) = zero
                             end do
                             x( j ) = one
                             scale = zero
                             xmax = zero
                          end if
                          145 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - csumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = stdlib_cladiv( x( j ), tjjs ) - csumj
                    end if
                    xmax = max( xmax, cabs1( x( j ) ) )
                    jlen = jlen + 1_ilp
                    ip = ip + jinc*jlen
                 end do loop_150
              else
                 ! solve a**h * x = b
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = 1_ilp
                 loop_190: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = cabs1( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = conjg( ap( ip ) )*tscal
                       else
                          tjjs = tscal
                       end if
                          tjj = cabs1( tjjs )
                          if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                             rec = min( one, rec*tjj )
                             uscal = stdlib_cladiv( uscal, tjjs )
                          end if
                       if( rec<one ) then
                          call stdlib_csscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    csumj = zero
                    if( uscal==cmplx( one,KIND=sp) ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_cdotc to perform the dot product.
                       if( upper ) then
                          csumj = stdlib_cdotc( j-1, ap( ip-j+1 ), 1_ilp, x, 1_ilp )
                       else if( j<n ) then
                          csumj = stdlib_cdotc( n-j, ap( ip+1 ), 1_ilp, x( j+1 ), 1_ilp )
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          do i = 1, j - 1
                             csumj = csumj + ( conjg( ap( ip-j+i ) )*uscal )*x( i )
                          end do
                       else if( j<n ) then
                          do i = 1, n - j
                             csumj = csumj + ( conjg( ap( ip+i ) )*uscal )*x( j+i )
                          end do
                       end if
                    end if
                    if( uscal==cmplx( tscal,KIND=sp) ) then
                       ! compute x(j) := ( x(j) - csumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - csumj
                       xj = cabs1( x( j ) )
                       if( nounit ) then
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjjs = conjg( ap( ip ) )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 185
                       end if
                          tjj = cabs1( tjjs )
                          if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                             if( tjj<one ) then
                                if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                   rec = one / xj
                                   call stdlib_csscal( n, rec, x, 1_ilp )
                                   scale = scale*rec
                                   xmax = xmax*rec
                                end if
                             end if
                             x( j ) = stdlib_cladiv( x( j ), tjjs )
                          else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                             if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                                rec = ( tjj*bignum ) / xj
                                call stdlib_csscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                             x( j ) = stdlib_cladiv( x( j ), tjjs )
                          else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0 and compute a solution to a**h *x = 0.
                             do i = 1, n
                                x( i ) = zero
                             end do
                             x( j ) = one
                             scale = zero
                             xmax = zero
                          end if
                          185 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - csumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = stdlib_cladiv( x( j ), tjjs ) - csumj
                    end if
                    xmax = max( xmax, cabs1( x( j ) ) )
                    jlen = jlen + 1_ilp
                    ip = ip + jinc*jlen
                 end do loop_190
              end if
              scale = scale / tscal
           end if
           ! scale the column norms by 1/tscal for return.
           if( tscal/=one ) then
              call stdlib_sscal( n, one / tscal, cnorm, 1_ilp )
           end if
           return
     end subroutine stdlib_clatps

     pure module subroutine stdlib_zlatps( uplo, trans, diag, normin, n, ap, x, scale,cnorm, info )
     !! ZLATPS solves one of the triangular systems
     !! A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b,
     !! with scaling to prevent overflow, where A is an upper or lower
     !! triangular matrix stored in packed form.  Here A**T denotes the
     !! transpose of A, A**H denotes the conjugate transpose of A, x and b
     !! are n-element vectors, and s is a scaling factor, usually less than
     !! or equal to 1, chosen so that the components of x will be less than
     !! the overflow threshold.  If the unscaled problem will not cause
     !! overflow, the Level 2 BLAS routine ZTPSV is called. If the matrix A
     !! is singular (A(j,j) = 0 for some j), then s is set to 0 and a
     !! non-trivial solution to A*x = 0 is returned.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, normin, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: scale
           ! Array Arguments 
           real(dp), intent(inout) :: cnorm(*)
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           integer(ilp) :: i, imax, ip, j, jfirst, jinc, jlast, jlen
           real(dp) :: bignum, grow, rec, smlnum, tjj, tmax, tscal, xbnd, xj, xmax
           complex(dp) :: csumj, tjjs, uscal, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1, cabs2
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           cabs2( zdum ) = abs( real( zdum,KIND=dp) / 2._dp ) +abs( aimag( zdum ) / 2._dp )
                     
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           ! test the input parameters.
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( normin, 'Y' ) .and. .not.stdlib_lsame( normin, 'N' ) ) &
                     then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLATPS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine machine dependent parameters to control overflow.
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = smlnum / stdlib_dlamch( 'PRECISION' )
           bignum = one / smlnum
           scale = one
           if( stdlib_lsame( normin, 'N' ) ) then
              ! compute the 1-norm of each column, not including the diagonal.
              if( upper ) then
                 ! a is upper triangular.
                 ip = 1_ilp
                 do j = 1, n
                    cnorm( j ) = stdlib_dzasum( j-1, ap( ip ), 1_ilp )
                    ip = ip + j
                 end do
              else
                 ! a is lower triangular.
                 ip = 1_ilp
                 do j = 1, n - 1
                    cnorm( j ) = stdlib_dzasum( n-j, ap( ip+1 ), 1_ilp )
                    ip = ip + n - j + 1_ilp
                 end do
                 cnorm( n ) = zero
              end if
           end if
           ! scale the column norms by tscal if the maximum element in cnorm is
           ! greater than bignum/2.
           imax = stdlib_idamax( n, cnorm, 1_ilp )
           tmax = cnorm( imax )
           if( tmax<=bignum*half ) then
              tscal = one
           else
              tscal = half / ( smlnum*tmax )
              call stdlib_dscal( n, tscal, cnorm, 1_ilp )
           end if
           ! compute a bound on the computed solution vector to see if the
           ! level 2 blas routine stdlib_ztpsv can be used.
           xmax = zero
           do j = 1, n
              xmax = max( xmax, cabs2( x( j ) ) )
           end do
           xbnd = xmax
           if( notran ) then
              ! compute the growth in a * x = b.
              if( upper ) then
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              else
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 60
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, g(0) = max{x(i), i=1,...,n}.
                 grow = half / max( xbnd, smlnum )
                 xbnd = grow
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = n
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 60
                    tjjs = ap( ip )
                    tjj = cabs1( tjjs )
                    if( tjj>=smlnum ) then
                       ! m(j) = g(j-1) / abs(a(j,j))
                       xbnd = min( xbnd, min( one, tjj )*grow )
                    else
                       ! m(j) could overflow, set xbnd to 0.
                       xbnd = zero
                    end if
                    if( tjj+cnorm( j )>=smlnum ) then
                       ! g(j) = g(j-1)*( 1 + cnorm(j) / abs(a(j,j)) )
                       grow = grow*( tjj / ( tjj+cnorm( j ) ) )
                    else
                       ! g(j) could overflow, set grow to 0.
                       grow = zero
                    end if
                    ip = ip + jinc*jlen
                    jlen = jlen - 1_ilp
                 end do
                 grow = xbnd
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, half / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 60
                    ! g(j) = g(j-1)*( 1 + cnorm(j) )
                    grow = grow*( one / ( one+cnorm( j ) ) )
                 end do
              end if
              60 continue
           else
              ! compute the growth in a**t * x = b  or  a**h * x = b.
              if( upper ) then
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
              else
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 90
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, m(0) = max{x(i), i=1,...,n}.
                 grow = half / max( xbnd, smlnum )
                 xbnd = grow
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = 1_ilp
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 90
                    ! g(j) = max( g(j-1), m(j-1)*( 1 + cnorm(j) ) )
                    xj = one + cnorm( j )
                    grow = min( grow, xbnd / xj )
                    tjjs = ap( ip )
                    tjj = cabs1( tjjs )
                    if( tjj>=smlnum ) then
                       ! m(j) = m(j-1)*( 1 + cnorm(j) ) / abs(a(j,j))
                       if( xj>tjj )xbnd = xbnd*( tjj / xj )
                    else
                       ! m(j) could overflow, set xbnd to 0.
                       xbnd = zero
                    end if
                    jlen = jlen + 1_ilp
                    ip = ip + jinc*jlen
                 end do
                 grow = min( grow, xbnd )
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, half / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 90
                    ! g(j) = ( 1 + cnorm(j) )*g(j-1)
                    xj = one + cnorm( j )
                    grow = grow / xj
                 end do
              end if
              90 continue
           end if
           if( ( grow*tscal )>smlnum ) then
              ! use the level 2 blas solve if the reciprocal of the bound on
              ! elements of x is not too small.
              call stdlib_ztpsv( uplo, trans, diag, n, ap, x, 1_ilp )
           else
              ! use a level 1 blas solve, scaling intermediate results.
              if( xmax>bignum*half ) then
                 ! scale x so that its components are less than or equal to
                 ! bignum in absolute value.
                 scale = ( bignum*half ) / xmax
                 call stdlib_zdscal( n, scale, x, 1_ilp )
                 xmax = bignum
              else
                 xmax = xmax*two
              end if
              if( notran ) then
                 ! solve a * x = b
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 loop_120: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) / a(j,j), scaling x if necessary.
                    xj = cabs1( x( j ) )
                    if( nounit ) then
                       tjjs = ap( ip )*tscal
                    else
                       tjjs = tscal
                       if( tscal==one )go to 110
                    end if
                    tjj = cabs1( tjjs )
                    if( tjj>smlnum ) then
                          ! abs(a(j,j)) > smlnum:
                       if( tjj<one ) then
                          if( xj>tjj*bignum ) then
                                ! scale x by 1/b(j).
                             rec = one / xj
                             call stdlib_zdscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j ) = stdlib_zladiv( x( j ), tjjs )
                       xj = cabs1( x( j ) )
                    else if( tjj>zero ) then
                          ! 0 < abs(a(j,j)) <= smlnum:
                       if( xj>tjj*bignum ) then
                             ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum
                             ! to avoid overflow when dividing by a(j,j).
                          rec = ( tjj*bignum ) / xj
                          if( cnorm( j )>one ) then
                                ! scale by 1/cnorm(j) to avoid overflow when
                                ! multiplying x(j) times column j.
                             rec = rec / cnorm( j )
                          end if
                          call stdlib_zdscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                       x( j ) = stdlib_zladiv( x( j ), tjjs )
                       xj = cabs1( x( j ) )
                    else
                          ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                          ! scale = 0, and compute a solution to a*x = 0.
                       do i = 1, n
                          x( i ) = zero
                       end do
                       x( j ) = one
                       xj = one
                       scale = zero
                       xmax = zero
                    end if
                    110 continue
                    ! scale x if necessary to avoid overflow when adding a
                    ! multiple of column j of a.
                    if( xj>one ) then
                       rec = one / xj
                       if( cnorm( j )>( bignum-xmax )*rec ) then
                          ! scale x by 1/(2*abs(x(j))).
                          rec = rec*half
                          call stdlib_zdscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                       end if
                    else if( xj*cnorm( j )>( bignum-xmax ) ) then
                       ! scale x by 1/2.
                       call stdlib_zdscal( n, half, x, 1_ilp )
                       scale = scale*half
                    end if
                    if( upper ) then
                       if( j>1_ilp ) then
                          ! compute the update
                             ! x(1:j-1) := x(1:j-1) - x(j) * a(1:j-1,j)
                          call stdlib_zaxpy( j-1, -x( j )*tscal, ap( ip-j+1 ), 1_ilp, x,1_ilp )
                          i = stdlib_izamax( j-1, x, 1_ilp )
                          xmax = cabs1( x( i ) )
                       end if
                       ip = ip - j
                    else
                       if( j<n ) then
                          ! compute the update
                             ! x(j+1:n) := x(j+1:n) - x(j) * a(j+1:n,j)
                          call stdlib_zaxpy( n-j, -x( j )*tscal, ap( ip+1 ), 1_ilp,x( j+1 ), 1_ilp )
                                    
                          i = j + stdlib_izamax( n-j, x( j+1 ), 1_ilp )
                          xmax = cabs1( x( i ) )
                       end if
                       ip = ip + n - j + 1_ilp
                    end if
                 end do loop_120
              else if( stdlib_lsame( trans, 'T' ) ) then
                 ! solve a**t * x = b
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = 1_ilp
                 loop_170: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = cabs1( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = ap( ip )*tscal
                       else
                          tjjs = tscal
                       end if
                       tjj = cabs1( tjjs )
                       if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                          rec = min( one, rec*tjj )
                          uscal = stdlib_zladiv( uscal, tjjs )
                       end if
                       if( rec<one ) then
                          call stdlib_zdscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    csumj = zero
                    if( uscal==cmplx( one,KIND=dp) ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_zdotu to perform the dot product.
                       if( upper ) then
                          csumj = stdlib_zdotu( j-1, ap( ip-j+1 ), 1_ilp, x, 1_ilp )
                       else if( j<n ) then
                          csumj = stdlib_zdotu( n-j, ap( ip+1 ), 1_ilp, x( j+1 ), 1_ilp )
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          do i = 1, j - 1
                             csumj = csumj + ( ap( ip-j+i )*uscal )*x( i )
                          end do
                       else if( j<n ) then
                          do i = 1, n - j
                             csumj = csumj + ( ap( ip+i )*uscal )*x( j+i )
                          end do
                       end if
                    end if
                    if( uscal==cmplx( tscal,KIND=dp) ) then
                       ! compute x(j) := ( x(j) - csumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - csumj
                       xj = cabs1( x( j ) )
                       if( nounit ) then
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjjs = ap( ip )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 160
                       end if
                       tjj = cabs1( tjjs )
                       if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                rec = one / xj
                                call stdlib_zdscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = stdlib_zladiv( x( j ), tjjs )
                       else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                             rec = ( tjj*bignum ) / xj
                             call stdlib_zdscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = stdlib_zladiv( x( j ), tjjs )
                       else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0 and compute a solution to a**t *x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          scale = zero
                          xmax = zero
                       end if
                       160 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - csumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = stdlib_zladiv( x( j ), tjjs ) - csumj
                    end if
                    xmax = max( xmax, cabs1( x( j ) ) )
                    jlen = jlen + 1_ilp
                    ip = ip + jinc*jlen
                 end do loop_170
              else
                 ! solve a**h * x = b
                 ip = jfirst*( jfirst+1 ) / 2_ilp
                 jlen = 1_ilp
                 loop_220: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = cabs1( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = conjg( ap( ip ) )*tscal
                       else
                          tjjs = tscal
                       end if
                       tjj = cabs1( tjjs )
                       if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                          rec = min( one, rec*tjj )
                          uscal = stdlib_zladiv( uscal, tjjs )
                       end if
                       if( rec<one ) then
                          call stdlib_zdscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    csumj = zero
                    if( uscal==cmplx( one,KIND=dp) ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_zdotc to perform the dot product.
                       if( upper ) then
                          csumj = stdlib_zdotc( j-1, ap( ip-j+1 ), 1_ilp, x, 1_ilp )
                       else if( j<n ) then
                          csumj = stdlib_zdotc( n-j, ap( ip+1 ), 1_ilp, x( j+1 ), 1_ilp )
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          do i = 1, j - 1
                             csumj = csumj + ( conjg( ap( ip-j+i ) )*uscal )*x( i )
                          end do
                       else if( j<n ) then
                          do i = 1, n - j
                             csumj = csumj + ( conjg( ap( ip+i ) )*uscal )*x( j+i )
                          end do
                       end if
                    end if
                    if( uscal==cmplx( tscal,KIND=dp) ) then
                       ! compute x(j) := ( x(j) - csumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - csumj
                       xj = cabs1( x( j ) )
                       if( nounit ) then
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjjs = conjg( ap( ip ) )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 210
                       end if
                       tjj = cabs1( tjjs )
                       if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                rec = one / xj
                                call stdlib_zdscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = stdlib_zladiv( x( j ), tjjs )
                       else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                             rec = ( tjj*bignum ) / xj
                             call stdlib_zdscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = stdlib_zladiv( x( j ), tjjs )
                       else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0 and compute a solution to a**h *x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          scale = zero
                          xmax = zero
                       end if
                       210 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - csumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = stdlib_zladiv( x( j ), tjjs ) - csumj
                    end if
                    xmax = max( xmax, cabs1( x( j ) ) )
                    jlen = jlen + 1_ilp
                    ip = ip + jinc*jlen
                 end do loop_220
              end if
              scale = scale / tscal
           end if
           ! scale the column norms by 1/tscal for return.
           if( tscal/=one ) then
              call stdlib_dscal( n, one / tscal, cnorm, 1_ilp )
           end if
           return
     end subroutine stdlib_zlatps




     pure module subroutine stdlib_stptri( uplo, diag, n, ap, info )
     !! STPTRI computes the inverse of a real upper or lower triangular
     !! matrix A stored in packed format.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j, jc, jclast, jj
           real(sp) :: ajj
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STPTRI', -info )
              return
           end if
           ! check for singularity if non-unit.
           if( nounit ) then
              if( upper ) then
                 jj = 0_ilp
                 do info = 1, n
                    jj = jj + info
                    if( ap( jj )==zero )return
                 end do
              else
                 jj = 1_ilp
                 do info = 1, n
                    if( ap( jj )==zero )return
                    jj = jj + n - info + 1_ilp
                 end do
              end if
              info = 0_ilp
           end if
           if( upper ) then
              ! compute inverse of upper triangular matrix.
              jc = 1_ilp
              do j = 1, n
                 if( nounit ) then
                    ap( jc+j-1 ) = one / ap( jc+j-1 )
                    ajj = -ap( jc+j-1 )
                 else
                    ajj = -one
                 end if
                 ! compute elements 1:j-1 of j-th column.
                 call stdlib_stpmv( 'UPPER', 'NO TRANSPOSE', diag, j-1, ap,ap( jc ), 1_ilp )
                 call stdlib_sscal( j-1, ajj, ap( jc ), 1_ilp )
                 jc = jc + j
              end do
           else
              ! compute inverse of lower triangular matrix.
              jc = n*( n+1 ) / 2_ilp
              do j = n, 1, -1
                 if( nounit ) then
                    ap( jc ) = one / ap( jc )
                    ajj = -ap( jc )
                 else
                    ajj = -one
                 end if
                 if( j<n ) then
                    ! compute elements j+1:n of j-th column.
                    call stdlib_stpmv( 'LOWER', 'NO TRANSPOSE', diag, n-j,ap( jclast ), ap( jc+1 )&
                              , 1_ilp )
                    call stdlib_sscal( n-j, ajj, ap( jc+1 ), 1_ilp )
                 end if
                 jclast = jc
                 jc = jc - n + j - 2_ilp
              end do
           end if
           return
     end subroutine stdlib_stptri

     pure module subroutine stdlib_dtptri( uplo, diag, n, ap, info )
     !! DTPTRI computes the inverse of a real upper or lower triangular
     !! matrix A stored in packed format.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j, jc, jclast, jj
           real(dp) :: ajj
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTPTRI', -info )
              return
           end if
           ! check for singularity if non-unit.
           if( nounit ) then
              if( upper ) then
                 jj = 0_ilp
                 do info = 1, n
                    jj = jj + info
                    if( ap( jj )==zero )return
                 end do
              else
                 jj = 1_ilp
                 do info = 1, n
                    if( ap( jj )==zero )return
                    jj = jj + n - info + 1_ilp
                 end do
              end if
              info = 0_ilp
           end if
           if( upper ) then
              ! compute inverse of upper triangular matrix.
              jc = 1_ilp
              do j = 1, n
                 if( nounit ) then
                    ap( jc+j-1 ) = one / ap( jc+j-1 )
                    ajj = -ap( jc+j-1 )
                 else
                    ajj = -one
                 end if
                 ! compute elements 1:j-1 of j-th column.
                 call stdlib_dtpmv( 'UPPER', 'NO TRANSPOSE', diag, j-1, ap,ap( jc ), 1_ilp )
                 call stdlib_dscal( j-1, ajj, ap( jc ), 1_ilp )
                 jc = jc + j
              end do
           else
              ! compute inverse of lower triangular matrix.
              jc = n*( n+1 ) / 2_ilp
              do j = n, 1, -1
                 if( nounit ) then
                    ap( jc ) = one / ap( jc )
                    ajj = -ap( jc )
                 else
                    ajj = -one
                 end if
                 if( j<n ) then
                    ! compute elements j+1:n of j-th column.
                    call stdlib_dtpmv( 'LOWER', 'NO TRANSPOSE', diag, n-j,ap( jclast ), ap( jc+1 )&
                              , 1_ilp )
                    call stdlib_dscal( n-j, ajj, ap( jc+1 ), 1_ilp )
                 end if
                 jclast = jc
                 jc = jc - n + j - 2_ilp
              end do
           end if
           return
     end subroutine stdlib_dtptri


     pure module subroutine stdlib_ctptri( uplo, diag, n, ap, info )
     !! CTPTRI computes the inverse of a complex upper or lower triangular
     !! matrix A stored in packed format.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(sp), intent(inout) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j, jc, jclast, jj
           complex(sp) :: ajj
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTPTRI', -info )
              return
           end if
           ! check for singularity if non-unit.
           if( nounit ) then
              if( upper ) then
                 jj = 0_ilp
                 do info = 1, n
                    jj = jj + info
                    if( ap( jj )==czero )return
                 end do
              else
                 jj = 1_ilp
                 do info = 1, n
                    if( ap( jj )==czero )return
                    jj = jj + n - info + 1_ilp
                 end do
              end if
              info = 0_ilp
           end if
           if( upper ) then
              ! compute inverse of upper triangular matrix.
              jc = 1_ilp
              do j = 1, n
                 if( nounit ) then
                    ap( jc+j-1 ) = cone / ap( jc+j-1 )
                    ajj = -ap( jc+j-1 )
                 else
                    ajj = -cone
                 end if
                 ! compute elements 1:j-1 of j-th column.
                 call stdlib_ctpmv( 'UPPER', 'NO TRANSPOSE', diag, j-1, ap,ap( jc ), 1_ilp )
                 call stdlib_cscal( j-1, ajj, ap( jc ), 1_ilp )
                 jc = jc + j
              end do
           else
              ! compute inverse of lower triangular matrix.
              jc = n*( n+1 ) / 2_ilp
              do j = n, 1, -1
                 if( nounit ) then
                    ap( jc ) = cone / ap( jc )
                    ajj = -ap( jc )
                 else
                    ajj = -cone
                 end if
                 if( j<n ) then
                    ! compute elements j+1:n of j-th column.
                    call stdlib_ctpmv( 'LOWER', 'NO TRANSPOSE', diag, n-j,ap( jclast ), ap( jc+1 )&
                              , 1_ilp )
                    call stdlib_cscal( n-j, ajj, ap( jc+1 ), 1_ilp )
                 end if
                 jclast = jc
                 jc = jc - n + j - 2_ilp
              end do
           end if
           return
     end subroutine stdlib_ctptri

     pure module subroutine stdlib_ztptri( uplo, diag, n, ap, info )
     !! ZTPTRI computes the inverse of a complex upper or lower triangular
     !! matrix A stored in packed format.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(dp), intent(inout) :: ap(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j, jc, jclast, jj
           complex(dp) :: ajj
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTPTRI', -info )
              return
           end if
           ! check for singularity if non-unit.
           if( nounit ) then
              if( upper ) then
                 jj = 0_ilp
                 do info = 1, n
                    jj = jj + info
                    if( ap( jj )==czero )return
                 end do
              else
                 jj = 1_ilp
                 do info = 1, n
                    if( ap( jj )==czero )return
                    jj = jj + n - info + 1_ilp
                 end do
              end if
              info = 0_ilp
           end if
           if( upper ) then
              ! compute inverse of upper triangular matrix.
              jc = 1_ilp
              do j = 1, n
                 if( nounit ) then
                    ap( jc+j-1 ) = cone / ap( jc+j-1 )
                    ajj = -ap( jc+j-1 )
                 else
                    ajj = -cone
                 end if
                 ! compute elements 1:j-1 of j-th column.
                 call stdlib_ztpmv( 'UPPER', 'NO TRANSPOSE', diag, j-1, ap,ap( jc ), 1_ilp )
                 call stdlib_zscal( j-1, ajj, ap( jc ), 1_ilp )
                 jc = jc + j
              end do
           else
              ! compute inverse of lower triangular matrix.
              jc = n*( n+1 ) / 2_ilp
              do j = n, 1, -1
                 if( nounit ) then
                    ap( jc ) = cone / ap( jc )
                    ajj = -ap( jc )
                 else
                    ajj = -cone
                 end if
                 if( j<n ) then
                    ! compute elements j+1:n of j-th column.
                    call stdlib_ztpmv( 'LOWER', 'NO TRANSPOSE', diag, n-j,ap( jclast ), ap( jc+1 )&
                              , 1_ilp )
                    call stdlib_zscal( n-j, ajj, ap( jc+1 ), 1_ilp )
                 end if
                 jclast = jc
                 jc = jc - n + j - 2_ilp
              end do
           end if
           return
     end subroutine stdlib_ztptri




     pure module subroutine stdlib_stprfs( uplo, trans, diag, n, nrhs, ap, b, ldb, x, ldx,ferr, berr, &
     !! STPRFS provides error bounds and backward error estimates for the
     !! solution to a system of linear equations with a triangular packed
     !! coefficient matrix.
     !! The solution matrix X must be computed by STPTRS or some other
     !! means before entering this routine.  STPRFS does not do iterative
     !! refinement because doing so cannot improve the backward error.
               work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: ap(*), b(ldb,*), x(ldx,*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           character :: transt
           integer(ilp) :: i, j, k, kase, kc, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STPRFS', -info )
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
           if( notran ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_250: do j = 1, nrhs
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a or a**t, depending on trans.
              call stdlib_scopy( n, x( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_stpmv( uplo, trans, diag, n, ap, work( n+1 ), 1_ilp )
              call stdlib_saxpy( n, -one, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              if( notran ) then
                 ! compute abs(a)*abs(x) + abs(b).
                 if( upper ) then
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = 1, k
                             work( i ) = work( i ) + abs( ap( kc+i-1 ) )*xk
                          end do
                          kc = kc + k
                       end do
                    else
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = 1, k - 1
                             work( i ) = work( i ) + abs( ap( kc+i-1 ) )*xk
                          end do
                          work( k ) = work( k ) + xk
                          kc = kc + k
                       end do
                    end if
                 else
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = k, n
                             work( i ) = work( i ) + abs( ap( kc+i-k ) )*xk
                          end do
                          kc = kc + n - k + 1_ilp
                       end do
                    else
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = k + 1, n
                             work( i ) = work( i ) + abs( ap( kc+i-k ) )*xk
                          end do
                          work( k ) = work( k ) + xk
                          kc = kc + n - k + 1_ilp
                       end do
                    end if
                 end if
              else
                 ! compute abs(a**t)*abs(x) + abs(b).
                 if( upper ) then
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = 1, k
                             s = s + abs( ap( kc+i-1 ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                          kc = kc + k
                       end do
                    else
                       do k = 1, n
                          s = abs( x( k, j ) )
                          do i = 1, k - 1
                             s = s + abs( ap( kc+i-1 ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                          kc = kc + k
                       end do
                    end if
                 else
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = k, n
                             s = s + abs( ap( kc+i-k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                          kc = kc + n - k + 1_ilp
                       end do
                    else
                       do k = 1, n
                          s = abs( x( k, j ) )
                          do i = k + 1, n
                             s = s + abs( ap( kc+i-k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                          kc = kc + n - k + 1_ilp
                       end do
                    end if
                 end if
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_slacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              210 continue
              call stdlib_slacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**t).
                    call stdlib_stpsv( uplo, transt, diag, n, ap, work( n+1 ), 1_ilp )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_stpsv( uplo, trans, diag, n, ap, work( n+1 ), 1_ilp )
                 end if
                 go to 210
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_250
           return
     end subroutine stdlib_stprfs

     pure module subroutine stdlib_dtprfs( uplo, trans, diag, n, nrhs, ap, b, ldb, x, ldx,ferr, berr, &
     !! DTPRFS provides error bounds and backward error estimates for the
     !! solution to a system of linear equations with a triangular packed
     !! coefficient matrix.
     !! The solution matrix X must be computed by DTPTRS or some other
     !! means before entering this routine.  DTPRFS does not do iterative
     !! refinement because doing so cannot improve the backward error.
               work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: ap(*), b(ldb,*), x(ldx,*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           character :: transt
           integer(ilp) :: i, j, k, kase, kc, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTPRFS', -info )
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
           if( notran ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_250: do j = 1, nrhs
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a or a**t, depending on trans.
              call stdlib_dcopy( n, x( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_dtpmv( uplo, trans, diag, n, ap, work( n+1 ), 1_ilp )
              call stdlib_daxpy( n, -one, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              if( notran ) then
                 ! compute abs(a)*abs(x) + abs(b).
                 if( upper ) then
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = 1, k
                             work( i ) = work( i ) + abs( ap( kc+i-1 ) )*xk
                          end do
                          kc = kc + k
                       end do
                    else
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = 1, k - 1
                             work( i ) = work( i ) + abs( ap( kc+i-1 ) )*xk
                          end do
                          work( k ) = work( k ) + xk
                          kc = kc + k
                       end do
                    end if
                 else
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = k, n
                             work( i ) = work( i ) + abs( ap( kc+i-k ) )*xk
                          end do
                          kc = kc + n - k + 1_ilp
                       end do
                    else
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = k + 1, n
                             work( i ) = work( i ) + abs( ap( kc+i-k ) )*xk
                          end do
                          work( k ) = work( k ) + xk
                          kc = kc + n - k + 1_ilp
                       end do
                    end if
                 end if
              else
                 ! compute abs(a**t)*abs(x) + abs(b).
                 if( upper ) then
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = 1, k
                             s = s + abs( ap( kc+i-1 ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                          kc = kc + k
                       end do
                    else
                       do k = 1, n
                          s = abs( x( k, j ) )
                          do i = 1, k - 1
                             s = s + abs( ap( kc+i-1 ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                          kc = kc + k
                       end do
                    end if
                 else
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = k, n
                             s = s + abs( ap( kc+i-k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                          kc = kc + n - k + 1_ilp
                       end do
                    else
                       do k = 1, n
                          s = abs( x( k, j ) )
                          do i = k + 1, n
                             s = s + abs( ap( kc+i-k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                          kc = kc + n - k + 1_ilp
                       end do
                    end if
                 end if
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_dlacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              210 continue
              call stdlib_dlacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**t).
                    call stdlib_dtpsv( uplo, transt, diag, n, ap, work( n+1 ), 1_ilp )
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_dtpsv( uplo, trans, diag, n, ap, work( n+1 ), 1_ilp )
                 end if
                 go to 210
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_250
           return
     end subroutine stdlib_dtprfs


     pure module subroutine stdlib_ctprfs( uplo, trans, diag, n, nrhs, ap, b, ldb, x, ldx,ferr, berr, &
     !! CTPRFS provides error bounds and backward error estimates for the
     !! solution to a system of linear equations with a triangular packed
     !! coefficient matrix.
     !! The solution matrix X must be computed by CTPTRS or some other
     !! means before entering this routine.  CTPRFS does not do iterative
     !! refinement because doing so cannot improve the backward error.
               work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: ap(*), b(ldb,*), x(ldx,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           character :: transn, transt
           integer(ilp) :: i, j, k, kase, kc, nz
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
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTPRFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'C'
           else
              transn = 'C'
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_250: do j = 1, nrhs
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_ccopy( n, x( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_ctpmv( uplo, trans, diag, n, ap, work, 1_ilp )
              call stdlib_caxpy( n, -cone, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              if( notran ) then
                 ! compute abs(a)*abs(x) + abs(b).
                 if( upper ) then
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = 1, k
                             rwork( i ) = rwork( i ) +cabs1( ap( kc+i-1 ) )*xk
                          end do
                          kc = kc + k
                       end do
                    else
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = 1, k - 1
                             rwork( i ) = rwork( i ) +cabs1( ap( kc+i-1 ) )*xk
                          end do
                          rwork( k ) = rwork( k ) + xk
                          kc = kc + k
                       end do
                    end if
                 else
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = k, n
                             rwork( i ) = rwork( i ) +cabs1( ap( kc+i-k ) )*xk
                          end do
                          kc = kc + n - k + 1_ilp
                       end do
                    else
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = k + 1, n
                             rwork( i ) = rwork( i ) +cabs1( ap( kc+i-k ) )*xk
                          end do
                          rwork( k ) = rwork( k ) + xk
                          kc = kc + n - k + 1_ilp
                       end do
                    end if
                 end if
              else
                 ! compute abs(a**h)*abs(x) + abs(b).
                 if( upper ) then
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = 1, k
                             s = s + cabs1( ap( kc+i-1 ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                          kc = kc + k
                       end do
                    else
                       do k = 1, n
                          s = cabs1( x( k, j ) )
                          do i = 1, k - 1
                             s = s + cabs1( ap( kc+i-1 ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                          kc = kc + k
                       end do
                    end if
                 else
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = k, n
                             s = s + cabs1( ap( kc+i-k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                          kc = kc + n - k + 1_ilp
                       end do
                    else
                       do k = 1, n
                          s = cabs1( x( k, j ) )
                          do i = k + 1, n
                             s = s + cabs1( ap( kc+i-k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                          kc = kc + n - k + 1_ilp
                       end do
                    end if
                 end if
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
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_clacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              210 continue
              call stdlib_clacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**h).
                    call stdlib_ctpsv( uplo, transt, diag, n, ap, work, 1_ilp )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_ctpsv( uplo, transn, diag, n, ap, work, 1_ilp )
                 end if
                 go to 210
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_250
           return
     end subroutine stdlib_ctprfs

     pure module subroutine stdlib_ztprfs( uplo, trans, diag, n, nrhs, ap, b, ldb, x, ldx,ferr, berr, &
     !! ZTPRFS provides error bounds and backward error estimates for the
     !! solution to a system of linear equations with a triangular packed
     !! coefficient matrix.
     !! The solution matrix X must be computed by ZTPTRS or some other
     !! means before entering this routine.  ZTPRFS does not do iterative
     !! refinement because doing so cannot improve the backward error.
               work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: ap(*), b(ldb,*), x(ldx,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           character :: transn, transt
           integer(ilp) :: i, j, k, kase, kc, nz
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
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( nrhs<0_ilp ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTPRFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'C'
           else
              transn = 'C'
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = n + 1_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_250: do j = 1, nrhs
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_zcopy( n, x( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_ztpmv( uplo, trans, diag, n, ap, work, 1_ilp )
              call stdlib_zaxpy( n, -cone, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              if( notran ) then
                 ! compute abs(a)*abs(x) + abs(b).
                 if( upper ) then
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = 1, k
                             rwork( i ) = rwork( i ) +cabs1( ap( kc+i-1 ) )*xk
                          end do
                          kc = kc + k
                       end do
                    else
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = 1, k - 1
                             rwork( i ) = rwork( i ) +cabs1( ap( kc+i-1 ) )*xk
                          end do
                          rwork( k ) = rwork( k ) + xk
                          kc = kc + k
                       end do
                    end if
                 else
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = k, n
                             rwork( i ) = rwork( i ) +cabs1( ap( kc+i-k ) )*xk
                          end do
                          kc = kc + n - k + 1_ilp
                       end do
                    else
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = k + 1, n
                             rwork( i ) = rwork( i ) +cabs1( ap( kc+i-k ) )*xk
                          end do
                          rwork( k ) = rwork( k ) + xk
                          kc = kc + n - k + 1_ilp
                       end do
                    end if
                 end if
              else
                 ! compute abs(a**h)*abs(x) + abs(b).
                 if( upper ) then
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = 1, k
                             s = s + cabs1( ap( kc+i-1 ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                          kc = kc + k
                       end do
                    else
                       do k = 1, n
                          s = cabs1( x( k, j ) )
                          do i = 1, k - 1
                             s = s + cabs1( ap( kc+i-1 ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                          kc = kc + k
                       end do
                    end if
                 else
                    kc = 1_ilp
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = k, n
                             s = s + cabs1( ap( kc+i-k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                          kc = kc + n - k + 1_ilp
                       end do
                    else
                       do k = 1, n
                          s = cabs1( x( k, j ) )
                          do i = k + 1, n
                             s = s + cabs1( ap( kc+i-k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                          kc = kc + n - k + 1_ilp
                       end do
                    end if
                 end if
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
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_zlacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              210 continue
              call stdlib_zlacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**h).
                    call stdlib_ztpsv( uplo, transt, diag, n, ap, work, 1_ilp )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_ztpsv( uplo, transn, diag, n, ap, work, 1_ilp )
                 end if
                 go to 210
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_250
           return
     end subroutine stdlib_ztprfs




     pure module subroutine stdlib_stftri( transr, uplo, diag, n, a, info )
     !! STFTRI computes the inverse of a triangular matrix A stored in RFP
     !! format.
     !! This is a Level 3 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo, diag
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: a(0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( .not.stdlib_lsame( diag, 'N' ) .and. .not.stdlib_lsame( diag, 'U' ) )&
                     then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STFTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
           else
              nisodd = .true.
           end if
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1)
                    call stdlib_strtri( 'L', diag, n1, a( 0_ilp ), n, info )
                    if( info>0 )return
                    call stdlib_strmm( 'R', 'L', 'N', diag, n2, n1, -one, a( 0_ilp ),n, a( n1 ), n )
                              
                    call stdlib_strtri( 'U', diag, n2, a( n ), n, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_strmm( 'L', 'U', 'T', diag, n2, n1, one, a( n ), n,a( n1 ), n )
                              
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    call stdlib_strtri( 'L', diag, n1, a( n2 ), n, info )
                    if( info>0 )return
                    call stdlib_strmm( 'L', 'L', 'T', diag, n1, n2, -one, a( n2 ),n, a( 0_ilp ), n )
                              
                    call stdlib_strtri( 'U', diag, n2, a( n1 ), n, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_strmm( 'R', 'U', 'N', diag, n1, n2, one, a( n1 ),n, a( 0_ilp ), n )
                              
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0), t2 -> a(1), s -> a(0+n1*n1)
                    call stdlib_strtri( 'U', diag, n1, a( 0_ilp ), n1, info )
                    if( info>0 )return
                    call stdlib_strmm( 'L', 'U', 'N', diag, n1, n2, -one, a( 0_ilp ),n1, a( n1*n1 ), &
                              n1 )
                    call stdlib_strtri( 'L', diag, n2, a( 1_ilp ), n1, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_strmm( 'R', 'L', 'T', diag, n1, n2, one, a( 1_ilp ),n1, a( n1*n1 ), &
                              n1 )
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0+n2*n2), t2 -> a(0+n1*n2), s -> a(0)
                    call stdlib_strtri( 'U', diag, n1, a( n2*n2 ), n2, info )
                    if( info>0 )return
                    call stdlib_strmm( 'R', 'U', 'T', diag, n2, n1, -one,a( n2*n2 ), n2, a( 0_ilp ), &
                              n2 )
                    call stdlib_strtri( 'L', diag, n2, a( n1*n2 ), n2, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_strmm( 'L', 'L', 'N', diag, n2, n1, one,a( n1*n2 ), n2, a( 0_ilp ), &
                              n2 )
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    call stdlib_strtri( 'L', diag, k, a( 1_ilp ), n+1, info )
                    if( info>0 )return
                    call stdlib_strmm( 'R', 'L', 'N', diag, k, k, -one, a( 1_ilp ),n+1, a( k+1 ), n+1 &
                              )
                    call stdlib_strtri( 'U', diag, k, a( 0_ilp ), n+1, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_strmm( 'L', 'U', 'T', diag, k, k, one, a( 0_ilp ), n+1,a( k+1 ), n+1 )
                              
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    call stdlib_strtri( 'L', diag, k, a( k+1 ), n+1, info )
                    if( info>0 )return
                    call stdlib_strmm( 'L', 'L', 'T', diag, k, k, -one, a( k+1 ),n+1, a( 0_ilp ), n+1 &
                              )
                    call stdlib_strtri( 'U', diag, k, a( k ), n+1, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_strmm( 'R', 'U', 'N', diag, k, k, one, a( k ), n+1,a( 0_ilp ), n+1 )
                              
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    call stdlib_strtri( 'U', diag, k, a( k ), k, info )
                    if( info>0 )return
                    call stdlib_strmm( 'L', 'U', 'N', diag, k, k, -one, a( k ), k,a( k*( k+1 ) ), &
                              k )
                    call stdlib_strtri( 'L', diag, k, a( 0_ilp ), k, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_strmm( 'R', 'L', 'T', diag, k, k, one, a( 0_ilp ), k,a( k*( k+1 ) ), &
                              k )
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    call stdlib_strtri( 'U', diag, k, a( k*( k+1 ) ), k, info )
                    if( info>0 )return
                    call stdlib_strmm( 'R', 'U', 'T', diag, k, k, -one,a( k*( k+1 ) ), k, a( 0_ilp ), &
                              k )
                    call stdlib_strtri( 'L', diag, k, a( k*k ), k, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_strmm( 'L', 'L', 'N', diag, k, k, one, a( k*k ), k,a( 0_ilp ), k )
                              
                 end if
              end if
           end if
           return
     end subroutine stdlib_stftri

     pure module subroutine stdlib_dtftri( transr, uplo, diag, n, a, info )
     !! DTFTRI computes the inverse of a triangular matrix A stored in RFP
     !! format.
     !! This is a Level 3 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo, diag
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: a(0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( .not.stdlib_lsame( diag, 'N' ) .and. .not.stdlib_lsame( diag, 'U' ) )&
                     then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTFTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
           else
              nisodd = .true.
           end if
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1)
                    call stdlib_dtrtri( 'L', diag, n1, a( 0_ilp ), n, info )
                    if( info>0 )return
                    call stdlib_dtrmm( 'R', 'L', 'N', diag, n2, n1, -one, a( 0_ilp ),n, a( n1 ), n )
                              
                    call stdlib_dtrtri( 'U', diag, n2, a( n ), n, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_dtrmm( 'L', 'U', 'T', diag, n2, n1, one, a( n ), n,a( n1 ), n )
                              
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    call stdlib_dtrtri( 'L', diag, n1, a( n2 ), n, info )
                    if( info>0 )return
                    call stdlib_dtrmm( 'L', 'L', 'T', diag, n1, n2, -one, a( n2 ),n, a( 0_ilp ), n )
                              
                    call stdlib_dtrtri( 'U', diag, n2, a( n1 ), n, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_dtrmm( 'R', 'U', 'N', diag, n1, n2, one, a( n1 ),n, a( 0_ilp ), n )
                              
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0), t2 -> a(1), s -> a(0+n1*n1)
                    call stdlib_dtrtri( 'U', diag, n1, a( 0_ilp ), n1, info )
                    if( info>0 )return
                    call stdlib_dtrmm( 'L', 'U', 'N', diag, n1, n2, -one, a( 0_ilp ),n1, a( n1*n1 ), &
                              n1 )
                    call stdlib_dtrtri( 'L', diag, n2, a( 1_ilp ), n1, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_dtrmm( 'R', 'L', 'T', diag, n1, n2, one, a( 1_ilp ),n1, a( n1*n1 ), &
                              n1 )
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0+n2*n2), t2 -> a(0+n1*n2), s -> a(0)
                    call stdlib_dtrtri( 'U', diag, n1, a( n2*n2 ), n2, info )
                    if( info>0 )return
                    call stdlib_dtrmm( 'R', 'U', 'T', diag, n2, n1, -one,a( n2*n2 ), n2, a( 0_ilp ), &
                              n2 )
                    call stdlib_dtrtri( 'L', diag, n2, a( n1*n2 ), n2, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_dtrmm( 'L', 'L', 'N', diag, n2, n1, one,a( n1*n2 ), n2, a( 0_ilp ), &
                              n2 )
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    call stdlib_dtrtri( 'L', diag, k, a( 1_ilp ), n+1, info )
                    if( info>0 )return
                    call stdlib_dtrmm( 'R', 'L', 'N', diag, k, k, -one, a( 1_ilp ),n+1, a( k+1 ), n+1 &
                              )
                    call stdlib_dtrtri( 'U', diag, k, a( 0_ilp ), n+1, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_dtrmm( 'L', 'U', 'T', diag, k, k, one, a( 0_ilp ), n+1,a( k+1 ), n+1 )
                              
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    call stdlib_dtrtri( 'L', diag, k, a( k+1 ), n+1, info )
                    if( info>0 )return
                    call stdlib_dtrmm( 'L', 'L', 'T', diag, k, k, -one, a( k+1 ),n+1, a( 0_ilp ), n+1 &
                              )
                    call stdlib_dtrtri( 'U', diag, k, a( k ), n+1, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_dtrmm( 'R', 'U', 'N', diag, k, k, one, a( k ), n+1,a( 0_ilp ), n+1 )
                              
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    call stdlib_dtrtri( 'U', diag, k, a( k ), k, info )
                    if( info>0 )return
                    call stdlib_dtrmm( 'L', 'U', 'N', diag, k, k, -one, a( k ), k,a( k*( k+1 ) ), &
                              k )
                    call stdlib_dtrtri( 'L', diag, k, a( 0_ilp ), k, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_dtrmm( 'R', 'L', 'T', diag, k, k, one, a( 0_ilp ), k,a( k*( k+1 ) ), &
                              k )
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    call stdlib_dtrtri( 'U', diag, k, a( k*( k+1 ) ), k, info )
                    if( info>0 )return
                    call stdlib_dtrmm( 'R', 'U', 'T', diag, k, k, -one,a( k*( k+1 ) ), k, a( 0_ilp ), &
                              k )
                    call stdlib_dtrtri( 'L', diag, k, a( k*k ), k, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_dtrmm( 'L', 'L', 'N', diag, k, k, one, a( k*k ), k,a( 0_ilp ), k )
                              
                 end if
              end if
           end if
           return
     end subroutine stdlib_dtftri


     pure module subroutine stdlib_ctftri( transr, uplo, diag, n, a, info )
     !! CTFTRI computes the inverse of a triangular matrix A stored in RFP
     !! format.
     !! This is a Level 3 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo, diag
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( .not.stdlib_lsame( diag, 'N' ) .and. .not.stdlib_lsame( diag, 'U' ) )&
                     then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTFTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
           else
              nisodd = .true.
           end if
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1)
                    call stdlib_ctrtri( 'L', diag, n1, a( 0_ilp ), n, info )
                    if( info>0 )return
                    call stdlib_ctrmm( 'R', 'L', 'N', diag, n2, n1, -cone, a( 0_ilp ),n, a( n1 ), n )
                              
                    call stdlib_ctrtri( 'U', diag, n2, a( n ), n, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_ctrmm( 'L', 'U', 'C', diag, n2, n1, cone, a( n ), n,a( n1 ), n )
                              
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    call stdlib_ctrtri( 'L', diag, n1, a( n2 ), n, info )
                    if( info>0 )return
                    call stdlib_ctrmm( 'L', 'L', 'C', diag, n1, n2, -cone, a( n2 ),n, a( 0_ilp ), n )
                              
                    call stdlib_ctrtri( 'U', diag, n2, a( n1 ), n, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_ctrmm( 'R', 'U', 'N', diag, n1, n2, cone, a( n1 ),n, a( 0_ilp ), n )
                              
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0), t2 -> a(1), s -> a(0+n1*n1)
                    call stdlib_ctrtri( 'U', diag, n1, a( 0_ilp ), n1, info )
                    if( info>0 )return
                    call stdlib_ctrmm( 'L', 'U', 'N', diag, n1, n2, -cone, a( 0_ilp ),n1, a( n1*n1 ), &
                              n1 )
                    call stdlib_ctrtri( 'L', diag, n2, a( 1_ilp ), n1, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_ctrmm( 'R', 'L', 'C', diag, n1, n2, cone, a( 1_ilp ),n1, a( n1*n1 ), &
                              n1 )
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0+n2*n2), t2 -> a(0+n1*n2), s -> a(0)
                    call stdlib_ctrtri( 'U', diag, n1, a( n2*n2 ), n2, info )
                    if( info>0 )return
                    call stdlib_ctrmm( 'R', 'U', 'C', diag, n2, n1, -cone,a( n2*n2 ), n2, a( 0_ilp ), &
                              n2 )
                    call stdlib_ctrtri( 'L', diag, n2, a( n1*n2 ), n2, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_ctrmm( 'L', 'L', 'N', diag, n2, n1, cone,a( n1*n2 ), n2, a( 0_ilp ), &
                              n2 )
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    call stdlib_ctrtri( 'L', diag, k, a( 1_ilp ), n+1, info )
                    if( info>0 )return
                    call stdlib_ctrmm( 'R', 'L', 'N', diag, k, k, -cone, a( 1_ilp ),n+1, a( k+1 ), n+&
                              1_ilp )
                    call stdlib_ctrtri( 'U', diag, k, a( 0_ilp ), n+1, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_ctrmm( 'L', 'U', 'C', diag, k, k, cone, a( 0_ilp ), n+1,a( k+1 ), n+1 &
                              )
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    call stdlib_ctrtri( 'L', diag, k, a( k+1 ), n+1, info )
                    if( info>0 )return
                    call stdlib_ctrmm( 'L', 'L', 'C', diag, k, k, -cone, a( k+1 ),n+1, a( 0_ilp ), n+&
                              1_ilp )
                    call stdlib_ctrtri( 'U', diag, k, a( k ), n+1, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_ctrmm( 'R', 'U', 'N', diag, k, k, cone, a( k ), n+1,a( 0_ilp ), n+1 )
                              
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    call stdlib_ctrtri( 'U', diag, k, a( k ), k, info )
                    if( info>0 )return
                    call stdlib_ctrmm( 'L', 'U', 'N', diag, k, k, -cone, a( k ), k,a( k*( k+1 ) ),&
                               k )
                    call stdlib_ctrtri( 'L', diag, k, a( 0_ilp ), k, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_ctrmm( 'R', 'L', 'C', diag, k, k, cone, a( 0_ilp ), k,a( k*( k+1 ) ), &
                              k )
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    call stdlib_ctrtri( 'U', diag, k, a( k*( k+1 ) ), k, info )
                    if( info>0 )return
                    call stdlib_ctrmm( 'R', 'U', 'C', diag, k, k, -cone,a( k*( k+1 ) ), k, a( 0_ilp ),&
                               k )
                    call stdlib_ctrtri( 'L', diag, k, a( k*k ), k, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_ctrmm( 'L', 'L', 'N', diag, k, k, cone, a( k*k ), k,a( 0_ilp ), k )
                              
                 end if
              end if
           end if
           return
     end subroutine stdlib_ctftri

     pure module subroutine stdlib_ztftri( transr, uplo, diag, n, a, info )
     !! ZTFTRI computes the inverse of a triangular matrix A stored in RFP
     !! format.
     !! This is a Level 3 BLAS version of the algorithm.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo, diag
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( .not.stdlib_lsame( diag, 'N' ) .and. .not.stdlib_lsame( diag, 'U' ) )&
                     then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTFTRI', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
           else
              nisodd = .true.
           end if
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1)
                    call stdlib_ztrtri( 'L', diag, n1, a( 0_ilp ), n, info )
                    if( info>0 )return
                    call stdlib_ztrmm( 'R', 'L', 'N', diag, n2, n1, -cone, a( 0_ilp ),n, a( n1 ), n )
                              
                    call stdlib_ztrtri( 'U', diag, n2, a( n ), n, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_ztrmm( 'L', 'U', 'C', diag, n2, n1, cone, a( n ), n,a( n1 ), n )
                              
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    call stdlib_ztrtri( 'L', diag, n1, a( n2 ), n, info )
                    if( info>0 )return
                    call stdlib_ztrmm( 'L', 'L', 'C', diag, n1, n2, -cone, a( n2 ),n, a( 0_ilp ), n )
                              
                    call stdlib_ztrtri( 'U', diag, n2, a( n1 ), n, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_ztrmm( 'R', 'U', 'N', diag, n1, n2, cone, a( n1 ),n, a( 0_ilp ), n )
                              
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0), t2 -> a(1), s -> a(0+n1*n1)
                    call stdlib_ztrtri( 'U', diag, n1, a( 0_ilp ), n1, info )
                    if( info>0 )return
                    call stdlib_ztrmm( 'L', 'U', 'N', diag, n1, n2, -cone, a( 0_ilp ),n1, a( n1*n1 ), &
                              n1 )
                    call stdlib_ztrtri( 'L', diag, n2, a( 1_ilp ), n1, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_ztrmm( 'R', 'L', 'C', diag, n1, n2, cone, a( 1_ilp ),n1, a( n1*n1 ), &
                              n1 )
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0+n2*n2), t2 -> a(0+n1*n2), s -> a(0)
                    call stdlib_ztrtri( 'U', diag, n1, a( n2*n2 ), n2, info )
                    if( info>0 )return
                    call stdlib_ztrmm( 'R', 'U', 'C', diag, n2, n1, -cone,a( n2*n2 ), n2, a( 0_ilp ), &
                              n2 )
                    call stdlib_ztrtri( 'L', diag, n2, a( n1*n2 ), n2, info )
                    if( info>0_ilp )info = info + n1
                    if( info>0 )return
                    call stdlib_ztrmm( 'L', 'L', 'N', diag, n2, n1, cone,a( n1*n2 ), n2, a( 0_ilp ), &
                              n2 )
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    call stdlib_ztrtri( 'L', diag, k, a( 1_ilp ), n+1, info )
                    if( info>0 )return
                    call stdlib_ztrmm( 'R', 'L', 'N', diag, k, k, -cone, a( 1_ilp ),n+1, a( k+1 ), n+&
                              1_ilp )
                    call stdlib_ztrtri( 'U', diag, k, a( 0_ilp ), n+1, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_ztrmm( 'L', 'U', 'C', diag, k, k, cone, a( 0_ilp ), n+1,a( k+1 ), n+1 &
                              )
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    call stdlib_ztrtri( 'L', diag, k, a( k+1 ), n+1, info )
                    if( info>0 )return
                    call stdlib_ztrmm( 'L', 'L', 'C', diag, k, k, -cone, a( k+1 ),n+1, a( 0_ilp ), n+&
                              1_ilp )
                    call stdlib_ztrtri( 'U', diag, k, a( k ), n+1, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_ztrmm( 'R', 'U', 'N', diag, k, k, cone, a( k ), n+1,a( 0_ilp ), n+1 )
                              
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    call stdlib_ztrtri( 'U', diag, k, a( k ), k, info )
                    if( info>0 )return
                    call stdlib_ztrmm( 'L', 'U', 'N', diag, k, k, -cone, a( k ), k,a( k*( k+1 ) ),&
                               k )
                    call stdlib_ztrtri( 'L', diag, k, a( 0_ilp ), k, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_ztrmm( 'R', 'L', 'C', diag, k, k, cone, a( 0_ilp ), k,a( k*( k+1 ) ), &
                              k )
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    call stdlib_ztrtri( 'U', diag, k, a( k*( k+1 ) ), k, info )
                    if( info>0 )return
                    call stdlib_ztrmm( 'R', 'U', 'C', diag, k, k, -cone,a( k*( k+1 ) ), k, a( 0_ilp ),&
                               k )
                    call stdlib_ztrtri( 'L', diag, k, a( k*k ), k, info )
                    if( info>0_ilp )info = info + k
                    if( info>0 )return
                    call stdlib_ztrmm( 'L', 'L', 'N', diag, k, k, cone, a( k*k ), k,a( 0_ilp ), k )
                              
                 end if
              end if
           end if
           return
     end subroutine stdlib_ztftri




     module subroutine stdlib_stbcon( norm, uplo, diag, n, kd, ab, ldab, rcond, work,iwork, info )
     !! STBCON estimates the reciprocal of the condition number of a
     !! triangular band matrix A, in either the 1-norm or the infinity-norm.
     !! The norm of A is computed and an estimate is obtained for
     !! norm(inv(A)), then the reciprocal of the condition number is
     !! computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, onenrm, upper
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(sp) :: ainvnm, anorm, scale, smlnum, xnorm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( ldab<kd+1 ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STBCON', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              rcond = one
              return
           end if
           rcond = zero
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )*real( max( 1_ilp, n ),KIND=sp)
           ! compute the norm of the triangular matrix a.
           anorm = stdlib_slantb( norm, uplo, diag, n, kd, ab, ldab, work )
           ! continue only if anorm > 0.
           if( anorm>zero ) then
              ! estimate the norm of the inverse of a.
              ainvnm = zero
              normin = 'N'
              if( onenrm ) then
                 kase1 = 1_ilp
              else
                 kase1 = 2_ilp
              end if
              kase = 0_ilp
              10 continue
              call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==kase1 ) then
                    ! multiply by inv(a).
                    call stdlib_slatbs( uplo, 'NO TRANSPOSE', diag, normin, n, kd,ab, ldab, work, &
                              scale, work( 2_ilp*n+1 ), info )
                 else
                    ! multiply by inv(a**t).
                    call stdlib_slatbs( uplo, 'TRANSPOSE', diag, normin, n, kd, ab,ldab, work, &
                              scale, work( 2_ilp*n+1 ), info )
                 end if
                 normin = 'Y'
                 ! multiply by 1/scale if doing so will not cause overflow.
                 if( scale/=one ) then
                    ix = stdlib_isamax( n, work, 1_ilp )
                    xnorm = abs( work( ix ) )
                    if( scale<xnorm*smlnum .or. scale==zero )go to 20
                    call stdlib_srscl( n, scale, work, 1_ilp )
                 end if
                 go to 10
              end if
              ! compute the estimate of the reciprocal condition number.
              if( ainvnm/=zero )rcond = ( one / anorm ) / ainvnm
           end if
           20 continue
           return
     end subroutine stdlib_stbcon

     module subroutine stdlib_dtbcon( norm, uplo, diag, n, kd, ab, ldab, rcond, work,iwork, info )
     !! DTBCON estimates the reciprocal of the condition number of a
     !! triangular band matrix A, in either the 1-norm or the infinity-norm.
     !! The norm of A is computed and an estimate is obtained for
     !! norm(inv(A)), then the reciprocal of the condition number is
     !! computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, onenrm, upper
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(dp) :: ainvnm, anorm, scale, smlnum, xnorm
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( ldab<kd+1 ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTBCON', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              rcond = one
              return
           end if
           rcond = zero
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )*real( max( 1_ilp, n ),KIND=dp)
           ! compute the norm of the triangular matrix a.
           anorm = stdlib_dlantb( norm, uplo, diag, n, kd, ab, ldab, work )
           ! continue only if anorm > 0.
           if( anorm>zero ) then
              ! estimate the norm of the inverse of a.
              ainvnm = zero
              normin = 'N'
              if( onenrm ) then
                 kase1 = 1_ilp
              else
                 kase1 = 2_ilp
              end if
              kase = 0_ilp
              10 continue
              call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==kase1 ) then
                    ! multiply by inv(a).
                    call stdlib_dlatbs( uplo, 'NO TRANSPOSE', diag, normin, n, kd,ab, ldab, work, &
                              scale, work( 2_ilp*n+1 ), info )
                 else
                    ! multiply by inv(a**t).
                    call stdlib_dlatbs( uplo, 'TRANSPOSE', diag, normin, n, kd, ab,ldab, work, &
                              scale, work( 2_ilp*n+1 ), info )
                 end if
                 normin = 'Y'
                 ! multiply by 1/scale if doing so will not cause overflow.
                 if( scale/=one ) then
                    ix = stdlib_idamax( n, work, 1_ilp )
                    xnorm = abs( work( ix ) )
                    if( scale<xnorm*smlnum .or. scale==zero )go to 20
                    call stdlib_drscl( n, scale, work, 1_ilp )
                 end if
                 go to 10
              end if
              ! compute the estimate of the reciprocal condition number.
              if( ainvnm/=zero )rcond = ( one / anorm ) / ainvnm
           end if
           20 continue
           return
     end subroutine stdlib_dtbcon


     module subroutine stdlib_ctbcon( norm, uplo, diag, n, kd, ab, ldab, rcond, work,rwork, info )
     !! CTBCON estimates the reciprocal of the condition number of a
     !! triangular band matrix A, in either the 1-norm or the infinity-norm.
     !! The norm of A is computed and an estimate is obtained for
     !! norm(inv(A)), then the reciprocal of the condition number is
     !! computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(out) :: rcond
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: ab(ldab,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, onenrm, upper
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(sp) :: ainvnm, anorm, scale, smlnum, xnorm
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
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( ldab<kd+1 ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTBCON', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              rcond = one
              return
           end if
           rcond = zero
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )*real( max( n, 1_ilp ),KIND=sp)
           ! compute the 1-norm of the triangular matrix a or a**h.
           anorm = stdlib_clantb( norm, uplo, diag, n, kd, ab, ldab, rwork )
           ! continue only if anorm > 0.
           if( anorm>zero ) then
              ! estimate the 1-norm of the inverse of a.
              ainvnm = zero
              normin = 'N'
              if( onenrm ) then
                 kase1 = 1_ilp
              else
                 kase1 = 2_ilp
              end if
              kase = 0_ilp
              10 continue
              call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==kase1 ) then
                    ! multiply by inv(a).
                    call stdlib_clatbs( uplo, 'NO TRANSPOSE', diag, normin, n, kd,ab, ldab, work, &
                              scale, rwork, info )
                 else
                    ! multiply by inv(a**h).
                    call stdlib_clatbs( uplo, 'CONJUGATE TRANSPOSE', diag, normin,n, kd, ab, ldab,&
                               work, scale, rwork, info )
                 end if
                 normin = 'Y'
                 ! multiply by 1/scale if doing so will not cause overflow.
                 if( scale/=one ) then
                    ix = stdlib_icamax( n, work, 1_ilp )
                    xnorm = cabs1( work( ix ) )
                    if( scale<xnorm*smlnum .or. scale==zero )go to 20
                    call stdlib_csrscl( n, scale, work, 1_ilp )
                 end if
                 go to 10
              end if
              ! compute the estimate of the reciprocal condition number.
              if( ainvnm/=zero )rcond = ( one / anorm ) / ainvnm
           end if
           20 continue
           return
     end subroutine stdlib_ctbcon

     module subroutine stdlib_ztbcon( norm, uplo, diag, n, kd, ab, ldab, rcond, work,rwork, info )
     !! ZTBCON estimates the reciprocal of the condition number of a
     !! triangular band matrix A, in either the 1-norm or the infinity-norm.
     !! The norm of A is computed and an estimate is obtained for
     !! norm(inv(A)), then the reciprocal of the condition number is
     !! computed as
     !! RCOND = 1 / ( norm(A) * norm(inv(A)) ).
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(out) :: rcond
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: ab(ldab,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, onenrm, upper
           character :: normin
           integer(ilp) :: ix, kase, kase1
           real(dp) :: ainvnm, anorm, scale, smlnum, xnorm
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
           onenrm = norm=='1' .or. stdlib_lsame( norm, 'O' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.onenrm .and. .not.stdlib_lsame( norm, 'I' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( ldab<kd+1 ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTBCON', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              rcond = one
              return
           end if
           rcond = zero
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )*real( max( n, 1_ilp ),KIND=dp)
           ! compute the 1-norm of the triangular matrix a or a**h.
           anorm = stdlib_zlantb( norm, uplo, diag, n, kd, ab, ldab, rwork )
           ! continue only if anorm > 0.
           if( anorm>zero ) then
              ! estimate the 1-norm of the inverse of a.
              ainvnm = zero
              normin = 'N'
              if( onenrm ) then
                 kase1 = 1_ilp
              else
                 kase1 = 2_ilp
              end if
              kase = 0_ilp
              10 continue
              call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==kase1 ) then
                    ! multiply by inv(a).
                    call stdlib_zlatbs( uplo, 'NO TRANSPOSE', diag, normin, n, kd,ab, ldab, work, &
                              scale, rwork, info )
                 else
                    ! multiply by inv(a**h).
                    call stdlib_zlatbs( uplo, 'CONJUGATE TRANSPOSE', diag, normin,n, kd, ab, ldab,&
                               work, scale, rwork, info )
                 end if
                 normin = 'Y'
                 ! multiply by 1/scale if doing so will not cause overflow.
                 if( scale/=one ) then
                    ix = stdlib_izamax( n, work, 1_ilp )
                    xnorm = cabs1( work( ix ) )
                    if( scale<xnorm*smlnum .or. scale==zero )go to 20
                    call stdlib_zdrscl( n, scale, work, 1_ilp )
                 end if
                 go to 10
              end if
              ! compute the estimate of the reciprocal condition number.
              if( ainvnm/=zero )rcond = ( one / anorm ) / ainvnm
           end if
           20 continue
           return
     end subroutine stdlib_ztbcon




     pure module subroutine stdlib_stbtrs( uplo, trans, diag, n, kd, nrhs, ab, ldab, b,ldb, info )
     !! STBTRS solves a triangular system of the form
     !! A * X = B  or  A**T * X = B,
     !! where A is a triangular band matrix of order N, and B is an
     !! N-by NRHS matrix.  A check is made to verify that A is nonsingular.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nounit = stdlib_lsame( diag, 'N' )
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) .and. &
                     .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( nrhs<0_ilp ) then
              info = -6_ilp
           else if( ldab<kd+1 ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STBTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity.
           if( nounit ) then
              if( upper ) then
                 do info = 1, n
                    if( ab( kd+1, info )==zero )return
                 end do
              else
                 do info = 1, n
                    if( ab( 1, info )==zero )return
                 end do
              end if
           end if
           info = 0_ilp
           ! solve a * x = b  or  a**t * x = b.
           do j = 1, nrhs
              call stdlib_stbsv( uplo, trans, diag, n, kd, ab, ldab, b( 1_ilp, j ), 1_ilp )
           end do
           return
     end subroutine stdlib_stbtrs

     pure module subroutine stdlib_dtbtrs( uplo, trans, diag, n, kd, nrhs, ab, ldab, b,ldb, info )
     !! DTBTRS solves a triangular system of the form
     !! A * X = B  or  A**T * X = B,
     !! where A is a triangular band matrix of order N, and B is an
     !! N-by NRHS matrix.  A check is made to verify that A is nonsingular.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nounit = stdlib_lsame( diag, 'N' )
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) .and. &
                     .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( nrhs<0_ilp ) then
              info = -6_ilp
           else if( ldab<kd+1 ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTBTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity.
           if( nounit ) then
              if( upper ) then
                 do info = 1, n
                    if( ab( kd+1, info )==zero )return
                 end do
              else
                 do info = 1, n
                    if( ab( 1, info )==zero )return
                 end do
              end if
           end if
           info = 0_ilp
           ! solve a * x = b  or  a**t * x = b.
           do j = 1, nrhs
              call stdlib_dtbsv( uplo, trans, diag, n, kd, ab, ldab, b( 1_ilp, j ), 1_ilp )
           end do
           return
     end subroutine stdlib_dtbtrs


     pure module subroutine stdlib_ctbtrs( uplo, trans, diag, n, kd, nrhs, ab, ldab, b,ldb, info )
     !! CTBTRS solves a triangular system of the form
     !! A * X = B,  A**T * X = B,  or  A**H * X = B,
     !! where A is a triangular band matrix of order N, and B is an
     !! N-by-NRHS matrix.  A check is made to verify that A is nonsingular.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           complex(sp), intent(in) :: ab(ldab,*)
           complex(sp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nounit = stdlib_lsame( diag, 'N' )
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) .and. &
                     .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( nrhs<0_ilp ) then
              info = -6_ilp
           else if( ldab<kd+1 ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTBTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity.
           if( nounit ) then
              if( upper ) then
                 do info = 1, n
                    if( ab( kd+1, info )==czero )return
                 end do
              else
                 do info = 1, n
                    if( ab( 1, info )==czero )return
                 end do
              end if
           end if
           info = 0_ilp
           ! solve a * x = b,  a**t * x = b,  or  a**h * x = b.
           do j = 1, nrhs
              call stdlib_ctbsv( uplo, trans, diag, n, kd, ab, ldab, b( 1_ilp, j ), 1_ilp )
           end do
           return
     end subroutine stdlib_ctbtrs

     pure module subroutine stdlib_ztbtrs( uplo, trans, diag, n, kd, nrhs, ab, ldab, b,ldb, info )
     !! ZTBTRS solves a triangular system of the form
     !! A * X = B,  A**T * X = B,  or  A**H * X = B,
     !! where A is a triangular band matrix of order N, and B is an
     !! N-by-NRHS matrix.  A check is made to verify that A is nonsingular.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, n, nrhs
           ! Array Arguments 
           complex(dp), intent(in) :: ab(ldab,*)
           complex(dp), intent(inout) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: nounit, upper
           integer(ilp) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           nounit = stdlib_lsame( diag, 'N' )
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) .and. &
                     .not.stdlib_lsame( trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( nrhs<0_ilp ) then
              info = -6_ilp
           else if( ldab<kd+1 ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTBTRS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! check for singularity.
           if( nounit ) then
              if( upper ) then
                 do info = 1, n
                    if( ab( kd+1, info )==czero )return
                 end do
              else
                 do info = 1, n
                    if( ab( 1, info )==czero )return
                 end do
              end if
           end if
           info = 0_ilp
           ! solve a * x = b,  a**t * x = b,  or  a**h * x = b.
           do j = 1, nrhs
              call stdlib_ztbsv( uplo, trans, diag, n, kd, ab, ldab, b( 1_ilp, j ), 1_ilp )
           end do
           return
     end subroutine stdlib_ztbtrs




     pure module subroutine stdlib_slatbs( uplo, trans, diag, normin, n, kd, ab, ldab, x,scale, cnorm, &
     !! SLATBS solves one of the triangular systems
     !! A *x = s*b  or  A**T*x = s*b
     !! with scaling to prevent overflow, where A is an upper or lower
     !! triangular band matrix.  Here A**T denotes the transpose of A, x and b
     !! are n-element vectors, and s is a scaling factor, usually less than
     !! or equal to 1, chosen so that the components of x will be less than
     !! the overflow threshold.  If the unscaled problem will not cause
     !! overflow, the Level 2 BLAS routine STBSV is called.  If the matrix A
     !! is singular (A(j,j) = 0 for some j), then s is set to 0 and a
     !! non-trivial solution to A*x = 0 is returned.
               info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, normin, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(out) :: scale
           ! Array Arguments 
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(inout) :: cnorm(*), x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           integer(ilp) :: i, imax, j, jfirst, jinc, jlast, jlen, maind
           real(sp) :: bignum, grow, rec, smlnum, sumj, tjj, tjjs, tmax, tscal, uscal, xbnd, xj, &
                     xmax
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           ! test the input parameters.
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( normin, 'Y' ) .and. .not.stdlib_lsame( normin, 'N' ) ) &
                     then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( kd<0_ilp ) then
              info = -6_ilp
           else if( ldab<kd+1 ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLATBS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine machine dependent parameters to control overflow.
           smlnum = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           bignum = one / smlnum
           scale = one
           if( stdlib_lsame( normin, 'N' ) ) then
              ! compute the 1-norm of each column, not including the diagonal.
              if( upper ) then
                 ! a is upper triangular.
                 do j = 1, n
                    jlen = min( kd, j-1 )
                    cnorm( j ) = stdlib_sasum( jlen, ab( kd+1-jlen, j ), 1_ilp )
                 end do
              else
                 ! a is lower triangular.
                 do j = 1, n
                    jlen = min( kd, n-j )
                    if( jlen>0_ilp ) then
                       cnorm( j ) = stdlib_sasum( jlen, ab( 2_ilp, j ), 1_ilp )
                    else
                       cnorm( j ) = zero
                    end if
                 end do
              end if
           end if
           ! scale the column norms by tscal if the maximum element in cnorm is
           ! greater than bignum.
           imax = stdlib_isamax( n, cnorm, 1_ilp )
           tmax = cnorm( imax )
           if( tmax<=bignum ) then
              tscal = one
           else
              tscal = one / ( smlnum*tmax )
              call stdlib_sscal( n, tscal, cnorm, 1_ilp )
           end if
           ! compute a bound on the computed solution vector to see if the
           ! level 2 blas routine stdlib_stbsv can be used.
           j = stdlib_isamax( n, x, 1_ilp )
           xmax = abs( x( j ) )
           xbnd = xmax
           if( notran ) then
              ! compute the growth in a * x = b.
              if( upper ) then
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
                 maind = kd + 1_ilp
              else
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
                 maind = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 50
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, g(0) = max{x(i), i=1,...,n}.
                 grow = one / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 50
                    ! m(j) = g(j-1) / abs(a(j,j))
                    tjj = abs( ab( maind, j ) )
                    xbnd = min( xbnd, min( one, tjj )*grow )
                    if( tjj+cnorm( j )>=smlnum ) then
                       ! g(j) = g(j-1)*( 1 + cnorm(j) / abs(a(j,j)) )
                       grow = grow*( tjj / ( tjj+cnorm( j ) ) )
                    else
                       ! g(j) could overflow, set grow to 0.
                       grow = zero
                    end if
                 end do
                 grow = xbnd
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, one / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 50
                    ! g(j) = g(j-1)*( 1 + cnorm(j) )
                    grow = grow*( one / ( one+cnorm( j ) ) )
                 end do
              end if
              50 continue
           else
              ! compute the growth in a**t * x = b.
              if( upper ) then
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
                 maind = kd + 1_ilp
              else
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
                 maind = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 80
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, m(0) = max{x(i), i=1,...,n}.
                 grow = one / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 80
                    ! g(j) = max( g(j-1), m(j-1)*( 1 + cnorm(j) ) )
                    xj = one + cnorm( j )
                    grow = min( grow, xbnd / xj )
                    ! m(j) = m(j-1)*( 1 + cnorm(j) ) / abs(a(j,j))
                    tjj = abs( ab( maind, j ) )
                    if( xj>tjj )xbnd = xbnd*( tjj / xj )
                 end do
                 grow = min( grow, xbnd )
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, one / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 80
                    ! g(j) = ( 1 + cnorm(j) )*g(j-1)
                    xj = one + cnorm( j )
                    grow = grow / xj
                 end do
              end if
              80 continue
           end if
           if( ( grow*tscal )>smlnum ) then
              ! use the level 2 blas solve if the reciprocal of the bound on
              ! elements of x is not too small.
              call stdlib_stbsv( uplo, trans, diag, n, kd, ab, ldab, x, 1_ilp )
           else
              ! use a level 1 blas solve, scaling intermediate results.
              if( xmax>bignum ) then
                 ! scale x so that its components are less than or equal to
                 ! bignum in absolute value.
                 scale = bignum / xmax
                 call stdlib_sscal( n, scale, x, 1_ilp )
                 xmax = bignum
              end if
              if( notran ) then
                 ! solve a * x = b
                 loop_100: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) / a(j,j), scaling x if necessary.
                    xj = abs( x( j ) )
                    if( nounit ) then
                       tjjs = ab( maind, j )*tscal
                    else
                       tjjs = tscal
                       if( tscal==one )go to 95
                    end if
                       tjj = abs( tjjs )
                       if( tjj>smlnum ) then
                          ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                ! scale x by 1/b(j).
                                rec = one / xj
                                call stdlib_sscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = x( j ) / tjjs
                          xj = abs( x( j ) )
                       else if( tjj>zero ) then
                          ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                             ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum
                             ! to avoid overflow when dividing by a(j,j).
                             rec = ( tjj*bignum ) / xj
                             if( cnorm( j )>one ) then
                                ! scale by 1/cnorm(j) to avoid overflow when
                                ! multiplying x(j) times column j.
                                rec = rec / cnorm( j )
                             end if
                             call stdlib_sscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = x( j ) / tjjs
                          xj = abs( x( j ) )
                       else
                          ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                          ! scale = 0, and compute a solution to a*x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          xj = one
                          scale = zero
                          xmax = zero
                       end if
                       95 continue
                    ! scale x if necessary to avoid overflow when adding a
                    ! multiple of column j of a.
                    if( xj>one ) then
                       rec = one / xj
                       if( cnorm( j )>( bignum-xmax )*rec ) then
                          ! scale x by 1/(2*abs(x(j))).
                          rec = rec*half
                          call stdlib_sscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                       end if
                    else if( xj*cnorm( j )>( bignum-xmax ) ) then
                       ! scale x by 1/2.
                       call stdlib_sscal( n, half, x, 1_ilp )
                       scale = scale*half
                    end if
                    if( upper ) then
                       if( j>1_ilp ) then
                          ! compute the update
                             ! x(max(1,j-kd):j-1) := x(max(1,j-kd):j-1) -
                                                   ! x(j)* a(max(1,j-kd):j-1,j)
                          jlen = min( kd, j-1 )
                          call stdlib_saxpy( jlen, -x( j )*tscal,ab( kd+1-jlen, j ), 1_ilp, x( j-jlen &
                                    ), 1_ilp )
                          i = stdlib_isamax( j-1, x, 1_ilp )
                          xmax = abs( x( i ) )
                       end if
                    else if( j<n ) then
                       ! compute the update
                          ! x(j+1:min(j+kd,n)) := x(j+1:min(j+kd,n)) -
                                                ! x(j) * a(j+1:min(j+kd,n),j)
                       jlen = min( kd, n-j )
                       if( jlen>0_ilp )call stdlib_saxpy( jlen, -x( j )*tscal, ab( 2_ilp, j ), 1_ilp,x( j+1 ),&
                                  1_ilp )
                       i = j + stdlib_isamax( n-j, x( j+1 ), 1_ilp )
                       xmax = abs( x( i ) )
                    end if
                 end do loop_100
              else
                 ! solve a**t * x = b
                 loop_140: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = abs( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = ab( maind, j )*tscal
                       else
                          tjjs = tscal
                       end if
                          tjj = abs( tjjs )
                          if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                             rec = min( one, rec*tjj )
                             uscal = uscal / tjjs
                          end if
                       if( rec<one ) then
                          call stdlib_sscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    sumj = zero
                    if( uscal==one ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_sdot to perform the dot product.
                       if( upper ) then
                          jlen = min( kd, j-1 )
                          sumj = stdlib_sdot( jlen, ab( kd+1-jlen, j ), 1_ilp,x( j-jlen ), 1_ilp )
                       else
                          jlen = min( kd, n-j )
                          if( jlen>0_ilp )sumj = stdlib_sdot( jlen, ab( 2_ilp, j ), 1_ilp, x( j+1 ), 1_ilp )
                                    
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          jlen = min( kd, j-1 )
                          do i = 1, jlen
                             sumj = sumj + ( ab( kd+i-jlen, j )*uscal )*x( j-jlen-1+i )
                          end do
                       else
                          jlen = min( kd, n-j )
                          do i = 1, jlen
                             sumj = sumj + ( ab( i+1, j )*uscal )*x( j+i )
                          end do
                       end if
                    end if
                    if( uscal==tscal ) then
                       ! compute x(j) := ( x(j) - sumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - sumj
                       xj = abs( x( j ) )
                       if( nounit ) then
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjjs = ab( maind, j )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 135
                       end if
                          tjj = abs( tjjs )
                          if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                             if( tjj<one ) then
                                if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                   rec = one / xj
                                   call stdlib_sscal( n, rec, x, 1_ilp )
                                   scale = scale*rec
                                   xmax = xmax*rec
                                end if
                             end if
                             x( j ) = x( j ) / tjjs
                          else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                             if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                                rec = ( tjj*bignum ) / xj
                                call stdlib_sscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                             x( j ) = x( j ) / tjjs
                          else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0, and compute a solution to a**t*x = 0.
                             do i = 1, n
                                x( i ) = zero
                             end do
                             x( j ) = one
                             scale = zero
                             xmax = zero
                          end if
                          135 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - sumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = x( j ) / tjjs - sumj
                    end if
                    xmax = max( xmax, abs( x( j ) ) )
                 end do loop_140
              end if
              scale = scale / tscal
           end if
           ! scale the column norms by 1/tscal for return.
           if( tscal/=one ) then
              call stdlib_sscal( n, one / tscal, cnorm, 1_ilp )
           end if
           return
     end subroutine stdlib_slatbs

     pure module subroutine stdlib_dlatbs( uplo, trans, diag, normin, n, kd, ab, ldab, x,scale, cnorm, &
     !! DLATBS solves one of the triangular systems
     !! A *x = s*b  or  A**T*x = s*b
     !! with scaling to prevent overflow, where A is an upper or lower
     !! triangular band matrix.  Here A**T denotes the transpose of A, x and b
     !! are n-element vectors, and s is a scaling factor, usually less than
     !! or equal to 1, chosen so that the components of x will be less than
     !! the overflow threshold.  If the unscaled problem will not cause
     !! overflow, the Level 2 BLAS routine DTBSV is called.  If the matrix A
     !! is singular (A(j,j) = 0 for some j), then s is set to 0 and a
     !! non-trivial solution to A*x = 0 is returned.
               info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, normin, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(out) :: scale
           ! Array Arguments 
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(inout) :: cnorm(*), x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           integer(ilp) :: i, imax, j, jfirst, jinc, jlast, jlen, maind
           real(dp) :: bignum, grow, rec, smlnum, sumj, tjj, tjjs, tmax, tscal, uscal, xbnd, xj, &
                     xmax
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           ! test the input parameters.
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( normin, 'Y' ) .and. .not.stdlib_lsame( normin, 'N' ) ) &
                     then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( kd<0_ilp ) then
              info = -6_ilp
           else if( ldab<kd+1 ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLATBS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine machine dependent parameters to control overflow.
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           bignum = one / smlnum
           scale = one
           if( stdlib_lsame( normin, 'N' ) ) then
              ! compute the 1-norm of each column, not including the diagonal.
              if( upper ) then
                 ! a is upper triangular.
                 do j = 1, n
                    jlen = min( kd, j-1 )
                    cnorm( j ) = stdlib_dasum( jlen, ab( kd+1-jlen, j ), 1_ilp )
                 end do
              else
                 ! a is lower triangular.
                 do j = 1, n
                    jlen = min( kd, n-j )
                    if( jlen>0_ilp ) then
                       cnorm( j ) = stdlib_dasum( jlen, ab( 2_ilp, j ), 1_ilp )
                    else
                       cnorm( j ) = zero
                    end if
                 end do
              end if
           end if
           ! scale the column norms by tscal if the maximum element in cnorm is
           ! greater than bignum.
           imax = stdlib_idamax( n, cnorm, 1_ilp )
           tmax = cnorm( imax )
           if( tmax<=bignum ) then
              tscal = one
           else
              tscal = one / ( smlnum*tmax )
              call stdlib_dscal( n, tscal, cnorm, 1_ilp )
           end if
           ! compute a bound on the computed solution vector to see if the
           ! level 2 blas routine stdlib_dtbsv can be used.
           j = stdlib_idamax( n, x, 1_ilp )
           xmax = abs( x( j ) )
           xbnd = xmax
           if( notran ) then
              ! compute the growth in a * x = b.
              if( upper ) then
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
                 maind = kd + 1_ilp
              else
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
                 maind = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 50
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, g(0) = max{x(i), i=1,...,n}.
                 grow = one / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 50
                    ! m(j) = g(j-1) / abs(a(j,j))
                    tjj = abs( ab( maind, j ) )
                    xbnd = min( xbnd, min( one, tjj )*grow )
                    if( tjj+cnorm( j )>=smlnum ) then
                       ! g(j) = g(j-1)*( 1 + cnorm(j) / abs(a(j,j)) )
                       grow = grow*( tjj / ( tjj+cnorm( j ) ) )
                    else
                       ! g(j) could overflow, set grow to 0.
                       grow = zero
                    end if
                 end do
                 grow = xbnd
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, one / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 50
                    ! g(j) = g(j-1)*( 1 + cnorm(j) )
                    grow = grow*( one / ( one+cnorm( j ) ) )
                 end do
              end if
              50 continue
           else
              ! compute the growth in a**t * x = b.
              if( upper ) then
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
                 maind = kd + 1_ilp
              else
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
                 maind = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 80
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, m(0) = max{x(i), i=1,...,n}.
                 grow = one / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 80
                    ! g(j) = max( g(j-1), m(j-1)*( 1 + cnorm(j) ) )
                    xj = one + cnorm( j )
                    grow = min( grow, xbnd / xj )
                    ! m(j) = m(j-1)*( 1 + cnorm(j) ) / abs(a(j,j))
                    tjj = abs( ab( maind, j ) )
                    if( xj>tjj )xbnd = xbnd*( tjj / xj )
                 end do
                 grow = min( grow, xbnd )
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, one / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 80
                    ! g(j) = ( 1 + cnorm(j) )*g(j-1)
                    xj = one + cnorm( j )
                    grow = grow / xj
                 end do
              end if
              80 continue
           end if
           if( ( grow*tscal )>smlnum ) then
              ! use the level 2 blas solve if the reciprocal of the bound on
              ! elements of x is not too small.
              call stdlib_dtbsv( uplo, trans, diag, n, kd, ab, ldab, x, 1_ilp )
           else
              ! use a level 1 blas solve, scaling intermediate results.
              if( xmax>bignum ) then
                 ! scale x so that its components are less than or equal to
                 ! bignum in absolute value.
                 scale = bignum / xmax
                 call stdlib_dscal( n, scale, x, 1_ilp )
                 xmax = bignum
              end if
              if( notran ) then
                 ! solve a * x = b
                 loop_110: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) / a(j,j), scaling x if necessary.
                    xj = abs( x( j ) )
                    if( nounit ) then
                       tjjs = ab( maind, j )*tscal
                    else
                       tjjs = tscal
                       if( tscal==one )go to 100
                    end if
                    tjj = abs( tjjs )
                    if( tjj>smlnum ) then
                          ! abs(a(j,j)) > smlnum:
                       if( tjj<one ) then
                          if( xj>tjj*bignum ) then
                                ! scale x by 1/b(j).
                             rec = one / xj
                             call stdlib_dscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j ) = x( j ) / tjjs
                       xj = abs( x( j ) )
                    else if( tjj>zero ) then
                          ! 0 < abs(a(j,j)) <= smlnum:
                       if( xj>tjj*bignum ) then
                             ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum
                             ! to avoid overflow when dividing by a(j,j).
                          rec = ( tjj*bignum ) / xj
                          if( cnorm( j )>one ) then
                                ! scale by 1/cnorm(j) to avoid overflow when
                                ! multiplying x(j) times column j.
                             rec = rec / cnorm( j )
                          end if
                          call stdlib_dscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                       x( j ) = x( j ) / tjjs
                       xj = abs( x( j ) )
                    else
                          ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                          ! scale = 0, and compute a solution to a*x = 0.
                       do i = 1, n
                          x( i ) = zero
                       end do
                       x( j ) = one
                       xj = one
                       scale = zero
                       xmax = zero
                    end if
                    100 continue
                    ! scale x if necessary to avoid overflow when adding a
                    ! multiple of column j of a.
                    if( xj>one ) then
                       rec = one / xj
                       if( cnorm( j )>( bignum-xmax )*rec ) then
                          ! scale x by 1/(2*abs(x(j))).
                          rec = rec*half
                          call stdlib_dscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                       end if
                    else if( xj*cnorm( j )>( bignum-xmax ) ) then
                       ! scale x by 1/2.
                       call stdlib_dscal( n, half, x, 1_ilp )
                       scale = scale*half
                    end if
                    if( upper ) then
                       if( j>1_ilp ) then
                          ! compute the update
                             ! x(max(1,j-kd):j-1) := x(max(1,j-kd):j-1) -
                                                   ! x(j)* a(max(1,j-kd):j-1,j)
                          jlen = min( kd, j-1 )
                          call stdlib_daxpy( jlen, -x( j )*tscal,ab( kd+1-jlen, j ), 1_ilp, x( j-jlen &
                                    ), 1_ilp )
                          i = stdlib_idamax( j-1, x, 1_ilp )
                          xmax = abs( x( i ) )
                       end if
                    else if( j<n ) then
                       ! compute the update
                          ! x(j+1:min(j+kd,n)) := x(j+1:min(j+kd,n)) -
                                                ! x(j) * a(j+1:min(j+kd,n),j)
                       jlen = min( kd, n-j )
                       if( jlen>0_ilp )call stdlib_daxpy( jlen, -x( j )*tscal, ab( 2_ilp, j ), 1_ilp,x( j+1 ),&
                                  1_ilp )
                       i = j + stdlib_idamax( n-j, x( j+1 ), 1_ilp )
                       xmax = abs( x( i ) )
                    end if
                 end do loop_110
              else
                 ! solve a**t * x = b
                 loop_160: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = abs( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = ab( maind, j )*tscal
                       else
                          tjjs = tscal
                       end if
                       tjj = abs( tjjs )
                       if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                          rec = min( one, rec*tjj )
                          uscal = uscal / tjjs
                       end if
                       if( rec<one ) then
                          call stdlib_dscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    sumj = zero
                    if( uscal==one ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_ddot to perform the dot product.
                       if( upper ) then
                          jlen = min( kd, j-1 )
                          sumj = stdlib_ddot( jlen, ab( kd+1-jlen, j ), 1_ilp,x( j-jlen ), 1_ilp )
                       else
                          jlen = min( kd, n-j )
                          if( jlen>0_ilp )sumj = stdlib_ddot( jlen, ab( 2_ilp, j ), 1_ilp, x( j+1 ), 1_ilp )
                                    
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          jlen = min( kd, j-1 )
                          do i = 1, jlen
                             sumj = sumj + ( ab( kd+i-jlen, j )*uscal )*x( j-jlen-1+i )
                          end do
                       else
                          jlen = min( kd, n-j )
                          do i = 1, jlen
                             sumj = sumj + ( ab( i+1, j )*uscal )*x( j+i )
                          end do
                       end if
                    end if
                    if( uscal==tscal ) then
                       ! compute x(j) := ( x(j) - sumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - sumj
                       xj = abs( x( j ) )
                       if( nounit ) then
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjjs = ab( maind, j )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 150
                       end if
                       tjj = abs( tjjs )
                       if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                rec = one / xj
                                call stdlib_dscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = x( j ) / tjjs
                       else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                             rec = ( tjj*bignum ) / xj
                             call stdlib_dscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = x( j ) / tjjs
                       else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0, and compute a solution to a**t*x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          scale = zero
                          xmax = zero
                       end if
                       150 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - sumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = x( j ) / tjjs - sumj
                    end if
                    xmax = max( xmax, abs( x( j ) ) )
                 end do loop_160
              end if
              scale = scale / tscal
           end if
           ! scale the column norms by 1/tscal for return.
           if( tscal/=one ) then
              call stdlib_dscal( n, one / tscal, cnorm, 1_ilp )
           end if
           return
     end subroutine stdlib_dlatbs


     pure module subroutine stdlib_clatbs( uplo, trans, diag, normin, n, kd, ab, ldab, x,scale, cnorm, &
     !! CLATBS solves one of the triangular systems
     !! A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b,
     !! with scaling to prevent overflow, where A is an upper or lower
     !! triangular band matrix.  Here A**T denotes the transpose of A, x and b
     !! are n-element vectors, and s is a scaling factor, usually less than
     !! or equal to 1, chosen so that the components of x will be less than
     !! the overflow threshold.  If the unscaled problem will not cause
     !! overflow, the Level 2 BLAS routine CTBSV is called.  If the matrix A
     !! is singular (A(j,j) = 0 for some j), then s is set to 0 and a
     !! non-trivial solution to A*x = 0 is returned.
               info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, normin, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(out) :: scale
           ! Array Arguments 
           real(sp), intent(inout) :: cnorm(*)
           complex(sp), intent(in) :: ab(ldab,*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           integer(ilp) :: i, imax, j, jfirst, jinc, jlast, jlen, maind
           real(sp) :: bignum, grow, rec, smlnum, tjj, tmax, tscal, xbnd, xj, xmax
           complex(sp) :: csumj, tjjs, uscal, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1, cabs2
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           cabs2( zdum ) = abs( real( zdum,KIND=sp) / 2. ) +abs( aimag( zdum ) / 2. )
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           ! test the input parameters.
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( normin, 'Y' ) .and. .not.stdlib_lsame( normin, 'N' ) ) &
                     then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( kd<0_ilp ) then
              info = -6_ilp
           else if( ldab<kd+1 ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLATBS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine machine dependent parameters to control overflow.
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           smlnum = smlnum / stdlib_slamch( 'PRECISION' )
           bignum = one / smlnum
           scale = one
           if( stdlib_lsame( normin, 'N' ) ) then
              ! compute the 1-norm of each column, not including the diagonal.
              if( upper ) then
                 ! a is upper triangular.
                 do j = 1, n
                    jlen = min( kd, j-1 )
                    cnorm( j ) = stdlib_scasum( jlen, ab( kd+1-jlen, j ), 1_ilp )
                 end do
              else
                 ! a is lower triangular.
                 do j = 1, n
                    jlen = min( kd, n-j )
                    if( jlen>0_ilp ) then
                       cnorm( j ) = stdlib_scasum( jlen, ab( 2_ilp, j ), 1_ilp )
                    else
                       cnorm( j ) = zero
                    end if
                 end do
              end if
           end if
           ! scale the column norms by tscal if the maximum element in cnorm is
           ! greater than bignum/2.
           imax = stdlib_isamax( n, cnorm, 1_ilp )
           tmax = cnorm( imax )
           if( tmax<=bignum*half ) then
              tscal = one
           else
              tscal = half / ( smlnum*tmax )
              call stdlib_sscal( n, tscal, cnorm, 1_ilp )
           end if
           ! compute a bound on the computed solution vector to see if the
           ! level 2 blas routine stdlib_ctbsv can be used.
           xmax = zero
           do j = 1, n
              xmax = max( xmax, cabs2( x( j ) ) )
           end do
           xbnd = xmax
           if( notran ) then
              ! compute the growth in a * x = b.
              if( upper ) then
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
                 maind = kd + 1_ilp
              else
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
                 maind = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 60
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, g(0) = max{x(i), i=1,...,n}.
                 grow = half / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 60
                    tjjs = ab( maind, j )
                    tjj = cabs1( tjjs )
                    if( tjj>=smlnum ) then
                       ! m(j) = g(j-1) / abs(a(j,j))
                       xbnd = min( xbnd, min( one, tjj )*grow )
                    else
                       ! m(j) could overflow, set xbnd to 0.
                       xbnd = zero
                    end if
                    if( tjj+cnorm( j )>=smlnum ) then
                       ! g(j) = g(j-1)*( 1 + cnorm(j) / abs(a(j,j)) )
                       grow = grow*( tjj / ( tjj+cnorm( j ) ) )
                    else
                       ! g(j) could overflow, set grow to 0.
                       grow = zero
                    end if
                 end do
                 grow = xbnd
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, half / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 60
                    ! g(j) = g(j-1)*( 1 + cnorm(j) )
                    grow = grow*( one / ( one+cnorm( j ) ) )
                 end do
              end if
              60 continue
           else
              ! compute the growth in a**t * x = b  or  a**h * x = b.
              if( upper ) then
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
                 maind = kd + 1_ilp
              else
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
                 maind = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 90
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, m(0) = max{x(i), i=1,...,n}.
                 grow = half / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 90
                    ! g(j) = max( g(j-1), m(j-1)*( 1 + cnorm(j) ) )
                    xj = one + cnorm( j )
                    grow = min( grow, xbnd / xj )
                    tjjs = ab( maind, j )
                    tjj = cabs1( tjjs )
                    if( tjj>=smlnum ) then
                       ! m(j) = m(j-1)*( 1 + cnorm(j) ) / abs(a(j,j))
                       if( xj>tjj )xbnd = xbnd*( tjj / xj )
                    else
                       ! m(j) could overflow, set xbnd to 0.
                       xbnd = zero
                    end if
                 end do
                 grow = min( grow, xbnd )
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, half / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 90
                    ! g(j) = ( 1 + cnorm(j) )*g(j-1)
                    xj = one + cnorm( j )
                    grow = grow / xj
                 end do
              end if
              90 continue
           end if
           if( ( grow*tscal )>smlnum ) then
              ! use the level 2 blas solve if the reciprocal of the bound on
              ! elements of x is not too small.
              call stdlib_ctbsv( uplo, trans, diag, n, kd, ab, ldab, x, 1_ilp )
           else
              ! use a level 1 blas solve, scaling intermediate results.
              if( xmax>bignum*half ) then
                 ! scale x so that its components are less than or equal to
                 ! bignum in absolute value.
                 scale = ( bignum*half ) / xmax
                 call stdlib_csscal( n, scale, x, 1_ilp )
                 xmax = bignum
              else
                 xmax = xmax*two
              end if
              if( notran ) then
                 ! solve a * x = b
                 loop_110: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) / a(j,j), scaling x if necessary.
                    xj = cabs1( x( j ) )
                    if( nounit ) then
                       tjjs = ab( maind, j )*tscal
                    else
                       tjjs = tscal
                       if( tscal==one )go to 105
                    end if
                       tjj = cabs1( tjjs )
                       if( tjj>smlnum ) then
                          ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                ! scale x by 1/b(j).
                                rec = one / xj
                                call stdlib_csscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = stdlib_cladiv( x( j ), tjjs )
                          xj = cabs1( x( j ) )
                       else if( tjj>zero ) then
                          ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                             ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum
                             ! to avoid overflow when dividing by a(j,j).
                             rec = ( tjj*bignum ) / xj
                             if( cnorm( j )>one ) then
                                ! scale by 1/cnorm(j) to avoid overflow when
                                ! multiplying x(j) times column j.
                                rec = rec / cnorm( j )
                             end if
                             call stdlib_csscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = stdlib_cladiv( x( j ), tjjs )
                          xj = cabs1( x( j ) )
                       else
                          ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                          ! scale = 0, and compute a solution to a*x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          xj = one
                          scale = zero
                          xmax = zero
                       end if
                       105 continue
                    ! scale x if necessary to avoid overflow when adding a
                    ! multiple of column j of a.
                    if( xj>one ) then
                       rec = one / xj
                       if( cnorm( j )>( bignum-xmax )*rec ) then
                          ! scale x by 1/(2*abs(x(j))).
                          rec = rec*half
                          call stdlib_csscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                       end if
                    else if( xj*cnorm( j )>( bignum-xmax ) ) then
                       ! scale x by 1/2.
                       call stdlib_csscal( n, half, x, 1_ilp )
                       scale = scale*half
                    end if
                    if( upper ) then
                       if( j>1_ilp ) then
                          ! compute the update
                             ! x(max(1,j-kd):j-1) := x(max(1,j-kd):j-1) -
                                                   ! x(j)* a(max(1,j-kd):j-1,j)
                          jlen = min( kd, j-1 )
                          call stdlib_caxpy( jlen, -x( j )*tscal,ab( kd+1-jlen, j ), 1_ilp, x( j-jlen &
                                    ), 1_ilp )
                          i = stdlib_icamax( j-1, x, 1_ilp )
                          xmax = cabs1( x( i ) )
                       end if
                    else if( j<n ) then
                       ! compute the update
                          ! x(j+1:min(j+kd,n)) := x(j+1:min(j+kd,n)) -
                                                ! x(j) * a(j+1:min(j+kd,n),j)
                       jlen = min( kd, n-j )
                       if( jlen>0_ilp )call stdlib_caxpy( jlen, -x( j )*tscal, ab( 2_ilp, j ), 1_ilp,x( j+1 ),&
                                  1_ilp )
                       i = j + stdlib_icamax( n-j, x( j+1 ), 1_ilp )
                       xmax = cabs1( x( i ) )
                    end if
                 end do loop_110
              else if( stdlib_lsame( trans, 'T' ) ) then
                 ! solve a**t * x = b
                 loop_150: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = cabs1( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = ab( maind, j )*tscal
                       else
                          tjjs = tscal
                       end if
                          tjj = cabs1( tjjs )
                          if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                             rec = min( one, rec*tjj )
                             uscal = stdlib_cladiv( uscal, tjjs )
                          end if
                       if( rec<one ) then
                          call stdlib_csscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    csumj = zero
                    if( uscal==cmplx( one,KIND=sp) ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_cdotu to perform the dot product.
                       if( upper ) then
                          jlen = min( kd, j-1 )
                          csumj = stdlib_cdotu( jlen, ab( kd+1-jlen, j ), 1_ilp,x( j-jlen ), 1_ilp )
                                    
                       else
                          jlen = min( kd, n-j )
                          if( jlen>1_ilp )csumj = stdlib_cdotu( jlen, ab( 2_ilp, j ), 1_ilp, x( j+1 ),1_ilp )
                                    
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          jlen = min( kd, j-1 )
                          do i = 1, jlen
                             csumj = csumj + ( ab( kd+i-jlen, j )*uscal )*x( j-jlen-1+i )
                          end do
                       else
                          jlen = min( kd, n-j )
                          do i = 1, jlen
                             csumj = csumj + ( ab( i+1, j )*uscal )*x( j+i )
                          end do
                       end if
                    end if
                    if( uscal==cmplx( tscal,KIND=sp) ) then
                       ! compute x(j) := ( x(j) - csumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - csumj
                       xj = cabs1( x( j ) )
                       if( nounit ) then
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjjs = ab( maind, j )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 145
                       end if
                          tjj = cabs1( tjjs )
                          if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                             if( tjj<one ) then
                                if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                   rec = one / xj
                                   call stdlib_csscal( n, rec, x, 1_ilp )
                                   scale = scale*rec
                                   xmax = xmax*rec
                                end if
                             end if
                             x( j ) = stdlib_cladiv( x( j ), tjjs )
                          else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                             if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                                rec = ( tjj*bignum ) / xj
                                call stdlib_csscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                             x( j ) = stdlib_cladiv( x( j ), tjjs )
                          else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0 and compute a solution to a**t *x = 0.
                             do i = 1, n
                                x( i ) = zero
                             end do
                             x( j ) = one
                             scale = zero
                             xmax = zero
                          end if
                          145 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - csumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = stdlib_cladiv( x( j ), tjjs ) - csumj
                    end if
                    xmax = max( xmax, cabs1( x( j ) ) )
                 end do loop_150
              else
                 ! solve a**h * x = b
                 loop_190: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = cabs1( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = conjg( ab( maind, j ) )*tscal
                       else
                          tjjs = tscal
                       end if
                          tjj = cabs1( tjjs )
                          if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                             rec = min( one, rec*tjj )
                             uscal = stdlib_cladiv( uscal, tjjs )
                          end if
                       if( rec<one ) then
                          call stdlib_csscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    csumj = zero
                    if( uscal==cmplx( one,KIND=sp) ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_cdotc to perform the dot product.
                       if( upper ) then
                          jlen = min( kd, j-1 )
                          csumj = stdlib_cdotc( jlen, ab( kd+1-jlen, j ), 1_ilp,x( j-jlen ), 1_ilp )
                                    
                       else
                          jlen = min( kd, n-j )
                          if( jlen>1_ilp )csumj = stdlib_cdotc( jlen, ab( 2_ilp, j ), 1_ilp, x( j+1 ),1_ilp )
                                    
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          jlen = min( kd, j-1 )
                          do i = 1, jlen
                             csumj = csumj + ( conjg( ab( kd+i-jlen, j ) )*uscal )*x( j-jlen-1+i )
                                       
                          end do
                       else
                          jlen = min( kd, n-j )
                          do i = 1, jlen
                             csumj = csumj + ( conjg( ab( i+1, j ) )*uscal )*x( j+i )
                          end do
                       end if
                    end if
                    if( uscal==cmplx( tscal,KIND=sp) ) then
                       ! compute x(j) := ( x(j) - csumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - csumj
                       xj = cabs1( x( j ) )
                       if( nounit ) then
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjjs = conjg( ab( maind, j ) )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 185
                       end if
                          tjj = cabs1( tjjs )
                          if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                             if( tjj<one ) then
                                if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                   rec = one / xj
                                   call stdlib_csscal( n, rec, x, 1_ilp )
                                   scale = scale*rec
                                   xmax = xmax*rec
                                end if
                             end if
                             x( j ) = stdlib_cladiv( x( j ), tjjs )
                          else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                             if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                                rec = ( tjj*bignum ) / xj
                                call stdlib_csscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                             x( j ) = stdlib_cladiv( x( j ), tjjs )
                          else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0 and compute a solution to a**h *x = 0.
                             do i = 1, n
                                x( i ) = zero
                             end do
                             x( j ) = one
                             scale = zero
                             xmax = zero
                          end if
                          185 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - csumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = stdlib_cladiv( x( j ), tjjs ) - csumj
                    end if
                    xmax = max( xmax, cabs1( x( j ) ) )
                 end do loop_190
              end if
              scale = scale / tscal
           end if
           ! scale the column norms by 1/tscal for return.
           if( tscal/=one ) then
              call stdlib_sscal( n, one / tscal, cnorm, 1_ilp )
           end if
           return
     end subroutine stdlib_clatbs

     pure module subroutine stdlib_zlatbs( uplo, trans, diag, normin, n, kd, ab, ldab, x,scale, cnorm, &
     !! ZLATBS solves one of the triangular systems
     !! A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b,
     !! with scaling to prevent overflow, where A is an upper or lower
     !! triangular band matrix.  Here A**T denotes the transpose of A, x and b
     !! are n-element vectors, and s is a scaling factor, usually less than
     !! or equal to 1, chosen so that the components of x will be less than
     !! the overflow threshold.  If the unscaled problem will not cause
     !! overflow, the Level 2 BLAS routine ZTBSV is called.  If the matrix A
     !! is singular (A(j,j) = 0 for some j), then s is set to 0 and a
     !! non-trivial solution to A*x = 0 is returned.
               info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, normin, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(out) :: scale
           ! Array Arguments 
           real(dp), intent(inout) :: cnorm(*)
           complex(dp), intent(in) :: ab(ldab,*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           integer(ilp) :: i, imax, j, jfirst, jinc, jlast, jlen, maind
           real(dp) :: bignum, grow, rec, smlnum, tjj, tmax, tscal, xbnd, xj, xmax
           complex(dp) :: csumj, tjjs, uscal, zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1, cabs2
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           cabs2( zdum ) = abs( real( zdum,KIND=dp) / 2._dp ) +abs( aimag( zdum ) / 2._dp )
                     
           ! Executable Statements 
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           ! test the input parameters.
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( .not.stdlib_lsame( normin, 'Y' ) .and. .not.stdlib_lsame( normin, 'N' ) ) &
                     then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( kd<0_ilp ) then
              info = -6_ilp
           else if( ldab<kd+1 ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLATBS', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine machine dependent parameters to control overflow.
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           smlnum = smlnum / stdlib_dlamch( 'PRECISION' )
           bignum = one / smlnum
           scale = one
           if( stdlib_lsame( normin, 'N' ) ) then
              ! compute the 1-norm of each column, not including the diagonal.
              if( upper ) then
                 ! a is upper triangular.
                 do j = 1, n
                    jlen = min( kd, j-1 )
                    cnorm( j ) = stdlib_dzasum( jlen, ab( kd+1-jlen, j ), 1_ilp )
                 end do
              else
                 ! a is lower triangular.
                 do j = 1, n
                    jlen = min( kd, n-j )
                    if( jlen>0_ilp ) then
                       cnorm( j ) = stdlib_dzasum( jlen, ab( 2_ilp, j ), 1_ilp )
                    else
                       cnorm( j ) = zero
                    end if
                 end do
              end if
           end if
           ! scale the column norms by tscal if the maximum element in cnorm is
           ! greater than bignum/2.
           imax = stdlib_idamax( n, cnorm, 1_ilp )
           tmax = cnorm( imax )
           if( tmax<=bignum*half ) then
              tscal = one
           else
              tscal = half / ( smlnum*tmax )
              call stdlib_dscal( n, tscal, cnorm, 1_ilp )
           end if
           ! compute a bound on the computed solution vector to see if the
           ! level 2 blas routine stdlib_ztbsv can be used.
           xmax = zero
           do j = 1, n
              xmax = max( xmax, cabs2( x( j ) ) )
           end do
           xbnd = xmax
           if( notran ) then
              ! compute the growth in a * x = b.
              if( upper ) then
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
                 maind = kd + 1_ilp
              else
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
                 maind = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 60
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, g(0) = max{x(i), i=1,...,n}.
                 grow = half / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 60
                    tjjs = ab( maind, j )
                    tjj = cabs1( tjjs )
                    if( tjj>=smlnum ) then
                       ! m(j) = g(j-1) / abs(a(j,j))
                       xbnd = min( xbnd, min( one, tjj )*grow )
                    else
                       ! m(j) could overflow, set xbnd to 0.
                       xbnd = zero
                    end if
                    if( tjj+cnorm( j )>=smlnum ) then
                       ! g(j) = g(j-1)*( 1 + cnorm(j) / abs(a(j,j)) )
                       grow = grow*( tjj / ( tjj+cnorm( j ) ) )
                    else
                       ! g(j) could overflow, set grow to 0.
                       grow = zero
                    end if
                 end do
                 grow = xbnd
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, half / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 60
                    ! g(j) = g(j-1)*( 1 + cnorm(j) )
                    grow = grow*( one / ( one+cnorm( j ) ) )
                 end do
              end if
              60 continue
           else
              ! compute the growth in a**t * x = b  or  a**h * x = b.
              if( upper ) then
                 jfirst = 1_ilp
                 jlast = n
                 jinc = 1_ilp
                 maind = kd + 1_ilp
              else
                 jfirst = n
                 jlast = 1_ilp
                 jinc = -1_ilp
                 maind = 1_ilp
              end if
              if( tscal/=one ) then
                 grow = zero
                 go to 90
              end if
              if( nounit ) then
                 ! a is non-unit triangular.
                 ! compute grow = 1/g(j) and xbnd = 1/m(j).
                 ! initially, m(0) = max{x(i), i=1,...,n}.
                 grow = half / max( xbnd, smlnum )
                 xbnd = grow
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 90
                    ! g(j) = max( g(j-1), m(j-1)*( 1 + cnorm(j) ) )
                    xj = one + cnorm( j )
                    grow = min( grow, xbnd / xj )
                    tjjs = ab( maind, j )
                    tjj = cabs1( tjjs )
                    if( tjj>=smlnum ) then
                       ! m(j) = m(j-1)*( 1 + cnorm(j) ) / abs(a(j,j))
                       if( xj>tjj )xbnd = xbnd*( tjj / xj )
                    else
                       ! m(j) could overflow, set xbnd to 0.
                       xbnd = zero
                    end if
                 end do
                 grow = min( grow, xbnd )
              else
                 ! a is unit triangular.
                 ! compute grow = 1/g(j), where g(0) = max{x(i), i=1,...,n}.
                 grow = min( one, half / max( xbnd, smlnum ) )
                 do j = jfirst, jlast, jinc
                    ! exit the loop if the growth factor is too small.
                    if( grow<=smlnum )go to 90
                    ! g(j) = ( 1 + cnorm(j) )*g(j-1)
                    xj = one + cnorm( j )
                    grow = grow / xj
                 end do
              end if
              90 continue
           end if
           if( ( grow*tscal )>smlnum ) then
              ! use the level 2 blas solve if the reciprocal of the bound on
              ! elements of x is not too small.
              call stdlib_ztbsv( uplo, trans, diag, n, kd, ab, ldab, x, 1_ilp )
           else
              ! use a level 1 blas solve, scaling intermediate results.
              if( xmax>bignum*half ) then
                 ! scale x so that its components are less than or equal to
                 ! bignum in absolute value.
                 scale = ( bignum*half ) / xmax
                 call stdlib_zdscal( n, scale, x, 1_ilp )
                 xmax = bignum
              else
                 xmax = xmax*two
              end if
              if( notran ) then
                 ! solve a * x = b
                 loop_120: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) / a(j,j), scaling x if necessary.
                    xj = cabs1( x( j ) )
                    if( nounit ) then
                       tjjs = ab( maind, j )*tscal
                    else
                       tjjs = tscal
                       if( tscal==one )go to 110
                    end if
                    tjj = cabs1( tjjs )
                    if( tjj>smlnum ) then
                          ! abs(a(j,j)) > smlnum:
                       if( tjj<one ) then
                          if( xj>tjj*bignum ) then
                                ! scale x by 1/b(j).
                             rec = one / xj
                             call stdlib_zdscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                       end if
                       x( j ) = stdlib_zladiv( x( j ), tjjs )
                       xj = cabs1( x( j ) )
                    else if( tjj>zero ) then
                          ! 0 < abs(a(j,j)) <= smlnum:
                       if( xj>tjj*bignum ) then
                             ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum
                             ! to avoid overflow when dividing by a(j,j).
                          rec = ( tjj*bignum ) / xj
                          if( cnorm( j )>one ) then
                                ! scale by 1/cnorm(j) to avoid overflow when
                                ! multiplying x(j) times column j.
                             rec = rec / cnorm( j )
                          end if
                          call stdlib_zdscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                       x( j ) = stdlib_zladiv( x( j ), tjjs )
                       xj = cabs1( x( j ) )
                    else
                          ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                          ! scale = 0, and compute a solution to a*x = 0.
                       do i = 1, n
                          x( i ) = zero
                       end do
                       x( j ) = one
                       xj = one
                       scale = zero
                       xmax = zero
                    end if
                    110 continue
                    ! scale x if necessary to avoid overflow when adding a
                    ! multiple of column j of a.
                    if( xj>one ) then
                       rec = one / xj
                       if( cnorm( j )>( bignum-xmax )*rec ) then
                          ! scale x by 1/(2*abs(x(j))).
                          rec = rec*half
                          call stdlib_zdscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                       end if
                    else if( xj*cnorm( j )>( bignum-xmax ) ) then
                       ! scale x by 1/2.
                       call stdlib_zdscal( n, half, x, 1_ilp )
                       scale = scale*half
                    end if
                    if( upper ) then
                       if( j>1_ilp ) then
                          ! compute the update
                             ! x(max(1,j-kd):j-1) := x(max(1,j-kd):j-1) -
                                                   ! x(j)* a(max(1,j-kd):j-1,j)
                          jlen = min( kd, j-1 )
                          call stdlib_zaxpy( jlen, -x( j )*tscal,ab( kd+1-jlen, j ), 1_ilp, x( j-jlen &
                                    ), 1_ilp )
                          i = stdlib_izamax( j-1, x, 1_ilp )
                          xmax = cabs1( x( i ) )
                       end if
                    else if( j<n ) then
                       ! compute the update
                          ! x(j+1:min(j+kd,n)) := x(j+1:min(j+kd,n)) -
                                                ! x(j) * a(j+1:min(j+kd,n),j)
                       jlen = min( kd, n-j )
                       if( jlen>0_ilp )call stdlib_zaxpy( jlen, -x( j )*tscal, ab( 2_ilp, j ), 1_ilp,x( j+1 ),&
                                  1_ilp )
                       i = j + stdlib_izamax( n-j, x( j+1 ), 1_ilp )
                       xmax = cabs1( x( i ) )
                    end if
                 end do loop_120
              else if( stdlib_lsame( trans, 'T' ) ) then
                 ! solve a**t * x = b
                 loop_170: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = cabs1( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = ab( maind, j )*tscal
                       else
                          tjjs = tscal
                       end if
                       tjj = cabs1( tjjs )
                       if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                          rec = min( one, rec*tjj )
                          uscal = stdlib_zladiv( uscal, tjjs )
                       end if
                       if( rec<one ) then
                          call stdlib_zdscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    csumj = zero
                    if( uscal==cmplx( one,KIND=dp) ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_zdotu to perform the dot product.
                       if( upper ) then
                          jlen = min( kd, j-1 )
                          csumj = stdlib_zdotu( jlen, ab( kd+1-jlen, j ), 1_ilp,x( j-jlen ), 1_ilp )
                                    
                       else
                          jlen = min( kd, n-j )
                          if( jlen>1_ilp )csumj = stdlib_zdotu( jlen, ab( 2_ilp, j ), 1_ilp, x( j+1 ),1_ilp )
                                    
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          jlen = min( kd, j-1 )
                          do i = 1, jlen
                             csumj = csumj + ( ab( kd+i-jlen, j )*uscal )*x( j-jlen-1+i )
                          end do
                       else
                          jlen = min( kd, n-j )
                          do i = 1, jlen
                             csumj = csumj + ( ab( i+1, j )*uscal )*x( j+i )
                          end do
                       end if
                    end if
                    if( uscal==cmplx( tscal,KIND=dp) ) then
                       ! compute x(j) := ( x(j) - csumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - csumj
                       xj = cabs1( x( j ) )
                       if( nounit ) then
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjjs = ab( maind, j )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 160
                       end if
                       tjj = cabs1( tjjs )
                       if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                rec = one / xj
                                call stdlib_zdscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = stdlib_zladiv( x( j ), tjjs )
                       else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                             rec = ( tjj*bignum ) / xj
                             call stdlib_zdscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = stdlib_zladiv( x( j ), tjjs )
                       else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0 and compute a solution to a**t *x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          scale = zero
                          xmax = zero
                       end if
                       160 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - csumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = stdlib_zladiv( x( j ), tjjs ) - csumj
                    end if
                    xmax = max( xmax, cabs1( x( j ) ) )
                 end do loop_170
              else
                 ! solve a**h * x = b
                 loop_220: do j = jfirst, jlast, jinc
                    ! compute x(j) = b(j) - sum a(k,j)*x(k).
                                          ! k<>j
                    xj = cabs1( x( j ) )
                    uscal = tscal
                    rec = one / max( xmax, one )
                    if( cnorm( j )>( bignum-xj )*rec ) then
                       ! if x(j) could overflow, scale x by 1/(2*xmax).
                       rec = rec*half
                       if( nounit ) then
                          tjjs = conjg( ab( maind, j ) )*tscal
                       else
                          tjjs = tscal
                       end if
                       tjj = cabs1( tjjs )
                       if( tjj>one ) then
                             ! divide by a(j,j) when scaling x if a(j,j) > 1.
                          rec = min( one, rec*tjj )
                          uscal = stdlib_zladiv( uscal, tjjs )
                       end if
                       if( rec<one ) then
                          call stdlib_zdscal( n, rec, x, 1_ilp )
                          scale = scale*rec
                          xmax = xmax*rec
                       end if
                    end if
                    csumj = zero
                    if( uscal==cmplx( one,KIND=dp) ) then
                       ! if the scaling needed for a in the dot product is 1,
                       ! call stdlib_zdotc to perform the dot product.
                       if( upper ) then
                          jlen = min( kd, j-1 )
                          csumj = stdlib_zdotc( jlen, ab( kd+1-jlen, j ), 1_ilp,x( j-jlen ), 1_ilp )
                                    
                       else
                          jlen = min( kd, n-j )
                          if( jlen>1_ilp )csumj = stdlib_zdotc( jlen, ab( 2_ilp, j ), 1_ilp, x( j+1 ),1_ilp )
                                    
                       end if
                    else
                       ! otherwise, use in-line code for the dot product.
                       if( upper ) then
                          jlen = min( kd, j-1 )
                          do i = 1, jlen
                             csumj = csumj + ( conjg( ab( kd+i-jlen, j ) )*uscal )*x( j-jlen-1+i )
                                       
                          end do
                       else
                          jlen = min( kd, n-j )
                          do i = 1, jlen
                             csumj = csumj + ( conjg( ab( i+1, j ) )*uscal )*x( j+i )
                          end do
                       end if
                    end if
                    if( uscal==cmplx( tscal,KIND=dp) ) then
                       ! compute x(j) := ( x(j) - csumj ) / a(j,j) if 1/a(j,j)
                       ! was not used to scale the dotproduct.
                       x( j ) = x( j ) - csumj
                       xj = cabs1( x( j ) )
                       if( nounit ) then
                          ! compute x(j) = x(j) / a(j,j), scaling if necessary.
                          tjjs = conjg( ab( maind, j ) )*tscal
                       else
                          tjjs = tscal
                          if( tscal==one )go to 210
                       end if
                       tjj = cabs1( tjjs )
                       if( tjj>smlnum ) then
                             ! abs(a(j,j)) > smlnum:
                          if( tjj<one ) then
                             if( xj>tjj*bignum ) then
                                   ! scale x by 1/abs(x(j)).
                                rec = one / xj
                                call stdlib_zdscal( n, rec, x, 1_ilp )
                                scale = scale*rec
                                xmax = xmax*rec
                             end if
                          end if
                          x( j ) = stdlib_zladiv( x( j ), tjjs )
                       else if( tjj>zero ) then
                             ! 0 < abs(a(j,j)) <= smlnum:
                          if( xj>tjj*bignum ) then
                                ! scale x by (1/abs(x(j)))*abs(a(j,j))*bignum.
                             rec = ( tjj*bignum ) / xj
                             call stdlib_zdscal( n, rec, x, 1_ilp )
                             scale = scale*rec
                             xmax = xmax*rec
                          end if
                          x( j ) = stdlib_zladiv( x( j ), tjjs )
                       else
                             ! a(j,j) = 0:  set x(1:n) = 0, x(j) = 1, and
                             ! scale = 0 and compute a solution to a**h *x = 0.
                          do i = 1, n
                             x( i ) = zero
                          end do
                          x( j ) = one
                          scale = zero
                          xmax = zero
                       end if
                       210 continue
                    else
                       ! compute x(j) := x(j) / a(j,j) - csumj if the dot
                       ! product has already been divided by 1/a(j,j).
                       x( j ) = stdlib_zladiv( x( j ), tjjs ) - csumj
                    end if
                    xmax = max( xmax, cabs1( x( j ) ) )
                 end do loop_220
              end if
              scale = scale / tscal
           end if
           ! scale the column norms by 1/tscal for return.
           if( tscal/=one ) then
              call stdlib_dscal( n, one / tscal, cnorm, 1_ilp )
           end if
           return
     end subroutine stdlib_zlatbs




     pure module subroutine stdlib_stbrfs( uplo, trans, diag, n, kd, nrhs, ab, ldab, b,ldb, x, ldx, ferr,&
     !! STBRFS provides error bounds and backward error estimates for the
     !! solution to a system of linear equations with a triangular band
     !! coefficient matrix.
     !! The solution matrix X must be computed by STBTRS or some other
     !! means before entering this routine.  STBRFS does not do iterative
     !! refinement because doing so cannot improve the backward error.
                berr, work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: ab(ldab,*), b(ldb,*), x(ldx,*)
           real(sp), intent(out) :: berr(*), ferr(*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           character :: transt
           integer(ilp) :: i, j, k, kase, nz
           real(sp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( nrhs<0_ilp ) then
              info = -6_ilp
           else if( ldab<kd+1 ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STBRFS', -info )
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
           if( notran ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = kd + 2_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_250: do j = 1, nrhs
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a or a**t, depending on trans.
              call stdlib_scopy( n, x( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_stbmv( uplo, trans, diag, n, kd, ab, ldab, work( n+1 ),1_ilp )
              call stdlib_saxpy( n, -one, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              if( notran ) then
                 ! compute abs(a)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = max( 1, k-kd ), k
                             work( i ) = work( i ) +abs( ab( kd+1+i-k, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = max( 1, k-kd ), k - 1
                             work( i ) = work( i ) +abs( ab( kd+1+i-k, k ) )*xk
                          end do
                          work( k ) = work( k ) + xk
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = k, min( n, k+kd )
                             work( i ) = work( i ) + abs( ab( 1_ilp+i-k, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = k + 1, min( n, k+kd )
                             work( i ) = work( i ) + abs( ab( 1_ilp+i-k, k ) )*xk
                          end do
                          work( k ) = work( k ) + xk
                       end do
                    end if
                 end if
              else
                 ! compute abs(a**t)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = max( 1, k-kd ), k
                             s = s + abs( ab( kd+1+i-k, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = abs( x( k, j ) )
                          do i = max( 1, k-kd ), k - 1
                             s = s + abs( ab( kd+1+i-k, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = k, min( n, k+kd )
                             s = s + abs( ab( 1_ilp+i-k, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = abs( x( k, j ) )
                          do i = k + 1, min( n, k+kd )
                             s = s + abs( ab( 1_ilp+i-k, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    end if
                 end if
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_slacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              210 continue
              call stdlib_slacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**t).
                    call stdlib_stbsv( uplo, transt, diag, n, kd, ab, ldab,work( n+1 ), 1_ilp )
                              
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_stbsv( uplo, trans, diag, n, kd, ab, ldab,work( n+1 ), 1_ilp )
                 end if
                 go to 210
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_250
           return
     end subroutine stdlib_stbrfs

     pure module subroutine stdlib_dtbrfs( uplo, trans, diag, n, kd, nrhs, ab, ldab, b,ldb, x, ldx, ferr,&
     !! DTBRFS provides error bounds and backward error estimates for the
     !! solution to a system of linear equations with a triangular band
     !! coefficient matrix.
     !! The solution matrix X must be computed by DTBTRS or some other
     !! means before entering this routine.  DTBRFS does not do iterative
     !! refinement because doing so cannot improve the backward error.
                berr, work, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, ldx, n, nrhs
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: ab(ldab,*), b(ldb,*), x(ldx,*)
           real(dp), intent(out) :: berr(*), ferr(*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           character :: transt
           integer(ilp) :: i, j, k, kase, nz
           real(dp) :: eps, lstres, s, safe1, safe2, safmin, xk
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( nrhs<0_ilp ) then
              info = -6_ilp
           else if( ldab<kd+1 ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTBRFS', -info )
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
           if( notran ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = kd + 2_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_250: do j = 1, nrhs
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a or a**t, depending on trans.
              call stdlib_dcopy( n, x( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              call stdlib_dtbmv( uplo, trans, diag, n, kd, ab, ldab, work( n+1 ),1_ilp )
              call stdlib_daxpy( n, -one, b( 1_ilp, j ), 1_ilp, work( n+1 ), 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 work( i ) = abs( b( i, j ) )
              end do
              if( notran ) then
                 ! compute abs(a)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = max( 1, k-kd ), k
                             work( i ) = work( i ) +abs( ab( kd+1+i-k, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = max( 1, k-kd ), k - 1
                             work( i ) = work( i ) +abs( ab( kd+1+i-k, k ) )*xk
                          end do
                          work( k ) = work( k ) + xk
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = k, min( n, k+kd )
                             work( i ) = work( i ) + abs( ab( 1_ilp+i-k, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = abs( x( k, j ) )
                          do i = k + 1, min( n, k+kd )
                             work( i ) = work( i ) + abs( ab( 1_ilp+i-k, k ) )*xk
                          end do
                          work( k ) = work( k ) + xk
                       end do
                    end if
                 end if
              else
                 ! compute abs(a**t)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = max( 1, k-kd ), k
                             s = s + abs( ab( kd+1+i-k, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = abs( x( k, j ) )
                          do i = max( 1, k-kd ), k - 1
                             s = s + abs( ab( kd+1+i-k, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = k, min( n, k+kd )
                             s = s + abs( ab( 1_ilp+i-k, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = abs( x( k, j ) )
                          do i = k + 1, min( n, k+kd )
                             s = s + abs( ab( 1_ilp+i-k, k ) )*abs( x( i, j ) )
                          end do
                          work( k ) = work( k ) + s
                       end do
                    end if
                 end if
              end if
              s = zero
              do i = 1, n
                 if( work( i )>safe2 ) then
                    s = max( s, abs( work( n+i ) ) / work( i ) )
                 else
                    s = max( s, ( abs( work( n+i ) )+safe1 ) /( work( i )+safe1 ) )
                 end if
              end do
              berr( j ) = s
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_dlacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( work( i )>safe2 ) then
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i )
                 else
                    work( i ) = abs( work( n+i ) ) + nz*eps*work( i ) + safe1
                 end if
              end do
              kase = 0_ilp
              210 continue
              call stdlib_dlacn2( n, work( 2_ilp*n+1 ), work( n+1 ), iwork, ferr( j ),kase, isave )
                        
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**t).
                    call stdlib_dtbsv( uplo, transt, diag, n, kd, ab, ldab,work( n+1 ), 1_ilp )
                              
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( n+i ) = work( i )*work( n+i )
                    end do
                    call stdlib_dtbsv( uplo, trans, diag, n, kd, ab, ldab,work( n+1 ), 1_ilp )
                 end if
                 go to 210
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, abs( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_250
           return
     end subroutine stdlib_dtbrfs


     pure module subroutine stdlib_ctbrfs( uplo, trans, diag, n, kd, nrhs, ab, ldab, b,ldb, x, ldx, ferr,&
     !! CTBRFS provides error bounds and backward error estimates for the
     !! solution to a system of linear equations with a triangular band
     !! coefficient matrix.
     !! The solution matrix X must be computed by CTBTRS or some other
     !! means before entering this routine.  CTBRFS does not do iterative
     !! refinement because doing so cannot improve the backward error.
                berr, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, ldx, n, nrhs
           ! Array Arguments 
           real(sp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(sp), intent(in) :: ab(ldab,*), b(ldb,*), x(ldx,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           character :: transn, transt
           integer(ilp) :: i, j, k, kase, nz
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
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( nrhs<0_ilp ) then
              info = -6_ilp
           else if( ldab<kd+1 ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTBRFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'C'
           else
              transn = 'C'
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = kd + 2_ilp
           eps = stdlib_slamch( 'EPSILON' )
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_250: do j = 1, nrhs
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_ccopy( n, x( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_ctbmv( uplo, trans, diag, n, kd, ab, ldab, work, 1_ilp )
              call stdlib_caxpy( n, -cone, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              if( notran ) then
                 ! compute abs(a)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = max( 1, k-kd ), k
                             rwork( i ) = rwork( i ) +cabs1( ab( kd+1+i-k, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = max( 1, k-kd ), k - 1
                             rwork( i ) = rwork( i ) +cabs1( ab( kd+1+i-k, k ) )*xk
                          end do
                          rwork( k ) = rwork( k ) + xk
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = k, min( n, k+kd )
                             rwork( i ) = rwork( i ) +cabs1( ab( 1_ilp+i-k, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = k + 1, min( n, k+kd )
                             rwork( i ) = rwork( i ) +cabs1( ab( 1_ilp+i-k, k ) )*xk
                          end do
                          rwork( k ) = rwork( k ) + xk
                       end do
                    end if
                 end if
              else
                 ! compute abs(a**h)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = max( 1, k-kd ), k
                             s = s + cabs1( ab( kd+1+i-k, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = cabs1( x( k, j ) )
                          do i = max( 1, k-kd ), k - 1
                             s = s + cabs1( ab( kd+1+i-k, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = k, min( n, k+kd )
                             s = s + cabs1( ab( 1_ilp+i-k, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = cabs1( x( k, j ) )
                          do i = k + 1, min( n, k+kd )
                             s = s + cabs1( ab( 1_ilp+i-k, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    end if
                 end if
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
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_clacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              210 continue
              call stdlib_clacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**h).
                    call stdlib_ctbsv( uplo, transt, diag, n, kd, ab, ldab, work,1_ilp )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_ctbsv( uplo, transn, diag, n, kd, ab, ldab, work,1_ilp )
                 end if
                 go to 210
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_250
           return
     end subroutine stdlib_ctbrfs

     pure module subroutine stdlib_ztbrfs( uplo, trans, diag, n, kd, nrhs, ab, ldab, b,ldb, x, ldx, ferr,&
     !! ZTBRFS provides error bounds and backward error estimates for the
     !! solution to a system of linear equations with a triangular band
     !! coefficient matrix.
     !! The solution matrix X must be computed by ZTBTRS or some other
     !! means before entering this routine.  ZTBRFS does not do iterative
     !! refinement because doing so cannot improve the backward error.
                berr, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldb, ldx, n, nrhs
           ! Array Arguments 
           real(dp), intent(out) :: berr(*), ferr(*), rwork(*)
           complex(dp), intent(in) :: ab(ldab,*), b(ldb,*), x(ldx,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: notran, nounit, upper
           character :: transn, transt
           integer(ilp) :: i, j, k, kase, nz
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
           notran = stdlib_lsame( trans, 'N' )
           nounit = stdlib_lsame( diag, 'N' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -2_ilp
           else if( .not.nounit .and. .not.stdlib_lsame( diag, 'U' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( nrhs<0_ilp ) then
              info = -6_ilp
           else if( ldab<kd+1 ) then
              info = -8_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -10_ilp
           else if( ldx<max( 1_ilp, n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTBRFS', -info )
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
           if( notran ) then
              transn = 'N'
              transt = 'C'
           else
              transn = 'C'
              transt = 'N'
           end if
           ! nz = maximum number of nonzero elements in each row of a, plus 1
           nz = kd + 2_ilp
           eps = stdlib_dlamch( 'EPSILON' )
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = nz*safmin
           safe2 = safe1 / eps
           ! do for each right hand side
           loop_250: do j = 1, nrhs
              ! compute residual r = b - op(a) * x,
              ! where op(a) = a, a**t, or a**h, depending on trans.
              call stdlib_zcopy( n, x( 1_ilp, j ), 1_ilp, work, 1_ilp )
              call stdlib_ztbmv( uplo, trans, diag, n, kd, ab, ldab, work, 1_ilp )
              call stdlib_zaxpy( n, -cone, b( 1_ilp, j ), 1_ilp, work, 1_ilp )
              ! compute componentwise relative backward error from formula
              ! max(i) ( abs(r(i)) / ( abs(op(a))*abs(x) + abs(b) )(i) )
              ! where abs(z) is the componentwise absolute value of the matrix
              ! or vector z.  if the i-th component of the denominator is less
              ! than safe2, then safe1 is added to the i-th components of the
              ! numerator and denominator before dividing.
              do i = 1, n
                 rwork( i ) = cabs1( b( i, j ) )
              end do
              if( notran ) then
                 ! compute abs(a)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = max( 1, k-kd ), k
                             rwork( i ) = rwork( i ) +cabs1( ab( kd+1+i-k, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = max( 1, k-kd ), k - 1
                             rwork( i ) = rwork( i ) +cabs1( ab( kd+1+i-k, k ) )*xk
                          end do
                          rwork( k ) = rwork( k ) + xk
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = k, min( n, k+kd )
                             rwork( i ) = rwork( i ) +cabs1( ab( 1_ilp+i-k, k ) )*xk
                          end do
                       end do
                    else
                       do k = 1, n
                          xk = cabs1( x( k, j ) )
                          do i = k + 1, min( n, k+kd )
                             rwork( i ) = rwork( i ) +cabs1( ab( 1_ilp+i-k, k ) )*xk
                          end do
                          rwork( k ) = rwork( k ) + xk
                       end do
                    end if
                 end if
              else
                 ! compute abs(a**h)*abs(x) + abs(b).
                 if( upper ) then
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = max( 1, k-kd ), k
                             s = s + cabs1( ab( kd+1+i-k, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = cabs1( x( k, j ) )
                          do i = max( 1, k-kd ), k - 1
                             s = s + cabs1( ab( kd+1+i-k, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    end if
                 else
                    if( nounit ) then
                       do k = 1, n
                          s = zero
                          do i = k, min( n, k+kd )
                             s = s + cabs1( ab( 1_ilp+i-k, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    else
                       do k = 1, n
                          s = cabs1( x( k, j ) )
                          do i = k + 1, min( n, k+kd )
                             s = s + cabs1( ab( 1_ilp+i-k, k ) )*cabs1( x( i, j ) )
                          end do
                          rwork( k ) = rwork( k ) + s
                       end do
                    end if
                 end if
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
              ! bound error from formula
              ! norm(x - xtrue) / norm(x) .le. ferr =
              ! norm( abs(inv(op(a)))*
                 ! ( abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) ))) / norm(x)
              ! where
                ! norm(z) is the magnitude of the largest component of z
                ! inv(op(a)) is the inverse of op(a)
                ! abs(z) is the componentwise absolute value of the matrix or
                   ! vector z
                ! nz is the maximum number of nonzeros in any row of a, plus 1
                ! eps is machine epsilon
              ! the i-th component of abs(r)+nz*eps*(abs(op(a))*abs(x)+abs(b))
              ! is incremented by safe1 if the i-th component of
              ! abs(op(a))*abs(x) + abs(b) is less than safe2.
              ! use stdlib_zlacn2 to estimate the infinity-norm of the matrix
                 ! inv(op(a)) * diag(w),
              ! where w = abs(r) + nz*eps*( abs(op(a))*abs(x)+abs(b) )))
              do i = 1, n
                 if( rwork( i )>safe2 ) then
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i )
                 else
                    rwork( i ) = cabs1( work( i ) ) + nz*eps*rwork( i ) +safe1
                 end if
              end do
              kase = 0_ilp
              210 continue
              call stdlib_zlacn2( n, work( n+1 ), work, ferr( j ), kase, isave )
              if( kase/=0_ilp ) then
                 if( kase==1_ilp ) then
                    ! multiply by diag(w)*inv(op(a)**h).
                    call stdlib_ztbsv( uplo, transt, diag, n, kd, ab, ldab, work,1_ilp )
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                 else
                    ! multiply by inv(op(a))*diag(w).
                    do i = 1, n
                       work( i ) = rwork( i )*work( i )
                    end do
                    call stdlib_ztbsv( uplo, transn, diag, n, kd, ab, ldab, work,1_ilp )
                 end if
                 go to 210
              end if
              ! normalize error.
              lstres = zero
              do i = 1, n
                 lstres = max( lstres, cabs1( x( i, j ) ) )
              end do
              if( lstres/=zero )ferr( j ) = ferr( j ) / lstres
           end do loop_250
           return
     end subroutine stdlib_ztbrfs



end submodule stdlib_lapack_solve_tri_comp
