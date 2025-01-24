submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_sym_comp
  implicit none


  contains

     pure module subroutine stdlib_ssygst( itype, uplo, n, a, lda, b, ldb, info )
     !! SSYGST reduces a real symmetric-definite generalized eigenproblem
     !! to standard form.
     !! If ITYPE = 1, the problem is A*x = lambda*B*x,
     !! and A is overwritten by inv(U**T)*A*inv(U) or inv(L)*A*inv(L**T)
     !! If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
     !! B*A*x = lambda*x, and A is overwritten by U*A*U**T or L**T*A*L.
     !! B must have been previously factorized as U**T*U or L*L**T by SPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k, kb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYGST', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'SSYGST', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_ssygs2( itype, uplo, n, a, lda, b, ldb, info )
           else
              ! use blocked code
              if( itype==1_ilp ) then
                 if( upper ) then
                    ! compute inv(u**t)*a*inv(u)
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the upper triangle of a(k:n,k:n)
                       call stdlib_ssygs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                       if( k+kb<=n ) then
                          call stdlib_strsm( 'LEFT', uplo, 'TRANSPOSE', 'NON-UNIT',kb, n-k-kb+1, &
                                    one, b( k, k ), ldb,a( k, k+kb ), lda )
                          call stdlib_ssymm( 'LEFT', uplo, kb, n-k-kb+1, -half,a( k, k ), lda, b( &
                                    k, k+kb ), ldb, one,a( k, k+kb ), lda )
                          call stdlib_ssyr2k( uplo, 'TRANSPOSE', n-k-kb+1, kb, -one,a( k, k+kb ), &
                                    lda, b( k, k+kb ), ldb,one, a( k+kb, k+kb ), lda )
                          call stdlib_ssymm( 'LEFT', uplo, kb, n-k-kb+1, -half,a( k, k ), lda, b( &
                                    k, k+kb ), ldb, one,a( k, k+kb ), lda )
                          call stdlib_strsm( 'RIGHT', uplo, 'NO TRANSPOSE','NON-UNIT', kb, n-k-kb+&
                                    1_ilp, one,b( k+kb, k+kb ), ldb, a( k, k+kb ),lda )
                       end if
                    end do
                 else
                    ! compute inv(l)*a*inv(l**t)
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the lower triangle of a(k:n,k:n)
                       call stdlib_ssygs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                       if( k+kb<=n ) then
                          call stdlib_strsm( 'RIGHT', uplo, 'TRANSPOSE', 'NON-UNIT',n-k-kb+1, kb, &
                                    one, b( k, k ), ldb,a( k+kb, k ), lda )
                          call stdlib_ssymm( 'RIGHT', uplo, n-k-kb+1, kb, -half,a( k, k ), lda, b(&
                                     k+kb, k ), ldb, one,a( k+kb, k ), lda )
                          call stdlib_ssyr2k( uplo, 'NO TRANSPOSE', n-k-kb+1, kb,-one, a( k+kb, k &
                                    ), lda, b( k+kb, k ),ldb, one, a( k+kb, k+kb ), lda )
                          call stdlib_ssymm( 'RIGHT', uplo, n-k-kb+1, kb, -half,a( k, k ), lda, b(&
                                     k+kb, k ), ldb, one,a( k+kb, k ), lda )
                          call stdlib_strsm( 'LEFT', uplo, 'NO TRANSPOSE','NON-UNIT', n-k-kb+1, &
                                    kb, one,b( k+kb, k+kb ), ldb, a( k+kb, k ),lda )
                       end if
                    end do
                 end if
              else
                 if( upper ) then
                    ! compute u*a*u**t
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the upper triangle of a(1:k+kb-1,1:k+kb-1)
                       call stdlib_strmm( 'LEFT', uplo, 'NO TRANSPOSE', 'NON-UNIT',k-1, kb, one, &
                                 b, ldb, a( 1_ilp, k ), lda )
                       call stdlib_ssymm( 'RIGHT', uplo, k-1, kb, half, a( k, k ),lda, b( 1_ilp, k ), &
                                 ldb, one, a( 1_ilp, k ), lda )
                       call stdlib_ssyr2k( uplo, 'NO TRANSPOSE', k-1, kb, one,a( 1_ilp, k ), lda, b( &
                                 1_ilp, k ), ldb, one, a,lda )
                       call stdlib_ssymm( 'RIGHT', uplo, k-1, kb, half, a( k, k ),lda, b( 1_ilp, k ), &
                                 ldb, one, a( 1_ilp, k ), lda )
                       call stdlib_strmm( 'RIGHT', uplo, 'TRANSPOSE', 'NON-UNIT',k-1, kb, one, b( &
                                 k, k ), ldb, a( 1_ilp, k ),lda )
                       call stdlib_ssygs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                    end do
                 else
                    ! compute l**t*a*l
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the lower triangle of a(1:k+kb-1,1:k+kb-1)
                       call stdlib_strmm( 'RIGHT', uplo, 'NO TRANSPOSE', 'NON-UNIT',kb, k-1, one, &
                                 b, ldb, a( k, 1_ilp ), lda )
                       call stdlib_ssymm( 'LEFT', uplo, kb, k-1, half, a( k, k ),lda, b( k, 1_ilp ), &
                                 ldb, one, a( k, 1_ilp ), lda )
                       call stdlib_ssyr2k( uplo, 'TRANSPOSE', k-1, kb, one,a( k, 1_ilp ), lda, b( k, &
                                 1_ilp ), ldb, one, a,lda )
                       call stdlib_ssymm( 'LEFT', uplo, kb, k-1, half, a( k, k ),lda, b( k, 1_ilp ), &
                                 ldb, one, a( k, 1_ilp ), lda )
                       call stdlib_strmm( 'LEFT', uplo, 'TRANSPOSE', 'NON-UNIT', kb,k-1, one, b( &
                                 k, k ), ldb, a( k, 1_ilp ), lda )
                       call stdlib_ssygs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_ssygst

     pure module subroutine stdlib_dsygst( itype, uplo, n, a, lda, b, ldb, info )
     !! DSYGST reduces a real symmetric-definite generalized eigenproblem
     !! to standard form.
     !! If ITYPE = 1, the problem is A*x = lambda*B*x,
     !! and A is overwritten by inv(U**T)*A*inv(U) or inv(L)*A*inv(L**T)
     !! If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
     !! B*A*x = lambda*x, and A is overwritten by U*A*U**T or L**T*A*L.
     !! B must have been previously factorized as U**T*U or L*L**T by DPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k, kb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYGST', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'DSYGST', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_dsygs2( itype, uplo, n, a, lda, b, ldb, info )
           else
              ! use blocked code
              if( itype==1_ilp ) then
                 if( upper ) then
                    ! compute inv(u**t)*a*inv(u)
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the upper triangle of a(k:n,k:n)
                       call stdlib_dsygs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                       if( k+kb<=n ) then
                          call stdlib_dtrsm( 'LEFT', uplo, 'TRANSPOSE', 'NON-UNIT',kb, n-k-kb+1, &
                                    one, b( k, k ), ldb,a( k, k+kb ), lda )
                          call stdlib_dsymm( 'LEFT', uplo, kb, n-k-kb+1, -half,a( k, k ), lda, b( &
                                    k, k+kb ), ldb, one,a( k, k+kb ), lda )
                          call stdlib_dsyr2k( uplo, 'TRANSPOSE', n-k-kb+1, kb, -one,a( k, k+kb ), &
                                    lda, b( k, k+kb ), ldb,one, a( k+kb, k+kb ), lda )
                          call stdlib_dsymm( 'LEFT', uplo, kb, n-k-kb+1, -half,a( k, k ), lda, b( &
                                    k, k+kb ), ldb, one,a( k, k+kb ), lda )
                          call stdlib_dtrsm( 'RIGHT', uplo, 'NO TRANSPOSE','NON-UNIT', kb, n-k-kb+&
                                    1_ilp, one,b( k+kb, k+kb ), ldb, a( k, k+kb ),lda )
                       end if
                    end do
                 else
                    ! compute inv(l)*a*inv(l**t)
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the lower triangle of a(k:n,k:n)
                       call stdlib_dsygs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                       if( k+kb<=n ) then
                          call stdlib_dtrsm( 'RIGHT', uplo, 'TRANSPOSE', 'NON-UNIT',n-k-kb+1, kb, &
                                    one, b( k, k ), ldb,a( k+kb, k ), lda )
                          call stdlib_dsymm( 'RIGHT', uplo, n-k-kb+1, kb, -half,a( k, k ), lda, b(&
                                     k+kb, k ), ldb, one,a( k+kb, k ), lda )
                          call stdlib_dsyr2k( uplo, 'NO TRANSPOSE', n-k-kb+1, kb,-one, a( k+kb, k &
                                    ), lda, b( k+kb, k ),ldb, one, a( k+kb, k+kb ), lda )
                          call stdlib_dsymm( 'RIGHT', uplo, n-k-kb+1, kb, -half,a( k, k ), lda, b(&
                                     k+kb, k ), ldb, one,a( k+kb, k ), lda )
                          call stdlib_dtrsm( 'LEFT', uplo, 'NO TRANSPOSE','NON-UNIT', n-k-kb+1, &
                                    kb, one,b( k+kb, k+kb ), ldb, a( k+kb, k ),lda )
                       end if
                    end do
                 end if
              else
                 if( upper ) then
                    ! compute u*a*u**t
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the upper triangle of a(1:k+kb-1,1:k+kb-1)
                       call stdlib_dtrmm( 'LEFT', uplo, 'NO TRANSPOSE', 'NON-UNIT',k-1, kb, one, &
                                 b, ldb, a( 1_ilp, k ), lda )
                       call stdlib_dsymm( 'RIGHT', uplo, k-1, kb, half, a( k, k ),lda, b( 1_ilp, k ), &
                                 ldb, one, a( 1_ilp, k ), lda )
                       call stdlib_dsyr2k( uplo, 'NO TRANSPOSE', k-1, kb, one,a( 1_ilp, k ), lda, b( &
                                 1_ilp, k ), ldb, one, a,lda )
                       call stdlib_dsymm( 'RIGHT', uplo, k-1, kb, half, a( k, k ),lda, b( 1_ilp, k ), &
                                 ldb, one, a( 1_ilp, k ), lda )
                       call stdlib_dtrmm( 'RIGHT', uplo, 'TRANSPOSE', 'NON-UNIT',k-1, kb, one, b( &
                                 k, k ), ldb, a( 1_ilp, k ),lda )
                       call stdlib_dsygs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                    end do
                 else
                    ! compute l**t*a*l
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the lower triangle of a(1:k+kb-1,1:k+kb-1)
                       call stdlib_dtrmm( 'RIGHT', uplo, 'NO TRANSPOSE', 'NON-UNIT',kb, k-1, one, &
                                 b, ldb, a( k, 1_ilp ), lda )
                       call stdlib_dsymm( 'LEFT', uplo, kb, k-1, half, a( k, k ),lda, b( k, 1_ilp ), &
                                 ldb, one, a( k, 1_ilp ), lda )
                       call stdlib_dsyr2k( uplo, 'TRANSPOSE', k-1, kb, one,a( k, 1_ilp ), lda, b( k, &
                                 1_ilp ), ldb, one, a,lda )
                       call stdlib_dsymm( 'LEFT', uplo, kb, k-1, half, a( k, k ),lda, b( k, 1_ilp ), &
                                 ldb, one, a( k, 1_ilp ), lda )
                       call stdlib_dtrmm( 'LEFT', uplo, 'TRANSPOSE', 'NON-UNIT', kb,k-1, one, b( &
                                 k, k ), ldb, a( k, 1_ilp ), lda )
                       call stdlib_dsygs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_dsygst




     pure module subroutine stdlib_ssygs2( itype, uplo, n, a, lda, b, ldb, info )
     !! SSYGS2 reduces a real symmetric-definite generalized eigenproblem
     !! to standard form.
     !! If ITYPE = 1, the problem is A*x = lambda*B*x,
     !! and A is overwritten by inv(U**T)*A*inv(U) or inv(L)*A*inv(L**T)
     !! If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
     !! B*A*x = lambda*x, and A is overwritten by U*A*U**T or L**T *A*L.
     !! B must have been previously factorized as U**T *U or L*L**T by SPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k
           real(sp) :: akk, bkk, ct
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYGS2', -info )
              return
           end if
           if( itype==1_ilp ) then
              if( upper ) then
                 ! compute inv(u**t)*a*inv(u)
                 do k = 1, n
                    ! update the upper triangle of a(k:n,k:n)
                    akk = a( k, k )
                    bkk = b( k, k )
                    akk = akk / bkk**2_ilp
                    a( k, k ) = akk
                    if( k<n ) then
                       call stdlib_sscal( n-k, one / bkk, a( k, k+1 ), lda )
                       ct = -half*akk
                       call stdlib_saxpy( n-k, ct, b( k, k+1 ), ldb, a( k, k+1 ),lda )
                       call stdlib_ssyr2( uplo, n-k, -one, a( k, k+1 ), lda,b( k, k+1 ), ldb, a( &
                                 k+1, k+1 ), lda )
                       call stdlib_saxpy( n-k, ct, b( k, k+1 ), ldb, a( k, k+1 ),lda )
                       call stdlib_strsv( uplo, 'TRANSPOSE', 'NON-UNIT', n-k,b( k+1, k+1 ), ldb, &
                                 a( k, k+1 ), lda )
                    end if
                 end do
              else
                 ! compute inv(l)*a*inv(l**t)
                 do k = 1, n
                    ! update the lower triangle of a(k:n,k:n)
                    akk = a( k, k )
                    bkk = b( k, k )
                    akk = akk / bkk**2_ilp
                    a( k, k ) = akk
                    if( k<n ) then
                       call stdlib_sscal( n-k, one / bkk, a( k+1, k ), 1_ilp )
                       ct = -half*akk
                       call stdlib_saxpy( n-k, ct, b( k+1, k ), 1_ilp, a( k+1, k ), 1_ilp )
                       call stdlib_ssyr2( uplo, n-k, -one, a( k+1, k ), 1_ilp,b( k+1, k ), 1_ilp, a( k+1, &
                                 k+1 ), lda )
                       call stdlib_saxpy( n-k, ct, b( k+1, k ), 1_ilp, a( k+1, k ), 1_ilp )
                       call stdlib_strsv( uplo, 'NO TRANSPOSE', 'NON-UNIT', n-k,b( k+1, k+1 ), &
                                 ldb, a( k+1, k ), 1_ilp )
                    end if
                 end do
              end if
           else
              if( upper ) then
                 ! compute u*a*u**t
                 do k = 1, n
                    ! update the upper triangle of a(1:k,1:k)
                    akk = a( k, k )
                    bkk = b( k, k )
                    call stdlib_strmv( uplo, 'NO TRANSPOSE', 'NON-UNIT', k-1, b,ldb, a( 1_ilp, k ), 1_ilp &
                              )
                    ct = half*akk
                    call stdlib_saxpy( k-1, ct, b( 1_ilp, k ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    call stdlib_ssyr2( uplo, k-1, one, a( 1_ilp, k ), 1_ilp, b( 1_ilp, k ), 1_ilp,a, lda )
                    call stdlib_saxpy( k-1, ct, b( 1_ilp, k ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    call stdlib_sscal( k-1, bkk, a( 1_ilp, k ), 1_ilp )
                    a( k, k ) = akk*bkk**2_ilp
                 end do
              else
                 ! compute l**t *a*l
                 do k = 1, n
                    ! update the lower triangle of a(1:k,1:k)
                    akk = a( k, k )
                    bkk = b( k, k )
                    call stdlib_strmv( uplo, 'TRANSPOSE', 'NON-UNIT', k-1, b, ldb,a( k, 1_ilp ), lda )
                              
                    ct = half*akk
                    call stdlib_saxpy( k-1, ct, b( k, 1_ilp ), ldb, a( k, 1_ilp ), lda )
                    call stdlib_ssyr2( uplo, k-1, one, a( k, 1_ilp ), lda, b( k, 1_ilp ),ldb, a, lda )
                              
                    call stdlib_saxpy( k-1, ct, b( k, 1_ilp ), ldb, a( k, 1_ilp ), lda )
                    call stdlib_sscal( k-1, bkk, a( k, 1_ilp ), lda )
                    a( k, k ) = akk*bkk**2_ilp
                 end do
              end if
           end if
           return
     end subroutine stdlib_ssygs2

     pure module subroutine stdlib_dsygs2( itype, uplo, n, a, lda, b, ldb, info )
     !! DSYGS2 reduces a real symmetric-definite generalized eigenproblem
     !! to standard form.
     !! If ITYPE = 1, the problem is A*x = lambda*B*x,
     !! and A is overwritten by inv(U**T)*A*inv(U) or inv(L)*A*inv(L**T)
     !! If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
     !! B*A*x = lambda*x, and A is overwritten by U*A*U**T or L**T *A*L.
     !! B must have been previously factorized as U**T *U or L*L**T by DPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: b(ldb,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k
           real(dp) :: akk, bkk, ct
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYGS2', -info )
              return
           end if
           if( itype==1_ilp ) then
              if( upper ) then
                 ! compute inv(u**t)*a*inv(u)
                 do k = 1, n
                    ! update the upper triangle of a(k:n,k:n)
                    akk = a( k, k )
                    bkk = b( k, k )
                    akk = akk / bkk**2_ilp
                    a( k, k ) = akk
                    if( k<n ) then
                       call stdlib_dscal( n-k, one / bkk, a( k, k+1 ), lda )
                       ct = -half*akk
                       call stdlib_daxpy( n-k, ct, b( k, k+1 ), ldb, a( k, k+1 ),lda )
                       call stdlib_dsyr2( uplo, n-k, -one, a( k, k+1 ), lda,b( k, k+1 ), ldb, a( &
                                 k+1, k+1 ), lda )
                       call stdlib_daxpy( n-k, ct, b( k, k+1 ), ldb, a( k, k+1 ),lda )
                       call stdlib_dtrsv( uplo, 'TRANSPOSE', 'NON-UNIT', n-k,b( k+1, k+1 ), ldb, &
                                 a( k, k+1 ), lda )
                    end if
                 end do
              else
                 ! compute inv(l)*a*inv(l**t)
                 do k = 1, n
                    ! update the lower triangle of a(k:n,k:n)
                    akk = a( k, k )
                    bkk = b( k, k )
                    akk = akk / bkk**2_ilp
                    a( k, k ) = akk
                    if( k<n ) then
                       call stdlib_dscal( n-k, one / bkk, a( k+1, k ), 1_ilp )
                       ct = -half*akk
                       call stdlib_daxpy( n-k, ct, b( k+1, k ), 1_ilp, a( k+1, k ), 1_ilp )
                       call stdlib_dsyr2( uplo, n-k, -one, a( k+1, k ), 1_ilp,b( k+1, k ), 1_ilp, a( k+1, &
                                 k+1 ), lda )
                       call stdlib_daxpy( n-k, ct, b( k+1, k ), 1_ilp, a( k+1, k ), 1_ilp )
                       call stdlib_dtrsv( uplo, 'NO TRANSPOSE', 'NON-UNIT', n-k,b( k+1, k+1 ), &
                                 ldb, a( k+1, k ), 1_ilp )
                    end if
                 end do
              end if
           else
              if( upper ) then
                 ! compute u*a*u**t
                 do k = 1, n
                    ! update the upper triangle of a(1:k,1:k)
                    akk = a( k, k )
                    bkk = b( k, k )
                    call stdlib_dtrmv( uplo, 'NO TRANSPOSE', 'NON-UNIT', k-1, b,ldb, a( 1_ilp, k ), 1_ilp &
                              )
                    ct = half*akk
                    call stdlib_daxpy( k-1, ct, b( 1_ilp, k ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    call stdlib_dsyr2( uplo, k-1, one, a( 1_ilp, k ), 1_ilp, b( 1_ilp, k ), 1_ilp,a, lda )
                    call stdlib_daxpy( k-1, ct, b( 1_ilp, k ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    call stdlib_dscal( k-1, bkk, a( 1_ilp, k ), 1_ilp )
                    a( k, k ) = akk*bkk**2_ilp
                 end do
              else
                 ! compute l**t *a*l
                 do k = 1, n
                    ! update the lower triangle of a(1:k,1:k)
                    akk = a( k, k )
                    bkk = b( k, k )
                    call stdlib_dtrmv( uplo, 'TRANSPOSE', 'NON-UNIT', k-1, b, ldb,a( k, 1_ilp ), lda )
                              
                    ct = half*akk
                    call stdlib_daxpy( k-1, ct, b( k, 1_ilp ), ldb, a( k, 1_ilp ), lda )
                    call stdlib_dsyr2( uplo, k-1, one, a( k, 1_ilp ), lda, b( k, 1_ilp ),ldb, a, lda )
                              
                    call stdlib_daxpy( k-1, ct, b( k, 1_ilp ), ldb, a( k, 1_ilp ), lda )
                    call stdlib_dscal( k-1, bkk, a( k, 1_ilp ), lda )
                    a( k, k ) = akk*bkk**2_ilp
                 end do
              end if
           end if
           return
     end subroutine stdlib_dsygs2




     pure module subroutine stdlib_sspgst( itype, uplo, n, ap, bp, info )
     !! SSPGST reduces a real symmetric-definite generalized eigenproblem
     !! to standard form, using packed storage.
     !! If ITYPE = 1, the problem is A*x = lambda*B*x,
     !! and A is overwritten by inv(U**T)*A*inv(U) or inv(L)*A*inv(L**T)
     !! If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
     !! B*A*x = lambda*x, and A is overwritten by U*A*U**T or L**T*A*L.
     !! B must have been previously factorized as U**T*U or L*L**T by SPPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, n
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(in) :: bp(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, j1, j1j1, jj, k, k1, k1k1, kk
           real(sp) :: ajj, akk, bjj, bkk, ct
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPGST', -info )
              return
           end if
           if( itype==1_ilp ) then
              if( upper ) then
                 ! compute inv(u**t)*a*inv(u)
                 ! j1 and jj are the indices of a(1,j) and a(j,j)
                 jj = 0_ilp
                 do j = 1, n
                    j1 = jj + 1_ilp
                    jj = jj + j
                    ! compute the j-th column of the upper triangle of a
                    bjj = bp( jj )
                    call stdlib_stpsv( uplo, 'TRANSPOSE', 'NONUNIT', j, bp,ap( j1 ), 1_ilp )
                    call stdlib_sspmv( uplo, j-1, -one, ap, bp( j1 ), 1_ilp, one,ap( j1 ), 1_ilp )
                    call stdlib_sscal( j-1, one / bjj, ap( j1 ), 1_ilp )
                    ap( jj ) = ( ap( jj )-stdlib_sdot( j-1, ap( j1 ), 1_ilp, bp( j1 ),1_ilp ) ) / &
                              bjj
                 end do
              else
                 ! compute inv(l)*a*inv(l**t)
                 ! kk and k1k1 are the indices of a(k,k) and a(k+1,k+1)
                 kk = 1_ilp
                 do k = 1, n
                    k1k1 = kk + n - k + 1_ilp
                    ! update the lower triangle of a(k:n,k:n)
                    akk = ap( kk )
                    bkk = bp( kk )
                    akk = akk / bkk**2_ilp
                    ap( kk ) = akk
                    if( k<n ) then
                       call stdlib_sscal( n-k, one / bkk, ap( kk+1 ), 1_ilp )
                       ct = -half*akk
                       call stdlib_saxpy( n-k, ct, bp( kk+1 ), 1_ilp, ap( kk+1 ), 1_ilp )
                       call stdlib_sspr2( uplo, n-k, -one, ap( kk+1 ), 1_ilp,bp( kk+1 ), 1_ilp, ap( k1k1 )&
                                  )
                       call stdlib_saxpy( n-k, ct, bp( kk+1 ), 1_ilp, ap( kk+1 ), 1_ilp )
                       call stdlib_stpsv( uplo, 'NO TRANSPOSE', 'NON-UNIT', n-k,bp( k1k1 ), ap( &
                                 kk+1 ), 1_ilp )
                    end if
                    kk = k1k1
                 end do
              end if
           else
              if( upper ) then
                 ! compute u*a*u**t
                 ! k1 and kk are the indices of a(1,k) and a(k,k)
                 kk = 0_ilp
                 do k = 1, n
                    k1 = kk + 1_ilp
                    kk = kk + k
                    ! update the upper triangle of a(1:k,1:k)
                    akk = ap( kk )
                    bkk = bp( kk )
                    call stdlib_stpmv( uplo, 'NO TRANSPOSE', 'NON-UNIT', k-1, bp,ap( k1 ), 1_ilp )
                              
                    ct = half*akk
                    call stdlib_saxpy( k-1, ct, bp( k1 ), 1_ilp, ap( k1 ), 1_ilp )
                    call stdlib_sspr2( uplo, k-1, one, ap( k1 ), 1_ilp, bp( k1 ), 1_ilp,ap )
                    call stdlib_saxpy( k-1, ct, bp( k1 ), 1_ilp, ap( k1 ), 1_ilp )
                    call stdlib_sscal( k-1, bkk, ap( k1 ), 1_ilp )
                    ap( kk ) = akk*bkk**2_ilp
                 end do
              else
                 ! compute l**t *a*l
                 ! jj and j1j1 are the indices of a(j,j) and a(j+1,j+1)
                 jj = 1_ilp
                 do j = 1, n
                    j1j1 = jj + n - j + 1_ilp
                    ! compute the j-th column of the lower triangle of a
                    ajj = ap( jj )
                    bjj = bp( jj )
                    ap( jj ) = ajj*bjj + stdlib_sdot( n-j, ap( jj+1 ), 1_ilp,bp( jj+1 ), 1_ilp )
                    call stdlib_sscal( n-j, bjj, ap( jj+1 ), 1_ilp )
                    call stdlib_sspmv( uplo, n-j, one, ap( j1j1 ), bp( jj+1 ), 1_ilp,one, ap( jj+1 ), &
                              1_ilp )
                    call stdlib_stpmv( uplo, 'TRANSPOSE', 'NON-UNIT', n-j+1,bp( jj ), ap( jj ), 1_ilp &
                              )
                    jj = j1j1
                 end do
              end if
           end if
           return
     end subroutine stdlib_sspgst

     pure module subroutine stdlib_dspgst( itype, uplo, n, ap, bp, info )
     !! DSPGST reduces a real symmetric-definite generalized eigenproblem
     !! to standard form, using packed storage.
     !! If ITYPE = 1, the problem is A*x = lambda*B*x,
     !! and A is overwritten by inv(U**T)*A*inv(U) or inv(L)*A*inv(L**T)
     !! If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
     !! B*A*x = lambda*x, and A is overwritten by U*A*U**T or L**T*A*L.
     !! B must have been previously factorized as U**T*U or L*L**T by DPPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, n
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(in) :: bp(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, j1, j1j1, jj, k, k1, k1k1, kk
           real(dp) :: ajj, akk, bjj, bkk, ct
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPGST', -info )
              return
           end if
           if( itype==1_ilp ) then
              if( upper ) then
                 ! compute inv(u**t)*a*inv(u)
                 ! j1 and jj are the indices of a(1,j) and a(j,j)
                 jj = 0_ilp
                 do j = 1, n
                    j1 = jj + 1_ilp
                    jj = jj + j
                    ! compute the j-th column of the upper triangle of a
                    bjj = bp( jj )
                    call stdlib_dtpsv( uplo, 'TRANSPOSE', 'NONUNIT', j, bp,ap( j1 ), 1_ilp )
                    call stdlib_dspmv( uplo, j-1, -one, ap, bp( j1 ), 1_ilp, one,ap( j1 ), 1_ilp )
                    call stdlib_dscal( j-1, one / bjj, ap( j1 ), 1_ilp )
                    ap( jj ) = ( ap( jj )-stdlib_ddot( j-1, ap( j1 ), 1_ilp, bp( j1 ),1_ilp ) ) / &
                              bjj
                 end do
              else
                 ! compute inv(l)*a*inv(l**t)
                 ! kk and k1k1 are the indices of a(k,k) and a(k+1,k+1)
                 kk = 1_ilp
                 do k = 1, n
                    k1k1 = kk + n - k + 1_ilp
                    ! update the lower triangle of a(k:n,k:n)
                    akk = ap( kk )
                    bkk = bp( kk )
                    akk = akk / bkk**2_ilp
                    ap( kk ) = akk
                    if( k<n ) then
                       call stdlib_dscal( n-k, one / bkk, ap( kk+1 ), 1_ilp )
                       ct = -half*akk
                       call stdlib_daxpy( n-k, ct, bp( kk+1 ), 1_ilp, ap( kk+1 ), 1_ilp )
                       call stdlib_dspr2( uplo, n-k, -one, ap( kk+1 ), 1_ilp,bp( kk+1 ), 1_ilp, ap( k1k1 )&
                                  )
                       call stdlib_daxpy( n-k, ct, bp( kk+1 ), 1_ilp, ap( kk+1 ), 1_ilp )
                       call stdlib_dtpsv( uplo, 'NO TRANSPOSE', 'NON-UNIT', n-k,bp( k1k1 ), ap( &
                                 kk+1 ), 1_ilp )
                    end if
                    kk = k1k1
                 end do
              end if
           else
              if( upper ) then
                 ! compute u*a*u**t
                 ! k1 and kk are the indices of a(1,k) and a(k,k)
                 kk = 0_ilp
                 do k = 1, n
                    k1 = kk + 1_ilp
                    kk = kk + k
                    ! update the upper triangle of a(1:k,1:k)
                    akk = ap( kk )
                    bkk = bp( kk )
                    call stdlib_dtpmv( uplo, 'NO TRANSPOSE', 'NON-UNIT', k-1, bp,ap( k1 ), 1_ilp )
                              
                    ct = half*akk
                    call stdlib_daxpy( k-1, ct, bp( k1 ), 1_ilp, ap( k1 ), 1_ilp )
                    call stdlib_dspr2( uplo, k-1, one, ap( k1 ), 1_ilp, bp( k1 ), 1_ilp,ap )
                    call stdlib_daxpy( k-1, ct, bp( k1 ), 1_ilp, ap( k1 ), 1_ilp )
                    call stdlib_dscal( k-1, bkk, ap( k1 ), 1_ilp )
                    ap( kk ) = akk*bkk**2_ilp
                 end do
              else
                 ! compute l**t *a*l
                 ! jj and j1j1 are the indices of a(j,j) and a(j+1,j+1)
                 jj = 1_ilp
                 do j = 1, n
                    j1j1 = jj + n - j + 1_ilp
                    ! compute the j-th column of the lower triangle of a
                    ajj = ap( jj )
                    bjj = bp( jj )
                    ap( jj ) = ajj*bjj + stdlib_ddot( n-j, ap( jj+1 ), 1_ilp,bp( jj+1 ), 1_ilp )
                    call stdlib_dscal( n-j, bjj, ap( jj+1 ), 1_ilp )
                    call stdlib_dspmv( uplo, n-j, one, ap( j1j1 ), bp( jj+1 ), 1_ilp,one, ap( jj+1 ), &
                              1_ilp )
                    call stdlib_dtpmv( uplo, 'TRANSPOSE', 'NON-UNIT', n-j+1,bp( jj ), ap( jj ), 1_ilp &
                              )
                    jj = j1j1
                 end do
              end if
           end if
           return
     end subroutine stdlib_dspgst




     pure module subroutine stdlib_ssbgst( vect, uplo, n, ka, kb, ab, ldab, bb, ldbb, x,ldx, work, info )
     !! SSBGST reduces a real symmetric-definite banded generalized
     !! eigenproblem  A*x = lambda*B*x  to standard form  C*y = lambda*y,
     !! such that C has the same bandwidth as A.
     !! B must have been previously factorized as S**T*S by SPBSTF, using a
     !! split Cholesky factorization. A is overwritten by C = X**T*A*X, where
     !! X = S**(-1)*Q and Q is an orthogonal matrix chosen to preserve the
     !! bandwidth of A.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldx, n
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(in) :: bb(ldbb,*)
           real(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: update, upper, wantx
           integer(ilp) :: i, i0, i1, i2, inca, j, j1, j1t, j2, j2t, k, ka1, kb1, kbt, l, m, nr, &
                     nrt, nx
           real(sp) :: bii, ra, ra1, t
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           wantx = stdlib_lsame( vect, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           ka1 = ka + 1_ilp
           kb1 = kb + 1_ilp
           info = 0_ilp
           if( .not.wantx .and. .not.stdlib_lsame( vect, 'N' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ka<0_ilp ) then
              info = -4_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -5_ilp
           else if( ldab<ka+1 ) then
              info = -7_ilp
           else if( ldbb<kb+1 ) then
              info = -9_ilp
           else if( ldx<1_ilp .or. wantx .and. ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSBGST', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           inca = ldab*ka1
           ! initialize x to the unit matrix, if needed
           if( wantx )call stdlib_slaset( 'FULL', n, n, zero, one, x, ldx )
           ! set m to the splitting point m. it must be the same value as is
           ! used in stdlib_spbstf. the chosen value allows the arrays work and rwork
           ! to be of dimension (n).
           m = ( n+kb ) / 2_ilp
           ! the routine works in two phases, corresponding to the two halves
           ! of the split cholesky factorization of b as s**t*s where
           ! s = ( u    )
               ! ( m  l )
           ! with u upper triangular of order m, and l lower triangular of
           ! order n-m. s has the same bandwidth as b.
           ! s is treated as a product of elementary matrices:
           ! s = s(m)*s(m-1)*...*s(2)*s(1)*s(m+1)*s(m+2)*...*s(n-1)*s(n)
           ! where s(i) is determined by the i-th row of s.
           ! in phase 1, the index i takes the values n, n-1, ... , m+1;
           ! in phase 2, it takes the values 1, 2, ... , m.
           ! for each value of i, the current matrix a is updated by forming
           ! inv(s(i))**t*a*inv(s(i)). this creates a triangular bulge outside
           ! the band of a. the bulge is then pushed down toward the bottom of
           ! a in phase 1, and up toward the top of a in phase 2, by applying
           ! plane rotations.
           ! there are kb*(kb+1)/2 elements in the bulge, but at most 2*kb-1
           ! of them are linearly independent, so annihilating a bulge requires
           ! only 2*kb-1 plane rotations. the rotations are divided into a 1st
           ! set of kb-1 rotations, and a 2nd set of kb rotations.
           ! wherever possible, rotations are generated and applied in vector
           ! operations of length nr between the indices j1 and j2 (sometimes
           ! replaced by modified values nrt, j1t or j2t).
           ! the cosines and sines of the rotations are stored in the array
           ! work. the cosines of the 1st set of rotations are stored in
           ! elements n+2:n+m-kb-1 and the sines of the 1st set in elements
           ! 2:m-kb-1; the cosines of the 2nd set are stored in elements
           ! n+m-kb+1:2*n and the sines of the second set in elements m-kb+1:n.
           ! the bulges are not formed explicitly; nonzero elements outside the
           ! band are created only when they are required for generating new
           ! rotations; they are stored in the array work, in positions where
           ! they are later overwritten by the sines of the rotations which
           ! annihilate them.
           ! **************************** phase 1 *****************************
           ! the logical structure of this phase is:
           ! update = .true.
           ! do i = n, m + 1, -1
              ! use s(i) to update a and create a new bulge
              ! apply rotations to push all bulges ka positions downward
           ! end do
           ! update = .false.
           ! do i = m + ka + 1, n - 1
              ! apply rotations to push all bulges ka positions downward
           ! end do
           ! to avoid duplicating code, the two loops are merged.
           update = .true.
           i = n + 1_ilp
           10 continue
           if( update ) then
              i = i - 1_ilp
              kbt = min( kb, i-1 )
              i0 = i - 1_ilp
              i1 = min( n, i+ka )
              i2 = i - kbt + ka1
              if( i<m+1 ) then
                 update = .false.
                 i = i + 1_ilp
                 i0 = m
                 if( ka==0 )go to 480
                 go to 10
              end if
           else
              i = i + ka
              if( i>n-1 )go to 480
           end if
           if( upper ) then
              ! transform a, working with the upper triangle
              if( update ) then
                 ! form  inv(s(i))**t * a * inv(s(i))
                 bii = bb( kb1, i )
                 do j = i, i1
                    ab( i-j+ka1, j ) = ab( i-j+ka1, j ) / bii
                 end do
                 do j = max( 1, i-ka ), i
                    ab( j-i+ka1, i ) = ab( j-i+ka1, i ) / bii
                 end do
                 do k = i - kbt, i - 1
                    do j = i - kbt, k
                       ab( j-k+ka1, k ) = ab( j-k+ka1, k ) -bb( j-i+kb1, i )*ab( k-i+ka1, i ) -bb(&
                        k-i+kb1, i )*ab( j-i+ka1, i ) +ab( ka1, i )*bb( j-i+kb1, i )*bb( k-i+kb1, &
                                  i )
                    end do
                    do j = max( 1, i-ka ), i - kbt - 1
                       ab( j-k+ka1, k ) = ab( j-k+ka1, k ) -bb( k-i+kb1, i )*ab( j-i+ka1, i )
                                 
                    end do
                 end do
                 do j = i, i1
                    do k = max( j-ka, i-kbt ), i - 1
                       ab( k-j+ka1, j ) = ab( k-j+ka1, j ) -bb( k-i+kb1, i )*ab( i-j+ka1, j )
                                 
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_sscal( n-m, one / bii, x( m+1, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_sger( n-m, kbt, -one, x( m+1, i ), 1_ilp,bb( kb1-kbt, i ), &
                              1_ilp, x( m+1, i-kbt ), ldx )
                 end if
                 ! store a(i,i1) in ra1 for use in next loop over k
                 ra1 = ab( i-i1+ka1, i1 )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions down toward the bottom of the
              ! band
              loop_130: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i-k+ka<n .and. i-k>1_ilp ) then
                       ! generate rotation to annihilate a(i,i-k+ka+1)
                       call stdlib_slartg( ab( k+1, i-k+ka ), ra1,work( n+i-k+ka-m ), work( i-k+&
                                 ka-m ),ra )
                       ! create nonzero element a(i-k,i-k+ka+1) outside the
                       ! band and store it in work(i-k)
                       t = -bb( kb1-k, i )*ra1
                       work( i-k ) = work( n+i-k+ka-m )*t -work( i-k+ka-m )*ab( 1_ilp, i-k+ka )
                                 
                       ab( 1_ilp, i-k+ka ) = work( i-k+ka-m )*t +work( n+i-k+ka-m )*ab( 1_ilp, i-k+ka )
                                 
                       ra1 = ra
                    end if
                 end if
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( update ) then
                    j2t = max( j2, i+2*ka-k+1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( n-j2t+ka ) / ka1
                 do j = j2t, j1, ka1
                    ! create nonzero element a(j-ka,j+1) outside the band
                    ! and store it in work(j-m)
                    work( j-m ) = work( j-m )*ab( 1_ilp, j+1 )
                    ab( 1_ilp, j+1 ) = work( n+j-m )*ab( 1_ilp, j+1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_slargv( nrt, ab( 1_ilp, j2t ), inca, work( j2t-m ), ka1,work( &
                           n+j2t-m ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the right
                    do l = 1, ka - 1
                       call stdlib_slartv( nr, ab( ka1-l, j2 ), inca,ab( ka-l, j2+1 ), inca, work(&
                                  n+j2-m ),work( j2-m ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_slar2v( nr, ab( ka1, j2 ), ab( ka1, j2+1 ),ab( ka, j2+1 ), inca, &
                              work( n+j2-m ),work( j2-m ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( l, j2+ka1-l ), inca,ab( l+1, j2+ka1-l &
                              ), inca,work( n+j2-m ), work( j2-m ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j2, j1, ka1
                       call stdlib_srot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,work( n+j-m ), &
                                 work( j-m ) )
                    end do
                 end if
              end do loop_130
              if( update ) then
                 if( i2<=n .and. kbt>0_ilp ) then
                    ! create nonzero element a(i-kbt,i-kbt+ka+1) outside the
                    ! band and store it in work(i-kbt)
                    work( i-kbt ) = -bb( kb1-kbt, i )*ra1
                 end if
              end if
              loop_170: do k = kb, 1, -1
                 if( update ) then
                    j2 = i - k - 1_ilp + max( 2_ilp, k-i0+1 )*ka1
                 else
                    j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the left
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+ka+l ) / ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( l, j2-l+1 ), inca,ab( l+1, j2-l+1 ), &
                              inca, work( n+j2-ka ),work( j2-ka ), ka1 )
                 end do
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 do j = j1, j2, -ka1
                    work( j ) = work( j-ka )
                    work( n+j ) = work( n+j-ka )
                 end do
                 do j = j2, j1, ka1
                    ! create nonzero element a(j-ka,j+1) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( 1_ilp, j+1 )
                    ab( 1_ilp, j+1 ) = work( n+j )*ab( 1_ilp, j+1 )
                 end do
                 if( update ) then
                    if( i-k<n-ka .and. k<=kbt )work( i-k+ka ) = work( i-k )
                 end if
              end do loop_170
              loop_210: do k = kb, 1, -1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_slargv( nr, ab( 1_ilp, j2 ), inca, work( j2 ), ka1,work( n+j2 ), ka1 )
                              
                    ! apply rotations in 2nd set from the right
                    do l = 1, ka - 1
                       call stdlib_slartv( nr, ab( ka1-l, j2 ), inca,ab( ka-l, j2+1 ), inca, work(&
                                  n+j2 ),work( j2 ), ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_slar2v( nr, ab( ka1, j2 ), ab( ka1, j2+1 ),ab( ka, j2+1 ), inca, &
                              work( n+j2 ),work( j2 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( l, j2+ka1-l ), inca,ab( l+1, j2+ka1-l &
                              ), inca, work( n+j2 ),work( j2 ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j2, j1, ka1
                       call stdlib_srot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,work( n+j ), work( &
                                 j ) )
                    end do
                 end if
              end do loop_210
              do k = 1, kb - 1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 ! finish applying rotations in 1st set from the left
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( l, j2+ka1-l ), inca,ab( l+1, j2+ka1-l &
                              ), inca,work( n+j2-m ), work( j2-m ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = n - 1, i - kb + 2*ka + 1, -1
                    work( n+j-m ) = work( n+j-ka-m )
                    work( j-m ) = work( j-ka-m )
                 end do
              end if
           else
              ! transform a, working with the lower triangle
              if( update ) then
                 ! form  inv(s(i))**t * a * inv(s(i))
                 bii = bb( 1_ilp, i )
                 do j = i, i1
                    ab( j-i+1, i ) = ab( j-i+1, i ) / bii
                 end do
                 do j = max( 1, i-ka ), i
                    ab( i-j+1, j ) = ab( i-j+1, j ) / bii
                 end do
                 do k = i - kbt, i - 1
                    do j = i - kbt, k
                       ab( k-j+1, j ) = ab( k-j+1, j ) -bb( i-j+1, j )*ab( i-k+1, k ) -bb( i-k+1, &
                                 k )*ab( i-j+1, j ) +ab( 1_ilp, i )*bb( i-j+1, j )*bb( i-k+1, k )
                    end do
                    do j = max( 1, i-ka ), i - kbt - 1
                       ab( k-j+1, j ) = ab( k-j+1, j ) -bb( i-k+1, k )*ab( i-j+1, j )
                    end do
                 end do
                 do j = i, i1
                    do k = max( j-ka, i-kbt ), i - 1
                       ab( j-k+1, k ) = ab( j-k+1, k ) -bb( i-k+1, k )*ab( j-i+1, i )
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_sscal( n-m, one / bii, x( m+1, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_sger( n-m, kbt, -one, x( m+1, i ), 1_ilp,bb( kbt+1, i-kbt )&
                              , ldbb-1,x( m+1, i-kbt ), ldx )
                 end if
                 ! store a(i1,i) in ra1 for use in next loop over k
                 ra1 = ab( i1-i+1, i )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions down toward the bottom of the
              ! band
              loop_360: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i-k+ka<n .and. i-k>1_ilp ) then
                       ! generate rotation to annihilate a(i-k+ka+1,i)
                       call stdlib_slartg( ab( ka1-k, i ), ra1, work( n+i-k+ka-m ),work( i-k+ka-m &
                                 ), ra )
                       ! create nonzero element a(i-k+ka+1,i-k) outside the
                       ! band and store it in work(i-k)
                       t = -bb( k+1, i-k )*ra1
                       work( i-k ) = work( n+i-k+ka-m )*t -work( i-k+ka-m )*ab( ka1, i-k )
                       ab( ka1, i-k ) = work( i-k+ka-m )*t +work( n+i-k+ka-m )*ab( ka1, i-k )
                                 
                       ra1 = ra
                    end if
                 end if
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( update ) then
                    j2t = max( j2, i+2*ka-k+1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( n-j2t+ka ) / ka1
                 do j = j2t, j1, ka1
                    ! create nonzero element a(j+1,j-ka) outside the band
                    ! and store it in work(j-m)
                    work( j-m ) = work( j-m )*ab( ka1, j-ka+1 )
                    ab( ka1, j-ka+1 ) = work( n+j-m )*ab( ka1, j-ka+1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_slargv( nrt, ab( ka1, j2t-ka ), inca, work( j2t-m ),ka1, &
                           work( n+j2t-m ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the left
                    do l = 1, ka - 1
                       call stdlib_slartv( nr, ab( l+1, j2-l ), inca,ab( l+2, j2-l ), inca, work( &
                                 n+j2-m ),work( j2-m ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_slar2v( nr, ab( 1_ilp, j2 ), ab( 1_ilp, j2+1 ), ab( 2_ilp, j2 ),inca, work( n+&
                              j2-m ), work( j2-m ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( ka1-l+1, j2 ), inca,ab( ka1-l, j2+1 ),&
                               inca, work( n+j2-m ),work( j2-m ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j2, j1, ka1
                       call stdlib_srot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,work( n+j-m ), &
                                 work( j-m ) )
                    end do
                 end if
              end do loop_360
              if( update ) then
                 if( i2<=n .and. kbt>0_ilp ) then
                    ! create nonzero element a(i-kbt+ka+1,i-kbt) outside the
                    ! band and store it in work(i-kbt)
                    work( i-kbt ) = -bb( kbt+1, i-kbt )*ra1
                 end if
              end if
              loop_400: do k = kb, 1, -1
                 if( update ) then
                    j2 = i - k - 1_ilp + max( 2_ilp, k-i0+1 )*ka1
                 else
                    j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the right
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+ka+l ) / ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( ka1-l+1, j2-ka ), inca,ab( ka1-l, j2-&
                              ka+1 ), inca,work( n+j2-ka ), work( j2-ka ), ka1 )
                 end do
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 do j = j1, j2, -ka1
                    work( j ) = work( j-ka )
                    work( n+j ) = work( n+j-ka )
                 end do
                 do j = j2, j1, ka1
                    ! create nonzero element a(j+1,j-ka) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( ka1, j-ka+1 )
                    ab( ka1, j-ka+1 ) = work( n+j )*ab( ka1, j-ka+1 )
                 end do
                 if( update ) then
                    if( i-k<n-ka .and. k<=kbt )work( i-k+ka ) = work( i-k )
                 end if
              end do loop_400
              loop_440: do k = kb, 1, -1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_slargv( nr, ab( ka1, j2-ka ), inca, work( j2 ), ka1,work( n+j2 ), &
                              ka1 )
                    ! apply rotations in 2nd set from the left
                    do l = 1, ka - 1
                       call stdlib_slartv( nr, ab( l+1, j2-l ), inca,ab( l+2, j2-l ), inca, work( &
                                 n+j2 ),work( j2 ), ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_slar2v( nr, ab( 1_ilp, j2 ), ab( 1_ilp, j2+1 ), ab( 2_ilp, j2 ),inca, work( n+&
                              j2 ), work( j2 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( ka1-l+1, j2 ), inca,ab( ka1-l, j2+1 ),&
                               inca, work( n+j2 ),work( j2 ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j2, j1, ka1
                       call stdlib_srot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,work( n+j ), work( &
                                 j ) )
                    end do
                 end if
              end do loop_440
              do k = 1, kb - 1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 ! finish applying rotations in 1st set from the right
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( ka1-l+1, j2 ), inca,ab( ka1-l, j2+1 ),&
                               inca, work( n+j2-m ),work( j2-m ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = n - 1, i - kb + 2*ka + 1, -1
                    work( n+j-m ) = work( n+j-ka-m )
                    work( j-m ) = work( j-ka-m )
                 end do
              end if
           end if
           go to 10
           480 continue
           ! **************************** phase 2 *****************************
           ! the logical structure of this phase is:
           ! update = .true.
           ! do i = 1, m
              ! use s(i) to update a and create a new bulge
              ! apply rotations to push all bulges ka positions upward
           ! end do
           ! update = .false.
           ! do i = m - ka - 1, 2, -1
              ! apply rotations to push all bulges ka positions upward
           ! end do
           ! to avoid duplicating code, the two loops are merged.
           update = .true.
           i = 0_ilp
           490 continue
           if( update ) then
              i = i + 1_ilp
              kbt = min( kb, m-i )
              i0 = i + 1_ilp
              i1 = max( 1_ilp, i-ka )
              i2 = i + kbt - ka1
              if( i>m ) then
                 update = .false.
                 i = i - 1_ilp
                 i0 = m + 1_ilp
                 if( ka==0 )return
                 go to 490
              end if
           else
              i = i - ka
              if( i<2 )return
           end if
           if( i<m-kbt ) then
              nx = m
           else
              nx = n
           end if
           if( upper ) then
              ! transform a, working with the upper triangle
              if( update ) then
                 ! form  inv(s(i))**t * a * inv(s(i))
                 bii = bb( kb1, i )
                 do j = i1, i
                    ab( j-i+ka1, i ) = ab( j-i+ka1, i ) / bii
                 end do
                 do j = i, min( n, i+ka )
                    ab( i-j+ka1, j ) = ab( i-j+ka1, j ) / bii
                 end do
                 do k = i + 1, i + kbt
                    do j = k, i + kbt
                       ab( k-j+ka1, j ) = ab( k-j+ka1, j ) -bb( i-j+kb1, j )*ab( i-k+ka1, k ) -bb(&
                        i-k+kb1, k )*ab( i-j+ka1, j ) +ab( ka1, i )*bb( i-j+kb1, j )*bb( i-k+kb1, &
                                  k )
                    end do
                    do j = i + kbt + 1, min( n, i+ka )
                       ab( k-j+ka1, j ) = ab( k-j+ka1, j ) -bb( i-k+kb1, k )*ab( i-j+ka1, j )
                                 
                    end do
                 end do
                 do j = i1, i
                    do k = i + 1, min( j+ka, i+kbt )
                       ab( j-k+ka1, k ) = ab( j-k+ka1, k ) -bb( i-k+kb1, k )*ab( j-i+ka1, i )
                                 
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_sscal( nx, one / bii, x( 1_ilp, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_sger( nx, kbt, -one, x( 1_ilp, i ), 1_ilp, bb( kb, i+1 ),ldbb-&
                              1_ilp, x( 1_ilp, i+1 ), ldx )
                 end if
                 ! store a(i1,i) in ra1 for use in next loop over k
                 ra1 = ab( i1-i+ka1, i )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions up toward the top of the band
              loop_610: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i+k-ka1>0_ilp .and. i+k<m ) then
                       ! generate rotation to annihilate a(i+k-ka-1,i)
                       call stdlib_slartg( ab( k+1, i ), ra1, work( n+i+k-ka ),work( i+k-ka ), ra &
                                 )
                       ! create nonzero element a(i+k-ka-1,i+k) outside the
                       ! band and store it in work(m-kb+i+k)
                       t = -bb( kb1-k, i+k )*ra1
                       work( m-kb+i+k ) = work( n+i+k-ka )*t -work( i+k-ka )*ab( 1_ilp, i+k )
                       ab( 1_ilp, i+k ) = work( i+k-ka )*t +work( n+i+k-ka )*ab( 1_ilp, i+k )
                       ra1 = ra
                    end if
                 end if
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( update ) then
                    j2t = min( j2, i-2*ka+k-1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( j2t+ka-1 ) / ka1
                 do j = j1, j2t, ka1
                    ! create nonzero element a(j-1,j+ka) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( 1_ilp, j+ka-1 )
                    ab( 1_ilp, j+ka-1 ) = work( n+j )*ab( 1_ilp, j+ka-1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_slargv( nrt, ab( 1_ilp, j1+ka ), inca, work( j1 ), ka1,work( &
                           n+j1 ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the left
                    do l = 1, ka - 1
                       call stdlib_slartv( nr, ab( ka1-l, j1+l ), inca,ab( ka-l, j1+l ), inca, &
                                 work( n+j1 ),work( j1 ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_slar2v( nr, ab( ka1, j1 ), ab( ka1, j1-1 ),ab( ka, j1 ), inca, &
                              work( n+j1 ),work( j1 ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( l, j1t ), inca,ab( l+1, j1t-1 ), inca,&
                               work( n+j1t ),work( j1t ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j1, j2, ka1
                       call stdlib_srot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,work( n+j ), work( j ) )
                                 
                    end do
                 end if
              end do loop_610
              if( update ) then
                 if( i2>0_ilp .and. kbt>0_ilp ) then
                    ! create nonzero element a(i+kbt-ka-1,i+kbt) outside the
                    ! band and store it in work(m-kb+i+kbt)
                    work( m-kb+i+kbt ) = -bb( kb1-kbt, i+kbt )*ra1
                 end if
              end if
              loop_650: do k = kb, 1, -1
                 if( update ) then
                    j2 = i + k + 1_ilp - max( 2_ilp, k+i0-m )*ka1
                 else
                    j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the right
                 do l = kb - k, 1, -1
                    nrt = ( j2+ka+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( l, j1t+ka ), inca,ab( l+1, j1t+ka-1 ),&
                               inca,work( n+m-kb+j1t+ka ),work( m-kb+j1t+ka ), ka1 )
                 end do
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 do j = j1, j2, ka1
                    work( m-kb+j ) = work( m-kb+j+ka )
                    work( n+m-kb+j ) = work( n+m-kb+j+ka )
                 end do
                 do j = j1, j2, ka1
                    ! create nonzero element a(j-1,j+ka) outside the band
                    ! and store it in work(m-kb+j)
                    work( m-kb+j ) = work( m-kb+j )*ab( 1_ilp, j+ka-1 )
                    ab( 1_ilp, j+ka-1 ) = work( n+m-kb+j )*ab( 1_ilp, j+ka-1 )
                 end do
                 if( update ) then
                    if( i+k>ka1 .and. k<=kbt )work( m-kb+i+k-ka ) = work( m-kb+i+k )
                 end if
              end do loop_650
              loop_690: do k = kb, 1, -1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_slargv( nr, ab( 1_ilp, j1+ka ), inca, work( m-kb+j1 ),ka1, work( n+m-&
                              kb+j1 ), ka1 )
                    ! apply rotations in 2nd set from the left
                    do l = 1, ka - 1
                       call stdlib_slartv( nr, ab( ka1-l, j1+l ), inca,ab( ka-l, j1+l ), inca,&
                                 work( n+m-kb+j1 ), work( m-kb+j1 ), ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_slar2v( nr, ab( ka1, j1 ), ab( ka1, j1-1 ),ab( ka, j1 ), inca, &
                              work( n+m-kb+j1 ),work( m-kb+j1 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( l, j1t ), inca,ab( l+1, j1t-1 ), inca,&
                              work( n+m-kb+j1t ), work( m-kb+j1t ),ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j1, j2, ka1
                       call stdlib_srot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,work( n+m-kb+j ), work( &
                                 m-kb+j ) )
                    end do
                 end if
              end do loop_690
              do k = 1, kb - 1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 ! finish applying rotations in 1st set from the right
                 do l = kb - k, 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( l, j1t ), inca,ab( l+1, j1t-1 ), inca,&
                               work( n+j1t ),work( j1t ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = 2, min( i+kb, m ) - 2*ka - 1
                    work( n+j ) = work( n+j+ka )
                    work( j ) = work( j+ka )
                 end do
              end if
           else
              ! transform a, working with the lower triangle
              if( update ) then
                 ! form  inv(s(i))**t * a * inv(s(i))
                 bii = bb( 1_ilp, i )
                 do j = i1, i
                    ab( i-j+1, j ) = ab( i-j+1, j ) / bii
                 end do
                 do j = i, min( n, i+ka )
                    ab( j-i+1, i ) = ab( j-i+1, i ) / bii
                 end do
                 do k = i + 1, i + kbt
                    do j = k, i + kbt
                       ab( j-k+1, k ) = ab( j-k+1, k ) -bb( j-i+1, i )*ab( k-i+1, i ) -bb( k-i+1, &
                                 i )*ab( j-i+1, i ) +ab( 1_ilp, i )*bb( j-i+1, i )*bb( k-i+1, i )
                    end do
                    do j = i + kbt + 1, min( n, i+ka )
                       ab( j-k+1, k ) = ab( j-k+1, k ) -bb( k-i+1, i )*ab( j-i+1, i )
                    end do
                 end do
                 do j = i1, i
                    do k = i + 1, min( j+ka, i+kbt )
                       ab( k-j+1, j ) = ab( k-j+1, j ) -bb( k-i+1, i )*ab( i-j+1, j )
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_sscal( nx, one / bii, x( 1_ilp, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_sger( nx, kbt, -one, x( 1_ilp, i ), 1_ilp, bb( 2_ilp, i ), 1_ilp,x( 1_ilp, &
                              i+1 ), ldx )
                 end if
                 ! store a(i,i1) in ra1 for use in next loop over k
                 ra1 = ab( i-i1+1, i1 )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions up toward the top of the band
              loop_840: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i+k-ka1>0_ilp .and. i+k<m ) then
                       ! generate rotation to annihilate a(i,i+k-ka-1)
                       call stdlib_slartg( ab( ka1-k, i+k-ka ), ra1,work( n+i+k-ka ), work( i+k-&
                                 ka ), ra )
                       ! create nonzero element a(i+k,i+k-ka-1) outside the
                       ! band and store it in work(m-kb+i+k)
                       t = -bb( k+1, i )*ra1
                       work( m-kb+i+k ) = work( n+i+k-ka )*t -work( i+k-ka )*ab( ka1, i+k-ka )
                                 
                       ab( ka1, i+k-ka ) = work( i+k-ka )*t +work( n+i+k-ka )*ab( ka1, i+k-ka )
                                 
                       ra1 = ra
                    end if
                 end if
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( update ) then
                    j2t = min( j2, i-2*ka+k-1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( j2t+ka-1 ) / ka1
                 do j = j1, j2t, ka1
                    ! create nonzero element a(j+ka,j-1) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( ka1, j-1 )
                    ab( ka1, j-1 ) = work( n+j )*ab( ka1, j-1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_slargv( nrt, ab( ka1, j1 ), inca, work( j1 ), ka1,work( n+&
                           j1 ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the right
                    do l = 1, ka - 1
                       call stdlib_slartv( nr, ab( l+1, j1 ), inca, ab( l+2, j1-1 ),inca, work( n+&
                                 j1 ), work( j1 ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_slar2v( nr, ab( 1_ilp, j1 ), ab( 1_ilp, j1-1 ),ab( 2_ilp, j1-1 ), inca, work( &
                              n+j1 ),work( j1 ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( ka1-l+1, j1t-ka1+l ), inca,ab( ka1-l, &
                              j1t-ka1+l ), inca,work( n+j1t ), work( j1t ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j1, j2, ka1
                       call stdlib_srot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,work( n+j ), work( j ) )
                                 
                    end do
                 end if
              end do loop_840
              if( update ) then
                 if( i2>0_ilp .and. kbt>0_ilp ) then
                    ! create nonzero element a(i+kbt,i+kbt-ka-1) outside the
                    ! band and store it in work(m-kb+i+kbt)
                    work( m-kb+i+kbt ) = -bb( kbt+1, i )*ra1
                 end if
              end if
              loop_880: do k = kb, 1, -1
                 if( update ) then
                    j2 = i + k + 1_ilp - max( 2_ilp, k+i0-m )*ka1
                 else
                    j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the left
                 do l = kb - k, 1, -1
                    nrt = ( j2+ka+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( ka1-l+1, j1t+l-1 ), inca,ab( ka1-l, &
                              j1t+l-1 ), inca,work( n+m-kb+j1t+ka ),work( m-kb+j1t+ka ), ka1 )
                 end do
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 do j = j1, j2, ka1
                    work( m-kb+j ) = work( m-kb+j+ka )
                    work( n+m-kb+j ) = work( n+m-kb+j+ka )
                 end do
                 do j = j1, j2, ka1
                    ! create nonzero element a(j+ka,j-1) outside the band
                    ! and store it in work(m-kb+j)
                    work( m-kb+j ) = work( m-kb+j )*ab( ka1, j-1 )
                    ab( ka1, j-1 ) = work( n+m-kb+j )*ab( ka1, j-1 )
                 end do
                 if( update ) then
                    if( i+k>ka1 .and. k<=kbt )work( m-kb+i+k-ka ) = work( m-kb+i+k )
                 end if
              end do loop_880
              loop_920: do k = kb, 1, -1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_slargv( nr, ab( ka1, j1 ), inca, work( m-kb+j1 ),ka1, work( n+m-&
                              kb+j1 ), ka1 )
                    ! apply rotations in 2nd set from the right
                    do l = 1, ka - 1
                       call stdlib_slartv( nr, ab( l+1, j1 ), inca, ab( l+2, j1-1 ),inca, work( n+&
                                 m-kb+j1 ), work( m-kb+j1 ),ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_slar2v( nr, ab( 1_ilp, j1 ), ab( 1_ilp, j1-1 ),ab( 2_ilp, j1-1 ), inca, work( &
                              n+m-kb+j1 ),work( m-kb+j1 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( ka1-l+1, j1t-ka1+l ), inca,ab( ka1-l, &
                              j1t-ka1+l ), inca,work( n+m-kb+j1t ), work( m-kb+j1t ),ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j1, j2, ka1
                       call stdlib_srot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,work( n+m-kb+j ), work( &
                                 m-kb+j ) )
                    end do
                 end if
              end do loop_920
              do k = 1, kb - 1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 ! finish applying rotations in 1st set from the left
                 do l = kb - k, 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_slartv( nrt, ab( ka1-l+1, j1t-ka1+l ), inca,ab( ka1-l, &
                              j1t-ka1+l ), inca,work( n+j1t ), work( j1t ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = 2, min( i+kb, m ) - 2*ka - 1
                    work( n+j ) = work( n+j+ka )
                    work( j ) = work( j+ka )
                 end do
              end if
           end if
           go to 490
     end subroutine stdlib_ssbgst

     pure module subroutine stdlib_dsbgst( vect, uplo, n, ka, kb, ab, ldab, bb, ldbb, x,ldx, work, info )
     !! DSBGST reduces a real symmetric-definite banded generalized
     !! eigenproblem  A*x = lambda*B*x  to standard form  C*y = lambda*y,
     !! such that C has the same bandwidth as A.
     !! B must have been previously factorized as S**T*S by DPBSTF, using a
     !! split Cholesky factorization. A is overwritten by C = X**T*A*X, where
     !! X = S**(-1)*Q and Q is an orthogonal matrix chosen to preserve the
     !! bandwidth of A.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldx, n
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(in) :: bb(ldbb,*)
           real(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: update, upper, wantx
           integer(ilp) :: i, i0, i1, i2, inca, j, j1, j1t, j2, j2t, k, ka1, kb1, kbt, l, m, nr, &
                     nrt, nx
           real(dp) :: bii, ra, ra1, t
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           wantx = stdlib_lsame( vect, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           ka1 = ka + 1_ilp
           kb1 = kb + 1_ilp
           info = 0_ilp
           if( .not.wantx .and. .not.stdlib_lsame( vect, 'N' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ka<0_ilp ) then
              info = -4_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -5_ilp
           else if( ldab<ka+1 ) then
              info = -7_ilp
           else if( ldbb<kb+1 ) then
              info = -9_ilp
           else if( ldx<1_ilp .or. wantx .and. ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSBGST', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           inca = ldab*ka1
           ! initialize x to the unit matrix, if needed
           if( wantx )call stdlib_dlaset( 'FULL', n, n, zero, one, x, ldx )
           ! set m to the splitting point m. it must be the same value as is
           ! used in stdlib_dpbstf. the chosen value allows the arrays work and rwork
           ! to be of dimension (n).
           m = ( n+kb ) / 2_ilp
           ! the routine works in two phases, corresponding to the two halves
           ! of the split cholesky factorization of b as s**t*s where
           ! s = ( u    )
               ! ( m  l )
           ! with u upper triangular of order m, and l lower triangular of
           ! order n-m. s has the same bandwidth as b.
           ! s is treated as a product of elementary matrices:
           ! s = s(m)*s(m-1)*...*s(2)*s(1)*s(m+1)*s(m+2)*...*s(n-1)*s(n)
           ! where s(i) is determined by the i-th row of s.
           ! in phase 1, the index i takes the values n, n-1, ... , m+1;
           ! in phase 2, it takes the values 1, 2, ... , m.
           ! for each value of i, the current matrix a is updated by forming
           ! inv(s(i))**t*a*inv(s(i)). this creates a triangular bulge outside
           ! the band of a. the bulge is then pushed down toward the bottom of
           ! a in phase 1, and up toward the top of a in phase 2, by applying
           ! plane rotations.
           ! there are kb*(kb+1)/2 elements in the bulge, but at most 2*kb-1
           ! of them are linearly independent, so annihilating a bulge requires
           ! only 2*kb-1 plane rotations. the rotations are divided into a 1st
           ! set of kb-1 rotations, and a 2nd set of kb rotations.
           ! wherever possible, rotations are generated and applied in vector
           ! operations of length nr between the indices j1 and j2 (sometimes
           ! replaced by modified values nrt, j1t or j2t).
           ! the cosines and sines of the rotations are stored in the array
           ! work. the cosines of the 1st set of rotations are stored in
           ! elements n+2:n+m-kb-1 and the sines of the 1st set in elements
           ! 2:m-kb-1; the cosines of the 2nd set are stored in elements
           ! n+m-kb+1:2*n and the sines of the second set in elements m-kb+1:n.
           ! the bulges are not formed explicitly; nonzero elements outside the
           ! band are created only when they are required for generating new
           ! rotations; they are stored in the array work, in positions where
           ! they are later overwritten by the sines of the rotations which
           ! annihilate them.
           ! **************************** phase 1 *****************************
           ! the logical structure of this phase is:
           ! update = .true.
           ! do i = n, m + 1, -1
              ! use s(i) to update a and create a new bulge
              ! apply rotations to push all bulges ka positions downward
           ! end do
           ! update = .false.
           ! do i = m + ka + 1, n - 1
              ! apply rotations to push all bulges ka positions downward
           ! end do
           ! to avoid duplicating code, the two loops are merged.
           update = .true.
           i = n + 1_ilp
           10 continue
           if( update ) then
              i = i - 1_ilp
              kbt = min( kb, i-1 )
              i0 = i - 1_ilp
              i1 = min( n, i+ka )
              i2 = i - kbt + ka1
              if( i<m+1 ) then
                 update = .false.
                 i = i + 1_ilp
                 i0 = m
                 if( ka==0 )go to 480
                 go to 10
              end if
           else
              i = i + ka
              if( i>n-1 )go to 480
           end if
           if( upper ) then
              ! transform a, working with the upper triangle
              if( update ) then
                 ! form  inv(s(i))**t * a * inv(s(i))
                 bii = bb( kb1, i )
                 do j = i, i1
                    ab( i-j+ka1, j ) = ab( i-j+ka1, j ) / bii
                 end do
                 do j = max( 1, i-ka ), i
                    ab( j-i+ka1, i ) = ab( j-i+ka1, i ) / bii
                 end do
                 do k = i - kbt, i - 1
                    do j = i - kbt, k
                       ab( j-k+ka1, k ) = ab( j-k+ka1, k ) -bb( j-i+kb1, i )*ab( k-i+ka1, i ) -bb(&
                        k-i+kb1, i )*ab( j-i+ka1, i ) +ab( ka1, i )*bb( j-i+kb1, i )*bb( k-i+kb1, &
                                  i )
                    end do
                    do j = max( 1, i-ka ), i - kbt - 1
                       ab( j-k+ka1, k ) = ab( j-k+ka1, k ) -bb( k-i+kb1, i )*ab( j-i+ka1, i )
                                 
                    end do
                 end do
                 do j = i, i1
                    do k = max( j-ka, i-kbt ), i - 1
                       ab( k-j+ka1, j ) = ab( k-j+ka1, j ) -bb( k-i+kb1, i )*ab( i-j+ka1, j )
                                 
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_dscal( n-m, one / bii, x( m+1, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_dger( n-m, kbt, -one, x( m+1, i ), 1_ilp,bb( kb1-kbt, i ), &
                              1_ilp, x( m+1, i-kbt ), ldx )
                 end if
                 ! store a(i,i1) in ra1 for use in next loop over k
                 ra1 = ab( i-i1+ka1, i1 )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions down toward the bottom of the
              ! band
              loop_130: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i-k+ka<n .and. i-k>1_ilp ) then
                       ! generate rotation to annihilate a(i,i-k+ka+1)
                       call stdlib_dlartg( ab( k+1, i-k+ka ), ra1,work( n+i-k+ka-m ), work( i-k+&
                                 ka-m ),ra )
                       ! create nonzero element a(i-k,i-k+ka+1) outside the
                       ! band and store it in work(i-k)
                       t = -bb( kb1-k, i )*ra1
                       work( i-k ) = work( n+i-k+ka-m )*t -work( i-k+ka-m )*ab( 1_ilp, i-k+ka )
                                 
                       ab( 1_ilp, i-k+ka ) = work( i-k+ka-m )*t +work( n+i-k+ka-m )*ab( 1_ilp, i-k+ka )
                                 
                       ra1 = ra
                    end if
                 end if
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( update ) then
                    j2t = max( j2, i+2*ka-k+1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( n-j2t+ka ) / ka1
                 do j = j2t, j1, ka1
                    ! create nonzero element a(j-ka,j+1) outside the band
                    ! and store it in work(j-m)
                    work( j-m ) = work( j-m )*ab( 1_ilp, j+1 )
                    ab( 1_ilp, j+1 ) = work( n+j-m )*ab( 1_ilp, j+1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_dlargv( nrt, ab( 1_ilp, j2t ), inca, work( j2t-m ), ka1,work( &
                           n+j2t-m ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the right
                    do l = 1, ka - 1
                       call stdlib_dlartv( nr, ab( ka1-l, j2 ), inca,ab( ka-l, j2+1 ), inca, work(&
                                  n+j2-m ),work( j2-m ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_dlar2v( nr, ab( ka1, j2 ), ab( ka1, j2+1 ),ab( ka, j2+1 ), inca, &
                              work( n+j2-m ),work( j2-m ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( l, j2+ka1-l ), inca,ab( l+1, j2+ka1-l &
                              ), inca,work( n+j2-m ), work( j2-m ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j2, j1, ka1
                       call stdlib_drot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,work( n+j-m ), &
                                 work( j-m ) )
                    end do
                 end if
              end do loop_130
              if( update ) then
                 if( i2<=n .and. kbt>0_ilp ) then
                    ! create nonzero element a(i-kbt,i-kbt+ka+1) outside the
                    ! band and store it in work(i-kbt)
                    work( i-kbt ) = -bb( kb1-kbt, i )*ra1
                 end if
              end if
              loop_170: do k = kb, 1, -1
                 if( update ) then
                    j2 = i - k - 1_ilp + max( 2_ilp, k-i0+1 )*ka1
                 else
                    j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the left
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+ka+l ) / ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( l, j2-l+1 ), inca,ab( l+1, j2-l+1 ), &
                              inca, work( n+j2-ka ),work( j2-ka ), ka1 )
                 end do
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 do j = j1, j2, -ka1
                    work( j ) = work( j-ka )
                    work( n+j ) = work( n+j-ka )
                 end do
                 do j = j2, j1, ka1
                    ! create nonzero element a(j-ka,j+1) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( 1_ilp, j+1 )
                    ab( 1_ilp, j+1 ) = work( n+j )*ab( 1_ilp, j+1 )
                 end do
                 if( update ) then
                    if( i-k<n-ka .and. k<=kbt )work( i-k+ka ) = work( i-k )
                 end if
              end do loop_170
              loop_210: do k = kb, 1, -1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_dlargv( nr, ab( 1_ilp, j2 ), inca, work( j2 ), ka1,work( n+j2 ), ka1 )
                              
                    ! apply rotations in 2nd set from the right
                    do l = 1, ka - 1
                       call stdlib_dlartv( nr, ab( ka1-l, j2 ), inca,ab( ka-l, j2+1 ), inca, work(&
                                  n+j2 ),work( j2 ), ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_dlar2v( nr, ab( ka1, j2 ), ab( ka1, j2+1 ),ab( ka, j2+1 ), inca, &
                              work( n+j2 ),work( j2 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( l, j2+ka1-l ), inca,ab( l+1, j2+ka1-l &
                              ), inca, work( n+j2 ),work( j2 ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j2, j1, ka1
                       call stdlib_drot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,work( n+j ), work( &
                                 j ) )
                    end do
                 end if
              end do loop_210
              do k = 1, kb - 1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 ! finish applying rotations in 1st set from the left
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( l, j2+ka1-l ), inca,ab( l+1, j2+ka1-l &
                              ), inca,work( n+j2-m ), work( j2-m ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = n - 1, i - kb + 2*ka + 1, -1
                    work( n+j-m ) = work( n+j-ka-m )
                    work( j-m ) = work( j-ka-m )
                 end do
              end if
           else
              ! transform a, working with the lower triangle
              if( update ) then
                 ! form  inv(s(i))**t * a * inv(s(i))
                 bii = bb( 1_ilp, i )
                 do j = i, i1
                    ab( j-i+1, i ) = ab( j-i+1, i ) / bii
                 end do
                 do j = max( 1, i-ka ), i
                    ab( i-j+1, j ) = ab( i-j+1, j ) / bii
                 end do
                 do k = i - kbt, i - 1
                    do j = i - kbt, k
                       ab( k-j+1, j ) = ab( k-j+1, j ) -bb( i-j+1, j )*ab( i-k+1, k ) -bb( i-k+1, &
                                 k )*ab( i-j+1, j ) +ab( 1_ilp, i )*bb( i-j+1, j )*bb( i-k+1, k )
                    end do
                    do j = max( 1, i-ka ), i - kbt - 1
                       ab( k-j+1, j ) = ab( k-j+1, j ) -bb( i-k+1, k )*ab( i-j+1, j )
                    end do
                 end do
                 do j = i, i1
                    do k = max( j-ka, i-kbt ), i - 1
                       ab( j-k+1, k ) = ab( j-k+1, k ) -bb( i-k+1, k )*ab( j-i+1, i )
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_dscal( n-m, one / bii, x( m+1, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_dger( n-m, kbt, -one, x( m+1, i ), 1_ilp,bb( kbt+1, i-kbt )&
                              , ldbb-1,x( m+1, i-kbt ), ldx )
                 end if
                 ! store a(i1,i) in ra1 for use in next loop over k
                 ra1 = ab( i1-i+1, i )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions down toward the bottom of the
              ! band
              loop_360: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i-k+ka<n .and. i-k>1_ilp ) then
                       ! generate rotation to annihilate a(i-k+ka+1,i)
                       call stdlib_dlartg( ab( ka1-k, i ), ra1, work( n+i-k+ka-m ),work( i-k+ka-m &
                                 ), ra )
                       ! create nonzero element a(i-k+ka+1,i-k) outside the
                       ! band and store it in work(i-k)
                       t = -bb( k+1, i-k )*ra1
                       work( i-k ) = work( n+i-k+ka-m )*t -work( i-k+ka-m )*ab( ka1, i-k )
                       ab( ka1, i-k ) = work( i-k+ka-m )*t +work( n+i-k+ka-m )*ab( ka1, i-k )
                                 
                       ra1 = ra
                    end if
                 end if
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( update ) then
                    j2t = max( j2, i+2*ka-k+1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( n-j2t+ka ) / ka1
                 do j = j2t, j1, ka1
                    ! create nonzero element a(j+1,j-ka) outside the band
                    ! and store it in work(j-m)
                    work( j-m ) = work( j-m )*ab( ka1, j-ka+1 )
                    ab( ka1, j-ka+1 ) = work( n+j-m )*ab( ka1, j-ka+1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_dlargv( nrt, ab( ka1, j2t-ka ), inca, work( j2t-m ),ka1, &
                           work( n+j2t-m ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the left
                    do l = 1, ka - 1
                       call stdlib_dlartv( nr, ab( l+1, j2-l ), inca,ab( l+2, j2-l ), inca, work( &
                                 n+j2-m ),work( j2-m ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_dlar2v( nr, ab( 1_ilp, j2 ), ab( 1_ilp, j2+1 ), ab( 2_ilp, j2 ),inca, work( n+&
                              j2-m ), work( j2-m ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( ka1-l+1, j2 ), inca,ab( ka1-l, j2+1 ),&
                               inca, work( n+j2-m ),work( j2-m ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j2, j1, ka1
                       call stdlib_drot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,work( n+j-m ), &
                                 work( j-m ) )
                    end do
                 end if
              end do loop_360
              if( update ) then
                 if( i2<=n .and. kbt>0_ilp ) then
                    ! create nonzero element a(i-kbt+ka+1,i-kbt) outside the
                    ! band and store it in work(i-kbt)
                    work( i-kbt ) = -bb( kbt+1, i-kbt )*ra1
                 end if
              end if
              loop_400: do k = kb, 1, -1
                 if( update ) then
                    j2 = i - k - 1_ilp + max( 2_ilp, k-i0+1 )*ka1
                 else
                    j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the right
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+ka+l ) / ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( ka1-l+1, j2-ka ), inca,ab( ka1-l, j2-&
                              ka+1 ), inca,work( n+j2-ka ), work( j2-ka ), ka1 )
                 end do
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 do j = j1, j2, -ka1
                    work( j ) = work( j-ka )
                    work( n+j ) = work( n+j-ka )
                 end do
                 do j = j2, j1, ka1
                    ! create nonzero element a(j+1,j-ka) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( ka1, j-ka+1 )
                    ab( ka1, j-ka+1 ) = work( n+j )*ab( ka1, j-ka+1 )
                 end do
                 if( update ) then
                    if( i-k<n-ka .and. k<=kbt )work( i-k+ka ) = work( i-k )
                 end if
              end do loop_400
              loop_440: do k = kb, 1, -1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_dlargv( nr, ab( ka1, j2-ka ), inca, work( j2 ), ka1,work( n+j2 ), &
                              ka1 )
                    ! apply rotations in 2nd set from the left
                    do l = 1, ka - 1
                       call stdlib_dlartv( nr, ab( l+1, j2-l ), inca,ab( l+2, j2-l ), inca, work( &
                                 n+j2 ),work( j2 ), ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_dlar2v( nr, ab( 1_ilp, j2 ), ab( 1_ilp, j2+1 ), ab( 2_ilp, j2 ),inca, work( n+&
                              j2 ), work( j2 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( ka1-l+1, j2 ), inca,ab( ka1-l, j2+1 ),&
                               inca, work( n+j2 ),work( j2 ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j2, j1, ka1
                       call stdlib_drot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,work( n+j ), work( &
                                 j ) )
                    end do
                 end if
              end do loop_440
              do k = 1, kb - 1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 ! finish applying rotations in 1st set from the right
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( ka1-l+1, j2 ), inca,ab( ka1-l, j2+1 ),&
                               inca, work( n+j2-m ),work( j2-m ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = n - 1, i - kb + 2*ka + 1, -1
                    work( n+j-m ) = work( n+j-ka-m )
                    work( j-m ) = work( j-ka-m )
                 end do
              end if
           end if
           go to 10
           480 continue
           ! **************************** phase 2 *****************************
           ! the logical structure of this phase is:
           ! update = .true.
           ! do i = 1, m
              ! use s(i) to update a and create a new bulge
              ! apply rotations to push all bulges ka positions upward
           ! end do
           ! update = .false.
           ! do i = m - ka - 1, 2, -1
              ! apply rotations to push all bulges ka positions upward
           ! end do
           ! to avoid duplicating code, the two loops are merged.
           update = .true.
           i = 0_ilp
           490 continue
           if( update ) then
              i = i + 1_ilp
              kbt = min( kb, m-i )
              i0 = i + 1_ilp
              i1 = max( 1_ilp, i-ka )
              i2 = i + kbt - ka1
              if( i>m ) then
                 update = .false.
                 i = i - 1_ilp
                 i0 = m + 1_ilp
                 if( ka==0 )return
                 go to 490
              end if
           else
              i = i - ka
              if( i<2 )return
           end if
           if( i<m-kbt ) then
              nx = m
           else
              nx = n
           end if
           if( upper ) then
              ! transform a, working with the upper triangle
              if( update ) then
                 ! form  inv(s(i))**t * a * inv(s(i))
                 bii = bb( kb1, i )
                 do j = i1, i
                    ab( j-i+ka1, i ) = ab( j-i+ka1, i ) / bii
                 end do
                 do j = i, min( n, i+ka )
                    ab( i-j+ka1, j ) = ab( i-j+ka1, j ) / bii
                 end do
                 do k = i + 1, i + kbt
                    do j = k, i + kbt
                       ab( k-j+ka1, j ) = ab( k-j+ka1, j ) -bb( i-j+kb1, j )*ab( i-k+ka1, k ) -bb(&
                        i-k+kb1, k )*ab( i-j+ka1, j ) +ab( ka1, i )*bb( i-j+kb1, j )*bb( i-k+kb1, &
                                  k )
                    end do
                    do j = i + kbt + 1, min( n, i+ka )
                       ab( k-j+ka1, j ) = ab( k-j+ka1, j ) -bb( i-k+kb1, k )*ab( i-j+ka1, j )
                                 
                    end do
                 end do
                 do j = i1, i
                    do k = i + 1, min( j+ka, i+kbt )
                       ab( j-k+ka1, k ) = ab( j-k+ka1, k ) -bb( i-k+kb1, k )*ab( j-i+ka1, i )
                                 
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_dscal( nx, one / bii, x( 1_ilp, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_dger( nx, kbt, -one, x( 1_ilp, i ), 1_ilp, bb( kb, i+1 ),ldbb-&
                              1_ilp, x( 1_ilp, i+1 ), ldx )
                 end if
                 ! store a(i1,i) in ra1 for use in next loop over k
                 ra1 = ab( i1-i+ka1, i )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions up toward the top of the band
              loop_610: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i+k-ka1>0_ilp .and. i+k<m ) then
                       ! generate rotation to annihilate a(i+k-ka-1,i)
                       call stdlib_dlartg( ab( k+1, i ), ra1, work( n+i+k-ka ),work( i+k-ka ), ra &
                                 )
                       ! create nonzero element a(i+k-ka-1,i+k) outside the
                       ! band and store it in work(m-kb+i+k)
                       t = -bb( kb1-k, i+k )*ra1
                       work( m-kb+i+k ) = work( n+i+k-ka )*t -work( i+k-ka )*ab( 1_ilp, i+k )
                       ab( 1_ilp, i+k ) = work( i+k-ka )*t +work( n+i+k-ka )*ab( 1_ilp, i+k )
                       ra1 = ra
                    end if
                 end if
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( update ) then
                    j2t = min( j2, i-2*ka+k-1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( j2t+ka-1 ) / ka1
                 do j = j1, j2t, ka1
                    ! create nonzero element a(j-1,j+ka) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( 1_ilp, j+ka-1 )
                    ab( 1_ilp, j+ka-1 ) = work( n+j )*ab( 1_ilp, j+ka-1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_dlargv( nrt, ab( 1_ilp, j1+ka ), inca, work( j1 ), ka1,work( &
                           n+j1 ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the left
                    do l = 1, ka - 1
                       call stdlib_dlartv( nr, ab( ka1-l, j1+l ), inca,ab( ka-l, j1+l ), inca, &
                                 work( n+j1 ),work( j1 ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_dlar2v( nr, ab( ka1, j1 ), ab( ka1, j1-1 ),ab( ka, j1 ), inca, &
                              work( n+j1 ),work( j1 ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( l, j1t ), inca,ab( l+1, j1t-1 ), inca,&
                               work( n+j1t ),work( j1t ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j1, j2, ka1
                       call stdlib_drot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,work( n+j ), work( j ) )
                                 
                    end do
                 end if
              end do loop_610
              if( update ) then
                 if( i2>0_ilp .and. kbt>0_ilp ) then
                    ! create nonzero element a(i+kbt-ka-1,i+kbt) outside the
                    ! band and store it in work(m-kb+i+kbt)
                    work( m-kb+i+kbt ) = -bb( kb1-kbt, i+kbt )*ra1
                 end if
              end if
              loop_650: do k = kb, 1, -1
                 if( update ) then
                    j2 = i + k + 1_ilp - max( 2_ilp, k+i0-m )*ka1
                 else
                    j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the right
                 do l = kb - k, 1, -1
                    nrt = ( j2+ka+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( l, j1t+ka ), inca,ab( l+1, j1t+ka-1 ),&
                               inca,work( n+m-kb+j1t+ka ),work( m-kb+j1t+ka ), ka1 )
                 end do
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 do j = j1, j2, ka1
                    work( m-kb+j ) = work( m-kb+j+ka )
                    work( n+m-kb+j ) = work( n+m-kb+j+ka )
                 end do
                 do j = j1, j2, ka1
                    ! create nonzero element a(j-1,j+ka) outside the band
                    ! and store it in work(m-kb+j)
                    work( m-kb+j ) = work( m-kb+j )*ab( 1_ilp, j+ka-1 )
                    ab( 1_ilp, j+ka-1 ) = work( n+m-kb+j )*ab( 1_ilp, j+ka-1 )
                 end do
                 if( update ) then
                    if( i+k>ka1 .and. k<=kbt )work( m-kb+i+k-ka ) = work( m-kb+i+k )
                 end if
              end do loop_650
              loop_690: do k = kb, 1, -1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_dlargv( nr, ab( 1_ilp, j1+ka ), inca, work( m-kb+j1 ),ka1, work( n+m-&
                              kb+j1 ), ka1 )
                    ! apply rotations in 2nd set from the left
                    do l = 1, ka - 1
                       call stdlib_dlartv( nr, ab( ka1-l, j1+l ), inca,ab( ka-l, j1+l ), inca,&
                                 work( n+m-kb+j1 ), work( m-kb+j1 ), ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_dlar2v( nr, ab( ka1, j1 ), ab( ka1, j1-1 ),ab( ka, j1 ), inca, &
                              work( n+m-kb+j1 ),work( m-kb+j1 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( l, j1t ), inca,ab( l+1, j1t-1 ), inca,&
                              work( n+m-kb+j1t ), work( m-kb+j1t ),ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j1, j2, ka1
                       call stdlib_drot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,work( n+m-kb+j ), work( &
                                 m-kb+j ) )
                    end do
                 end if
              end do loop_690
              do k = 1, kb - 1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 ! finish applying rotations in 1st set from the right
                 do l = kb - k, 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( l, j1t ), inca,ab( l+1, j1t-1 ), inca,&
                               work( n+j1t ),work( j1t ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = 2, min( i+kb, m ) - 2*ka - 1
                    work( n+j ) = work( n+j+ka )
                    work( j ) = work( j+ka )
                 end do
              end if
           else
              ! transform a, working with the lower triangle
              if( update ) then
                 ! form  inv(s(i))**t * a * inv(s(i))
                 bii = bb( 1_ilp, i )
                 do j = i1, i
                    ab( i-j+1, j ) = ab( i-j+1, j ) / bii
                 end do
                 do j = i, min( n, i+ka )
                    ab( j-i+1, i ) = ab( j-i+1, i ) / bii
                 end do
                 do k = i + 1, i + kbt
                    do j = k, i + kbt
                       ab( j-k+1, k ) = ab( j-k+1, k ) -bb( j-i+1, i )*ab( k-i+1, i ) -bb( k-i+1, &
                                 i )*ab( j-i+1, i ) +ab( 1_ilp, i )*bb( j-i+1, i )*bb( k-i+1, i )
                    end do
                    do j = i + kbt + 1, min( n, i+ka )
                       ab( j-k+1, k ) = ab( j-k+1, k ) -bb( k-i+1, i )*ab( j-i+1, i )
                    end do
                 end do
                 do j = i1, i
                    do k = i + 1, min( j+ka, i+kbt )
                       ab( k-j+1, j ) = ab( k-j+1, j ) -bb( k-i+1, i )*ab( i-j+1, j )
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_dscal( nx, one / bii, x( 1_ilp, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_dger( nx, kbt, -one, x( 1_ilp, i ), 1_ilp, bb( 2_ilp, i ), 1_ilp,x( 1_ilp, &
                              i+1 ), ldx )
                 end if
                 ! store a(i,i1) in ra1 for use in next loop over k
                 ra1 = ab( i-i1+1, i1 )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions up toward the top of the band
              loop_840: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i+k-ka1>0_ilp .and. i+k<m ) then
                       ! generate rotation to annihilate a(i,i+k-ka-1)
                       call stdlib_dlartg( ab( ka1-k, i+k-ka ), ra1,work( n+i+k-ka ), work( i+k-&
                                 ka ), ra )
                       ! create nonzero element a(i+k,i+k-ka-1) outside the
                       ! band and store it in work(m-kb+i+k)
                       t = -bb( k+1, i )*ra1
                       work( m-kb+i+k ) = work( n+i+k-ka )*t -work( i+k-ka )*ab( ka1, i+k-ka )
                                 
                       ab( ka1, i+k-ka ) = work( i+k-ka )*t +work( n+i+k-ka )*ab( ka1, i+k-ka )
                                 
                       ra1 = ra
                    end if
                 end if
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( update ) then
                    j2t = min( j2, i-2*ka+k-1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( j2t+ka-1 ) / ka1
                 do j = j1, j2t, ka1
                    ! create nonzero element a(j+ka,j-1) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( ka1, j-1 )
                    ab( ka1, j-1 ) = work( n+j )*ab( ka1, j-1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_dlargv( nrt, ab( ka1, j1 ), inca, work( j1 ), ka1,work( n+&
                           j1 ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the right
                    do l = 1, ka - 1
                       call stdlib_dlartv( nr, ab( l+1, j1 ), inca, ab( l+2, j1-1 ),inca, work( n+&
                                 j1 ), work( j1 ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_dlar2v( nr, ab( 1_ilp, j1 ), ab( 1_ilp, j1-1 ),ab( 2_ilp, j1-1 ), inca, work( &
                              n+j1 ),work( j1 ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( ka1-l+1, j1t-ka1+l ), inca,ab( ka1-l, &
                              j1t-ka1+l ), inca,work( n+j1t ), work( j1t ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j1, j2, ka1
                       call stdlib_drot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,work( n+j ), work( j ) )
                                 
                    end do
                 end if
              end do loop_840
              if( update ) then
                 if( i2>0_ilp .and. kbt>0_ilp ) then
                    ! create nonzero element a(i+kbt,i+kbt-ka-1) outside the
                    ! band and store it in work(m-kb+i+kbt)
                    work( m-kb+i+kbt ) = -bb( kbt+1, i )*ra1
                 end if
              end if
              loop_880: do k = kb, 1, -1
                 if( update ) then
                    j2 = i + k + 1_ilp - max( 2_ilp, k+i0-m )*ka1
                 else
                    j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the left
                 do l = kb - k, 1, -1
                    nrt = ( j2+ka+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( ka1-l+1, j1t+l-1 ), inca,ab( ka1-l, &
                              j1t+l-1 ), inca,work( n+m-kb+j1t+ka ),work( m-kb+j1t+ka ), ka1 )
                 end do
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 do j = j1, j2, ka1
                    work( m-kb+j ) = work( m-kb+j+ka )
                    work( n+m-kb+j ) = work( n+m-kb+j+ka )
                 end do
                 do j = j1, j2, ka1
                    ! create nonzero element a(j+ka,j-1) outside the band
                    ! and store it in work(m-kb+j)
                    work( m-kb+j ) = work( m-kb+j )*ab( ka1, j-1 )
                    ab( ka1, j-1 ) = work( n+m-kb+j )*ab( ka1, j-1 )
                 end do
                 if( update ) then
                    if( i+k>ka1 .and. k<=kbt )work( m-kb+i+k-ka ) = work( m-kb+i+k )
                 end if
              end do loop_880
              loop_920: do k = kb, 1, -1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_dlargv( nr, ab( ka1, j1 ), inca, work( m-kb+j1 ),ka1, work( n+m-&
                              kb+j1 ), ka1 )
                    ! apply rotations in 2nd set from the right
                    do l = 1, ka - 1
                       call stdlib_dlartv( nr, ab( l+1, j1 ), inca, ab( l+2, j1-1 ),inca, work( n+&
                                 m-kb+j1 ), work( m-kb+j1 ),ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_dlar2v( nr, ab( 1_ilp, j1 ), ab( 1_ilp, j1-1 ),ab( 2_ilp, j1-1 ), inca, work( &
                              n+m-kb+j1 ),work( m-kb+j1 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( ka1-l+1, j1t-ka1+l ), inca,ab( ka1-l, &
                              j1t-ka1+l ), inca,work( n+m-kb+j1t ), work( m-kb+j1t ),ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j1, j2, ka1
                       call stdlib_drot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,work( n+m-kb+j ), work( &
                                 m-kb+j ) )
                    end do
                 end if
              end do loop_920
              do k = 1, kb - 1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 ! finish applying rotations in 1st set from the left
                 do l = kb - k, 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( ka1-l+1, j1t-ka1+l ), inca,ab( ka1-l, &
                              j1t-ka1+l ), inca,work( n+j1t ), work( j1t ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = 2, min( i+kb, m ) - 2*ka - 1
                    work( n+j ) = work( n+j+ka )
                    work( j ) = work( j+ka )
                 end do
              end if
           end if
           go to 490
     end subroutine stdlib_dsbgst




     pure module subroutine stdlib_chegst( itype, uplo, n, a, lda, b, ldb, info )
     !! CHEGST reduces a complex Hermitian-definite generalized
     !! eigenproblem to standard form.
     !! If ITYPE = 1, the problem is A*x = lambda*B*x,
     !! and A is overwritten by inv(U**H)*A*inv(U) or inv(L)*A*inv(L**H)
     !! If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
     !! B*A*x = lambda*x, and A is overwritten by U*A*U**H or L**H*A*L.
     !! B must have been previously factorized as U**H*U or L*L**H by CPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k, kb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHEGST', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'CHEGST', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_chegs2( itype, uplo, n, a, lda, b, ldb, info )
           else
              ! use blocked code
              if( itype==1_ilp ) then
                 if( upper ) then
                    ! compute inv(u**h)*a*inv(u)
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the upper triangle of a(k:n,k:n)
                       call stdlib_chegs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                       if( k+kb<=n ) then
                          call stdlib_ctrsm( 'LEFT', uplo, 'CONJUGATE TRANSPOSE','NON-UNIT', kb, &
                                    n-k-kb+1, cone,b( k, k ), ldb, a( k, k+kb ), lda )
                          call stdlib_chemm( 'LEFT', uplo, kb, n-k-kb+1, -chalf,a( k, k ), lda, b(&
                                     k, k+kb ), ldb,cone, a( k, k+kb ), lda )
                          call stdlib_cher2k( uplo, 'CONJUGATE TRANSPOSE', n-k-kb+1,kb, -cone, a( &
                                    k, k+kb ), lda,b( k, k+kb ), ldb, one,a( k+kb, k+kb ), lda )
                          call stdlib_chemm( 'LEFT', uplo, kb, n-k-kb+1, -chalf,a( k, k ), lda, b(&
                                     k, k+kb ), ldb,cone, a( k, k+kb ), lda )
                          call stdlib_ctrsm( 'RIGHT', uplo, 'NO TRANSPOSE','NON-UNIT', kb, n-k-kb+&
                                    1_ilp, cone,b( k+kb, k+kb ), ldb, a( k, k+kb ),lda )
                       end if
                    end do
                 else
                    ! compute inv(l)*a*inv(l**h)
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the lower triangle of a(k:n,k:n)
                       call stdlib_chegs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                       if( k+kb<=n ) then
                          call stdlib_ctrsm( 'RIGHT', uplo, 'CONJUGATE TRANSPOSE','NON-UNIT', n-k-&
                                    kb+1, kb, cone,b( k, k ), ldb, a( k+kb, k ), lda )
                          call stdlib_chemm( 'RIGHT', uplo, n-k-kb+1, kb, -chalf,a( k, k ), lda, &
                                    b( k+kb, k ), ldb,cone, a( k+kb, k ), lda )
                          call stdlib_cher2k( uplo, 'NO TRANSPOSE', n-k-kb+1, kb,-cone, a( k+kb, &
                                    k ), lda,b( k+kb, k ), ldb, one,a( k+kb, k+kb ), lda )
                          call stdlib_chemm( 'RIGHT', uplo, n-k-kb+1, kb, -chalf,a( k, k ), lda, &
                                    b( k+kb, k ), ldb,cone, a( k+kb, k ), lda )
                          call stdlib_ctrsm( 'LEFT', uplo, 'NO TRANSPOSE','NON-UNIT', n-k-kb+1, &
                                    kb, cone,b( k+kb, k+kb ), ldb, a( k+kb, k ),lda )
                       end if
                    end do
                 end if
              else
                 if( upper ) then
                    ! compute u*a*u**h
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the upper triangle of a(1:k+kb-1,1:k+kb-1)
                       call stdlib_ctrmm( 'LEFT', uplo, 'NO TRANSPOSE', 'NON-UNIT',k-1, kb, cone, &
                                 b, ldb, a( 1_ilp, k ), lda )
                       call stdlib_chemm( 'RIGHT', uplo, k-1, kb, chalf, a( k, k ),lda, b( 1_ilp, k ),&
                                  ldb, cone, a( 1_ilp, k ),lda )
                       call stdlib_cher2k( uplo, 'NO TRANSPOSE', k-1, kb, cone,a( 1_ilp, k ), lda, b( &
                                 1_ilp, k ), ldb, one, a,lda )
                       call stdlib_chemm( 'RIGHT', uplo, k-1, kb, chalf, a( k, k ),lda, b( 1_ilp, k ),&
                                  ldb, cone, a( 1_ilp, k ),lda )
                       call stdlib_ctrmm( 'RIGHT', uplo, 'CONJUGATE TRANSPOSE','NON-UNIT', k-1, &
                                 kb, cone, b( k, k ), ldb,a( 1_ilp, k ), lda )
                       call stdlib_chegs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                    end do
                 else
                    ! compute l**h*a*l
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the lower triangle of a(1:k+kb-1,1:k+kb-1)
                       call stdlib_ctrmm( 'RIGHT', uplo, 'NO TRANSPOSE', 'NON-UNIT',kb, k-1, cone,&
                                  b, ldb, a( k, 1_ilp ), lda )
                       call stdlib_chemm( 'LEFT', uplo, kb, k-1, chalf, a( k, k ),lda, b( k, 1_ilp ), &
                                 ldb, cone, a( k, 1_ilp ),lda )
                       call stdlib_cher2k( uplo, 'CONJUGATE TRANSPOSE', k-1, kb,cone, a( k, 1_ilp ), &
                                 lda, b( k, 1_ilp ), ldb,one, a, lda )
                       call stdlib_chemm( 'LEFT', uplo, kb, k-1, chalf, a( k, k ),lda, b( k, 1_ilp ), &
                                 ldb, cone, a( k, 1_ilp ),lda )
                       call stdlib_ctrmm( 'LEFT', uplo, 'CONJUGATE TRANSPOSE','NON-UNIT', kb, k-1,&
                                  cone, b( k, k ), ldb,a( k, 1_ilp ), lda )
                       call stdlib_chegs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_chegst

     pure module subroutine stdlib_zhegst( itype, uplo, n, a, lda, b, ldb, info )
     !! ZHEGST reduces a complex Hermitian-definite generalized
     !! eigenproblem to standard form.
     !! If ITYPE = 1, the problem is A*x = lambda*B*x,
     !! and A is overwritten by inv(U**H)*A*inv(U) or inv(L)*A*inv(L**H)
     !! If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
     !! B*A*x = lambda*x, and A is overwritten by U*A*U**H or L**H*A*L.
     !! B must have been previously factorized as U**H*U or L*L**H by ZPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k, kb, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHEGST', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! determine the block size for this environment.
           nb = stdlib_ilaenv( 1_ilp, 'ZHEGST', uplo, n, -1_ilp, -1_ilp, -1_ilp )
           if( nb<=1_ilp .or. nb>=n ) then
              ! use unblocked code
              call stdlib_zhegs2( itype, uplo, n, a, lda, b, ldb, info )
           else
              ! use blocked code
              if( itype==1_ilp ) then
                 if( upper ) then
                    ! compute inv(u**h)*a*inv(u)
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the upper triangle of a(k:n,k:n)
                       call stdlib_zhegs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                       if( k+kb<=n ) then
                          call stdlib_ztrsm( 'LEFT', uplo, 'CONJUGATE TRANSPOSE','NON-UNIT', kb, &
                                    n-k-kb+1, cone,b( k, k ), ldb, a( k, k+kb ), lda )
                          call stdlib_zhemm( 'LEFT', uplo, kb, n-k-kb+1, -chalf,a( k, k ), lda, b(&
                                     k, k+kb ), ldb,cone, a( k, k+kb ), lda )
                          call stdlib_zher2k( uplo, 'CONJUGATE TRANSPOSE', n-k-kb+1,kb, -cone, a( &
                                    k, k+kb ), lda,b( k, k+kb ), ldb, one,a( k+kb, k+kb ), lda )
                          call stdlib_zhemm( 'LEFT', uplo, kb, n-k-kb+1, -chalf,a( k, k ), lda, b(&
                                     k, k+kb ), ldb,cone, a( k, k+kb ), lda )
                          call stdlib_ztrsm( 'RIGHT', uplo, 'NO TRANSPOSE','NON-UNIT', kb, n-k-kb+&
                                    1_ilp, cone,b( k+kb, k+kb ), ldb, a( k, k+kb ),lda )
                       end if
                    end do
                 else
                    ! compute inv(l)*a*inv(l**h)
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the lower triangle of a(k:n,k:n)
                       call stdlib_zhegs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                       if( k+kb<=n ) then
                          call stdlib_ztrsm( 'RIGHT', uplo, 'CONJUGATE TRANSPOSE','NON-UNIT', n-k-&
                                    kb+1, kb, cone,b( k, k ), ldb, a( k+kb, k ), lda )
                          call stdlib_zhemm( 'RIGHT', uplo, n-k-kb+1, kb, -chalf,a( k, k ), lda, &
                                    b( k+kb, k ), ldb,cone, a( k+kb, k ), lda )
                          call stdlib_zher2k( uplo, 'NO TRANSPOSE', n-k-kb+1, kb,-cone, a( k+kb, &
                                    k ), lda,b( k+kb, k ), ldb, one,a( k+kb, k+kb ), lda )
                          call stdlib_zhemm( 'RIGHT', uplo, n-k-kb+1, kb, -chalf,a( k, k ), lda, &
                                    b( k+kb, k ), ldb,cone, a( k+kb, k ), lda )
                          call stdlib_ztrsm( 'LEFT', uplo, 'NO TRANSPOSE','NON-UNIT', n-k-kb+1, &
                                    kb, cone,b( k+kb, k+kb ), ldb, a( k+kb, k ),lda )
                       end if
                    end do
                 end if
              else
                 if( upper ) then
                    ! compute u*a*u**h
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the upper triangle of a(1:k+kb-1,1:k+kb-1)
                       call stdlib_ztrmm( 'LEFT', uplo, 'NO TRANSPOSE', 'NON-UNIT',k-1, kb, cone, &
                                 b, ldb, a( 1_ilp, k ), lda )
                       call stdlib_zhemm( 'RIGHT', uplo, k-1, kb, chalf, a( k, k ),lda, b( 1_ilp, k ),&
                                  ldb, cone, a( 1_ilp, k ),lda )
                       call stdlib_zher2k( uplo, 'NO TRANSPOSE', k-1, kb, cone,a( 1_ilp, k ), lda, b( &
                                 1_ilp, k ), ldb, one, a,lda )
                       call stdlib_zhemm( 'RIGHT', uplo, k-1, kb, chalf, a( k, k ),lda, b( 1_ilp, k ),&
                                  ldb, cone, a( 1_ilp, k ),lda )
                       call stdlib_ztrmm( 'RIGHT', uplo, 'CONJUGATE TRANSPOSE','NON-UNIT', k-1, &
                                 kb, cone, b( k, k ), ldb,a( 1_ilp, k ), lda )
                       call stdlib_zhegs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                    end do
                 else
                    ! compute l**h*a*l
                    do k = 1, n, nb
                       kb = min( n-k+1, nb )
                       ! update the lower triangle of a(1:k+kb-1,1:k+kb-1)
                       call stdlib_ztrmm( 'RIGHT', uplo, 'NO TRANSPOSE', 'NON-UNIT',kb, k-1, cone,&
                                  b, ldb, a( k, 1_ilp ), lda )
                       call stdlib_zhemm( 'LEFT', uplo, kb, k-1, chalf, a( k, k ),lda, b( k, 1_ilp ), &
                                 ldb, cone, a( k, 1_ilp ),lda )
                       call stdlib_zher2k( uplo, 'CONJUGATE TRANSPOSE', k-1, kb,cone, a( k, 1_ilp ), &
                                 lda, b( k, 1_ilp ), ldb,one, a, lda )
                       call stdlib_zhemm( 'LEFT', uplo, kb, k-1, chalf, a( k, k ),lda, b( k, 1_ilp ), &
                                 ldb, cone, a( k, 1_ilp ),lda )
                       call stdlib_ztrmm( 'LEFT', uplo, 'CONJUGATE TRANSPOSE','NON-UNIT', kb, k-1,&
                                  cone, b( k, k ), ldb,a( k, 1_ilp ), lda )
                       call stdlib_zhegs2( itype, uplo, kb, a( k, k ), lda,b( k, k ), ldb, info )
                                 
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_zhegst




     pure module subroutine stdlib_chegs2( itype, uplo, n, a, lda, b, ldb, info )
     !! CHEGS2 reduces a complex Hermitian-definite generalized
     !! eigenproblem to standard form.
     !! If ITYPE = 1, the problem is A*x = lambda*B*x,
     !! and A is overwritten by inv(U**H)*A*inv(U) or inv(L)*A*inv(L**H)
     !! If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
     !! B*A*x = lambda*x, and A is overwritten by U*A*U**H or L**H *A*L.
     !! B must have been previously factorized as U**H *U or L*L**H by ZPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k
           real(sp) :: akk, bkk
           complex(sp) :: ct
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHEGS2', -info )
              return
           end if
           if( itype==1_ilp ) then
              if( upper ) then
                 ! compute inv(u**h)*a*inv(u)
                 do k = 1, n
                    ! update the upper triangle of a(k:n,k:n)
                    akk = real( a( k, k ),KIND=sp)
                    bkk = real( b( k, k ),KIND=sp)
                    akk = akk / bkk**2_ilp
                    a( k, k ) = akk
                    if( k<n ) then
                       call stdlib_csscal( n-k, one / bkk, a( k, k+1 ), lda )
                       ct = -half*akk
                       call stdlib_clacgv( n-k, a( k, k+1 ), lda )
                       call stdlib_clacgv( n-k, b( k, k+1 ), ldb )
                       call stdlib_caxpy( n-k, ct, b( k, k+1 ), ldb, a( k, k+1 ),lda )
                       call stdlib_cher2( uplo, n-k, -cone, a( k, k+1 ), lda,b( k, k+1 ), ldb, a( &
                                 k+1, k+1 ), lda )
                       call stdlib_caxpy( n-k, ct, b( k, k+1 ), ldb, a( k, k+1 ),lda )
                       call stdlib_clacgv( n-k, b( k, k+1 ), ldb )
                       call stdlib_ctrsv( uplo, 'CONJUGATE TRANSPOSE', 'NON-UNIT',n-k, b( k+1, k+&
                                 1_ilp ), ldb, a( k, k+1 ),lda )
                       call stdlib_clacgv( n-k, a( k, k+1 ), lda )
                    end if
                 end do
              else
                 ! compute inv(l)*a*inv(l**h)
                 do k = 1, n
                    ! update the lower triangle of a(k:n,k:n)
                    akk = real( a( k, k ),KIND=sp)
                    bkk = real( b( k, k ),KIND=sp)
                    akk = akk / bkk**2_ilp
                    a( k, k ) = akk
                    if( k<n ) then
                       call stdlib_csscal( n-k, one / bkk, a( k+1, k ), 1_ilp )
                       ct = -half*akk
                       call stdlib_caxpy( n-k, ct, b( k+1, k ), 1_ilp, a( k+1, k ), 1_ilp )
                       call stdlib_cher2( uplo, n-k, -cone, a( k+1, k ), 1_ilp,b( k+1, k ), 1_ilp, a( k+1,&
                                  k+1 ), lda )
                       call stdlib_caxpy( n-k, ct, b( k+1, k ), 1_ilp, a( k+1, k ), 1_ilp )
                       call stdlib_ctrsv( uplo, 'NO TRANSPOSE', 'NON-UNIT', n-k,b( k+1, k+1 ), &
                                 ldb, a( k+1, k ), 1_ilp )
                    end if
                 end do
              end if
           else
              if( upper ) then
                 ! compute u*a*u**h
                 do k = 1, n
                    ! update the upper triangle of a(1:k,1:k)
                    akk = real( a( k, k ),KIND=sp)
                    bkk = real( b( k, k ),KIND=sp)
                    call stdlib_ctrmv( uplo, 'NO TRANSPOSE', 'NON-UNIT', k-1, b,ldb, a( 1_ilp, k ), 1_ilp &
                              )
                    ct = half*akk
                    call stdlib_caxpy( k-1, ct, b( 1_ilp, k ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    call stdlib_cher2( uplo, k-1, cone, a( 1_ilp, k ), 1_ilp, b( 1_ilp, k ), 1_ilp,a, lda )
                              
                    call stdlib_caxpy( k-1, ct, b( 1_ilp, k ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    call stdlib_csscal( k-1, bkk, a( 1_ilp, k ), 1_ilp )
                    a( k, k ) = akk*bkk**2_ilp
                 end do
              else
                 ! compute l**h *a*l
                 do k = 1, n
                    ! update the lower triangle of a(1:k,1:k)
                    akk = real( a( k, k ),KIND=sp)
                    bkk = real( b( k, k ),KIND=sp)
                    call stdlib_clacgv( k-1, a( k, 1_ilp ), lda )
                    call stdlib_ctrmv( uplo, 'CONJUGATE TRANSPOSE', 'NON-UNIT', k-1,b, ldb, a( k, &
                              1_ilp ), lda )
                    ct = half*akk
                    call stdlib_clacgv( k-1, b( k, 1_ilp ), ldb )
                    call stdlib_caxpy( k-1, ct, b( k, 1_ilp ), ldb, a( k, 1_ilp ), lda )
                    call stdlib_cher2( uplo, k-1, cone, a( k, 1_ilp ), lda, b( k, 1_ilp ),ldb, a, lda )
                              
                    call stdlib_caxpy( k-1, ct, b( k, 1_ilp ), ldb, a( k, 1_ilp ), lda )
                    call stdlib_clacgv( k-1, b( k, 1_ilp ), ldb )
                    call stdlib_csscal( k-1, bkk, a( k, 1_ilp ), lda )
                    call stdlib_clacgv( k-1, a( k, 1_ilp ), lda )
                    a( k, k ) = akk*bkk**2_ilp
                 end do
              end if
           end if
           return
     end subroutine stdlib_chegs2

     pure module subroutine stdlib_zhegs2( itype, uplo, n, a, lda, b, ldb, info )
     !! ZHEGS2 reduces a complex Hermitian-definite generalized
     !! eigenproblem to standard form.
     !! If ITYPE = 1, the problem is A*x = lambda*B*x,
     !! and A is overwritten by inv(U**H)*A*inv(U) or inv(L)*A*inv(L**H)
     !! If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
     !! B*A*x = lambda*x, and A is overwritten by U*A*U**H or L**H *A*L.
     !! B must have been previously factorized as U**H *U or L*L**H by ZPOTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: k
           real(dp) :: akk, bkk
           complex(dp) :: ct
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHEGS2', -info )
              return
           end if
           if( itype==1_ilp ) then
              if( upper ) then
                 ! compute inv(u**h)*a*inv(u)
                 do k = 1, n
                    ! update the upper triangle of a(k:n,k:n)
                    akk = real( a( k, k ),KIND=dp)
                    bkk = real( b( k, k ),KIND=dp)
                    akk = akk / bkk**2_ilp
                    a( k, k ) = akk
                    if( k<n ) then
                       call stdlib_zdscal( n-k, one / bkk, a( k, k+1 ), lda )
                       ct = -half*akk
                       call stdlib_zlacgv( n-k, a( k, k+1 ), lda )
                       call stdlib_zlacgv( n-k, b( k, k+1 ), ldb )
                       call stdlib_zaxpy( n-k, ct, b( k, k+1 ), ldb, a( k, k+1 ),lda )
                       call stdlib_zher2( uplo, n-k, -cone, a( k, k+1 ), lda,b( k, k+1 ), ldb, a( &
                                 k+1, k+1 ), lda )
                       call stdlib_zaxpy( n-k, ct, b( k, k+1 ), ldb, a( k, k+1 ),lda )
                       call stdlib_zlacgv( n-k, b( k, k+1 ), ldb )
                       call stdlib_ztrsv( uplo, 'CONJUGATE TRANSPOSE', 'NON-UNIT',n-k, b( k+1, k+&
                                 1_ilp ), ldb, a( k, k+1 ),lda )
                       call stdlib_zlacgv( n-k, a( k, k+1 ), lda )
                    end if
                 end do
              else
                 ! compute inv(l)*a*inv(l**h)
                 do k = 1, n
                    ! update the lower triangle of a(k:n,k:n)
                    akk = real( a( k, k ),KIND=dp)
                    bkk = real( b( k, k ),KIND=dp)
                    akk = akk / bkk**2_ilp
                    a( k, k ) = akk
                    if( k<n ) then
                       call stdlib_zdscal( n-k, one / bkk, a( k+1, k ), 1_ilp )
                       ct = -half*akk
                       call stdlib_zaxpy( n-k, ct, b( k+1, k ), 1_ilp, a( k+1, k ), 1_ilp )
                       call stdlib_zher2( uplo, n-k, -cone, a( k+1, k ), 1_ilp,b( k+1, k ), 1_ilp, a( k+1,&
                                  k+1 ), lda )
                       call stdlib_zaxpy( n-k, ct, b( k+1, k ), 1_ilp, a( k+1, k ), 1_ilp )
                       call stdlib_ztrsv( uplo, 'NO TRANSPOSE', 'NON-UNIT', n-k,b( k+1, k+1 ), &
                                 ldb, a( k+1, k ), 1_ilp )
                    end if
                 end do
              end if
           else
              if( upper ) then
                 ! compute u*a*u**h
                 do k = 1, n
                    ! update the upper triangle of a(1:k,1:k)
                    akk = real( a( k, k ),KIND=dp)
                    bkk = real( b( k, k ),KIND=dp)
                    call stdlib_ztrmv( uplo, 'NO TRANSPOSE', 'NON-UNIT', k-1, b,ldb, a( 1_ilp, k ), 1_ilp &
                              )
                    ct = half*akk
                    call stdlib_zaxpy( k-1, ct, b( 1_ilp, k ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    call stdlib_zher2( uplo, k-1, cone, a( 1_ilp, k ), 1_ilp, b( 1_ilp, k ), 1_ilp,a, lda )
                              
                    call stdlib_zaxpy( k-1, ct, b( 1_ilp, k ), 1_ilp, a( 1_ilp, k ), 1_ilp )
                    call stdlib_zdscal( k-1, bkk, a( 1_ilp, k ), 1_ilp )
                    a( k, k ) = akk*bkk**2_ilp
                 end do
              else
                 ! compute l**h *a*l
                 do k = 1, n
                    ! update the lower triangle of a(1:k,1:k)
                    akk = real( a( k, k ),KIND=dp)
                    bkk = real( b( k, k ),KIND=dp)
                    call stdlib_zlacgv( k-1, a( k, 1_ilp ), lda )
                    call stdlib_ztrmv( uplo, 'CONJUGATE TRANSPOSE', 'NON-UNIT', k-1,b, ldb, a( k, &
                              1_ilp ), lda )
                    ct = half*akk
                    call stdlib_zlacgv( k-1, b( k, 1_ilp ), ldb )
                    call stdlib_zaxpy( k-1, ct, b( k, 1_ilp ), ldb, a( k, 1_ilp ), lda )
                    call stdlib_zher2( uplo, k-1, cone, a( k, 1_ilp ), lda, b( k, 1_ilp ),ldb, a, lda )
                              
                    call stdlib_zaxpy( k-1, ct, b( k, 1_ilp ), ldb, a( k, 1_ilp ), lda )
                    call stdlib_zlacgv( k-1, b( k, 1_ilp ), ldb )
                    call stdlib_zdscal( k-1, bkk, a( k, 1_ilp ), lda )
                    call stdlib_zlacgv( k-1, a( k, 1_ilp ), lda )
                    a( k, k ) = akk*bkk**2_ilp
                 end do
              end if
           end if
           return
     end subroutine stdlib_zhegs2




     pure module subroutine stdlib_chpgst( itype, uplo, n, ap, bp, info )
     !! CHPGST reduces a complex Hermitian-definite generalized
     !! eigenproblem to standard form, using packed storage.
     !! If ITYPE = 1, the problem is A*x = lambda*B*x,
     !! and A is overwritten by inv(U**H)*A*inv(U) or inv(L)*A*inv(L**H)
     !! If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
     !! B*A*x = lambda*x, and A is overwritten by U*A*U**H or L**H*A*L.
     !! B must have been previously factorized as U**H*U or L*L**H by CPPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, n
           ! Array Arguments 
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(in) :: bp(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, j1, j1j1, jj, k, k1, k1k1, kk
           real(sp) :: ajj, akk, bjj, bkk
           complex(sp) :: ct
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHPGST', -info )
              return
           end if
           if( itype==1_ilp ) then
              if( upper ) then
                 ! compute inv(u**h)*a*inv(u)
                 ! j1 and jj are the indices of a(1,j) and a(j,j)
                 jj = 0_ilp
                 do j = 1, n
                    j1 = jj + 1_ilp
                    jj = jj + j
                    ! compute the j-th column of the upper triangle of a
                    ap( jj ) = real( ap( jj ),KIND=sp)
                    bjj = real( bp( jj ),KIND=sp)
                    call stdlib_ctpsv( uplo, 'CONJUGATE TRANSPOSE', 'NON-UNIT', j,bp, ap( j1 ), 1_ilp &
                              )
                    call stdlib_chpmv( uplo, j-1, -cone, ap, bp( j1 ), 1_ilp, cone,ap( j1 ), 1_ilp )
                              
                    call stdlib_csscal( j-1, one / bjj, ap( j1 ), 1_ilp )
                    ap( jj ) = ( ap( jj )-stdlib_cdotc( j-1, ap( j1 ), 1_ilp, bp( j1 ),1_ilp ) ) / &
                              bjj
                 end do
              else
                 ! compute inv(l)*a*inv(l**h)
                 ! kk and k1k1 are the indices of a(k,k) and a(k+1,k+1)
                 kk = 1_ilp
                 do k = 1, n
                    k1k1 = kk + n - k + 1_ilp
                    ! update the lower triangle of a(k:n,k:n)
                    akk = real( ap( kk ),KIND=sp)
                    bkk = real( bp( kk ),KIND=sp)
                    akk = akk / bkk**2_ilp
                    ap( kk ) = akk
                    if( k<n ) then
                       call stdlib_csscal( n-k, one / bkk, ap( kk+1 ), 1_ilp )
                       ct = -half*akk
                       call stdlib_caxpy( n-k, ct, bp( kk+1 ), 1_ilp, ap( kk+1 ), 1_ilp )
                       call stdlib_chpr2( uplo, n-k, -cone, ap( kk+1 ), 1_ilp,bp( kk+1 ), 1_ilp, ap( k1k1 &
                                 ) )
                       call stdlib_caxpy( n-k, ct, bp( kk+1 ), 1_ilp, ap( kk+1 ), 1_ilp )
                       call stdlib_ctpsv( uplo, 'NO TRANSPOSE', 'NON-UNIT', n-k,bp( k1k1 ), ap( &
                                 kk+1 ), 1_ilp )
                    end if
                    kk = k1k1
                 end do
              end if
           else
              if( upper ) then
                 ! compute u*a*u**h
                 ! k1 and kk are the indices of a(1,k) and a(k,k)
                 kk = 0_ilp
                 do k = 1, n
                    k1 = kk + 1_ilp
                    kk = kk + k
                    ! update the upper triangle of a(1:k,1:k)
                    akk = real( ap( kk ),KIND=sp)
                    bkk = real( bp( kk ),KIND=sp)
                    call stdlib_ctpmv( uplo, 'NO TRANSPOSE', 'NON-UNIT', k-1, bp,ap( k1 ), 1_ilp )
                              
                    ct = half*akk
                    call stdlib_caxpy( k-1, ct, bp( k1 ), 1_ilp, ap( k1 ), 1_ilp )
                    call stdlib_chpr2( uplo, k-1, cone, ap( k1 ), 1_ilp, bp( k1 ), 1_ilp,ap )
                    call stdlib_caxpy( k-1, ct, bp( k1 ), 1_ilp, ap( k1 ), 1_ilp )
                    call stdlib_csscal( k-1, bkk, ap( k1 ), 1_ilp )
                    ap( kk ) = akk*bkk**2_ilp
                 end do
              else
                 ! compute l**h *a*l
                 ! jj and j1j1 are the indices of a(j,j) and a(j+1,j+1)
                 jj = 1_ilp
                 do j = 1, n
                    j1j1 = jj + n - j + 1_ilp
                    ! compute the j-th column of the lower triangle of a
                    ajj = real( ap( jj ),KIND=sp)
                    bjj = real( bp( jj ),KIND=sp)
                    ap( jj ) = ajj*bjj + stdlib_cdotc( n-j, ap( jj+1 ), 1_ilp,bp( jj+1 ), 1_ilp )
                    call stdlib_csscal( n-j, bjj, ap( jj+1 ), 1_ilp )
                    call stdlib_chpmv( uplo, n-j, cone, ap( j1j1 ), bp( jj+1 ), 1_ilp,cone, ap( jj+1 )&
                              , 1_ilp )
                    call stdlib_ctpmv( uplo, 'CONJUGATE TRANSPOSE', 'NON-UNIT',n-j+1, bp( jj ), &
                              ap( jj ), 1_ilp )
                    jj = j1j1
                 end do
              end if
           end if
           return
     end subroutine stdlib_chpgst

     pure module subroutine stdlib_zhpgst( itype, uplo, n, ap, bp, info )
     !! ZHPGST reduces a complex Hermitian-definite generalized
     !! eigenproblem to standard form, using packed storage.
     !! If ITYPE = 1, the problem is A*x = lambda*B*x,
     !! and A is overwritten by inv(U**H)*A*inv(U) or inv(L)*A*inv(L**H)
     !! If ITYPE = 2 or 3, the problem is A*B*x = lambda*x or
     !! B*A*x = lambda*x, and A is overwritten by U*A*U**H or L**H*A*L.
     !! B must have been previously factorized as U**H*U or L*L**H by ZPPTRF.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, n
           ! Array Arguments 
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(in) :: bp(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, j1, j1j1, jj, k, k1, k1k1, kk
           real(dp) :: ajj, akk, bjj, bkk
           complex(dp) :: ct
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHPGST', -info )
              return
           end if
           if( itype==1_ilp ) then
              if( upper ) then
                 ! compute inv(u**h)*a*inv(u)
                 ! j1 and jj are the indices of a(1,j) and a(j,j)
                 jj = 0_ilp
                 do j = 1, n
                    j1 = jj + 1_ilp
                    jj = jj + j
                    ! compute the j-th column of the upper triangle of a
                    ap( jj ) = real( ap( jj ),KIND=dp)
                    bjj = real( bp( jj ),KIND=dp)
                    call stdlib_ztpsv( uplo, 'CONJUGATE TRANSPOSE', 'NON-UNIT', j,bp, ap( j1 ), 1_ilp &
                              )
                    call stdlib_zhpmv( uplo, j-1, -cone, ap, bp( j1 ), 1_ilp, cone,ap( j1 ), 1_ilp )
                              
                    call stdlib_zdscal( j-1, one / bjj, ap( j1 ), 1_ilp )
                    ap( jj ) = ( ap( jj )-stdlib_zdotc( j-1, ap( j1 ), 1_ilp, bp( j1 ),1_ilp ) ) / &
                              bjj
                 end do
              else
                 ! compute inv(l)*a*inv(l**h)
                 ! kk and k1k1 are the indices of a(k,k) and a(k+1,k+1)
                 kk = 1_ilp
                 do k = 1, n
                    k1k1 = kk + n - k + 1_ilp
                    ! update the lower triangle of a(k:n,k:n)
                    akk = real( ap( kk ),KIND=dp)
                    bkk = real( bp( kk ),KIND=dp)
                    akk = akk / bkk**2_ilp
                    ap( kk ) = akk
                    if( k<n ) then
                       call stdlib_zdscal( n-k, one / bkk, ap( kk+1 ), 1_ilp )
                       ct = -half*akk
                       call stdlib_zaxpy( n-k, ct, bp( kk+1 ), 1_ilp, ap( kk+1 ), 1_ilp )
                       call stdlib_zhpr2( uplo, n-k, -cone, ap( kk+1 ), 1_ilp,bp( kk+1 ), 1_ilp, ap( k1k1 &
                                 ) )
                       call stdlib_zaxpy( n-k, ct, bp( kk+1 ), 1_ilp, ap( kk+1 ), 1_ilp )
                       call stdlib_ztpsv( uplo, 'NO TRANSPOSE', 'NON-UNIT', n-k,bp( k1k1 ), ap( &
                                 kk+1 ), 1_ilp )
                    end if
                    kk = k1k1
                 end do
              end if
           else
              if( upper ) then
                 ! compute u*a*u**h
                 ! k1 and kk are the indices of a(1,k) and a(k,k)
                 kk = 0_ilp
                 do k = 1, n
                    k1 = kk + 1_ilp
                    kk = kk + k
                    ! update the upper triangle of a(1:k,1:k)
                    akk = real( ap( kk ),KIND=dp)
                    bkk = real( bp( kk ),KIND=dp)
                    call stdlib_ztpmv( uplo, 'NO TRANSPOSE', 'NON-UNIT', k-1, bp,ap( k1 ), 1_ilp )
                              
                    ct = half*akk
                    call stdlib_zaxpy( k-1, ct, bp( k1 ), 1_ilp, ap( k1 ), 1_ilp )
                    call stdlib_zhpr2( uplo, k-1, cone, ap( k1 ), 1_ilp, bp( k1 ), 1_ilp,ap )
                    call stdlib_zaxpy( k-1, ct, bp( k1 ), 1_ilp, ap( k1 ), 1_ilp )
                    call stdlib_zdscal( k-1, bkk, ap( k1 ), 1_ilp )
                    ap( kk ) = akk*bkk**2_ilp
                 end do
              else
                 ! compute l**h *a*l
                 ! jj and j1j1 are the indices of a(j,j) and a(j+1,j+1)
                 jj = 1_ilp
                 do j = 1, n
                    j1j1 = jj + n - j + 1_ilp
                    ! compute the j-th column of the lower triangle of a
                    ajj = real( ap( jj ),KIND=dp)
                    bjj = real( bp( jj ),KIND=dp)
                    ap( jj ) = ajj*bjj + stdlib_zdotc( n-j, ap( jj+1 ), 1_ilp,bp( jj+1 ), 1_ilp )
                    call stdlib_zdscal( n-j, bjj, ap( jj+1 ), 1_ilp )
                    call stdlib_zhpmv( uplo, n-j, cone, ap( j1j1 ), bp( jj+1 ), 1_ilp,cone, ap( jj+1 )&
                              , 1_ilp )
                    call stdlib_ztpmv( uplo, 'CONJUGATE TRANSPOSE', 'NON-UNIT',n-j+1, bp( jj ), &
                              ap( jj ), 1_ilp )
                    jj = j1j1
                 end do
              end if
           end if
           return
     end subroutine stdlib_zhpgst




     pure module subroutine stdlib_chbgst( vect, uplo, n, ka, kb, ab, ldab, bb, ldbb, x,ldx, work, rwork,&
     !! CHBGST reduces a complex Hermitian-definite banded generalized
     !! eigenproblem  A*x = lambda*B*x  to standard form  C*y = lambda*y,
     !! such that C has the same bandwidth as A.
     !! B must have been previously factorized as S**H*S by CPBSTF, using a
     !! split Cholesky factorization. A is overwritten by C = X**H*A*X, where
     !! X = S**(-1)*Q and Q is a unitary matrix chosen to preserve the
     !! bandwidth of A.
                info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldx, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: ab(ldab,*)
           complex(sp), intent(in) :: bb(ldbb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: update, upper, wantx
           integer(ilp) :: i, i0, i1, i2, inca, j, j1, j1t, j2, j2t, k, ka1, kb1, kbt, l, m, nr, &
                     nrt, nx
           real(sp) :: bii
           complex(sp) :: ra, ra1, t
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           wantx = stdlib_lsame( vect, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           ka1 = ka + 1_ilp
           kb1 = kb + 1_ilp
           info = 0_ilp
           if( .not.wantx .and. .not.stdlib_lsame( vect, 'N' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ka<0_ilp ) then
              info = -4_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -5_ilp
           else if( ldab<ka+1 ) then
              info = -7_ilp
           else if( ldbb<kb+1 ) then
              info = -9_ilp
           else if( ldx<1_ilp .or. wantx .and. ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHBGST', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           inca = ldab*ka1
           ! initialize x to the unit matrix, if needed
           if( wantx )call stdlib_claset( 'FULL', n, n, czero, cone, x, ldx )
           ! set m to the splitting point m. it must be the same value as is
           ! used in stdlib_cpbstf. the chosen value allows the arrays work and rwork
           ! to be of dimension (n).
           m = ( n+kb ) / 2_ilp
           ! the routine works in two phases, corresponding to the two halves
           ! of the split cholesky factorization of b as s**h*s where
           ! s = ( u    )
               ! ( m  l )
           ! with u upper triangular of order m, and l lower triangular of
           ! order n-m. s has the same bandwidth as b.
           ! s is treated as a product of elementary matrices:
           ! s = s(m)*s(m-1)*...*s(2)*s(1)*s(m+1)*s(m+2)*...*s(n-1)*s(n)
           ! where s(i) is determined by the i-th row of s.
           ! in phase 1, the index i takes the values n, n-1, ... , m+1;
           ! in phase 2, it takes the values 1, 2, ... , m.
           ! for each value of i, the current matrix a is updated by forming
           ! inv(s(i))**h*a*inv(s(i)). this creates a triangular bulge outside
           ! the band of a. the bulge is then pushed down toward the bottom of
           ! a in phase 1, and up toward the top of a in phase 2, by applying
           ! plane rotations.
           ! there are kb*(kb+1)/2 elements in the bulge, but at most 2*kb-1
           ! of them are linearly independent, so annihilating a bulge requires
           ! only 2*kb-1 plane rotations. the rotations are divided into a 1st
           ! set of kb-1 rotations, and a 2nd set of kb rotations.
           ! wherever possible, rotations are generated and applied in vector
           ! operations of length nr between the indices j1 and j2 (sometimes
           ! replaced by modified values nrt, j1t or j2t).
           ! the real cosines and complex sines of the rotations are stored in
           ! the arrays rwork and work, those of the 1st set in elements
           ! 2:m-kb-1, and those of the 2nd set in elements m-kb+1:n.
           ! the bulges are not formed explicitly; nonzero elements outside the
           ! band are created only when they are required for generating new
           ! rotations; they are stored in the array work, in positions where
           ! they are later overwritten by the sines of the rotations which
           ! annihilate them.
           ! **************************** phase 1 *****************************
           ! the logical structure of this phase is:
           ! update = .true.
           ! do i = n, m + 1, -1
              ! use s(i) to update a and create a new bulge
              ! apply rotations to push all bulges ka positions downward
           ! end do
           ! update = .false.
           ! do i = m + ka + 1, n - 1
              ! apply rotations to push all bulges ka positions downward
           ! end do
           ! to avoid duplicating code, the two loops are merged.
           update = .true.
           i = n + 1_ilp
           10 continue
           if( update ) then
              i = i - 1_ilp
              kbt = min( kb, i-1 )
              i0 = i - 1_ilp
              i1 = min( n, i+ka )
              i2 = i - kbt + ka1
              if( i<m+1 ) then
                 update = .false.
                 i = i + 1_ilp
                 i0 = m
                 if( ka==0 )go to 480
                 go to 10
              end if
           else
              i = i + ka
              if( i>n-1 )go to 480
           end if
           if( upper ) then
              ! transform a, working with the upper triangle
              if( update ) then
                 ! form  inv(s(i))**h * a * inv(s(i))
                 bii = real( bb( kb1, i ),KIND=sp)
                 ab( ka1, i ) = ( real( ab( ka1, i ),KIND=sp) / bii ) / bii
                 do j = i + 1, i1
                    ab( i-j+ka1, j ) = ab( i-j+ka1, j ) / bii
                 end do
                 do j = max( 1, i-ka ), i - 1
                    ab( j-i+ka1, i ) = ab( j-i+ka1, i ) / bii
                 end do
                 do k = i - kbt, i - 1
                    do j = i - kbt, k
                       ab( j-k+ka1, k ) = ab( j-k+ka1, k ) -bb( j-i+kb1, i )*conjg( ab( k-i+ka1, &
                       i ) ) -conjg( bb( k-i+kb1, i ) )*ab( j-i+ka1, i ) +real( ab( ka1, i ),&
                                 KIND=sp)*bb( j-i+kb1, i )*conjg( bb( k-i+kb1, i ) )
                    end do
                    do j = max( 1, i-ka ), i - kbt - 1
                       ab( j-k+ka1, k ) = ab( j-k+ka1, k ) -conjg( bb( k-i+kb1, i ) )*ab( j-i+ka1,&
                                  i )
                    end do
                 end do
                 do j = i, i1
                    do k = max( j-ka, i-kbt ), i - 1
                       ab( k-j+ka1, j ) = ab( k-j+ka1, j ) -bb( k-i+kb1, i )*ab( i-j+ka1, j )
                                 
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_csscal( n-m, one / bii, x( m+1, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_cgerc( n-m, kbt, -cone, x( m+1, i ), 1_ilp,bb( kb1-kbt, i )&
                              , 1_ilp, x( m+1, i-kbt ),ldx )
                 end if
                 ! store a(i,i1) in ra1 for use in next loop over k
                 ra1 = ab( i-i1+ka1, i1 )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions down toward the bottom of the
              ! band
              loop_130: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i-k+ka<n .and. i-k>1_ilp ) then
                       ! generate rotation to annihilate a(i,i-k+ka+1)
                       call stdlib_clartg( ab( k+1, i-k+ka ), ra1,rwork( i-k+ka-m ), work( i-k+ka-&
                                 m ), ra )
                       ! create nonzero element a(i-k,i-k+ka+1) outside the
                       ! band and store it in work(i-k)
                       t = -bb( kb1-k, i )*ra1
                       work( i-k ) = rwork( i-k+ka-m )*t -conjg( work( i-k+ka-m ) )*ab( 1_ilp, i-k+ka &
                                 )
                       ab( 1_ilp, i-k+ka ) = work( i-k+ka-m )*t +rwork( i-k+ka-m )*ab( 1_ilp, i-k+ka )
                                 
                       ra1 = ra
                    end if
                 end if
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( update ) then
                    j2t = max( j2, i+2*ka-k+1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( n-j2t+ka ) / ka1
                 do j = j2t, j1, ka1
                    ! create nonzero element a(j-ka,j+1) outside the band
                    ! and store it in work(j-m)
                    work( j-m ) = work( j-m )*ab( 1_ilp, j+1 )
                    ab( 1_ilp, j+1 ) = rwork( j-m )*ab( 1_ilp, j+1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_clargv( nrt, ab( 1_ilp, j2t ), inca, work( j2t-m ), ka1,rwork(&
                            j2t-m ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the right
                    do l = 1, ka - 1
                       call stdlib_clartv( nr, ab( ka1-l, j2 ), inca,ab( ka-l, j2+1 ), inca, &
                                 rwork( j2-m ),work( j2-m ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_clar2v( nr, ab( ka1, j2 ), ab( ka1, j2+1 ),ab( ka, j2+1 ), inca, &
                              rwork( j2-m ),work( j2-m ), ka1 )
                    call stdlib_clacgv( nr, work( j2-m ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( l, j2+ka1-l ), inca,ab( l+1, j2+ka1-l &
                              ), inca, rwork( j2-m ),work( j2-m ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j2, j1, ka1
                       call stdlib_crot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,rwork( j-m ), &
                                 conjg( work( j-m ) ) )
                    end do
                 end if
              end do loop_130
              if( update ) then
                 if( i2<=n .and. kbt>0_ilp ) then
                    ! create nonzero element a(i-kbt,i-kbt+ka+1) outside the
                    ! band and store it in work(i-kbt)
                    work( i-kbt ) = -bb( kb1-kbt, i )*ra1
                 end if
              end if
              loop_170: do k = kb, 1, -1
                 if( update ) then
                    j2 = i - k - 1_ilp + max( 2_ilp, k-i0+1 )*ka1
                 else
                    j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the left
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+ka+l ) / ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( l, j2-l+1 ), inca,ab( l+1, j2-l+1 ), &
                              inca, rwork( j2-ka ),work( j2-ka ), ka1 )
                 end do
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 do j = j1, j2, -ka1
                    work( j ) = work( j-ka )
                    rwork( j ) = rwork( j-ka )
                 end do
                 do j = j2, j1, ka1
                    ! create nonzero element a(j-ka,j+1) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( 1_ilp, j+1 )
                    ab( 1_ilp, j+1 ) = rwork( j )*ab( 1_ilp, j+1 )
                 end do
                 if( update ) then
                    if( i-k<n-ka .and. k<=kbt )work( i-k+ka ) = work( i-k )
                 end if
              end do loop_170
              loop_210: do k = kb, 1, -1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_clargv( nr, ab( 1_ilp, j2 ), inca, work( j2 ), ka1,rwork( j2 ), ka1 )
                              
                    ! apply rotations in 2nd set from the right
                    do l = 1, ka - 1
                       call stdlib_clartv( nr, ab( ka1-l, j2 ), inca,ab( ka-l, j2+1 ), inca, &
                                 rwork( j2 ),work( j2 ), ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_clar2v( nr, ab( ka1, j2 ), ab( ka1, j2+1 ),ab( ka, j2+1 ), inca, &
                              rwork( j2 ),work( j2 ), ka1 )
                    call stdlib_clacgv( nr, work( j2 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( l, j2+ka1-l ), inca,ab( l+1, j2+ka1-l &
                              ), inca, rwork( j2 ),work( j2 ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j2, j1, ka1
                       call stdlib_crot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,rwork( j ), conjg( &
                                 work( j ) ) )
                    end do
                 end if
              end do loop_210
              do k = 1, kb - 1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 ! finish applying rotations in 1st set from the left
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( l, j2+ka1-l ), inca,ab( l+1, j2+ka1-l &
                              ), inca, rwork( j2-m ),work( j2-m ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = n - 1, j2 + ka, -1
                    rwork( j-m ) = rwork( j-ka-m )
                    work( j-m ) = work( j-ka-m )
                 end do
              end if
           else
              ! transform a, working with the lower triangle
              if( update ) then
                 ! form  inv(s(i))**h * a * inv(s(i))
                 bii = real( bb( 1_ilp, i ),KIND=sp)
                 ab( 1_ilp, i ) = ( real( ab( 1_ilp, i ),KIND=sp) / bii ) / bii
                 do j = i + 1, i1
                    ab( j-i+1, i ) = ab( j-i+1, i ) / bii
                 end do
                 do j = max( 1, i-ka ), i - 1
                    ab( i-j+1, j ) = ab( i-j+1, j ) / bii
                 end do
                 do k = i - kbt, i - 1
                    do j = i - kbt, k
                       ab( k-j+1, j ) = ab( k-j+1, j ) -bb( i-j+1, j )*conjg( ab( i-k+1,k ) ) - &
                       conjg( bb( i-k+1, k ) )*ab( i-j+1, j ) + real( ab( 1_ilp, i ),KIND=sp)*bb( i-j+&
                                 1_ilp, j )*conjg( bb( i-k+1,k ) )
                    end do
                    do j = max( 1, i-ka ), i - kbt - 1
                       ab( k-j+1, j ) = ab( k-j+1, j ) -conjg( bb( i-k+1, k ) )*ab( i-j+1, j )
                                 
                    end do
                 end do
                 do j = i, i1
                    do k = max( j-ka, i-kbt ), i - 1
                       ab( j-k+1, k ) = ab( j-k+1, k ) -bb( i-k+1, k )*ab( j-i+1, i )
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_csscal( n-m, one / bii, x( m+1, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_cgeru( n-m, kbt, -cone, x( m+1, i ), 1_ilp,bb( kbt+1, i-&
                              kbt ), ldbb-1,x( m+1, i-kbt ), ldx )
                 end if
                 ! store a(i1,i) in ra1 for use in next loop over k
                 ra1 = ab( i1-i+1, i )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions down toward the bottom of the
              ! band
              loop_360: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i-k+ka<n .and. i-k>1_ilp ) then
                       ! generate rotation to annihilate a(i-k+ka+1,i)
                       call stdlib_clartg( ab( ka1-k, i ), ra1, rwork( i-k+ka-m ),work( i-k+ka-m )&
                                 , ra )
                       ! create nonzero element a(i-k+ka+1,i-k) outside the
                       ! band and store it in work(i-k)
                       t = -bb( k+1, i-k )*ra1
                       work( i-k ) = rwork( i-k+ka-m )*t -conjg( work( i-k+ka-m ) )*ab( ka1, i-k )
                                 
                       ab( ka1, i-k ) = work( i-k+ka-m )*t +rwork( i-k+ka-m )*ab( ka1, i-k )
                                 
                       ra1 = ra
                    end if
                 end if
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( update ) then
                    j2t = max( j2, i+2*ka-k+1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( n-j2t+ka ) / ka1
                 do j = j2t, j1, ka1
                    ! create nonzero element a(j+1,j-ka) outside the band
                    ! and store it in work(j-m)
                    work( j-m ) = work( j-m )*ab( ka1, j-ka+1 )
                    ab( ka1, j-ka+1 ) = rwork( j-m )*ab( ka1, j-ka+1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_clargv( nrt, ab( ka1, j2t-ka ), inca, work( j2t-m ),ka1, &
                           rwork( j2t-m ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the left
                    do l = 1, ka - 1
                       call stdlib_clartv( nr, ab( l+1, j2-l ), inca,ab( l+2, j2-l ), inca, rwork(&
                                  j2-m ),work( j2-m ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_clar2v( nr, ab( 1_ilp, j2 ), ab( 1_ilp, j2+1 ), ab( 2_ilp, j2 ),inca, rwork( &
                              j2-m ), work( j2-m ), ka1 )
                    call stdlib_clacgv( nr, work( j2-m ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( ka1-l+1, j2 ), inca,ab( ka1-l, j2+1 ),&
                               inca, rwork( j2-m ),work( j2-m ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j2, j1, ka1
                       call stdlib_crot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,rwork( j-m ), work(&
                                  j-m ) )
                    end do
                 end if
              end do loop_360
              if( update ) then
                 if( i2<=n .and. kbt>0_ilp ) then
                    ! create nonzero element a(i-kbt+ka+1,i-kbt) outside the
                    ! band and store it in work(i-kbt)
                    work( i-kbt ) = -bb( kbt+1, i-kbt )*ra1
                 end if
              end if
              loop_400: do k = kb, 1, -1
                 if( update ) then
                    j2 = i - k - 1_ilp + max( 2_ilp, k-i0+1 )*ka1
                 else
                    j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the right
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+ka+l ) / ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( ka1-l+1, j2-ka ), inca,ab( ka1-l, j2-&
                              ka+1 ), inca,rwork( j2-ka ), work( j2-ka ), ka1 )
                 end do
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 do j = j1, j2, -ka1
                    work( j ) = work( j-ka )
                    rwork( j ) = rwork( j-ka )
                 end do
                 do j = j2, j1, ka1
                    ! create nonzero element a(j+1,j-ka) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( ka1, j-ka+1 )
                    ab( ka1, j-ka+1 ) = rwork( j )*ab( ka1, j-ka+1 )
                 end do
                 if( update ) then
                    if( i-k<n-ka .and. k<=kbt )work( i-k+ka ) = work( i-k )
                 end if
              end do loop_400
              loop_440: do k = kb, 1, -1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_clargv( nr, ab( ka1, j2-ka ), inca, work( j2 ), ka1,rwork( j2 ), &
                              ka1 )
                    ! apply rotations in 2nd set from the left
                    do l = 1, ka - 1
                       call stdlib_clartv( nr, ab( l+1, j2-l ), inca,ab( l+2, j2-l ), inca, rwork(&
                                  j2 ),work( j2 ), ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_clar2v( nr, ab( 1_ilp, j2 ), ab( 1_ilp, j2+1 ), ab( 2_ilp, j2 ),inca, rwork( &
                              j2 ), work( j2 ), ka1 )
                    call stdlib_clacgv( nr, work( j2 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( ka1-l+1, j2 ), inca,ab( ka1-l, j2+1 ),&
                               inca, rwork( j2 ),work( j2 ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j2, j1, ka1
                       call stdlib_crot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,rwork( j ), work( &
                                 j ) )
                    end do
                 end if
              end do loop_440
              do k = 1, kb - 1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 ! finish applying rotations in 1st set from the right
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( ka1-l+1, j2 ), inca,ab( ka1-l, j2+1 ),&
                               inca, rwork( j2-m ),work( j2-m ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = n - 1, j2 + ka, -1
                    rwork( j-m ) = rwork( j-ka-m )
                    work( j-m ) = work( j-ka-m )
                 end do
              end if
           end if
           go to 10
           480 continue
           ! **************************** phase 2 *****************************
           ! the logical structure of this phase is:
           ! update = .true.
           ! do i = 1, m
              ! use s(i) to update a and create a new bulge
              ! apply rotations to push all bulges ka positions upward
           ! end do
           ! update = .false.
           ! do i = m - ka - 1, 2, -1
              ! apply rotations to push all bulges ka positions upward
           ! end do
           ! to avoid duplicating code, the two loops are merged.
           update = .true.
           i = 0_ilp
           490 continue
           if( update ) then
              i = i + 1_ilp
              kbt = min( kb, m-i )
              i0 = i + 1_ilp
              i1 = max( 1_ilp, i-ka )
              i2 = i + kbt - ka1
              if( i>m ) then
                 update = .false.
                 i = i - 1_ilp
                 i0 = m + 1_ilp
                 if( ka==0 )return
                 go to 490
              end if
           else
              i = i - ka
              if( i<2 )return
           end if
           if( i<m-kbt ) then
              nx = m
           else
              nx = n
           end if
           if( upper ) then
              ! transform a, working with the upper triangle
              if( update ) then
                 ! form  inv(s(i))**h * a * inv(s(i))
                 bii = real( bb( kb1, i ),KIND=sp)
                 ab( ka1, i ) = ( real( ab( ka1, i ),KIND=sp) / bii ) / bii
                 do j = i1, i - 1
                    ab( j-i+ka1, i ) = ab( j-i+ka1, i ) / bii
                 end do
                 do j = i + 1, min( n, i+ka )
                    ab( i-j+ka1, j ) = ab( i-j+ka1, j ) / bii
                 end do
                 do k = i + 1, i + kbt
                    do j = k, i + kbt
                       ab( k-j+ka1, j ) = ab( k-j+ka1, j ) -bb( i-j+kb1, j )*conjg( ab( i-k+ka1, &
                       k ) ) -conjg( bb( i-k+kb1, k ) )*ab( i-j+ka1, j ) +real( ab( ka1, i ),&
                                 KIND=sp)*bb( i-j+kb1, j )*conjg( bb( i-k+kb1, k ) )
                    end do
                    do j = i + kbt + 1, min( n, i+ka )
                       ab( k-j+ka1, j ) = ab( k-j+ka1, j ) -conjg( bb( i-k+kb1, k ) )*ab( i-j+ka1,&
                                  j )
                    end do
                 end do
                 do j = i1, i
                    do k = i + 1, min( j+ka, i+kbt )
                       ab( j-k+ka1, k ) = ab( j-k+ka1, k ) -bb( i-k+kb1, k )*ab( j-i+ka1, i )
                                 
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_csscal( nx, one / bii, x( 1_ilp, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_cgeru( nx, kbt, -cone, x( 1_ilp, i ), 1_ilp,bb( kb, i+1 ), &
                              ldbb-1, x( 1_ilp, i+1 ), ldx )
                 end if
                 ! store a(i1,i) in ra1 for use in next loop over k
                 ra1 = ab( i1-i+ka1, i )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions up toward the top of the band
              loop_610: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i+k-ka1>0_ilp .and. i+k<m ) then
                       ! generate rotation to annihilate a(i+k-ka-1,i)
                       call stdlib_clartg( ab( k+1, i ), ra1, rwork( i+k-ka ),work( i+k-ka ), ra )
                                 
                       ! create nonzero element a(i+k-ka-1,i+k) outside the
                       ! band and store it in work(m-kb+i+k)
                       t = -bb( kb1-k, i+k )*ra1
                       work( m-kb+i+k ) = rwork( i+k-ka )*t -conjg( work( i+k-ka ) )*ab( 1_ilp, i+k )
                                 
                       ab( 1_ilp, i+k ) = work( i+k-ka )*t +rwork( i+k-ka )*ab( 1_ilp, i+k )
                       ra1 = ra
                    end if
                 end if
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( update ) then
                    j2t = min( j2, i-2*ka+k-1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( j2t+ka-1 ) / ka1
                 do j = j1, j2t, ka1
                    ! create nonzero element a(j-1,j+ka) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( 1_ilp, j+ka-1 )
                    ab( 1_ilp, j+ka-1 ) = rwork( j )*ab( 1_ilp, j+ka-1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_clargv( nrt, ab( 1_ilp, j1+ka ), inca, work( j1 ), ka1,rwork( &
                           j1 ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the left
                    do l = 1, ka - 1
                       call stdlib_clartv( nr, ab( ka1-l, j1+l ), inca,ab( ka-l, j1+l ), inca, &
                                 rwork( j1 ),work( j1 ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_clar2v( nr, ab( ka1, j1 ), ab( ka1, j1-1 ),ab( ka, j1 ), inca, &
                              rwork( j1 ), work( j1 ),ka1 )
                    call stdlib_clacgv( nr, work( j1 ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( l, j1t ), inca,ab( l+1, j1t-1 ), inca,&
                               rwork( j1t ),work( j1t ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j1, j2, ka1
                       call stdlib_crot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,rwork( j ), work( j ) )
                                 
                    end do
                 end if
              end do loop_610
              if( update ) then
                 if( i2>0_ilp .and. kbt>0_ilp ) then
                    ! create nonzero element a(i+kbt-ka-1,i+kbt) outside the
                    ! band and store it in work(m-kb+i+kbt)
                    work( m-kb+i+kbt ) = -bb( kb1-kbt, i+kbt )*ra1
                 end if
              end if
              loop_650: do k = kb, 1, -1
                 if( update ) then
                    j2 = i + k + 1_ilp - max( 2_ilp, k+i0-m )*ka1
                 else
                    j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the right
                 do l = kb - k, 1, -1
                    nrt = ( j2+ka+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( l, j1t+ka ), inca,ab( l+1, j1t+ka-1 ),&
                               inca,rwork( m-kb+j1t+ka ),work( m-kb+j1t+ka ), ka1 )
                 end do
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 do j = j1, j2, ka1
                    work( m-kb+j ) = work( m-kb+j+ka )
                    rwork( m-kb+j ) = rwork( m-kb+j+ka )
                 end do
                 do j = j1, j2, ka1
                    ! create nonzero element a(j-1,j+ka) outside the band
                    ! and store it in work(m-kb+j)
                    work( m-kb+j ) = work( m-kb+j )*ab( 1_ilp, j+ka-1 )
                    ab( 1_ilp, j+ka-1 ) = rwork( m-kb+j )*ab( 1_ilp, j+ka-1 )
                 end do
                 if( update ) then
                    if( i+k>ka1 .and. k<=kbt )work( m-kb+i+k-ka ) = work( m-kb+i+k )
                 end if
              end do loop_650
              loop_690: do k = kb, 1, -1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_clargv( nr, ab( 1_ilp, j1+ka ), inca, work( m-kb+j1 ),ka1, rwork( m-&
                              kb+j1 ), ka1 )
                    ! apply rotations in 2nd set from the left
                    do l = 1, ka - 1
                       call stdlib_clartv( nr, ab( ka1-l, j1+l ), inca,ab( ka-l, j1+l ), inca, &
                                 rwork( m-kb+j1 ),work( m-kb+j1 ), ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_clar2v( nr, ab( ka1, j1 ), ab( ka1, j1-1 ),ab( ka, j1 ), inca, &
                              rwork( m-kb+j1 ),work( m-kb+j1 ), ka1 )
                    call stdlib_clacgv( nr, work( m-kb+j1 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( l, j1t ), inca,ab( l+1, j1t-1 ), inca,&
                              rwork( m-kb+j1t ), work( m-kb+j1t ),ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j1, j2, ka1
                       call stdlib_crot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,rwork( m-kb+j ), work( &
                                 m-kb+j ) )
                    end do
                 end if
              end do loop_690
              do k = 1, kb - 1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 ! finish applying rotations in 1st set from the right
                 do l = kb - k, 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( l, j1t ), inca,ab( l+1, j1t-1 ), inca,&
                               rwork( j1t ),work( j1t ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = 2, i2 - ka
                    rwork( j ) = rwork( j+ka )
                    work( j ) = work( j+ka )
                 end do
              end if
           else
              ! transform a, working with the lower triangle
              if( update ) then
                 ! form  inv(s(i))**h * a * inv(s(i))
                 bii = real( bb( 1_ilp, i ),KIND=sp)
                 ab( 1_ilp, i ) = ( real( ab( 1_ilp, i ),KIND=sp) / bii ) / bii
                 do j = i1, i - 1
                    ab( i-j+1, j ) = ab( i-j+1, j ) / bii
                 end do
                 do j = i + 1, min( n, i+ka )
                    ab( j-i+1, i ) = ab( j-i+1, i ) / bii
                 end do
                 do k = i + 1, i + kbt
                    do j = k, i + kbt
                       ab( j-k+1, k ) = ab( j-k+1, k ) -bb( j-i+1, i )*conjg( ab( k-i+1,i ) ) - &
                       conjg( bb( k-i+1, i ) )*ab( j-i+1, i ) + real( ab( 1_ilp, i ),KIND=sp)*bb( j-i+&
                                 1_ilp, i )*conjg( bb( k-i+1,i ) )
                    end do
                    do j = i + kbt + 1, min( n, i+ka )
                       ab( j-k+1, k ) = ab( j-k+1, k ) -conjg( bb( k-i+1, i ) )*ab( j-i+1, i )
                                 
                    end do
                 end do
                 do j = i1, i
                    do k = i + 1, min( j+ka, i+kbt )
                       ab( k-j+1, j ) = ab( k-j+1, j ) -bb( k-i+1, i )*ab( i-j+1, j )
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_csscal( nx, one / bii, x( 1_ilp, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_cgerc( nx, kbt, -cone, x( 1_ilp, i ), 1_ilp, bb( 2_ilp, i ),1_ilp, x( &
                              1_ilp, i+1 ), ldx )
                 end if
                 ! store a(i,i1) in ra1 for use in next loop over k
                 ra1 = ab( i-i1+1, i1 )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions up toward the top of the band
              loop_840: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i+k-ka1>0_ilp .and. i+k<m ) then
                       ! generate rotation to annihilate a(i,i+k-ka-1)
                       call stdlib_clartg( ab( ka1-k, i+k-ka ), ra1,rwork( i+k-ka ), work( i+k-ka &
                                 ), ra )
                       ! create nonzero element a(i+k,i+k-ka-1) outside the
                       ! band and store it in work(m-kb+i+k)
                       t = -bb( k+1, i )*ra1
                       work( m-kb+i+k ) = rwork( i+k-ka )*t -conjg( work( i+k-ka ) )*ab( ka1, i+k-&
                                 ka )
                       ab( ka1, i+k-ka ) = work( i+k-ka )*t +rwork( i+k-ka )*ab( ka1, i+k-ka )
                                 
                       ra1 = ra
                    end if
                 end if
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( update ) then
                    j2t = min( j2, i-2*ka+k-1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( j2t+ka-1 ) / ka1
                 do j = j1, j2t, ka1
                    ! create nonzero element a(j+ka,j-1) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( ka1, j-1 )
                    ab( ka1, j-1 ) = rwork( j )*ab( ka1, j-1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_clargv( nrt, ab( ka1, j1 ), inca, work( j1 ), ka1,rwork( &
                           j1 ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the right
                    do l = 1, ka - 1
                       call stdlib_clartv( nr, ab( l+1, j1 ), inca, ab( l+2, j1-1 ),inca, rwork( &
                                 j1 ), work( j1 ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_clar2v( nr, ab( 1_ilp, j1 ), ab( 1_ilp, j1-1 ),ab( 2_ilp, j1-1 ), inca, rwork(&
                               j1 ),work( j1 ), ka1 )
                    call stdlib_clacgv( nr, work( j1 ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( ka1-l+1, j1t-ka1+l ), inca,ab( ka1-l, &
                              j1t-ka1+l ), inca,rwork( j1t ), work( j1t ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j1, j2, ka1
                       call stdlib_crot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,rwork( j ), conjg( work(&
                                  j ) ) )
                    end do
                 end if
              end do loop_840
              if( update ) then
                 if( i2>0_ilp .and. kbt>0_ilp ) then
                    ! create nonzero element a(i+kbt,i+kbt-ka-1) outside the
                    ! band and store it in work(m-kb+i+kbt)
                    work( m-kb+i+kbt ) = -bb( kbt+1, i )*ra1
                 end if
              end if
              loop_880: do k = kb, 1, -1
                 if( update ) then
                    j2 = i + k + 1_ilp - max( 2_ilp, k+i0-m )*ka1
                 else
                    j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the left
                 do l = kb - k, 1, -1
                    nrt = ( j2+ka+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( ka1-l+1, j1t+l-1 ), inca,ab( ka1-l, &
                              j1t+l-1 ), inca,rwork( m-kb+j1t+ka ),work( m-kb+j1t+ka ), ka1 )
                 end do
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 do j = j1, j2, ka1
                    work( m-kb+j ) = work( m-kb+j+ka )
                    rwork( m-kb+j ) = rwork( m-kb+j+ka )
                 end do
                 do j = j1, j2, ka1
                    ! create nonzero element a(j+ka,j-1) outside the band
                    ! and store it in work(m-kb+j)
                    work( m-kb+j ) = work( m-kb+j )*ab( ka1, j-1 )
                    ab( ka1, j-1 ) = rwork( m-kb+j )*ab( ka1, j-1 )
                 end do
                 if( update ) then
                    if( i+k>ka1 .and. k<=kbt )work( m-kb+i+k-ka ) = work( m-kb+i+k )
                 end if
              end do loop_880
              loop_920: do k = kb, 1, -1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_clargv( nr, ab( ka1, j1 ), inca, work( m-kb+j1 ),ka1, rwork( m-kb+&
                              j1 ), ka1 )
                    ! apply rotations in 2nd set from the right
                    do l = 1, ka - 1
                       call stdlib_clartv( nr, ab( l+1, j1 ), inca, ab( l+2, j1-1 ),inca, rwork( &
                                 m-kb+j1 ), work( m-kb+j1 ),ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_clar2v( nr, ab( 1_ilp, j1 ), ab( 1_ilp, j1-1 ),ab( 2_ilp, j1-1 ), inca, rwork(&
                               m-kb+j1 ),work( m-kb+j1 ), ka1 )
                    call stdlib_clacgv( nr, work( m-kb+j1 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( ka1-l+1, j1t-ka1+l ), inca,ab( ka1-l, &
                              j1t-ka1+l ), inca,rwork( m-kb+j1t ), work( m-kb+j1t ),ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j1, j2, ka1
                       call stdlib_crot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,rwork( m-kb+j ), conjg( &
                                 work( m-kb+j ) ) )
                    end do
                 end if
              end do loop_920
              do k = 1, kb - 1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 ! finish applying rotations in 1st set from the left
                 do l = kb - k, 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_clartv( nrt, ab( ka1-l+1, j1t-ka1+l ), inca,ab( ka1-l, &
                              j1t-ka1+l ), inca,rwork( j1t ), work( j1t ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = 2, i2 - ka
                    rwork( j ) = rwork( j+ka )
                    work( j ) = work( j+ka )
                 end do
              end if
           end if
           go to 490
     end subroutine stdlib_chbgst

     pure module subroutine stdlib_zhbgst( vect, uplo, n, ka, kb, ab, ldab, bb, ldbb, x,ldx, work, rwork,&
     !! ZHBGST reduces a complex Hermitian-definite banded generalized
     !! eigenproblem  A*x = lambda*B*x  to standard form  C*y = lambda*y,
     !! such that C has the same bandwidth as A.
     !! B must have been previously factorized as S**H*S by ZPBSTF, using a
     !! split Cholesky factorization. A is overwritten by C = X**H*A*X, where
     !! X = S**(-1)*Q and Q is a unitary matrix chosen to preserve the
     !! bandwidth of A.
                info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldx, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: ab(ldab,*)
           complex(dp), intent(in) :: bb(ldbb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: update, upper, wantx
           integer(ilp) :: i, i0, i1, i2, inca, j, j1, j1t, j2, j2t, k, ka1, kb1, kbt, l, m, nr, &
                     nrt, nx
           real(dp) :: bii
           complex(dp) :: ra, ra1, t
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           wantx = stdlib_lsame( vect, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           ka1 = ka + 1_ilp
           kb1 = kb + 1_ilp
           info = 0_ilp
           if( .not.wantx .and. .not.stdlib_lsame( vect, 'N' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ka<0_ilp ) then
              info = -4_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -5_ilp
           else if( ldab<ka+1 ) then
              info = -7_ilp
           else if( ldbb<kb+1 ) then
              info = -9_ilp
           else if( ldx<1_ilp .or. wantx .and. ldx<max( 1_ilp, n ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHBGST', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           inca = ldab*ka1
           ! initialize x to the unit matrix, if needed
           if( wantx )call stdlib_zlaset( 'FULL', n, n, czero, cone, x, ldx )
           ! set m to the splitting point m. it must be the same value as is
           ! used in stdlib_zpbstf. the chosen value allows the arrays work and rwork
           ! to be of dimension (n).
           m = ( n+kb ) / 2_ilp
           ! the routine works in two phases, corresponding to the two halves
           ! of the split cholesky factorization of b as s**h*s where
           ! s = ( u    )
               ! ( m  l )
           ! with u upper triangular of order m, and l lower triangular of
           ! order n-m. s has the same bandwidth as b.
           ! s is treated as a product of elementary matrices:
           ! s = s(m)*s(m-1)*...*s(2)*s(1)*s(m+1)*s(m+2)*...*s(n-1)*s(n)
           ! where s(i) is determined by the i-th row of s.
           ! in phase 1, the index i takes the values n, n-1, ... , m+1;
           ! in phase 2, it takes the values 1, 2, ... , m.
           ! for each value of i, the current matrix a is updated by forming
           ! inv(s(i))**h*a*inv(s(i)). this creates a triangular bulge outside
           ! the band of a. the bulge is then pushed down toward the bottom of
           ! a in phase 1, and up toward the top of a in phase 2, by applying
           ! plane rotations.
           ! there are kb*(kb+1)/2 elements in the bulge, but at most 2*kb-1
           ! of them are linearly independent, so annihilating a bulge requires
           ! only 2*kb-1 plane rotations. the rotations are divided into a 1st
           ! set of kb-1 rotations, and a 2nd set of kb rotations.
           ! wherever possible, rotations are generated and applied in vector
           ! operations of length nr between the indices j1 and j2 (sometimes
           ! replaced by modified values nrt, j1t or j2t).
           ! the real cosines and complex sines of the rotations are stored in
           ! the arrays rwork and work, those of the 1st set in elements
           ! 2:m-kb-1, and those of the 2nd set in elements m-kb+1:n.
           ! the bulges are not formed explicitly; nonzero elements outside the
           ! band are created only when they are required for generating new
           ! rotations; they are stored in the array work, in positions where
           ! they are later overwritten by the sines of the rotations which
           ! annihilate them.
           ! **************************** phase 1 *****************************
           ! the logical structure of this phase is:
           ! update = .true.
           ! do i = n, m + 1, -1
              ! use s(i) to update a and create a new bulge
              ! apply rotations to push all bulges ka positions downward
           ! end do
           ! update = .false.
           ! do i = m + ka + 1, n - 1
              ! apply rotations to push all bulges ka positions downward
           ! end do
           ! to avoid duplicating code, the two loops are merged.
           update = .true.
           i = n + 1_ilp
           10 continue
           if( update ) then
              i = i - 1_ilp
              kbt = min( kb, i-1 )
              i0 = i - 1_ilp
              i1 = min( n, i+ka )
              i2 = i - kbt + ka1
              if( i<m+1 ) then
                 update = .false.
                 i = i + 1_ilp
                 i0 = m
                 if( ka==0 )go to 480
                 go to 10
              end if
           else
              i = i + ka
              if( i>n-1 )go to 480
           end if
           if( upper ) then
              ! transform a, working with the upper triangle
              if( update ) then
                 ! form  inv(s(i))**h * a * inv(s(i))
                 bii = real( bb( kb1, i ),KIND=dp)
                 ab( ka1, i ) = ( real( ab( ka1, i ),KIND=dp) / bii ) / bii
                 do j = i + 1, i1
                    ab( i-j+ka1, j ) = ab( i-j+ka1, j ) / bii
                 end do
                 do j = max( 1, i-ka ), i - 1
                    ab( j-i+ka1, i ) = ab( j-i+ka1, i ) / bii
                 end do
                 do k = i - kbt, i - 1
                    do j = i - kbt, k
                       ab( j-k+ka1, k ) = ab( j-k+ka1, k ) -bb( j-i+kb1, i )*conjg( ab( k-i+ka1, &
                       i ) ) -conjg( bb( k-i+kb1, i ) )*ab( j-i+ka1, i ) +real( ab( ka1, i ),&
                                 KIND=dp)*bb( j-i+kb1, i )*conjg( bb( k-i+kb1, i ) )
                    end do
                    do j = max( 1, i-ka ), i - kbt - 1
                       ab( j-k+ka1, k ) = ab( j-k+ka1, k ) -conjg( bb( k-i+kb1, i ) )*ab( j-i+ka1,&
                                  i )
                    end do
                 end do
                 do j = i, i1
                    do k = max( j-ka, i-kbt ), i - 1
                       ab( k-j+ka1, j ) = ab( k-j+ka1, j ) -bb( k-i+kb1, i )*ab( i-j+ka1, j )
                                 
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_zdscal( n-m, one / bii, x( m+1, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_zgerc( n-m, kbt, -cone, x( m+1, i ), 1_ilp,bb( kb1-kbt, i )&
                              , 1_ilp, x( m+1, i-kbt ),ldx )
                 end if
                 ! store a(i,i1) in ra1 for use in next loop over k
                 ra1 = ab( i-i1+ka1, i1 )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions down toward the bottom of the
              ! band
              loop_130: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i-k+ka<n .and. i-k>1_ilp ) then
                       ! generate rotation to annihilate a(i,i-k+ka+1)
                       call stdlib_zlartg( ab( k+1, i-k+ka ), ra1,rwork( i-k+ka-m ), work( i-k+ka-&
                                 m ), ra )
                       ! create nonzero element a(i-k,i-k+ka+1) outside the
                       ! band and store it in work(i-k)
                       t = -bb( kb1-k, i )*ra1
                       work( i-k ) = rwork( i-k+ka-m )*t -conjg( work( i-k+ka-m ) )*ab( 1_ilp, i-k+ka &
                                 )
                       ab( 1_ilp, i-k+ka ) = work( i-k+ka-m )*t +rwork( i-k+ka-m )*ab( 1_ilp, i-k+ka )
                                 
                       ra1 = ra
                    end if
                 end if
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( update ) then
                    j2t = max( j2, i+2*ka-k+1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( n-j2t+ka ) / ka1
                 do j = j2t, j1, ka1
                    ! create nonzero element a(j-ka,j+1) outside the band
                    ! and store it in work(j-m)
                    work( j-m ) = work( j-m )*ab( 1_ilp, j+1 )
                    ab( 1_ilp, j+1 ) = rwork( j-m )*ab( 1_ilp, j+1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_zlargv( nrt, ab( 1_ilp, j2t ), inca, work( j2t-m ), ka1,rwork(&
                            j2t-m ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the right
                    do l = 1, ka - 1
                       call stdlib_zlartv( nr, ab( ka1-l, j2 ), inca,ab( ka-l, j2+1 ), inca, &
                                 rwork( j2-m ),work( j2-m ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_zlar2v( nr, ab( ka1, j2 ), ab( ka1, j2+1 ),ab( ka, j2+1 ), inca, &
                              rwork( j2-m ),work( j2-m ), ka1 )
                    call stdlib_zlacgv( nr, work( j2-m ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( l, j2+ka1-l ), inca,ab( l+1, j2+ka1-l &
                              ), inca, rwork( j2-m ),work( j2-m ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j2, j1, ka1
                       call stdlib_zrot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,rwork( j-m ), &
                                 conjg( work( j-m ) ) )
                    end do
                 end if
              end do loop_130
              if( update ) then
                 if( i2<=n .and. kbt>0_ilp ) then
                    ! create nonzero element a(i-kbt,i-kbt+ka+1) outside the
                    ! band and store it in work(i-kbt)
                    work( i-kbt ) = -bb( kb1-kbt, i )*ra1
                 end if
              end if
              loop_170: do k = kb, 1, -1
                 if( update ) then
                    j2 = i - k - 1_ilp + max( 2_ilp, k-i0+1 )*ka1
                 else
                    j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the left
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+ka+l ) / ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( l, j2-l+1 ), inca,ab( l+1, j2-l+1 ), &
                              inca, rwork( j2-ka ),work( j2-ka ), ka1 )
                 end do
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 do j = j1, j2, -ka1
                    work( j ) = work( j-ka )
                    rwork( j ) = rwork( j-ka )
                 end do
                 do j = j2, j1, ka1
                    ! create nonzero element a(j-ka,j+1) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( 1_ilp, j+1 )
                    ab( 1_ilp, j+1 ) = rwork( j )*ab( 1_ilp, j+1 )
                 end do
                 if( update ) then
                    if( i-k<n-ka .and. k<=kbt )work( i-k+ka ) = work( i-k )
                 end if
              end do loop_170
              loop_210: do k = kb, 1, -1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_zlargv( nr, ab( 1_ilp, j2 ), inca, work( j2 ), ka1,rwork( j2 ), ka1 )
                              
                    ! apply rotations in 2nd set from the right
                    do l = 1, ka - 1
                       call stdlib_zlartv( nr, ab( ka1-l, j2 ), inca,ab( ka-l, j2+1 ), inca, &
                                 rwork( j2 ),work( j2 ), ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_zlar2v( nr, ab( ka1, j2 ), ab( ka1, j2+1 ),ab( ka, j2+1 ), inca, &
                              rwork( j2 ),work( j2 ), ka1 )
                    call stdlib_zlacgv( nr, work( j2 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( l, j2+ka1-l ), inca,ab( l+1, j2+ka1-l &
                              ), inca, rwork( j2 ),work( j2 ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j2, j1, ka1
                       call stdlib_zrot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,rwork( j ), conjg( &
                                 work( j ) ) )
                    end do
                 end if
              end do loop_210
              do k = 1, kb - 1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 ! finish applying rotations in 1st set from the left
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( l, j2+ka1-l ), inca,ab( l+1, j2+ka1-l &
                              ), inca, rwork( j2-m ),work( j2-m ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = n - 1, j2 + ka, -1
                    rwork( j-m ) = rwork( j-ka-m )
                    work( j-m ) = work( j-ka-m )
                 end do
              end if
           else
              ! transform a, working with the lower triangle
              if( update ) then
                 ! form  inv(s(i))**h * a * inv(s(i))
                 bii = real( bb( 1_ilp, i ),KIND=dp)
                 ab( 1_ilp, i ) = ( real( ab( 1_ilp, i ),KIND=dp) / bii ) / bii
                 do j = i + 1, i1
                    ab( j-i+1, i ) = ab( j-i+1, i ) / bii
                 end do
                 do j = max( 1, i-ka ), i - 1
                    ab( i-j+1, j ) = ab( i-j+1, j ) / bii
                 end do
                 do k = i - kbt, i - 1
                    do j = i - kbt, k
                       ab( k-j+1, j ) = ab( k-j+1, j ) -bb( i-j+1, j )*conjg( ab( i-k+1,k ) ) - &
                       conjg( bb( i-k+1, k ) )*ab( i-j+1, j ) + real( ab( 1_ilp, i ),KIND=dp)*bb( i-j+&
                                 1_ilp, j )*conjg( bb( i-k+1,k ) )
                    end do
                    do j = max( 1, i-ka ), i - kbt - 1
                       ab( k-j+1, j ) = ab( k-j+1, j ) -conjg( bb( i-k+1, k ) )*ab( i-j+1, j )
                                 
                    end do
                 end do
                 do j = i, i1
                    do k = max( j-ka, i-kbt ), i - 1
                       ab( j-k+1, k ) = ab( j-k+1, k ) -bb( i-k+1, k )*ab( j-i+1, i )
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_zdscal( n-m, one / bii, x( m+1, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_zgeru( n-m, kbt, -cone, x( m+1, i ), 1_ilp,bb( kbt+1, i-&
                              kbt ), ldbb-1,x( m+1, i-kbt ), ldx )
                 end if
                 ! store a(i1,i) in ra1 for use in next loop over k
                 ra1 = ab( i1-i+1, i )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions down toward the bottom of the
              ! band
              loop_360: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i-k+ka<n .and. i-k>1_ilp ) then
                       ! generate rotation to annihilate a(i-k+ka+1,i)
                       call stdlib_zlartg( ab( ka1-k, i ), ra1, rwork( i-k+ka-m ),work( i-k+ka-m )&
                                 , ra )
                       ! create nonzero element a(i-k+ka+1,i-k) outside the
                       ! band and store it in work(i-k)
                       t = -bb( k+1, i-k )*ra1
                       work( i-k ) = rwork( i-k+ka-m )*t -conjg( work( i-k+ka-m ) )*ab( ka1, i-k )
                                 
                       ab( ka1, i-k ) = work( i-k+ka-m )*t +rwork( i-k+ka-m )*ab( ka1, i-k )
                                 
                       ra1 = ra
                    end if
                 end if
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( update ) then
                    j2t = max( j2, i+2*ka-k+1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( n-j2t+ka ) / ka1
                 do j = j2t, j1, ka1
                    ! create nonzero element a(j+1,j-ka) outside the band
                    ! and store it in work(j-m)
                    work( j-m ) = work( j-m )*ab( ka1, j-ka+1 )
                    ab( ka1, j-ka+1 ) = rwork( j-m )*ab( ka1, j-ka+1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_zlargv( nrt, ab( ka1, j2t-ka ), inca, work( j2t-m ),ka1, &
                           rwork( j2t-m ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the left
                    do l = 1, ka - 1
                       call stdlib_zlartv( nr, ab( l+1, j2-l ), inca,ab( l+2, j2-l ), inca, rwork(&
                                  j2-m ),work( j2-m ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_zlar2v( nr, ab( 1_ilp, j2 ), ab( 1_ilp, j2+1 ), ab( 2_ilp, j2 ),inca, rwork( &
                              j2-m ), work( j2-m ), ka1 )
                    call stdlib_zlacgv( nr, work( j2-m ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( ka1-l+1, j2 ), inca,ab( ka1-l, j2+1 ),&
                               inca, rwork( j2-m ),work( j2-m ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j2, j1, ka1
                       call stdlib_zrot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,rwork( j-m ), work(&
                                  j-m ) )
                    end do
                 end if
              end do loop_360
              if( update ) then
                 if( i2<=n .and. kbt>0_ilp ) then
                    ! create nonzero element a(i-kbt+ka+1,i-kbt) outside the
                    ! band and store it in work(i-kbt)
                    work( i-kbt ) = -bb( kbt+1, i-kbt )*ra1
                 end if
              end if
              loop_400: do k = kb, 1, -1
                 if( update ) then
                    j2 = i - k - 1_ilp + max( 2_ilp, k-i0+1 )*ka1
                 else
                    j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the right
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+ka+l ) / ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( ka1-l+1, j2-ka ), inca,ab( ka1-l, j2-&
                              ka+1 ), inca,rwork( j2-ka ), work( j2-ka ), ka1 )
                 end do
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 do j = j1, j2, -ka1
                    work( j ) = work( j-ka )
                    rwork( j ) = rwork( j-ka )
                 end do
                 do j = j2, j1, ka1
                    ! create nonzero element a(j+1,j-ka) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( ka1, j-ka+1 )
                    ab( ka1, j-ka+1 ) = rwork( j )*ab( ka1, j-ka+1 )
                 end do
                 if( update ) then
                    if( i-k<n-ka .and. k<=kbt )work( i-k+ka ) = work( i-k )
                 end if
              end do loop_400
              loop_440: do k = kb, 1, -1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+1 )*ka1
                 nr = ( n-j2+ka ) / ka1
                 j1 = j2 + ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_zlargv( nr, ab( ka1, j2-ka ), inca, work( j2 ), ka1,rwork( j2 ), &
                              ka1 )
                    ! apply rotations in 2nd set from the left
                    do l = 1, ka - 1
                       call stdlib_zlartv( nr, ab( l+1, j2-l ), inca,ab( l+2, j2-l ), inca, rwork(&
                                  j2 ),work( j2 ), ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_zlar2v( nr, ab( 1_ilp, j2 ), ab( 1_ilp, j2+1 ), ab( 2_ilp, j2 ),inca, rwork( &
                              j2 ), work( j2 ), ka1 )
                    call stdlib_zlacgv( nr, work( j2 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( ka1-l+1, j2 ), inca,ab( ka1-l, j2+1 ),&
                               inca, rwork( j2 ),work( j2 ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j2, j1, ka1
                       call stdlib_zrot( n-m, x( m+1, j ), 1_ilp, x( m+1, j+1 ), 1_ilp,rwork( j ), work( &
                                 j ) )
                    end do
                 end if
              end do loop_440
              do k = 1, kb - 1
                 j2 = i - k - 1_ilp + max( 1_ilp, k-i0+2 )*ka1
                 ! finish applying rotations in 1st set from the right
                 do l = kb - k, 1, -1
                    nrt = ( n-j2+l ) / ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( ka1-l+1, j2 ), inca,ab( ka1-l, j2+1 ),&
                               inca, rwork( j2-m ),work( j2-m ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = n - 1, j2 + ka, -1
                    rwork( j-m ) = rwork( j-ka-m )
                    work( j-m ) = work( j-ka-m )
                 end do
              end if
           end if
           go to 10
           480 continue
           ! **************************** phase 2 *****************************
           ! the logical structure of this phase is:
           ! update = .true.
           ! do i = 1, m
              ! use s(i) to update a and create a new bulge
              ! apply rotations to push all bulges ka positions upward
           ! end do
           ! update = .false.
           ! do i = m - ka - 1, 2, -1
              ! apply rotations to push all bulges ka positions upward
           ! end do
           ! to avoid duplicating code, the two loops are merged.
           update = .true.
           i = 0_ilp
           490 continue
           if( update ) then
              i = i + 1_ilp
              kbt = min( kb, m-i )
              i0 = i + 1_ilp
              i1 = max( 1_ilp, i-ka )
              i2 = i + kbt - ka1
              if( i>m ) then
                 update = .false.
                 i = i - 1_ilp
                 i0 = m + 1_ilp
                 if( ka==0 )return
                 go to 490
              end if
           else
              i = i - ka
              if( i<2 )return
           end if
           if( i<m-kbt ) then
              nx = m
           else
              nx = n
           end if
           if( upper ) then
              ! transform a, working with the upper triangle
              if( update ) then
                 ! form  inv(s(i))**h * a * inv(s(i))
                 bii = real( bb( kb1, i ),KIND=dp)
                 ab( ka1, i ) = ( real( ab( ka1, i ),KIND=dp) / bii ) / bii
                 do j = i1, i - 1
                    ab( j-i+ka1, i ) = ab( j-i+ka1, i ) / bii
                 end do
                 do j = i + 1, min( n, i+ka )
                    ab( i-j+ka1, j ) = ab( i-j+ka1, j ) / bii
                 end do
                 do k = i + 1, i + kbt
                    do j = k, i + kbt
                       ab( k-j+ka1, j ) = ab( k-j+ka1, j ) -bb( i-j+kb1, j )*conjg( ab( i-k+ka1, &
                       k ) ) -conjg( bb( i-k+kb1, k ) )*ab( i-j+ka1, j ) +real( ab( ka1, i ),&
                                 KIND=dp)*bb( i-j+kb1, j )*conjg( bb( i-k+kb1, k ) )
                    end do
                    do j = i + kbt + 1, min( n, i+ka )
                       ab( k-j+ka1, j ) = ab( k-j+ka1, j ) -conjg( bb( i-k+kb1, k ) )*ab( i-j+ka1,&
                                  j )
                    end do
                 end do
                 do j = i1, i
                    do k = i + 1, min( j+ka, i+kbt )
                       ab( j-k+ka1, k ) = ab( j-k+ka1, k ) -bb( i-k+kb1, k )*ab( j-i+ka1, i )
                                 
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_zdscal( nx, one / bii, x( 1_ilp, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_zgeru( nx, kbt, -cone, x( 1_ilp, i ), 1_ilp,bb( kb, i+1 ), &
                              ldbb-1, x( 1_ilp, i+1 ), ldx )
                 end if
                 ! store a(i1,i) in ra1 for use in next loop over k
                 ra1 = ab( i1-i+ka1, i )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions up toward the top of the band
              loop_610: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i+k-ka1>0_ilp .and. i+k<m ) then
                       ! generate rotation to annihilate a(i+k-ka-1,i)
                       call stdlib_zlartg( ab( k+1, i ), ra1, rwork( i+k-ka ),work( i+k-ka ), ra )
                                 
                       ! create nonzero element a(i+k-ka-1,i+k) outside the
                       ! band and store it in work(m-kb+i+k)
                       t = -bb( kb1-k, i+k )*ra1
                       work( m-kb+i+k ) = rwork( i+k-ka )*t -conjg( work( i+k-ka ) )*ab( 1_ilp, i+k )
                                 
                       ab( 1_ilp, i+k ) = work( i+k-ka )*t +rwork( i+k-ka )*ab( 1_ilp, i+k )
                       ra1 = ra
                    end if
                 end if
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( update ) then
                    j2t = min( j2, i-2*ka+k-1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( j2t+ka-1 ) / ka1
                 do j = j1, j2t, ka1
                    ! create nonzero element a(j-1,j+ka) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( 1_ilp, j+ka-1 )
                    ab( 1_ilp, j+ka-1 ) = rwork( j )*ab( 1_ilp, j+ka-1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_zlargv( nrt, ab( 1_ilp, j1+ka ), inca, work( j1 ), ka1,rwork( &
                           j1 ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the left
                    do l = 1, ka - 1
                       call stdlib_zlartv( nr, ab( ka1-l, j1+l ), inca,ab( ka-l, j1+l ), inca, &
                                 rwork( j1 ),work( j1 ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_zlar2v( nr, ab( ka1, j1 ), ab( ka1, j1-1 ),ab( ka, j1 ), inca, &
                              rwork( j1 ), work( j1 ),ka1 )
                    call stdlib_zlacgv( nr, work( j1 ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( l, j1t ), inca,ab( l+1, j1t-1 ), inca,&
                               rwork( j1t ),work( j1t ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j1, j2, ka1
                       call stdlib_zrot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,rwork( j ), work( j ) )
                                 
                    end do
                 end if
              end do loop_610
              if( update ) then
                 if( i2>0_ilp .and. kbt>0_ilp ) then
                    ! create nonzero element a(i+kbt-ka-1,i+kbt) outside the
                    ! band and store it in work(m-kb+i+kbt)
                    work( m-kb+i+kbt ) = -bb( kb1-kbt, i+kbt )*ra1
                 end if
              end if
              loop_650: do k = kb, 1, -1
                 if( update ) then
                    j2 = i + k + 1_ilp - max( 2_ilp, k+i0-m )*ka1
                 else
                    j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the right
                 do l = kb - k, 1, -1
                    nrt = ( j2+ka+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( l, j1t+ka ), inca,ab( l+1, j1t+ka-1 ),&
                               inca,rwork( m-kb+j1t+ka ),work( m-kb+j1t+ka ), ka1 )
                 end do
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 do j = j1, j2, ka1
                    work( m-kb+j ) = work( m-kb+j+ka )
                    rwork( m-kb+j ) = rwork( m-kb+j+ka )
                 end do
                 do j = j1, j2, ka1
                    ! create nonzero element a(j-1,j+ka) outside the band
                    ! and store it in work(m-kb+j)
                    work( m-kb+j ) = work( m-kb+j )*ab( 1_ilp, j+ka-1 )
                    ab( 1_ilp, j+ka-1 ) = rwork( m-kb+j )*ab( 1_ilp, j+ka-1 )
                 end do
                 if( update ) then
                    if( i+k>ka1 .and. k<=kbt )work( m-kb+i+k-ka ) = work( m-kb+i+k )
                 end if
              end do loop_650
              loop_690: do k = kb, 1, -1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_zlargv( nr, ab( 1_ilp, j1+ka ), inca, work( m-kb+j1 ),ka1, rwork( m-&
                              kb+j1 ), ka1 )
                    ! apply rotations in 2nd set from the left
                    do l = 1, ka - 1
                       call stdlib_zlartv( nr, ab( ka1-l, j1+l ), inca,ab( ka-l, j1+l ), inca, &
                                 rwork( m-kb+j1 ),work( m-kb+j1 ), ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_zlar2v( nr, ab( ka1, j1 ), ab( ka1, j1-1 ),ab( ka, j1 ), inca, &
                              rwork( m-kb+j1 ),work( m-kb+j1 ), ka1 )
                    call stdlib_zlacgv( nr, work( m-kb+j1 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the right
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( l, j1t ), inca,ab( l+1, j1t-1 ), inca,&
                              rwork( m-kb+j1t ), work( m-kb+j1t ),ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j1, j2, ka1
                       call stdlib_zrot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,rwork( m-kb+j ), work( &
                                 m-kb+j ) )
                    end do
                 end if
              end do loop_690
              do k = 1, kb - 1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 ! finish applying rotations in 1st set from the right
                 do l = kb - k, 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( l, j1t ), inca,ab( l+1, j1t-1 ), inca,&
                               rwork( j1t ),work( j1t ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = 2, i2 - ka
                    rwork( j ) = rwork( j+ka )
                    work( j ) = work( j+ka )
                 end do
              end if
           else
              ! transform a, working with the lower triangle
              if( update ) then
                 ! form  inv(s(i))**h * a * inv(s(i))
                 bii = real( bb( 1_ilp, i ),KIND=dp)
                 ab( 1_ilp, i ) = ( real( ab( 1_ilp, i ),KIND=dp) / bii ) / bii
                 do j = i1, i - 1
                    ab( i-j+1, j ) = ab( i-j+1, j ) / bii
                 end do
                 do j = i + 1, min( n, i+ka )
                    ab( j-i+1, i ) = ab( j-i+1, i ) / bii
                 end do
                 do k = i + 1, i + kbt
                    do j = k, i + kbt
                       ab( j-k+1, k ) = ab( j-k+1, k ) -bb( j-i+1, i )*conjg( ab( k-i+1,i ) ) - &
                       conjg( bb( k-i+1, i ) )*ab( j-i+1, i ) + real( ab( 1_ilp, i ),KIND=dp)*bb( j-i+&
                                 1_ilp, i )*conjg( bb( k-i+1,i ) )
                    end do
                    do j = i + kbt + 1, min( n, i+ka )
                       ab( j-k+1, k ) = ab( j-k+1, k ) -conjg( bb( k-i+1, i ) )*ab( j-i+1, i )
                                 
                    end do
                 end do
                 do j = i1, i
                    do k = i + 1, min( j+ka, i+kbt )
                       ab( k-j+1, j ) = ab( k-j+1, j ) -bb( k-i+1, i )*ab( i-j+1, j )
                    end do
                 end do
                 if( wantx ) then
                    ! post-multiply x by inv(s(i))
                    call stdlib_zdscal( nx, one / bii, x( 1_ilp, i ), 1_ilp )
                    if( kbt>0_ilp )call stdlib_zgerc( nx, kbt, -cone, x( 1_ilp, i ), 1_ilp, bb( 2_ilp, i ),1_ilp, x( &
                              1_ilp, i+1 ), ldx )
                 end if
                 ! store a(i,i1) in ra1 for use in next loop over k
                 ra1 = ab( i-i1+1, i1 )
              end if
              ! generate and apply vectors of rotations to chase all the
              ! existing bulges ka positions up toward the top of the band
              loop_840: do k = 1, kb - 1
                 if( update ) then
                    ! determine the rotations which would annihilate the bulge
                    ! which has in theory just been created
                    if( i+k-ka1>0_ilp .and. i+k<m ) then
                       ! generate rotation to annihilate a(i,i+k-ka-1)
                       call stdlib_zlartg( ab( ka1-k, i+k-ka ), ra1,rwork( i+k-ka ), work( i+k-ka &
                                 ), ra )
                       ! create nonzero element a(i+k,i+k-ka-1) outside the
                       ! band and store it in work(m-kb+i+k)
                       t = -bb( k+1, i )*ra1
                       work( m-kb+i+k ) = rwork( i+k-ka )*t -conjg( work( i+k-ka ) )*ab( ka1, i+k-&
                                 ka )
                       ab( ka1, i+k-ka ) = work( i+k-ka )*t +rwork( i+k-ka )*ab( ka1, i+k-ka )
                                 
                       ra1 = ra
                    end if
                 end if
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( update ) then
                    j2t = min( j2, i-2*ka+k-1 )
                 else
                    j2t = j2
                 end if
                 nrt = ( j2t+ka-1 ) / ka1
                 do j = j1, j2t, ka1
                    ! create nonzero element a(j+ka,j-1) outside the band
                    ! and store it in work(j)
                    work( j ) = work( j )*ab( ka1, j-1 )
                    ab( ka1, j-1 ) = rwork( j )*ab( ka1, j-1 )
                 end do
                 ! generate rotations in 1st set to annihilate elements which
                 ! have been created outside the band
                 if( nrt>0_ilp )call stdlib_zlargv( nrt, ab( ka1, j1 ), inca, work( j1 ), ka1,rwork( &
                           j1 ), ka1 )
                 if( nr>0_ilp ) then
                    ! apply rotations in 1st set from the right
                    do l = 1, ka - 1
                       call stdlib_zlartv( nr, ab( l+1, j1 ), inca, ab( l+2, j1-1 ),inca, rwork( &
                                 j1 ), work( j1 ), ka1 )
                    end do
                    ! apply rotations in 1st set from both sides to diagonal
                    ! blocks
                    call stdlib_zlar2v( nr, ab( 1_ilp, j1 ), ab( 1_ilp, j1-1 ),ab( 2_ilp, j1-1 ), inca, rwork(&
                               j1 ),work( j1 ), ka1 )
                    call stdlib_zlacgv( nr, work( j1 ), ka1 )
                 end if
                 ! start applying rotations in 1st set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( ka1-l+1, j1t-ka1+l ), inca,ab( ka1-l, &
                              j1t-ka1+l ), inca,rwork( j1t ), work( j1t ), ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 1st set
                    do j = j1, j2, ka1
                       call stdlib_zrot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,rwork( j ), conjg( work(&
                                  j ) ) )
                    end do
                 end if
              end do loop_840
              if( update ) then
                 if( i2>0_ilp .and. kbt>0_ilp ) then
                    ! create nonzero element a(i+kbt,i+kbt-ka-1) outside the
                    ! band and store it in work(m-kb+i+kbt)
                    work( m-kb+i+kbt ) = -bb( kbt+1, i )*ra1
                 end if
              end if
              loop_880: do k = kb, 1, -1
                 if( update ) then
                    j2 = i + k + 1_ilp - max( 2_ilp, k+i0-m )*ka1
                 else
                    j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 end if
                 ! finish applying rotations in 2nd set from the left
                 do l = kb - k, 1, -1
                    nrt = ( j2+ka+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( ka1-l+1, j1t+l-1 ), inca,ab( ka1-l, &
                              j1t+l-1 ), inca,rwork( m-kb+j1t+ka ),work( m-kb+j1t+ka ), ka1 )
                 end do
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 do j = j1, j2, ka1
                    work( m-kb+j ) = work( m-kb+j+ka )
                    rwork( m-kb+j ) = rwork( m-kb+j+ka )
                 end do
                 do j = j1, j2, ka1
                    ! create nonzero element a(j+ka,j-1) outside the band
                    ! and store it in work(m-kb+j)
                    work( m-kb+j ) = work( m-kb+j )*ab( ka1, j-1 )
                    ab( ka1, j-1 ) = rwork( m-kb+j )*ab( ka1, j-1 )
                 end do
                 if( update ) then
                    if( i+k>ka1 .and. k<=kbt )work( m-kb+i+k-ka ) = work( m-kb+i+k )
                 end if
              end do loop_880
              loop_920: do k = kb, 1, -1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m )*ka1
                 nr = ( j2+ka-1 ) / ka1
                 j1 = j2 - ( nr-1 )*ka1
                 if( nr>0_ilp ) then
                    ! generate rotations in 2nd set to annihilate elements
                    ! which have been created outside the band
                    call stdlib_zlargv( nr, ab( ka1, j1 ), inca, work( m-kb+j1 ),ka1, rwork( m-kb+&
                              j1 ), ka1 )
                    ! apply rotations in 2nd set from the right
                    do l = 1, ka - 1
                       call stdlib_zlartv( nr, ab( l+1, j1 ), inca, ab( l+2, j1-1 ),inca, rwork( &
                                 m-kb+j1 ), work( m-kb+j1 ),ka1 )
                    end do
                    ! apply rotations in 2nd set from both sides to diagonal
                    ! blocks
                    call stdlib_zlar2v( nr, ab( 1_ilp, j1 ), ab( 1_ilp, j1-1 ),ab( 2_ilp, j1-1 ), inca, rwork(&
                               m-kb+j1 ),work( m-kb+j1 ), ka1 )
                    call stdlib_zlacgv( nr, work( m-kb+j1 ), ka1 )
                 end if
                 ! start applying rotations in 2nd set from the left
                 do l = ka - 1, kb - k + 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( ka1-l+1, j1t-ka1+l ), inca,ab( ka1-l, &
                              j1t-ka1+l ), inca,rwork( m-kb+j1t ), work( m-kb+j1t ),ka1 )
                 end do
                 if( wantx ) then
                    ! post-multiply x by product of rotations in 2nd set
                    do j = j1, j2, ka1
                       call stdlib_zrot( nx, x( 1_ilp, j ), 1_ilp, x( 1_ilp, j-1 ), 1_ilp,rwork( m-kb+j ), conjg( &
                                 work( m-kb+j ) ) )
                    end do
                 end if
              end do loop_920
              do k = 1, kb - 1
                 j2 = i + k + 1_ilp - max( 1_ilp, k+i0-m+1 )*ka1
                 ! finish applying rotations in 1st set from the left
                 do l = kb - k, 1, -1
                    nrt = ( j2+l-1 ) / ka1
                    j1t = j2 - ( nrt-1 )*ka1
                    if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( ka1-l+1, j1t-ka1+l ), inca,ab( ka1-l, &
                              j1t-ka1+l ), inca,rwork( j1t ), work( j1t ), ka1 )
                 end do
              end do
              if( kb>1_ilp ) then
                 do j = 2, i2 - ka
                    rwork( j ) = rwork( j+ka )
                    work( j ) = work( j+ka )
                 end do
              end if
           end if
           go to 490
     end subroutine stdlib_zhbgst




     pure module subroutine stdlib_spbstf( uplo, n, kd, ab, ldab, info )
     !! SPBSTF computes a split Cholesky factorization of a real
     !! symmetric positive definite band matrix A.
     !! This routine is designed to be used in conjunction with SSBGST.
     !! The factorization has the form  A = S**T*S  where S is a band matrix
     !! of the same bandwidth as A and the following structure:
     !! S = ( U    )
     !! ( M  L )
     !! where U is upper triangular of order m = (n+kd)/2, and L is lower
     !! triangular of order n-m.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, kld, km, m
           real(sp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SPBSTF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           kld = max( 1_ilp, ldab-1 )
           ! set the splitting point m.
           m = ( n+kd ) / 2_ilp
           if( upper ) then
              ! factorize a(m+1:n,m+1:n) as l**t*l, and update a(1:m,1:m).
              do j = n, m + 1, -1
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = ab( kd+1, j )
                 if( ajj<=zero )go to 50
                 ajj = sqrt( ajj )
                 ab( kd+1, j ) = ajj
                 km = min( j-1, kd )
                 ! compute elements j-km:j-1 of the j-th column and update the
                 ! the leading submatrix within the band.
                 call stdlib_sscal( km, one / ajj, ab( kd+1-km, j ), 1_ilp )
                 call stdlib_ssyr( 'UPPER', km, -one, ab( kd+1-km, j ), 1_ilp,ab( kd+1, j-km ), kld )
                           
              end do
              ! factorize the updated submatrix a(1:m,1:m) as u**t*u.
              do j = 1, m
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = ab( kd+1, j )
                 if( ajj<=zero )go to 50
                 ajj = sqrt( ajj )
                 ab( kd+1, j ) = ajj
                 km = min( kd, m-j )
                 ! compute elements j+1:j+km of the j-th row and update the
                 ! trailing submatrix within the band.
                 if( km>0_ilp ) then
                    call stdlib_sscal( km, one / ajj, ab( kd, j+1 ), kld )
                    call stdlib_ssyr( 'UPPER', km, -one, ab( kd, j+1 ), kld,ab( kd+1, j+1 ), kld )
                              
                 end if
              end do
           else
              ! factorize a(m+1:n,m+1:n) as l**t*l, and update a(1:m,1:m).
              do j = n, m + 1, -1
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = ab( 1_ilp, j )
                 if( ajj<=zero )go to 50
                 ajj = sqrt( ajj )
                 ab( 1_ilp, j ) = ajj
                 km = min( j-1, kd )
                 ! compute elements j-km:j-1 of the j-th row and update the
                 ! trailing submatrix within the band.
                 call stdlib_sscal( km, one / ajj, ab( km+1, j-km ), kld )
                 call stdlib_ssyr( 'LOWER', km, -one, ab( km+1, j-km ), kld,ab( 1_ilp, j-km ), kld )
                           
              end do
              ! factorize the updated submatrix a(1:m,1:m) as u**t*u.
              do j = 1, m
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = ab( 1_ilp, j )
                 if( ajj<=zero )go to 50
                 ajj = sqrt( ajj )
                 ab( 1_ilp, j ) = ajj
                 km = min( kd, m-j )
                 ! compute elements j+1:j+km of the j-th column and update the
                 ! trailing submatrix within the band.
                 if( km>0_ilp ) then
                    call stdlib_sscal( km, one / ajj, ab( 2_ilp, j ), 1_ilp )
                    call stdlib_ssyr( 'LOWER', km, -one, ab( 2_ilp, j ), 1_ilp,ab( 1_ilp, j+1 ), kld )
                 end if
              end do
           end if
           return
           50 continue
           info = j
           return
     end subroutine stdlib_spbstf

     pure module subroutine stdlib_dpbstf( uplo, n, kd, ab, ldab, info )
     !! DPBSTF computes a split Cholesky factorization of a real
     !! symmetric positive definite band matrix A.
     !! This routine is designed to be used in conjunction with DSBGST.
     !! The factorization has the form  A = S**T*S  where S is a band matrix
     !! of the same bandwidth as A and the following structure:
     !! S = ( U    )
     !! ( M  L )
     !! where U is upper triangular of order m = (n+kd)/2, and L is lower
     !! triangular of order n-m.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, kld, km, m
           real(dp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DPBSTF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           kld = max( 1_ilp, ldab-1 )
           ! set the splitting point m.
           m = ( n+kd ) / 2_ilp
           if( upper ) then
              ! factorize a(m+1:n,m+1:n) as l**t*l, and update a(1:m,1:m).
              do j = n, m + 1, -1
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = ab( kd+1, j )
                 if( ajj<=zero )go to 50
                 ajj = sqrt( ajj )
                 ab( kd+1, j ) = ajj
                 km = min( j-1, kd )
                 ! compute elements j-km:j-1 of the j-th column and update the
                 ! the leading submatrix within the band.
                 call stdlib_dscal( km, one / ajj, ab( kd+1-km, j ), 1_ilp )
                 call stdlib_dsyr( 'UPPER', km, -one, ab( kd+1-km, j ), 1_ilp,ab( kd+1, j-km ), kld )
                           
              end do
              ! factorize the updated submatrix a(1:m,1:m) as u**t*u.
              do j = 1, m
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = ab( kd+1, j )
                 if( ajj<=zero )go to 50
                 ajj = sqrt( ajj )
                 ab( kd+1, j ) = ajj
                 km = min( kd, m-j )
                 ! compute elements j+1:j+km of the j-th row and update the
                 ! trailing submatrix within the band.
                 if( km>0_ilp ) then
                    call stdlib_dscal( km, one / ajj, ab( kd, j+1 ), kld )
                    call stdlib_dsyr( 'UPPER', km, -one, ab( kd, j+1 ), kld,ab( kd+1, j+1 ), kld )
                              
                 end if
              end do
           else
              ! factorize a(m+1:n,m+1:n) as l**t*l, and update a(1:m,1:m).
              do j = n, m + 1, -1
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = ab( 1_ilp, j )
                 if( ajj<=zero )go to 50
                 ajj = sqrt( ajj )
                 ab( 1_ilp, j ) = ajj
                 km = min( j-1, kd )
                 ! compute elements j-km:j-1 of the j-th row and update the
                 ! trailing submatrix within the band.
                 call stdlib_dscal( km, one / ajj, ab( km+1, j-km ), kld )
                 call stdlib_dsyr( 'LOWER', km, -one, ab( km+1, j-km ), kld,ab( 1_ilp, j-km ), kld )
                           
              end do
              ! factorize the updated submatrix a(1:m,1:m) as u**t*u.
              do j = 1, m
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = ab( 1_ilp, j )
                 if( ajj<=zero )go to 50
                 ajj = sqrt( ajj )
                 ab( 1_ilp, j ) = ajj
                 km = min( kd, m-j )
                 ! compute elements j+1:j+km of the j-th column and update the
                 ! trailing submatrix within the band.
                 if( km>0_ilp ) then
                    call stdlib_dscal( km, one / ajj, ab( 2_ilp, j ), 1_ilp )
                    call stdlib_dsyr( 'LOWER', km, -one, ab( 2_ilp, j ), 1_ilp,ab( 1_ilp, j+1 ), kld )
                 end if
              end do
           end if
           return
           50 continue
           info = j
           return
     end subroutine stdlib_dpbstf


     pure module subroutine stdlib_cpbstf( uplo, n, kd, ab, ldab, info )
     !! CPBSTF computes a split Cholesky factorization of a complex
     !! Hermitian positive definite band matrix A.
     !! This routine is designed to be used in conjunction with CHBGST.
     !! The factorization has the form  A = S**H*S  where S is a band matrix
     !! of the same bandwidth as A and the following structure:
     !! S = ( U    )
     !! ( M  L )
     !! where U is upper triangular of order m = (n+kd)/2, and L is lower
     !! triangular of order n-m.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           ! Array Arguments 
           complex(sp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, kld, km, m
           real(sp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CPBSTF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           kld = max( 1_ilp, ldab-1 )
           ! set the splitting point m.
           m = ( n+kd ) / 2_ilp
           if( upper ) then
              ! factorize a(m+1:n,m+1:n) as l**h*l, and update a(1:m,1:m).
              do j = n, m + 1, -1
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = real( ab( kd+1, j ),KIND=sp)
                 if( ajj<=zero ) then
                    ab( kd+1, j ) = ajj
                    go to 50
                 end if
                 ajj = sqrt( ajj )
                 ab( kd+1, j ) = ajj
                 km = min( j-1, kd )
                 ! compute elements j-km:j-1 of the j-th column and update the
                 ! the leading submatrix within the band.
                 call stdlib_csscal( km, one / ajj, ab( kd+1-km, j ), 1_ilp )
                 call stdlib_cher( 'UPPER', km, -one, ab( kd+1-km, j ), 1_ilp,ab( kd+1, j-km ), kld )
                           
              end do
              ! factorize the updated submatrix a(1:m,1:m) as u**h*u.
              do j = 1, m
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = real( ab( kd+1, j ),KIND=sp)
                 if( ajj<=zero ) then
                    ab( kd+1, j ) = ajj
                    go to 50
                 end if
                 ajj = sqrt( ajj )
                 ab( kd+1, j ) = ajj
                 km = min( kd, m-j )
                 ! compute elements j+1:j+km of the j-th row and update the
                 ! trailing submatrix within the band.
                 if( km>0_ilp ) then
                    call stdlib_csscal( km, one / ajj, ab( kd, j+1 ), kld )
                    call stdlib_clacgv( km, ab( kd, j+1 ), kld )
                    call stdlib_cher( 'UPPER', km, -one, ab( kd, j+1 ), kld,ab( kd+1, j+1 ), kld )
                              
                    call stdlib_clacgv( km, ab( kd, j+1 ), kld )
                 end if
              end do
           else
              ! factorize a(m+1:n,m+1:n) as l**h*l, and update a(1:m,1:m).
              do j = n, m + 1, -1
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = real( ab( 1_ilp, j ),KIND=sp)
                 if( ajj<=zero ) then
                    ab( 1_ilp, j ) = ajj
                    go to 50
                 end if
                 ajj = sqrt( ajj )
                 ab( 1_ilp, j ) = ajj
                 km = min( j-1, kd )
                 ! compute elements j-km:j-1 of the j-th row and update the
                 ! trailing submatrix within the band.
                 call stdlib_csscal( km, one / ajj, ab( km+1, j-km ), kld )
                 call stdlib_clacgv( km, ab( km+1, j-km ), kld )
                 call stdlib_cher( 'LOWER', km, -one, ab( km+1, j-km ), kld,ab( 1_ilp, j-km ), kld )
                           
                 call stdlib_clacgv( km, ab( km+1, j-km ), kld )
              end do
              ! factorize the updated submatrix a(1:m,1:m) as u**h*u.
              do j = 1, m
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = real( ab( 1_ilp, j ),KIND=sp)
                 if( ajj<=zero ) then
                    ab( 1_ilp, j ) = ajj
                    go to 50
                 end if
                 ajj = sqrt( ajj )
                 ab( 1_ilp, j ) = ajj
                 km = min( kd, m-j )
                 ! compute elements j+1:j+km of the j-th column and update the
                 ! trailing submatrix within the band.
                 if( km>0_ilp ) then
                    call stdlib_csscal( km, one / ajj, ab( 2_ilp, j ), 1_ilp )
                    call stdlib_cher( 'LOWER', km, -one, ab( 2_ilp, j ), 1_ilp,ab( 1_ilp, j+1 ), kld )
                 end if
              end do
           end if
           return
           50 continue
           info = j
           return
     end subroutine stdlib_cpbstf

     pure module subroutine stdlib_zpbstf( uplo, n, kd, ab, ldab, info )
     !! ZPBSTF computes a split Cholesky factorization of a complex
     !! Hermitian positive definite band matrix A.
     !! This routine is designed to be used in conjunction with ZHBGST.
     !! The factorization has the form  A = S**H*S  where S is a band matrix
     !! of the same bandwidth as A and the following structure:
     !! S = ( U    )
     !! ( M  L )
     !! where U is upper triangular of order m = (n+kd)/2, and L is lower
     !! triangular of order n-m.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           ! Array Arguments 
           complex(dp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: j, kld, km, m
           real(dp) :: ajj
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( ldab<kd+1 ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZPBSTF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           kld = max( 1_ilp, ldab-1 )
           ! set the splitting point m.
           m = ( n+kd ) / 2_ilp
           if( upper ) then
              ! factorize a(m+1:n,m+1:n) as l**h*l, and update a(1:m,1:m).
              do j = n, m + 1, -1
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = real( ab( kd+1, j ),KIND=dp)
                 if( ajj<=zero ) then
                    ab( kd+1, j ) = ajj
                    go to 50
                 end if
                 ajj = sqrt( ajj )
                 ab( kd+1, j ) = ajj
                 km = min( j-1, kd )
                 ! compute elements j-km:j-1 of the j-th column and update the
                 ! the leading submatrix within the band.
                 call stdlib_zdscal( km, one / ajj, ab( kd+1-km, j ), 1_ilp )
                 call stdlib_zher( 'UPPER', km, -one, ab( kd+1-km, j ), 1_ilp,ab( kd+1, j-km ), kld )
                           
              end do
              ! factorize the updated submatrix a(1:m,1:m) as u**h*u.
              do j = 1, m
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = real( ab( kd+1, j ),KIND=dp)
                 if( ajj<=zero ) then
                    ab( kd+1, j ) = ajj
                    go to 50
                 end if
                 ajj = sqrt( ajj )
                 ab( kd+1, j ) = ajj
                 km = min( kd, m-j )
                 ! compute elements j+1:j+km of the j-th row and update the
                 ! trailing submatrix within the band.
                 if( km>0_ilp ) then
                    call stdlib_zdscal( km, one / ajj, ab( kd, j+1 ), kld )
                    call stdlib_zlacgv( km, ab( kd, j+1 ), kld )
                    call stdlib_zher( 'UPPER', km, -one, ab( kd, j+1 ), kld,ab( kd+1, j+1 ), kld )
                              
                    call stdlib_zlacgv( km, ab( kd, j+1 ), kld )
                 end if
              end do
           else
              ! factorize a(m+1:n,m+1:n) as l**h*l, and update a(1:m,1:m).
              do j = n, m + 1, -1
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = real( ab( 1_ilp, j ),KIND=dp)
                 if( ajj<=zero ) then
                    ab( 1_ilp, j ) = ajj
                    go to 50
                 end if
                 ajj = sqrt( ajj )
                 ab( 1_ilp, j ) = ajj
                 km = min( j-1, kd )
                 ! compute elements j-km:j-1 of the j-th row and update the
                 ! trailing submatrix within the band.
                 call stdlib_zdscal( km, one / ajj, ab( km+1, j-km ), kld )
                 call stdlib_zlacgv( km, ab( km+1, j-km ), kld )
                 call stdlib_zher( 'LOWER', km, -one, ab( km+1, j-km ), kld,ab( 1_ilp, j-km ), kld )
                           
                 call stdlib_zlacgv( km, ab( km+1, j-km ), kld )
              end do
              ! factorize the updated submatrix a(1:m,1:m) as u**h*u.
              do j = 1, m
                 ! compute s(j,j) and test for non-positive-definiteness.
                 ajj = real( ab( 1_ilp, j ),KIND=dp)
                 if( ajj<=zero ) then
                    ab( 1_ilp, j ) = ajj
                    go to 50
                 end if
                 ajj = sqrt( ajj )
                 ab( 1_ilp, j ) = ajj
                 km = min( kd, m-j )
                 ! compute elements j+1:j+km of the j-th column and update the
                 ! trailing submatrix within the band.
                 if( km>0_ilp ) then
                    call stdlib_zdscal( km, one / ajj, ab( 2_ilp, j ), 1_ilp )
                    call stdlib_zher( 'LOWER', km, -one, ab( 2_ilp, j ), 1_ilp,ab( 1_ilp, j+1 ), kld )
                 end if
              end do
           end if
           return
           50 continue
           info = j
           return
     end subroutine stdlib_zpbstf




     pure module subroutine stdlib_slag2( a, lda, b, ldb, safmin, scale1, scale2, wr1,wr2, wi )
     !! SLAG2 computes the eigenvalues of a 2 x 2 generalized eigenvalue
     !! problem  A - w B, with scaling as necessary to avoid over-/underflow.
     !! The scaling factor "s" results in a modified eigenvalue equation
     !! s A - w B
     !! where  s  is a non-negative scaling factor chosen so that  w,  w B,
     !! and  s A  do not overflow and, if possible, do not underflow, either.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, ldb
           real(sp), intent(in) :: safmin
           real(sp), intent(out) :: scale1, scale2, wi, wr1, wr2
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: fuzzy1 = one+1.0e-5_sp
           
           
           
           ! Local Scalars 
           real(sp) :: a11, a12, a21, a22, abi22, anorm, as11, as12, as22, ascale, b11, b12, b22, &
           binv11, binv22, bmin, bnorm, bscale, bsize, c1, c2, c3, c4, c5, diff, discr, pp, qq, r,&
            rtmax, rtmin, s1, s2, safmax, shift, ss, sum, wabs, wbig, wdet, wscale, wsize, &
                      wsmall
           ! Intrinsic Functions 
           ! Executable Statements 
           rtmin = sqrt( safmin )
           rtmax = one / rtmin
           safmax = one / safmin
           ! scale a
           anorm = max( abs( a( 1_ilp, 1_ilp ) )+abs( a( 2_ilp, 1_ilp ) ),abs( a( 1_ilp, 2_ilp ) )+abs( a( 2_ilp, 2_ilp ) ), &
                     safmin )
           ascale = one / anorm
           a11 = ascale*a( 1_ilp, 1_ilp )
           a21 = ascale*a( 2_ilp, 1_ilp )
           a12 = ascale*a( 1_ilp, 2_ilp )
           a22 = ascale*a( 2_ilp, 2_ilp )
           ! perturb b if necessary to insure non-singularity
           b11 = b( 1_ilp, 1_ilp )
           b12 = b( 1_ilp, 2_ilp )
           b22 = b( 2_ilp, 2_ilp )
           bmin = rtmin*max( abs( b11 ), abs( b12 ), abs( b22 ), rtmin )
           if( abs( b11 )<bmin )b11 = sign( bmin, b11 )
           if( abs( b22 )<bmin )b22 = sign( bmin, b22 )
           ! scale b
           bnorm = max( abs( b11 ), abs( b12 )+abs( b22 ), safmin )
           bsize = max( abs( b11 ), abs( b22 ) )
           bscale = one / bsize
           b11 = b11*bscale
           b12 = b12*bscale
           b22 = b22*bscale
           ! compute larger eigenvalue by method described by c. van loan
           ! ( as is a shifted by -shift*b )
           binv11 = one / b11
           binv22 = one / b22
           s1 = a11*binv11
           s2 = a22*binv22
           if( abs( s1 )<=abs( s2 ) ) then
              as12 = a12 - s1*b12
              as22 = a22 - s1*b22
              ss = a21*( binv11*binv22 )
              abi22 = as22*binv22 - ss*b12
              pp = half*abi22
              shift = s1
           else
              as12 = a12 - s2*b12
              as11 = a11 - s2*b11
              ss = a21*( binv11*binv22 )
              abi22 = -ss*b12
              pp = half*( as11*binv11+abi22 )
              shift = s2
           end if
           qq = ss*as12
           if( abs( pp*rtmin )>=one ) then
              discr = ( rtmin*pp )**2_ilp + qq*safmin
              r = sqrt( abs( discr ) )*rtmax
           else
              if( pp**2_ilp+abs( qq )<=safmin ) then
                 discr = ( rtmax*pp )**2_ilp + qq*safmax
                 r = sqrt( abs( discr ) )*rtmin
              else
                 discr = pp**2_ilp + qq
                 r = sqrt( abs( discr ) )
              end if
           end if
           ! note: the test of r in the following if is to cover the case when
                 ! discr is small and negative and is flushed to zero during
                 ! the calculation of r.  on machines which have a consistent
                 ! flush-to-zero threshold and handle numbers above that
                 ! threshold correctly, it would not be necessary.
           if( discr>=zero .or. r==zero ) then
              sum = pp + sign( r, pp )
              diff = pp - sign( r, pp )
              wbig = shift + sum
              ! compute smaller eigenvalue
              wsmall = shift + diff
              if( half*abs( wbig )>max( abs( wsmall ), safmin ) ) then
                 wdet = ( a11*a22-a12*a21 )*( binv11*binv22 )
                 wsmall = wdet / wbig
              end if
              ! choose (real) eigenvalue closest to 2,2 element of a*b**(-1)
              ! for wr1.
              if( pp>abi22 ) then
                 wr1 = min( wbig, wsmall )
                 wr2 = max( wbig, wsmall )
              else
                 wr1 = max( wbig, wsmall )
                 wr2 = min( wbig, wsmall )
              end if
              wi = zero
           else
              ! complex eigenvalues
              wr1 = shift + pp
              wr2 = wr1
              wi = r
           end if
           ! further scaling to avoid underflow and overflow in computing
           ! scale1 and overflow in computing w*b.
           ! this scale factor (wscale) is bounded from above using c1 and c2,
           ! and from below using c3 and c4.
              ! c1 implements the condition  s a  must never overflow.
              ! c2 implements the condition  w b  must never overflow.
              ! c3, with c2,
                 ! implement the condition that s a - w b must never overflow.
              ! c4 implements the condition  s    should not underflow.
              ! c5 implements the condition  max(s,|w|) should be at least 2.
           c1 = bsize*( safmin*max( one, ascale ) )
           c2 = safmin*max( one, bnorm )
           c3 = bsize*safmin
           if( ascale<=one .and. bsize<=one ) then
              c4 = min( one, ( ascale / safmin )*bsize )
           else
              c4 = one
           end if
           if( ascale<=one .or. bsize<=one ) then
              c5 = min( one, ascale*bsize )
           else
              c5 = one
           end if
           ! scale first eigenvalue
           wabs = abs( wr1 ) + abs( wi )
           wsize = max( safmin, c1, fuzzy1*( wabs*c2+c3 ),min( c4, half*max( wabs, c5 ) ) )
                     
           if( wsize/=one ) then
              wscale = one / wsize
              if( wsize>one ) then
                 scale1 = ( max( ascale, bsize )*wscale )*min( ascale, bsize )
              else
                 scale1 = ( min( ascale, bsize )*wscale )*max( ascale, bsize )
              end if
              wr1 = wr1*wscale
              if( wi/=zero ) then
                 wi = wi*wscale
                 wr2 = wr1
                 scale2 = scale1
              end if
           else
              scale1 = ascale*bsize
              scale2 = scale1
           end if
           ! scale second eigenvalue (if real)
           if( wi==zero ) then
              wsize = max( safmin, c1, fuzzy1*( abs( wr2 )*c2+c3 ),min( c4, half*max( abs( wr2 ), &
                        c5 ) ) )
              if( wsize/=one ) then
                 wscale = one / wsize
                 if( wsize>one ) then
                    scale2 = ( max( ascale, bsize )*wscale )*min( ascale, bsize )
                 else
                    scale2 = ( min( ascale, bsize )*wscale )*max( ascale, bsize )
                 end if
                 wr2 = wr2*wscale
              else
                 scale2 = ascale*bsize
              end if
           end if
           return
     end subroutine stdlib_slag2

     pure module subroutine stdlib_dlag2( a, lda, b, ldb, safmin, scale1, scale2, wr1,wr2, wi )
     !! DLAG2 computes the eigenvalues of a 2 x 2 generalized eigenvalue
     !! problem  A - w B, with scaling as necessary to avoid over-/underflow.
     !! The scaling factor "s" results in a modified eigenvalue equation
     !! s A - w B
     !! where  s  is a non-negative scaling factor chosen so that  w,  w B,
     !! and  s A  do not overflow and, if possible, do not underflow, either.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, ldb
           real(dp), intent(in) :: safmin
           real(dp), intent(out) :: scale1, scale2, wi, wr1, wr2
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: fuzzy1 = one+1.0e-5_dp
           
           
           
           ! Local Scalars 
           real(dp) :: a11, a12, a21, a22, abi22, anorm, as11, as12, as22, ascale, b11, b12, b22, &
           binv11, binv22, bmin, bnorm, bscale, bsize, c1, c2, c3, c4, c5, diff, discr, pp, qq, r,&
            rtmax, rtmin, s1, s2, safmax, shift, ss, sum, wabs, wbig, wdet, wscale, wsize, &
                      wsmall
           ! Intrinsic Functions 
           ! Executable Statements 
           rtmin = sqrt( safmin )
           rtmax = one / rtmin
           safmax = one / safmin
           ! scale a
           anorm = max( abs( a( 1_ilp, 1_ilp ) )+abs( a( 2_ilp, 1_ilp ) ),abs( a( 1_ilp, 2_ilp ) )+abs( a( 2_ilp, 2_ilp ) ), &
                     safmin )
           ascale = one / anorm
           a11 = ascale*a( 1_ilp, 1_ilp )
           a21 = ascale*a( 2_ilp, 1_ilp )
           a12 = ascale*a( 1_ilp, 2_ilp )
           a22 = ascale*a( 2_ilp, 2_ilp )
           ! perturb b if necessary to insure non-singularity
           b11 = b( 1_ilp, 1_ilp )
           b12 = b( 1_ilp, 2_ilp )
           b22 = b( 2_ilp, 2_ilp )
           bmin = rtmin*max( abs( b11 ), abs( b12 ), abs( b22 ), rtmin )
           if( abs( b11 )<bmin )b11 = sign( bmin, b11 )
           if( abs( b22 )<bmin )b22 = sign( bmin, b22 )
           ! scale b
           bnorm = max( abs( b11 ), abs( b12 )+abs( b22 ), safmin )
           bsize = max( abs( b11 ), abs( b22 ) )
           bscale = one / bsize
           b11 = b11*bscale
           b12 = b12*bscale
           b22 = b22*bscale
           ! compute larger eigenvalue by method described by c. van loan
           ! ( as is a shifted by -shift*b )
           binv11 = one / b11
           binv22 = one / b22
           s1 = a11*binv11
           s2 = a22*binv22
           if( abs( s1 )<=abs( s2 ) ) then
              as12 = a12 - s1*b12
              as22 = a22 - s1*b22
              ss = a21*( binv11*binv22 )
              abi22 = as22*binv22 - ss*b12
              pp = half*abi22
              shift = s1
           else
              as12 = a12 - s2*b12
              as11 = a11 - s2*b11
              ss = a21*( binv11*binv22 )
              abi22 = -ss*b12
              pp = half*( as11*binv11+abi22 )
              shift = s2
           end if
           qq = ss*as12
           if( abs( pp*rtmin )>=one ) then
              discr = ( rtmin*pp )**2_ilp + qq*safmin
              r = sqrt( abs( discr ) )*rtmax
           else
              if( pp**2_ilp+abs( qq )<=safmin ) then
                 discr = ( rtmax*pp )**2_ilp + qq*safmax
                 r = sqrt( abs( discr ) )*rtmin
              else
                 discr = pp**2_ilp + qq
                 r = sqrt( abs( discr ) )
              end if
           end if
           ! note: the test of r in the following if is to cover the case when
                 ! discr is small and negative and is flushed to zero during
                 ! the calculation of r.  on machines which have a consistent
                 ! flush-to-zero threshold and handle numbers above that
                 ! threshold correctly, it would not be necessary.
           if( discr>=zero .or. r==zero ) then
              sum = pp + sign( r, pp )
              diff = pp - sign( r, pp )
              wbig = shift + sum
              ! compute smaller eigenvalue
              wsmall = shift + diff
              if( half*abs( wbig )>max( abs( wsmall ), safmin ) ) then
                 wdet = ( a11*a22-a12*a21 )*( binv11*binv22 )
                 wsmall = wdet / wbig
              end if
              ! choose (real) eigenvalue closest to 2,2 element of a*b**(-1)
              ! for wr1.
              if( pp>abi22 ) then
                 wr1 = min( wbig, wsmall )
                 wr2 = max( wbig, wsmall )
              else
                 wr1 = max( wbig, wsmall )
                 wr2 = min( wbig, wsmall )
              end if
              wi = zero
           else
              ! complex eigenvalues
              wr1 = shift + pp
              wr2 = wr1
              wi = r
           end if
           ! further scaling to avoid underflow and overflow in computing
           ! scale1 and overflow in computing w*b.
           ! this scale factor (wscale) is bounded from above using c1 and c2,
           ! and from below using c3 and c4.
              ! c1 implements the condition  s a  must never overflow.
              ! c2 implements the condition  w b  must never overflow.
              ! c3, with c2,
                 ! implement the condition that s a - w b must never overflow.
              ! c4 implements the condition  s    should not underflow.
              ! c5 implements the condition  max(s,|w|) should be at least 2.
           c1 = bsize*( safmin*max( one, ascale ) )
           c2 = safmin*max( one, bnorm )
           c3 = bsize*safmin
           if( ascale<=one .and. bsize<=one ) then
              c4 = min( one, ( ascale / safmin )*bsize )
           else
              c4 = one
           end if
           if( ascale<=one .or. bsize<=one ) then
              c5 = min( one, ascale*bsize )
           else
              c5 = one
           end if
           ! scale first eigenvalue
           wabs = abs( wr1 ) + abs( wi )
           wsize = max( safmin, c1, fuzzy1*( wabs*c2+c3 ),min( c4, half*max( wabs, c5 ) ) )
                     
           if( wsize/=one ) then
              wscale = one / wsize
              if( wsize>one ) then
                 scale1 = ( max( ascale, bsize )*wscale )*min( ascale, bsize )
              else
                 scale1 = ( min( ascale, bsize )*wscale )*max( ascale, bsize )
              end if
              wr1 = wr1*wscale
              if( wi/=zero ) then
                 wi = wi*wscale
                 wr2 = wr1
                 scale2 = scale1
              end if
           else
              scale1 = ascale*bsize
              scale2 = scale1
           end if
           ! scale second eigenvalue (if real)
           if( wi==zero ) then
              wsize = max( safmin, c1, fuzzy1*( abs( wr2 )*c2+c3 ),min( c4, half*max( abs( wr2 ), &
                        c5 ) ) )
              if( wsize/=one ) then
                 wscale = one / wsize
                 if( wsize>one ) then
                    scale2 = ( max( ascale, bsize )*wscale )*min( ascale, bsize )
                 else
                    scale2 = ( min( ascale, bsize )*wscale )*max( ascale, bsize )
                 end if
                 wr2 = wr2*wscale
              else
                 scale2 = ascale*bsize
              end if
           end if
           return
     end subroutine stdlib_dlag2





     pure module subroutine stdlib_sorm22( side, trans, m, n, n1, n2, q, ldq, c, ldc,work, lwork, info )
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: m, n, n1, n2, ldq, ldc, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(in) :: q(ldq,*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           integer(ilp) :: i, ldwork, len, lwkopt, nb, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q;
           ! nw is the minimum dimension of work.
           if( left ) then
              nq = m
           else
              nq = n
           end if
           nw = nq
           if( n1==0_ilp .or. n2==0_ilp ) nw = 1_ilp
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) )&
                     then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( n1<0_ilp .or. n1+n2/=nq ) then
              info = -5_ilp
           else if( n2<0_ilp ) then
              info = -6_ilp
           else if( ldq<max( 1_ilp, nq ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = m*n
              work( 1_ilp ) = real( lwkopt,KIND=sp)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORM22', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ! degenerate cases (n1 = 0 or n2 = 0) are handled using stdlib_strmm.
           if( n1==0_ilp ) then
              call stdlib_strmm( side, 'UPPER', trans, 'NON-UNIT', m, n, one,q, ldq, c, ldc )
                        
              work( 1_ilp ) = one
              return
           else if( n2==0_ilp ) then
              call stdlib_strmm( side, 'LOWER', trans, 'NON-UNIT', m, n, one,q, ldq, c, ldc )
                        
              work( 1_ilp ) = one
              return
           end if
           ! compute the largest chunk size available from the workspace.
           nb = max( 1_ilp, min( lwork, lwkopt ) / nq )
           if( left ) then
              if( notran ) then
                 do i = 1, n, nb
                    len = min( nb, n-i+1 )
                    ldwork = m
                    ! multiply bottom part of c by q12.
                    call stdlib_slacpy( 'ALL', n1, len, c( n2+1, i ), ldc, work,ldwork )
                    call stdlib_strmm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'NON-UNIT',n1, len, one, &
                              q( 1_ilp, n2+1 ), ldq, work,ldwork )
                    ! multiply top part of c by q11.
                    call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', n1, len, n2,one, q, ldq, c(&
                               1_ilp, i ), ldc, one, work,ldwork )
                    ! multiply top part of c by q21.
                    call stdlib_slacpy( 'ALL', n2, len, c( 1_ilp, i ), ldc,work( n1+1 ), ldwork )
                              
                    call stdlib_strmm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT',n2, len, one, &
                              q( n1+1, 1_ilp ), ldq,work( n1+1 ), ldwork )
                    ! multiply bottom part of c by q22.
                    call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', n2, len, n1,one, q( n1+1, &
                              n2+1 ), ldq, c( n2+1, i ), ldc,one, work( n1+1 ), ldwork )
                    ! copy everything back.
                    call stdlib_slacpy( 'ALL', m, len, work, ldwork, c( 1_ilp, i ),ldc )
                 end do
              else
                 do i = 1, n, nb
                    len = min( nb, n-i+1 )
                    ldwork = m
                    ! multiply bottom part of c by q21**t.
                    call stdlib_slacpy( 'ALL', n2, len, c( n1+1, i ), ldc, work,ldwork )
                    call stdlib_strmm( 'LEFT', 'UPPER', 'TRANSPOSE', 'NON-UNIT',n2, len, one, q( &
                              n1+1, 1_ilp ), ldq, work,ldwork )
                    ! multiply top part of c by q11**t.
                    call stdlib_sgemm( 'TRANSPOSE', 'NO TRANSPOSE', n2, len, n1,one, q, ldq, c( 1_ilp,&
                               i ), ldc, one, work,ldwork )
                    ! multiply top part of c by q12**t.
                    call stdlib_slacpy( 'ALL', n1, len, c( 1_ilp, i ), ldc,work( n2+1 ), ldwork )
                              
                    call stdlib_strmm( 'LEFT', 'LOWER', 'TRANSPOSE', 'NON-UNIT',n1, len, one, q( &
                              1_ilp, n2+1 ), ldq,work( n2+1 ), ldwork )
                    ! multiply bottom part of c by q22**t.
                    call stdlib_sgemm( 'TRANSPOSE', 'NO TRANSPOSE', n1, len, n2,one, q( n1+1, n2+&
                              1_ilp ), ldq, c( n1+1, i ), ldc,one, work( n2+1 ), ldwork )
                    ! copy everything back.
                    call stdlib_slacpy( 'ALL', m, len, work, ldwork, c( 1_ilp, i ),ldc )
                 end do
              end if
           else
              if( notran ) then
                 do i = 1, m, nb
                    len = min( nb, m-i+1 )
                    ldwork = len
                    ! multiply right part of c by q21.
                    call stdlib_slacpy( 'ALL', len, n2, c( i, n1+1 ), ldc, work,ldwork )
                    call stdlib_strmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT',len, n2, one, &
                              q( n1+1, 1_ilp ), ldq, work,ldwork )
                    ! multiply left part of c by q11.
                    call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', len, n2, n1,one, c( i, 1_ilp ),&
                               ldc, q, ldq, one, work,ldwork )
                    ! multiply left part of c by q12.
                    call stdlib_slacpy( 'ALL', len, n1, c( i, 1_ilp ), ldc,work( 1_ilp + n2*ldwork ), &
                              ldwork )
                    call stdlib_strmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'NON-UNIT',len, n1, one, &
                              q( 1_ilp, n2+1 ), ldq,work( 1_ilp + n2*ldwork ), ldwork )
                    ! multiply right part of c by q22.
                    call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', len, n1, n2,one, c( i, n1+&
                              1_ilp ), ldc, q( n1+1, n2+1 ), ldq,one, work( 1_ilp + n2*ldwork ), ldwork )
                    ! copy everything back.
                    call stdlib_slacpy( 'ALL', len, n, work, ldwork, c( i, 1_ilp ),ldc )
                 end do
              else
                 do i = 1, m, nb
                    len = min( nb, m-i+1 )
                    ldwork = len
                    ! multiply right part of c by q12**t.
                    call stdlib_slacpy( 'ALL', len, n1, c( i, n2+1 ), ldc, work,ldwork )
                    call stdlib_strmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'NON-UNIT',len, n1, one, q( &
                              1_ilp, n2+1 ), ldq, work,ldwork )
                    ! multiply left part of c by q11**t.
                    call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', len, n1, n2,one, c( i, 1_ilp ), &
                              ldc, q, ldq, one, work,ldwork )
                    ! multiply left part of c by q21**t.
                    call stdlib_slacpy( 'ALL', len, n2, c( i, 1_ilp ), ldc,work( 1_ilp + n1*ldwork ), &
                              ldwork )
                    call stdlib_strmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'NON-UNIT',len, n2, one, q( &
                              n1+1, 1_ilp ), ldq,work( 1_ilp + n1*ldwork ), ldwork )
                    ! multiply right part of c by q22**t.
                    call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', len, n2, n1,one, c( i, n2+1 ),&
                               ldc, q( n1+1, n2+1 ), ldq,one, work( 1_ilp + n1*ldwork ), ldwork )
                    ! copy everything back.
                    call stdlib_slacpy( 'ALL', len, n, work, ldwork, c( i, 1_ilp ),ldc )
                 end do
              end if
           end if
           work( 1_ilp ) = real( lwkopt,KIND=sp)
           return
     end subroutine stdlib_sorm22

     pure module subroutine stdlib_dorm22( side, trans, m, n, n1, n2, q, ldq, c, ldc,work, lwork, info )
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: m, n, n1, n2, ldq, ldc, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(in) :: q(ldq,*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: left, lquery, notran
           integer(ilp) :: i, ldwork, len, lwkopt, nb, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q;
           ! nw is the minimum dimension of work.
           if( left ) then
              nq = m
           else
              nq = n
           end if
           nw = nq
           if( n1==0_ilp .or. n2==0_ilp ) nw = 1_ilp
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) )&
                     then
              info = -2_ilp
           else if( m<0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( n1<0_ilp .or. n1+n2/=nq ) then
              info = -5_ilp
           else if( n2<0_ilp ) then
              info = -6_ilp
           else if( ldq<max( 1_ilp, nq ) ) then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              lwkopt = m*n
              work( 1_ilp ) = real( lwkopt,KIND=dp)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORM22', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ! degenerate cases (n1 = 0 or n2 = 0) are handled using stdlib_dtrmm.
           if( n1==0_ilp ) then
              call stdlib_dtrmm( side, 'UPPER', trans, 'NON-UNIT', m, n, one,q, ldq, c, ldc )
                        
              work( 1_ilp ) = one
              return
           else if( n2==0_ilp ) then
              call stdlib_dtrmm( side, 'LOWER', trans, 'NON-UNIT', m, n, one,q, ldq, c, ldc )
                        
              work( 1_ilp ) = one
              return
           end if
           ! compute the largest chunk size available from the workspace.
           nb = max( 1_ilp, min( lwork, lwkopt ) / nq )
           if( left ) then
              if( notran ) then
                 do i = 1, n, nb
                    len = min( nb, n-i+1 )
                    ldwork = m
                    ! multiply bottom part of c by q12.
                    call stdlib_dlacpy( 'ALL', n1, len, c( n2+1, i ), ldc, work,ldwork )
                    call stdlib_dtrmm( 'LEFT', 'LOWER', 'NO TRANSPOSE', 'NON-UNIT',n1, len, one, &
                              q( 1_ilp, n2+1 ), ldq, work,ldwork )
                    ! multiply top part of c by q11.
                    call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', n1, len, n2,one, q, ldq, c(&
                               1_ilp, i ), ldc, one, work,ldwork )
                    ! multiply top part of c by q21.
                    call stdlib_dlacpy( 'ALL', n2, len, c( 1_ilp, i ), ldc,work( n1+1 ), ldwork )
                              
                    call stdlib_dtrmm( 'LEFT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT',n2, len, one, &
                              q( n1+1, 1_ilp ), ldq,work( n1+1 ), ldwork )
                    ! multiply bottom part of c by q22.
                    call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', n2, len, n1,one, q( n1+1, &
                              n2+1 ), ldq, c( n2+1, i ), ldc,one, work( n1+1 ), ldwork )
                    ! copy everything back.
                    call stdlib_dlacpy( 'ALL', m, len, work, ldwork, c( 1_ilp, i ),ldc )
                 end do
              else
                 do i = 1, n, nb
                    len = min( nb, n-i+1 )
                    ldwork = m
                    ! multiply bottom part of c by q21**t.
                    call stdlib_dlacpy( 'ALL', n2, len, c( n1+1, i ), ldc, work,ldwork )
                    call stdlib_dtrmm( 'LEFT', 'UPPER', 'TRANSPOSE', 'NON-UNIT',n2, len, one, q( &
                              n1+1, 1_ilp ), ldq, work,ldwork )
                    ! multiply top part of c by q11**t.
                    call stdlib_dgemm( 'TRANSPOSE', 'NO TRANSPOSE', n2, len, n1,one, q, ldq, c( 1_ilp,&
                               i ), ldc, one, work,ldwork )
                    ! multiply top part of c by q12**t.
                    call stdlib_dlacpy( 'ALL', n1, len, c( 1_ilp, i ), ldc,work( n2+1 ), ldwork )
                              
                    call stdlib_dtrmm( 'LEFT', 'LOWER', 'TRANSPOSE', 'NON-UNIT',n1, len, one, q( &
                              1_ilp, n2+1 ), ldq,work( n2+1 ), ldwork )
                    ! multiply bottom part of c by q22**t.
                    call stdlib_dgemm( 'TRANSPOSE', 'NO TRANSPOSE', n1, len, n2,one, q( n1+1, n2+&
                              1_ilp ), ldq, c( n1+1, i ), ldc,one, work( n2+1 ), ldwork )
                    ! copy everything back.
                    call stdlib_dlacpy( 'ALL', m, len, work, ldwork, c( 1_ilp, i ),ldc )
                 end do
              end if
           else
              if( notran ) then
                 do i = 1, m, nb
                    len = min( nb, m-i+1 )
                    ldwork = len
                    ! multiply right part of c by q21.
                    call stdlib_dlacpy( 'ALL', len, n2, c( i, n1+1 ), ldc, work,ldwork )
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'NON-UNIT',len, n2, one, &
                              q( n1+1, 1_ilp ), ldq, work,ldwork )
                    ! multiply left part of c by q11.
                    call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', len, n2, n1,one, c( i, 1_ilp ),&
                               ldc, q, ldq, one, work,ldwork )
                    ! multiply left part of c by q12.
                    call stdlib_dlacpy( 'ALL', len, n1, c( i, 1_ilp ), ldc,work( 1_ilp + n2*ldwork ), &
                              ldwork )
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'NON-UNIT',len, n1, one, &
                              q( 1_ilp, n2+1 ), ldq,work( 1_ilp + n2*ldwork ), ldwork )
                    ! multiply right part of c by q22.
                    call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', len, n1, n2,one, c( i, n1+&
                              1_ilp ), ldc, q( n1+1, n2+1 ), ldq,one, work( 1_ilp + n2*ldwork ), ldwork )
                    ! copy everything back.
                    call stdlib_dlacpy( 'ALL', len, n, work, ldwork, c( i, 1_ilp ),ldc )
                 end do
              else
                 do i = 1, m, nb
                    len = min( nb, m-i+1 )
                    ldwork = len
                    ! multiply right part of c by q12**t.
                    call stdlib_dlacpy( 'ALL', len, n1, c( i, n2+1 ), ldc, work,ldwork )
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'NON-UNIT',len, n1, one, q( &
                              1_ilp, n2+1 ), ldq, work,ldwork )
                    ! multiply left part of c by q11**t.
                    call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', len, n1, n2,one, c( i, 1_ilp ), &
                              ldc, q, ldq, one, work,ldwork )
                    ! multiply left part of c by q21**t.
                    call stdlib_dlacpy( 'ALL', len, n2, c( i, 1_ilp ), ldc,work( 1_ilp + n1*ldwork ), &
                              ldwork )
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'NON-UNIT',len, n2, one, q( &
                              n1+1, 1_ilp ), ldq,work( 1_ilp + n1*ldwork ), ldwork )
                    ! multiply right part of c by q22**t.
                    call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', len, n2, n1,one, c( i, n2+1 ),&
                               ldc, q( n1+1, n2+1 ), ldq,one, work( 1_ilp + n1*ldwork ), ldwork )
                    ! copy everything back.
                    call stdlib_dlacpy( 'ALL', len, n, work, ldwork, c( i, 1_ilp ),ldc )
                 end do
              end if
           end if
           work( 1_ilp ) = real( lwkopt,KIND=dp)
           return
     end subroutine stdlib_dorm22




     pure module subroutine stdlib_sdisna( job, m, n, d, sep, info )
     !! SDISNA computes the reciprocal condition numbers for the eigenvectors
     !! of a real symmetric or complex Hermitian matrix or for the left or
     !! right singular vectors of a general m-by-n matrix. The reciprocal
     !! condition number is the 'gap' between the corresponding eigenvalue or
     !! singular value and the nearest other one.
     !! The bound on the error, measured by angle in radians, in the I-th
     !! computed vector is given by
     !! SLAMCH( 'E' ) * ( ANORM / SEP( I ) )
     !! where ANORM = 2-norm(A) = max( abs( D(j) ) ).  SEP(I) is not allowed
     !! to be smaller than SLAMCH( 'E' )*ANORM in order to limit the size of
     !! the error bound.
     !! SDISNA may also be used to compute error bounds for eigenvectors of
     !! the generalized symmetric definite eigenproblem.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: m, n
           ! Array Arguments 
           real(sp), intent(in) :: d(*)
           real(sp), intent(out) :: sep(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: decr, eigen, incr, left, right, sing
           integer(ilp) :: i, k
           real(sp) :: anorm, eps, newgap, oldgap, safmin, thresh
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           eigen = stdlib_lsame( job, 'E' )
           left = stdlib_lsame( job, 'L' )
           right = stdlib_lsame( job, 'R' )
           sing = left .or. right
           if( eigen ) then
              k = m
           else if( sing ) then
              k = min( m, n )
           end if
           if( .not.eigen .and. .not.sing ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( k<0_ilp ) then
              info = -3_ilp
           else
              incr = .true.
              decr = .true.
              do i = 1, k - 1
                 if( incr )incr = incr .and. d( i )<=d( i+1 )
                 if( decr )decr = decr .and. d( i )>=d( i+1 )
              end do
              if( sing .and. k>0_ilp ) then
                 if( incr )incr = incr .and. zero<=d( 1_ilp )
                 if( decr )decr = decr .and. d( k )>=zero
              end if
              if( .not.( incr .or. decr ) )info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SDISNA', -info )
              return
           end if
           ! quick return if possible
           if( k==0 )return
           ! compute reciprocal condition numbers
           if( k==1_ilp ) then
              sep( 1_ilp ) = stdlib_slamch( 'O' )
           else
              oldgap = abs( d( 2_ilp )-d( 1_ilp ) )
              sep( 1_ilp ) = oldgap
              do i = 2, k - 1
                 newgap = abs( d( i+1 )-d( i ) )
                 sep( i ) = min( oldgap, newgap )
                 oldgap = newgap
              end do
              sep( k ) = oldgap
           end if
           if( sing ) then
              if( ( left .and. m>n ) .or. ( right .and. m<n ) ) then
                 if( incr )sep( 1_ilp ) = min( sep( 1_ilp ), d( 1_ilp ) )
                 if( decr )sep( k ) = min( sep( k ), d( k ) )
              end if
           end if
           ! ensure that reciprocal condition numbers are not less than
           ! threshold, in order to limit the size of the error bound
           eps = stdlib_slamch( 'E' )
           safmin = stdlib_slamch( 'S' )
           anorm = max( abs( d( 1_ilp ) ), abs( d( k ) ) )
           if( anorm==zero ) then
              thresh = eps
           else
              thresh = max( eps*anorm, safmin )
           end if
           do i = 1, k
              sep( i ) = max( sep( i ), thresh )
           end do
           return
     end subroutine stdlib_sdisna

     pure module subroutine stdlib_ddisna( job, m, n, d, sep, info )
     !! DDISNA computes the reciprocal condition numbers for the eigenvectors
     !! of a real symmetric or complex Hermitian matrix or for the left or
     !! right singular vectors of a general m-by-n matrix. The reciprocal
     !! condition number is the 'gap' between the corresponding eigenvalue or
     !! singular value and the nearest other one.
     !! The bound on the error, measured by angle in radians, in the I-th
     !! computed vector is given by
     !! DLAMCH( 'E' ) * ( ANORM / SEP( I ) )
     !! where ANORM = 2-norm(A) = max( abs( D(j) ) ).  SEP(I) is not allowed
     !! to be smaller than DLAMCH( 'E' )*ANORM in order to limit the size of
     !! the error bound.
     !! DDISNA may also be used to compute error bounds for eigenvectors of
     !! the generalized symmetric definite eigenproblem.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: m, n
           ! Array Arguments 
           real(dp), intent(in) :: d(*)
           real(dp), intent(out) :: sep(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: decr, eigen, incr, left, right, sing
           integer(ilp) :: i, k
           real(dp) :: anorm, eps, newgap, oldgap, safmin, thresh
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           eigen = stdlib_lsame( job, 'E' )
           left = stdlib_lsame( job, 'L' )
           right = stdlib_lsame( job, 'R' )
           sing = left .or. right
           if( eigen ) then
              k = m
           else if( sing ) then
              k = min( m, n )
           end if
           if( .not.eigen .and. .not.sing ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( k<0_ilp ) then
              info = -3_ilp
           else
              incr = .true.
              decr = .true.
              do i = 1, k - 1
                 if( incr )incr = incr .and. d( i )<=d( i+1 )
                 if( decr )decr = decr .and. d( i )>=d( i+1 )
              end do
              if( sing .and. k>0_ilp ) then
                 if( incr )incr = incr .and. zero<=d( 1_ilp )
                 if( decr )decr = decr .and. d( k )>=zero
              end if
              if( .not.( incr .or. decr ) )info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DDISNA', -info )
              return
           end if
           ! quick return if possible
           if( k==0 )return
           ! compute reciprocal condition numbers
           if( k==1_ilp ) then
              sep( 1_ilp ) = stdlib_dlamch( 'O' )
           else
              oldgap = abs( d( 2_ilp )-d( 1_ilp ) )
              sep( 1_ilp ) = oldgap
              do i = 2, k - 1
                 newgap = abs( d( i+1 )-d( i ) )
                 sep( i ) = min( oldgap, newgap )
                 oldgap = newgap
              end do
              sep( k ) = oldgap
           end if
           if( sing ) then
              if( ( left .and. m>n ) .or. ( right .and. m<n ) ) then
                 if( incr )sep( 1_ilp ) = min( sep( 1_ilp ), d( 1_ilp ) )
                 if( decr )sep( k ) = min( sep( k ), d( k ) )
              end if
           end if
           ! ensure that reciprocal condition numbers are not less than
           ! threshold, in order to limit the size of the error bound
           eps = stdlib_dlamch( 'E' )
           safmin = stdlib_dlamch( 'S' )
           anorm = max( abs( d( 1_ilp ) ), abs( d( k ) ) )
           if( anorm==zero ) then
              thresh = eps
           else
              thresh = max( eps*anorm, safmin )
           end if
           do i = 1, k
              sep( i ) = max( sep( i ), thresh )
           end do
           return
     end subroutine stdlib_ddisna




     pure module subroutine stdlib_slatrd( uplo, n, nb, a, lda, e, tau, w, ldw )
     !! SLATRD reduces NB rows and columns of a real symmetric matrix A to
     !! symmetric tridiagonal form by an orthogonal similarity
     !! transformation Q**T * A * Q, and returns the matrices V and W which are
     !! needed to apply the transformation to the unreduced part of A.
     !! If UPLO = 'U', SLATRD reduces the last NB rows and columns of a
     !! matrix, of which the upper triangle is supplied;
     !! if UPLO = 'L', SLATRD reduces the first NB rows and columns of a
     !! matrix, of which the lower triangle is supplied.
     !! This is an auxiliary routine called by SSYTRD.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: e(*), tau(*), w(ldw,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iw
           real(sp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0 )return
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! reduce last nb columns of upper triangle
              loop_10: do i = n, n - nb + 1, -1
                 iw = i - n + nb
                 if( i<n ) then
                    ! update a(1:i,i)
                    call stdlib_sgemv( 'NO TRANSPOSE', i, n-i, -one, a( 1_ilp, i+1 ),lda, w( i, iw+1 )&
                              , ldw, one, a( 1_ilp, i ), 1_ilp )
                    call stdlib_sgemv( 'NO TRANSPOSE', i, n-i, -one, w( 1_ilp, iw+1 ),ldw, a( i, i+1 )&
                              , lda, one, a( 1_ilp, i ), 1_ilp )
                 end if
                 if( i>1_ilp ) then
                    ! generate elementary reflector h(i) to annihilate
                    ! a(1:i-2,i)
                    call stdlib_slarfg( i-1, a( i-1, i ), a( 1_ilp, i ), 1_ilp, tau( i-1 ) )
                    e( i-1 ) = a( i-1, i )
                    a( i-1, i ) = one
                    ! compute w(1:i-1,i)
                    call stdlib_ssymv( 'UPPER', i-1, one, a, lda, a( 1_ilp, i ), 1_ilp,zero, w( 1_ilp, iw ), &
                              1_ilp )
                    if( i<n ) then
                       call stdlib_sgemv( 'TRANSPOSE', i-1, n-i, one, w( 1_ilp, iw+1 ),ldw, a( 1_ilp, i ),&
                                  1_ilp, zero, w( i+1, iw ), 1_ilp )
                       call stdlib_sgemv( 'NO TRANSPOSE', i-1, n-i, -one,a( 1_ilp, i+1 ), lda, w( i+1,&
                                  iw ), 1_ilp, one,w( 1_ilp, iw ), 1_ilp )
                       call stdlib_sgemv( 'TRANSPOSE', i-1, n-i, one, a( 1_ilp, i+1 ),lda, a( 1_ilp, i ), &
                                 1_ilp, zero, w( i+1, iw ), 1_ilp )
                       call stdlib_sgemv( 'NO TRANSPOSE', i-1, n-i, -one,w( 1_ilp, iw+1 ), ldw, w( i+&
                                 1_ilp, iw ), 1_ilp, one,w( 1_ilp, iw ), 1_ilp )
                    end if
                    call stdlib_sscal( i-1, tau( i-1 ), w( 1_ilp, iw ), 1_ilp )
                    alpha = -half*tau( i-1 )*stdlib_sdot( i-1, w( 1_ilp, iw ), 1_ilp,a( 1_ilp, i ), 1_ilp )
                              
                    call stdlib_saxpy( i-1, alpha, a( 1_ilp, i ), 1_ilp, w( 1_ilp, iw ), 1_ilp )
                 end if
              end do loop_10
           else
              ! reduce first nb columns of lower triangle
              do i = 1, nb
                 ! update a(i:n,i)
                 call stdlib_sgemv( 'NO TRANSPOSE', n-i+1, i-1, -one, a( i, 1_ilp ),lda, w( i, 1_ilp ), &
                           ldw, one, a( i, i ), 1_ilp )
                 call stdlib_sgemv( 'NO TRANSPOSE', n-i+1, i-1, -one, w( i, 1_ilp ),ldw, a( i, 1_ilp ), &
                           lda, one, a( i, i ), 1_ilp )
                 if( i<n ) then
                    ! generate elementary reflector h(i) to annihilate
                    ! a(i+2:n,i)
                    call stdlib_slarfg( n-i, a( i+1, i ), a( min( i+2, n ), i ), 1_ilp,tau( i ) )
                              
                    e( i ) = a( i+1, i )
                    a( i+1, i ) = one
                    ! compute w(i+1:n,i)
                    call stdlib_ssymv( 'LOWER', n-i, one, a( i+1, i+1 ), lda,a( i+1, i ), 1_ilp, zero,&
                               w( i+1, i ), 1_ilp )
                    call stdlib_sgemv( 'TRANSPOSE', n-i, i-1, one, w( i+1, 1_ilp ), ldw,a( i+1, i ), &
                              1_ilp, zero, w( 1_ilp, i ), 1_ilp )
                    call stdlib_sgemv( 'NO TRANSPOSE', n-i, i-1, -one, a( i+1, 1_ilp ),lda, w( 1_ilp, i ),&
                               1_ilp, one, w( i+1, i ), 1_ilp )
                    call stdlib_sgemv( 'TRANSPOSE', n-i, i-1, one, a( i+1, 1_ilp ), lda,a( i+1, i ), &
                              1_ilp, zero, w( 1_ilp, i ), 1_ilp )
                    call stdlib_sgemv( 'NO TRANSPOSE', n-i, i-1, -one, w( i+1, 1_ilp ),ldw, w( 1_ilp, i ),&
                               1_ilp, one, w( i+1, i ), 1_ilp )
                    call stdlib_sscal( n-i, tau( i ), w( i+1, i ), 1_ilp )
                    alpha = -half*tau( i )*stdlib_sdot( n-i, w( i+1, i ), 1_ilp,a( i+1, i ), 1_ilp )
                              
                    call stdlib_saxpy( n-i, alpha, a( i+1, i ), 1_ilp, w( i+1, i ), 1_ilp )
                 end if
              end do
           end if
           return
     end subroutine stdlib_slatrd

     pure module subroutine stdlib_dlatrd( uplo, n, nb, a, lda, e, tau, w, ldw )
     !! DLATRD reduces NB rows and columns of a real symmetric matrix A to
     !! symmetric tridiagonal form by an orthogonal similarity
     !! transformation Q**T * A * Q, and returns the matrices V and W which are
     !! needed to apply the transformation to the unreduced part of A.
     !! If UPLO = 'U', DLATRD reduces the last NB rows and columns of a
     !! matrix, of which the upper triangle is supplied;
     !! if UPLO = 'L', DLATRD reduces the first NB rows and columns of a
     !! matrix, of which the lower triangle is supplied.
     !! This is an auxiliary routine called by DSYTRD.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: e(*), tau(*), w(ldw,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iw
           real(dp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0 )return
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! reduce last nb columns of upper triangle
              loop_10: do i = n, n - nb + 1, -1
                 iw = i - n + nb
                 if( i<n ) then
                    ! update a(1:i,i)
                    call stdlib_dgemv( 'NO TRANSPOSE', i, n-i, -one, a( 1_ilp, i+1 ),lda, w( i, iw+1 )&
                              , ldw, one, a( 1_ilp, i ), 1_ilp )
                    call stdlib_dgemv( 'NO TRANSPOSE', i, n-i, -one, w( 1_ilp, iw+1 ),ldw, a( i, i+1 )&
                              , lda, one, a( 1_ilp, i ), 1_ilp )
                 end if
                 if( i>1_ilp ) then
                    ! generate elementary reflector h(i) to annihilate
                    ! a(1:i-2,i)
                    call stdlib_dlarfg( i-1, a( i-1, i ), a( 1_ilp, i ), 1_ilp, tau( i-1 ) )
                    e( i-1 ) = a( i-1, i )
                    a( i-1, i ) = one
                    ! compute w(1:i-1,i)
                    call stdlib_dsymv( 'UPPER', i-1, one, a, lda, a( 1_ilp, i ), 1_ilp,zero, w( 1_ilp, iw ), &
                              1_ilp )
                    if( i<n ) then
                       call stdlib_dgemv( 'TRANSPOSE', i-1, n-i, one, w( 1_ilp, iw+1 ),ldw, a( 1_ilp, i ),&
                                  1_ilp, zero, w( i+1, iw ), 1_ilp )
                       call stdlib_dgemv( 'NO TRANSPOSE', i-1, n-i, -one,a( 1_ilp, i+1 ), lda, w( i+1,&
                                  iw ), 1_ilp, one,w( 1_ilp, iw ), 1_ilp )
                       call stdlib_dgemv( 'TRANSPOSE', i-1, n-i, one, a( 1_ilp, i+1 ),lda, a( 1_ilp, i ), &
                                 1_ilp, zero, w( i+1, iw ), 1_ilp )
                       call stdlib_dgemv( 'NO TRANSPOSE', i-1, n-i, -one,w( 1_ilp, iw+1 ), ldw, w( i+&
                                 1_ilp, iw ), 1_ilp, one,w( 1_ilp, iw ), 1_ilp )
                    end if
                    call stdlib_dscal( i-1, tau( i-1 ), w( 1_ilp, iw ), 1_ilp )
                    alpha = -half*tau( i-1 )*stdlib_ddot( i-1, w( 1_ilp, iw ), 1_ilp,a( 1_ilp, i ), 1_ilp )
                              
                    call stdlib_daxpy( i-1, alpha, a( 1_ilp, i ), 1_ilp, w( 1_ilp, iw ), 1_ilp )
                 end if
              end do loop_10
           else
              ! reduce first nb columns of lower triangle
              do i = 1, nb
                 ! update a(i:n,i)
                 call stdlib_dgemv( 'NO TRANSPOSE', n-i+1, i-1, -one, a( i, 1_ilp ),lda, w( i, 1_ilp ), &
                           ldw, one, a( i, i ), 1_ilp )
                 call stdlib_dgemv( 'NO TRANSPOSE', n-i+1, i-1, -one, w( i, 1_ilp ),ldw, a( i, 1_ilp ), &
                           lda, one, a( i, i ), 1_ilp )
                 if( i<n ) then
                    ! generate elementary reflector h(i) to annihilate
                    ! a(i+2:n,i)
                    call stdlib_dlarfg( n-i, a( i+1, i ), a( min( i+2, n ), i ), 1_ilp,tau( i ) )
                              
                    e( i ) = a( i+1, i )
                    a( i+1, i ) = one
                    ! compute w(i+1:n,i)
                    call stdlib_dsymv( 'LOWER', n-i, one, a( i+1, i+1 ), lda,a( i+1, i ), 1_ilp, zero,&
                               w( i+1, i ), 1_ilp )
                    call stdlib_dgemv( 'TRANSPOSE', n-i, i-1, one, w( i+1, 1_ilp ), ldw,a( i+1, i ), &
                              1_ilp, zero, w( 1_ilp, i ), 1_ilp )
                    call stdlib_dgemv( 'NO TRANSPOSE', n-i, i-1, -one, a( i+1, 1_ilp ),lda, w( 1_ilp, i ),&
                               1_ilp, one, w( i+1, i ), 1_ilp )
                    call stdlib_dgemv( 'TRANSPOSE', n-i, i-1, one, a( i+1, 1_ilp ), lda,a( i+1, i ), &
                              1_ilp, zero, w( 1_ilp, i ), 1_ilp )
                    call stdlib_dgemv( 'NO TRANSPOSE', n-i, i-1, -one, w( i+1, 1_ilp ),ldw, w( 1_ilp, i ),&
                               1_ilp, one, w( i+1, i ), 1_ilp )
                    call stdlib_dscal( n-i, tau( i ), w( i+1, i ), 1_ilp )
                    alpha = -half*tau( i )*stdlib_ddot( n-i, w( i+1, i ), 1_ilp,a( i+1, i ), 1_ilp )
                              
                    call stdlib_daxpy( n-i, alpha, a( i+1, i ), 1_ilp, w( i+1, i ), 1_ilp )
                 end if
              end do
           end if
           return
     end subroutine stdlib_dlatrd


     pure module subroutine stdlib_clatrd( uplo, n, nb, a, lda, e, tau, w, ldw )
     !! CLATRD reduces NB rows and columns of a complex Hermitian matrix A to
     !! Hermitian tridiagonal form by a unitary similarity
     !! transformation Q**H * A * Q, and returns the matrices V and W which are
     !! needed to apply the transformation to the unreduced part of A.
     !! If UPLO = 'U', CLATRD reduces the last NB rows and columns of a
     !! matrix, of which the upper triangle is supplied;
     !! if UPLO = 'L', CLATRD reduces the first NB rows and columns of a
     !! matrix, of which the lower triangle is supplied.
     !! This is an auxiliary routine called by CHETRD.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           real(sp), intent(out) :: e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), w(ldw,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iw
           complex(sp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0 )return
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! reduce last nb columns of upper triangle
              loop_10: do i = n, n - nb + 1, -1
                 iw = i - n + nb
                 if( i<n ) then
                    ! update a(1:i,i)
                    a( i, i ) = real( a( i, i ),KIND=sp)
                    call stdlib_clacgv( n-i, w( i, iw+1 ), ldw )
                    call stdlib_cgemv( 'NO TRANSPOSE', i, n-i, -cone, a( 1_ilp, i+1 ),lda, w( i, iw+1 &
                              ), ldw, cone, a( 1_ilp, i ), 1_ilp )
                    call stdlib_clacgv( n-i, w( i, iw+1 ), ldw )
                    call stdlib_clacgv( n-i, a( i, i+1 ), lda )
                    call stdlib_cgemv( 'NO TRANSPOSE', i, n-i, -cone, w( 1_ilp, iw+1 ),ldw, a( i, i+1 &
                              ), lda, cone, a( 1_ilp, i ), 1_ilp )
                    call stdlib_clacgv( n-i, a( i, i+1 ), lda )
                    a( i, i ) = real( a( i, i ),KIND=sp)
                 end if
                 if( i>1_ilp ) then
                    ! generate elementary reflector h(i) to annihilate
                    ! a(1:i-2,i)
                    alpha = a( i-1, i )
                    call stdlib_clarfg( i-1, alpha, a( 1_ilp, i ), 1_ilp, tau( i-1 ) )
                    e( i-1 ) = real( alpha,KIND=sp)
                    a( i-1, i ) = cone
                    ! compute w(1:i-1,i)
                    call stdlib_chemv( 'UPPER', i-1, cone, a, lda, a( 1_ilp, i ), 1_ilp,czero, w( 1_ilp, iw ),&
                               1_ilp )
                    if( i<n ) then
                       call stdlib_cgemv( 'CONJUGATE TRANSPOSE', i-1, n-i, cone,w( 1_ilp, iw+1 ), ldw,&
                                  a( 1_ilp, i ), 1_ilp, czero,w( i+1, iw ), 1_ilp )
                       call stdlib_cgemv( 'NO TRANSPOSE', i-1, n-i, -cone,a( 1_ilp, i+1 ), lda, w( i+&
                                 1_ilp, iw ), 1_ilp, cone,w( 1_ilp, iw ), 1_ilp )
                       call stdlib_cgemv( 'CONJUGATE TRANSPOSE', i-1, n-i, cone,a( 1_ilp, i+1 ), lda, &
                                 a( 1_ilp, i ), 1_ilp, czero,w( i+1, iw ), 1_ilp )
                       call stdlib_cgemv( 'NO TRANSPOSE', i-1, n-i, -cone,w( 1_ilp, iw+1 ), ldw, w( i+&
                                 1_ilp, iw ), 1_ilp, cone,w( 1_ilp, iw ), 1_ilp )
                    end if
                    call stdlib_cscal( i-1, tau( i-1 ), w( 1_ilp, iw ), 1_ilp )
                    alpha = -chalf*tau( i-1 )*stdlib_cdotc( i-1, w( 1_ilp, iw ), 1_ilp,a( 1_ilp, i ), 1_ilp )
                              
                    call stdlib_caxpy( i-1, alpha, a( 1_ilp, i ), 1_ilp, w( 1_ilp, iw ), 1_ilp )
                 end if
              end do loop_10
           else
              ! reduce first nb columns of lower triangle
              loop_20: do i = 1, nb
                 ! update a(i:n,i)
                 a( i, i ) = real( a( i, i ),KIND=sp)
                 call stdlib_clacgv( i-1, w( i, 1_ilp ), ldw )
                 call stdlib_cgemv( 'NO TRANSPOSE', n-i+1, i-1, -cone, a( i, 1_ilp ),lda, w( i, 1_ilp ), &
                           ldw, cone, a( i, i ), 1_ilp )
                 call stdlib_clacgv( i-1, w( i, 1_ilp ), ldw )
                 call stdlib_clacgv( i-1, a( i, 1_ilp ), lda )
                 call stdlib_cgemv( 'NO TRANSPOSE', n-i+1, i-1, -cone, w( i, 1_ilp ),ldw, a( i, 1_ilp ), &
                           lda, cone, a( i, i ), 1_ilp )
                 call stdlib_clacgv( i-1, a( i, 1_ilp ), lda )
                 a( i, i ) = real( a( i, i ),KIND=sp)
                 if( i<n ) then
                    ! generate elementary reflector h(i) to annihilate
                    ! a(i+2:n,i)
                    alpha = a( i+1, i )
                    call stdlib_clarfg( n-i, alpha, a( min( i+2, n ), i ), 1_ilp,tau( i ) )
                    e( i ) = real( alpha,KIND=sp)
                    a( i+1, i ) = cone
                    ! compute w(i+1:n,i)
                    call stdlib_chemv( 'LOWER', n-i, cone, a( i+1, i+1 ), lda,a( i+1, i ), 1_ilp, &
                              czero, w( i+1, i ), 1_ilp )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-i, i-1, cone,w( i+1, 1_ilp ), ldw, a( &
                              i+1, i ), 1_ilp, czero,w( 1_ilp, i ), 1_ilp )
                    call stdlib_cgemv( 'NO TRANSPOSE', n-i, i-1, -cone, a( i+1, 1_ilp ),lda, w( 1_ilp, i )&
                              , 1_ilp, cone, w( i+1, i ), 1_ilp )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-i, i-1, cone,a( i+1, 1_ilp ), lda, a( &
                              i+1, i ), 1_ilp, czero,w( 1_ilp, i ), 1_ilp )
                    call stdlib_cgemv( 'NO TRANSPOSE', n-i, i-1, -cone, w( i+1, 1_ilp ),ldw, w( 1_ilp, i )&
                              , 1_ilp, cone, w( i+1, i ), 1_ilp )
                    call stdlib_cscal( n-i, tau( i ), w( i+1, i ), 1_ilp )
                    alpha = -chalf*tau( i )*stdlib_cdotc( n-i, w( i+1, i ), 1_ilp,a( i+1, i ), 1_ilp )
                              
                    call stdlib_caxpy( n-i, alpha, a( i+1, i ), 1_ilp, w( i+1, i ), 1_ilp )
                 end if
              end do loop_20
           end if
           return
     end subroutine stdlib_clatrd

     pure module subroutine stdlib_zlatrd( uplo, n, nb, a, lda, e, tau, w, ldw )
     !! ZLATRD reduces NB rows and columns of a complex Hermitian matrix A to
     !! Hermitian tridiagonal form by a unitary similarity
     !! transformation Q**H * A * Q, and returns the matrices V and W which are
     !! needed to apply the transformation to the unreduced part of A.
     !! If UPLO = 'U', ZLATRD reduces the last NB rows and columns of a
     !! matrix, of which the upper triangle is supplied;
     !! if UPLO = 'L', ZLATRD reduces the first NB rows and columns of a
     !! matrix, of which the lower triangle is supplied.
     !! This is an auxiliary routine called by ZHETRD.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldw, n, nb
           ! Array Arguments 
           real(dp), intent(out) :: e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), w(ldw,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iw
           complex(dp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0 )return
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! reduce last nb columns of upper triangle
              loop_10: do i = n, n - nb + 1, -1
                 iw = i - n + nb
                 if( i<n ) then
                    ! update a(1:i,i)
                    a( i, i ) = real( a( i, i ),KIND=dp)
                    call stdlib_zlacgv( n-i, w( i, iw+1 ), ldw )
                    call stdlib_zgemv( 'NO TRANSPOSE', i, n-i, -cone, a( 1_ilp, i+1 ),lda, w( i, iw+1 &
                              ), ldw, cone, a( 1_ilp, i ), 1_ilp )
                    call stdlib_zlacgv( n-i, w( i, iw+1 ), ldw )
                    call stdlib_zlacgv( n-i, a( i, i+1 ), lda )
                    call stdlib_zgemv( 'NO TRANSPOSE', i, n-i, -cone, w( 1_ilp, iw+1 ),ldw, a( i, i+1 &
                              ), lda, cone, a( 1_ilp, i ), 1_ilp )
                    call stdlib_zlacgv( n-i, a( i, i+1 ), lda )
                    a( i, i ) = real( a( i, i ),KIND=dp)
                 end if
                 if( i>1_ilp ) then
                    ! generate elementary reflector h(i) to annihilate
                    ! a(1:i-2,i)
                    alpha = a( i-1, i )
                    call stdlib_zlarfg( i-1, alpha, a( 1_ilp, i ), 1_ilp, tau( i-1 ) )
                    e( i-1 ) = real( alpha,KIND=dp)
                    a( i-1, i ) = cone
                    ! compute w(1:i-1,i)
                    call stdlib_zhemv( 'UPPER', i-1, cone, a, lda, a( 1_ilp, i ), 1_ilp,czero, w( 1_ilp, iw ),&
                               1_ilp )
                    if( i<n ) then
                       call stdlib_zgemv( 'CONJUGATE TRANSPOSE', i-1, n-i, cone,w( 1_ilp, iw+1 ), ldw,&
                                  a( 1_ilp, i ), 1_ilp, czero,w( i+1, iw ), 1_ilp )
                       call stdlib_zgemv( 'NO TRANSPOSE', i-1, n-i, -cone,a( 1_ilp, i+1 ), lda, w( i+&
                                 1_ilp, iw ), 1_ilp, cone,w( 1_ilp, iw ), 1_ilp )
                       call stdlib_zgemv( 'CONJUGATE TRANSPOSE', i-1, n-i, cone,a( 1_ilp, i+1 ), lda, &
                                 a( 1_ilp, i ), 1_ilp, czero,w( i+1, iw ), 1_ilp )
                       call stdlib_zgemv( 'NO TRANSPOSE', i-1, n-i, -cone,w( 1_ilp, iw+1 ), ldw, w( i+&
                                 1_ilp, iw ), 1_ilp, cone,w( 1_ilp, iw ), 1_ilp )
                    end if
                    call stdlib_zscal( i-1, tau( i-1 ), w( 1_ilp, iw ), 1_ilp )
                    alpha = -chalf*tau( i-1 )*stdlib_zdotc( i-1, w( 1_ilp, iw ), 1_ilp,a( 1_ilp, i ), 1_ilp )
                              
                    call stdlib_zaxpy( i-1, alpha, a( 1_ilp, i ), 1_ilp, w( 1_ilp, iw ), 1_ilp )
                 end if
              end do loop_10
           else
              ! reduce first nb columns of lower triangle
              loop_20: do i = 1, nb
                 ! update a(i:n,i)
                 a( i, i ) = real( a( i, i ),KIND=dp)
                 call stdlib_zlacgv( i-1, w( i, 1_ilp ), ldw )
                 call stdlib_zgemv( 'NO TRANSPOSE', n-i+1, i-1, -cone, a( i, 1_ilp ),lda, w( i, 1_ilp ), &
                           ldw, cone, a( i, i ), 1_ilp )
                 call stdlib_zlacgv( i-1, w( i, 1_ilp ), ldw )
                 call stdlib_zlacgv( i-1, a( i, 1_ilp ), lda )
                 call stdlib_zgemv( 'NO TRANSPOSE', n-i+1, i-1, -cone, w( i, 1_ilp ),ldw, a( i, 1_ilp ), &
                           lda, cone, a( i, i ), 1_ilp )
                 call stdlib_zlacgv( i-1, a( i, 1_ilp ), lda )
                 a( i, i ) = real( a( i, i ),KIND=dp)
                 if( i<n ) then
                    ! generate elementary reflector h(i) to annihilate
                    ! a(i+2:n,i)
                    alpha = a( i+1, i )
                    call stdlib_zlarfg( n-i, alpha, a( min( i+2, n ), i ), 1_ilp,tau( i ) )
                    e( i ) = real( alpha,KIND=dp)
                    a( i+1, i ) = cone
                    ! compute w(i+1:n,i)
                    call stdlib_zhemv( 'LOWER', n-i, cone, a( i+1, i+1 ), lda,a( i+1, i ), 1_ilp, &
                              czero, w( i+1, i ), 1_ilp )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-i, i-1, cone,w( i+1, 1_ilp ), ldw, a( &
                              i+1, i ), 1_ilp, czero,w( 1_ilp, i ), 1_ilp )
                    call stdlib_zgemv( 'NO TRANSPOSE', n-i, i-1, -cone, a( i+1, 1_ilp ),lda, w( 1_ilp, i )&
                              , 1_ilp, cone, w( i+1, i ), 1_ilp )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-i, i-1, cone,a( i+1, 1_ilp ), lda, a( &
                              i+1, i ), 1_ilp, czero,w( 1_ilp, i ), 1_ilp )
                    call stdlib_zgemv( 'NO TRANSPOSE', n-i, i-1, -cone, w( i+1, 1_ilp ),ldw, w( 1_ilp, i )&
                              , 1_ilp, cone, w( i+1, i ), 1_ilp )
                    call stdlib_zscal( n-i, tau( i ), w( i+1, i ), 1_ilp )
                    alpha = -chalf*tau( i )*stdlib_zdotc( n-i, w( i+1, i ), 1_ilp,a( i+1, i ), 1_ilp )
                              
                    call stdlib_zaxpy( n-i, alpha, a( i+1, i ), 1_ilp, w( i+1, i ), 1_ilp )
                 end if
              end do loop_20
           end if
           return
     end subroutine stdlib_zlatrd




     pure module subroutine stdlib_slae2( a, b, c, rt1, rt2 )
     !! SLAE2 computes the eigenvalues of a 2-by-2 symmetric matrix
     !! [  A   B  ]
     !! [  B   C  ].
     !! On return, RT1 is the eigenvalue of larger absolute value, and RT2
     !! is the eigenvalue of smaller absolute value.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: a, b, c
           real(sp), intent(out) :: rt1, rt2
       ! =====================================================================
           
           
           
           
           ! Local Scalars 
           real(sp) :: ab, acmn, acmx, adf, df, rt, sm, tb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! compute the eigenvalues
           sm = a + c
           df = a - c
           adf = abs( df )
           tb = b + b
           ab = abs( tb )
           if( abs( a )>abs( c ) ) then
              acmx = a
              acmn = c
           else
              acmx = c
              acmn = a
           end if
           if( adf>ab ) then
              rt = adf*sqrt( one+( ab / adf )**2_ilp )
           else if( adf<ab ) then
              rt = ab*sqrt( one+( adf / ab )**2_ilp )
           else
              ! includes case ab=adf=0
              rt = ab*sqrt( two )
           end if
           if( sm<zero ) then
              rt1 = half*( sm-rt )
              ! order of execution important.
              ! to get fully accurate smaller eigenvalue,
              ! next line needs to be executed in higher precision.
              rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
           else if( sm>zero ) then
              rt1 = half*( sm+rt )
              ! order of execution important.
              ! to get fully accurate smaller eigenvalue,
              ! next line needs to be executed in higher precision.
              rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
           else
              ! includes case rt1 = rt2 = 0
              rt1 = half*rt
              rt2 = -half*rt
           end if
           return
     end subroutine stdlib_slae2

     pure module subroutine stdlib_dlae2( a, b, c, rt1, rt2 )
     !! DLAE2 computes the eigenvalues of a 2-by-2 symmetric matrix
     !! [  A   B  ]
     !! [  B   C  ].
     !! On return, RT1 is the eigenvalue of larger absolute value, and RT2
     !! is the eigenvalue of smaller absolute value.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: a, b, c
           real(dp), intent(out) :: rt1, rt2
       ! =====================================================================
           
           
           
           
           ! Local Scalars 
           real(dp) :: ab, acmn, acmx, adf, df, rt, sm, tb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! compute the eigenvalues
           sm = a + c
           df = a - c
           adf = abs( df )
           tb = b + b
           ab = abs( tb )
           if( abs( a )>abs( c ) ) then
              acmx = a
              acmn = c
           else
              acmx = c
              acmn = a
           end if
           if( adf>ab ) then
              rt = adf*sqrt( one+( ab / adf )**2_ilp )
           else if( adf<ab ) then
              rt = ab*sqrt( one+( adf / ab )**2_ilp )
           else
              ! includes case ab=adf=0
              rt = ab*sqrt( two )
           end if
           if( sm<zero ) then
              rt1 = half*( sm-rt )
              ! order of execution important.
              ! to get fully accurate smaller eigenvalue,
              ! next line needs to be executed in higher precision.
              rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
           else if( sm>zero ) then
              rt1 = half*( sm+rt )
              ! order of execution important.
              ! to get fully accurate smaller eigenvalue,
              ! next line needs to be executed in higher precision.
              rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
           else
              ! includes case rt1 = rt2 = 0
              rt1 = half*rt
              rt2 = -half*rt
           end if
           return
     end subroutine stdlib_dlae2




     pure module subroutine stdlib_claesy( a, b, c, rt1, rt2, evscal, cs1, sn1 )
     !! CLAESY computes the eigendecomposition of a 2-by-2 symmetric matrix
     !! ( ( A, B );( B, C ) )
     !! provided the norm of the matrix of eigenvectors is larger than
     !! some threshold value.
     !! RT1 is the eigenvalue of larger absolute value, and RT2 of
     !! smaller absolute value.  If the eigenvectors are computed, then
     !! on return ( CS1, SN1 ) is the unit eigenvector for RT1, hence
     !! [  CS1     SN1   ] . [ A  B ] . [ CS1    -SN1   ] = [ RT1  0  ]
     !! [ -SN1     CS1   ]   [ B  C ]   [ SN1     CS1   ]   [  0  RT2 ]
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           complex(sp), intent(in) :: a, b, c
           complex(sp), intent(out) :: cs1, evscal, rt1, rt2, sn1
       ! =====================================================================
           ! Parameters 
           real(sp), parameter :: thresh = 0.1_sp
           
           
           
           
           
           ! Local Scalars 
           real(sp) :: babs, evnorm, tabs, z
           complex(sp) :: s, t, tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! special case:  the matrix is actually diagonal.
           ! to avoid divide by zero later, we treat this case separately.
           if( abs( b )==zero ) then
              rt1 = a
              rt2 = c
              if( abs( rt1 )<abs( rt2 ) ) then
                 tmp = rt1
                 rt1 = rt2
                 rt2 = tmp
                 cs1 = zero
                 sn1 = one
              else
                 cs1 = one
                 sn1 = zero
              end if
           else
              ! compute the eigenvalues and eigenvectors.
              ! the characteristic equation is
                 ! lambda **2 - (a+c) lambda + (a*c - b*b)
              ! and we solve it using the quadratic formula.
              s = ( a+c )*half
              t = ( a-c )*half
              ! take the square root carefully to avoid over/under flow.
              babs = abs( b )
              tabs = abs( t )
              z = max( babs, tabs )
              if( z>zero )t = z*sqrt( ( t / z )**2_ilp+( b / z )**2_ilp )
              ! compute the two eigenvalues.  rt1 and rt2 are exchanged
              ! if necessary so that rt1 will have the greater magnitude.
              rt1 = s + t
              rt2 = s - t
              if( abs( rt1 )<abs( rt2 ) ) then
                 tmp = rt1
                 rt1 = rt2
                 rt2 = tmp
              end if
              ! choose cs1 = 1 and sn1 to satisfy the first equation, then
              ! scale the components of this eigenvector so that the matrix
              ! of eigenvectors x satisfies  x * x**t = i .  (no scaling is
              ! done if the norm of the eigenvalue matrix is less than thresh.)
              sn1 = ( rt1-a ) / b
              tabs = abs( sn1 )
              if( tabs>one ) then
                 t = tabs*sqrt( ( one / tabs )**2_ilp+( sn1 / tabs )**2_ilp )
              else
                 t = sqrt( cone+sn1*sn1 )
              end if
              evnorm = abs( t )
              if( evnorm>=thresh ) then
                 evscal = cone / t
                 cs1 = evscal
                 sn1 = sn1*evscal
              else
                 evscal = zero
              end if
           end if
           return
     end subroutine stdlib_claesy

     pure module subroutine stdlib_zlaesy( a, b, c, rt1, rt2, evscal, cs1, sn1 )
     !! ZLAESY computes the eigendecomposition of a 2-by-2 symmetric matrix
     !! ( ( A, B );( B, C ) )
     !! provided the norm of the matrix of eigenvectors is larger than
     !! some threshold value.
     !! RT1 is the eigenvalue of larger absolute value, and RT2 of
     !! smaller absolute value.  If the eigenvectors are computed, then
     !! on return ( CS1, SN1 ) is the unit eigenvector for RT1, hence
     !! [  CS1     SN1   ] . [ A  B ] . [ CS1    -SN1   ] = [ RT1  0  ]
     !! [ -SN1     CS1   ]   [ B  C ]   [ SN1     CS1   ]   [  0  RT2 ]
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           complex(dp), intent(in) :: a, b, c
           complex(dp), intent(out) :: cs1, evscal, rt1, rt2, sn1
       ! =====================================================================
           ! Parameters 
           real(dp), parameter :: thresh = 0.1_dp
           
           
           
           
           
           ! Local Scalars 
           real(dp) :: babs, evnorm, tabs, z
           complex(dp) :: s, t, tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! special case:  the matrix is actually diagonal.
           ! to avoid divide by zero later, we treat this case separately.
           if( abs( b )==zero ) then
              rt1 = a
              rt2 = c
              if( abs( rt1 )<abs( rt2 ) ) then
                 tmp = rt1
                 rt1 = rt2
                 rt2 = tmp
                 cs1 = zero
                 sn1 = one
              else
                 cs1 = one
                 sn1 = zero
              end if
           else
              ! compute the eigenvalues and eigenvectors.
              ! the characteristic equation is
                 ! lambda **2 - (a+c) lambda + (a*c - b*b)
              ! and we solve it using the quadratic formula.
              s = ( a+c )*half
              t = ( a-c )*half
              ! take the square root carefully to avoid over/under flow.
              babs = abs( b )
              tabs = abs( t )
              z = max( babs, tabs )
              if( z>zero )t = z*sqrt( ( t / z )**2_ilp+( b / z )**2_ilp )
              ! compute the two eigenvalues.  rt1 and rt2 are exchanged
              ! if necessary so that rt1 will have the greater magnitude.
              rt1 = s + t
              rt2 = s - t
              if( abs( rt1 )<abs( rt2 ) ) then
                 tmp = rt1
                 rt1 = rt2
                 rt2 = tmp
              end if
              ! choose cs1 = 1 and sn1 to satisfy the first equation, then
              ! scale the components of this eigenvector so that the matrix
              ! of eigenvectors x satisfies  x * x**t = i .  (no scaling is
              ! done if the norm of the eigenvalue matrix is less than thresh.)
              sn1 = ( rt1-a ) / b
              tabs = abs( sn1 )
              if( tabs>one ) then
                 t = tabs*sqrt( ( one / tabs )**2_ilp+( sn1 / tabs )**2_ilp )
              else
                 t = sqrt( cone+sn1*sn1 )
              end if
              evnorm = abs( t )
              if( evnorm>=thresh ) then
                 evscal = cone / t
                 cs1 = evscal
                 sn1 = sn1*evscal
              else
                 evscal = zero
              end if
           end if
           return
     end subroutine stdlib_zlaesy




     pure module subroutine stdlib_slaev2( a, b, c, rt1, rt2, cs1, sn1 )
     !! SLAEV2 computes the eigendecomposition of a 2-by-2 symmetric matrix
     !! [  A   B  ]
     !! [  B   C  ].
     !! On return, RT1 is the eigenvalue of larger absolute value, RT2 is the
     !! eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right
     !! eigenvector for RT1, giving the decomposition
     !! [ CS1  SN1 ] [  A   B  ] [ CS1 -SN1 ]  =  [ RT1  0  ]
     !! [-SN1  CS1 ] [  B   C  ] [ SN1  CS1 ]     [  0  RT2 ].
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: a, b, c
           real(sp), intent(out) :: cs1, rt1, rt2, sn1
       ! =====================================================================
           
           
           
           
           ! Local Scalars 
           integer(ilp) :: sgn1, sgn2
           real(sp) :: ab, acmn, acmx, acs, adf, cs, ct, df, rt, sm, tb, tn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! compute the eigenvalues
           sm = a + c
           df = a - c
           adf = abs( df )
           tb = b + b
           ab = abs( tb )
           if( abs( a )>abs( c ) ) then
              acmx = a
              acmn = c
           else
              acmx = c
              acmn = a
           end if
           if( adf>ab ) then
              rt = adf*sqrt( one+( ab / adf )**2_ilp )
           else if( adf<ab ) then
              rt = ab*sqrt( one+( adf / ab )**2_ilp )
           else
              ! includes case ab=adf=0
              rt = ab*sqrt( two )
           end if
           if( sm<zero ) then
              rt1 = half*( sm-rt )
              sgn1 = -1_ilp
              ! order of execution important.
              ! to get fully accurate smaller eigenvalue,
              ! next line needs to be executed in higher precision.
              rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
           else if( sm>zero ) then
              rt1 = half*( sm+rt )
              sgn1 = 1_ilp
              ! order of execution important.
              ! to get fully accurate smaller eigenvalue,
              ! next line needs to be executed in higher precision.
              rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
           else
              ! includes case rt1 = rt2 = 0
              rt1 = half*rt
              rt2 = -half*rt
              sgn1 = 1_ilp
           end if
           ! compute the eigenvector
           if( df>=zero ) then
              cs = df + rt
              sgn2 = 1_ilp
           else
              cs = df - rt
              sgn2 = -1_ilp
           end if
           acs = abs( cs )
           if( acs>ab ) then
              ct = -tb / cs
              sn1 = one / sqrt( one+ct*ct )
              cs1 = ct*sn1
           else
              if( ab==zero ) then
                 cs1 = one
                 sn1 = zero
              else
                 tn = -cs / tb
                 cs1 = one / sqrt( one+tn*tn )
                 sn1 = tn*cs1
              end if
           end if
           if( sgn1==sgn2 ) then
              tn = cs1
              cs1 = -sn1
              sn1 = tn
           end if
           return
     end subroutine stdlib_slaev2

     pure module subroutine stdlib_dlaev2( a, b, c, rt1, rt2, cs1, sn1 )
     !! DLAEV2 computes the eigendecomposition of a 2-by-2 symmetric matrix
     !! [  A   B  ]
     !! [  B   C  ].
     !! On return, RT1 is the eigenvalue of larger absolute value, RT2 is the
     !! eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right
     !! eigenvector for RT1, giving the decomposition
     !! [ CS1  SN1 ] [  A   B  ] [ CS1 -SN1 ]  =  [ RT1  0  ]
     !! [-SN1  CS1 ] [  B   C  ] [ SN1  CS1 ]     [  0  RT2 ].
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: a, b, c
           real(dp), intent(out) :: cs1, rt1, rt2, sn1
       ! =====================================================================
           
           
           
           
           ! Local Scalars 
           integer(ilp) :: sgn1, sgn2
           real(dp) :: ab, acmn, acmx, acs, adf, cs, ct, df, rt, sm, tb, tn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! compute the eigenvalues
           sm = a + c
           df = a - c
           adf = abs( df )
           tb = b + b
           ab = abs( tb )
           if( abs( a )>abs( c ) ) then
              acmx = a
              acmn = c
           else
              acmx = c
              acmn = a
           end if
           if( adf>ab ) then
              rt = adf*sqrt( one+( ab / adf )**2_ilp )
           else if( adf<ab ) then
              rt = ab*sqrt( one+( adf / ab )**2_ilp )
           else
              ! includes case ab=adf=0
              rt = ab*sqrt( two )
           end if
           if( sm<zero ) then
              rt1 = half*( sm-rt )
              sgn1 = -1_ilp
              ! order of execution important.
              ! to get fully accurate smaller eigenvalue,
              ! next line needs to be executed in higher precision.
              rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
           else if( sm>zero ) then
              rt1 = half*( sm+rt )
              sgn1 = 1_ilp
              ! order of execution important.
              ! to get fully accurate smaller eigenvalue,
              ! next line needs to be executed in higher precision.
              rt2 = ( acmx / rt1 )*acmn - ( b / rt1 )*b
           else
              ! includes case rt1 = rt2 = 0
              rt1 = half*rt
              rt2 = -half*rt
              sgn1 = 1_ilp
           end if
           ! compute the eigenvector
           if( df>=zero ) then
              cs = df + rt
              sgn2 = 1_ilp
           else
              cs = df - rt
              sgn2 = -1_ilp
           end if
           acs = abs( cs )
           if( acs>ab ) then
              ct = -tb / cs
              sn1 = one / sqrt( one+ct*ct )
              cs1 = ct*sn1
           else
              if( ab==zero ) then
                 cs1 = one
                 sn1 = zero
              else
                 tn = -cs / tb
                 cs1 = one / sqrt( one+tn*tn )
                 sn1 = tn*cs1
              end if
           end if
           if( sgn1==sgn2 ) then
              tn = cs1
              cs1 = -sn1
              sn1 = tn
           end if
           return
     end subroutine stdlib_dlaev2


     pure module subroutine stdlib_claev2( a, b, c, rt1, rt2, cs1, sn1 )
     !! CLAEV2 computes the eigendecomposition of a 2-by-2 Hermitian matrix
     !! [  A         B  ]
     !! [  CONJG(B)  C  ].
     !! On return, RT1 is the eigenvalue of larger absolute value, RT2 is the
     !! eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right
     !! eigenvector for RT1, giving the decomposition
     !! [ CS1  CONJG(SN1) ] [    A     B ] [ CS1 -CONJG(SN1) ] = [ RT1  0  ]
     !! [-SN1     CS1     ] [ CONJG(B) C ] [ SN1     CS1     ]   [  0  RT2 ].
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(out) :: cs1, rt1, rt2
           complex(sp), intent(in) :: a, b, c
           complex(sp), intent(out) :: sn1
       ! =====================================================================
           
           
           ! Local Scalars 
           real(sp) :: t
           complex(sp) :: w
           ! Intrinsic Functions 
           ! Executable Statements 
           if( abs( b )==zero ) then
              w = one
           else
              w = conjg( b ) / abs( b )
           end if
           call stdlib_slaev2( real( a,KIND=sp), abs( b ), real( c,KIND=sp), rt1, rt2, cs1, t )
                     
           sn1 = w*t
           return
     end subroutine stdlib_claev2

     pure module subroutine stdlib_zlaev2( a, b, c, rt1, rt2, cs1, sn1 )
     !! ZLAEV2 computes the eigendecomposition of a 2-by-2 Hermitian matrix
     !! [  A         B  ]
     !! [  CONJG(B)  C  ].
     !! On return, RT1 is the eigenvalue of larger absolute value, RT2 is the
     !! eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right
     !! eigenvector for RT1, giving the decomposition
     !! [ CS1  CONJG(SN1) ] [    A     B ] [ CS1 -CONJG(SN1) ] = [ RT1  0  ]
     !! [-SN1     CS1     ] [ CONJG(B) C ] [ SN1     CS1     ]   [  0  RT2 ].
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(out) :: cs1, rt1, rt2
           complex(dp), intent(in) :: a, b, c
           complex(dp), intent(out) :: sn1
       ! =====================================================================
           
           
           ! Local Scalars 
           real(dp) :: t
           complex(dp) :: w
           ! Intrinsic Functions 
           ! Executable Statements 
           if( abs( b )==zero ) then
              w = one
           else
              w = conjg( b ) / abs( b )
           end if
           call stdlib_dlaev2( real( a,KIND=dp), abs( b ), real( c,KIND=dp), rt1, rt2, cs1, t )
                     
           sn1 = w*t
           return
     end subroutine stdlib_zlaev2




     pure module subroutine stdlib_slagtf( n, a, lambda, b, c, tol, d, in, info )
     !! SLAGTF factorizes the matrix (T - lambda*I), where T is an n by n
     !! tridiagonal matrix and lambda is a scalar, as
     !! T - lambda*I = PLU,
     !! where P is a permutation matrix, L is a unit lower tridiagonal matrix
     !! with at most one non-zero sub-diagonal elements per column and U is
     !! an upper triangular matrix with at most two non-zero super-diagonal
     !! elements per column.
     !! The factorization is obtained by Gaussian elimination with partial
     !! pivoting and implicit row scaling.
     !! The parameter LAMBDA is included in the routine so that SLAGTF may
     !! be used, in conjunction with SLAGTS, to obtain eigenvectors of T by
     !! inverse iteration.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: lambda, tol
           ! Array Arguments 
           integer(ilp), intent(out) :: in(*)
           real(sp), intent(inout) :: a(*), b(*), c(*)
           real(sp), intent(out) :: d(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: k
           real(sp) :: eps, mult, piv1, piv2, scale1, scale2, temp, tl
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'SLAGTF', -info )
              return
           end if
           if( n==0 )return
           a( 1_ilp ) = a( 1_ilp ) - lambda
           in( n ) = 0_ilp
           if( n==1_ilp ) then
              if( a( 1_ilp )==zero )in( 1_ilp ) = 1_ilp
              return
           end if
           eps = stdlib_slamch( 'EPSILON' )
           tl = max( tol, eps )
           scale1 = abs( a( 1_ilp ) ) + abs( b( 1_ilp ) )
           loop_10: do k = 1, n - 1
              a( k+1 ) = a( k+1 ) - lambda
              scale2 = abs( c( k ) ) + abs( a( k+1 ) )
              if( k<( n-1 ) )scale2 = scale2 + abs( b( k+1 ) )
              if( a( k )==zero ) then
                 piv1 = zero
              else
                 piv1 = abs( a( k ) ) / scale1
              end if
              if( c( k )==zero ) then
                 in( k ) = 0_ilp
                 piv2 = zero
                 scale1 = scale2
                 if( k<( n-1 ) )d( k ) = zero
              else
                 piv2 = abs( c( k ) ) / scale2
                 if( piv2<=piv1 ) then
                    in( k ) = 0_ilp
                    scale1 = scale2
                    c( k ) = c( k ) / a( k )
                    a( k+1 ) = a( k+1 ) - c( k )*b( k )
                    if( k<( n-1 ) )d( k ) = zero
                 else
                    in( k ) = 1_ilp
                    mult = a( k ) / c( k )
                    a( k ) = c( k )
                    temp = a( k+1 )
                    a( k+1 ) = b( k ) - mult*temp
                    if( k<( n-1 ) ) then
                       d( k ) = b( k+1 )
                       b( k+1 ) = -mult*d( k )
                    end if
                    b( k ) = temp
                    c( k ) = mult
                 end if
              end if
              if( ( max( piv1, piv2 )<=tl ) .and. ( in( n )==0_ilp ) )in( n ) = k
           end do loop_10
           if( ( abs( a( n ) )<=scale1*tl ) .and. ( in( n )==0_ilp ) )in( n ) = n
           return
     end subroutine stdlib_slagtf

     pure module subroutine stdlib_dlagtf( n, a, lambda, b, c, tol, d, in, info )
     !! DLAGTF factorizes the matrix (T - lambda*I), where T is an n by n
     !! tridiagonal matrix and lambda is a scalar, as
     !! T - lambda*I = PLU,
     !! where P is a permutation matrix, L is a unit lower tridiagonal matrix
     !! with at most one non-zero sub-diagonal elements per column and U is
     !! an upper triangular matrix with at most two non-zero super-diagonal
     !! elements per column.
     !! The factorization is obtained by Gaussian elimination with partial
     !! pivoting and implicit row scaling.
     !! The parameter LAMBDA is included in the routine so that DLAGTF may
     !! be used, in conjunction with DLAGTS, to obtain eigenvectors of T by
     !! inverse iteration.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: lambda, tol
           ! Array Arguments 
           integer(ilp), intent(out) :: in(*)
           real(dp), intent(inout) :: a(*), b(*), c(*)
           real(dp), intent(out) :: d(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: k
           real(dp) :: eps, mult, piv1, piv2, scale1, scale2, temp, tl
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
              call stdlib_xerbla( 'DLAGTF', -info )
              return
           end if
           if( n==0 )return
           a( 1_ilp ) = a( 1_ilp ) - lambda
           in( n ) = 0_ilp
           if( n==1_ilp ) then
              if( a( 1_ilp )==zero )in( 1_ilp ) = 1_ilp
              return
           end if
           eps = stdlib_dlamch( 'EPSILON' )
           tl = max( tol, eps )
           scale1 = abs( a( 1_ilp ) ) + abs( b( 1_ilp ) )
           loop_10: do k = 1, n - 1
              a( k+1 ) = a( k+1 ) - lambda
              scale2 = abs( c( k ) ) + abs( a( k+1 ) )
              if( k<( n-1 ) )scale2 = scale2 + abs( b( k+1 ) )
              if( a( k )==zero ) then
                 piv1 = zero
              else
                 piv1 = abs( a( k ) ) / scale1
              end if
              if( c( k )==zero ) then
                 in( k ) = 0_ilp
                 piv2 = zero
                 scale1 = scale2
                 if( k<( n-1 ) )d( k ) = zero
              else
                 piv2 = abs( c( k ) ) / scale2
                 if( piv2<=piv1 ) then
                    in( k ) = 0_ilp
                    scale1 = scale2
                    c( k ) = c( k ) / a( k )
                    a( k+1 ) = a( k+1 ) - c( k )*b( k )
                    if( k<( n-1 ) )d( k ) = zero
                 else
                    in( k ) = 1_ilp
                    mult = a( k ) / c( k )
                    a( k ) = c( k )
                    temp = a( k+1 )
                    a( k+1 ) = b( k ) - mult*temp
                    if( k<( n-1 ) ) then
                       d( k ) = b( k+1 )
                       b( k+1 ) = -mult*d( k )
                    end if
                    b( k ) = temp
                    c( k ) = mult
                 end if
              end if
              if( ( max( piv1, piv2 )<=tl ) .and. ( in( n )==0_ilp ) )in( n ) = k
           end do loop_10
           if( ( abs( a( n ) )<=scale1*tl ) .and. ( in( n )==0_ilp ) )in( n ) = n
           return
     end subroutine stdlib_dlagtf




     pure module subroutine stdlib_slagts( job, n, a, b, c, d, in, y, tol, info )
     !! SLAGTS may be used to solve one of the systems of equations
     !! (T - lambda*I)*x = y   or   (T - lambda*I)**T*x = y,
     !! where T is an n by n tridiagonal matrix, for x, following the
     !! factorization of (T - lambda*I) as
     !! (T - lambda*I) = P*L*U ,
     !! by routine SLAGTF. The choice of equation to be solved is
     !! controlled by the argument JOB, and in each case there is an option
     !! to perturb zero or very small diagonal elements of U, this option
     !! being intended for use in applications such as inverse iteration.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: job, n
           real(sp), intent(inout) :: tol
           ! Array Arguments 
           integer(ilp), intent(in) :: in(*)
           real(sp), intent(in) :: a(*), b(*), c(*), d(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: k
           real(sp) :: absak, ak, bignum, eps, pert, sfmin, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           if( ( abs( job )>2_ilp ) .or. ( job==0_ilp ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLAGTS', -info )
              return
           end if
           if( n==0 )return
           eps = stdlib_slamch( 'EPSILON' )
           sfmin = stdlib_slamch( 'SAFE MINIMUM' )
           bignum = one / sfmin
           if( job<0_ilp ) then
              if( tol<=zero ) then
                 tol = abs( a( 1_ilp ) )
                 if( n>1_ilp )tol = max( tol, abs( a( 2_ilp ) ), abs( b( 1_ilp ) ) )
                 do k = 3, n
                    tol = max( tol, abs( a( k ) ), abs( b( k-1 ) ),abs( d( k-2 ) ) )
                 end do
                 tol = tol*eps
                 if( tol==zero )tol = eps
              end if
           end if
           if( abs( job )==1_ilp ) then
              do k = 2, n
                 if( in( k-1 )==0_ilp ) then
                    y( k ) = y( k ) - c( k-1 )*y( k-1 )
                 else
                    temp = y( k-1 )
                    y( k-1 ) = y( k )
                    y( k ) = temp - c( k-1 )*y( k )
                 end if
              end do
              if( job==1_ilp ) then
                 loop_30: do k = n, 1, -1
                    if( k<=n-2 ) then
                       temp = y( k ) - b( k )*y( k+1 ) - d( k )*y( k+2 )
                    else if( k==n-1 ) then
                       temp = y( k ) - b( k )*y( k+1 )
                    else
                       temp = y( k )
                    end if
                    ak = a( k )
                    absak = abs( ak )
                    if( absak<one ) then
                       if( absak<sfmin ) then
                          if( absak==zero .or. abs( temp )*sfmin>absak )then
                             info = k
                             return
                          else
                             temp = temp*bignum
                             ak = ak*bignum
                          end if
                       else if( abs( temp )>absak*bignum ) then
                          info = k
                          return
                       end if
                    end if
                    y( k ) = temp / ak
                 end do loop_30
              else
                 loop_50: do k = n, 1, -1
                    if( k<=n-2 ) then
                       temp = y( k ) - b( k )*y( k+1 ) - d( k )*y( k+2 )
                    else if( k==n-1 ) then
                       temp = y( k ) - b( k )*y( k+1 )
                    else
                       temp = y( k )
                    end if
                    ak = a( k )
                    pert = sign( tol, ak )
                    40 continue
                    absak = abs( ak )
                    if( absak<one ) then
                       if( absak<sfmin ) then
                          if( absak==zero .or. abs( temp )*sfmin>absak )then
                             ak = ak + pert
                             pert = 2_ilp*pert
                             go to 40
                          else
                             temp = temp*bignum
                             ak = ak*bignum
                          end if
                       else if( abs( temp )>absak*bignum ) then
                          ak = ak + pert
                          pert = 2_ilp*pert
                          go to 40
                       end if
                    end if
                    y( k ) = temp / ak
                 end do loop_50
              end if
           else
              ! come to here if  job = 2 or -2
              if( job==2_ilp ) then
                 loop_60: do k = 1, n
                    if( k>=3_ilp ) then
                       temp = y( k ) - b( k-1 )*y( k-1 ) - d( k-2 )*y( k-2 )
                    else if( k==2_ilp ) then
                       temp = y( k ) - b( k-1 )*y( k-1 )
                    else
                       temp = y( k )
                    end if
                    ak = a( k )
                    absak = abs( ak )
                    if( absak<one ) then
                       if( absak<sfmin ) then
                          if( absak==zero .or. abs( temp )*sfmin>absak )then
                             info = k
                             return
                          else
                             temp = temp*bignum
                             ak = ak*bignum
                          end if
                       else if( abs( temp )>absak*bignum ) then
                          info = k
                          return
                       end if
                    end if
                    y( k ) = temp / ak
                 end do loop_60
              else
                 loop_80: do k = 1, n
                    if( k>=3_ilp ) then
                       temp = y( k ) - b( k-1 )*y( k-1 ) - d( k-2 )*y( k-2 )
                    else if( k==2_ilp ) then
                       temp = y( k ) - b( k-1 )*y( k-1 )
                    else
                       temp = y( k )
                    end if
                    ak = a( k )
                    pert = sign( tol, ak )
                    70 continue
                    absak = abs( ak )
                    if( absak<one ) then
                       if( absak<sfmin ) then
                          if( absak==zero .or. abs( temp )*sfmin>absak )then
                             ak = ak + pert
                             pert = 2_ilp*pert
                             go to 70
                          else
                             temp = temp*bignum
                             ak = ak*bignum
                          end if
                       else if( abs( temp )>absak*bignum ) then
                          ak = ak + pert
                          pert = 2_ilp*pert
                          go to 70
                       end if
                    end if
                    y( k ) = temp / ak
                 end do loop_80
              end if
              do k = n, 2, -1
                 if( in( k-1 )==0_ilp ) then
                    y( k-1 ) = y( k-1 ) - c( k-1 )*y( k )
                 else
                    temp = y( k-1 )
                    y( k-1 ) = y( k )
                    y( k ) = temp - c( k-1 )*y( k )
                 end if
              end do
           end if
     end subroutine stdlib_slagts

     pure module subroutine stdlib_dlagts( job, n, a, b, c, d, in, y, tol, info )
     !! DLAGTS may be used to solve one of the systems of equations
     !! (T - lambda*I)*x = y   or   (T - lambda*I)**T*x = y,
     !! where T is an n by n tridiagonal matrix, for x, following the
     !! factorization of (T - lambda*I) as
     !! (T - lambda*I) = P*L*U ,
     !! by routine DLAGTF. The choice of equation to be solved is
     !! controlled by the argument JOB, and in each case there is an option
     !! to perturb zero or very small diagonal elements of U, this option
     !! being intended for use in applications such as inverse iteration.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: job, n
           real(dp), intent(inout) :: tol
           ! Array Arguments 
           integer(ilp), intent(in) :: in(*)
           real(dp), intent(in) :: a(*), b(*), c(*), d(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: k
           real(dp) :: absak, ak, bignum, eps, pert, sfmin, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           info = 0_ilp
           if( ( abs( job )>2_ilp ) .or. ( job==0_ilp ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLAGTS', -info )
              return
           end if
           if( n==0 )return
           eps = stdlib_dlamch( 'EPSILON' )
           sfmin = stdlib_dlamch( 'SAFE MINIMUM' )
           bignum = one / sfmin
           if( job<0_ilp ) then
              if( tol<=zero ) then
                 tol = abs( a( 1_ilp ) )
                 if( n>1_ilp )tol = max( tol, abs( a( 2_ilp ) ), abs( b( 1_ilp ) ) )
                 do k = 3, n
                    tol = max( tol, abs( a( k ) ), abs( b( k-1 ) ),abs( d( k-2 ) ) )
                 end do
                 tol = tol*eps
                 if( tol==zero )tol = eps
              end if
           end if
           if( abs( job )==1_ilp ) then
              do k = 2, n
                 if( in( k-1 )==0_ilp ) then
                    y( k ) = y( k ) - c( k-1 )*y( k-1 )
                 else
                    temp = y( k-1 )
                    y( k-1 ) = y( k )
                    y( k ) = temp - c( k-1 )*y( k )
                 end if
              end do
              if( job==1_ilp ) then
                 loop_30: do k = n, 1, -1
                    if( k<=n-2 ) then
                       temp = y( k ) - b( k )*y( k+1 ) - d( k )*y( k+2 )
                    else if( k==n-1 ) then
                       temp = y( k ) - b( k )*y( k+1 )
                    else
                       temp = y( k )
                    end if
                    ak = a( k )
                    absak = abs( ak )
                    if( absak<one ) then
                       if( absak<sfmin ) then
                          if( absak==zero .or. abs( temp )*sfmin>absak )then
                             info = k
                             return
                          else
                             temp = temp*bignum
                             ak = ak*bignum
                          end if
                       else if( abs( temp )>absak*bignum ) then
                          info = k
                          return
                       end if
                    end if
                    y( k ) = temp / ak
                 end do loop_30
              else
                 loop_50: do k = n, 1, -1
                    if( k<=n-2 ) then
                       temp = y( k ) - b( k )*y( k+1 ) - d( k )*y( k+2 )
                    else if( k==n-1 ) then
                       temp = y( k ) - b( k )*y( k+1 )
                    else
                       temp = y( k )
                    end if
                    ak = a( k )
                    pert = sign( tol, ak )
                    40 continue
                    absak = abs( ak )
                    if( absak<one ) then
                       if( absak<sfmin ) then
                          if( absak==zero .or. abs( temp )*sfmin>absak )then
                             ak = ak + pert
                             pert = 2_ilp*pert
                             go to 40
                          else
                             temp = temp*bignum
                             ak = ak*bignum
                          end if
                       else if( abs( temp )>absak*bignum ) then
                          ak = ak + pert
                          pert = 2_ilp*pert
                          go to 40
                       end if
                    end if
                    y( k ) = temp / ak
                 end do loop_50
              end if
           else
              ! come to here if  job = 2 or -2
              if( job==2_ilp ) then
                 loop_60: do k = 1, n
                    if( k>=3_ilp ) then
                       temp = y( k ) - b( k-1 )*y( k-1 ) - d( k-2 )*y( k-2 )
                    else if( k==2_ilp ) then
                       temp = y( k ) - b( k-1 )*y( k-1 )
                    else
                       temp = y( k )
                    end if
                    ak = a( k )
                    absak = abs( ak )
                    if( absak<one ) then
                       if( absak<sfmin ) then
                          if( absak==zero .or. abs( temp )*sfmin>absak )then
                             info = k
                             return
                          else
                             temp = temp*bignum
                             ak = ak*bignum
                          end if
                       else if( abs( temp )>absak*bignum ) then
                          info = k
                          return
                       end if
                    end if
                    y( k ) = temp / ak
                 end do loop_60
              else
                 loop_80: do k = 1, n
                    if( k>=3_ilp ) then
                       temp = y( k ) - b( k-1 )*y( k-1 ) - d( k-2 )*y( k-2 )
                    else if( k==2_ilp ) then
                       temp = y( k ) - b( k-1 )*y( k-1 )
                    else
                       temp = y( k )
                    end if
                    ak = a( k )
                    pert = sign( tol, ak )
                    70 continue
                    absak = abs( ak )
                    if( absak<one ) then
                       if( absak<sfmin ) then
                          if( absak==zero .or. abs( temp )*sfmin>absak )then
                             ak = ak + pert
                             pert = 2_ilp*pert
                             go to 70
                          else
                             temp = temp*bignum
                             ak = ak*bignum
                          end if
                       else if( abs( temp )>absak*bignum ) then
                          ak = ak + pert
                          pert = 2_ilp*pert
                          go to 70
                       end if
                    end if
                    y( k ) = temp / ak
                 end do loop_80
              end if
              do k = n, 2, -1
                 if( in( k-1 )==0_ilp ) then
                    y( k-1 ) = y( k-1 ) - c( k-1 )*y( k )
                 else
                    temp = y( k-1 )
                    y( k-1 ) = y( k )
                    y( k ) = temp - c( k-1 )*y( k )
                 end if
              end do
           end if
     end subroutine stdlib_dlagts




     pure module subroutine stdlib_ssptrd( uplo, n, ap, d, e, tau, info )
     !! SSPTRD reduces a real symmetric matrix A stored in packed form to
     !! symmetric tridiagonal form T by an orthogonal similarity
     !! transformation: Q**T * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(out) :: d(*), e(*), tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, i1, i1i1, ii
           real(sp) :: alpha, taui
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPTRD', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           if( upper ) then
              ! reduce the upper triangle of a.
              ! i1 is the index in ap of a(1,i+1).
              i1 = n*( n-1 ) / 2_ilp + 1_ilp
              do i = n - 1, 1, -1
                 ! generate elementary reflector h(i) = i - tau * v * v**t
                 ! to annihilate a(1:i-1,i+1)
                 call stdlib_slarfg( i, ap( i1+i-1 ), ap( i1 ), 1_ilp, taui )
                 e( i ) = ap( i1+i-1 )
                 if( taui/=zero ) then
                    ! apply h(i) from both sides to a(1:i,1:i)
                    ap( i1+i-1 ) = one
                    ! compute  y := tau * a * v  storing y in tau(1:i)
                    call stdlib_sspmv( uplo, i, taui, ap, ap( i1 ), 1_ilp, zero, tau,1_ilp )
                    ! compute  w := y - 1/2 * tau * (y**t *v) * v
                    alpha = -half*taui*stdlib_sdot( i, tau, 1_ilp, ap( i1 ), 1_ilp )
                    call stdlib_saxpy( i, alpha, ap( i1 ), 1_ilp, tau, 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**t - w * v**t
                    call stdlib_sspr2( uplo, i, -one, ap( i1 ), 1_ilp, tau, 1_ilp, ap )
                    ap( i1+i-1 ) = e( i )
                 end if
                 d( i+1 ) = ap( i1+i )
                 tau( i ) = taui
                 i1 = i1 - i
              end do
              d( 1_ilp ) = ap( 1_ilp )
           else
              ! reduce the lower triangle of a. ii is the index in ap of
              ! a(i,i) and i1i1 is the index of a(i+1,i+1).
              ii = 1_ilp
              do i = 1, n - 1
                 i1i1 = ii + n - i + 1_ilp
                 ! generate elementary reflector h(i) = i - tau * v * v**t
                 ! to annihilate a(i+2:n,i)
                 call stdlib_slarfg( n-i, ap( ii+1 ), ap( ii+2 ), 1_ilp, taui )
                 e( i ) = ap( ii+1 )
                 if( taui/=zero ) then
                    ! apply h(i) from both sides to a(i+1:n,i+1:n)
                    ap( ii+1 ) = one
                    ! compute  y := tau * a * v  storing y in tau(i:n-1)
                    call stdlib_sspmv( uplo, n-i, taui, ap( i1i1 ), ap( ii+1 ), 1_ilp,zero, tau( i ), &
                              1_ilp )
                    ! compute  w := y - 1/2 * tau * (y**t *v) * v
                    alpha = -half*taui*stdlib_sdot( n-i, tau( i ), 1_ilp, ap( ii+1 ),1_ilp )
                    call stdlib_saxpy( n-i, alpha, ap( ii+1 ), 1_ilp, tau( i ), 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**t - w * v**t
                    call stdlib_sspr2( uplo, n-i, -one, ap( ii+1 ), 1_ilp, tau( i ), 1_ilp,ap( i1i1 ) )
                              
                    ap( ii+1 ) = e( i )
                 end if
                 d( i ) = ap( ii )
                 tau( i ) = taui
                 ii = i1i1
              end do
              d( n ) = ap( ii )
           end if
           return
     end subroutine stdlib_ssptrd

     pure module subroutine stdlib_dsptrd( uplo, n, ap, d, e, tau, info )
     !! DSPTRD reduces a real symmetric matrix A stored in packed form to
     !! symmetric tridiagonal form T by an orthogonal similarity
     !! transformation: Q**T * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(out) :: d(*), e(*), tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, i1, i1i1, ii
           real(dp) :: alpha, taui
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPTRD', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           if( upper ) then
              ! reduce the upper triangle of a.
              ! i1 is the index in ap of a(1,i+1).
              i1 = n*( n-1 ) / 2_ilp + 1_ilp
              do i = n - 1, 1, -1
                 ! generate elementary reflector h(i) = i - tau * v * v**t
                 ! to annihilate a(1:i-1,i+1)
                 call stdlib_dlarfg( i, ap( i1+i-1 ), ap( i1 ), 1_ilp, taui )
                 e( i ) = ap( i1+i-1 )
                 if( taui/=zero ) then
                    ! apply h(i) from both sides to a(1:i,1:i)
                    ap( i1+i-1 ) = one
                    ! compute  y := tau * a * v  storing y in tau(1:i)
                    call stdlib_dspmv( uplo, i, taui, ap, ap( i1 ), 1_ilp, zero, tau,1_ilp )
                    ! compute  w := y - 1/2 * tau * (y**t *v) * v
                    alpha = -half*taui*stdlib_ddot( i, tau, 1_ilp, ap( i1 ), 1_ilp )
                    call stdlib_daxpy( i, alpha, ap( i1 ), 1_ilp, tau, 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**t - w * v**t
                    call stdlib_dspr2( uplo, i, -one, ap( i1 ), 1_ilp, tau, 1_ilp, ap )
                    ap( i1+i-1 ) = e( i )
                 end if
                 d( i+1 ) = ap( i1+i )
                 tau( i ) = taui
                 i1 = i1 - i
              end do
              d( 1_ilp ) = ap( 1_ilp )
           else
              ! reduce the lower triangle of a. ii is the index in ap of
              ! a(i,i) and i1i1 is the index of a(i+1,i+1).
              ii = 1_ilp
              do i = 1, n - 1
                 i1i1 = ii + n - i + 1_ilp
                 ! generate elementary reflector h(i) = i - tau * v * v**t
                 ! to annihilate a(i+2:n,i)
                 call stdlib_dlarfg( n-i, ap( ii+1 ), ap( ii+2 ), 1_ilp, taui )
                 e( i ) = ap( ii+1 )
                 if( taui/=zero ) then
                    ! apply h(i) from both sides to a(i+1:n,i+1:n)
                    ap( ii+1 ) = one
                    ! compute  y := tau * a * v  storing y in tau(i:n-1)
                    call stdlib_dspmv( uplo, n-i, taui, ap( i1i1 ), ap( ii+1 ), 1_ilp,zero, tau( i ), &
                              1_ilp )
                    ! compute  w := y - 1/2 * tau * (y**t *v) * v
                    alpha = -half*taui*stdlib_ddot( n-i, tau( i ), 1_ilp, ap( ii+1 ),1_ilp )
                    call stdlib_daxpy( n-i, alpha, ap( ii+1 ), 1_ilp, tau( i ), 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**t - w * v**t
                    call stdlib_dspr2( uplo, n-i, -one, ap( ii+1 ), 1_ilp, tau( i ), 1_ilp,ap( i1i1 ) )
                              
                    ap( ii+1 ) = e( i )
                 end if
                 d( i ) = ap( ii )
                 tau( i ) = taui
                 ii = i1i1
              end do
              d( n ) = ap( ii )
           end if
           return
     end subroutine stdlib_dsptrd




     pure module subroutine stdlib_sopgtr( uplo, n, ap, tau, q, ldq, work, info )
     !! SOPGTR generates a real orthogonal matrix Q which is defined as the
     !! product of n-1 elementary reflectors H(i) of order n, as returned by
     !! SSPTRD using packed storage:
     !! if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, n
           ! Array Arguments 
           real(sp), intent(in) :: ap(*), tau(*)
           real(sp), intent(out) :: q(ldq,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, iinfo, ij, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SOPGTR', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! q was determined by a call to stdlib_ssptrd with uplo = 'u'
              ! unpack the vectors which define the elementary reflectors and
              ! set the last row and column of q equal to those of the unit
              ! matrix
              ij = 2_ilp
              do j = 1, n - 1
                 do i = 1, j - 1
                    q( i, j ) = ap( ij )
                    ij = ij + 1_ilp
                 end do
                 ij = ij + 2_ilp
                 q( n, j ) = zero
              end do
              do i = 1, n - 1
                 q( i, n ) = zero
              end do
              q( n, n ) = one
              ! generate q(1:n-1,1:n-1)
              call stdlib_sorg2l( n-1, n-1, n-1, q, ldq, tau, work, iinfo )
           else
              ! q was determined by a call to stdlib_ssptrd with uplo = 'l'.
              ! unpack the vectors which define the elementary reflectors and
              ! set the first row and column of q equal to those of the unit
              ! matrix
              q( 1_ilp, 1_ilp ) = one
              do i = 2, n
                 q( i, 1_ilp ) = zero
              end do
              ij = 3_ilp
              do j = 2, n
                 q( 1_ilp, j ) = zero
                 do i = j + 1, n
                    q( i, j ) = ap( ij )
                    ij = ij + 1_ilp
                 end do
                 ij = ij + 2_ilp
              end do
              if( n>1_ilp ) then
                 ! generate q(2:n,2:n)
                 call stdlib_sorg2r( n-1, n-1, n-1, q( 2_ilp, 2_ilp ), ldq, tau, work,iinfo )
              end if
           end if
           return
     end subroutine stdlib_sopgtr

     pure module subroutine stdlib_dopgtr( uplo, n, ap, tau, q, ldq, work, info )
     !! DOPGTR generates a real orthogonal matrix Q which is defined as the
     !! product of n-1 elementary reflectors H(i) of order n, as returned by
     !! DSPTRD using packed storage:
     !! if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, n
           ! Array Arguments 
           real(dp), intent(in) :: ap(*), tau(*)
           real(dp), intent(out) :: q(ldq,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, iinfo, ij, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DOPGTR', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! q was determined by a call to stdlib_dsptrd with uplo = 'u'
              ! unpack the vectors which define the elementary reflectors and
              ! set the last row and column of q equal to those of the unit
              ! matrix
              ij = 2_ilp
              do j = 1, n - 1
                 do i = 1, j - 1
                    q( i, j ) = ap( ij )
                    ij = ij + 1_ilp
                 end do
                 ij = ij + 2_ilp
                 q( n, j ) = zero
              end do
              do i = 1, n - 1
                 q( i, n ) = zero
              end do
              q( n, n ) = one
              ! generate q(1:n-1,1:n-1)
              call stdlib_dorg2l( n-1, n-1, n-1, q, ldq, tau, work, iinfo )
           else
              ! q was determined by a call to stdlib_dsptrd with uplo = 'l'.
              ! unpack the vectors which define the elementary reflectors and
              ! set the first row and column of q equal to those of the unit
              ! matrix
              q( 1_ilp, 1_ilp ) = one
              do i = 2, n
                 q( i, 1_ilp ) = zero
              end do
              ij = 3_ilp
              do j = 2, n
                 q( 1_ilp, j ) = zero
                 do i = j + 1, n
                    q( i, j ) = ap( ij )
                    ij = ij + 1_ilp
                 end do
                 ij = ij + 2_ilp
              end do
              if( n>1_ilp ) then
                 ! generate q(2:n,2:n)
                 call stdlib_dorg2r( n-1, n-1, n-1, q( 2_ilp, 2_ilp ), ldq, tau, work,iinfo )
              end if
           end if
           return
     end subroutine stdlib_dopgtr




     pure module subroutine stdlib_sopmtr( side, uplo, trans, m, n, ap, tau, c, ldc, work,info )
     !! SOPMTR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! nq-1 elementary reflectors, as returned by SSPTRD using packed
     !! storage:
     !! if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: forwrd, left, notran, upper
           integer(ilp) :: i, i1, i2, i3, ic, ii, jc, mi, ni, nq
           real(sp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           upper = stdlib_lsame( uplo, 'U' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SOPMTR', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           if( upper ) then
              ! q was determined by a call to stdlib_ssptrd with uplo = 'u'
              forwrd = ( left .and. notran ) .or.( .not.left .and. .not.notran )
              if( forwrd ) then
                 i1 = 1_ilp
                 i2 = nq - 1_ilp
                 i3 = 1_ilp
                 ii = 2_ilp
              else
                 i1 = nq - 1_ilp
                 i2 = 1_ilp
                 i3 = -1_ilp
                 ii = nq*( nq+1 ) / 2_ilp - 1_ilp
              end if
              if( left ) then
                 ni = n
              else
                 mi = m
              end if
              do i = i1, i2, i3
                 if( left ) then
                    ! h(i) is applied to c(1:i,1:n)
                    mi = i
                 else
                    ! h(i) is applied to c(1:m,1:i)
                    ni = i
                 end if
                 ! apply h(i)
                 aii = ap( ii )
                 ap( ii ) = one
                 call stdlib_slarf( side, mi, ni, ap( ii-i+1 ), 1_ilp, tau( i ), c, ldc,work )
                 ap( ii ) = aii
                 if( forwrd ) then
                    ii = ii + i + 2_ilp
                 else
                    ii = ii - i - 1_ilp
                 end if
              end do
           else
              ! q was determined by a call to stdlib_ssptrd with uplo = 'l'.
              forwrd = ( left .and. .not.notran ) .or.( .not.left .and. notran )
              if( forwrd ) then
                 i1 = 1_ilp
                 i2 = nq - 1_ilp
                 i3 = 1_ilp
                 ii = 2_ilp
              else
                 i1 = nq - 1_ilp
                 i2 = 1_ilp
                 i3 = -1_ilp
                 ii = nq*( nq+1 ) / 2_ilp - 1_ilp
              end if
              if( left ) then
                 ni = n
                 jc = 1_ilp
              else
                 mi = m
                 ic = 1_ilp
              end if
              do i = i1, i2, i3
                 aii = ap( ii )
                 ap( ii ) = one
                 if( left ) then
                    ! h(i) is applied to c(i+1:m,1:n)
                    mi = m - i
                    ic = i + 1_ilp
                 else
                    ! h(i) is applied to c(1:m,i+1:n)
                    ni = n - i
                    jc = i + 1_ilp
                 end if
                 ! apply h(i)
                 call stdlib_slarf( side, mi, ni, ap( ii ), 1_ilp, tau( i ),c( ic, jc ), ldc, work )
                           
                 ap( ii ) = aii
                 if( forwrd ) then
                    ii = ii + nq - i + 1_ilp
                 else
                    ii = ii - nq + i - 2_ilp
                 end if
              end do
           end if
           return
     end subroutine stdlib_sopmtr

     pure module subroutine stdlib_dopmtr( side, uplo, trans, m, n, ap, tau, c, ldc, work,info )
     !! DOPMTR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! nq-1 elementary reflectors, as returned by DSPTRD using packed
     !! storage:
     !! if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: forwrd, left, notran, upper
           integer(ilp) :: i, i1, i2, i3, ic, ii, jc, mi, ni, nq
           real(dp) :: aii
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           upper = stdlib_lsame( uplo, 'U' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DOPMTR', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           if( upper ) then
              ! q was determined by a call to stdlib_dsptrd with uplo = 'u'
              forwrd = ( left .and. notran ) .or.( .not.left .and. .not.notran )
              if( forwrd ) then
                 i1 = 1_ilp
                 i2 = nq - 1_ilp
                 i3 = 1_ilp
                 ii = 2_ilp
              else
                 i1 = nq - 1_ilp
                 i2 = 1_ilp
                 i3 = -1_ilp
                 ii = nq*( nq+1 ) / 2_ilp - 1_ilp
              end if
              if( left ) then
                 ni = n
              else
                 mi = m
              end if
              do i = i1, i2, i3
                 if( left ) then
                    ! h(i) is applied to c(1:i,1:n)
                    mi = i
                 else
                    ! h(i) is applied to c(1:m,1:i)
                    ni = i
                 end if
                 ! apply h(i)
                 aii = ap( ii )
                 ap( ii ) = one
                 call stdlib_dlarf( side, mi, ni, ap( ii-i+1 ), 1_ilp, tau( i ), c, ldc,work )
                 ap( ii ) = aii
                 if( forwrd ) then
                    ii = ii + i + 2_ilp
                 else
                    ii = ii - i - 1_ilp
                 end if
              end do
           else
              ! q was determined by a call to stdlib_dsptrd with uplo = 'l'.
              forwrd = ( left .and. .not.notran ) .or.( .not.left .and. notran )
              if( forwrd ) then
                 i1 = 1_ilp
                 i2 = nq - 1_ilp
                 i3 = 1_ilp
                 ii = 2_ilp
              else
                 i1 = nq - 1_ilp
                 i2 = 1_ilp
                 i3 = -1_ilp
                 ii = nq*( nq+1 ) / 2_ilp - 1_ilp
              end if
              if( left ) then
                 ni = n
                 jc = 1_ilp
              else
                 mi = m
                 ic = 1_ilp
              end if
              do i = i1, i2, i3
                 aii = ap( ii )
                 ap( ii ) = one
                 if( left ) then
                    ! h(i) is applied to c(i+1:m,1:n)
                    mi = m - i
                    ic = i + 1_ilp
                 else
                    ! h(i) is applied to c(1:m,i+1:n)
                    ni = n - i
                    jc = i + 1_ilp
                 end if
                 ! apply h(i)
                 call stdlib_dlarf( side, mi, ni, ap( ii ), 1_ilp, tau( i ),c( ic, jc ), ldc, work )
                           
                 ap( ii ) = aii
                 if( forwrd ) then
                    ii = ii + nq - i + 1_ilp
                 else
                    ii = ii - nq + i - 2_ilp
                 end if
              end do
           end if
           return
     end subroutine stdlib_dopmtr




     pure module subroutine stdlib_ssbtrd( vect, uplo, n, kd, ab, ldab, d, e, q, ldq,work, info )
     !! SSBTRD reduces a real symmetric band matrix A to symmetric
     !! tridiagonal form T by an orthogonal similarity transformation:
     !! Q**T * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldq, n
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*), q(ldq,*)
           real(sp), intent(out) :: d(*), e(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: initq, upper, wantq
           integer(ilp) :: i, i2, ibl, inca, incx, iqaend, iqb, iqend, j, j1, j1end, j1inc, j2, &
                     jend, jin, jinc, k, kd1, kdm1, kdn, l, last, lend, nq, nr, nrt
           real(sp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           initq = stdlib_lsame( vect, 'V' )
           wantq = initq .or. stdlib_lsame( vect, 'U' )
           upper = stdlib_lsame( uplo, 'U' )
           kd1 = kd + 1_ilp
           kdm1 = kd - 1_ilp
           incx = ldab - 1_ilp
           iqend = 1_ilp
           info = 0_ilp
           if( .not.wantq .and. .not.stdlib_lsame( vect, 'N' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd1 ) then
              info = -6_ilp
           else if( ldq<max( 1_ilp, n ) .and. wantq ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSBTRD', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! initialize q to the unit matrix, if needed
           if( initq )call stdlib_slaset( 'FULL', n, n, zero, one, q, ldq )
           ! wherever possible, plane rotations are generated and applied in
           ! vector operations of length nr over the index set j1:j2:kd1.
           ! the cosines and sines of the plane rotations are stored in the
           ! arrays d and work.
           inca = kd1*ldab
           kdn = min( n-1, kd )
           if( upper ) then
              if( kd>1_ilp ) then
                 ! reduce to tridiagonal form, working with upper triangle
                 nr = 0_ilp
                 j1 = kdn + 2_ilp
                 j2 = 1_ilp
                 loop_90: do i = 1, n - 2
                    ! reduce i-th row of matrix to tridiagonal form
                    loop_80: do k = kdn + 1, 2, -1
                       j1 = j1 + kdn
                       j2 = j2 + kdn
                       if( nr>0_ilp ) then
                          ! generate plane rotations to annihilate nonzero
                          ! elements which have been created outside the band
                          call stdlib_slargv( nr, ab( 1_ilp, j1-1 ), inca, work( j1 ),kd1, d( j1 ), &
                                    kd1 )
                          ! apply rotations from the right
                          ! dependent on the the number of diagonals either
                          ! stdlib_slartv or stdlib_srot is used
                          if( nr>=2_ilp*kd-1 ) then
                             do l = 1, kd - 1
                                call stdlib_slartv( nr, ab( l+1, j1-1 ), inca,ab( l, j1 ), inca, &
                                          d( j1 ),work( j1 ), kd1 )
                             end do
                          else
                             jend = j1 + ( nr-1 )*kd1
                             do jinc = j1, jend, kd1
                                call stdlib_srot( kdm1, ab( 2_ilp, jinc-1 ), 1_ilp,ab( 1_ilp, jinc ), 1_ilp, d( &
                                          jinc ),work( jinc ) )
                             end do
                          end if
                       end if
                       if( k>2_ilp ) then
                          if( k<=n-i+1 ) then
                             ! generate plane rotation to annihilate a(i,i+k-1)
                             ! within the band
                             call stdlib_slartg( ab( kd-k+3, i+k-2 ),ab( kd-k+2, i+k-1 ), d( i+k-&
                                       1_ilp ),work( i+k-1 ), temp )
                             ab( kd-k+3, i+k-2 ) = temp
                             ! apply rotation from the right
                             call stdlib_srot( k-3, ab( kd-k+4, i+k-2 ), 1_ilp,ab( kd-k+3, i+k-1 ), 1_ilp,&
                                        d( i+k-1 ),work( i+k-1 ) )
                          end if
                          nr = nr + 1_ilp
                          j1 = j1 - kdn - 1_ilp
                       end if
                       ! apply plane rotations from both sides to diagonal
                       ! blocks
                       if( nr>0_ilp )call stdlib_slar2v( nr, ab( kd1, j1-1 ), ab( kd1, j1 ),ab( kd, &
                                 j1 ), inca, d( j1 ),work( j1 ), kd1 )
                       ! apply plane rotations from the left
                       if( nr>0_ilp ) then
                          if( 2_ilp*kd-1<nr ) then
                          ! dependent on the the number of diagonals either
                          ! stdlib_slartv or stdlib_srot is used
                             do l = 1, kd - 1
                                if( j2+l>n ) then
                                   nrt = nr - 1_ilp
                                else
                                   nrt = nr
                                end if
                                if( nrt>0_ilp )call stdlib_slartv( nrt, ab( kd-l, j1+l ), inca,ab( kd-&
                                          l+1, j1+l ), inca,d( j1 ), work( j1 ), kd1 )
                             end do
                          else
                             j1end = j1 + kd1*( nr-2 )
                             if( j1end>=j1 ) then
                                do jin = j1, j1end, kd1
                                   call stdlib_srot( kd-1, ab( kd-1, jin+1 ), incx,ab( kd, jin+1 )&
                                             , incx,d( jin ), work( jin ) )
                                end do
                             end if
                             lend = min( kdm1, n-j2 )
                             last = j1end + kd1
                             if( lend>0_ilp )call stdlib_srot( lend, ab( kd-1, last+1 ), incx,ab( kd, &
                                       last+1 ), incx, d( last ),work( last ) )
                          end if
                       end if
                       if( wantq ) then
                          ! accumulate product of plane rotations in q
                          if( initq ) then
                       ! take advantage of the fact that q was
                       ! initially the identity matrix
                             iqend = max( iqend, j2 )
                             i2 = max( 0_ilp, k-3 )
                             iqaend = 1_ilp + i*kd
                             if( k==2_ilp )iqaend = iqaend + kd
                             iqaend = min( iqaend, iqend )
                             do j = j1, j2, kd1
                                ibl = i - i2 / kdm1
                                i2 = i2 + 1_ilp
                                iqb = max( 1_ilp, j-ibl )
                                nq = 1_ilp + iqaend - iqb
                                iqaend = min( iqaend+kd, iqend )
                                call stdlib_srot( nq, q( iqb, j-1 ), 1_ilp, q( iqb, j ),1_ilp, d( j ), &
                                          work( j ) )
                             end do
                          else
                             do j = j1, j2, kd1
                                call stdlib_srot( n, q( 1_ilp, j-1 ), 1_ilp, q( 1_ilp, j ), 1_ilp,d( j ), work( j &
                                          ) )
                             end do
                          end if
                       end if
                       if( j2+kdn>n ) then
                          ! adjust j2 to keep within the bounds of the matrix
                          nr = nr - 1_ilp
                          j2 = j2 - kdn - 1_ilp
                       end if
                       do j = j1, j2, kd1
                          ! create nonzero element a(j-1,j+kd) outside the band
                          ! and store it in work
                          work( j+kd ) = work( j )*ab( 1_ilp, j+kd )
                          ab( 1_ilp, j+kd ) = d( j )*ab( 1_ilp, j+kd )
                       end do
                    end do loop_80
                 end do loop_90
              end if
              if( kd>0_ilp ) then
                 ! copy off-diagonal elements to e
                 do i = 1, n - 1
                    e( i ) = ab( kd, i+1 )
                 end do
              else
                 ! set e to zero if original matrix was diagonal
                 do i = 1, n - 1
                    e( i ) = zero
                 end do
              end if
              ! copy diagonal elements to d
              do i = 1, n
                 d( i ) = ab( kd1, i )
              end do
           else
              if( kd>1_ilp ) then
                 ! reduce to tridiagonal form, working with lower triangle
                 nr = 0_ilp
                 j1 = kdn + 2_ilp
                 j2 = 1_ilp
                 loop_210: do i = 1, n - 2
                    ! reduce i-th column of matrix to tridiagonal form
                    loop_200: do k = kdn + 1, 2, -1
                       j1 = j1 + kdn
                       j2 = j2 + kdn
                       if( nr>0_ilp ) then
                          ! generate plane rotations to annihilate nonzero
                          ! elements which have been created outside the band
                          call stdlib_slargv( nr, ab( kd1, j1-kd1 ), inca,work( j1 ), kd1, d( j1 )&
                                    , kd1 )
                          ! apply plane rotations from one side
                          ! dependent on the the number of diagonals either
                          ! stdlib_slartv or stdlib_srot is used
                          if( nr>2_ilp*kd-1 ) then
                             do l = 1, kd - 1
                                call stdlib_slartv( nr, ab( kd1-l, j1-kd1+l ), inca,ab( kd1-l+1, &
                                          j1-kd1+l ), inca,d( j1 ), work( j1 ), kd1 )
                             end do
                          else
                             jend = j1 + kd1*( nr-1 )
                             do jinc = j1, jend, kd1
                                call stdlib_srot( kdm1, ab( kd, jinc-kd ), incx,ab( kd1, jinc-kd )&
                                          , incx,d( jinc ), work( jinc ) )
                             end do
                          end if
                       end if
                       if( k>2_ilp ) then
                          if( k<=n-i+1 ) then
                             ! generate plane rotation to annihilate a(i+k-1,i)
                             ! within the band
                             call stdlib_slartg( ab( k-1, i ), ab( k, i ),d( i+k-1 ), work( i+k-1 &
                                       ), temp )
                             ab( k-1, i ) = temp
                             ! apply rotation from the left
                             call stdlib_srot( k-3, ab( k-2, i+1 ), ldab-1,ab( k-1, i+1 ), ldab-1,&
                                        d( i+k-1 ),work( i+k-1 ) )
                          end if
                          nr = nr + 1_ilp
                          j1 = j1 - kdn - 1_ilp
                       end if
                       ! apply plane rotations from both sides to diagonal
                       ! blocks
                       if( nr>0_ilp )call stdlib_slar2v( nr, ab( 1_ilp, j1-1 ), ab( 1_ilp, j1 ),ab( 2_ilp, j1-1 ),&
                                  inca, d( j1 ),work( j1 ), kd1 )
                       ! apply plane rotations from the right
                          ! dependent on the the number of diagonals either
                          ! stdlib_slartv or stdlib_srot is used
                       if( nr>0_ilp ) then
                          if( nr>2_ilp*kd-1 ) then
                             do l = 1, kd - 1
                                if( j2+l>n ) then
                                   nrt = nr - 1_ilp
                                else
                                   nrt = nr
                                end if
                                if( nrt>0_ilp )call stdlib_slartv( nrt, ab( l+2, j1-1 ), inca,ab( l+1,&
                                           j1 ), inca, d( j1 ),work( j1 ), kd1 )
                             end do
                          else
                             j1end = j1 + kd1*( nr-2 )
                             if( j1end>=j1 ) then
                                do j1inc = j1, j1end, kd1
                                   call stdlib_srot( kdm1, ab( 3_ilp, j1inc-1 ), 1_ilp,ab( 2_ilp, j1inc ), 1_ilp, &
                                             d( j1inc ),work( j1inc ) )
                                end do
                             end if
                             lend = min( kdm1, n-j2 )
                             last = j1end + kd1
                             if( lend>0_ilp )call stdlib_srot( lend, ab( 3_ilp, last-1 ), 1_ilp,ab( 2_ilp, last ),&
                                        1_ilp, d( last ),work( last ) )
                          end if
                       end if
                       if( wantq ) then
                          ! accumulate product of plane rotations in q
                          if( initq ) then
                       ! take advantage of the fact that q was
                       ! initially the identity matrix
                             iqend = max( iqend, j2 )
                             i2 = max( 0_ilp, k-3 )
                             iqaend = 1_ilp + i*kd
                             if( k==2_ilp )iqaend = iqaend + kd
                             iqaend = min( iqaend, iqend )
                             do j = j1, j2, kd1
                                ibl = i - i2 / kdm1
                                i2 = i2 + 1_ilp
                                iqb = max( 1_ilp, j-ibl )
                                nq = 1_ilp + iqaend - iqb
                                iqaend = min( iqaend+kd, iqend )
                                call stdlib_srot( nq, q( iqb, j-1 ), 1_ilp, q( iqb, j ),1_ilp, d( j ), &
                                          work( j ) )
                             end do
                          else
                             do j = j1, j2, kd1
                                call stdlib_srot( n, q( 1_ilp, j-1 ), 1_ilp, q( 1_ilp, j ), 1_ilp,d( j ), work( j &
                                          ) )
                             end do
                          end if
                       end if
                       if( j2+kdn>n ) then
                          ! adjust j2 to keep within the bounds of the matrix
                          nr = nr - 1_ilp
                          j2 = j2 - kdn - 1_ilp
                       end if
                       do j = j1, j2, kd1
                          ! create nonzero element a(j+kd,j-1) outside the
                          ! band and store it in work
                          work( j+kd ) = work( j )*ab( kd1, j )
                          ab( kd1, j ) = d( j )*ab( kd1, j )
                       end do
                    end do loop_200
                 end do loop_210
              end if
              if( kd>0_ilp ) then
                 ! copy off-diagonal elements to e
                 do i = 1, n - 1
                    e( i ) = ab( 2_ilp, i )
                 end do
              else
                 ! set e to zero if original matrix was diagonal
                 do i = 1, n - 1
                    e( i ) = zero
                 end do
              end if
              ! copy diagonal elements to d
              do i = 1, n
                 d( i ) = ab( 1_ilp, i )
              end do
           end if
           return
     end subroutine stdlib_ssbtrd

     pure module subroutine stdlib_dsbtrd( vect, uplo, n, kd, ab, ldab, d, e, q, ldq,work, info )
     !! DSBTRD reduces a real symmetric band matrix A to symmetric
     !! tridiagonal form T by an orthogonal similarity transformation:
     !! Q**T * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldq, n
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*), q(ldq,*)
           real(dp), intent(out) :: d(*), e(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: initq, upper, wantq
           integer(ilp) :: i, i2, ibl, inca, incx, iqaend, iqb, iqend, j, j1, j1end, j1inc, j2, &
                     jend, jin, jinc, k, kd1, kdm1, kdn, l, last, lend, nq, nr, nrt
           real(dp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           initq = stdlib_lsame( vect, 'V' )
           wantq = initq .or. stdlib_lsame( vect, 'U' )
           upper = stdlib_lsame( uplo, 'U' )
           kd1 = kd + 1_ilp
           kdm1 = kd - 1_ilp
           incx = ldab - 1_ilp
           iqend = 1_ilp
           info = 0_ilp
           if( .not.wantq .and. .not.stdlib_lsame( vect, 'N' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd1 ) then
              info = -6_ilp
           else if( ldq<max( 1_ilp, n ) .and. wantq ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSBTRD', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! initialize q to the unit matrix, if needed
           if( initq )call stdlib_dlaset( 'FULL', n, n, zero, one, q, ldq )
           ! wherever possible, plane rotations are generated and applied in
           ! vector operations of length nr over the index set j1:j2:kd1.
           ! the cosines and sines of the plane rotations are stored in the
           ! arrays d and work.
           inca = kd1*ldab
           kdn = min( n-1, kd )
           if( upper ) then
              if( kd>1_ilp ) then
                 ! reduce to tridiagonal form, working with upper triangle
                 nr = 0_ilp
                 j1 = kdn + 2_ilp
                 j2 = 1_ilp
                 loop_90: do i = 1, n - 2
                    ! reduce i-th row of matrix to tridiagonal form
                    loop_80: do k = kdn + 1, 2, -1
                       j1 = j1 + kdn
                       j2 = j2 + kdn
                       if( nr>0_ilp ) then
                          ! generate plane rotations to annihilate nonzero
                          ! elements which have been created outside the band
                          call stdlib_dlargv( nr, ab( 1_ilp, j1-1 ), inca, work( j1 ),kd1, d( j1 ), &
                                    kd1 )
                          ! apply rotations from the right
                          ! dependent on the the number of diagonals either
                          ! stdlib_dlartv or stdlib_drot is used
                          if( nr>=2_ilp*kd-1 ) then
                             do l = 1, kd - 1
                                call stdlib_dlartv( nr, ab( l+1, j1-1 ), inca,ab( l, j1 ), inca, &
                                          d( j1 ),work( j1 ), kd1 )
                             end do
                          else
                             jend = j1 + ( nr-1 )*kd1
                             do jinc = j1, jend, kd1
                                call stdlib_drot( kdm1, ab( 2_ilp, jinc-1 ), 1_ilp,ab( 1_ilp, jinc ), 1_ilp, d( &
                                          jinc ),work( jinc ) )
                             end do
                          end if
                       end if
                       if( k>2_ilp ) then
                          if( k<=n-i+1 ) then
                             ! generate plane rotation to annihilate a(i,i+k-1)
                             ! within the band
                             call stdlib_dlartg( ab( kd-k+3, i+k-2 ),ab( kd-k+2, i+k-1 ), d( i+k-&
                                       1_ilp ),work( i+k-1 ), temp )
                             ab( kd-k+3, i+k-2 ) = temp
                             ! apply rotation from the right
                             call stdlib_drot( k-3, ab( kd-k+4, i+k-2 ), 1_ilp,ab( kd-k+3, i+k-1 ), 1_ilp,&
                                        d( i+k-1 ),work( i+k-1 ) )
                          end if
                          nr = nr + 1_ilp
                          j1 = j1 - kdn - 1_ilp
                       end if
                       ! apply plane rotations from both sides to diagonal
                       ! blocks
                       if( nr>0_ilp )call stdlib_dlar2v( nr, ab( kd1, j1-1 ), ab( kd1, j1 ),ab( kd, &
                                 j1 ), inca, d( j1 ),work( j1 ), kd1 )
                       ! apply plane rotations from the left
                       if( nr>0_ilp ) then
                          if( 2_ilp*kd-1<nr ) then
                          ! dependent on the the number of diagonals either
                          ! stdlib_dlartv or stdlib_drot is used
                             do l = 1, kd - 1
                                if( j2+l>n ) then
                                   nrt = nr - 1_ilp
                                else
                                   nrt = nr
                                end if
                                if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( kd-l, j1+l ), inca,ab( kd-&
                                          l+1, j1+l ), inca,d( j1 ), work( j1 ), kd1 )
                             end do
                          else
                             j1end = j1 + kd1*( nr-2 )
                             if( j1end>=j1 ) then
                                do jin = j1, j1end, kd1
                                   call stdlib_drot( kd-1, ab( kd-1, jin+1 ), incx,ab( kd, jin+1 )&
                                             , incx,d( jin ), work( jin ) )
                                end do
                             end if
                             lend = min( kdm1, n-j2 )
                             last = j1end + kd1
                             if( lend>0_ilp )call stdlib_drot( lend, ab( kd-1, last+1 ), incx,ab( kd, &
                                       last+1 ), incx, d( last ),work( last ) )
                          end if
                       end if
                       if( wantq ) then
                          ! accumulate product of plane rotations in q
                          if( initq ) then
                       ! take advantage of the fact that q was
                       ! initially the identity matrix
                             iqend = max( iqend, j2 )
                             i2 = max( 0_ilp, k-3 )
                             iqaend = 1_ilp + i*kd
                             if( k==2_ilp )iqaend = iqaend + kd
                             iqaend = min( iqaend, iqend )
                             do j = j1, j2, kd1
                                ibl = i - i2 / kdm1
                                i2 = i2 + 1_ilp
                                iqb = max( 1_ilp, j-ibl )
                                nq = 1_ilp + iqaend - iqb
                                iqaend = min( iqaend+kd, iqend )
                                call stdlib_drot( nq, q( iqb, j-1 ), 1_ilp, q( iqb, j ),1_ilp, d( j ), &
                                          work( j ) )
                             end do
                          else
                             do j = j1, j2, kd1
                                call stdlib_drot( n, q( 1_ilp, j-1 ), 1_ilp, q( 1_ilp, j ), 1_ilp,d( j ), work( j &
                                          ) )
                             end do
                          end if
                       end if
                       if( j2+kdn>n ) then
                          ! adjust j2 to keep within the bounds of the matrix
                          nr = nr - 1_ilp
                          j2 = j2 - kdn - 1_ilp
                       end if
                       do j = j1, j2, kd1
                          ! create nonzero element a(j-1,j+kd) outside the band
                          ! and store it in work
                          work( j+kd ) = work( j )*ab( 1_ilp, j+kd )
                          ab( 1_ilp, j+kd ) = d( j )*ab( 1_ilp, j+kd )
                       end do
                    end do loop_80
                 end do loop_90
              end if
              if( kd>0_ilp ) then
                 ! copy off-diagonal elements to e
                 do i = 1, n - 1
                    e( i ) = ab( kd, i+1 )
                 end do
              else
                 ! set e to zero if original matrix was diagonal
                 do i = 1, n - 1
                    e( i ) = zero
                 end do
              end if
              ! copy diagonal elements to d
              do i = 1, n
                 d( i ) = ab( kd1, i )
              end do
           else
              if( kd>1_ilp ) then
                 ! reduce to tridiagonal form, working with lower triangle
                 nr = 0_ilp
                 j1 = kdn + 2_ilp
                 j2 = 1_ilp
                 loop_210: do i = 1, n - 2
                    ! reduce i-th column of matrix to tridiagonal form
                    loop_200: do k = kdn + 1, 2, -1
                       j1 = j1 + kdn
                       j2 = j2 + kdn
                       if( nr>0_ilp ) then
                          ! generate plane rotations to annihilate nonzero
                          ! elements which have been created outside the band
                          call stdlib_dlargv( nr, ab( kd1, j1-kd1 ), inca,work( j1 ), kd1, d( j1 )&
                                    , kd1 )
                          ! apply plane rotations from one side
                          ! dependent on the the number of diagonals either
                          ! stdlib_dlartv or stdlib_drot is used
                          if( nr>2_ilp*kd-1 ) then
                             do l = 1, kd - 1
                                call stdlib_dlartv( nr, ab( kd1-l, j1-kd1+l ), inca,ab( kd1-l+1, &
                                          j1-kd1+l ), inca,d( j1 ), work( j1 ), kd1 )
                             end do
                          else
                             jend = j1 + kd1*( nr-1 )
                             do jinc = j1, jend, kd1
                                call stdlib_drot( kdm1, ab( kd, jinc-kd ), incx,ab( kd1, jinc-kd )&
                                          , incx,d( jinc ), work( jinc ) )
                             end do
                          end if
                       end if
                       if( k>2_ilp ) then
                          if( k<=n-i+1 ) then
                             ! generate plane rotation to annihilate a(i+k-1,i)
                             ! within the band
                             call stdlib_dlartg( ab( k-1, i ), ab( k, i ),d( i+k-1 ), work( i+k-1 &
                                       ), temp )
                             ab( k-1, i ) = temp
                             ! apply rotation from the left
                             call stdlib_drot( k-3, ab( k-2, i+1 ), ldab-1,ab( k-1, i+1 ), ldab-1,&
                                        d( i+k-1 ),work( i+k-1 ) )
                          end if
                          nr = nr + 1_ilp
                          j1 = j1 - kdn - 1_ilp
                       end if
                       ! apply plane rotations from both sides to diagonal
                       ! blocks
                       if( nr>0_ilp )call stdlib_dlar2v( nr, ab( 1_ilp, j1-1 ), ab( 1_ilp, j1 ),ab( 2_ilp, j1-1 ),&
                                  inca, d( j1 ),work( j1 ), kd1 )
                       ! apply plane rotations from the right
                          ! dependent on the the number of diagonals either
                          ! stdlib_dlartv or stdlib_drot is used
                       if( nr>0_ilp ) then
                          if( nr>2_ilp*kd-1 ) then
                             do l = 1, kd - 1
                                if( j2+l>n ) then
                                   nrt = nr - 1_ilp
                                else
                                   nrt = nr
                                end if
                                if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( l+2, j1-1 ), inca,ab( l+1,&
                                           j1 ), inca, d( j1 ),work( j1 ), kd1 )
                             end do
                          else
                             j1end = j1 + kd1*( nr-2 )
                             if( j1end>=j1 ) then
                                do j1inc = j1, j1end, kd1
                                   call stdlib_drot( kdm1, ab( 3_ilp, j1inc-1 ), 1_ilp,ab( 2_ilp, j1inc ), 1_ilp, &
                                             d( j1inc ),work( j1inc ) )
                                end do
                             end if
                             lend = min( kdm1, n-j2 )
                             last = j1end + kd1
                             if( lend>0_ilp )call stdlib_drot( lend, ab( 3_ilp, last-1 ), 1_ilp,ab( 2_ilp, last ),&
                                        1_ilp, d( last ),work( last ) )
                          end if
                       end if
                       if( wantq ) then
                          ! accumulate product of plane rotations in q
                          if( initq ) then
                       ! take advantage of the fact that q was
                       ! initially the identity matrix
                             iqend = max( iqend, j2 )
                             i2 = max( 0_ilp, k-3 )
                             iqaend = 1_ilp + i*kd
                             if( k==2_ilp )iqaend = iqaend + kd
                             iqaend = min( iqaend, iqend )
                             do j = j1, j2, kd1
                                ibl = i - i2 / kdm1
                                i2 = i2 + 1_ilp
                                iqb = max( 1_ilp, j-ibl )
                                nq = 1_ilp + iqaend - iqb
                                iqaend = min( iqaend+kd, iqend )
                                call stdlib_drot( nq, q( iqb, j-1 ), 1_ilp, q( iqb, j ),1_ilp, d( j ), &
                                          work( j ) )
                             end do
                          else
                             do j = j1, j2, kd1
                                call stdlib_drot( n, q( 1_ilp, j-1 ), 1_ilp, q( 1_ilp, j ), 1_ilp,d( j ), work( j &
                                          ) )
                             end do
                          end if
                       end if
                       if( j2+kdn>n ) then
                          ! adjust j2 to keep within the bounds of the matrix
                          nr = nr - 1_ilp
                          j2 = j2 - kdn - 1_ilp
                       end if
                       do j = j1, j2, kd1
                          ! create nonzero element a(j+kd,j-1) outside the
                          ! band and store it in work
                          work( j+kd ) = work( j )*ab( kd1, j )
                          ab( kd1, j ) = d( j )*ab( kd1, j )
                       end do
                    end do loop_200
                 end do loop_210
              end if
              if( kd>0_ilp ) then
                 ! copy off-diagonal elements to e
                 do i = 1, n - 1
                    e( i ) = ab( 2_ilp, i )
                 end do
              else
                 ! set e to zero if original matrix was diagonal
                 do i = 1, n - 1
                    e( i ) = zero
                 end do
              end if
              ! copy diagonal elements to d
              do i = 1, n
                 d( i ) = ab( 1_ilp, i )
              end do
           end if
           return
     end subroutine stdlib_dsbtrd




     pure module subroutine stdlib_chptrd( uplo, n, ap, d, e, tau, info )
     !! CHPTRD reduces a complex Hermitian matrix A stored in packed form to
     !! real symmetric tridiagonal form T by a unitary similarity
     !! transformation: Q**H * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(out) :: tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, i1, i1i1, ii
           complex(sp) :: alpha, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHPTRD', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           if( upper ) then
              ! reduce the upper triangle of a.
              ! i1 is the index in ap of a(1,i+1).
              i1 = n*( n-1 ) / 2_ilp + 1_ilp
              ap( i1+n-1 ) = real( ap( i1+n-1 ),KIND=sp)
              do i = n - 1, 1, -1
                 ! generate elementary reflector h(i) = i - tau * v * v**h
                 ! to annihilate a(1:i-1,i+1)
                 alpha = ap( i1+i-1 )
                 call stdlib_clarfg( i, alpha, ap( i1 ), 1_ilp, taui )
                 e( i ) = real( alpha,KIND=sp)
                 if( taui/=czero ) then
                    ! apply h(i) from both sides to a(1:i,1:i)
                    ap( i1+i-1 ) = cone
                    ! compute  y := tau * a * v  storing y in tau(1:i)
                    call stdlib_chpmv( uplo, i, taui, ap, ap( i1 ), 1_ilp, czero, tau,1_ilp )
                    ! compute  w := y - 1/2 * tau * (y**h *v) * v
                    alpha = -chalf*taui*stdlib_cdotc( i, tau, 1_ilp, ap( i1 ), 1_ilp )
                    call stdlib_caxpy( i, alpha, ap( i1 ), 1_ilp, tau, 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**h - w * v**h
                    call stdlib_chpr2( uplo, i, -cone, ap( i1 ), 1_ilp, tau, 1_ilp, ap )
                 end if
                 ap( i1+i-1 ) = e( i )
                 d( i+1 ) = real( ap( i1+i ),KIND=sp)
                 tau( i ) = taui
                 i1 = i1 - i
              end do
              d( 1_ilp ) = real( ap( 1_ilp ),KIND=sp)
           else
              ! reduce the lower triangle of a. ii is the index in ap of
              ! a(i,i) and i1i1 is the index of a(i+1,i+1).
              ii = 1_ilp
              ap( 1_ilp ) = real( ap( 1_ilp ),KIND=sp)
              do i = 1, n - 1
                 i1i1 = ii + n - i + 1_ilp
                 ! generate elementary reflector h(i) = i - tau * v * v**h
                 ! to annihilate a(i+2:n,i)
                 alpha = ap( ii+1 )
                 call stdlib_clarfg( n-i, alpha, ap( ii+2 ), 1_ilp, taui )
                 e( i ) = real( alpha,KIND=sp)
                 if( taui/=czero ) then
                    ! apply h(i) from both sides to a(i+1:n,i+1:n)
                    ap( ii+1 ) = cone
                    ! compute  y := tau * a * v  storing y in tau(i:n-1)
                    call stdlib_chpmv( uplo, n-i, taui, ap( i1i1 ), ap( ii+1 ), 1_ilp,czero, tau( i ),&
                               1_ilp )
                    ! compute  w := y - 1/2 * tau * (y**h *v) * v
                    alpha = -chalf*taui*stdlib_cdotc( n-i, tau( i ), 1_ilp, ap( ii+1 ),1_ilp )
                    call stdlib_caxpy( n-i, alpha, ap( ii+1 ), 1_ilp, tau( i ), 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**h - w * v**h
                    call stdlib_chpr2( uplo, n-i, -cone, ap( ii+1 ), 1_ilp, tau( i ), 1_ilp,ap( i1i1 ) )
                              
                 end if
                 ap( ii+1 ) = e( i )
                 d( i ) = real( ap( ii ),KIND=sp)
                 tau( i ) = taui
                 ii = i1i1
              end do
              d( n ) = real( ap( ii ),KIND=sp)
           end if
           return
     end subroutine stdlib_chptrd

     pure module subroutine stdlib_zhptrd( uplo, n, ap, d, e, tau, info )
     !! ZHPTRD reduces a complex Hermitian matrix A stored in packed form to
     !! real symmetric tridiagonal form T by a unitary similarity
     !! transformation: Q**H * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(out) :: tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, i1, i1i1, ii
           complex(dp) :: alpha, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHPTRD', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           if( upper ) then
              ! reduce the upper triangle of a.
              ! i1 is the index in ap of a(1,i+1).
              i1 = n*( n-1 ) / 2_ilp + 1_ilp
              ap( i1+n-1 ) = real( ap( i1+n-1 ),KIND=dp)
              do i = n - 1, 1, -1
                 ! generate elementary reflector h(i) = i - tau * v * v**h
                 ! to annihilate a(1:i-1,i+1)
                 alpha = ap( i1+i-1 )
                 call stdlib_zlarfg( i, alpha, ap( i1 ), 1_ilp, taui )
                 e( i ) = real( alpha,KIND=dp)
                 if( taui/=czero ) then
                    ! apply h(i) from both sides to a(1:i,1:i)
                    ap( i1+i-1 ) = cone
                    ! compute  y := tau * a * v  storing y in tau(1:i)
                    call stdlib_zhpmv( uplo, i, taui, ap, ap( i1 ), 1_ilp, czero, tau,1_ilp )
                    ! compute  w := y - 1/2 * tau * (y**h *v) * v
                    alpha = -chalf*taui*stdlib_zdotc( i, tau, 1_ilp, ap( i1 ), 1_ilp )
                    call stdlib_zaxpy( i, alpha, ap( i1 ), 1_ilp, tau, 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**h - w * v**h
                    call stdlib_zhpr2( uplo, i, -cone, ap( i1 ), 1_ilp, tau, 1_ilp, ap )
                 end if
                 ap( i1+i-1 ) = e( i )
                 d( i+1 ) = real( ap( i1+i ),KIND=dp)
                 tau( i ) = taui
                 i1 = i1 - i
              end do
              d( 1_ilp ) = real( ap( 1_ilp ),KIND=dp)
           else
              ! reduce the lower triangle of a. ii is the index in ap of
              ! a(i,i) and i1i1 is the index of a(i+1,i+1).
              ii = 1_ilp
              ap( 1_ilp ) = real( ap( 1_ilp ),KIND=dp)
              do i = 1, n - 1
                 i1i1 = ii + n - i + 1_ilp
                 ! generate elementary reflector h(i) = i - tau * v * v**h
                 ! to annihilate a(i+2:n,i)
                 alpha = ap( ii+1 )
                 call stdlib_zlarfg( n-i, alpha, ap( ii+2 ), 1_ilp, taui )
                 e( i ) = real( alpha,KIND=dp)
                 if( taui/=czero ) then
                    ! apply h(i) from both sides to a(i+1:n,i+1:n)
                    ap( ii+1 ) = cone
                    ! compute  y := tau * a * v  storing y in tau(i:n-1)
                    call stdlib_zhpmv( uplo, n-i, taui, ap( i1i1 ), ap( ii+1 ), 1_ilp,czero, tau( i ),&
                               1_ilp )
                    ! compute  w := y - 1/2 * tau * (y**h *v) * v
                    alpha = -chalf*taui*stdlib_zdotc( n-i, tau( i ), 1_ilp, ap( ii+1 ),1_ilp )
                    call stdlib_zaxpy( n-i, alpha, ap( ii+1 ), 1_ilp, tau( i ), 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**h - w * v**h
                    call stdlib_zhpr2( uplo, n-i, -cone, ap( ii+1 ), 1_ilp, tau( i ), 1_ilp,ap( i1i1 ) )
                              
                 end if
                 ap( ii+1 ) = e( i )
                 d( i ) = real( ap( ii ),KIND=dp)
                 tau( i ) = taui
                 ii = i1i1
              end do
              d( n ) = real( ap( ii ),KIND=dp)
           end if
           return
     end subroutine stdlib_zhptrd




     pure module subroutine stdlib_cupgtr( uplo, n, ap, tau, q, ldq, work, info )
     !! CUPGTR generates a complex unitary matrix Q which is defined as the
     !! product of n-1 elementary reflectors H(i) of order n, as returned by
     !! CHPTRD using packed storage:
     !! if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, n
           ! Array Arguments 
           complex(sp), intent(in) :: ap(*), tau(*)
           complex(sp), intent(out) :: q(ldq,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, iinfo, ij, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUPGTR', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! q was determined by a call to stdlib_chptrd with uplo = 'u'
              ! unpack the vectors which define the elementary reflectors and
              ! set the last row and column of q equal to those of the unit
              ! matrix
              ij = 2_ilp
              do j = 1, n - 1
                 do i = 1, j - 1
                    q( i, j ) = ap( ij )
                    ij = ij + 1_ilp
                 end do
                 ij = ij + 2_ilp
                 q( n, j ) = czero
              end do
              do i = 1, n - 1
                 q( i, n ) = czero
              end do
              q( n, n ) = cone
              ! generate q(1:n-1,1:n-1)
              call stdlib_cung2l( n-1, n-1, n-1, q, ldq, tau, work, iinfo )
           else
              ! q was determined by a call to stdlib_chptrd with uplo = 'l'.
              ! unpack the vectors which define the elementary reflectors and
              ! set the first row and column of q equal to those of the unit
              ! matrix
              q( 1_ilp, 1_ilp ) = cone
              do i = 2, n
                 q( i, 1_ilp ) = czero
              end do
              ij = 3_ilp
              do j = 2, n
                 q( 1_ilp, j ) = czero
                 do i = j + 1, n
                    q( i, j ) = ap( ij )
                    ij = ij + 1_ilp
                 end do
                 ij = ij + 2_ilp
              end do
              if( n>1_ilp ) then
                 ! generate q(2:n,2:n)
                 call stdlib_cung2r( n-1, n-1, n-1, q( 2_ilp, 2_ilp ), ldq, tau, work,iinfo )
              end if
           end if
           return
     end subroutine stdlib_cupgtr

     pure module subroutine stdlib_zupgtr( uplo, n, ap, tau, q, ldq, work, info )
     !! ZUPGTR generates a complex unitary matrix Q which is defined as the
     !! product of n-1 elementary reflectors H(i) of order n, as returned by
     !! ZHPTRD using packed storage:
     !! if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, n
           ! Array Arguments 
           complex(dp), intent(in) :: ap(*), tau(*)
           complex(dp), intent(out) :: q(ldq,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, iinfo, ij, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( ldq<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUPGTR', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( upper ) then
              ! q was determined by a call to stdlib_zhptrd with uplo = 'u'
              ! unpack the vectors which define the elementary reflectors and
              ! set the last row and column of q equal to those of the unit
              ! matrix
              ij = 2_ilp
              do j = 1, n - 1
                 do i = 1, j - 1
                    q( i, j ) = ap( ij )
                    ij = ij + 1_ilp
                 end do
                 ij = ij + 2_ilp
                 q( n, j ) = czero
              end do
              do i = 1, n - 1
                 q( i, n ) = czero
              end do
              q( n, n ) = cone
              ! generate q(1:n-1,1:n-1)
              call stdlib_zung2l( n-1, n-1, n-1, q, ldq, tau, work, iinfo )
           else
              ! q was determined by a call to stdlib_zhptrd with uplo = 'l'.
              ! unpack the vectors which define the elementary reflectors and
              ! set the first row and column of q equal to those of the unit
              ! matrix
              q( 1_ilp, 1_ilp ) = cone
              do i = 2, n
                 q( i, 1_ilp ) = czero
              end do
              ij = 3_ilp
              do j = 2, n
                 q( 1_ilp, j ) = czero
                 do i = j + 1, n
                    q( i, j ) = ap( ij )
                    ij = ij + 1_ilp
                 end do
                 ij = ij + 2_ilp
              end do
              if( n>1_ilp ) then
                 ! generate q(2:n,2:n)
                 call stdlib_zung2r( n-1, n-1, n-1, q( 2_ilp, 2_ilp ), ldq, tau, work,iinfo )
              end if
           end if
           return
     end subroutine stdlib_zupgtr




     pure module subroutine stdlib_cupmtr( side, uplo, trans, m, n, ap, tau, c, ldc, work,info )
     !! CUPMTR overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! nq-1 elementary reflectors, as returned by CHPTRD using packed
     !! storage:
     !! if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: ap(*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: forwrd, left, notran, upper
           integer(ilp) :: i, i1, i2, i3, ic, ii, jc, mi, ni, nq
           complex(sp) :: aii, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           upper = stdlib_lsame( uplo, 'U' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUPMTR', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           if( upper ) then
              ! q was determined by a call to stdlib_chptrd with uplo = 'u'
              forwrd = ( left .and. notran ) .or.( .not.left .and. .not.notran )
              if( forwrd ) then
                 i1 = 1_ilp
                 i2 = nq - 1_ilp
                 i3 = 1_ilp
                 ii = 2_ilp
              else
                 i1 = nq - 1_ilp
                 i2 = 1_ilp
                 i3 = -1_ilp
                 ii = nq*( nq+1 ) / 2_ilp - 1_ilp
              end if
              if( left ) then
                 ni = n
              else
                 mi = m
              end if
              do i = i1, i2, i3
                 if( left ) then
                    ! h(i) or h(i)**h is applied to c(1:i,1:n)
                    mi = i
                 else
                    ! h(i) or h(i)**h is applied to c(1:m,1:i)
                    ni = i
                 end if
                 ! apply h(i) or h(i)**h
                 if( notran ) then
                    taui = tau( i )
                 else
                    taui = conjg( tau( i ) )
                 end if
                 aii = ap( ii )
                 ap( ii ) = cone
                 call stdlib_clarf( side, mi, ni, ap( ii-i+1 ), 1_ilp, taui, c, ldc,work )
                 ap( ii ) = aii
                 if( forwrd ) then
                    ii = ii + i + 2_ilp
                 else
                    ii = ii - i - 1_ilp
                 end if
              end do
           else
              ! q was determined by a call to stdlib_chptrd with uplo = 'l'.
              forwrd = ( left .and. .not.notran ) .or.( .not.left .and. notran )
              if( forwrd ) then
                 i1 = 1_ilp
                 i2 = nq - 1_ilp
                 i3 = 1_ilp
                 ii = 2_ilp
              else
                 i1 = nq - 1_ilp
                 i2 = 1_ilp
                 i3 = -1_ilp
                 ii = nq*( nq+1 ) / 2_ilp - 1_ilp
              end if
              if( left ) then
                 ni = n
                 jc = 1_ilp
              else
                 mi = m
                 ic = 1_ilp
              end if
              loop_20: do i = i1, i2, i3
                 aii = ap( ii )
                 ap( ii ) = cone
                 if( left ) then
                    ! h(i) or h(i)**h is applied to c(i+1:m,1:n)
                    mi = m - i
                    ic = i + 1_ilp
                 else
                    ! h(i) or h(i)**h is applied to c(1:m,i+1:n)
                    ni = n - i
                    jc = i + 1_ilp
                 end if
                 ! apply h(i) or h(i)**h
                 if( notran ) then
                    taui = tau( i )
                 else
                    taui = conjg( tau( i ) )
                 end if
                 call stdlib_clarf( side, mi, ni, ap( ii ), 1_ilp, taui, c( ic, jc ),ldc, work )
                           
                 ap( ii ) = aii
                 if( forwrd ) then
                    ii = ii + nq - i + 1_ilp
                 else
                    ii = ii - nq + i - 2_ilp
                 end if
              end do loop_20
           end if
           return
     end subroutine stdlib_cupmtr

     pure module subroutine stdlib_zupmtr( side, uplo, trans, m, n, ap, tau, c, ldc, work,info )
     !! ZUPMTR overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! nq-1 elementary reflectors, as returned by ZHPTRD using packed
     !! storage:
     !! if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: ap(*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: forwrd, left, notran, upper
           integer(ilp) :: i, i1, i2, i3, ic, ii, jc, mi, ni, nq
           complex(dp) :: aii, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           upper = stdlib_lsame( uplo, 'U' )
           ! nq is the order of q
           if( left ) then
              nq = m
           else
              nq = n
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUPMTR', -info )
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           if( upper ) then
              ! q was determined by a call to stdlib_zhptrd with uplo = 'u'
              forwrd = ( left .and. notran ) .or.( .not.left .and. .not.notran )
              if( forwrd ) then
                 i1 = 1_ilp
                 i2 = nq - 1_ilp
                 i3 = 1_ilp
                 ii = 2_ilp
              else
                 i1 = nq - 1_ilp
                 i2 = 1_ilp
                 i3 = -1_ilp
                 ii = nq*( nq+1 ) / 2_ilp - 1_ilp
              end if
              if( left ) then
                 ni = n
              else
                 mi = m
              end if
              do i = i1, i2, i3
                 if( left ) then
                    ! h(i) or h(i)**h is applied to c(1:i,1:n)
                    mi = i
                 else
                    ! h(i) or h(i)**h is applied to c(1:m,1:i)
                    ni = i
                 end if
                 ! apply h(i) or h(i)**h
                 if( notran ) then
                    taui = tau( i )
                 else
                    taui = conjg( tau( i ) )
                 end if
                 aii = ap( ii )
                 ap( ii ) = cone
                 call stdlib_zlarf( side, mi, ni, ap( ii-i+1 ), 1_ilp, taui, c, ldc,work )
                 ap( ii ) = aii
                 if( forwrd ) then
                    ii = ii + i + 2_ilp
                 else
                    ii = ii - i - 1_ilp
                 end if
              end do
           else
              ! q was determined by a call to stdlib_zhptrd with uplo = 'l'.
              forwrd = ( left .and. .not.notran ) .or.( .not.left .and. notran )
              if( forwrd ) then
                 i1 = 1_ilp
                 i2 = nq - 1_ilp
                 i3 = 1_ilp
                 ii = 2_ilp
              else
                 i1 = nq - 1_ilp
                 i2 = 1_ilp
                 i3 = -1_ilp
                 ii = nq*( nq+1 ) / 2_ilp - 1_ilp
              end if
              if( left ) then
                 ni = n
                 jc = 1_ilp
              else
                 mi = m
                 ic = 1_ilp
              end if
              loop_20: do i = i1, i2, i3
                 aii = ap( ii )
                 ap( ii ) = cone
                 if( left ) then
                    ! h(i) or h(i)**h is applied to c(i+1:m,1:n)
                    mi = m - i
                    ic = i + 1_ilp
                 else
                    ! h(i) or h(i)**h is applied to c(1:m,i+1:n)
                    ni = n - i
                    jc = i + 1_ilp
                 end if
                 ! apply h(i) or h(i)**h
                 if( notran ) then
                    taui = tau( i )
                 else
                    taui = conjg( tau( i ) )
                 end if
                 call stdlib_zlarf( side, mi, ni, ap( ii ), 1_ilp, taui, c( ic, jc ),ldc, work )
                           
                 ap( ii ) = aii
                 if( forwrd ) then
                    ii = ii + nq - i + 1_ilp
                 else
                    ii = ii - nq + i - 2_ilp
                 end if
              end do loop_20
           end if
           return
     end subroutine stdlib_zupmtr




     pure module subroutine stdlib_chbtrd( vect, uplo, n, kd, ab, ldab, d, e, q, ldq,work, info )
     !! CHBTRD reduces a complex Hermitian band matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldq, n
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: ab(ldab,*), q(ldq,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: initq, upper, wantq
           integer(ilp) :: i, i2, ibl, inca, incx, iqaend, iqb, iqend, j, j1, j1end, j1inc, j2, &
                     jend, jin, jinc, k, kd1, kdm1, kdn, l, last, lend, nq, nr, nrt
           real(sp) :: abst
           complex(sp) :: t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           initq = stdlib_lsame( vect, 'V' )
           wantq = initq .or. stdlib_lsame( vect, 'U' )
           upper = stdlib_lsame( uplo, 'U' )
           kd1 = kd + 1_ilp
           kdm1 = kd - 1_ilp
           incx = ldab - 1_ilp
           iqend = 1_ilp
           info = 0_ilp
           if( .not.wantq .and. .not.stdlib_lsame( vect, 'N' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd1 ) then
              info = -6_ilp
           else if( ldq<max( 1_ilp, n ) .and. wantq ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHBTRD', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! initialize q to the unit matrix, if needed
           if( initq )call stdlib_claset( 'FULL', n, n, czero, cone, q, ldq )
           ! wherever possible, plane rotations are generated and applied in
           ! vector operations of length nr over the index set j1:j2:kd1.
           ! the real cosines and complex sines of the plane rotations are
           ! stored in the arrays d and work.
           inca = kd1*ldab
           kdn = min( n-1, kd )
           if( upper ) then
              if( kd>1_ilp ) then
                 ! reduce to complex hermitian tridiagonal form, working with
                 ! the upper triangle
                 nr = 0_ilp
                 j1 = kdn + 2_ilp
                 j2 = 1_ilp
                 ab( kd1, 1_ilp ) = real( ab( kd1, 1_ilp ),KIND=sp)
                 loop_90: do i = 1, n - 2
                    ! reduce i-th row of matrix to tridiagonal form
                    loop_80: do k = kdn + 1, 2, -1
                       j1 = j1 + kdn
                       j2 = j2 + kdn
                       if( nr>0_ilp ) then
                          ! generate plane rotations to annihilate nonzero
                          ! elements which have been created outside the band
                          call stdlib_clargv( nr, ab( 1_ilp, j1-1 ), inca, work( j1 ),kd1, d( j1 ), &
                                    kd1 )
                          ! apply rotations from the right
                          ! dependent on the the number of diagonals either
                          ! stdlib_clartv or stdlib_crot is used
                          if( nr>=2_ilp*kd-1 ) then
                             do l = 1, kd - 1
                                call stdlib_clartv( nr, ab( l+1, j1-1 ), inca,ab( l, j1 ), inca, &
                                          d( j1 ),work( j1 ), kd1 )
                             end do
                          else
                             jend = j1 + ( nr-1 )*kd1
                             do jinc = j1, jend, kd1
                                call stdlib_crot( kdm1, ab( 2_ilp, jinc-1 ), 1_ilp,ab( 1_ilp, jinc ), 1_ilp, d( &
                                          jinc ),work( jinc ) )
                             end do
                          end if
                       end if
                       if( k>2_ilp ) then
                          if( k<=n-i+1 ) then
                             ! generate plane rotation to annihilate a(i,i+k-1)
                             ! within the band
                             call stdlib_clartg( ab( kd-k+3, i+k-2 ),ab( kd-k+2, i+k-1 ), d( i+k-&
                                       1_ilp ),work( i+k-1 ), temp )
                             ab( kd-k+3, i+k-2 ) = temp
                             ! apply rotation from the right
                             call stdlib_crot( k-3, ab( kd-k+4, i+k-2 ), 1_ilp,ab( kd-k+3, i+k-1 ), 1_ilp,&
                                        d( i+k-1 ),work( i+k-1 ) )
                          end if
                          nr = nr + 1_ilp
                          j1 = j1 - kdn - 1_ilp
                       end if
                       ! apply plane rotations from both sides to diagonal
                       ! blocks
                       if( nr>0_ilp )call stdlib_clar2v( nr, ab( kd1, j1-1 ), ab( kd1, j1 ),ab( kd, &
                                 j1 ), inca, d( j1 ),work( j1 ), kd1 )
                       ! apply plane rotations from the left
                       if( nr>0_ilp ) then
                          call stdlib_clacgv( nr, work( j1 ), kd1 )
                          if( 2_ilp*kd-1<nr ) then
                          ! dependent on the the number of diagonals either
                          ! stdlib_clartv or stdlib_crot is used
                             do l = 1, kd - 1
                                if( j2+l>n ) then
                                   nrt = nr - 1_ilp
                                else
                                   nrt = nr
                                end if
                                if( nrt>0_ilp )call stdlib_clartv( nrt, ab( kd-l, j1+l ), inca,ab( kd-&
                                          l+1, j1+l ), inca,d( j1 ), work( j1 ), kd1 )
                             end do
                          else
                             j1end = j1 + kd1*( nr-2 )
                             if( j1end>=j1 ) then
                                do jin = j1, j1end, kd1
                                   call stdlib_crot( kd-1, ab( kd-1, jin+1 ), incx,ab( kd, jin+1 )&
                                             , incx,d( jin ), work( jin ) )
                                end do
                             end if
                             lend = min( kdm1, n-j2 )
                             last = j1end + kd1
                             if( lend>0_ilp )call stdlib_crot( lend, ab( kd-1, last+1 ), incx,ab( kd, &
                                       last+1 ), incx, d( last ),work( last ) )
                          end if
                       end if
                       if( wantq ) then
                          ! accumulate product of plane rotations in q
                          if( initq ) then
                       ! take advantage of the fact that q was
                       ! initially the identity matrix
                             iqend = max( iqend, j2 )
                             i2 = max( 0_ilp, k-3 )
                             iqaend = 1_ilp + i*kd
                             if( k==2_ilp )iqaend = iqaend + kd
                             iqaend = min( iqaend, iqend )
                             do j = j1, j2, kd1
                                ibl = i - i2 / kdm1
                                i2 = i2 + 1_ilp
                                iqb = max( 1_ilp, j-ibl )
                                nq = 1_ilp + iqaend - iqb
                                iqaend = min( iqaend+kd, iqend )
                                call stdlib_crot( nq, q( iqb, j-1 ), 1_ilp, q( iqb, j ),1_ilp, d( j ), &
                                          conjg( work( j ) ) )
                             end do
                          else
                             do j = j1, j2, kd1
                                call stdlib_crot( n, q( 1_ilp, j-1 ), 1_ilp, q( 1_ilp, j ), 1_ilp,d( j ), conjg( &
                                          work( j ) ) )
                             end do
                          end if
                       end if
                       if( j2+kdn>n ) then
                          ! adjust j2 to keep within the bounds of the matrix
                          nr = nr - 1_ilp
                          j2 = j2 - kdn - 1_ilp
                       end if
                       do j = j1, j2, kd1
                          ! create nonzero element a(j-1,j+kd) outside the band
                          ! and store it in work
                          work( j+kd ) = work( j )*ab( 1_ilp, j+kd )
                          ab( 1_ilp, j+kd ) = d( j )*ab( 1_ilp, j+kd )
                       end do
                    end do loop_80
                 end do loop_90
              end if
              if( kd>0_ilp ) then
                 ! make off-diagonal elements real and copy them to e
                 do i = 1, n - 1
                    t = ab( kd, i+1 )
                    abst = abs( t )
                    ab( kd, i+1 ) = abst
                    e( i ) = abst
                    if( abst/=zero ) then
                       t = t / abst
                    else
                       t = cone
                    end if
                    if( i<n-1 )ab( kd, i+2 ) = ab( kd, i+2 )*t
                    if( wantq ) then
                       call stdlib_cscal( n, conjg( t ), q( 1_ilp, i+1 ), 1_ilp )
                    end if
                 end do
              else
                 ! set e to zero if original matrix was diagonal
                 do i = 1, n - 1
                    e( i ) = zero
                 end do
              end if
              ! copy diagonal elements to d
              do i = 1, n
                 d( i ) = real( ab( kd1, i ),KIND=sp)
              end do
           else
              if( kd>1_ilp ) then
                 ! reduce to complex hermitian tridiagonal form, working with
                 ! the lower triangle
                 nr = 0_ilp
                 j1 = kdn + 2_ilp
                 j2 = 1_ilp
                 ab( 1_ilp, 1_ilp ) = real( ab( 1_ilp, 1_ilp ),KIND=sp)
                 loop_210: do i = 1, n - 2
                    ! reduce i-th column of matrix to tridiagonal form
                    loop_200: do k = kdn + 1, 2, -1
                       j1 = j1 + kdn
                       j2 = j2 + kdn
                       if( nr>0_ilp ) then
                          ! generate plane rotations to annihilate nonzero
                          ! elements which have been created outside the band
                          call stdlib_clargv( nr, ab( kd1, j1-kd1 ), inca,work( j1 ), kd1, d( j1 )&
                                    , kd1 )
                          ! apply plane rotations from one side
                          ! dependent on the the number of diagonals either
                          ! stdlib_clartv or stdlib_crot is used
                          if( nr>2_ilp*kd-1 ) then
                             do l = 1, kd - 1
                                call stdlib_clartv( nr, ab( kd1-l, j1-kd1+l ), inca,ab( kd1-l+1, &
                                          j1-kd1+l ), inca,d( j1 ), work( j1 ), kd1 )
                             end do
                          else
                             jend = j1 + kd1*( nr-1 )
                             do jinc = j1, jend, kd1
                                call stdlib_crot( kdm1, ab( kd, jinc-kd ), incx,ab( kd1, jinc-kd )&
                                          , incx,d( jinc ), work( jinc ) )
                             end do
                          end if
                       end if
                       if( k>2_ilp ) then
                          if( k<=n-i+1 ) then
                             ! generate plane rotation to annihilate a(i+k-1,i)
                             ! within the band
                             call stdlib_clartg( ab( k-1, i ), ab( k, i ),d( i+k-1 ), work( i+k-1 &
                                       ), temp )
                             ab( k-1, i ) = temp
                             ! apply rotation from the left
                             call stdlib_crot( k-3, ab( k-2, i+1 ), ldab-1,ab( k-1, i+1 ), ldab-1,&
                                        d( i+k-1 ),work( i+k-1 ) )
                          end if
                          nr = nr + 1_ilp
                          j1 = j1 - kdn - 1_ilp
                       end if
                       ! apply plane rotations from both sides to diagonal
                       ! blocks
                       if( nr>0_ilp )call stdlib_clar2v( nr, ab( 1_ilp, j1-1 ), ab( 1_ilp, j1 ),ab( 2_ilp, j1-1 ),&
                                  inca, d( j1 ),work( j1 ), kd1 )
                       ! apply plane rotations from the right
                          ! dependent on the the number of diagonals either
                          ! stdlib_clartv or stdlib_crot is used
                       if( nr>0_ilp ) then
                          call stdlib_clacgv( nr, work( j1 ), kd1 )
                          if( nr>2_ilp*kd-1 ) then
                             do l = 1, kd - 1
                                if( j2+l>n ) then
                                   nrt = nr - 1_ilp
                                else
                                   nrt = nr
                                end if
                                if( nrt>0_ilp )call stdlib_clartv( nrt, ab( l+2, j1-1 ), inca,ab( l+1,&
                                           j1 ), inca, d( j1 ),work( j1 ), kd1 )
                             end do
                          else
                             j1end = j1 + kd1*( nr-2 )
                             if( j1end>=j1 ) then
                                do j1inc = j1, j1end, kd1
                                   call stdlib_crot( kdm1, ab( 3_ilp, j1inc-1 ), 1_ilp,ab( 2_ilp, j1inc ), 1_ilp, &
                                             d( j1inc ),work( j1inc ) )
                                end do
                             end if
                             lend = min( kdm1, n-j2 )
                             last = j1end + kd1
                             if( lend>0_ilp )call stdlib_crot( lend, ab( 3_ilp, last-1 ), 1_ilp,ab( 2_ilp, last ),&
                                        1_ilp, d( last ),work( last ) )
                          end if
                       end if
                       if( wantq ) then
                          ! accumulate product of plane rotations in q
                          if( initq ) then
                       ! take advantage of the fact that q was
                       ! initially the identity matrix
                             iqend = max( iqend, j2 )
                             i2 = max( 0_ilp, k-3 )
                             iqaend = 1_ilp + i*kd
                             if( k==2_ilp )iqaend = iqaend + kd
                             iqaend = min( iqaend, iqend )
                             do j = j1, j2, kd1
                                ibl = i - i2 / kdm1
                                i2 = i2 + 1_ilp
                                iqb = max( 1_ilp, j-ibl )
                                nq = 1_ilp + iqaend - iqb
                                iqaend = min( iqaend+kd, iqend )
                                call stdlib_crot( nq, q( iqb, j-1 ), 1_ilp, q( iqb, j ),1_ilp, d( j ), &
                                          work( j ) )
                             end do
                          else
                             do j = j1, j2, kd1
                                call stdlib_crot( n, q( 1_ilp, j-1 ), 1_ilp, q( 1_ilp, j ), 1_ilp,d( j ), work( j &
                                          ) )
                             end do
                          end if
                       end if
                       if( j2+kdn>n ) then
                          ! adjust j2 to keep within the bounds of the matrix
                          nr = nr - 1_ilp
                          j2 = j2 - kdn - 1_ilp
                       end if
                       do j = j1, j2, kd1
                          ! create nonzero element a(j+kd,j-1) outside the
                          ! band and store it in work
                          work( j+kd ) = work( j )*ab( kd1, j )
                          ab( kd1, j ) = d( j )*ab( kd1, j )
                       end do
                    end do loop_200
                 end do loop_210
              end if
              if( kd>0_ilp ) then
                 ! make off-diagonal elements real and copy them to e
                 do i = 1, n - 1
                    t = ab( 2_ilp, i )
                    abst = abs( t )
                    ab( 2_ilp, i ) = abst
                    e( i ) = abst
                    if( abst/=zero ) then
                       t = t / abst
                    else
                       t = cone
                    end if
                    if( i<n-1 )ab( 2_ilp, i+1 ) = ab( 2_ilp, i+1 )*t
                    if( wantq ) then
                       call stdlib_cscal( n, t, q( 1_ilp, i+1 ), 1_ilp )
                    end if
                 end do
              else
                 ! set e to zero if original matrix was diagonal
                 do i = 1, n - 1
                    e( i ) = zero
                 end do
              end if
              ! copy diagonal elements to d
              do i = 1, n
                 d( i ) = real( ab( 1_ilp, i ),KIND=sp)
              end do
           end if
           return
     end subroutine stdlib_chbtrd

     pure module subroutine stdlib_zhbtrd( vect, uplo, n, kd, ab, ldab, d, e, q, ldq,work, info )
     !! ZHBTRD reduces a complex Hermitian band matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldq, n
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: ab(ldab,*), q(ldq,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: initq, upper, wantq
           integer(ilp) :: i, i2, ibl, inca, incx, iqaend, iqb, iqend, j, j1, j1end, j1inc, j2, &
                     jend, jin, jinc, k, kd1, kdm1, kdn, l, last, lend, nq, nr, nrt
           real(dp) :: abst
           complex(dp) :: t, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           initq = stdlib_lsame( vect, 'V' )
           wantq = initq .or. stdlib_lsame( vect, 'U' )
           upper = stdlib_lsame( uplo, 'U' )
           kd1 = kd + 1_ilp
           kdm1 = kd - 1_ilp
           incx = ldab - 1_ilp
           iqend = 1_ilp
           info = 0_ilp
           if( .not.wantq .and. .not.stdlib_lsame( vect, 'N' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( kd<0_ilp ) then
              info = -4_ilp
           else if( ldab<kd1 ) then
              info = -6_ilp
           else if( ldq<max( 1_ilp, n ) .and. wantq ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHBTRD', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! initialize q to the unit matrix, if needed
           if( initq )call stdlib_zlaset( 'FULL', n, n, czero, cone, q, ldq )
           ! wherever possible, plane rotations are generated and applied in
           ! vector operations of length nr over the index set j1:j2:kd1.
           ! the real cosines and complex sines of the plane rotations are
           ! stored in the arrays d and work.
           inca = kd1*ldab
           kdn = min( n-1, kd )
           if( upper ) then
              if( kd>1_ilp ) then
                 ! reduce to complex hermitian tridiagonal form, working with
                 ! the upper triangle
                 nr = 0_ilp
                 j1 = kdn + 2_ilp
                 j2 = 1_ilp
                 ab( kd1, 1_ilp ) = real( ab( kd1, 1_ilp ),KIND=dp)
                 loop_90: do i = 1, n - 2
                    ! reduce i-th row of matrix to tridiagonal form
                    loop_80: do k = kdn + 1, 2, -1
                       j1 = j1 + kdn
                       j2 = j2 + kdn
                       if( nr>0_ilp ) then
                          ! generate plane rotations to annihilate nonzero
                          ! elements which have been created outside the band
                          call stdlib_zlargv( nr, ab( 1_ilp, j1-1 ), inca, work( j1 ),kd1, d( j1 ), &
                                    kd1 )
                          ! apply rotations from the right
                          ! dependent on the the number of diagonals either
                          ! stdlib_zlartv or stdlib_zrot is used
                          if( nr>=2_ilp*kd-1 ) then
                             do l = 1, kd - 1
                                call stdlib_zlartv( nr, ab( l+1, j1-1 ), inca,ab( l, j1 ), inca, &
                                          d( j1 ),work( j1 ), kd1 )
                             end do
                          else
                             jend = j1 + ( nr-1 )*kd1
                             do jinc = j1, jend, kd1
                                call stdlib_zrot( kdm1, ab( 2_ilp, jinc-1 ), 1_ilp,ab( 1_ilp, jinc ), 1_ilp, d( &
                                          jinc ),work( jinc ) )
                             end do
                          end if
                       end if
                       if( k>2_ilp ) then
                          if( k<=n-i+1 ) then
                             ! generate plane rotation to annihilate a(i,i+k-1)
                             ! within the band
                             call stdlib_zlartg( ab( kd-k+3, i+k-2 ),ab( kd-k+2, i+k-1 ), d( i+k-&
                                       1_ilp ),work( i+k-1 ), temp )
                             ab( kd-k+3, i+k-2 ) = temp
                             ! apply rotation from the right
                             call stdlib_zrot( k-3, ab( kd-k+4, i+k-2 ), 1_ilp,ab( kd-k+3, i+k-1 ), 1_ilp,&
                                        d( i+k-1 ),work( i+k-1 ) )
                          end if
                          nr = nr + 1_ilp
                          j1 = j1 - kdn - 1_ilp
                       end if
                       ! apply plane rotations from both sides to diagonal
                       ! blocks
                       if( nr>0_ilp )call stdlib_zlar2v( nr, ab( kd1, j1-1 ), ab( kd1, j1 ),ab( kd, &
                                 j1 ), inca, d( j1 ),work( j1 ), kd1 )
                       ! apply plane rotations from the left
                       if( nr>0_ilp ) then
                          call stdlib_zlacgv( nr, work( j1 ), kd1 )
                          if( 2_ilp*kd-1<nr ) then
                          ! dependent on the the number of diagonals either
                          ! stdlib_zlartv or stdlib_zrot is used
                             do l = 1, kd - 1
                                if( j2+l>n ) then
                                   nrt = nr - 1_ilp
                                else
                                   nrt = nr
                                end if
                                if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( kd-l, j1+l ), inca,ab( kd-&
                                          l+1, j1+l ), inca,d( j1 ), work( j1 ), kd1 )
                             end do
                          else
                             j1end = j1 + kd1*( nr-2 )
                             if( j1end>=j1 ) then
                                do jin = j1, j1end, kd1
                                   call stdlib_zrot( kd-1, ab( kd-1, jin+1 ), incx,ab( kd, jin+1 )&
                                             , incx,d( jin ), work( jin ) )
                                end do
                             end if
                             lend = min( kdm1, n-j2 )
                             last = j1end + kd1
                             if( lend>0_ilp )call stdlib_zrot( lend, ab( kd-1, last+1 ), incx,ab( kd, &
                                       last+1 ), incx, d( last ),work( last ) )
                          end if
                       end if
                       if( wantq ) then
                          ! accumulate product of plane rotations in q
                          if( initq ) then
                       ! take advantage of the fact that q was
                       ! initially the identity matrix
                             iqend = max( iqend, j2 )
                             i2 = max( 0_ilp, k-3 )
                             iqaend = 1_ilp + i*kd
                             if( k==2_ilp )iqaend = iqaend + kd
                             iqaend = min( iqaend, iqend )
                             do j = j1, j2, kd1
                                ibl = i - i2 / kdm1
                                i2 = i2 + 1_ilp
                                iqb = max( 1_ilp, j-ibl )
                                nq = 1_ilp + iqaend - iqb
                                iqaend = min( iqaend+kd, iqend )
                                call stdlib_zrot( nq, q( iqb, j-1 ), 1_ilp, q( iqb, j ),1_ilp, d( j ), &
                                          conjg( work( j ) ) )
                             end do
                          else
                             do j = j1, j2, kd1
                                call stdlib_zrot( n, q( 1_ilp, j-1 ), 1_ilp, q( 1_ilp, j ), 1_ilp,d( j ), conjg( &
                                          work( j ) ) )
                             end do
                          end if
                       end if
                       if( j2+kdn>n ) then
                          ! adjust j2 to keep within the bounds of the matrix
                          nr = nr - 1_ilp
                          j2 = j2 - kdn - 1_ilp
                       end if
                       do j = j1, j2, kd1
                          ! create nonzero element a(j-1,j+kd) outside the band
                          ! and store it in work
                          work( j+kd ) = work( j )*ab( 1_ilp, j+kd )
                          ab( 1_ilp, j+kd ) = d( j )*ab( 1_ilp, j+kd )
                       end do
                    end do loop_80
                 end do loop_90
              end if
              if( kd>0_ilp ) then
                 ! make off-diagonal elements real and copy them to e
                 do i = 1, n - 1
                    t = ab( kd, i+1 )
                    abst = abs( t )
                    ab( kd, i+1 ) = abst
                    e( i ) = abst
                    if( abst/=zero ) then
                       t = t / abst
                    else
                       t = cone
                    end if
                    if( i<n-1 )ab( kd, i+2 ) = ab( kd, i+2 )*t
                    if( wantq ) then
                       call stdlib_zscal( n, conjg( t ), q( 1_ilp, i+1 ), 1_ilp )
                    end if
                 end do
              else
                 ! set e to zero if original matrix was diagonal
                 do i = 1, n - 1
                    e( i ) = zero
                 end do
              end if
              ! copy diagonal elements to d
              do i = 1, n
                 d( i ) = real( ab( kd1, i ),KIND=dp)
              end do
           else
              if( kd>1_ilp ) then
                 ! reduce to complex hermitian tridiagonal form, working with
                 ! the lower triangle
                 nr = 0_ilp
                 j1 = kdn + 2_ilp
                 j2 = 1_ilp
                 ab( 1_ilp, 1_ilp ) = real( ab( 1_ilp, 1_ilp ),KIND=dp)
                 loop_210: do i = 1, n - 2
                    ! reduce i-th column of matrix to tridiagonal form
                    loop_200: do k = kdn + 1, 2, -1
                       j1 = j1 + kdn
                       j2 = j2 + kdn
                       if( nr>0_ilp ) then
                          ! generate plane rotations to annihilate nonzero
                          ! elements which have been created outside the band
                          call stdlib_zlargv( nr, ab( kd1, j1-kd1 ), inca,work( j1 ), kd1, d( j1 )&
                                    , kd1 )
                          ! apply plane rotations from one side
                          ! dependent on the the number of diagonals either
                          ! stdlib_zlartv or stdlib_zrot is used
                          if( nr>2_ilp*kd-1 ) then
                             do l = 1, kd - 1
                                call stdlib_zlartv( nr, ab( kd1-l, j1-kd1+l ), inca,ab( kd1-l+1, &
                                          j1-kd1+l ), inca,d( j1 ), work( j1 ), kd1 )
                             end do
                          else
                             jend = j1 + kd1*( nr-1 )
                             do jinc = j1, jend, kd1
                                call stdlib_zrot( kdm1, ab( kd, jinc-kd ), incx,ab( kd1, jinc-kd )&
                                          , incx,d( jinc ), work( jinc ) )
                             end do
                          end if
                       end if
                       if( k>2_ilp ) then
                          if( k<=n-i+1 ) then
                             ! generate plane rotation to annihilate a(i+k-1,i)
                             ! within the band
                             call stdlib_zlartg( ab( k-1, i ), ab( k, i ),d( i+k-1 ), work( i+k-1 &
                                       ), temp )
                             ab( k-1, i ) = temp
                             ! apply rotation from the left
                             call stdlib_zrot( k-3, ab( k-2, i+1 ), ldab-1,ab( k-1, i+1 ), ldab-1,&
                                        d( i+k-1 ),work( i+k-1 ) )
                          end if
                          nr = nr + 1_ilp
                          j1 = j1 - kdn - 1_ilp
                       end if
                       ! apply plane rotations from both sides to diagonal
                       ! blocks
                       if( nr>0_ilp )call stdlib_zlar2v( nr, ab( 1_ilp, j1-1 ), ab( 1_ilp, j1 ),ab( 2_ilp, j1-1 ),&
                                  inca, d( j1 ),work( j1 ), kd1 )
                       ! apply plane rotations from the right
                          ! dependent on the the number of diagonals either
                          ! stdlib_zlartv or stdlib_zrot is used
                       if( nr>0_ilp ) then
                          call stdlib_zlacgv( nr, work( j1 ), kd1 )
                          if( nr>2_ilp*kd-1 ) then
                             do l = 1, kd - 1
                                if( j2+l>n ) then
                                   nrt = nr - 1_ilp
                                else
                                   nrt = nr
                                end if
                                if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( l+2, j1-1 ), inca,ab( l+1,&
                                           j1 ), inca, d( j1 ),work( j1 ), kd1 )
                             end do
                          else
                             j1end = j1 + kd1*( nr-2 )
                             if( j1end>=j1 ) then
                                do j1inc = j1, j1end, kd1
                                   call stdlib_zrot( kdm1, ab( 3_ilp, j1inc-1 ), 1_ilp,ab( 2_ilp, j1inc ), 1_ilp, &
                                             d( j1inc ),work( j1inc ) )
                                end do
                             end if
                             lend = min( kdm1, n-j2 )
                             last = j1end + kd1
                             if( lend>0_ilp )call stdlib_zrot( lend, ab( 3_ilp, last-1 ), 1_ilp,ab( 2_ilp, last ),&
                                        1_ilp, d( last ),work( last ) )
                          end if
                       end if
                       if( wantq ) then
                          ! accumulate product of plane rotations in q
                          if( initq ) then
                       ! take advantage of the fact that q was
                       ! initially the identity matrix
                             iqend = max( iqend, j2 )
                             i2 = max( 0_ilp, k-3 )
                             iqaend = 1_ilp + i*kd
                             if( k==2_ilp )iqaend = iqaend + kd
                             iqaend = min( iqaend, iqend )
                             do j = j1, j2, kd1
                                ibl = i - i2 / kdm1
                                i2 = i2 + 1_ilp
                                iqb = max( 1_ilp, j-ibl )
                                nq = 1_ilp + iqaend - iqb
                                iqaend = min( iqaend+kd, iqend )
                                call stdlib_zrot( nq, q( iqb, j-1 ), 1_ilp, q( iqb, j ),1_ilp, d( j ), &
                                          work( j ) )
                             end do
                          else
                             do j = j1, j2, kd1
                                call stdlib_zrot( n, q( 1_ilp, j-1 ), 1_ilp, q( 1_ilp, j ), 1_ilp,d( j ), work( j &
                                          ) )
                             end do
                          end if
                       end if
                       if( j2+kdn>n ) then
                          ! adjust j2 to keep within the bounds of the matrix
                          nr = nr - 1_ilp
                          j2 = j2 - kdn - 1_ilp
                       end if
                       do j = j1, j2, kd1
                          ! create nonzero element a(j+kd,j-1) outside the
                          ! band and store it in work
                          work( j+kd ) = work( j )*ab( kd1, j )
                          ab( kd1, j ) = d( j )*ab( kd1, j )
                       end do
                    end do loop_200
                 end do loop_210
              end if
              if( kd>0_ilp ) then
                 ! make off-diagonal elements real and copy them to e
                 do i = 1, n - 1
                    t = ab( 2_ilp, i )
                    abst = abs( t )
                    ab( 2_ilp, i ) = abst
                    e( i ) = abst
                    if( abst/=zero ) then
                       t = t / abst
                    else
                       t = cone
                    end if
                    if( i<n-1 )ab( 2_ilp, i+1 ) = ab( 2_ilp, i+1 )*t
                    if( wantq ) then
                       call stdlib_zscal( n, t, q( 1_ilp, i+1 ), 1_ilp )
                    end if
                 end do
              else
                 ! set e to zero if original matrix was diagonal
                 do i = 1, n - 1
                    e( i ) = zero
                 end do
              end if
              ! copy diagonal elements to d
              do i = 1, n
                 d( i ) = real( ab( 1_ilp, i ),KIND=dp)
              end do
           end if
           return
     end subroutine stdlib_zhbtrd



end submodule stdlib_lapack_eigv_sym_comp
